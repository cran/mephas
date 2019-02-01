#if (!require(shiny)) {install.packages("shiny")}; library(shiny)
#if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
#if (!require(mixOmics)) {install.packages("mixOmics")}; library(mixOmics)

##----------#----------#----------#----------
##
## 8MFSpcapls SERVER
##
## Language: EN
## 
## DT: 2019-01-15
##
##----------#----------#----------#----------
##' @title SERVER of PCA PLS regression

##' @export
server.pcapls <- function(){

shinyServer(

function(input, output, session) {

#----------0. dataset input----------

#source("0data_server.R", local=TRUE) 
##----------#----------#----------#----------
##
## 8MFSpcapls SERVER
##
##    >data
##
## Language: EN
## 
## DT: 2019-01-15
##
##----------#----------#----------#----------


X <- reactive({
# req(input$file)
inFile <- input$file.x
if (is.null(inFile)){
df = coloncancer[,1:100]
}
else{
df <- as.data.frame(
read.csv(
inFile$datapath.x,
header = input$header.x,
sep = input$sep.x,
quote = input$quote.x
)
)
}
return(df)
})

Y <- reactive({
# req(input$file)
inFile <- input$file.y
if (is.null(inFile))
# eg data
{df = coloncancer[,101:105]
}
else{

df <- as.data.frame(
read.csv(
inFile$datapath.y,
header = input$header.y,
sep = input$sep.y,
quote = input$quote.y
)
)
}
return(df)
})

output$table.x <- renderDataTable(
head(X()), options = list(pageLength = 5, scrollX = TRUE))
output$table.y <- renderDataTable(
head(Y()), options = list(pageLength = 5, scrollX = TRUE))

# summary variable

data <- reactive({
cbind.data.frame(X(),Y())
})

# Basic Descriptives


output$cv = renderUI({
selectInput(
'cv', h5('Select continuous variables from X or Y matrices'), 
selected = NULL, choices = names(data()), multiple = TRUE)
})

output$dv = renderUI({
selectInput(
'dv', h5('Select categorical/discrete variables from X or Y matrices'), 
selected = NULL, choices = names(data()), multiple = TRUE)
})

sum = eventReactive(input$Bc,  ##> cont var
          {
            pastecs::stat.desc(data()[, input$cv], desc = TRUE, norm=TRUE)
            #Hmisc::describe(X()[,input$cv])
          })

fsum = eventReactive(input$Bd, ##> dis var
           {
             data = as.data.frame(data()[, input$dv])
             colnames(data) = input$dv
             lapply(data, table)
           })

output$sum <- renderTable({sum()}, rownames = TRUE)

fsum = eventReactive(input$Bd, ##> dis var
           {
             data = as.data.frame(data()[, input$dv])
             colnames(data) = input$dv
             lapply(data, table)
           })

output$sum = renderTable({sum()}, rownames = TRUE)

output$fsum = renderPrint({fsum()})

# First Exploration of Variables

output$tx = renderUI({  
selectInput(
'tx', h5('Variable in the x-axis'),
selected = "NULL", 
choices = c("NULL",names(data())))

})

output$ty = renderUI({
selectInput(
'ty',
h5('Variable in the y-axis'),
selected = "NULL", 
choices = c("NULL",names(data())))

})

output$p1 <- renderPlot({
validate(
need(input$tx != "NULL", "Please select one continuous variable")
)
validate(
need(input$ty != "NULL", "Please select one continuous variable")
)
ggplot(data(), aes(x=data()[,input$tx], y=data()[,input$ty])) + geom_point(shape=1) + 
geom_smooth(method=lm) +xlab(input$tx) +ylab(input$ty)+ theme_minimal()
})

## histogram
output$hx = renderUI({

selectInput(
'hx',
h5('Histogram of the continuous variable'),
selected = "NULL",
choices = c("NULL",names(data())))
})



output$hxd = renderUI({
selectInput(
'hxd',
h5('Histogram of the categorical/discrete variable'),
selected = "NULL",
choices = c("NULL",names(data())))
})

output$p2 = renderPlot({
validate(
need(input$hx != "NULL", "Please select one continuous variable")
)
ggplot(data(), aes(x = data()[, input$hx])) + 
geom_histogram(binwidth = input$bin, colour = "black",fill = "white") + 
geom_density()+
xlab("") + theme_minimal() + theme(legend.title = element_blank())
})

output$p3 = renderPlot({
validate(
need(input$hxd != "NULL", "Please select one categorical/discrete variable")
)
ggplot(data(), aes(x = data()[, input$hxd])) + 
geom_histogram(colour = "black",fill = "white",  stat="count") + 
xlab("") + theme_minimal() + theme(legend.title = element_blank())
})

#----------1. PCA ----------
output$nc <- renderText({ input$nc })
# model
pca <- reactive({
pca = mixOmics::pca(as.matrix(X()), ncomp = input$nc, scale = TRUE)
pca})

pca.x <- reactive({ pca()$x })

output$fit  <- renderPrint({
res <- rbind(pca()$explained_variance,pca()$cum.var)
rownames(res) <- c("explained_variance", "cumulative_variance")
res})

output$comp <- renderDataTable({ round(pca.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$downloadData <- downloadHandler(
filename = function() {
"pca_components.csv"
},
content = function(file) {
write.csv(pca.x(), file, row.names = FALSE)
}
)

output$pca.ind  <- renderPlot({ plotIndiv(pca(), comp=c(input$c1, input$c2))})
output$pca.var  <- renderPlot({ plotVar(pca(),   comp=c(input$c1, input$c2))})
output$pca.bp   <- renderPlot({ biplot(pca())})
output$pca.plot <- renderPlot({ plot(pca())})

#----------2. PLS ----------

pls <- reactive({
pls = mixOmics::pls(as.matrix(X()),as.matrix(Y()), ncomp = input$nc.pls, scale = TRUE)
pls})

pls.x <- reactive({ pls()$variates$X })
pls.y <- reactive({ pls()$variates$Y })

output$comp.x <- renderDataTable({ round(pls.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$comp.y <- renderDataTable({ round(pls.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$downloadData.pls.x <- downloadHandler(
filename = function() {
"pls_components_x.csv"
},
content = function(file) {
write.csv(pls.x(), file, row.names = FALSE)
}
)

output$downloadData.pls.y <- downloadHandler(
filename = function() {
"pls_components_y.csv"
},
content = function(file) {
write.csv(pls.y(), file, row.names = FALSE)
}
)

output$pls.ind  <- renderPlot({ plotIndiv(pls(), comp=c(input$c1.pls, input$c2.pls)) })
output$pls.var  <- renderPlot({ plotVar(pls(),   comp=c(input$c1.pls, input$c2.pls)) })

#----------3. SPLS ----------

spls <- reactive({
spls = mixOmics::spls(as.matrix(X()),as.matrix(Y()), ncomp = input$nc.spls, scale = TRUE,
keepX=input$x.spls, keepY=input$y.spls)
spls})

spls.x <- reactive({ spls()$variates$X })
spls.y <- reactive({ spls()$variates$Y })

output$comp.sx <- renderDataTable({ round(spls.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$comp.sy <- renderDataTable({ round(spls.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$downloadData.spls.x <- downloadHandler(
filename = function() {
"spls_components_x.csv"
},
content = function(file) {
write.csv(spls.x(), file, row.names = FALSE)
}
)

output$downloadData.spls.y <- downloadHandler(
filename = function() {
"spls_components_y.csv"
},
content = function(file) {
write.csv(spls.y(), file, row.names = FALSE)
}
)

output$spls.ind  <- renderPlot({ plotIndiv(spls(), comp=c(input$c1.spls, input$c2.spls)) })
output$spls.var  <- renderPlot({ plotVar(spls(),   comp=c(input$c1.spls, input$c2.spls)) })
output$spls.load <- renderPlot({ plotLoadings(spls()) })

#---------------------------##

observe({
if (input$close > 0) stopApp()                             # stop shiny
})


})

}


