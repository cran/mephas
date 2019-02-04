##' MFSanova function creates a dynamic calculator which enables users to do ANOVA analysis and multiple comparison. Users can either input data manually or upload their dataset.
##'
##' This app includes one-way ANOVA, two-way ANOVA, and multiple comparison. The results include ANOVA table, descriptive statistics, and bar-plot of the group's mean value.
##' Please click "close" window to quit the application. "Rmisc" and "psych" packages are required.
##'
##' @title MEPHAS Shiny Application of ANOVA and Multiple Comparison
##' @return The shiny app and web page of the ANOVA and multiple comparison
##'
##' @import shiny
##' @import ggplot2
##'
##' @importFrom psych describeBy
##' @importFrom Rmisc summarySE
##' @importFrom graphics plot
##' @importFrom utils head read.csv write.csv
##' @importFrom stats TukeyHSD aov pairwise.t.test

##' @examples
##' # library(mephas)
##' # MFSanova()

##' @export
MFSanova <- function(){

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
####################
ui <- tagList(

navbarPage(

title = "Analysis of Variance",

##---------- Panel 1 ----------
tabPanel(
"One-way",

headerPanel("One-way ANOVA"),

tags$b("Assumptions"),
tags$ul(
tags$li("The differences of samples are numeric and continuous and based on the normal distribution"),
tags$li("The data collection process was random without replacement."),
tags$li("The samples are from the populations with same variances.")
),

sidebarLayout(
sidebarPanel(
h4("Hypotheses"),
tags$b("Null hypothesis"),
p("All group means are equal"),

tags$b("Alternative hypothesis"),
p("At least two of the group means are not the same"),
hr(),
##----Import data----##
h4("Data Preparation"),

tabsetPanel(
##-------input data-------##s
tabPanel(
"Manual input",
p(br()),
helpText("Please input the values and factors' name (missing value is input as NA)"),

splitLayout(

verticalLayout(
tags$b("Values"),
tags$textarea(
id = "x1",
rows = 10,
"0.55\n3.22\n1.08\n1.99\n0.93\n2.98\n2.93\n2.41\n1.98\n1.94\n2.27\n2.69\n2.23\n3.87\n1.43\n3.6\n1.51\n1.7\n2.79\n2.96\n4.67\n5.37\n1.77\n3.52\n5.62\n4.22\n3.33\n3.91\n4.85\n3.4"
)),

verticalLayout(
tags$b("Factors"),
tags$textarea(
id = "f11", #p
rows = 10,
"A1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3"
))
)
,

helpText("Change the names of sample (optinal)"),
tags$textarea(id = "cn1", rows = 2, "X\nA")),

##-------csv file-------##
tabPanel(
"Upload .csv",
p(br()),
fileInput(
'file1', 'Choose .csv', #p
accept = c(
'text/csv',
'text/comma-separated-values,text/plain',
'.csv'
)
),
checkboxInput('header1', 'Header', TRUE), #p
radioButtons('sep1', 'Separator', #p
   c(
     Comma = ',',
     Semicolon = ';',
     Tab = '\t'
   ),
   ','
))
)
),

mainPanel(
h4("ANOVA Table"),
tableOutput("anova1"),
hr(),

h4("Data Description"),
tabsetPanel(
tabPanel("Data Display",p(br()),
dataTableOutput("table1")
),

tabPanel('Basic Statistics',p(br()),
verbatimTextOutput("bas1")),

tabPanel("Marginal means plot",p(br()),
plotOutput("mmean1", width = "500px", height = "300px")
)
)
)
)
),

##---------- Panel 2 ----------

tabPanel(
"Two-way",

headerPanel("Two-way ANOVA"),

tags$b("Assumptions"),
tags$ul(
tags$li("The populations from which the samples were obtained are normally or approximately normally distributed."),
tags$li("The samples are independent."),
tags$li("The variances of the populations are equal."),
tags$li("The groups have the same sample size.")

),

sidebarLayout(
sidebarPanel(
h4("Hypotheses"),
tags$b("Null hypothesis 1"),
HTML("<p>The population means of the first factor are equal. </p>"),
tags$b("Null hypothesis 2"),
HTML("<p>The population means of the second factor are equal.</p>"),
tags$b("Null hypothesis 3"),
HTML("<p>There is no interaction between the two factors.</p>"),
hr(),

##----Import data----##
h4("Data Preparation"),

tabsetPanel(
##-------input data-------##s
tabPanel(
"Manual input",
p(br()),
helpText("Missing value is input as NA"),

splitLayout(

verticalLayout(
tags$b("Values"),
tags$textarea(
id = "x", #p
rows = 10,
"0.55\n3.22\n1.08\n1.99\n0.93\n2.98\n2.93\n2.41\n1.98\n1.94\n2.27\n2.69\n2.23\n3.87\n1.43\n3.6\n1.51\n1.7\n2.79\n2.96\n4.67\n5.37\n1.77\n3.52\n5.62\n4.22\n3.33\n3.91\n4.85\n3.4"
)),

verticalLayout(
tags$b("Factor 1"),
tags$textarea(
id = "f1", #p
rows = 10,
"A1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3"
)),

verticalLayout(
tags$b("Factor 2"),
tags$textarea(
id = "f2", #p
rows = 10,
"T1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT1\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2\nT2"
))
),

helpText("Change the names of sample (optinal)"),
tags$textarea(id = "cn", rows = 3, "X\nA\nB") #p
), #tabPanel(

##-------csv file-------##
tabPanel(
"Upload .csv",
p(br()),
fileInput(
'file', 'Choose .csv', #p
accept = c(
'text/csv',
'text/comma-separated-values,text/plain',
'.csv'
)
),
checkboxInput('header', 'Header', TRUE), #p
radioButtons('sep', 'Separator', #p
   c(
     Comma = ',',
     Semicolon = ';',
     Tab = '\t'
   ),
   ','
)
))
),


mainPanel(

h3(tags$b("ANOVA Table")),
checkboxInput('inter', 'Interaction', TRUE), #p
tableOutput("anova"),

hr(),

h4("Descriptive Statistics"),
tabsetPanel(
tabPanel("Data display", p(br()),
dataTableOutput("table")
),

tabPanel('Basic statistics',p(br()),
numericInput("grp", 'Choose the factor', 2, 2, 3, 1),
verbatimTextOutput("bas")
),

tabPanel("Means plot",p(br()),
checkboxInput('tick', 'Untick to change the group and x-axis', TRUE), #p
plotOutput("meanp.a", width = "500px", height = "300px")
),

tabPanel("Marginal means plot",p(br()),
checkboxInput('tick2', 'Untick to change the x-axis', TRUE), #p
plotOutput("mmean.a", width = "500px", height = "300px")
)
)
)
)
), ##

##---------- Panel 3 ----------

tabPanel(
"Multiple Comparison",
headerPanel("Multiple Comparison"),

tags$b("Assumptions"),
tags$ul(
tags$li("Significant effects have been found when there are three or more levels of a factor"),
tags$li("After an ANOVA, the means of your response variable may differ significantly across the factor, but it is unknown which pairs of the factor levels are significantly different from each other")
),

sidebarLayout(

sidebarPanel(
h4("Hypotheses"),
tags$b("Null hypothesis"),
HTML("<p>The population means of the factor are equal. </p>"),
hr(),

##----Import data----##
h4("Data Preparation"),

tabsetPanel(
##-------input data-------##s
tabPanel(
"Manual input",
p(br()),
helpText("Missing value is input as NA"),

splitLayout(
verticalLayout(
tags$b("Values"),
tags$textarea(
id = "xm", #p
rows = 10,
"0.55\n3.22\n1.08\n1.99\n0.93\n2.98\n2.93\n2.41\n1.98\n1.94\n2.27\n2.69\n2.23\n3.87\n1.43\n3.6\n1.51\n1.7\n2.79\n2.96\n4.67\n5.37\n1.77\n3.52\n5.62\n4.22\n3.33\n3.91\n4.85\n3.4"
)),

verticalLayout(
tags$b("Factors"),
tags$textarea(
id = "fm", #p
rows = 10,
"A1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3\nA1\nA2\nA3"
))
),

helpText("Change the names of sample (optinal)"),
tags$textarea(id = "cnm", rows = 2, "X\nA") #p
),

##-------csv file-------##
tabPanel(
"Upload .csv",
p(br()),
fileInput(
'filem', 'Choose .csv', #p
accept = c(
'text/csv',
'text/comma-separated-values,text/plain',
'.csv'
)
),
checkboxInput('headerm', 'Header', TRUE), #p
radioButtons('sepm', 'Separator', #p
   c(
     Comma = ',',
     Semicolon = ';',
     Tab = '\t'
   ),
   ','
)
))
),

mainPanel(
h4("Results"),

tabsetPanel(

tabPanel("Pairwise t-test", p(br()),
radioButtons("method", "Choose one method",
          c(Bonferroni = 'bonferroni',
            Holm = 'holm',
            Hochberg = 'hochberg',
            Hommel = 'hommel',
            FDR_Benjamini_Hochberg = 'BH',
            Benjamini_Yekutieli = 'BY'
          ),
          "bonferroni"),
verbatimTextOutput("multiple")
),

tabPanel("Tukey Honest Significant Differences", p(br()),
verbatimTextOutput("hsd")
)
),

h4("Data Display"),
dataTableOutput("tablem")

)
)
)

##---------- other panels ----------

#source("../0tabs/home.R",local=TRUE)$value,
#source("../0tabs/stop.R",local=TRUE)$value
#stop()

))
####################

####################
server <- function(input, output) {

##---------- 1.One way ANOVA ----------
Y1 <- reactive({
inFile <- input$file1
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$x1, "[\n, \t, ]")))
F1 <- unlist(strsplit(input$f11, "[\n, \t, ]"))
Y <- data.frame(X = X, F1 = F1)
names(Y) = unlist(strsplit(input$cn1, "[\n, \t, ]"))
return(Y)
}
else {
csv <- as.data.frame(
read.csv(
inFile$datapath,
header = input$header1,
sep = input$sep1
)
)
return(csv)
}
})

output$table1 <- renderDataTable({Y1()}, options = list(pageLength = 5, scrollX = TRUE))

output$bas1 <- renderPrint({
x <- Y1()
res <- describeBy(x[,1], x[,2])
return(res)
})



output$mmean1 = renderPlot({
x = Y1()
b = Rmisc::summarySE(x,names(x)[1], names(x)[2])


ggplot(b, aes(x=b[,1], y=b[,3], fill=b[,1])) +
geom_bar(stat="identity", position = "dodge")+ xlab("") +ylab("")+
geom_errorbar(aes(ymin=b[,3]-b[,5], ymax=b[,3]+b[,5]),
width=.2,                    # Width of the error bars
position=position_dodge(.9))+
scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title=element_blank())

})

output$anova1 <- renderTable({
x <- Y1()

res <- aov(x[,1]~x[,2])
res.table <- summary(res)[[1]]
rownames(res.table)[1] <- c(names(x)[2])

return(res.table)
},
width = "500px", rownames = TRUE)


##---------- 2. two way ANOVA ----------
Y <- reactive({
inFile <- input$file
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
F1 <- unlist(strsplit(input$f1, "[\n, \t, ]"))
F2 <- unlist(strsplit(input$f2, "[\n, \t, ]"))
Y <- data.frame(X = X, F1 = F1, F2 = F2)
names(Y) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(Y)
}
else {
csv <- as.data.frame(
read.csv(
inFile$datapath,
header = input$header,
sep = input$sep
)
)
return(csv)
}
})

output$table <- renderDataTable({Y()}, options = list(pageLength = 5, scrollX = TRUE))

output$bas <- renderPrint({
x <- Y()
res <- describeBy(x[,1], x[,input$grp])
#rownames(res) = c("number.var", "number.null", "number.na")
return(res)
})

output$meanp.a = renderPlot({
x = Y()
b = Rmisc::summarySE(x,names(x)[1], c(names(x)[2], names(x)[3]))

if (input$tick == "TRUE"){
ggplot(b, aes(x=b[,1], y=b[,4], colour=b[,2], group=b[,2])) +
geom_line() + xlab("") +ylab("")+
geom_point(shape=21, size=3, fill="white") +
theme_minimal() + theme(legend.title = element_blank())
}

else {
ggplot(b, aes(x=b[,2], y=b[,4], colour=b[,1], group=b[,1])) +
geom_line() + xlab("") +ylab("")+
geom_point(shape=21, size=3, fill="white") +
theme_minimal() + theme(legend.title = element_blank())
}

})

output$mmean.a = renderPlot({
x = Y()
b = Rmisc::summarySE(x,names(x)[1], c(names(x)[2], names(x)[3]))

if (input$tick2 == "TRUE"){
ggplot(b, aes(x=b[,1], y=b[,4], fill=b[,2])) +
geom_bar(stat="identity", position = "dodge")+ xlab("") +ylab("")+
geom_errorbar(aes(ymin=b[,4]-b[,6], ymax=b[,4]+b[,6]),
width=.2,                    # Width of the error bars
position=position_dodge(.9))+
scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title=element_blank())
}

else {
ggplot(b, aes(x=b[,2], y=b[,4], fill=b[,1])) +
geom_bar(stat="identity", position = "dodge")+ xlab("") +ylab("")+
geom_errorbar(aes(ymin=b[,4]-b[,6], ymax=b[,4]+b[,6]),
width=.2,                    # Width of the error bars
position=position_dodge(.9))+
scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title=element_blank())
}

})

output$anova <- renderTable({
x <- Y()

if (input$inter == "TRUE"){
res <- aov(x[,1]~x[,2]*x[,3])
res.table <- summary(res)[[1]]
rownames(res.table)[1:3] <- c(names(x)[2],names(x)[3], paste0(names(x)[2]," X ",names(x)[3]))
}

else {
res <- aov(x[,1]~x[,2]+x[,3])
res.table <- summary(res)[[1]]
rownames(res.table)[1:2] <- names(x)[2:3]
}

return(res.table)
},
width = "500px", rownames = TRUE)


##---------- 2. multiple comparison ----------
Ym <- reactive({
inFile <- input$filem
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$xm, "[\n, \t, ]")))
F1 <- unlist(strsplit(input$fm, "[\n, \t, ]"))
Y <- data.frame(X = X, F1 = F1)
names(Y) = unlist(strsplit(input$cnm, "[\n, \t, ]"))
return(Y)
}
else {
csv <- as.data.frame(
read.csv(
inFile$datapath,
header = input$headerm,
sep = input$sepm
)
)
#names(Y) = unlist(strsplit(input$cnm, "[\n, \t, ]"))
return(csv)
}
})

output$tablem <- renderDataTable({Ym()}, options = list(pageLength = 5, scrollX = TRUE))


output$multiple <- renderPrint({
x <- Ym()
res <- pairwise.t.test(x[,names(x)[1]], x[,names(x)[2]], p.adjust.method = input$method)
return(res)
})


output$hsd <- renderPrint({
x <- Ym()
res <- TukeyHSD(aov(x[,names(x)[1]]~x[,names(x)[2]]))
return(res)
})

}
####################

app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)

}

