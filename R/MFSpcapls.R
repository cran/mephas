
##' MFSpcapls function creates a dynamic calculator which enables users to do PCA, PLS, sparse PLS regression. Users can either input data manually or upload their dataset.
##'
##' This app includes one-way ANOVA, two-way ANOVA, and multiple comparison. The results include ANOVA table, descriptive statistics, and bar-plot of the group's mean value.
##' Please click "close" window to quit the application. "ggfortify", spls" and "pls" package may be required.
##' @title MEPHAS Shiny Application of PCA PLS Regression
##' @return The shiny web page of the PCA PLS regression
##'
##' @import shiny
##' @import ggfortify
##'
##' @importFrom utils install.packages
##' @importFrom stats coef prcomp screeplot
##' @importFrom pls mvr scoreplot loadingplot


##' @examples
##' # library(mephas)
##' # MFSpcapls()
##' # not run

##' @export
MFSpcapls <- function(){

####################

ui <- tagList(

navbarPage(


title = "Principal Components",

#----------1. dataset panel----------

tabPanel("Dataset",

titlePanel("Data Preparation"),

#source("0data_ui.R", local=TRUE, encoding="UTF-8")$value
pls.data.ui()

),


## 1. PCA ---------------------------------------------------------------------------------
tabPanel("PCA",

titlePanel("Principal component analysis"),

sidebarLayout(

sidebarPanel(
h4("Model's configuration"),

checkboxInput("scale1", "Scale the data (X)", FALSE),

numericInput("nc", "Number of components in PCA:", 5, min = 2, max = NA),
helpText("If data are complete, 'pca' uses Singular Value Decomposition; if there are some missing values, it uses the NIPALS algorithm."),

hr(),
h4("Figure's configuration"),
numericInput("c1", "Component at x-axis", 1, min = 1, max = NA),
numericInput("c2", "Component at y-axis", 2, min = 1, max = NA),
helpText("x and y must be different"),
p(br()),
checkboxInput("frame", "Add group circle in the plot", FALSE)


),

mainPanel(

h4("Explained and cumulative variance"),
p(br()),
verbatimTextOutput("fit"),

hr(),
h4("Plots"),

tabsetPanel(

tabPanel("Plot of two components" ,p(br()),
plotOutput("pca.ind", width = "400px", height = "400px"),

radioButtons("type", "The shape of circle by group",
 choices = c(T = 't',
             Normal = "norm",
             Convex = "convex",
             Euclid = "euclid"),
 selected = 't')
),

#tabPanel("Plot of variables' correlation circle" ,p(br()),
#  plotOutput("pca.var", width = "400px", height = "400px")),

tabPanel("Plot of the loadings of two components" ,p(br()),
plotOutput("pca.bp", width = "400px", height = "400px")),

tabPanel("Plot of the explained variance" ,p(br()),
plotOutput("pca.plot", width = "400px", height = "400px"))

),

hr(),

h4("Data Display"),
tabsetPanel(
tabPanel("Raw data" , p(br()),
dataTableOutput("table.z")),

tabPanel("New components", p(br()),
downloadButton("downloadData", "Download new components"), p(br()),
dataTableOutput("comp")
)
)
)
)
), #penal tab end

## 2.  PLS, ---------------------------------------------------------------------------------
tabPanel("PLS(R)",

titlePanel("Partial Least Squares (Regression)"),

#source("pls_ui.R", local=TRUE, encoding="UTF-8")$value
pls.ui()

),

## 3. SPLS, ---------------------------------------------------------------------------------
tabPanel("SPLS(R)",

titlePanel("Sparse Partial Least Squares (Regression)"),

sidebarLayout(
sidebarPanel(

h4("Whether to scale data"),
checkboxInput("sc.x", "Scale the predictors (X)", FALSE),
checkboxInput("sc.y", "Scale the responders (Y)", FALSE),
hr(),

h4("Cross-validation's configuration"),
helpText("Find the optimal number of component (K)"),
numericInput("cv1", "Minimum number of components", 2, min = 2, max = NA),
numericInput("cv2", "Maximum number of components", 4, min = 3, max = NA),
radioButtons("s.select", "Variables' selection method (SPLSR)",
 choices = c("PLS" = 'pls2',
             "SIMPLS" = "simpls"),
 selected = "pls2"),

radioButtons("s.fit", "Model fitting method (PLSR)",
 choices = c(
             "Kernel" = "kernelpls",
             "Wide kernel" = "widekernelpls",
             "SIMPLS" = "simpls",
             "Classical orthogonal scores"="oscorespls"),
 selected = "simpls"),

hr(),
h4("Model's configuration"),
helpText("Results from cross-validation can be used as references"),
numericInput("nc.spls", "Number of components", 2, min = 2, max = NA),
numericInput("eta", "Eta (0 to 1)", 0.5, min = 0, max = 1, step=0.1 ),
numericInput("kappa", "Kappa (0 to 0.5, default is 0.5)", 0.5, min = 0, max = 0.5, step=0.1),
checkboxInput("trace", "Show the process of variable selection", FALSE)
),

mainPanel(
h4("SPLS Results"),
tabsetPanel(
  tabPanel("Cross validation", p(br()),
  verbatimTextOutput("spls.cv")),

  tabPanel("SPLS", p(br()),
  verbatimTextOutput("spls") )
  ),

hr(),

h4("Plots"),
tabsetPanel(
tabPanel("Heatmap of cross-validated MSPE", p(br()),
plotOutput("heat.cv", width = "600px", height = "400px")),

tabPanel("Coefficient path plot of SPLS",  p(br()),
numericInput("yn", "The N'th Y vector", 1, min = 1, max = NA),
#numericInput("c2.spls", "Component at y-axis", 2, min = 1, max = 20)
plotOutput("coef.var", width = "400px", height = "400px")
)

),
hr(),

h4("Results"),
tabsetPanel(
  tabPanel("Selected vairbales (X)",p(br()),
    downloadButton("downloadData.s.sv", "Download1"), p(br()),
  dataTableOutput("s.sv") ),

  tabPanel("New components based on selected variables (X)",p(br()),
    downloadButton("downloadData.s.comp", "Download2"), p(br()),
  dataTableOutput("s.comp") ),

  tabPanel("Coefficients",p(br()),
    downloadButton("downloadData.s.cf", "Download3"), p(br()),
  dataTableOutput("s.cf") ),

  tabPanel("Projection",p(br()),
    downloadButton("downloadData.s.pj", "Download4"), p(br()),
  dataTableOutput("s.pj") ),

  tabPanel("Prediction", p(br()),
    downloadButton("downloadData.s.pd", "Download5"), p(br()),
  dataTableOutput("s.pd"))
  )

))
)
))


server <- function(input, output, session) {

#source("0data_server.R", local=TRUE, encoding="UTF-8")

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

#load("coloncancer.RData")


example.x <- reactive({
                switch(input$edata.x,
               "Gene sample1" = genesample1,
               "Gene sample2" = genesample2)
        })

X <- reactive({
  # req(input$file)
  inFile <- input$file.x
  if (is.null(inFile)){
    df = example.x()
  }
  else{
    df <- as.data.frame(
      read.csv(
        inFile$datapath,
        header = input$header.x,
        sep = input$sep.x,
        quote = input$quote.x
      )
    )
  }
  return(df)
})

example.y <- reactive({
                switch(input$edata.y,
               "Y group pca" = ygroup_pca,
               "Y array pls_spls" = yarray_s_pls,
               "Y matrix pls_spls"= ymatrix_s_pls)
        })

Y <- reactive({
  if (input$add.y == FALSE)
  {
    df = NULL
  }
  else
  {
    inFile <- input$file.y
  if (is.null(inFile))
    # eg data
  {
    df = example.y()
  }
  else{
  df <- as.data.frame(
    read.csv(
      inFile$datapath,
      header = input$header.y,
      sep = input$sep.y,
      quote = input$quote.y
      )
    )
  }
  }

  return(df)
})


 output$table.x <- renderDataTable(
    X(), options = list(pageLength = 5, scrollX = TRUE))
 output$table.y <- renderDataTable(
    Y(), options = list(pageLength = 5, scrollX = TRUE))

data <- reactive({
  if (input$add.y == FALSE) X()
  else cbind.data.frame(Y(),X())})

output$table.z <- renderDataTable(
    data(), options = list(pageLength = 5, scrollX = TRUE))


# Basic Descriptives


output$cv = renderUI({
  selectInput(
    'cv', h5('Select continuous variables from data'),
    selected = NULL, choices = names(data()), multiple = TRUE)
})

output$dv = renderUI({
selectInput(
'dv', h5('Select categorical/discrete variables from data'),
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
  prcomp(as.matrix(X()), rank.=input$nc, scale. = input$scale1)
  })

pca.x <- reactive({ pca()$x })

#output$fit  <- renderPrint({
#  res <- rbind(pca()$explained_variance,pca()$cum.var)
#  rownames(res) <- c("explained_variance", "cumulative_variance")
#  res})
output$fit  <- renderPrint({
  summary(pca())
  })

output$comp <- renderDataTable({ round(pca.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$downloadData <- downloadHandler(
    filename = function() {
      "pca_components.csv"
    },
    content = function(file) {
      write.csv(pca.x(), file, row.names = FALSE)
    }
  )
# Plot of two components
output$pca.ind  <- renderPlot({

  if (input$frame == FALSE)
  {
  autoplot(pca(), data = data(), x = input$c1, y = input$c2,
    scale = input$scale1, frame = FALSE,
    label = TRUE, label.size = 3, shape = FALSE)+ theme_minimal()
  }
  else
  {
  autoplot(pca(), data = data(), x = input$c1, y = input$c2,
    scale = input$scale1, frame = TRUE, frame.type = input$type,
    label = TRUE, label.size = 3, shape = FALSE, colour=names(data())[1])+ theme_minimal()
  }
  })
# Plot of the loadings of two components

output$pca.bp   <- renderPlot({
  autoplot(pca(), data=X(), x = input$c1, y = input$c2, label = TRUE, label.size = 3, shape = FALSE,
    loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)+ theme_minimal()
})

# Plot of the explained variance
output$pca.plot <- renderPlot({ screeplot(pca(), npcs= input$nc, type="lines", main="") })

#----------2. PLS ----------

#source("pls_server.R", local=TRUE, encoding="UTF-8")
##----------#----------#----------#----------
##
## 8MFSpcapls SERVER
##
##    >pls
##
## Language: EN
##
## DT: 2019-01-15
##
##----------#----------#----------#----------

pls <- reactive({
  Y <- as.matrix(Y())
  X <- as.matrix(X())
  pls = mvr(Y~X, ncomp = input$nc.pls, scale = input$scale2,
    method=input$mtd.pls, validation=input$val)
  pls})

output$pls.sum  <- renderPrint({
  summary(pls())
  })

pls.x <- reactive({
  xs <- as.matrix.data.frame(pls()$scores)
  dimnames(xs) <- dimnames(pls()$scores)
  xs
  })
pls.y <- reactive({
  ys <- as.matrix.data.frame(pls()$Yscores)
  dimnames(ys) <- dimnames(pls()$Yscores)
  ys
  })

pls.xload <- reactive({
  xl <- as.matrix.data.frame(pls()$loadings)
  dimnames(xl) <- dimnames(pls()$loadings)
  xl
  })
pls.yload <- reactive({
  yl <- as.matrix.data.frame(pls()$Yloadings)
  dimnames(yl) <- dimnames(pls()$Yloadings)
  yl
  })

pls.coef <- reactive({ as.data.frame(pls()$coefficients) })
pls.proj <- reactive({ as.data.frame(pls()$projection) })

pls.fit <- reactive({ as.data.frame(pls()$fitted.values) })
pls.res <- reactive({ as.data.frame(pls()$residuals) })

#table

output$comp.x <- renderDataTable({ round(pls.x(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$comp.y <- renderDataTable({ round(pls.y(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$load.x <- renderDataTable({ round(pls.xload(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$load.y <- renderDataTable({ round(pls.yload(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$coef <- renderDataTable({ round(pls.coef(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$proj <- renderDataTable({ round(pls.proj(),3)}, options = list(pageLength = 5, scrollX = TRUE))

output$fit.pls <- renderDataTable({ round(pls.fit(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$res.pls <- renderDataTable({ round(pls.res(),3)}, options = list(pageLength = 5, scrollX = TRUE))



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

output$downloadData.pls.xload <- downloadHandler(
    filename = function() {
      "pls_loadings_x.csv"
    },
    content = function(file) {
      write.csv(pls.xload(), file, row.names = FALSE)
    }
  )
output$downloadData.pls.yload <- downloadHandler(
    filename = function() {
      "pls_loadings_y.csv"
    },
    content = function(file) {
      write.csv(pls.yload(), file, row.names = FALSE)
    }
  )

output$downloadData.pls.coef <- downloadHandler(
    filename = function() {
      "pls_coefficient.csv"
    },
    content = function(file) {
      write.csv(pls.coef(), file, row.names = FALSE)
    }
  )
output$downloadData.pls.proj <- downloadHandler(
    filename = function() {
      "pls_project.csv"
    },
    content = function(file) {
      write.csv(pls.proj(), file, row.names = FALSE)
    }
  )

output$downloadData.pls.fit <- downloadHandler(
    filename = function() {
      "pls_fit_values.csv"
    },
    content = function(file) {
      write.csv(pls.fit(), file, row.names = FALSE)
    }
  )

output$downloadData.pls.res <- downloadHandler(
    filename = function() {
      "pls_residuals.csv"
    },
    content = function(file) {
      write.csv(pls.res(), file, row.names = FALSE)
    }
  )


output$pls.pbiplot <- renderPlot({ biplot(pls(), comps = c(input$c1.pls, input$c2.pls), which = input$which, var.axes = TRUE)})
output$pls.pscore  <- renderPlot({ scoreplot(pls(),comps=c(input$c1.pls, input$c2.pls)) })

output$pls.pload <- renderPlot({ loadingplot(pls(), comps=c(input$c1.pls: input$c2.pls)) })
output$pls.pcoef <- renderPlot({ plot(pls(), "coefficients", comps=c(input$c1.pls: input$c2.pls) ) })

output$pls.pred <- renderPlot({ plot(pls(), "prediction", ncomp= input$snum)})
output$pls.pval <- renderPlot({ plot(pls(), "validation") })
#----------3. SPLS ----------

#source("spls_server.R", local=TRUE, encoding="UTF-8")
##----------#----------#----------#----------
##
## 8MFSpcapls SERVER
##
##    >pls
##
## Language: EN
##
## DT: 2019-01-15
##
##----------#----------#----------#----------

output$spls.cv  <- renderPrint({
  spls::cv.spls(as.matrix(X()),as.matrix(Y()), eta = seq(0.1,0.9,0.1), K = c(input$cv1:input$cv2),
    select=input$s.select, fit = input$s.fit, scale.x = input$sc.x, scale.y = input$sc.y, plot.it = FALSE)

  })

output$heat.cv <- renderPlot({
  spls::cv.spls(as.matrix(X()),as.matrix(Y()), eta = seq(0.1,0.9,0.1), K = c(input$cv1:input$cv2),
    select=input$s.select, fit = input$s.fit, scale.x = input$sc.x, scale.y = input$sc.y, plot.it = TRUE) })


spls <- reactive({
  ss <- spls::spls(as.matrix(X()),as.matrix(Y()), K=input$nc.spls, eta=input$eta, kappa=input$kappa,
    select=input$s.select, fit=input$s.fit, scale.x=input$sc.x, scale.y=input$sc.y,
    eps=1e-4, maxstep=100, trace=input$trace)
  #cv <- print(ss)
  return(ss)
  })

output$spls <- renderPrint({
  spls()
  })

output$coef.var <- renderPlot({
  plot(spls(), yvar=input$yn)
   })

#output$coef.spls <- renderPlot({
#  coefplot.spls(spls(),nwin=c(2,2), xvar=c(input$xn1:input$xn1+3))
#  })

spls.sv <- reactive({ as.data.frame(X()[spls()$A])})
spls.comp <- reactive({ data.frame(as.matrix(X()[spls()$A])%*%as.matrix(spls()$projection))})
spls.cf <- reactive({ coef(spls()) })
spls.pj <- reactive({ spls()$projection})
spls.pd <- reactive({ predict(spls(), type="fit")})

output$s.sv <- renderDataTable({ spls.sv()}, options = list(pageLength = 5, scrollX = TRUE))
output$s.comp <- renderDataTable({ round(spls.comp(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$s.cf <- renderDataTable({ round(spls.cf(),3)}, options = list(pageLength = 5, scrollX = TRUE))
output$s.pj <- renderDataTable({ (spls.pj())}, options = list(pageLength = 5, scrollX = TRUE))
output$s.pd <- renderDataTable({ round(spls.pd(),3)}, options = list(pageLength = 5, scrollX = TRUE))


output$downloadData.s.sv <- downloadHandler(
    filename = function() {
      "spls_select_var.csv"
    },
    content = function(file) {
      write.csv(spls.sv(), file, row.names = FALSE)
    }
  )

output$downloadData.s.comp <- downloadHandler(
    filename = function() {
      "spls_components_x.csv"
    },
    content = function(file) {
      write.csv(spls.comp(), file, row.names = FALSE)
    }
  )

output$downloadData.s.cf <- downloadHandler(
    filename = function() {
      "spls_coefficient.csv"
    },
    content = function(file) {
      write.csv(spls.cf(), file, row.names = FALSE)
    }
  )

output$downloadData.s.pj <- downloadHandler(
    filename = function() {
      "spls_project.csv"
    },
    content = function(file) {
      write.csv(spls.pj(), file, row.names = FALSE)
    }
  )

output$downloadData.s.pd <- downloadHandler(
    filename = function() {
      "spls_predict.csv"
    },
    content = function(file) {
      write.csv(spls.pd(), file, row.names = FALSE)
    }
  )

}

app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)

}

