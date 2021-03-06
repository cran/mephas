##----------#----------#----------#----------
##
## 7MFSreg UI
##
##    >Linear regression
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------
##' @title UI of Linear Regression (univariate Regression)
##' @export
reg.lm.ui <- function(){
sidebarLayout(

sidebarPanel(

h4(tags$b("Given that dataset has been imported, please design you model")),      
uiOutput('y'),    
uiOutput('x'),
uiOutput('fx'),

radioButtons("intercept", "Intercept", ##> intercept or not
       choices = c("Remove Intercept" = "-1",
                   "Keep intercept" = ""),
       selected = "-1"),
h5("Additional terms (confounding or interaction)"), 
helpText('Note: Start with "+". For interactive term, please type "+ as.factor(var1):var2"'), 
tags$textarea(id='conf', cols=40, " " ), 
p(br()),
actionButton("F", "Create formula", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")

),

mainPanel(

h4(tags$b("Linear Regression Model")),

tags$style(type='text/css', '#formula {background-color: rgba(0,0,255,0.10); color: blue;}'),
verbatimTextOutput("formula", placeholder = TRUE),
helpText("Note: '-1' in the formula indicates that intercept has been removed"),
hr(),


h4(tags$b("Results of the linear regression")),
actionButton("B1", "Show the results"),
p(br()),
tabsetPanel(

tabPanel("Parameters' estimation",
p(br()),
#sliderInput("range", label = h3("choose subset"), min = 1, max = 100, value = c(1,10)),
tags$b("1. Regression's coefficients"),
htmlOutput("fit"), p(br()),
tags$b("2. ANOVA Table"), tableOutput("anova"),p(br()),
tags$b("3. Select a formula-based model by AIC"), verbatimTextOutput("step")
),

tabPanel("Model's diagnostics", 
p(br()),
tags$b("Diagnostic Plots"), 
radioButtons("num", "Choose plot",
           choices = c("Residuals vs fitted plot" = 1,
                       "Normal Q-Q" = 2,
                       "Scale-Location" = 3,
                       "Cook's distance" = 4,
                       "Residuals vs Leverage" = 5),
           selected = 1),
plotOutput("p.lm", width = "800px", height = "400px")
),

tabPanel("Estimated fitting values",
p(br()),
tags$b("Estimation is based on import dataset"),
dataTableOutput("fitdt0")),

tabPanel("Prediction on new data", p(br()),
#prediction part
##-------csv file for prediction -------##   
# Input: Select a file ----
fileInput("newfile", "Upload new .csv data set",
        multiple = TRUE,
        accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")),
# Input: Checkbox if file has header ----
checkboxInput("newheader", "Header", TRUE),

fluidRow(
column(3, 
 # Input: Select separator ----
radioButtons("newsep", "Separator",
           choices = c(Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"),
           selected = ",")),

column(3,
# Input: Select quotes ----
radioButtons("newquote", "Quote",
           choices = c(None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"),
           selected = '"')),
column(3,
# prediction type
radioButtons("interval", "Choose predictive interval (0.95-level)",
           choices = c(
                       "Confidence Interval" = "confidence",
                       "Prediction Interval" = "prediction"),
           selected = 'confidence'))
        ), ##fluidRow(

actionButton("B2", "Submit after the estimation of model"), 
helpText("If no data is uploaded, the example testing data (the first 10 rows of import dataset) will be shown."),

p(br()),
tags$b("Data display with prediction results"), 
p(br()),
dataTableOutput("pred")

) 


)
)
)
}