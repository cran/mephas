##' MFSttest function creates a dynamic calculator which enables users to do t tests . Users can either input data manually or upload their dataset.
##'
##' This app includes t tests for one sample, two samples, and two paired samples. The results include box-plots, histogram, descriptive statistics,and test results. "gridExtra", "reshape", "pastecs" package may be required.
##' @title MEPHAS Shiny Application of T test.
##' @return The shiny application web page of T test for one sample and two samples
##'
##' @import shiny
##' @import ggplot2
##'
##' @importFrom gridExtra grid.arrange
##' @importFrom reshape melt
##' @importFrom pastecs stat.desc
##' @importFrom stats t.test var.test
##' @importFrom utils read.csv

##' @examples
##' # library(mephas)
##' # MFSttest()
##' # not run

##' @export
MFSttest <- function(){

ui <- tagList(

navbarPage(

title = "Tests of Means",

##---------- Panel 1 ---------

tabPanel( "One Sample",

headerPanel("One Sample t-Test"),

HTML(
"
<b>Notations </b>

<ul>
<li> X is the dependent observations
<li> &#956 is the population mean
<li> &#956&#8320 is the specific mean
</ul>

<b>Assumptions </b>

<ul>
<li> X is numeric, continuous
<li> Each observation of X (sample) is independent and approximately normally distributed
<li> The data collection process is random without replacement
</ul>
"
),

hr(),

sidebarLayout(

sidebarPanel(

h4("Data Preparation"),

tabsetPanel(

tabPanel("Manual input", p(br()),

helpText("Missing value is input as NA"),

tags$textarea(
id = "x", #p
rows = 10,
"4.2\n5.3\n7.6\n6.7\n6.3\n3.5\n5.8\n6.3\n3.2\n4.6\n5.5\n5.2\n4.6\n4.8\n4.5\n5.3\n4.3\n4.3\n6.2\n6.7"
),

helpText("Change the name of sample (optional)"),
tags$textarea(id = "cn", rows = 1, "X") ), #tabPanel(


tabPanel("Upload CSV file", p(br()),

##-------csv file-------##   
fileInput('file', "Choose CSV file",
  accept = c("text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")),
#helpText("The columns of X are not suggested greater than 500"),
# Input: Checkbox if file has header ----
checkboxInput("header", "Header", TRUE),

# Input: Select separator ----
radioButtons("sep", "Separator",
     choices = c(Comma = ',',
                 Semicolon = ';',
                 Tab = '\t'),
     selected = ',')

) 
),

hr(),

h4("Configuration"),
numericInput('mu', HTML("Specify the mean, &#956&#8320"), 7), #p

h4("Hypotheses"),

tags$b("Null hypothesis"),
HTML("<p> &#956 = &#956&#8320: the population mean of X is &#956&#8320 </p>"),

radioButtons(
"alt", 
label = "Alternative hypothesis",
choiceNames = list(
HTML("&#956 &#8800 &#956&#8320: the population mean of X is not &#956&#8320"),
HTML("&#956 < &#956&#8320: the population mean of X is less than &#956&#8320"),
HTML("&#956 > &#956&#8320: the population mean of X is greater than &#956&#8320")
),
choiceValues = list("two.sided", "less", "greater"))

),


mainPanel(

h4("Descriptive Statistics"),

tabsetPanel(

tabPanel("Data Display", p(br()),  

dataTableOutput("table")),

tabPanel("Basic Descriptives", p(br()), 

splitLayout(
tableOutput("bas"), 
tableOutput("des"), 
tableOutput("nor"))  ),

tabPanel("Boxplot", p(br()), 
splitLayout(
plotOutput("bp", width = "400px", height = "400px", click = "plot_click1"),

wellPanel(
verbatimTextOutput("info1"), hr(),

helpText(
HTML(
"Notes:
<ul>
<li> Points are simulated and located randomly in the same horizontal line. </li>
<li> Outliers will be highlighted in red, if existing. </li>
<li> The red outlier may not cover the simulated point. </li>
<li> The red outlier only indicates the value in horizontal line.</li>
</ul>"
)
)
)
) ),

tabPanel("Mean and SD Plot", p(br()), 

plotOutput("meanp", width = "400px", height = "400px")),

tabPanel("Plots of Normality", p(br()), 

plotOutput("makeplot", width = "900px", height = "300px"), 
sliderInput("bin","The width of bins in histogram",min = 0.01,max = 5,value = 0.2))
),

hr(),
h4("Test Results"),
tableOutput("t.test")

)

)


),

##---------- Panel 2 ---------

tabPanel("Two Independent Samples",

headerPanel("Two-Sample t-Test"),

HTML(
"
<b> Notations </b>
<ul>
<li> The independent observations are designated X and Y
<li> &#956&#8321 is the population mean of X; &#956&#8322 is the population mean of Y
</ul>

<b> Assumptions </b>

<ul>
<li> Each of the two populations being compared should follow the normal distribution
<li> X and Y should be sampled independently from the two populations being compared
<li> The two populations being compared should have the same variance
</ul>
"
),

hr(),

sidebarLayout(

sidebarPanel(

h4("Data Preparation"),

tabsetPanel(
##-------input data-------##
tabPanel(
"Manual input",
p(br()),
helpText("Missing value is input as NA"),
tags$textarea(id = "x1",rows = 10,"4.2\n5.3\n7.6\n6.7\n6.3\n3.5\n5.8\n6.3\n3.2\n4.6\n5.5\n5.2\n4.6\n4.8\n4.5\n5.3\n4.3\n4.3\n6.2\n6.7"),
## disable on chrome
tags$textarea(id = "x2",rows = 10,"6.6\n6.9\n6.9\n8.4\n7.8\n6.6\n8.6\n5.5\n4.6\n6.1\n7.1\n4.0\n6.5\n5.6\n8.1\n5.8\n7.9\n6.4\n6.5\n7.4"),

helpText("Change the names of samples (optinal)"),
tags$textarea(id = "cn2", rows = 2, "X\nY")
),

##-------csv file-------##
tabPanel(
"Upload CSV file",
p(br()),

fileInput('file2','Choose CSV file', #p
accept = c(
'text/csv',
'text/comma-separated-values,text/plain',
'.csv'
)
),
checkboxInput('header2', 'Header', TRUE), #p
radioButtons("sep2", "Separator",
choices = c(Comma = ',',
           Semicolon = ';',
           Tab = '\t'),
selected = ',')
)
),

hr(),

h4("Hypotheses"),

tags$b("Null hypothesis"),
HTML("<p> &#956&#8321 = &#956&#8322: X and Y have equal population mean </p>"),

radioButtons("alt.t2", #p
label = "Alternative hypothesis",
choiceNames = list(
HTML("&#956&#8321 &#8800 &#956&#8322: the population means of X and Y are not equal"),
HTML("&#956&#8321 < &#956&#8322: the population means of X is less than Y"),
HTML("&#956&#8321 > &#956&#8322: the population means of X is greater than Y")
),
choiceValues = list("two.sided", "less", "greater")
)
),

mainPanel(

h4("Descriptive Statistics"),

tabsetPanel(

tabPanel("Data Display",p(br()), 

dataTableOutput("table2")),

tabPanel("Basic Statistics",p(br()), 

splitLayout(
tableOutput("bas2"),
tableOutput("des2"),
tableOutput("nor2")
)),

tabPanel("Boxplot",p(br()),     
splitLayout(
plotOutput("bp2",width = "400px",height = "400px",click = "plot_click2"),

wellPanel(
verbatimTextOutput("info2"), 
hr(),

helpText(
HTML(
"Notes:
<ul>
<li> Points are simulated and located randomly in the same horizontal line. </li>
<li> Outliers will be highlighted in red, if existing. </li>
<li> The red outlier may not cover the simulated point. </li>
<li> The red outlier only indicates the value in horizontal line.</li>
</ul>"
)
)
)              
)),

tabPanel("Mean and SD Plot", p(br()), 

plotOutput("meanp2", width = "400px", height = "400px")),

tabPanel("Plots of Normality", p(br()), 

plotOutput("makeplot2", width = "600px", height = "600px"),
sliderInput("bin2","The width of bins in histogram",min = 0.01,max = 5,value = 0.2))

),

hr(),
h4(("Test Results")),
tableOutput("var.test"),
helpText("When P value<0.05, please go to the 'Welch Two Sample t-test'"),
tableOutput("t.test2")

)
)

),

##---------- Panel 3 ---------

tabPanel("Two Paired Samples",

headerPanel("Paired t-Test"),

HTML("

<b> Notations </b>

<ul>
<li> The dependent observations are designated X and Y
<li> &#916 is the underlying mean differences between X and Y
</ul>

<b> Assumptions </b>

<ul>
<li> The differences of paired samples are approximately normally distributed
<li> The differences of paired samples are numeric and continuous and based on the normal distribution
<li> The data collection process was random without replacement
</ul>
"
),

helpText("A typical example of the pared sample is that the repeated measurements, where subjects are tested prior to a treatment, say for high blood pressure, and the same subjects are tested again after treatment with a blood-pressure lowering medication"),


hr(),

sidebarLayout(

sidebarPanel(

h4("Data Preparation"),

tabsetPanel(
##-------input data-------##
tabPanel(
"Manual input",
p(br()),
helpText("Missing value is input as NA"),
tags$textarea(id = "x1.p",rows = 10,
"4.2\n5.3\n7.6\n6.7\n6.3\n3.5\n5.8\n6.3\n3.2\n4.6\n5.5\n5.2\n4.6\n4.8\n4.5\n5.3\n4.3\n4.3\n6.2\n6.7"
),
## disable on chrome
tags$textarea(id = "x2.p",rows = 10,
"6.6\n6.9\n6.7\n8.4\n7.8\n6.6\n8.6\n5.5\n4.6\n6.1\n7.1\n4.0\n6.5\n5.6\n8.1\n5.8\n7.9\n6.4\n6.5\n7.4"
),
helpText("Change the names of two samples (optinal)"),
tags$textarea(id = "cn.p", rows = 2, "X\nY\n(X-Y)")

),

##-------csv file-------##
tabPanel("Upload CSV file",
p(br()),

fileInput(
'file.p',
'Choose CSV File',
accept = c(
'text/csv',
'text/comma-separated-values,text/plain',
'.csv'
)
),
checkboxInput('header.p', 'Header', TRUE), #p

radioButtons("sep.p", "Separator",
       choices = c(Comma = ',',
                   Semicolon = ';',
                   Tab = '\t'),
       selected = ',')
)
),

hr(),

h4("Hypotheses"),

tags$b("Null hypothesis"),
HTML("<p> &#916 = 0: X and Y have equal effect </p>"),

radioButtons(
"alt.pt",
label = "Alternative hypothesis",
choiceNames = list(
HTML("&#916 &#8800 0: the population mean of X and Y are not equal"),
HTML("&#916 < 0: the population mean of X is less than Y"),
HTML("&#916 > 0: the population mean of X is greater than Y")
),
choiceValues = list("two.sided", "less", "greater")
)

),

mainPanel(

h4("Descriptive Statistics"),

tabsetPanel(

tabPanel("Data display", p(br()),  

dataTableOutput("table.p")),

tabPanel("Basic Statistics",p(br()), 

splitLayout(
tableOutput("bas.p"),
tableOutput("des.p"),
tableOutput("nor.p")
)
),

tabPanel("Boxplot of the difference", p(br()), 
splitLayout(
plotOutput("bp.p",width = "400px",height = "400px",click = "plot_click3"),

wellPanel(
verbatimTextOutput("info3"), hr(),

helpText(
HTML(
"Notes:
<ul>
<li> Points are simulated and located randomly in the same horizontal line. </li>
<li> Outliers will be highlighted in red, if existing. </li>
<li> The red outlier may not cover the simulated point. </li>
<li> The red outlier only indicates the value in horizontal line.</li>
</ul>"
)
)
)
)
),

tabPanel("Mean and SD Plot", p(br()), 

plotOutput("meanp.p", width = "400px", height = "400px")),

tabPanel("Plots of Normality", p(br()), 

plotOutput("makeplot.p", width = "900px", height = "300px"),
sliderInput("bin.p","The width of bins in histogram",min = 0.01,max = 5,value = 0.2)
)
),

hr(),
h4("Test Results"),
tableOutput("t.test.p")
)
)

)

)
)

server <- function(input, output) {

##---------- 1. One sample t test---------

X <- reactive({
inFile <- input$file
if (is.null(inFile)) {
# input data
X <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
X <- data.frame(X = X)
names(X) = unlist(strsplit(input$cn, "[\n, \t, ]"))
}
else {
# CSV data
csv <- read.csv(inFile$datapath,
header = input$header,
sep = input$sep)

X <- as.data.frame(csv)
}
return(X)
})

output$table <-renderDataTable({X()}, options = list(pageLength = 5))

output$bas <- renderTable({
X <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
res <- stat.desc(X)[1:3]
names(res) = c("number.var", "number.null", "number.na")
#names(res) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)
},   
width = "200px", rownames = TRUE, digits = 0)

output$des <- renderTable({
X <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
res <- stat.desc(X)[4:14]
#names = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)
},   
width = "200px", rownames = TRUE)

output$nor <- renderTable({
X <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
res <- stat.desc(X, norm = TRUE)[15:20]
#names(res) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)
},   
width = "200px", rownames = TRUE)

# box plot
output$bp = renderPlot({
x = X()
ggplot(x, aes(x = "", y = x[, 1])) + geom_boxplot(width = 0.2, outlier.colour = "red") + geom_jitter(width = 0.1, size = 1.5) + ylab("") + xlab("") + ggtitle("") + theme_minimal()
})

output$info1 <- renderText({
xy_str = function(e) {
if (is.null(e))
return("NULL\n")
paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal position: ", "\n", xy_str(input$plot_click1))
})

output$meanp = renderPlot({
x <- as.numeric(unlist(strsplit(input$x, "[\n, \t, ]")))
des = data.frame(t(stat.desc(x)))
#p1 = ggplot(des, aes(x = rownames(des), y = mean)) + geom_errorbar(width = .1, aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev),data = des) +
#  xlab("") + ylab(expression(Mean %+-% SD)) + geom_point(shape = 21, size = 3) + theme_minimal() + theme(legend.title = element_blank())
p2 = ggplot(des, aes(x = rownames(des), y = mean)) + xlab("") + ylab(expression(Mean %+-% SD)) +  geom_bar(position = position_dodge(),stat = "identity",width = 0.2, alpha = .3) +
geom_errorbar(width = .1,position = position_dodge(.9),aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev),data = des) + theme_minimal() + theme(legend.title = element_blank())

grid.arrange(p2)
})

output$makeplot <- renderPlot({
x <- Z()
plot1 <- ggplot(x, aes(sample = x[, 1])) + stat_qq() + ggtitle("Normal Q-Q Plot") + xlab("") + theme_minimal()  ## add line,
plot2 <- ggplot(x, aes(x = x[, 1])) + geom_histogram(colour = "black",fill = "grey",binwidth = input$bin, position = "identity") + xlab("") + ggtitle("Histogram") + theme_minimal() + theme(legend.title =element_blank())
plot3 <- ggplot(x, aes(x = x[, 1])) + geom_density() + ggtitle("Density Plot") + xlab("") + theme_minimal() + theme(legend.title =element_blank())

grid.arrange(plot1, plot2, plot3, ncol = 3)
})

output$t.test <- renderTable({
x <- X()
res <-t.test(
as.vector(x[, 1]),
mu = input$mu,
alternative = input$alt)
res.table <- t(
data.frame(
T_statistic = res$statistic,
P_value = res$p.value,
Estimated_mean = res$estimate,
Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4),")"),
Degree_of_freedom = res$parameter
)
)
colnames(res.table) <- res$method
return(res.table)
}, 
width = "500px", rownames = TRUE)

##---------- 2. Two sample t test---------

Y <- reactive({
inFile <- input$file2
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$x1, "[\n, \t, ]")))
Y <- as.numeric(unlist(strsplit(input$x2, "[\n, \t, ]")))
x <- data.frame(X = X, Y = Y)
names(x) = unlist(strsplit(input$cn2, "[\n, \t, ]"))
return(x)
}
else {
csv <- as.data.frame(
read.csv(
inFile$datapath,
header = input$header2,
sep = input$sep2
)
)
return(csv)
}

})

output$table2 <- renderDataTable({Y()}, options = list(pageLength = 5))

output$bas2 <- renderTable({
x <- Y()
res <- stat.desc(x)[1:3,]
rownames(res) = c("number.var", "number.null", "number.na")
return(res)
},   
width = "200px", rownames = TRUE, digits = 0)

output$des2 <- renderTable({
x <- Y()
res <- stat.desc(x)[4:14,]
return(res)
},   
width = "200px", rownames = TRUE)

output$nor2 <- renderTable({
x <- Y()
res <- stat.desc(x, norm = TRUE)[15:20,]
return(res)
},   
width = "200px", rownames = TRUE)

#plots
output$bp2 = renderPlot({
x = Y()
mx = melt(x, idvar = names(x))
ggplot(mx, aes(x = mx[,"variable"], y = mx[,"value"], fill = mx[,"variable"])) + geom_boxplot(width = 0.4,outlier.colour = "red",alpha = .3) + geom_jitter(width = 0.1, size = 1.5) + ylab(" ") + xlab(" ") + ggtitle("") + theme_minimal() + theme(legend.title =element_blank())
})

output$meanp2 = renderPlot({
x = Y()
des = data.frame(t(stat.desc(x)))
#p1 = ggplot(des, aes(x = rownames(des), y = mean, fill = rownames(des))) + 
#  geom_errorbar(width = .1, aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev), data = des) + 
#  xlab("") + ylab(expression(Mean %+-% SD)) + geom_point(shape = 21, size =3) + theme_minimal() + theme(legend.title = element_blank())
p2 = ggplot(des, aes(x = rownames(des), y = mean, fill = rownames(des))) + 
xlab("") + ylab(expression(Mean %+-% SD)) + geom_bar(position = position_dodge(), stat = "identity", width = 0.2, alpha = .3) + 
geom_errorbar(width = .1, position = position_dodge(.9), aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev), data = des) + 
theme_minimal() + theme(legend.title = element_blank())

grid.arrange(p2)
})

output$makeplot2 <- renderPlot({
x <- Y()
mx <- melt(x, idvar = names(x))  ###bug: using as id variables
# normal qq plot
plot1 <- ggplot(x, aes(sample = x[, 1])) + stat_qq(color = "brown1") + ggtitle(paste0("Normal Q-Q Plot of ", colnames(x[1]))) + theme_minimal()
plot2 <- ggplot(x, aes(sample = x[, 2])) + stat_qq(color = "forestgreen") + ggtitle(paste0("Normal Q-Q Plot of ", colnames(x[2]))) + theme_minimal()
# histogram and density
plot3 <- ggplot(mx, aes(x = mx[,"value"], colour = mx[,"variable"], fill = mx[,"variable"])) + 
geom_histogram(binwidth = input$bin2, alpha = .3, position = "identity") + 
ggtitle("Histogram") + xlab("") + theme_minimal() + theme(legend.title = element_blank())
plot4 <- ggplot(mx, aes(x = mx[,"value"], colour = mx[,"variable"])) + geom_density() + 
ggtitle("Density Plot") + xlab("") + theme_minimal() + theme(legend.title = element_blank())

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
})

output$info2 <- renderText({
xy_str = function(e) {
if (is.null(e))
return("NULL\n")
paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal postion: ", "\n", xy_str(input$plot_click2))
})

# test result
output$var.test <- renderTable({
x <- Y()
res <- var.test(as.vector(x[, 1]), as.vector(x[, 2]))
res.table <- t(
data.frame(
F_statistic = res$statistic,
P_value = res$p.value,
Confidence_interval_0.95 = paste0("(", round(res$conf.int[1], digits = 4), ", ", round(res$conf.int[2], digits = 4),")"),
Estimated_var_ratio = res$estimate
)
)
colnames(res.table) <- res$method
return(res.table)
}, 
width = "500px", rownames = TRUE)

output$t.test2 <- renderTable({
x <- Y()
res <- t.test(
as.vector(x[, 1]),
as.vector(x[, 2]),
alternative = input$alt.t2,
var.equal = TRUE
)

res.table <- t(
data.frame(
T_statistic = res$statistic,
P_value = res$p.value,
Estimated_mean_X = res$estimate[1],
Estimated_mean_Y = res$estimate[2],
Estimated_mean_diff = res$estimate[1] - res$estimate[2],
Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ", round(res$conf.int[2], digits = 4), ")" ),
Degree_of_freedom = res$parameter
)
)
res1 <- t.test(
as.vector(x[, 1]),
as.vector(x[, 2]),
alternative = input$alt.t2,
var.equal = FALSE
)
res1.table <- t(
data.frame(
T_statistic = res1$statistic,
P_value = res1$p.value,
Estimated_mean_X = res1$estimate[1],
Estimated_mean_Y = res1$estimate[2],
Estimated_mean_diff = res1$estimate[1] - res1$estimate[2],
Confidence_interval_0.95 = paste0("(",round(res1$conf.int[1], digits = 4),", ",round(res1$conf.int[2], digits = 4),")"),
Degree_of_freedom = res1$parameter
)
)

res2.table <- cbind(res.table, res1.table)
colnames(res2.table) <- c(res$method, res1$method)
return(res2.table)

}, 
width = "800px", rownames = TRUE)

##---------- 3. Paired sample t test ---------

#data
Z <- reactive({
# prepare dataset
inFile <- input$file.p
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$x1.p, "[\n, \t, ]")))
Y <- as.numeric(unlist(strsplit(input$x2.p, "[\n, \t, ]")))
x <- data.frame(X = X, Y = Y)
x$diff <- round(x[, 1] - x[, 2],4)
names(x) = unlist(strsplit(input$cn.p, "[\n, \t, ]"))
return(x)
}
else {
csv <- as.data.frame(
read.csv(
inFile$datapath,
header = input$header.p,
sep = input$sep.p
))
csv$diff <- round(csv[, 1] - csv[, 2],4)

return(csv)
}
})

output$table.p <-renderDataTable({Z()}, options = list(pageLength = 5))

output$bas.p <- renderTable({
x <- Z()
res <- stat.desc(x)[1:3,]
rownames(res) = c("number.var", "number.null", "number.na")
return(res)
},   
width = "200px", rownames = TRUE, digits = 0)

output$des.p <- renderTable({
x <- Z()
res <- stat.desc(x)[4:14,]
return(res)
},   
width = "200px", rownames = TRUE)

output$nor.p <- renderTable({
x <- Z()
res <- stat.desc(x, norm = TRUE)[15:20,]
return(res)
},   
width = "200px", rownames = TRUE)

output$bp.p = renderPlot({
x = Z()
ggplot(x, aes(x = "", y = x[, 3])) + geom_boxplot(width = 0.2, outlier.colour = "red") + geom_jitter(width = 0.1, size = 1.5) + ylab("") + xlab("") + ggtitle("") + theme_minimal()
})

output$meanp.p = renderPlot({
x = Z()
des = data.frame(t(stat.desc(x)))
#p1 = ggplot(des, aes(x = rownames(des), y = mean, fill = rownames(des))) + 
#  geom_errorbar(width = .1, aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev),data = des) +
#  xlab("") + ylab(expression(Mean %+-% SD)) + geom_point(shape = 21, size = 3) + theme_minimal() + theme(legend.title = element_blank())

p2 = ggplot(des, aes(x = rownames(des), y = mean, fill = rownames(des))) + xlab("") + ylab(expression(Mean %+-% SD)) + geom_bar(position = position_dodge(),stat = "identity",width = 0.2,alpha = .3) +
geom_errorbar(width = .1,position = position_dodge(.9),aes(ymin = mean - des$std.dev, ymax = mean + des$std.dev),data = des) + theme_minimal() + theme(legend.title = element_blank())

grid.arrange(p2)
})

output$info3 <- renderText({
xy_str = function(e) {
if (is.null(e))
return("NULL\n")
paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal postion: ", "\n", xy_str(input$plot_click3))
})

output$makeplot.p <- renderPlot({
x <- Z()
plot1 <- ggplot(x, aes(sample = x[, 3])) + stat_qq() + ggtitle("Normal Q-Q Plot of the Mean Differences") + xlab("") + theme_minimal()  ## add line,
plot2 <- ggplot(x, aes(x = x[, 3])) + geom_histogram(colour = "black",fill = "grey", binwidth = input$bin.p, position = "identity") + xlab("") + ggtitle("Histogram") + theme_minimal() + theme(legend.title =element_blank())
plot3 <- ggplot(x, aes(x = x[, 3])) + geom_density() + ggtitle("Density Plot") + xlab("") + theme_minimal() + theme(legend.title = element_blank())

grid.arrange(plot1, plot2, plot3, ncol = 3)
})

output$t.test.p <- renderTable({
x <- Z()
res <-t.test(
x[, 1],
x[, 2],
data = x,
paired = TRUE,
alternative = input$alt.pt
)

res.table <- t(
data.frame(
T_statistic = res$statistic,
P_value = res$p.value,
Estimated_mean_diff = res$estimate,
Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4),")"),
Degree_of_freedom = res$parameter
)
)
colnames(res.table) <- res$method
return(res.table)
}, 
width = "500px", rownames = TRUE)


}

app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)

}
