##' MFSnptest function creates a dynamic calculator which enables users to do non-parametric statistical tests. Users can either input data manually or upload their dataset.
##'
##' This app includes non-parametric tests for one sample, two samples, and two paired samples. The results include box-plots, histogram, descriptive statistics,and test results.
##' Please click "close" window to quit the application. "gridExtra", "reshape", "pastecs", "DescTools", and "RVAideMemoire" packages may be required.
##' @title MEPHAS Shiny Application of the Non-paramatric Tests
##' @return The shiny web page of non-parametric statistical test

##' @import shiny
##' @import ggplot2
##'
##' @importFrom gridExtra grid.arrange
##' @importFrom reshape melt
##' @importFrom pastecs stat.desc
##' @importFrom DescTools SignTest
##' @importFrom stats wilcox.test

##' @examples
##' # library(mephas)
##' # MFSn()
##' # not run

##' @export
MFSnptest <- function(){
sink( tempfile() )

ui <- tagList(
#source("../0tabs/font.R",local=TRUE, encoding="UTF-8")$value,

navbarPage(

title = "Non-parametric Test",

##---------- Panel 1 ----------
tabPanel("One Sample",

headerPanel("Sign Test, Wilcoxon Signed-Rank Test"),

HTML("

<b> Notations </b>

<ul>
<li> X is the randomly collected sample
<li> m is the population median of X, meaning the 50 percentile of the underlying distribution of the X
<li> m&#8320 is the specified value
</ul>


<b> Assumptions </b>

<ul>
<li>Each observation is independent and comes from the same population
<li>X could be continuous (i.e., interval or ratio) and ordinal
</ul>


<p> Both can be used to test whether the median of a collection of numbers is significantly greater than or less than a specified value. </p>

"),
hr(),

##---------- 1.1 ----------
nptest.onesample(),
hr(),

##---------- 1.2 ----------

h4("Sign Test"),
p("The sign test makes very few assumptions about the nature of the distributions under test, but may lack the statistical power of the alternative tests."),

nptest.signtest(),
hr(),

##---------- 1.3 ----------

h4("Wilcoxon Signed-Rank Test"),

HTML("

<p> Alternative to one-sample t-test when the data cannot be assumed to be normally distributed. It is used to determine whether the median of the sample is equal to a specified value.</p>

<b> Supplementary Assumptions </b>

<ul>
<li> The distribution of X is symmetric
<li> No ties (same values) in X
</ul>

"),

nptest.wstest()

),

##---------- Panel 2 ----------

tabPanel("Two Independent Samples",

headerPanel("Wilcoxon Rank-Sum Test (Mann-Whitney U Test), Mood's Median Test"),

HTML("

<p> To determine whether a randomly selected sample will be less than or greater than a second randomly selected sample. </p>

<b> Notations </b>
<ul>
<li> X is the first randomly selected sample, while Y is the second</li>
<li> m&#8321 is the population median of X, or the 50 percentile of the underlying distribution of X </li>
<li> m&#8322 is the population median of Y, or the 50 percentile of the underlying distribution of Y </li>
</ul>

<b> Assumptions </b>
<ul>
<li> All the observations from both groups are independent of each other, no paired or repeated data </li>
<li> X and Y could be continuous (i.e., interval or ratio) and ordinal (i.e., at least, of any two observations, which is the greater) </li>
<li> X and Y are similar in distribution's shape </li>
</ul>

"),
hr(),

##---------- 2.1 ----------
nptest.twosample(),
hr(),

##---------- 2.2 ----------
h4("Wilcoxon Rank-Sum Test, Mann-Whitney U Test, Mann-Whitney-Wilcoxon Test, Wilcoxon-Mann-Whitney Test"),

HTML("

<p> Not require the assumption of normal distributions; nearly as efficient as the t-test on normal distributions. </p>

<b> Supplementary Assumptions  </b>

<ul>
<li> No outliers (to determine if the distributions of the two groups are similar in shape and spread)
<li> If outliers exist, the test is used for testing distributions (to determine if the distributions of the two groups are different in shape and spread)
</ul>

<p> Outliers will affect the spread of data  </p>
"),

nptest.wrtest()

##---------- 2.3 ----------
#h4("Mood's Median Test"),

#p("A special case of Pearson's chi-squared test. It has low power (efficiency) for moderate to large sample sizes. "),

#nptest.mmtest()

),

##---------- Panel 3 ----------

tabPanel("Two Paired Samples",

headerPanel("Sign Test, Wilcoxon Signed-Rank Test"),

HTML("

<b> Assumptions </b>

<ul>
<li> The observations of (X, Y) are paired and come from the same population
<li> X's and Y's could be continuous (i.e., interval or ratio) and ordinal
<li> D's are independent and come from the same population
</ul>

<b> Notations </b>

<ul>
<li> The paired observations are designated X and Y ,
<li> D = X-Y, the differences between paired (X, Y)
<li> m is the population median of D, or the 50 percentile of the underlying distribution of the D.
</ul>

<p> Given pairs of observations (such as weight pre- and post-treatment) for each subject, both test determine if one of the pair (such as pre-treatment) tends to be greater than (or less than) the other pair (such as post-treatment).</p>

"),

nptest.psample(),

hr(),
##---------- 3.1 ----------

h4("Sign Test"),
p("The sign test makes very few assumptions about the nature of the distributions under test, but may lack the statistical power of the alternative tests."),

nptest.signtest.p(),

hr(),
##---------- 3.2 ----------

h4("Wilcoxon Signed-Rank Test"),

HTML("

<p> An alternative to the paired t-test for matched pairs, when the population cannot be assumed to be normally distributed. It can also be used to determine whether two dependent samples were selected from populations having the same distribution. </p>

<b> Supplementary Assumptions </b>

<ul>
<li> The distribution of D's is symmetric
<li> No ties in D's
</ul>

"),

helpText("Ties means the same values"),

nptest.wstest.p()

)#,
##---------- other panels ----------

#source("../0tabs/home.R",local=TRUE,encoding = "UTF-8")$value,
#source("../0tabs/stop.R",local=TRUE,encoding = "UTF-8")$value
#stop()

))


server <- function(input, output) {

##---------- 1. one sample ----------
A <- reactive({
inFile <- input$file
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$a, "[\n, \t, ]")))
x <- data.frame(X =X)
names(x) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(x)}
else {
  csv <- as.data.frame(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote))
  return(csv)} })

#table 
output$table <- renderDataTable({A()}, options = list(pageLength = 5))

output$bas <- renderTable({  
X <- as.numeric(unlist(strsplit(input$a, "[\n, \t, ]")))
res <- stat.desc(X)[1:3]
names(res) = c("number.var", "number.null", "number.na")
#names(res) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)},   width = "200px", rownames = TRUE, digits = 0)

output$des <- renderTable({  
X <- as.numeric(unlist(strsplit(input$a, "[\n, \t, ]")))
res <- stat.desc(X)[4:14 ]
#names = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)},   width = "200px", rownames = TRUE)

output$nor <- renderTable({  
X <- as.numeric(unlist(strsplit(input$a, "[\n, \t, ]")))
res <- stat.desc(X, norm = TRUE)[15:20]
#names(res) = unlist(strsplit(input$cn, "[\n, \t, ]"))
return(res)},   width = "200px", rownames = TRUE)

#plot
output$bp = renderPlot({
x = A()
ggplot(x, aes(x = "", y = x[,1])) + geom_boxplot(width = 0.2, outlier.colour = "red", outlier.size = 2) + geom_jitter(width = 0.1, size = 1.5) + ylab("") + xlab("") + ggtitle("") + theme_minimal()+ theme(legend.title=element_blank())}) 

output$info <- renderText({
xy_str = function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal position: ", "\n", xy_str(input$plot_click))})

output$makeplot <- renderPlot({  #shinysession 
x <- A()
plot1 <- ggplot(x, aes(x = x[,1])) + geom_histogram(colour="black", fill = "grey", binwidth=input$bin, position="identity") + xlab("") + ggtitle("Histogram") + theme_minimal() + theme(legend.title=element_blank())
plot2 <- ggplot(x, aes(x = x[,1])) + geom_density() + ggtitle("Density Plot") + xlab("") + theme_minimal() + theme(legend.title=element_blank())
grid.arrange(plot1, plot2, ncol=2)  })

output$sign.test<-renderTable({
x <- A()
res <- SignTest(as.vector(x[,1]), mu = input$med, alternative = input$alt.st)
res.table <- t(data.frame(S_statistic = res$statistic,
                          P_value = res$p.value,
                          Estimated_median = res$estimate,
                          Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
colnames(res.table) <- res$method
return(res.table)}, width = "500px", rownames = TRUE)

output$ws.test<-renderTable({
x <- A()
res <- wilcox.test(as.vector(x[,1]), mu = input$med, alternative = input$alt.wsr, correct = input$nap.wsr, conf.int = TRUE, conf.level = 0.95)
res.table <- t(data.frame(Z_statistic = res$statistic,
                          P_value = res$p.value,
                          Estimated_median = res$estimate,
                          Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
colnames(res.table) <- res$method
return(res.table)}, width = "500px", rownames = TRUE)

##---------- 2. two samples ----------

B <- reactive({
inFile <- input$file2
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$x1, "[\n, \t, ]")))
Y <- as.numeric(unlist(strsplit(input$x2, "[\n, \t, ]")))
x <- data.frame(X =X, Y = Y)
names(x) = unlist(strsplit(input$cn2, "[\n, \t, ]"))
return(x)}
else {
  csv <- as.data.frame(read.csv(inFile$datapath, header=input$header2, sep=input$sep2))
  return(csv)}
   })

#table
output$table2 <- renderDataTable({B()}, options = list(pageLength = 5))

output$bas2 <- renderTable({  ## don't use renerPrint to do renderTable
x <- B()
res <- stat.desc(x)[1:3, ]
rownames(res) = c("number.var", "number.null", "number.na")
return(res)},   width = "200px", rownames = TRUE, digits = 0)

output$des2 <- renderTable({  
x <- B()
res <- stat.desc(x)[4:14, ]
return(res)},   width = "200px", rownames = TRUE)

output$nor2 <- renderTable({  
x <- B()
res <- stat.desc(x, norm = TRUE)[15:20, ]
return(res)},   width = "200px", rownames = TRUE)

#plot
output$bp2 = renderPlot({
x <- B()
mx <- melt(B(), idvar = colnames(x))
ggplot(mx, aes(x = "variable", y = "value", fill="variable")) + geom_boxplot(alpha=.3, width = 0.2, outlier.color = "red", outlier.size = 2) + geom_jitter(width = 0.1, size = 1.5) + ylab("") + ggtitle("") + theme_minimal() + theme(legend.title=element_blank()) })

output$info2 <- renderText({
xy_str = function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal position: ", "\n", xy_str(input$plot_click2))})

output$makeplot2 <- renderPlot({
x <- B()
mx <- melt(B(), idvar = colnames(x))
# density plot
plot1 <- ggplot(mx, aes(x=mx[,"value"], fill=mx[,"variable"])) + geom_histogram(binwidth=input$bin2, alpha=.5, position="identity") + ylab("") + ggtitle("") + theme_minimal()+ theme(legend.title=element_blank())
plot2 <- ggplot(mx, aes(x=mx[,"value"], colour=mx[,"variable"])) + geom_density()+ ylab("") + ggtitle("") + theme_minimal()+ theme(legend.title=element_blank())
grid.arrange(plot1, plot2, ncol=2)  })

#test
output$mwu.test<-renderTable({
x <- B()
res <- wilcox.test(x[,1], x[,2], paired = FALSE, alternative = input$alt.mwt, correct = input$nap.mwt, conf.int = TRUE)
res.table <- t(data.frame(W_statistic = res$statistic, P_value = res$p.value, Estimated_diff = res$estimate,
                          Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
colnames(res.table) <- c(res$method)
return(res.table)}, width = "500px", rownames = TRUE)

#output$mood.test<-renderTable({
#X <- B()[,1]; Y <- B()[,2]
#data <- data.frame(value = c(X, Y), group = c(rep(1, length(X)), rep(2, length(Y))))
#res <- mood.medtest(value~group, data = data, alternative = input$alt.md)
#res.table <- t(data.frame(P_value = res$p.value))
#colnames(res.table) <- c(res$method)
#return(res.table)}, width = "500px", rownames = TRUE)

##---------- 3. paired sample ----------

C <- reactive({
inFile <- input$file3
if (is.null(inFile)) {
X <- as.numeric(unlist(strsplit(input$y1, "[\n, \t, ]")))
Y <- as.numeric(unlist(strsplit(input$y2, "[\n, \t, ]")))
d <- X-Y
x <- data.frame(X =X, Y = Y, diff = d)
names(x) = unlist(strsplit(input$cn3, "[\n, \t, ]"))
return(x)}
else {
  csv <- as.data.frame(read.csv(inFile$datapath, header=input$header3, sep=input$sep3))
  csv$diff <- round(csv[, 1] - csv[, 2],4)
  return(csv)} })

#table
output$table3 <- renderDataTable({C()}, options = list(pageLength = 5))

output$bas3 <- renderTable({  ## don't use renerPrint to do renderTable
x <- C()
res <- stat.desc(x)[1:3, ]
rownames(res) = c("number.var", "number.null", "number.na")
return(res)},   width = "200px", rownames = TRUE, digits = 0)

output$des3 <- renderTable({  
x <- C()
res <- stat.desc(x)[4:14, ]
return(res)},   width = "200px", rownames = TRUE)

output$nor3 <- renderTable({  
x <- C()
res <- stat.desc(x, norm = TRUE)[15:20, ]
return(res)},   width = "200px", rownames = TRUE)

# plots
output$bp3 = renderPlot({
x <- C()
mx <- melt(C(), idvar = colnames(x))
ggplot(mx, aes(x = "variable", y = "value", fill="variable")) + geom_boxplot(alpha=.3, width = 0.2, outlier.color = "red", outlier.size = 2) + geom_jitter(width = 0.1, size = 1.5) + ylab("") + ggtitle("") + theme_minimal() + theme(legend.title=element_blank()) })

output$info3 <- renderText({
xy_str = function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("The approximate value: ", round(e$y, 4))
}
paste0("Horizontal position: ", "\n", xy_str(input$plot_click3))})

output$makeplot3 <- renderPlot({
x <- C()
plot1 <- ggplot(x, aes(x=x[,3])) + geom_histogram(binwidth=.3, alpha=.5, position="identity") + ylab("Frequncy") + xlab("") +  ggtitle("Histogram") + theme_minimal() + theme(legend.title=element_blank())
plot2 <- ggplot(x, aes(x=x[,3])) + geom_density() + ggtitle("Density Plot") + theme_minimal() + ylab("Density") + xlab("") + theme(legend.title=element_blank())
grid.arrange(plot1, plot2, ncol=2)  })


output$psr.test <- renderTable({
x <- C()
res <- wilcox.test(x[,1], x[,2], paired = TRUE, alternative = input$alt.pwsr, correct = input$nap, conf.int = TRUE)
res.table <- t(data.frame(W_statistic = res$statistic,
                          P_value = res$p.value,
                          Estimated_diff = res$estimate,
                          Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
colnames(res.table) <- c(res$method)
return(res.table)}, width = "500px", rownames = TRUE)

output$psign.test <- renderTable({
x <- C()
res <- SignTest(x[,1], x[,2], alternative = input$alt.ps)
res.table <- t(data.frame(S_statistic = res$statistic,
                          P_value = res$p.value,
                          Estimated_diff = res$estimate,
                          Confidence_interval_0.95 = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
colnames(res.table) <- res$method
return(res.table)}, width = "500px", rownames = TRUE)

}

app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)
}

