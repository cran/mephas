##' MFSdistribution function creates a dynamic calculator which shows the commonly used probability distribution. Users can change the parameter to change the shape of distribution.
##'
##' This app includes the distributions for continuous variables (normal distribution, exponential distribution, and gamma distribution), for discrete variables (binomial distribution and Poisson distribution), and distributions derived from normal distribution (t distribution, chi-square distribution, and F distribution).
##' Please click "close" window to quit the application. No special packages are required.
##' @title MEPHAS Shiny application of the Statistical Distributions
##' @return The shiny web page of the statistical distribution
##'
##' @import shiny
##' @import ggplot2
##' @importFrom stats dchisq dnorm dt pbinom pnorm ppois qchisq qexp qf qgamma qnorm qt quantile rchisq rexp rf rgamma rnorm rt sd var
##' @importFrom utils head

##' @examples
##' # library(mephas)
##' # MFSdist()

##' @export
MFSdistribution<- function(){

sink( tempfile() )

ui <- ui.dist()
server <- server.dist()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE, quiet = TRUE)

}

