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

sink( tempfile() )

ui <- ui.t()

server <- server.t()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)

}
