##' MFSproptest function creates a dynamic calculator which enables users to do statistical tests on binomial proportional data. Users can either input data manually or upload their dataset.
##'
##' This app includes statistical tests for one single proportion, two proportions from independent groups, and two proportions from the matched data. Results include the corresponding test's outcome and pie-plot of data.
##' Please click "close" window to quit the application. "gridExtra", "reshape", and "pastecs" packages are required.
##' @title MEPHAS Shiny application of the binomial proportional data tests
##' @return The shiny web page of the tests based on binomial proportion

##' @import shiny
##' @import ggplot2
##'
##' @importFrom gridExtra grid.arrange
##' @importFrom reshape melt
##' @importFrom pastecs stat.desc
##' @importFrom stats addmargins binom.test chisq.test fisher.test mcnemar.test prop.test

##' @examples
##' # library(mephas)
##' # MFSproptest()
##' # not run

##' @export
MFSproptest <- function(){

sink( tempfile() )

ui <- ui.proptest()

server <- server.proptest()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)

}

