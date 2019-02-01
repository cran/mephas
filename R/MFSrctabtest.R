##' MFSrctabtest function creates a dynamic calculator which enables users to do statistical tests on R by C data. Users can either input data manually or upload their dataset.
##'
##' This app includes statistical tests for R by C table, trend test for 2 by K table, and goodness-of-fit test.
##' Please click "close" window to quit the application. "psych" package is required.
##' @title MEPHAS Shiny Application of Contingency Table Related Tests
##' @return The shiny web page of the tests for cross tab data
##'
##' @import shiny
##' @import ggplot2
##'
##' @importFrom psych cohen.kappa
##' @importFrom stats prop.trend.test reshape

##' @examples
##' # library(mephas)
##' # MFSrctabtest()
##' # not run

##' @export
MFSrctabtest <- function(){

sink( tempfile() )

ui <- ui.rctab()

server <- server.rctab()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)

}

