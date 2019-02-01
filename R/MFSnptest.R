##' MFSnptest function creates a dynamic calculator which enables users to do non-parametric statistical tests. Users can either input data manually or upload their dataset.
##'
##' This app includes non-parametric tests for one sample, two samples, and two paired samples. The results include box-plots, histogram, descriptive statistics,and test results.
##' Please click "close" window to quit the application. "gridExtra", "reshape", "pastecs", "DescTools", and "RVAideMemoire" packages may be required.
##' @title MEPHAS Shiny Application of the Non-paramatric Tests
##' @return The shiny web page of non-parametric statistical test

##' @import shiny
##' @import ggplot2

##' @importFrom gridExtra grid.arrange
##' @importFrom reshape melt
##' @importFrom pastecs stat.desc
##' @importFrom DescTools SignTest
##' @importFrom RVAideMemoire mood.medtest
##' @importFrom stats wilcox.test

##' @examples
##' # library(mephas)
##' # MFSn()
##' # not run

##' @export
MFSnptest <- function(){
sink( tempfile() )

ui <- ui.nptest()

server <- server.nptest()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser=TRUE)
}

