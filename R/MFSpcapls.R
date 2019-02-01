
##' MFSpcapls function creates a dynamic calculator which enables users to do PCA, PLS, sparse PLS regression. Users can either input data manually or upload their dataset.
##'
##' This app includes one-way ANOVA, two-way ANOVA, and multiple comparison. The results include ANOVA table, descriptive statistics, and bar-plot of the group's mean value.
##' Please click "close" window to quit the application. "mixOmics" package may be required.
##' @title MEPHAS Shiny Application of PCA PLS Regression
##' @return The shiny web page of the PCA PLS regression
##'
##' @import shiny
##' @import ggplot2
##'
##' @importFrom mixOmics pls spls plotIndiv plotVar plotLoadings


##' @examples
##' # library(mephas)
##' # MFSpcapls()
##' # not run

##' @export
MFSpcapls <- function(){
# options(warn = -1)
sink( tempfile() )

ui <- ui.pcapls()

server <- server.pcapls()

app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE, quiet = TRUE)

}

