##' MFSanova function creates a dynamic calculator which enables users to do ANOVA analysis and multiple comparison. Users can either input data manually or upload their dataset.
##'
##' This app includes one-way ANOVA, two-way ANOVA, and multiple comparison. The results include ANOVA table, descriptive statistics, and bar-plot of the group's mean value.
##' Please click "close" window to quit the application. "Rmisc" and "psych" packages are required.
##'
##' @title MEPHAS Shiny Application of ANOVA and Multiple Comparison
##' @return The shiny app and web page of the ANOVA and multiple comparison
##'
##' @import shiny
##' @import ggplot2
##'
##' @importFrom psych describeBy
##' @importFrom Rmisc summarySE
##' @importFrom graphics plot
##' @importFrom utils head read.csv write.csv
##' @importFrom stats TukeyHSD aov pairwise.t.test

##' @examples
##' # library(mephas)
##' # MFSanova()

##' @export
MFSanova <- function(){

sink( tempfile() )

ui <- ui.anova()

server <- server.anova()

app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)

}

