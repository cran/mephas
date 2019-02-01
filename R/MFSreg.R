##' MFSreg function creates a dynamic calculator which instructs users to do linear regression, logistic regression, and cox regression. Users can either input data manually or upload their dataset.
##'
##' This app includes linear regression, logistic regression, and cox regression. The results include ANOVA table, descriptive statistics, and residual plots.
##' Please click "close" window to quit the application. "survival", "survminer", and "ggfortify" packages may be required.
##' @title MEPHAS Shiny Application of Univariate Regression
##' @return The shiny web page of the tests univariate regression
##'
##' @import shiny
##' @import ggplot2
##' @import survival
##' @import survminer
##' @import ggfortify
##'
##' @importFrom xtable xtable
##' @importFrom stargazer stargazer
##' @importFrom ROCR performance prediction
##' @importFrom plotROC geom_roc
##' @importFrom stats anova as.formula binomial glm lm predict residuals step biplot

##' @examples
##' # library(mephas)
##' # MFSreg()
##' # not run

##' @export
MFSreg <- function(){

sink( tempfile() )

ui <- ui.reg()

server <- server.reg()


app <- shinyApp(ui = ui, server = server)
runApp(app, quiet = TRUE)

}

