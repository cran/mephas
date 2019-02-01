##----------#----------#----------#----------
##
## 7MFSreg UI
##
## Language: EN
## 
## DT: 2019-01-08
##
##----------#----------#----------#----------
##' @title UI of Univariate Regression

##' @export
ui.reg <- function(){

shinyUI(
tagList(
#source("../0tabs/font.R",local=TRUE, encoding="UTF-8")$value,

navbarPage(
title = "Regression Model",

#----------0. dataset panel----------

tabPanel("Dataset",

titlePanel("Data Preparation"),

#source("0data_ui.R", local=TRUE)
reg.data.ui()

    ),

#----------1. LM regression panel----------
tabPanel("Linear Regression (Continuous Outcomes)",

titlePanel("Linear Regression"),

#source("1lm_ui.R", local=TRUE)
reg.lm.ui()

), ## tabPanel

##-----------------------------------------------------------------------
## 2. logistic regression---------------------------------------------------------------------------------
tabPanel("Logistic Regression (1-0 Outcomes)",

titlePanel("Logistic Regression"),

#source("2lr_ui.R", local=TRUE)
reg.lr.ui()

), ## tabPanel(

##----------------------------------------------------------------------
## 3. cox regression---------------------------------------------------------------------------------
tabPanel("Cox Regression (Time-Event Outcomes)",

titlePanel("Cox Regression"),

#source("3cr_ui.R", local=TRUE)
reg.cr.ui()


) ## tabPanel(
,
##---------- other panels ----------

#source("../0tabs/home.R",local=TRUE)$value,
#source("../0tabs/stop.R",local=TRUE)$value
stop()
)
##-----------------------over
)
)
}

