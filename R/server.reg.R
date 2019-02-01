#if (!require(xtable)) {install.packages("xtable")}; library(xtable)
#if (!require(stargazer)) {install.packages("stargazer")}; library(stargazer)
#if (!require(ggfortify)) {install.packages("ggfortify")}; library(ggfortify)
#if (!require(plotROC)) {install.packages("plotROC")}; library(plotROC)
#if (!require(ROCR)) {install.packages("ROCR")}; library(ROCR)
#if (!require(survival)) {install.packages("survival")}; library(survival)
#if (!require(survminer)) {install.packages("survminer")}; library(survminer)
#if (!require(shiny)) {install.packages("shiny")}; library(shiny)
#if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
#if (!require(data.table)) {install.packages("data.table")}; library(data.table)
  
##----------#----------#----------#----------
##
## 7MFSreg SERVER
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------

# start server
##' @title SERVER of Univariate Regression

##' @export
server.reg <- function(){

load("./data/advertisement.rda")
load("./data/coloncancer.rda")
load("./data/insurance.rda")


shinyServer(

function(input, output, session) {

#----------0. dataset input----------
#source("MFSreg.data.server.R", local=TRUE) 
#MFSreg.data.server()

##----------#----------#----------#----------
##
## 7MFSreg SERVER
##
##    >data
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------

data <- reactive({
                switch(input$edata,
               "insurance" = insurance,
               "advertisement" = advertisement,
               "lung" = lung)
        })
X = eventReactive(input$choice,{
  inFile = input$file
  if (is.null(inFile)){
      df <- data() ##>  example data
    }
  else{
    df <- read.csv(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote)
  }
    return(df)
  })

X_var = eventReactive(input$choice,{
  inFile = input$file
  if (is.null(inFile)){
      df <- data() ##>  example data
    }
  else{
    df <- read.csv(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote)
  }
    vars <- names(df)
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    return(df)
  })

  output$data <- renderDataTable(
    head(X()), options = list(pageLength = 5, scrollX = TRUE))

  output$data_var <- renderDataTable(
    subset(X_var(), select = input$columns),
    options = list(pageLength = 5, scrollX = TRUE)
    )

# Basic Descriptives

output$cv = renderUI({
  selectInput(
    'cv', h5('Select continuous variables'),
    selected = NULL, choices = names(X()), multiple = TRUE)
})

output$dv = renderUI({
  selectInput(
    'dv', h5('Select categorical/discrete variables'), 
    selected = NULL, choices = names(X()), multiple = TRUE)
})

sum = eventReactive(input$Bc,  ##> cont var
        {
          pastecs::stat.desc(X()[, input$cv], desc = TRUE, norm=TRUE)
          #Hmisc::describe(X()[,input$cv])
        })
fsum = eventReactive(input$Bd, ##> dis var
       {
         data = as.data.frame(X()[, input$dv])
         colnames(data) = input$dv
         lapply(data, table)
       })

output$sum = renderTable({sum()}, rownames = TRUE)

output$fsum = renderPrint({fsum()})


# First Exploration of Variables

output$tx = renderUI({
  selectInput(
    'tx', h5('Variable in the x-axis'),
    selected = "NULL", 
    choices = c("NULL",names(X())))
  
  })

output$ty = renderUI({
  selectInput(
    'ty',
    h5('Variable in the y-axis'),
    selected = "NULL", 
    choices = c("NULL",names(X())))
  
})

## scatter plot
output$p1 = renderPlot({
   validate(
      need(input$tx != "NULL", "Please select one continuous variable")
    )
        validate(
      need(input$ty != "NULL", "Please select one continuous variable")
    )
  ggplot(X(), aes(x = X()[, input$tx], y = X()[, input$ty])) + geom_point(shape = 1) + 
    geom_smooth(method = lm) + xlab(input$tx) + ylab(input$ty) + theme_minimal()
  })

## histogram
output$hx = renderUI({
  selectInput(
    'hx',
    h5('Histogram of the continuous variable'),
    selected = "NULL", 
    choices = c("NULL",names(X())))
})

output$hxd = renderUI({
  selectInput(
    'hxd',
    h5('Histogram of the categorical/discrete variable'),
    selected = "NULL", 
    choices = c("NULL",names(X())))
})

output$p2 = renderPlot({
  validate(
      need(input$hx != "NULL", "Please select one continuous variable")
    )
  ggplot(X(), aes(x = X()[, input$hx])) + 
    geom_histogram(binwidth = input$bin, colour = "black",fill = "white") + 
    geom_density()+
    xlab("") + theme_minimal() + theme(legend.title = element_blank())
  })

output$p3 = renderPlot({
  validate(
      need(input$hxd != "NULL", "Please select one categorical/discrete variable")
    )
  ggplot(X(), aes(x = X()[, input$hxd])) + 
    geom_histogram(colour = "black",fill = "white",  stat="count") + 
    xlab("") + theme_minimal() + theme(legend.title = element_blank())
  })



#----------1. Linear regression----------
#source("MFSreg.lm.server.R", local=TRUE)
#MFSreg.lm.server()
##----------#----------#----------#----------
##
## 7MFSreg SERVER
##
##    >Linear regression
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------

## 2. choose variable to put in the model/ and summary
output$y = renderUI({
  selectInput(
    'y',
    h5('Continuous dependent variable (Y)'),
    selected = "NULL",
    choices = c("NULL", names(X()))
    )
  })

output$x = renderUI({
  selectInput(
    'x',
    h5('Continuous independent variable (X)'),
    selected = NULL,
    choices = names(X()),
    multiple = TRUE
    )
  })


output$fx = renderUI({
  selectInput(
    'fx',
    h5('Categorical/discrete independent variable (X)'),
    selected = NULL,
    choices = names(X()),
    multiple = TRUE
  )
})

### for summary


##3. regression formula
formula = eventReactive(input$F, {
  if (is.null(input$fx)) {
    fm = as.formula(paste0(
      "as.numeric(",
      input$y,
      ')~',
      paste0("as.numeric(",input$x, ")",collapse = "+"),
      input$conf,
      input$intercept
    ))
  }
  else{
    fm = as.formula(paste0(
      "as.numeric(",
      input$y,
      ')~',
      paste0("as.numeric(", input$x, ")",collapse = "+"),
      paste0("+ as.factor(", input$fx, ")", collapse = ""),
      input$conf,
      input$intercept
    ))
  }
  return(fm)
})

output$formula = renderPrint({formula()})

## 4. output results
### 4.2. model
fit = eventReactive(input$B1, {
  lm(formula(), data = X())
  })

sp = eventReactive(input$B1, {step(lm(formula(),  data = X()))})

#gfit = eventReactive(input$B1, {
#  glm(formula(), data = X())
#})
afit = eventReactive(input$B1, {anova(lm(formula(),  data = X()))})

output$fit = renderUI({
  #xtable(summary(gfit()), auto = TRUE)
  #list(Model = summary(fit()), AIC = summary(gfit())$aic)
  HTML(
    stargazer::stargazer(
      fit(),
      type = "html",
      style = "all",
      align = TRUE,
      ci = TRUE,
      single.row = TRUE,
      model.names = TRUE,
      header = FALSE

      )
    )
  })

output$anova = renderTable({xtable::xtable(afit())}, rownames = TRUE)
output$step = renderPrint({sp()})

# residual plot
output$p.lm = renderPlot({autoplot(fit(), which = as.numeric(input$num)) + theme_minimal()})



output$fitdt0 = renderDataTable({
  data.frame(
    Linear.redictors = round(predict(fit()), 4),
    Residuals = round(fit()$residuals, 4)
    )
  }, 
  options = list(pageLength = 5, scrollX = TRUE))

newX = reactive({
  inFile = input$newfile
  if (is.null(inFile))
  {
    df = X()[1:10, ] ##>  example data
    }
  else{
    df = read.csv(
      inFile$datapath,
      header = input$newheader,
      sep = input$newsep,
      quote = input$newquote
      )
    }
  return(df)
  })
#prediction plot
# prediction
pred = eventReactive(input$B2,
       {
         fit = lm(formula(), data = X())
         pfit = predict(fit, newdata = newX(), interval = input$interval)
         })

output$pred = renderDataTable({
  cbind(newX(), round(pred(), 4))
  }, 
  options = list(pageLength = 10, scrollX = TRUE))

output$px = renderUI({
  selectInput(
    'px',
    h5('Choose one independent Variable (X)'),
    selected = NULL,
    choices = names(newX())
  )
  })

#----------2. Logistic regression----------
#source("MFSreg.lr.server.R", local=TRUE)
#MFSreg.lr.server()
##----------#----------#----------#----------
##
## 7MFSreg SERVER
##
##    >Logistic regression
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------



## 2. choose variable to put in the model
output$y.l = renderUI({
  selectInput(
    'y.l',
    h5('Binary dependent Variable (Y)'),
    selected = "NULL",
    choices = c("NULL", names(X()))
    )
  })

output$x.l = renderUI({
  selectInput(
    'x.l',
    h5('Continuous independent variable (X)'),
    selected = NULL,
    choices = names(X()),
    multiple = TRUE
    )
  })

output$fx.l = renderUI({
  selectInput(
    'fx.l',
    h5('Categorical independent variable (X)'),
    selected = NULL,
    choices = names(X()),
    multiple = TRUE
    )
  })

# 3. regression formula
formula_l = eventReactive(input$F.l, {
  if (is.null(input$fx.l)) {
    fm = as.formula(paste0(
      "as.numeric(",
      input$y.l,
      ')~',
      paste0("as.numeric(", input$x.l, ")", collapse = "+"),
      input$conf.l,
      input$intercept.l
    ))
  }
  else{
    fm = as.formula(paste0(
      "as.numeric(",
      input$y.l,
      ')~',
      paste0("as.numeric(", input$x.l, ")",collapse = "+"),
      paste0("+ as.factor(", input$fx.l, ")", collapse = ""),
      input$conf.l,
      input$intercept.l
    ))
  }
  return(fm)
})

output$formula_l = renderPrint({
  formula_l()
})

### 4.2. model
fit.l = eventReactive(input$B1.l,
                      {
                        glm(formula_l(),
                            family = binomial(link = "logit"),
                            data = X())
                      })
output$fit.l = renderUI({
  HTML(
    stargazer::stargazer(
      fit.l(),
      type = "html",
      style = "all",
      align = TRUE,
      ci = TRUE,
      single.row = TRUE,
      model.names = TRUE
    )
  )
})
output$anova.l = renderTable({
  xtable::xtable(anova(fit.l()))
}, rownames = TRUE)

output$step.l = renderPrint({
  step(fit.l()) })


# ROC plot
fitdf = reactive({
  df = data.frame(
    fit.prob = round(fit.l()$fitted.values, 2),
    fit.value = ifelse(fit.l()$fitted.values > 0.5, 1, 0)
  )
  return(df)
})
output$fitdt = renderDataTable({
  fitdf()
}, options = list(pageLength = 5, scrollX = TRUE))

output$p2.l = renderPlot({
  df = data.frame(predictor = fit.l()$fitted.values,
                  y = X()[, input$y.l])
  ggplot(df, aes(d = df[,"y"], m = df[,"predictor"], model = NULL)) + geom_roc(n.cuts = 0) + theme_minimal()
})

output$auc = renderPrint({
  mis = mean(fitdf()$fit.value != X()[, input$y.l])
  auc = performance(prediction(fitdf()$fit.prob, X()[, input$y.l]), measure = "auc")
  list(Accuracy = 1 - mis, AUC = auc@y.values[[1]])
})

newX.l = reactive({
  inFile = input$newfile.l
  if (is.null(inFile))
  {
    df = X()[1:10, ] ##>  example data
  }
  else{
    df = read.csv(
      inFile$datapath,
      header = input$newheader.l,
      sep = input$newsep.l,
      quote = input$newquote.l
      )
    }
  return(df)
  
})
# prediction part
# prediction
pred.l = eventReactive(input$B2.l,
                       {
                         fit.l = glm(formula_l(),
                                     family = binomial(link = "logit"),
                                     data = X())
                         predict(fit.l, newdata = newX.l(), type = "response")
                       })
pred.v = eventReactive(input$B2.l,
                       {
                         ifelse(pred.l() > 0.5, 1, 0)
                       })
output$preddt.l = renderDataTable({
  data.frame(newX.l(), fit.prob = round(pred.l(), 4), fit = pred.v())
}, options = list(pageLength = 5, scrollX = TRUE))





#----------3. Cox regression----------
#source("MFSreg.cr.server.R", local=TRUE)
#MFSreg.cr.server()
##----------#----------#----------#----------
##
## 7MFSreg SERVER
##
##    >Cox regression
##
## Language: EN
## 
## DT: 2019-01-11
##
##----------#----------#----------#----------


### testing data
newX.c = reactive({
inFile = input$newfile.c
if (is.null(inFile))
{
  df = X()[1:10, ]
  
}
else{
  df = read.csv(
    # user data
    inFile$datapath,
    header = input$newheader.c,
    sep = input$newsep.c,
    quote = input$newquote.c
  )
}
return(df)
})

## 2. choose variable to put in the model
output$t1.c = renderUI({
selectInput(
  't1.c',
  h5('Continuous follow-up time (or start-up time-point)'),
  selected = "NULL",
  choices = c("NULL", names(X()))
)
})

output$t2.c = renderUI({
selectInput(
  't2.c',
  h5('NULL (or end-up time-point)'),
  selected = "NULL",
  choices = c("NULL", names(X()))
)
})

output$c.c = renderUI({
selectInput('c.c',
            h5('Status variable (0=censor, 1=event)'),
            selected = "NULL",
            choices = c("NULL", names(X()))
            )
})

output$x.c = renderUI({
selectInput(
  'x.c',
  h5('Continuous independent variable'),
  selected = NULL,
  choices = names(X()),
  multiple = TRUE
)
})

output$fx.c = renderUI({
selectInput(
  'fx.c',
  h5('Categorical independent variable'),
  selected = NULL,
  choices = names(X()),
  multiple = TRUE
)
})

output$sx.c = renderUI({
selectInput(
  'sx.c',
  h5('Stratified variable'),
  selected = NULL,
  choices = names(X()),
  multiple = TRUE
)
})

output$clx.c = renderUI({
selectInput(
  'clx.c',
  h5('Cluster variable'),
  selected = NULL,
  choices = names(X()),
  multiple = TRUE
)
})


# 3. regression formula
y = reactive({
if (input$t2.c == "NULL") {
  y = paste0("Surv(as.numeric(", input$t1.c, "),as.numeric(", input$c.c, "))")
}
else{
  y = paste0("Surv(as.numeric(", input$t1.c, "),as.numeric(", input$t2.c, "),as.numeric(", input$c.c, "))")
}
return(y)
})

formula_c = eventReactive(input$F.c, {
f1 = paste0(y(), '~', paste0(input$x.c, collapse = "+"), input$conf.c)

f2 = paste0(f1, "+ as.factor(", input$fx.c, ")")
f3 = paste0(f1, "+ strata(",    input$sx.c, ")")
f4 = paste0(f1, "+ cluster(",   input$clx.c, ")")

f5 = paste0(f1,
            "+ as.factor(",
            input$fx.c,
            ")",
            "+ strata(",
            input$sx.c,
            ")")
f6 = paste0(f1,
            "+ as.factor(",
            input$fx.c,
            ")",
            "+ cluster(",
            input$clx.c,
            ")")
f7 = paste0(f1, "+ strata(",  input$sx.c, ")", "+ cluster(", input$clx.c, ")")

f8 = paste0(
  f1,
  "+ as.factor(",
  input$fx.c,
  ")",
  "+ strata(",
  input$sx.c,
  ")",
  "+ cluster(",
  input$clx.c,
  ")"
)

if (is.null(input$fx.c) &&
    is.null(input$sx.c) && is.null(input$clx.c))
{
  f = as.formula(f1)
}
if (is.null(input$fx.c) &&
    is.null(input$sx.c) && (!is.null(input$clx.c)))
{
  f = as.formula(f4)
}

if (is.null(input$fx.c) &&
    !is.null(input$sx.c) && is.null(input$clx.c))
{
  f = as.formula(f3)
}
if (is.null(input$fx.c) &&
    !is.null(input$sx.c) && !is.null(input$clx.c))
{
  f = as.formula(f7)
}

if (!is.null(input$fx.c) &&
    is.null(input$sx.c) && is.null(input$clx.c))
{
  f = as.formula(f2)
}
if (!is.null(input$fx.c) &&
    is.null(input$sx.c) && !is.null(input$clx.c))
{
  f = as.formula(f6)
}

if (!is.null(input$fx.c) &&
    !is.null(input$sx.c) && is.null(input$clx.c))
{
  f = as.formula(f5)
}
if (!is.null(input$fx.c) &&
    !is.null(input$sx.c) && !is.null(input$clx.c))
{
  f = as.formula(f8)
}

return(f)
})


output$formula_c = renderPrint({
formula_c()
})

## 4. output results
### 4.1. variables' summary

### 4.2. model
fit.c = eventReactive(input$B1.c,
{
coxph(formula_c(), data = X())
})
output$fit.c = renderUI({
HTML(
  stargazer::stargazer(
    fit.c(),
    type = "html",
    style = "all",
    align = TRUE,
    ci = TRUE,
    single.row = TRUE,
    model.names = TRUE
  )
)
})
output$anova.c = renderTable({
xtable::xtable(anova(fit.c()))
}, rownames = TRUE)

output$step.c = renderPrint({
step(fit.c()) })


# K-M plot
y.c = eventReactive(input$Y.c,
{
y=y()
})
output$p0.c = renderPlot({
f = as.formula(paste0(y.c(), "~1"))
fit = surv_fit(f, data = X())
ggsurvplot(fit, data = X(), risk.table = TRUE)
#plot(fit)
})
output$tx.c = renderUI({
selectInput(
  'tx.c',
  h5('Categorical variable as group'),
  selected = "NULL",
  choices = c("NULL",names(X()))
)
})
output$p1.c = renderPlot({
validate(
  need(input$tx.c != "NULL", "Please select one group variable")
)
f = as.formula(paste0(y.c(), "~", "as.factor(",input$tx.c, ")"))
fit = surv_fit(f, data = X())

ggsurvplot(fit,
           data = X(),
           risk.table = TRUE,
           pval = TRUE)
#plot(fit)
})

# coxzph plot
zph = eventReactive(input$B1.c, {
cox.zph(fit.c())
})
output$zph.c = renderTable({
as.data.frame(zph()$table)
}, rownames=TRUE)

output$p2.c = renderPlot({
#ggcoxzph(zph())+ggtitle("")
#p1= ggcoxdiagnostics(fit.c(), type = "schoenfeld") + theme_minimal()
ggcoxdiagnostics(fit.c(), type = "schoenfeld", ox.scale = "time") + theme_minimal()
#grid.arrange( p1,p2, ncol=2)
})

# Residual output

output$p4.c = renderPlot({
if (input$res.c=="martingale")
{ggcoxdiagnostics(fit.c(), type = "martingale") + theme_minimal()}
else if (input$res.c=="deviance")
{ggcoxdiagnostics(fit.c(), type = "deviance") + theme_minimal()}
else
{
cox.snell = (as.numeric(X()[, input$c.c])) - residuals(fit.c(), type = "martingale")
coxph.res = survfit(coxph(Surv(cox.snell, X()[, input$c.c]) ~ 1, method = 'breslow'), type = 'aalen')
d = data.frame(x = as.numeric(coxph.res$time), y = -log(coxph.res$surv))
ggplot() + geom_step(data = d, mapping = aes(x = d[,"x"], y = d[,"y"])) + 
  geom_abline(intercept =0,slope = 1, color = "red") +
  theme_minimal() + xlab("Modified Cox-Snell residuals") + ylab("Cumulative hazard")
}
})

output$fitdt.c = renderDataTable({
data.frame(
  Residual = round(fit.c()$residuals, 4),
  Linear.predictors = round(fit.c()$linear.predictors, 4)
)
}, options = list(pageLength = 5, scrollX = TRUE))

#prediction plot
# prediction
pfit.c = eventReactive(input$B2.c, 
{coxph(formula_c(), data = X())}
)

output$pred.c = renderDataTable({
df = data.frame(
  risk = predict(pfit.c(), newdata = newX.c(), type = "risk"),
  #survival=predict(fit.c(), newdata=newX.c(), type="survival"),
  #expected=predict(fit.c(), newdata=newX.c(), type="expected"),
  linear.predictors = predict(pfit.c(), newdata = newX.c(), type = "lp")
)
cbind(newX.c(), round(df, 4))
}, options = list(pageLength = 5, scrollX = TRUE))



#---------------------------##
observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })
})

}


