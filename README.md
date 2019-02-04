# mephas 

**R package: Medical and Pharmaceutical Statistics Application Using Shiny**

### Required Software

- **R (latest version, >=3.5.0)**

  - The source programming language

- **RStudio**

  - The platform for Shiny 

## Installation from Github

Check the existence of "devtools" package

    > find.package("devtools")

- If yes, install "mephas" package from github:
      
      > devtools::install_github(“mephas/mephas”)
    
- If error, install "devtools" package firstly:

      > utils::install.packages("devtools")
      > devtools::install_github(“mephas/mephas”)
      
## Installation from ".tar.gz" file

### Required Packages Before / During the Installation

DescTools, ROCR, Rmisc, ggplot2, ggfortify, gridExtra, pastecs, plotROC, psych, reshape, stargazer, shiny, survminer, survival, xtable, spls, pls

    > install.packages(c("DescTools", "ROCR", "Rmisc", "ggfortify","pastecs","plotROC", "psych","reshape","stargazer","survminer","xtable","spls"))





