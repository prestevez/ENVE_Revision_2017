# package installer

### First, devtools:

if(!"devtools" %in% rownames(installed.packages()))
{
    install.packages("devtools")
}

# Next, install custom package "victim" from github

devtools::install_github("prestevez/victim")

## install the rest of the packages, except glmmadmb

install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("lme4")
install.packages("texreg")
install.packages("R2admb")
install.packages("coda")
install.packages("classInt")
install.packages("reshape2")
install.packages("lmtest")
install.packages("car")
install.packages("pscl")

# install glmmadmb from source repo

install.packages("glmmADMB",
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")

# All packages should now be installed

source("package_check.R")
