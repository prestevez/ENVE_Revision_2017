# Package checker script

packages <- c("devtools",
              "victim",
              "foreign",
              "ggplot2",
              "Cairo",
              "knitr",
              "lme4",
              "texreg",
              "R2admb",
              "glmmADMB",
              "classInt",
              "dplyr",
              "reshape2",
              "lmtest",
              "car",
              "pscl")

test <- packages %in% rownames(installed.packages())

if(all(test))
{
    cat("All required packages are installed.")
} else
{
    not <- packages[which(test == FALSE)]
    cat(paste0("Packages not installed: ", not,
                 ".\nPlease install them manually."))
}
