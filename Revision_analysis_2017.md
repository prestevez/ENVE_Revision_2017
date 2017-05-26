---
title: "Repeat extortion of Mexican businesses: Revision analysis"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "4/05/2016"
output:
  md_document:
    variant: "markdown"
pandoc_args: "--smart"
---



# Introduction

Script to analyze patterns of extortion victimization against Mexican businesses.

# Set up, data input and pre-process

## Session info

We first check details of the session and system, and for reproducibility, we set the random seed.


```r
starttime <- proc.time()
date()
```

```
[1] "Fri May 26 22:04:44 2017"
```

```r
sessionInfo()
```

```
R version 3.4.0 (2017-04-21)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux Server 7.2 (Maipo)

Matrix products: default
BLAS/LAPACK: /shared/ucl/apps/openblas/0.2.14/gnu-4.9.2/lib/libopenblasp-r0.2.14.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] Cairo_1.5-9       reshape2_1.4.2    dplyr_0.5.0      
 [4] classInt_0.1-24   lme4_1.1-13       Matrix_1.2-9     
 [7] knitr_1.16        ggplot2_2.2.1     foreign_0.8-67   
[10] vicarp_0.0.0.9000

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.10     highr_0.6        compiler_3.4.0   nloptr_1.0.4    
 [5] git2r_0.18.0     plyr_1.8.4       class_7.3-14     tools_3.4.0     
 [9] digest_0.6.12    evaluate_0.10    memoise_1.1.0    tibble_1.3.0    
[13] gtable_0.2.0     nlme_3.1-131     lattice_0.20-35  DBI_0.6-1       
[17] curl_2.6         e1071_1.6-8      withr_1.0.2      httr_1.2.1      
[21] stringr_1.2.0    devtools_1.13.1  grid_3.4.0       R6_2.2.1        
[25] ineq_0.2-13      minqa_1.2.4      magrittr_1.5     scales_0.4.1    
[29] codetools_0.2-15 splines_3.4.0    MASS_7.3-47      assertthat_0.2.0
[33] colorspace_1.3-2 labeling_0.3     stringi_1.1.5    lazyeval_0.2.0  
[37] munsell_0.4.3   
```

```r
set.seed(42)
options(scipen=0)
```

## Load packages and functions

Install custom package, requires `devtools`.


```r
# devtools::install_github("prestevez/vicarp")
```

Next we load the packages that we will use.


```r
library(vicarp)
library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
#library(texreg)
library(lme4)
library(glmmADMB)
```

```
Error in library(glmmADMB): there is no package called 'glmmADMB'
```

```r
library(classInt)
library(dplyr)
library(reshape2)
```



## Data input

Load the data for the study.


```r
enve_all <- read.dbf("enve2014cuest_ciega_2014.dbf")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
area_level_tmp <- read.csv("state_level_data.csv", header=TRUE)
area_level <- merge(area_level_tmp, cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("secode.csv", head=TRUE)
scode$Code <- scode$Code*10000
```


Quick overview of all the variables in the dataset


```r
summary(enve_all)
```

```
     P26_10        CVE_ENT     ID_ESTRATO      P33       SECTOR_FIN
 1      : 109   13     : 100   1:589      1      : 135   C:809     
 2      :  46   11     :  95   2:633      2      :  54   I:825     
 3      :  32   14     :  95   3:618      3      :  34   S:866     
 4      :  25   19     :  87   4:660      4      :  21             
 5      :  19   9      :  85              5      :  11             
 (Other):  66   21     :  84              (Other):  27             
 NA's   :2203   (Other):1954              NA's   :2218             
     P1_1B            P3         CVE_UNICA      P25_10   P29_1    P30_1   
 263010 :   4   2005   :  68   Min.   :    60   1: 297   1: 282   1: 282  
 383510 :   4   1982   :  66   1st Qu.: 10220   2:2153   2:2168   2:2168  
 390810 :   4   2008   :  66   Median : 19849   9:  50   9:  50   9:  50  
 419710 :   4   2013   :  66   Mean   : 32087                             
 589710 :   4   1991   :  65   3rd Qu.: 53126                             
 631010 :   4   2009   :  64   Max.   :100000                             
 (Other):2476   (Other):2105                                              
 P31_1    P32_1      ID_CONSECU    
 1: 282   1: 282   Min.   :    60  
 2:2168   2:2168   1st Qu.: 10220  
 9:  50   9:  50   Median : 19849  
                   Mean   : 32087  
                   3rd Qu.: 53126  
                   Max.   :100000  
                                   
```

Now select only the relevant variables


```r
enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))

enve_test$extortion_victim <- enve_all$P25_10
enve_test$extortions[enve_test$extortion_victim == 2] <- 0

summary(enve_test$extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5808  0.0000 33.0000      50 
```

```r
table(enve_test$extortions)
```

```

   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
2153  109   46   32   25   19    8    5    4    5    4    2    5    4    6 
  15   19   20   21   22   23   25   27   29   32   33 
   5    2    4    2    1    2    1    1    1    3    1 
```

```r
enve_test$extortions_nas <- enve_test$extortions
enve_test$extortions[is.na(enve_test$extortions)] <- 0

summary(enve_test$extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.5692  0.0000 33.0000 
```

```r
table(enve_test$extortions)
```

```

   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
2203  109   46   32   25   19    8    5    4    5    4    2    5    4    6 
  15   19   20   21   22   23   25   27   29   32   33 
   5    2    4    2    1    2    1    1    1    3    1 
```

```r
levels(enve_test$extortion_victim) <- c("yes", "no", "dk")
enve_test$extortion_victim <- relevel(enve_test$extortion_victim, ref = "no")

enve_test$rep_extortion_victim <- factor(enve_test$extortions)
levels(enve_test$rep_extortion_victim) <- c(0, 0,
                    rep(1, length(levels(enve_test$rep_extortion_victim)) - 2))

summary(enve_test$rep_extortion_victim)
```

```
   0    1 
2312  188 
```

```r
summary(enve_test)
```

```
   extortions      extortion_victim extortions_nas    rep_extortion_victim
 Min.   : 0.0000   no :2153         Min.   : 0.0000   0:2312              
 1st Qu.: 0.0000   yes: 297         1st Qu.: 0.0000   1: 188              
 Median : 0.0000   dk :  50         Median : 0.0000                       
 Mean   : 0.5692                    Mean   : 0.5808                       
 3rd Qu.: 0.0000                    3rd Qu.: 0.0000                       
 Max.   :33.0000                    Max.   :33.0000                       
                                    NA's   :50                            
```




```r
enve_test$bribes <- as.integer(as.character(enve_all$P33))
summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.486   3.000  16.000    2218 
```

```r
# 4 bribe cats
enve_test$bribe1 <- enve_all$P29_1
enve_test$bribe2 <- enve_all$P30_1
enve_test$bribe3 <- enve_all$P31_1
enve_test$bribe4 <- enve_all$P32_1

enve_test$bribes[with(enve_test,
                        bribe1 == 2 &
                        bribe2 == 2 &
                        bribe3 == 2 &
                        bribe4 == 2)] <- 0

summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2861  0.0000 16.0000      50 
```

```r
enve_test$bribe_victim[enve_test$bribes > 0] <- 1
enve_test$bribe_victim[with(enve_test,
                        bribe1 == 2 &
                        bribe2 == 2 &
                        bribe3 == 2 &
                        bribe4 == 2)] <- 0

enve_test$bribe_victim[with(enve_test,
                        bribe1 == 9 &
                        bribe2 == 9 &
                        bribe3 == 9 &
                        bribe4 == 9)] <- 9

enve_test %>%
    mutate(bribe_victim = as.factor(bribe_victim)) -> enve_test

levels(enve_test$bribe_victim) <- c("no", "yes", "dk")

bribecols <- c("bribe1",
               "bribe2",
               "bribe3",
               "bribe4")

colindbribe <- which(names(enve_test) %in% bribecols)

enve_test <- enve_test[,-colindbribe]

enve_test$bribes_nas <- enve_test$bribes

enve_test$bribes[is.na(enve_test$bribes)] <- 0


summary(enve_test)
```

```
   extortions      extortion_victim extortions_nas    rep_extortion_victim
 Min.   : 0.0000   no :2153         Min.   : 0.0000   0:2312              
 1st Qu.: 0.0000   yes: 297         1st Qu.: 0.0000   1: 188              
 Median : 0.0000   dk :  50         Median : 0.0000                       
 Mean   : 0.5692                    Mean   : 0.5808                       
 3rd Qu.: 0.0000                    3rd Qu.: 0.0000                       
 Max.   :33.0000                    Max.   :33.0000                       
                                    NA's   :50                            
     bribes        bribe_victim   bribes_nas     
 Min.   : 0.0000   no :2168     Min.   : 0.0000  
 1st Qu.: 0.0000   yes: 282     1st Qu.: 0.0000  
 Median : 0.0000   dk :  50     Median : 0.0000  
 Mean   : 0.2804                Mean   : 0.2861  
 3rd Qu.: 0.0000                3rd Qu.: 0.0000  
 Max.   :16.0000                Max.   :16.0000  
                                NA's   :50       
```


```r
enve_test$CVE_UNICA <- as.integer(as.character(enve_all$ID_CONSECU))

enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

# subsector
enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)
```

```
 [1] "Retail"         "Mining"         "Utilities"      "Construction"  
 [5] "Manufacturing"  "Wholesale"      "Transport"      "Media"         
 [9] "Finance"        "Real estate"    "Prof. services" "Corporate"     
[13] "Maintenance"    "Education"      "Health"         "Leisure"       
[17] "HotelsRestBar"  "Other"         
```

```r
### To exclude the subsectors with very few observations

# summary(enve_test)
# nrow(enve_test)
# enve_test %>%
#     filter(subsector != "Utilities") %>%
#     filter(subsector != "Corporate") -> enve_test_2
# 
# enve_test_2$subsector <- droplevels(enve_test_2$subsector)
# 
# levels(enve_test_2$subsector)
# nrow(enve_test_2)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
intyears <- classIntervals(enve_test$years, 5, style="quantile")
enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE,
                            include.lowest = TRUE)

intyears_deciles <- classIntervals(enve_test$years, 10, style="quantile")
enve_test$years_deciles <- cut(enve_test$years, intyears_deciles$brks, right=TRUE,
                            include.lowest = TRUE)


enve_test <- merge(enve_test, area_level, by="CVE_ENT", all.x=TRUE)

length(enve_test$extortions[is.na(enve_test$extortions)])
```

```
[1] 0
```

```r
length(enve_test$bribes[is.na(enve_test$bribes)])
```

```
[1] 0
```

```r
## Summary of final dataset
summary(enve_test)
```

```
    CVE_ENT        extortions      extortion_victim extortions_nas   
 Min.   : 1.00   Min.   : 0.0000   no :2153         Min.   : 0.0000  
 1st Qu.: 9.00   1st Qu.: 0.0000   yes: 297         1st Qu.: 0.0000  
 Median :16.00   Median : 0.0000   dk :  50         Median : 0.0000  
 Mean   :16.56   Mean   : 0.5692                    Mean   : 0.5808  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.: 0.0000  
 Max.   :32.00   Max.   :33.0000                    Max.   :33.0000  
                                                    NA's   :50       
 rep_extortion_victim     bribes        bribe_victim   bribes_nas     
 0:2312               Min.   : 0.0000   no :2168     Min.   : 0.0000  
 1: 188               1st Qu.: 0.0000   yes: 282     1st Qu.: 0.0000  
                      Median : 0.0000   dk :  50     Median : 0.0000  
                      Mean   : 0.2804                Mean   : 0.2861  
                      3rd Qu.: 0.0000                3rd Qu.: 0.0000  
                      Max.   :16.0000                Max.   :16.0000  
                                                     NA's   :50       
   CVE_UNICA          size     sector     tempsub      
 Min.   :    60   Large :589   C:809   Min.   :212410  
 1st Qu.: 10220   Medium:633   I:825   1st Qu.:361710  
 Median : 19849   Small :618   S:866   Median :511160  
 Mean   : 32087   Micro :660           Mean   :511102  
 3rd Qu.: 53126                        3rd Qu.:658810  
 Max.   :100000                        Max.   :812910  
                                                       
         subsector       years         yearsquant  years_deciles
 Manufacturing:484   Min.   : 0.00   [0,8]  :552   [0,4]  :299  
 Health       :391   1st Qu.:10.00   (8,16] :454   (25,30]:297  
 HotelsRestBar:367   Median :21.00   (16,25]:507   (16,21]:288  
 Construction :356   Mean   :20.94   (25,34]:525   (4,8]  :253  
 Maintenance  :206   3rd Qu.:31.00   (34,43]:462   (34,39]:248  
 Transport    :135   Max.   :43.00                 (12,16]:232  
 (Other)      :561                                 (Other):883  
   Prevalence       Incidence     Concentration   log_bribe_vic     
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.05294  
 1st Qu.: 45.00   1st Qu.: 63.0   1st Qu.:1.340   1st Qu.:-0.39616  
 Median : 59.00   Median :103.0   Median :1.606   Median :-0.16564  
 Mean   : 69.97   Mean   :115.7   Mean   :1.658   Mean   :-0.09983  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.27829  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.92312  
                                                                    
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.49642   Mean   :  0.2107   Mean   :-0.6768  
 3rd Qu.: 0.16603   3rd Qu.:  8.8654   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.16721  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.02588   Mean   :-0.3763   Mean   :-0.2396   Mean   :-0.03664  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index           comp_index                   NOM_ENT    
 Min.   :-33.02134   Min.   :-22.4096   HIDALGO         : 100  
 1st Qu.: -4.29495   1st Qu.: -5.1176   GUANAJUATO      :  95  
 Median :  1.13880   Median :  1.3520   JALISCO         :  95  
 Mean   :  0.03519   Mean   :  0.1425   NUEVO LEON      :  87  
 3rd Qu.:  7.76001   3rd Qu.:  5.0020   DISTRITO FEDERAL:  85  
 Max.   : 24.00707   Max.   : 20.1805   PUEBLA          :  84  
                                        (Other)         :1954  
    NOM_ABR    
 HGO.   : 100  
 GTO.   :  95  
 JAL.   :  95  
 NL     :  87  
 DF     :  85  
 PUE.   :  84  
 (Other):1954  
```
## Incident module

Add data from the incident module to briefly, **briefly**, analyse the distributions of extortion according to modus operandi.


```r
enve_incidents_all <- read.dbf("enve2014delitos_ciega_2014.dbf")

# Selecting only those relevant for extortion (code 10)
enve_incidents_all$delito <- as.integer(as.character(enve_incidents_all$ID_DELITO))
enve_incidents <- enve_incidents_all[enve_incidents_all$delito == 10,]

incident_df <- data.frame(CVE_UNICA=as.integer(as.character(enve_incidents$ID_CONSECU)))

incident_df$delito <- enve_incidents$delito

incident_df$type <- as.character(enve_incidents$M5_1)
incident_df$type <- as.factor(incident_df$type)
levels(incident_df$type) <- c("Telephone", "Internet", "Street",
                                        "Premises", "Cobro de piso", "Other")

incident_df$simp <- incident_df$type
levels(incident_df$simp) <- c("Remote", "Remote", "In person",
                                        "In person", "In person", "Other")

# Transform the factors into dummy variables

type_dummy <- data.frame(model.matrix( ~ type -1 , data = incident_df))

simple_dummy <- data.frame(model.matrix( ~ simp -1 , data = incident_df))

incident_df <- cbind(incident_df, type_dummy, simple_dummy)

summary(incident_df)
```

```
   CVE_UNICA         delito              type             simp     
 Min.   :   24   Min.   :10   Telephone    :6207   Remote   :6245  
 1st Qu.: 7108   1st Qu.:10   Internet     :  38   In person: 290  
 Median :11730   Median :10   Street       : 207   Other    : 120  
 Mean   :11935   Mean   :10   Premises     :  52                   
 3rd Qu.:17428   3rd Qu.:10   Cobro de piso:  31                   
 Max.   :23385   Max.   :10   Other        : 120                   
 typeTelephone     typeInternet       typeStreet      typePremises     
 Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.000000  
 1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.000000  
 Median :1.0000   Median :0.00000   Median :0.0000   Median :0.000000  
 Mean   :0.9327   Mean   :0.00571   Mean   :0.0311   Mean   :0.007814  
 3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.000000  
 Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.000000  
 typeCobro.de.piso    typeOther         simpRemote     simpIn.person    
 Min.   :0.000000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
 1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:1.0000   1st Qu.:0.00000  
 Median :0.000000   Median :0.00000   Median :1.0000   Median :0.00000  
 Mean   :0.004658   Mean   :0.01803   Mean   :0.9384   Mean   :0.04358  
 3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.00000  
 Max.   :1.000000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000  
   simpOther      
 Min.   :0.00000  
 1st Qu.:0.00000  
 Median :0.00000  
 Mean   :0.01803  
 3rd Qu.:0.00000  
 Max.   :1.00000  
```

Next we merge both incident-level and victim-level tables, and produce a new business-level table with counts based on the incident module.


```r
enve_incvic <- merge(incident_df, enve_test, by="CVE_UNICA", all.y = TRUE)

summary(enve_incvic)
```

```
   CVE_UNICA          delito                type             simp     
 Min.   :    60   Min.   :10     Telephone    :1475   Remote   :1481  
 1st Qu.:  9352   1st Qu.:10     Internet     :   6   In person:  86  
 Median : 18064   Median :10     Street       :  63   Other    :  33  
 Mean   : 29792   Mean   :10     Premises     :  16   NA's     :1221  
 3rd Qu.: 47996   3rd Qu.:10     Cobro de piso:   7                   
 Max.   :100000   Max.   :10     Other        :  33                   
                  NA's   :1221   NA's         :1221                   
 typeTelephone     typeInternet      typeStreet      typePremises 
 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00  
 1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00  
 Median :1.0000   Median :0.0000   Median :0.0000   Median :0.00  
 Mean   :0.9219   Mean   :0.0038   Mean   :0.0394   Mean   :0.01  
 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00  
 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00  
 NA's   :1221     NA's   :1221     NA's   :1221     NA's   :1221  
 typeCobro.de.piso   typeOther        simpRemote     simpIn.person   
 Min.   :0.0000    Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
 1st Qu.:0.0000    1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:0.0000  
 Median :0.0000    Median :0.0000   Median :1.0000   Median :0.0000  
 Mean   :0.0044    Mean   :0.0206   Mean   :0.9256   Mean   :0.0538  
 3rd Qu.:0.0000    3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
 Max.   :1.0000    Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
 NA's   :1221      NA's   :1221     NA's   :1221     NA's   :1221    
   simpOther         CVE_ENT        extortions      extortion_victim
 Min.   :0.0000   Min.   : 1.00   Min.   : 0.0000   no :2438        
 1st Qu.:0.0000   1st Qu.: 9.00   1st Qu.: 0.0000   yes: 330        
 Median :0.0000   Median :16.00   Median : 0.0000   dk :  53        
 Mean   :0.0206   Mean   :16.61   Mean   : 0.5778                   
 3rd Qu.:0.0000   3rd Qu.:24.00   3rd Qu.: 0.0000                   
 Max.   :1.0000   Max.   :32.00   Max.   :33.0000                   
 NA's   :1221                                                       
 extortions_nas    rep_extortion_victim     bribes        bribe_victim
 Min.   : 0.0000   0:2613               Min.   : 0.0000   no :2435    
 1st Qu.: 0.0000   1: 208               1st Qu.: 0.0000   yes: 331    
 Median : 0.0000                        Median : 0.0000   dk :  55    
 Mean   : 0.5889                        Mean   : 0.2974               
 3rd Qu.: 0.0000                        3rd Qu.: 0.0000               
 Max.   :33.0000                        Max.   :16.0000               
 NA's   :53                                                           
   bribes_nas          size     sector     tempsub      
 Min.   : 0.0000   Large :669   C:917   Min.   :212410  
 1st Qu.: 0.0000   Medium:709   I:912   1st Qu.:363310  
 Median : 0.0000   Small :699   S:992   Median :510810  
 Mean   : 0.3033   Micro :744           Mean   :511298  
 3rd Qu.: 0.0000                        3rd Qu.:658510  
 Max.   :16.0000                        Max.   :812910  
 NA's   :55                                             
         subsector       years         yearsquant  years_deciles 
 Manufacturing:561   Min.   : 0.00   [0,8]  :636   [0,4]  : 348  
 Health       :444   1st Qu.:10.00   (8,16] :518   (25,30]: 326  
 HotelsRestBar:413   Median :21.00   (16,25]:568   (16,21]: 318  
 Construction :395   Mean   :20.78   (25,34]:586   (4,8]  : 288  
 Maintenance  :229   3rd Qu.:31.00   (34,43]:513   (34,39]: 280  
 Transport    :155   Max.   :43.00                 (12,16]: 261  
 (Other)      :624                                 (Other):1000  
   Prevalence       Incidence     Concentration   log_bribe_vic    
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.0529  
 1st Qu.: 45.00   1st Qu.: 64.0   1st Qu.:1.340   1st Qu.:-0.3962  
 Median : 59.00   Median :103.0   Median :1.589   Median :-0.1656  
 Mean   : 69.91   Mean   :115.4   Mean   :1.654   Mean   :-0.1061  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.2783  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.9231  
                                                                   
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.47383   Mean   :  0.5245   Mean   :-0.6622  
 3rd Qu.: 0.16603   3rd Qu.: 11.6003   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.16721  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.04986   Mean   :-0.3522   Mean   :-0.2289   Mean   :-0.03554  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index           comp_index             NOM_ENT        NOM_ABR    
 Min.   :-33.02134   Min.   :-22.4096   JALISCO   : 120   JAL.   : 120  
 1st Qu.: -4.55576   1st Qu.: -5.1176   HIDALGO   : 111   HGO.   : 111  
 Median :  1.10949   Median :  1.3520   GUANAJUATO: 110   GTO.   : 110  
 Mean   : -0.07096   Mean   :  0.1388   OAXACA    : 106   OAX.   : 106  
 3rd Qu.:  7.76001   3rd Qu.:  5.0020   NUEVO LEON: 102   NL     : 102  
 Max.   : 24.00707   Max.   : 20.1805   ZACATECAS :  99   ZAC.   :  99  
                                        (Other)   :2173   (Other):2173  
```

```r
nrow(enve_incvic)
```

```
[1] 2821
```

```r
nrow(enve_test)
```

```
[1] 2500
```

```r
length(unique(enve_incvic$CVE_UNICA))
```

```
[1] 2500
```

```r
enve_incvic %>%
    group_by(CVE_UNICA) %>%
    summarise(typeTelephone = sum(typeTelephone),
              typeInternet = sum(typeInternet),
              typeStreet = sum(typeStreet),
              typePremises = sum(typePremises),
              typeCobro.de.piso = sum(typeCobro.de.piso),
              typeOther = sum(typeOther),
              simpRemote = sum(simpRemote),
              simpIn.person = sum(simpIn.person),
              simpOther = sum(simpOther)) -> enve_type_counts

enve_type_counts[(is.na(enve_type_counts))] <- 0

nrow(enve_type_counts)
```

```
[1] 2500
```

```r
enve_type_counts %>%
    mutate(cap_count = simpRemote + simpIn.person + simpOther,
           propTel = typeTelephone/cap_count,
           propInternet = typeInternet/cap_count,
           propStreet = typeStreet/cap_count,
           propPremises = typePremises/cap_count,
           propPiso = typeCobro.de.piso/cap_count,
           propOther = typeOther/cap_count,
           propRemote = simpRemote/cap_count,
           propIn.person = simpIn.person/cap_count) -> enve_type_counts_and_props

enve_type_counts_and_props[is.na(enve_type_counts_and_props)] <- 0

enve_test %>%
    left_join(enve_type_counts_and_props, by = "CVE_UNICA") %>%
    mutate(estTel = round(propTel * extortions),
           estInternet = round(propInternet * extortions),
           estStreet =  round(propStreet * extortions),
           estPremises = round(propPremises * extortions),
           estPiso = round(propPiso * extortions),
           estOther = round(propOther * extortions),
           estRemote = round(propRemote * extortions),
           estIn.person = round(propIn.person * extortions)) -> enve_final

summary(enve_final)
```

```
    CVE_ENT        extortions      extortion_victim extortions_nas   
 Min.   : 1.00   Min.   : 0.0000   no :2153         Min.   : 0.0000  
 1st Qu.: 9.00   1st Qu.: 0.0000   yes: 297         1st Qu.: 0.0000  
 Median :16.00   Median : 0.0000   dk :  50         Median : 0.0000  
 Mean   :16.56   Mean   : 0.5692                    Mean   : 0.5808  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.: 0.0000  
 Max.   :32.00   Max.   :33.0000                    Max.   :33.0000  
                                                    NA's   :50       
 rep_extortion_victim     bribes        bribe_victim   bribes_nas     
 0:2312               Min.   : 0.0000   no :2168     Min.   : 0.0000  
 1: 188               1st Qu.: 0.0000   yes: 282     1st Qu.: 0.0000  
                      Median : 0.0000   dk :  50     Median : 0.0000  
                      Mean   : 0.2804                Mean   : 0.2861  
                      3rd Qu.: 0.0000                3rd Qu.: 0.0000  
                      Max.   :16.0000                Max.   :16.0000  
                                                     NA's   :50       
   CVE_UNICA          size     sector     tempsub      
 Min.   :    60   Large :589   C:809   Min.   :212410  
 1st Qu.: 10220   Medium:633   I:825   1st Qu.:361710  
 Median : 19849   Small :618   S:866   Median :511160  
 Mean   : 32087   Micro :660           Mean   :511102  
 3rd Qu.: 53126                        3rd Qu.:658810  
 Max.   :100000                        Max.   :812910  
                                                       
         subsector       years         yearsquant  years_deciles
 Manufacturing:484   Min.   : 0.00   [0,8]  :552   [0,4]  :299  
 Health       :391   1st Qu.:10.00   (8,16] :454   (25,30]:297  
 HotelsRestBar:367   Median :21.00   (16,25]:507   (16,21]:288  
 Construction :356   Mean   :20.94   (25,34]:525   (4,8]  :253  
 Maintenance  :206   3rd Qu.:31.00   (34,43]:462   (34,39]:248  
 Transport    :135   Max.   :43.00                 (12,16]:232  
 (Other)      :561                                 (Other):883  
   Prevalence       Incidence     Concentration   log_bribe_vic     
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.05294  
 1st Qu.: 45.00   1st Qu.: 63.0   1st Qu.:1.340   1st Qu.:-0.39616  
 Median : 59.00   Median :103.0   Median :1.606   Median :-0.16564  
 Mean   : 69.97   Mean   :115.7   Mean   :1.658   Mean   :-0.09983  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.27829  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.92312  
                                                                    
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.49642   Mean   :  0.2107   Mean   :-0.6768  
 3rd Qu.: 0.16603   3rd Qu.:  8.8654   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.16721  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.02588   Mean   :-0.3763   Mean   :-0.2396   Mean   :-0.03664  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index           comp_index                   NOM_ENT    
 Min.   :-33.02134   Min.   :-22.4096   HIDALGO         : 100  
 1st Qu.: -4.29495   1st Qu.: -5.1176   GUANAJUATO      :  95  
 Median :  1.13880   Median :  1.3520   JALISCO         :  95  
 Mean   :  0.03519   Mean   :  0.1425   NUEVO LEON      :  87  
 3rd Qu.:  7.76001   3rd Qu.:  5.0020   DISTRITO FEDERAL:  85  
 Max.   : 24.00707   Max.   : 20.1805   PUEBLA          :  84  
                                        (Other)         :1954  
    NOM_ABR     typeTelephone   typeInternet      typeStreet    
 HGO.   : 100   Min.   :0.00   Min.   :0.0000   Min.   :0.0000  
 GTO.   :  95   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:0.0000  
 JAL.   :  95   Median :0.00   Median :0.0000   Median :0.0000  
 NL     :  87   Mean   :0.59   Mean   :0.0024   Mean   :0.0252  
 DF     :  85   3rd Qu.:1.00   3rd Qu.:0.0000   3rd Qu.:0.0000  
 PUE.   :  84   Max.   :5.00   Max.   :1.0000   Max.   :5.0000  
 (Other):1954                                                   
  typePremises    typeCobro.de.piso   typeOther        simpRemote    
 Min.   :0.0000   Min.   :0.0000    Min.   :0.0000   Min.   :0.0000  
 1st Qu.:0.0000   1st Qu.:0.0000    1st Qu.:0.0000   1st Qu.:0.0000  
 Median :0.0000   Median :0.0000    Median :0.0000   Median :0.0000  
 Mean   :0.0064   Mean   :0.0028    Mean   :0.0132   Mean   :0.5924  
 3rd Qu.:0.0000   3rd Qu.:0.0000    3rd Qu.:0.0000   3rd Qu.:1.0000  
 Max.   :5.0000   Max.   :3.0000    Max.   :4.0000   Max.   :5.0000  
                                                                     
 simpIn.person      simpOther        cap_count       propTel      
 Min.   :0.0000   Min.   :0.0000   Min.   :0.00   Min.   :0.0000  
 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.0000  
 Median :0.0000   Median :0.0000   Median :1.00   Median :0.0000  
 Mean   :0.0344   Mean   :0.0132   Mean   :0.64   Mean   :0.4722  
 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.00   3rd Qu.:1.0000  
 Max.   :5.0000   Max.   :4.0000   Max.   :5.00   Max.   :1.0000  
                                                                  
  propInternet      propStreet     propPremises       propPiso       
 Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.000000  
 1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.000000  
 Median :0.0000   Median :0.000   Median :0.0000   Median :0.000000  
 Mean   :0.0024   Mean   :0.021   Mean   :0.0038   Mean   :0.001733  
 3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.000000  
 Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.000000  
                                                                     
   propOther        propRemote     propIn.person         estTel      
 Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   : 0.000  
 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.: 0.000  
 Median :0.0000   Median :0.0000   Median :0.00000   Median : 0.000  
 Mean   :0.0105   Mean   :0.4746   Mean   :0.02653   Mean   : 0.276  
 3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.: 0.000  
 Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :33.000  
                                                                     
  estInternet   estStreet        estPremises       estPiso      
 Min.   :0    Min.   : 0.0000   Min.   :0e+00   Min.   :0.0000  
 1st Qu.:0    1st Qu.: 0.0000   1st Qu.:0e+00   1st Qu.:0.0000  
 Median :0    Median : 0.0000   Median :0e+00   Median :0.0000  
 Mean   :0    Mean   : 0.0116   Mean   :8e-04   Mean   :0.0016  
 3rd Qu.:0    3rd Qu.: 0.0000   3rd Qu.:0e+00   3rd Qu.:0.0000  
 Max.   :0    Max.   :27.0000   Max.   :2e+00   Max.   :4.0000  
                                                                
    estOther        estRemote       estIn.person   
 Min.   :0.0000   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:0.0000   1st Qu.: 0.000   1st Qu.: 0.000  
 Median :0.0000   Median : 0.000   Median : 0.000  
 Mean   :0.0032   Mean   : 0.276   Mean   : 0.014  
 3rd Qu.:0.0000   3rd Qu.: 0.000   3rd Qu.: 0.000  
 Max.   :4.0000   Max.   :33.000   Max.   :27.000  
                                                   
```

# EDA

Re-do some of the EDA

## Univariate extortion analysis

Distribution, lorenz plots and poisson tests for the aggregated extortion counts in the screening questionnaire


```r
extortion_columns1 <- c("extortions", "extortions_nas")

extortion_dist1 <- lapply(extortion_columns1, victim_table,
                          data = enve_final, print_option = "pandoc")

names(extortion_dist1) <- extortion_columns1

extortion_dist1
```

```
$extortions


Table: Victimisation distribution of extortions.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2203          --        --      88.12         --           --
      1          109         109        --       4.36     36.700        7.660
      2           46          92        46       1.84     15.488        6.465
      3           32          96        64       1.28     10.774        6.746
      4           25         100        75       1.00      8.418        7.027
      5           19          95        76       0.76      6.397        6.676
      6            8          48        40       0.32      2.694        3.373
      7            5          35        30       0.20      1.684        2.460
      8            4          32        28       0.16      1.347        2.249
      9            5          45        40       0.20      1.684        3.162
     10            4          40        36       0.16      1.347        2.811
     11            2          22        20       0.08      0.673        1.546
     12            5          60        55       0.20      1.684        4.216
     13            4          52        48       0.16      1.347        3.654
     14            6          84        78       0.24      2.020        5.903
     15            5          75        70       0.20      1.684        5.271
     19            2          38        36       0.08      0.673        2.670
     20            4          80        76       0.16      1.347        5.622
     21            2          42        40       0.08      0.673        2.952
     22            1          22        21       0.04      0.337        1.546
     23            2          46        44       0.08      0.673        3.233
     25            1          25        24       0.04      0.337        1.757
     27            1          27        26       0.04      0.337        1.897
     29            1          29        28       0.04      0.337        2.038
     32            3          96        93       0.12      1.010        6.746
     33            1          33        32       0.04      0.337        2.319

$extortions_nas


Table: Victimisation distribution of extortions_nas.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2153          --        --     87.878         --           --
      1          109         109        --      4.449     36.700        7.660
      2           46          92        46      1.878     15.488        6.465
      3           32          96        64      1.306     10.774        6.746
      4           25         100        75      1.020      8.418        7.027
      5           19          95        76      0.776      6.397        6.676
      6            8          48        40      0.327      2.694        3.373
      7            5          35        30      0.204      1.684        2.460
      8            4          32        28      0.163      1.347        2.249
      9            5          45        40      0.204      1.684        3.162
     10            4          40        36      0.163      1.347        2.811
     11            2          22        20      0.082      0.673        1.546
     12            5          60        55      0.204      1.684        4.216
     13            4          52        48      0.163      1.347        3.654
     14            6          84        78      0.245      2.020        5.903
     15            5          75        70      0.204      1.684        5.271
     19            2          38        36      0.082      0.673        2.670
     20            4          80        76      0.163      1.347        5.622
     21            2          42        40      0.082      0.673        2.952
     22            1          22        21      0.041      0.337        1.546
     23            2          46        44      0.082      0.673        3.233
     25            1          25        24      0.041      0.337        1.757
     27            1          27        26      0.041      0.337        1.897
     29            1          29        28      0.041      0.337        2.038
     32            3          96        93      0.122      1.010        6.746
     33            1          33        32      0.041      0.337        2.319
```

```r
extortion_dist1 <- lapply(extortion_columns1, victim_table,
                          data = enve_final, print_option = "none")

names(extortion_dist1) <- extortion_columns1

ext1_vicum <- lapply(extortion_dist1, victim_cumulative)

lapply(ext1_vicum, kable, format = "pandoc", digits = 3)
```

```
$extortions


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1         11.88   100.000     100.000
      2          7.52    63.300      92.340
      3          5.68    47.811      85.875
      4          4.40    37.037      79.129
      5          3.40    28.620      72.101
      6          2.64    22.222      65.425
      7          2.32    19.529      62.052
      8          2.12    17.845      59.592
      9          1.96    16.498      57.344
     10          1.76    14.815      54.181
     11          1.60    13.468      51.370
     12          1.52    12.795      49.824
     13          1.32    11.111      45.608
     14          1.16     9.764      41.954
     15          0.92     7.744      36.051
     19          0.72     6.061      30.780
     20          0.64     5.387      28.110
     21          0.48     4.040      22.488
     22          0.40     3.367      19.536
     23          0.36     3.030      17.990
     25          0.28     2.357      14.758
     27          0.24     2.020      13.001
     29          0.20     1.684      11.103
     32          0.16     1.347       9.065
     33          0.04     0.337       2.319

$extortions_nas


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1        12.122   100.000     100.000
      2         7.673    63.300      92.340
      3         5.796    47.811      85.875
      4         4.490    37.037      79.129
      5         3.469    28.620      72.101
      6         2.694    22.222      65.425
      7         2.367    19.529      62.052
      8         2.163    17.845      59.592
      9         2.000    16.498      57.344
     10         1.796    14.815      54.181
     11         1.633    13.468      51.370
     12         1.551    12.795      49.824
     13         1.347    11.111      45.608
     14         1.184     9.764      41.954
     15         0.939     7.744      36.051
     19         0.735     6.061      30.780
     20         0.653     5.387      28.110
     21         0.490     4.040      22.488
     22         0.408     3.367      19.536
     23         0.367     3.030      17.990
     25         0.286     2.357      14.758
     27         0.245     2.020      13.001
     29         0.204     1.684      11.103
     32         0.163     1.347       9.065
     33         0.041     0.337       2.319
```


```r
lapply(extortion_columns1, victim_lorenz, 
       data = enve_final, family = "none")
```

```
[[1]]
```

![plot of chunk extortions1-lorenz](figure/extortions1-lorenz-1.png)

```

[[2]]
```

![plot of chunk extortions1-lorenz](figure/extortions1-lorenz-2.png)

```r
victim_lorenz("extortions", enve_final, family = "poisson")
```

![plot of chunk extortions1-lorenz](figure/extortions1-lorenz-3.png)

```r
victim_lorenz("extortions", enve_final, family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

![plot of chunk extortions1-lorenz](figure/extortions1-lorenz-4.png)

Now do some MC gini tests, Index of dispersion tests, and KS tests


```r
mc_gini_test("extortions", enve_final, family = "poisson", plots = TRUE)
```

```
$DV
[1] "extortions"

$stat
Observed Gini Coefficient for extortions 
                               0.9478406 

$mc_mean
Monte Carlo mean 
       0.6462194 

$mc_confint
  MC 2.5%  MC 97.5% 
0.6295068 0.6634189 

$reps
Replicates 
      2000 

$mc_test
Alternative Hypothesis 
                  TRUE 

$plot
```

![plot of chunk extortions1-tests](figure/extortions1-tests-1.png)

```r
mc_gini_test("extortions", enve_final, family = "nbinom", plots = TRUE)
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
$DV
[1] "extortions"

$stat
Observed Gini Coefficient for extortions 
                               0.9478406 

$mc_mean
Monte Carlo mean 
       0.9459086 

$mc_confint
  MC 2.5%  MC 97.5% 
0.9382330 0.9531238 

$reps
Replicates 
      2000 

$mc_test
Alternative Hypothesis 
                 FALSE 

$plot
```

![plot of chunk extortions1-tests](figure/extortions1-tests-2.png)

```r
dispersion_batch("extortions", enve_final, print_option = "pandoc")
```



Table: Index of dispersion tests

               Mean   Variance   var/mu         I   p-value     df   95% Chi-sq  stars 
-----------  ------  ---------  -------  --------  --------  -----  -----------  ------
extortions    0.569      6.915   12.149   30360.2         0   2499     2616.411  ***   

```r
my_ks_test("extortions", enve_final)
```

```

	One-sample Kolmogorov-Smirnov test

data:  extortions vs. poisson
D^+ = 0.31494, p-value < 2.2e-16
alternative hypothesis: the CDF of x lies above the null hypothesis
```

```r
my_ks_test("extortions", enve_final, family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```

	One-sample Kolmogorov-Smirnov test

data:  extortions vs. nbinom
D^+ = 0.0044096, p-value = 0.9074
alternative hypothesis: the CDF of x lies above the null hypothesis
```

```r
ext_chisq_poiss <- chisq_count("extortions", data = enve_final, B = 2000,
                               family = "poisson")

ext_chisq_poiss
```

```

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  extortions vs. poisson
X-squared = 10260, df = NA, p-value = 0.0004998
```

```r
ext_chisq_poiss_tb <- data.frame(ext_chisq_poiss$observed,
                                 Exp = ext_chisq_poiss$expected)[,c(1,2,4)]

kable(ext_chisq_poiss_tb, format = "pandoc", digits = 3, 
      caption = "Poisson expected counts")
```



Table: Poisson expected counts

extortions    Freq   Exp.Freq
-----------  -----  ---------
0             2203   1414.979
1              109    805.330
2               46    229.270
3               32     43.486
4               25      6.147
5+              85      0.787

```r
ext_chisq_nbinom <- chisq_count("extortions", data = enve_final, B = 2000,
                               family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```r
ext_chisq_nbinom
```

```

	Chi-squared test for given probabilities

data:  extortions vs. nbinom
X-squared = 1.9647, df = 5, p-value = 0.854
```

```r
ext_chisq_nbinom_tb <- data.frame(ext_chisq_nbinom$observed,
                                 Exp = ext_chisq_nbinom$expected)[,c(1,2,4)]

kable(ext_chisq_nbinom_tb, format = "pandoc", digits = 3, 
      caption = "Negative binomial expected counts")
```



Table: Negative binomial expected counts

extortions    Freq   Exp.Freq
-----------  -----  ---------
0             2203   2203.701
1              109    101.755
2               46     49.207
3               32     30.664
4               25     21.709
5+              85     92.964

```r
## Neg Bin distribution parameters for extortion

MASS::fitdistr(enve_final$extortions, "Negative Binomial")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
      size           mu     
  0.050251154   0.569425017 
 (0.003721338) (0.053017432)
```

## Univariate: other extortion types

Now do the univariate analysis for the rest of the extortion variables. First for the capped counts.


```r
extortion_columns_capped <- c("typeTelephone",
                              "typeInternet",
                              "typeStreet",
                              "typePremises",
                              "typeCobro.de.piso",
                              "typeOther",
                              "simpRemote",
                              "simpIn.person",
                              "cap_count")

extortion_dist_capped <- lapply(extortion_columns_capped, victim_table,
                          data = enve_final, print_option = "pandoc")

names(extortion_dist_capped) <- extortion_columns_capped
extortion_dist_capped
```

```
$typeTelephone


Table: Victimisation distribution of typeTelephone.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         1318          --        --      52.72         --           --
      1          992         992        --      39.68     83.926       67.254
      2          121         242       121       4.84     10.237       16.407
      3           46         138        92       1.84      3.892        9.356
      4           12          48        36       0.48      1.015        3.254
      5           11          55        44       0.44      0.931        3.729

$typeInternet


Table: Victimisation distribution of typeInternet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2494          --        --      99.76         --           --
      1            6           6        --       0.24        100          100

$typeStreet


Table: Victimisation distribution of typeStreet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2446          --        --      97.84         --           --
      1           49          49        --       1.96     90.741       77.778
      2            3           6         3       0.12      5.556        9.524
      3            1           3         2       0.04      1.852        4.762
      5            1           5         4       0.04      1.852        7.937

$typePremises


Table: Victimisation distribution of typePremises.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2490          --        --      99.60         --           --
      1            7           7        --       0.28         70        43.75
      2            2           4         2       0.08         20        25.00
      5            1           5         4       0.04         10        31.25

$typeCobro.de.piso


Table: Victimisation distribution of typeCobro.de.piso.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2495          --        --      99.80         --           --
      1            4           4        --       0.16         80       57.143
      3            1           3         2       0.04         20       42.857

$typeOther


Table: Victimisation distribution of typeOther.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2472          --        --      98.88         --           --
      1           25          25        --       1.00     89.286       75.758
      2            2           4         2       0.08      7.143       12.121
      4            1           4         3       0.04      3.571       12.121

$simpRemote


Table: Victimisation distribution of simpRemote.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         1312          --        --      52.48         --           --
      1          998         998        --      39.92     84.007       67.387
      2          121         242       121       4.84     10.185       16.340
      3           46         138        92       1.84      3.872        9.318
      4           12          48        36       0.48      1.010        3.241
      5           11          55        44       0.44      0.926        3.714

$simpIn.person


Table: Victimisation distribution of simpIn.person.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2431          --        --      97.24         --           --
      1           60          60        --       2.40     86.957       69.767
      2            5          10         5       0.20      7.246       11.628
      3            2           6         4       0.08      2.899        6.977
      5            2          10         8       0.08      2.899       11.628

$cap_count


Table: Victimisation distribution of cap_count.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         1221          --        --      48.84         --           --
      1         1073        1073        --      42.92     83.894       67.062
      2          131         262       131       5.24     10.242       16.375
      3           48         144        96       1.92      3.753        9.000
      4           14          56        42       0.56      1.095        3.500
      5           13          65        52       0.52      1.016        4.062
```

```r
extortion_dist_capped_nformat <- lapply(extortion_columns_capped, victim_table,
                          data = enve_final, print_option = "none")

names(extortion_dist_capped_nformat) <- extortion_columns_capped

ext_capped_vicum <- lapply(extortion_dist_capped_nformat, victim_cumulative)

lapply(ext_capped_vicum, kable, format = "pandoc", digits = 3)
```

```
$typeTelephone


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1         47.28   100.000     100.000
      2          7.60    16.074      32.746
      3          2.76     5.838      16.339
      4          0.92     1.946       6.983
      5          0.44     0.931       3.729

$typeInternet


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          0.24       100         100

$typeStreet


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          2.16   100.000     100.000
      2          0.20     9.259      22.222
      3          0.08     3.704      12.698
      5          0.04     1.852       7.937

$typePremises


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          0.40       100      100.00
      2          0.12        30       56.25
      5          0.04        10       31.25

$typeCobro.de.piso


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0         1e+02        --          --
      1         2e-01       100     100.000
      3         4e-02        20      42.857

$typeOther


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          1.12   100.000     100.000
      2          0.12    10.714      24.242
      4          0.04     3.571      12.121

$simpRemote


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1         47.52   100.000     100.000
      2          7.60    15.993      32.613
      3          2.76     5.808      16.273
      4          0.92     1.936       6.955
      5          0.44     0.926       3.714

$simpIn.person


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          2.76   100.000     100.000
      2          0.36    13.043      30.233
      3          0.16     5.797      18.605
      5          0.08     2.899      11.628

$cap_count


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1         51.16   100.000     100.000
      2          8.24    16.106      32.938
      3          3.00     5.864      16.562
      4          1.08     2.111       7.562
      5          0.52     1.016       4.062
```


```r
lapply(extortion_columns_capped, victim_lorenz, data = enve_final,
       family = "poisson")
```

```
[[1]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-1.png)

```

[[2]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-2.png)

```

[[3]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-3.png)

```

[[4]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-4.png)

```

[[5]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-5.png)

```

[[6]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-6.png)

```

[[7]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-7.png)

```

[[8]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-8.png)

```

[[9]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-9.png)

```r
lapply(extortion_columns_capped, victim_lorenz, data = enve_final,
       family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
[[1]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-10.png)

```

[[2]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-11.png)

```

[[3]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-12.png)

```

[[4]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-13.png)

```

[[5]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-14.png)

```

[[6]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-15.png)

```

[[7]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-16.png)

```

[[8]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-17.png)

```

[[9]]
```

![plot of chunk extortion-capped-lorenz](figure/extortion-capped-lorenz-18.png)

Now do some hypothesis testing 


```r
lapply(extortion_columns_capped, mc_gini_test, data = enve_final,
       family = "poisson", plots = TRUE) 
```

```
[[1]]
[[1]]$DV
[1] "typeTelephone"

[[1]]$stat
Observed Gini Coefficient for typeTelephone 
                                  0.6098617 

[[1]]$mc_mean
Monte Carlo mean 
       0.6383809 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6213117 0.6548540 

[[1]]$reps
Replicates 
      2000 

[[1]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[1]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-1.png)

```


[[2]]
[[2]]$DV
[1] "typeInternet"

[[2]]$stat
Observed Gini Coefficient for typeInternet 
                                    0.9976 

[[2]]$mc_mean
Monte Carlo mean 
             NaN 

[[2]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9956   0.9992 

[[2]]$reps
Replicates 
      2000 

[[2]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[2]]$plot
```

```
Warning: Removed 1 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-2.png)

```


[[3]]
[[3]]$DV
[1] "typeStreet"

[[3]]$stat
Observed Gini Coefficient for typeStreet 
                               0.9812889 

[[3]]$mc_mean
Monte Carlo mean 
       0.9755845 

[[3]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9692   0.9816 

[[3]]$reps
Replicates 
      2000 

[[3]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[3]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-3.png)

```


[[4]]
[[4]]$DV
[1] "typePremises"

[[4]]$stat
Observed Gini Coefficient for typePremises 
                                    0.9972 

[[4]]$mc_mean
Monte Carlo mean 
       0.9936518 

[[4]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9904   0.9964 

[[4]]$reps
Replicates 
      2000 

[[4]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[4]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-4.png)

```


[[5]]
[[5]]$DV
[1] "typeCobro.de.piso"

[[5]]$stat
Observed Gini Coefficient for typeCobro.de.piso 
                                      0.9984571 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9952   0.9992 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 2 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-5.png)

```


[[6]]
[[6]]$DV
[1] "typeOther"

[[6]]$stat
Observed Gini Coefficient for typeOther 
                              0.9903636 

[[6]]$mc_mean
Monte Carlo mean 
       0.9869177 

[[6]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9820   0.9912 

[[6]]$reps
Replicates 
      2000 

[[6]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[6]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-6.png)

```


[[7]]
[[7]]$DV
[1] "simpRemote"

[[7]]$stat
Observed Gini Coefficient for simpRemote 
                               0.6076016 

[[7]]$mc_mean
Monte Carlo mean 
       0.6375019 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6212883 0.6540014 

[[7]]$reps
Replicates 
      2000 

[[7]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[7]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-7.png)

```


[[8]]
[[8]]$DV
[1] "simpIn.person"

[[8]]$stat
Observed Gini Coefficient for simpIn.person 
                                  0.9773674 

[[8]]$mc_mean
Monte Carlo mean 
       0.9666547 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9599321 0.9736000 

[[8]]$reps
Replicates 
      2000 

[[8]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[8]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-8.png)

```


[[9]]
[[9]]$DV
[1] "cap_count"

[[9]]$stat
Observed Gini Coefficient for cap_count 
                                 0.5788 

[[9]]$mc_mean
Monte Carlo mean 
       0.6212388 

[[9]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6049346 0.6377043 

[[9]]$reps
Replicates 
      2000 

[[9]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[9]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-9.png)

```r
lapply(extortion_columns_capped, mc_gini_test, data = enve_final,
       family = "nbinom", plots = TRUE) 
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
[[1]]
[[1]]$DV
[1] "typeTelephone"

[[1]]$stat
Observed Gini Coefficient for typeTelephone 
                                  0.6098617 

[[1]]$mc_mean
Monte Carlo mean 
        0.640116 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6229753 0.6561968 

[[1]]$reps
Replicates 
      2000 

[[1]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[1]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-10.png)

```


[[2]]
[[2]]$DV
[1] "typeInternet"

[[2]]$stat
Observed Gini Coefficient for typeInternet 
                                    0.9976 

[[2]]$mc_mean
Monte Carlo mean 
             NaN 

[[2]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9956   0.9992 

[[2]]$reps
Replicates 
      2000 

[[2]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[2]]$plot
```

```
Warning: Removed 8 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-11.png)

```


[[3]]
[[3]]$DV
[1] "typeStreet"

[[3]]$stat
Observed Gini Coefficient for typeStreet 
                               0.9812889 

[[3]]$mc_mean
Monte Carlo mean 
       0.9811021 

[[3]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9755261 0.9861737 

[[3]]$reps
Replicates 
      2000 

[[3]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[3]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-12.png)

```


[[4]]
[[4]]$DV
[1] "typePremises"

[[4]]$stat
Observed Gini Coefficient for typePremises 
                                    0.9972 

[[4]]$mc_mean
Monte Carlo mean 
             NaN 

[[4]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9947763 0.9986231 

[[4]]$reps
Replicates 
      2000 

[[4]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[4]]$plot
```

```
Warning: Removed 1 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-13.png)

```


[[5]]
[[5]]$DV
[1] "typeCobro.de.piso"

[[5]]$stat
Observed Gini Coefficient for typeCobro.de.piso 
                                      0.9984571 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9966875 0.9996000 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 14 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-14.png)

```


[[6]]
[[6]]$DV
[1] "typeOther"

[[6]]$stat
Observed Gini Coefficient for typeOther 
                              0.9903636 

[[6]]$mc_mean
Monte Carlo mean 
       0.9902688 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9864642 0.9939273 

[[6]]$reps
Replicates 
      2000 

[[6]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[6]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-15.png)

```


[[7]]
[[7]]$DV
[1] "simpRemote"

[[7]]$stat
Observed Gini Coefficient for simpRemote 
                               0.6076016 

[[7]]$mc_mean
Monte Carlo mean 
       0.6383951 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6220518 0.6549861 

[[7]]$reps
Replicates 
      2000 

[[7]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[7]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-16.png)

```


[[8]]
[[8]]$DV
[1] "simpIn.person"

[[8]]$stat
Observed Gini Coefficient for simpIn.person 
                                  0.9773674 

[[8]]$mc_mean
Monte Carlo mean 
       0.9770859 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9713352 0.9825794 

[[8]]$reps
Replicates 
      2000 

[[8]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[8]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-17.png)

```


[[9]]
[[9]]$DV
[1] "cap_count"

[[9]]$stat
Observed Gini Coefficient for cap_count 
                                 0.5788 

[[9]]$mc_mean
Monte Carlo mean 
       0.6225353 

[[9]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6055262 0.6392918 

[[9]]$reps
Replicates 
      2000 

[[9]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[9]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-18.png)

```r
dispersion_batch(extortion_columns_capped, enve_final, print_option = "pandoc")
```



Table: Index of dispersion tests

                      Mean   Variance   var/mu          I   p-value     df   95% Chi-sq  stars 
------------------  ------  ---------  -------  ---------  --------  -----  -----------  ------
typeTelephone        0.590      0.595    1.008   2519.915     0.380   2499     2616.411        
typeInternet         0.002      0.002    0.998   2494.000     0.524   2499     2616.411        
typeStreet           0.025      0.037    1.483   3706.841     0.000   2499     2616.411  ***   
typePremises         0.006      0.016    2.495   6234.000     0.000   2499     2616.411  ***   
typeCobro.de.piso    0.003      0.005    1.855   4635.857     0.000   2499     2616.411  ***   
typeOther            0.013      0.019    1.472   3679.121     0.000   2499     2616.411  ***   
simpRemote           0.592      0.595    1.004   2507.859     0.446   2499     2616.411        
simpIn.person        0.034      0.058    1.687   4216.326     0.000   2499     2616.411  ***   
cap_count            0.640      0.622    0.972   2428.125     0.842   2499     2616.411        

```r
ks_test_batch(extortion_columns_capped, enve_final, print_option = "pandoc", 
              family = "poisson")
```



Table: Kolmogorov-Smirnov Tests

                                 KS.Statistic   p-value  stars 
------------------------------  -------------  --------  ------
typeTelephone vs. poisson               0.043     0.000  ***   
typeInternet vs. poisson                0.000     1.000        
typeStreet vs. poisson                  0.003     0.949        
typePremises vs. poisson                0.002     0.970        
typeCobro.de.piso vs. poisson           0.001     0.997        
typeOther vs. poisson                   0.002     0.981        
simpRemote vs. poisson                  0.043     0.000  ***   
simpIn.person vs. poisson               0.006     0.837        
cap_count vs. poisson                   0.052     0.000  ***   

```r
ks_test_batch(extortion_columns_capped, enve_final, print_option = "pandoc", 
              family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```



Table: Kolmogorov-Smirnov Tests

                                KS.Statistic   p-value  stars 
-----------------------------  -------------  --------  ------
typeTelephone vs. nbinom               0.043     0.000  ***   
typeInternet vs. nbinom                0.000     1.000        
typeStreet vs. nbinom                  0.001     0.996        
typePremises vs. nbinom                0.000     1.000        
typeCobro.de.piso vs. nbinom           0.000     1.000        
typeOther vs. nbinom                   0.000     0.999        
simpRemote vs. nbinom                  0.044     0.000  ***   
simpIn.person vs. nbinom               0.002     0.988        
cap_count vs. nbinom                   0.053     0.000  ***   

```r
# poisson expected chisq

ext_cap_chisq_p <- lapply(extortion_columns_capped, function(x)
{
    tryCatch(chisq_count(x, data = enve_final, B = 2000), 
             error = function(e) NULL)
})

ext_cap_chisq_p
```

```
[[1]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeTelephone vs. poisson
X-squared = 215.13, df = NA, p-value = 0.0004998


[[2]]

	Chi-squared test for given probabilities

data:  typeInternet vs. poisson
X-squared = 4.1802e-06, df = 1, p-value = 0.9984


[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeStreet vs. poisson
X-squared = 538.4, df = NA, p-value = 0.0004998


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typePremises vs. poisson
X-squared = 172.12, df = NA, p-value = 0.0004998


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeCobro.de.piso vs. poisson
X-squared = 68.243, df = NA, p-value = 0.003998


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeOther vs. poisson
X-squared = 2014.7, df = NA, p-value = 0.0004998


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpRemote vs. poisson
X-squared = 212.9, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpIn.person vs. poisson
X-squared = 4257.6, df = NA, p-value = 0.0004998


[[9]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  cap_count vs. poisson
X-squared = 244.13, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_cap_chisq_p), format = "pandoc", digits = 3)
```

                                Chi-sq         Cramer's V   df   p.value        stars 
------------------------------  -------------  -----------  ---  -------------  ------
typeTelephone vs. poisson       215.1317       --           --   0.0004997501   ***   
typeInternet vs. poisson        4.180166e-06   --           1    0.9983687            
typeStreet vs. poisson          538.3998       --           --   0.0004997501   ***   
typePremises vs. poisson        172.1237       --           --   0.0004997501   ***   
typeCobro.de.piso vs. poisson   68.24345       --           --   0.003998001    **    
typeOther vs. poisson           2014.659       --           --   0.0004997501   ***   
simpRemote vs. poisson          212.8994       --           --   0.0004997501   ***   
simpIn.person vs. poisson       4257.58        --           --   0.0004997501   ***   
cap_count vs. poisson           244.1287       --           --   0.0004997501   ***   

```r
ext_cap_p_tb <- lapply(ext_cap_chisq_p, function(x)
    {
    kable(data.frame(x$observed, Exp = x$expected)[,c(1,2,4)],
          format = "pandoc", digits = 3, caption = "Expected: Poisson")
    })


print_kables(ext_cap_p_tb)
```

```


Table: Expected: Poisson

typeTelephone    Freq   Exp.Freq
--------------  -----  ---------
0                1318   1386.719
1                 992    816.908
2                 121    240.798
3                  46     47.741
4                  12      6.917
5                  11      0.918


Table: Expected: Poisson

typeInternet    Freq   Exp.Freq
-------------  -----  ---------
0               2494   2494.005
1                  6      5.995


Table: Expected: Poisson

typeStreet    Freq   Exp.Freq
-----------  -----  ---------
0             2446   2437.834
1               49     61.394
2                3      0.765
3+               2      0.008


Table: Expected: Poisson

typePremises    Freq   Exp.Freq
-------------  -----  ---------
0               2490   2484.050
1                  7     15.899
2+                 3      0.052


Table: Expected: Poisson

typeCobro.de.piso    Freq   Exp.Freq
------------------  -----  ---------
0                    2495   2493.021
1                       4      6.965
3                       1      0.014


Table: Expected: Poisson

typeOther    Freq   Exp.Freq
----------  -----  ---------
0            2472   2467.359
1              25     32.427
2               2      0.214
4               1      0.000


Table: Expected: Poisson

simpRemote    Freq   Exp.Freq
-----------  -----  ---------
0             1312   1383.010
1              998    817.925
2              121    243.064
3               46     47.995
4               12      7.045
5               11      0.962


Table: Expected: Poisson

simpIn.person    Freq   Exp.Freq
--------------  -----  ---------
0                2431   2415.624
1                  60     82.947
2                   5      1.412
3                   2      0.016
5                   2      0.001


Table: Expected: Poisson

cap_count    Freq   Exp.Freq
----------  -----  ---------
0            1221   1317.779
1            1073    844.380
2             131    269.508
3              48     57.731
4              14      9.244
5              13      1.357
```

```r
# now for negbin

ext_cap_chisq_nb <- lapply(extortion_columns_capped, function(x)
{
    tryCatch(chisq_count(x, data = enve_final, B = 2000, family = "nbinom"), 
             error = function(e) NULL)
})
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```r
ext_cap_chisq_nb
```

```
[[1]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeTelephone vs. nbinom
X-squared = 205.67, df = NA, p-value = 0.0004998


[[2]]

	Chi-squared test for given probabilities

data:  typeInternet vs. nbinom
X-squared = 1.3553e-05, df = 1, p-value = 0.9971


[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeStreet vs. nbinom
X-squared = 4.0991, df = NA, p-value = 0.3418


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typePremises vs. nbinom
X-squared = 0.1349, df = NA, p-value = 1


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeCobro.de.piso vs. nbinom
X-squared = 0.066947, df = NA, p-value = 1


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeOther vs. nbinom
X-squared = 0.62577, df = NA, p-value = 0.9195


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpRemote vs. nbinom
X-squared = 209.05, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpIn.person vs. nbinom
X-squared = 4.5127, df = NA, p-value = 0.3088


[[9]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  cap_count vs. nbinom
X-squared = 241.12, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_cap_chisq_nb), format = "pandoc", digits = 3)
```

                               Chi-sq         Cramer's V   df   p.value        stars 
-----------------------------  -------------  -----------  ---  -------------  ------
typeTelephone vs. nbinom       205.6743       --           --   0.0004997501   ***   
typeInternet vs. nbinom        1.355276e-05   --           1    0.9970627            
typeStreet vs. nbinom          4.099137       --           --   0.3418291            
typePremises vs. nbinom        0.1348991      --           --   1                    
typeCobro.de.piso vs. nbinom   0.06694651     --           --   1                    
typeOther vs. nbinom           0.6257735      --           --   0.9195402            
simpRemote vs. nbinom          209.0496       --           --   0.0004997501   ***   
simpIn.person vs. nbinom       4.512728       --           --   0.3088456            
cap_count vs. nbinom           241.1167       --           --   0.0004997501   ***   

```r
ext_cap_nb_tb <- lapply(ext_cap_chisq_nb, function(x)
    {
    kable(data.frame(x$observed, Exp = x$expected)[,c(1,2,4)],
          format = "pandoc", digits = 3, caption = "Expected: negbin")
    })


print_kables(ext_cap_nb_tb)
```

```


Table: Expected: negbin

typeTelephone    Freq   Exp.Freq
--------------  -----  ---------
0                1318   1388.849
1                 992    812.857
2                 121    241.569
3                  46     48.370
4                  12      7.349
5                  11      1.006


Table: Expected: negbin

typeInternet    Freq   Exp.Freq
-------------  -----  ---------
0               2494   2494.009
1                  6      5.991


Table: Expected: negbin

typeStreet    Freq   Exp.Freq
-----------  -----  ---------
0             2446   2446.142
1               49     46.416
2                3      6.169
3                1      1.028
5                1      0.245


Table: Expected: negbin

typePremises    Freq   Exp.Freq
-------------  -----  ---------
0               2490   2489.940
1                  7      6.704
2                  2      1.944
5                  1      1.412


Table: Expected: negbin

typeCobro.de.piso    Freq   Exp.Freq
------------------  -----  ---------
0                    2495   2494.923
1                       4      3.808
3                       1      1.269


Table: Expected: negbin

typeOther    Freq   Exp.Freq
----------  -----  ---------
0            2472   2471.892
1              25     24.018
2               2      3.314
4               1      0.776


Table: Expected: negbin

simpRemote    Freq   Exp.Freq
-----------  -----  ---------
0             1312   1384.081
1              998    817.116
2              121    242.564
3               46     48.023
4               12      7.225
5               11      0.991


Table: Expected: negbin

simpIn.person    Freq   Exp.Freq
--------------  -----  ---------
0                2431   2430.947
1                  60     56.015
2                   5      9.950
3                   2      2.275
5                   2      0.813


Table: Expected: negbin

cap_count    Freq   Exp.Freq
----------  -----  ---------
0            1221   1321.363
1            1073    839.643
2             131    269.610
3              48     58.349
4              14      9.605
5              13      1.431
```



Now do the estimated counts


```r
extortion_columns_estimated <- c("estTel",
                              "estInternet",
                              "estStreet",
                              "estPremises",
                              "estPiso",
                              "estOther",
                              "estRemote",
                              "estIn.person")

extortion_dist_estimated <- lapply(extortion_columns_estimated, function(x) 
                                {
                                    if(mean(enve_final[,x]) == 0){message(paste0("skipping ", x, ". No obs."))}
                                    else{victim_table(x, data = enve_final, print_option = "pandoc")}
                                })
```

```
skipping estInternet. No obs.
```

```r
names(extortion_dist_estimated) <- extortion_columns_estimated

extortion_dist_estimated
```

```
$estTel


Table: Victimisation distribution of estTel.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2359          --        --      94.36         --           --
      1           58          58        --       2.32     41.135        8.406
      2           19          38        19       0.76     13.475        5.507
      3           15          45        30       0.60     10.638        6.522
      4           12          48        36       0.48      8.511        6.957
      5            5          25        20       0.20      3.546        3.623
      6            3          18        15       0.12      2.128        2.609
      7            3          21        18       0.12      2.128        3.043
      8            1           8         7       0.04      0.709        1.159
      9            4          36        32       0.16      2.837        5.217
     10            1          10         9       0.04      0.709        1.449
     11            1          11        10       0.04      0.709        1.594
     12            2          24        22       0.08      1.418        3.478
     13            2          26        24       0.08      1.418        3.768
     14            2          28        26       0.08      1.418        4.058
     15            2          30        28       0.08      1.418        4.348
     19            2          38        36       0.08      1.418        5.507
     20            2          40        38       0.08      1.418        5.797
     21            2          42        40       0.08      1.418        6.087
     22            1          22        21       0.04      0.709        3.188
     25            1          25        24       0.04      0.709        3.623
     32            2          64        62       0.08      1.418        9.275
     33            1          33        32       0.04      0.709        4.783

$estInternet
NULL

$estStreet


Table: Victimisation distribution of estStreet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2497          --        --      99.88         --           --
      1            2           2        --       0.08     66.667        6.897
     27            1          27        26       0.04     33.333       93.103

$estPremises


Table: Victimisation distribution of estPremises.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2499          --        --      99.96         --           --
      2            1           2        --       0.04        100          100

$estPiso


Table: Victimisation distribution of estPiso.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2499          --        --      99.96         --           --
      4            1           4        --       0.04        100          100

$estOther


Table: Victimisation distribution of estOther.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2495          --        --      99.80         --           --
      1            4           4        --       0.16         80           50
      4            1           4         3       0.04         20           50

$estRemote


Table: Victimisation distribution of estRemote.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2359          --        --      94.36         --           --
      1           58          58        --       2.32     41.135        8.406
      2           19          38        19       0.76     13.475        5.507
      3           15          45        30       0.60     10.638        6.522
      4           12          48        36       0.48      8.511        6.957
      5            5          25        20       0.20      3.546        3.623
      6            3          18        15       0.12      2.128        2.609
      7            3          21        18       0.12      2.128        3.043
      8            1           8         7       0.04      0.709        1.159
      9            4          36        32       0.16      2.837        5.217
     10            1          10         9       0.04      0.709        1.449
     11            1          11        10       0.04      0.709        1.594
     12            2          24        22       0.08      1.418        3.478
     13            2          26        24       0.08      1.418        3.768
     14            2          28        26       0.08      1.418        4.058
     15            2          30        28       0.08      1.418        4.348
     19            2          38        36       0.08      1.418        5.507
     20            2          40        38       0.08      1.418        5.797
     21            2          42        40       0.08      1.418        6.087
     22            1          22        21       0.04      0.709        3.188
     25            1          25        24       0.04      0.709        3.623
     32            2          64        62       0.08      1.418        9.275
     33            1          33        32       0.04      0.709        4.783

$estIn.person


Table: Victimisation distribution of estIn.person.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2495          --        --      99.80         --           --
      1            2           2        --       0.08         40        5.714
      2            1           2         1       0.04         20        5.714
      4            1           4         3       0.04         20       11.429
     27            1          27        26       0.04         20       77.143
```

```r
extortion_dist_estimated_nfor <- lapply(extortion_columns_estimated, function(x) 
                                {
                                    if(mean(enve_final[,x]) == 0){message(paste0("skipping ", x, ". No obs."))}
                                    else{victim_table(x, data = enve_final, print_option = "none")}
                                })
```

```
skipping estInternet. No obs.
```

```r
names(extortion_dist_estimated_nfor) <- extortion_columns_estimated

ext_estim_vicum <- lapply(extortion_dist_estimated_nfor,
                          function(x) {if(is.null(x)){message("skipping NULL objects")}
                              else {victim_cumulative(x)}}
                          )
```

```
skipping NULL objects
```

```r
print_kables(lapply(ext_estim_vicum, 
       function(x) {if(is.null(x)){message("skipping NULL objects")}
                    else {kable(x, format = "pandoc", digits = 3)}}))
```

```
skipping NULL objects
```

```


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          5.64   100.000     100.000
      2          3.32    58.865      91.594
      3          2.56    45.390      86.087
      4          1.96    34.752      79.565
      5          1.48    26.241      72.609
      6          1.28    22.695      68.986
      7          1.16    20.567      66.377
      8          1.04    18.440      63.333
      9          1.00    17.730      62.174
     10          0.84    14.894      56.957
     11          0.80    14.184      55.507
     12          0.76    13.475      53.913
     13          0.68    12.057      50.435
     14          0.60    10.638      46.667
     15          0.52     9.220      42.609
     19          0.44     7.801      38.261
     20          0.36     6.383      32.754
     21          0.28     4.965      26.957
     22          0.20     3.546      20.870
     25          0.16     2.837      17.681
     32          0.12     2.128      14.058
     33          0.04     0.709       4.783
NULL


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          0.12   100.000     100.000
     27          0.04    33.333      93.103


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0         1e+02        --          --
      2         4e-02       100         100


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0         1e+02        --          --
      4         4e-02       100         100


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0         1e+02        --          --
      1         2e-01       100         100
      4         4e-02        20          50


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          5.64   100.000     100.000
      2          3.32    58.865      91.594
      3          2.56    45.390      86.087
      4          1.96    34.752      79.565
      5          1.48    26.241      72.609
      6          1.28    22.695      68.986
      7          1.16    20.567      66.377
      8          1.04    18.440      63.333
      9          1.00    17.730      62.174
     10          0.84    14.894      56.957
     11          0.80    14.184      55.507
     12          0.76    13.475      53.913
     13          0.68    12.057      50.435
     14          0.60    10.638      46.667
     15          0.52     9.220      42.609
     19          0.44     7.801      38.261
     20          0.36     6.383      32.754
     21          0.28     4.965      26.957
     22          0.20     3.546      20.870
     25          0.16     2.837      17.681
     32          0.12     2.128      14.058
     33          0.04     0.709       4.783


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0        100.00        --          --
      1          0.20       100     100.000
      2          0.12        60      94.286
      4          0.08        40      88.571
     27          0.04        20      77.143
```


```r
lapply(extortion_columns_estimated, function(x)
{
    tryCatch(victim_lorenz(x, data = enve_final, family = "poisson"),
             error = function(e) NULL)
})
```

```
[[1]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-1.png)

```

[[2]]
NULL

[[3]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-2.png)

```

[[4]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-3.png)

```

[[5]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-4.png)

```

[[6]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-5.png)

```

[[7]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-6.png)

```

[[8]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-7.png)

```r
lapply(extortion_columns_estimated, function(x)
{
    tryCatch(victim_lorenz(x, data = enve_final, family = "nbinom"),
             error = function(e) NULL)
})
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
[[1]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-8.png)

```

[[2]]
NULL

[[3]]
NULL

[[4]]
NULL

[[5]]
NULL

[[6]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-9.png)

```

[[7]]
```

![plot of chunk extortion-estimated-lorenz](figure/extortion-estimated-lorenz-10.png)

```

[[8]]
NULL
```



```r
lapply(extortion_columns_estimated, function(x)
{
    tryCatch(mc_gini_test(x, data = enve_final, family = "poisson",
                          plots = TRUE), error = function(e) NULL)
}) 
```

```
[[1]]
[[1]]$DV
[1] "estTel"

[[1]]$stat
Observed Gini Coefficient for estTel 
                           0.9765577 

[[1]]$mc_mean
Monte Carlo mean 
       0.7852879 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.7693921 0.8012621 

[[1]]$reps
Replicates 
      2000 

[[1]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[1]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-1.png)

```


[[2]]
[[2]]$DV
[1] "estInternet"

[[2]]$stat
Observed Gini Coefficient for estInternet 
                                      NaN 

[[2]]$mc_mean
Monte Carlo mean 
             NaN 

[[2]]$mc_confint
 MC 2.5% MC 97.5% 
      NA       NA 

[[2]]$reps
Replicates 
      2000 

[[2]]$mc_test
Alternative Hypothesis 
                    NA 

[[2]]$plot
```

```
Warning: Removed 2000 rows containing non-finite values (stat_density).
```

```
Warning: Removed 2 rows containing missing values (geom_vline).
```

```
Warning: Removed 1 rows containing missing values (geom_vline).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-2.png)

```


[[3]]
[[3]]$DV
[1] "estStreet"

[[3]]$stat
Observed Gini Coefficient for estStreet 
                              0.9995172 

[[3]]$mc_mean
Monte Carlo mean 
         0.98856 

[[3]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9843805 0.9924000 

[[3]]$reps
Replicates 
      2000 

[[3]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[3]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-3.png)

```


[[4]]
[[4]]$DV
[1] "estPremises"

[[4]]$stat
Observed Gini Coefficient for estPremises 
                                   0.9996 

[[4]]$mc_mean
Monte Carlo mean 
             NaN 

[[4]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9980   0.9996 

[[4]]$reps
Replicates 
      2000 

[[4]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[4]]$plot
```

```
Warning: Removed 268 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-4.png)

```


[[5]]
[[5]]$DV
[1] "estPiso"

[[5]]$stat
Observed Gini Coefficient for estPiso 
                               0.9996 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9968   0.9996 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 33 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-5.png)

```


[[6]]
[[6]]$DV
[1] "estOther"

[[6]]$stat
Observed Gini Coefficient for estOther 
                                0.9986 

[[6]]$mc_mean
Monte Carlo mean 
       0.9968204 

[[6]]$mc_confint
 MC 2.5% MC 97.5% 
  0.9944   0.9988 

[[6]]$reps
Replicates 
      2000 

[[6]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[6]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-6.png)

```


[[7]]
[[7]]$DV
[1] "estRemote"

[[7]]$stat
Observed Gini Coefficient for estRemote 
                              0.9765577 

[[7]]$mc_mean
Monte Carlo mean 
       0.7856741 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.7699903 0.8005522 

[[7]]$reps
Replicates 
      2000 

[[7]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[7]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-7.png)

```


[[8]]
[[8]]$DV
[1] "estIn.person"

[[8]]$stat
Observed Gini Coefficient for estIn.person 
                                 0.9992571 

[[8]]$mc_mean
Monte Carlo mean 
       0.9860952 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9816000 0.9904092 

[[8]]$reps
Replicates 
      2000 

[[8]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[8]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-8.png)

```r
lapply(extortion_columns_estimated, function(x)
{
    tryCatch(mc_gini_test(x, data = enve_final, family = "nbinom",
                          plots = TRUE), error = function(e) NULL)
}) 
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
[[1]]
[[1]]$DV
[1] "estTel"

[[1]]$stat
Observed Gini Coefficient for estTel 
                           0.9765577 

[[1]]$mc_mean
Monte Carlo mean 
       0.9745991 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9693358 0.9794966 

[[1]]$reps
Replicates 
      2000 

[[1]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[1]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-9.png)

```


[[2]]
NULL

[[3]]
NULL

[[4]]
NULL

[[5]]
NULL

[[6]]
[[6]]$DV
[1] "estOther"

[[6]]$stat
Observed Gini Coefficient for estOther 
                                0.9986 

[[6]]$mc_mean
Monte Carlo mean 
             NaN 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9967783 0.9996000 

[[6]]$reps
Replicates 
      2000 

[[6]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[6]]$plot
```

```
Warning: Removed 10 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-10.png)

```


[[7]]
[[7]]$DV
[1] "estRemote"

[[7]]$stat
Observed Gini Coefficient for estRemote 
                              0.9765577 

[[7]]$mc_mean
Monte Carlo mean 
       0.9746104 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9694272 0.9794506 

[[7]]$reps
Replicates 
      2000 

[[7]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[7]]$plot
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-11.png)

```


[[8]]
NULL
```

```r
dispersion_batch(extortion_columns_estimated, enve_final, print_option = "pandoc")
```



Table: Index of dispersion tests

                 Mean   Variance   var/mu          I   p-value     df   95% Chi-sq  stars 
-------------  ------  ---------  -------  ---------  --------  -----  -----------  ------
estTel          0.276      3.753   13.596   33976.67         0   2499     2616.411  ***   
estInternet     0.000      0.000       --         --        --   2499     2616.411  --    
estStreet       0.012      0.292   25.205   62988.24         0   2499     2616.411  ***   
estPremises     0.001      0.002    2.000    4998.00         0   2499     2616.411  ***   
estPiso         0.002      0.006    4.000    9996.00         0   2499     2616.411  ***   
estOther        0.003      0.008    2.498    6242.00         0   2499     2616.411  ***   
estRemote       0.276      3.753   13.596   33976.67         0   2499     2616.411  ***   
estIn.person    0.014      0.300   21.452   53607.86         0   2499     2616.411  ***   

```r
ks_test_batch(extortion_columns_estimated, enve_final, print_option = "pandoc", 
              family = "poisson")
```



Table: Kolmogorov-Smirnov Tests

                            KS.Statistic   p-value  stars 
-------------------------  -------------  --------  ------
estTel vs. poisson                 0.185     0.000  ***   
estInternet vs. poisson            0.000     1.000        
estStreet vs. poisson              0.011     0.574        
estPremises vs. poisson            0.000     0.999        
estPiso vs. poisson                0.001     0.993        
estOther vs. poisson               0.001     0.993        
estRemote vs. poisson              0.185     0.000  ***   
estIn.person vs. poisson           0.012     0.491        

```r
ks_test_batch(extortion_columns_estimated, enve_final, print_option = "pandoc", 
              family = "nbinom")
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```
Error in stats::optim(x = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, : non-finite finite-difference value [2]
```

```r
# poisson expected chisq

ext_est_chisq_p <- lapply(extortion_columns_estimated, function(x)
{
    tryCatch(chisq_count(x, data = enve_final, B = 2000), 
             error = function(e) NULL)
})

ext_est_chisq_p
```

```
[[1]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estTel vs. poisson
X-squared = 43564, df = NA, p-value = 0.0004998


[[2]]
NULL

[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estStreet vs. poisson
X-squared = 29.516, df = NA, p-value = 0.0004998


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estPremises vs. poisson
X-squared = 0.49627, df = NA, p-value = 0.7251


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estPiso vs. poisson
X-squared = 2.2292, df = NA, p-value = 0.1889


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estOther vs. poisson
X-squared = 90.932, df = NA, p-value = 0.001499


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estRemote vs. poisson
X-squared = 64469, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estIn.person vs. poisson
X-squared = 2695.7, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_est_chisq_p), format = "pandoc", digits = 3)
```

                           Chi-sq      Cramer's V   df   p.value        stars 
-------------------------  ----------  -----------  ---  -------------  ------
estTel vs. poisson         43563.8     --           --   0.0004997501   ***   
estStreet vs. poisson      29.51562    --           --   0.0004997501   ***   
estPremises vs. poisson    0.4962747   --           --   0.7251374            
estPiso vs. poisson        2.229179    --           --   0.1889055            
estOther vs. poisson       90.93239    --           --   0.00149925     **    
estRemote vs. poisson      64469.37    --           --   0.0004997501   ***   
estIn.person vs. poisson   2695.709    --           --   0.0004997501   ***   

```r
ext_est_p_tb <- lapply(ext_est_chisq_p, function(x)
    {
    if(is.null(x)) {"skipping null objects"}
    else
    {
        kable(data.frame(x$observed, Exp = x$expected)[,c(1,2,4)],
          format = "pandoc", digits = 3, caption = "Expected: Poisson")
    }
    })


print_kables(ext_est_p_tb)
```

```


Table: Expected: Poisson

estTel    Freq   Exp.Freq
-------  -----  ---------
0         2359   1896.961
1           58    524.010
2           19     71.917
3           15      6.606
4           12      0.475
5+          37      0.032
[1] "skipping null objects"


Table: Expected: Poisson

estStreet    Freq   Exp.Freq
----------  -----  ---------
0            2497   2471.302
1               2     28.542
27              1      0.156


Table: Expected: Poisson

estPremises    Freq   Exp.Freq
------------  -----  ---------
0              2499   2498.006
2                 1      1.994


Table: Expected: Poisson

estPiso    Freq   Exp.Freq
--------  -----  ---------
0          2499   2496.026
4             1      3.974


Table: Expected: Poisson

estOther    Freq   Exp.Freq
---------  -----  ---------
0           2495   2491.977
1              4      8.011
4              1      0.011


Table: Expected: Poisson

estRemote    Freq   Exp.Freq
----------  -----  ---------
0            2359   1897.180
1              58    523.361
2              19     72.396
3              15      6.586
4              12      0.456
5+             37      0.022


Table: Expected: Poisson

estIn.person    Freq   Exp.Freq
-------------  -----  ---------
0               2495   2465.477
1                  2     34.275
2                  1      0.247
3+                 2      0.002
```

```r
# nbinom expected chisq

ext_est_chisq_nb <- lapply(extortion_columns_estimated, function(x)
{
    tryCatch(chisq_count(x, data = enve_final, B = 2000, 
                         family = "nbinom"), 
             error = function(e) NULL)
})
```

```
Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced

Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

```r
ext_est_chisq_nb
```

```
[[1]]

	Chi-squared test for given probabilities

data:  estTel vs. nbinom
X-squared = 4.0463, df = 5, p-value = 0.5428


[[2]]
NULL

[[3]]
NULL

[[4]]
NULL

[[5]]
NULL

[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estOther vs. nbinom
X-squared = 0.40663, df = NA, p-value = 0.8171


[[7]]

	Chi-squared test for given probabilities

data:  estRemote vs. nbinom
X-squared = 4.1815, df = 5, p-value = 0.5236


[[8]]
NULL
```

```r
kable(chisq_tb(ext_est_chisq_nb), format = "pandoc", digits = 3)
```

                       Chi-sq      Cramer's V   df   p.value     stars 
---------------------  ----------  -----------  ---  ----------  ------
estTel vs. nbinom      4.046263    --           5    0.5427746         
estOther vs. nbinom    0.4066327   --           --   0.8170915         
estRemote vs. nbinom   4.181452    --           5    0.5235982         

```r
ext_est_nb_tb <- lapply(ext_est_chisq_nb, function(x)
    {
    if(is.null(x)) {"skipping null objects"}
    else
    {
      kable(data.frame(x$observed, Exp = x$expected)[,c(1,2,4)],
          format = "pandoc", digits = 3, caption = "Expected: nbinom")  
    }
    })


print_kables(ext_est_nb_tb)
```

```


Table: Expected: nbinom

estTel    Freq   Exp.Freq
-------  -----  ---------
0         2359   2359.282
1           58     48.933
2           19     23.092
3           15     14.261
4           12     10.055
5+          37     44.377
[1] "skipping null objects"
[1] "skipping null objects"
[1] "skipping null objects"
[1] "skipping null objects"


Table: Expected: nbinom

estOther    Freq   Exp.Freq
---------  -----  ---------
0           2495   2494.981
1              4      3.339
4              1      1.680


Table: Expected: nbinom

estRemote    Freq   Exp.Freq
----------  -----  ---------
0            2359   2359.366
1              58     48.590
2              19     23.133
3              15     14.508
4              12     10.041
5+             37     44.362
[1] "skipping null objects"
```

## IQV

Calculate the index of qualitative variation per business, then analyse these figures.


```r
cols <- c("typeTelephone", "typeInternet", "typeStreet",
          "typePremises", "typeCobro.de.piso", "typeOther")

iqv_long <- iqv(enve_final[,cols])

iqv_long
```

```
[1] 17.7645
```

```r
enve_final$iqv_long <- apply(enve_final[,cols], 1, iqv)

enve_final$iqv_long <- round(enve_final$iqv_long, 2)

summary(enve_final$iqv_long)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2645  0.0000 60.0000    1221 
```

```r
# now for simp cats

cols_simp <- c("simpRemote", "simpIn.person", "simpOther")

iqv_simp <- iqv(enve_final[,cols_simp])

iqv_simp
```

```
[1] 20.98559
```

```r
enve_final$iqv_simp <- apply(enve_final[,cols_simp], 1, iqv)

enve_final$iqv_simp <- round(enve_final$iqv_simp, 2)

summary(enve_final$iqv_simp)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3307  0.0000 75.0000    1221 
```

```r
## Contingency tables to extortion frequency

batch_iqvs <- batch_chisq(enve_final, "extortions", 
                          c("iqv_long", "iqv_simp"))

kable(chisq_tb(batch_iqvs, stars = TRUE), "pandoc", 
      caption = "Chi-squared tests between 'extortions' and 'IQVs'" )
```



Table: Chi-squared tests between 'extortions' and 'IQVs'

                          Chi-sq     Cramer's V   df   p.value      stars 
------------------------  ---------  -----------  ---  -----------  ------
extortions vs. iqv_long   63.66355   0.12881      --   0.07696152         
extortions vs. iqv_simp   63.66355   0.12881      --   0.08895552         

```r
print_kables(chisq_list(batch_iqvs, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) iqv_long vs. (cols) extortions

            0    1    2    3    4   5+
------  -----  ---  ---  ---  ---  ---
0        1123   64   19   15   14   38
45          0    0    1    0    0    0
53.33       1    0    0    0    0    0
60          4    0    0    0    0    0


Table: Observed counts of (rows) iqv_simp vs. (cols) extortions

            0    1    2    3    4   5+
------  -----  ---  ---  ---  ---  ---
0        1123   64   19   15   14   38
56.25       0    0    1    0    0    0
66.67       1    0    0    0    0    0
75          4    0    0    0    0    0
```

```r
print_kables(chisq_list(batch_iqvs, option = "ratio", print_option = "pandoc"))
```

```


Table: Ratio of observed to expected counts of (rows) iqv_long vs. (cols) extortions

             0       1        2       3       4      5+
------  ------  ------  -------  ------  ------  ------
0        1.000   1.005    0.954   1.005   1.005   1.005
45       0.000   0.000   63.950   0.000   0.000   0.000
53.33    1.134   0.000    0.000   0.000   0.000   0.000
60       1.134   0.000    0.000   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) iqv_simp vs. (cols) extortions

             0       1        2       3       4      5+
------  ------  ------  -------  ------  ------  ------
0        1.000   1.005    0.954   1.005   1.005   1.005
56.25    0.000   0.000   63.950   0.000   0.000   0.000
66.67    1.134   0.000    0.000   0.000   0.000   0.000
75       1.134   0.000    0.000   0.000   0.000   0.000
```

```r
print_kables(chisq_list(batch_iqvs, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) iqv_long vs. (cols) extortions

              0      1        2      3     4     5+
------  -------  -----  -------  -----  ----  -----
0         88.22   5.03     1.49   1.18   1.1   2.99
45         0.00   0.00   100.00   0.00   0.0   0.00
53.33    100.00   0.00     0.00   0.00   0.0   0.00
60       100.00   0.00     0.00   0.00   0.0   0.00


Table: Row percentages of (rows) iqv_simp vs. (cols) extortions

              0      1        2      3     4     5+
------  -------  -----  -------  -----  ----  -----
0         88.22   5.03     1.49   1.18   1.1   2.99
56.25      0.00   0.00   100.00   0.00   0.0   0.00
66.67    100.00   0.00     0.00   0.00   0.0   0.00
75       100.00   0.00     0.00   0.00   0.0   0.00
```

# Bivariate EDA

## Extortion prevalence


```r
ivs <- c("bribes",
         "bribe_victim",
         "size",
         "subsector",
         "yearsquant",
         "years_deciles")

ext_p_batch_chsq <- batch_chisq(df = enve_final, DV = "extortion_victim", IV = ivs)

kable(chisq_tb(ext_p_batch_chsq, stars = TRUE), "pandoc", 
      caption = "Chi-square tests between 'extortion prevalence' and a set of DV")
```



Table: Chi-square tests between 'extortion prevalence' and a set of DV

                                     Chi-sq     Cramer's V   df   p.value     stars 
-----------------------------------  ---------  -----------  ---  ----------  ------
extortion_victim vs. bribes          7.062152   0.03758231   --   0.7006497         
extortion_victim vs. bribe_victim    2.599229   0.02280013   --   0.6226887         
extortion_victim vs. size            7.403193   0.03847907   6    0.2851631         
extortion_victim vs. subsector       28.65253   0.07570011   --   0.7131434         
extortion_victim vs. yearsquant      4.764648   0.03086956   8    0.7824092         
extortion_victim vs. years_deciles   11.29793   0.04753511   --   0.8870565         

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) bribes vs. (cols) extortion_victim

        no   yes   dk
---  -----  ----  ---
0     1906   268   44
1      117    17    1
2       50     2    2
3       29     4    1
4       17     3    1
5+      34     3    1


Table: Observed counts of (rows) bribe_victim vs. (cols) extortion_victim

         no   yes   dk
----  -----  ----  ---
no     1860   265   43
yes     247    29    6
dk       46     3    1


Table: Observed counts of (rows) size vs. (cols) extortion_victim

           no   yes   dk
-------  ----  ----  ---
Large     503    75   11
Medium    553    65   15
Small     524    77   17
Micro     573    80    7


Table: Observed counts of (rows) subsector vs. (cols) extortion_victim

                   no   yes   dk
---------------  ----  ----  ---
Retail             71     8    3
Mining             26     2    2
Utilities          32     5    1
Construction      303    45    8
Manufacturing     418    62    4
Wholesale          95    16    4
Transport         115    14    6
Media              42     3    0
Finance            31     5    0
Real estate        35     3    2
Prof. services     29     8    1
Corporate          42     7    1
Maintenance       181    21    4
Education          33     4    0
Health            338    47    6
Leisure            33     6    1
HotelsRestBar     319    41    7
Other              10     0    0


Table: Observed counts of (rows) yearsquant vs. (cols) extortion_victim

            no   yes   dk
--------  ----  ----  ---
[0,8]      470    70   12
(8,16]     383    63    8
(16,25]    443    56    8
(25,34]    458    57   10
(34,43]    399    51   12


Table: Observed counts of (rows) years_deciles vs. (cols) extortion_victim

            no   yes   dk
--------  ----  ----  ---
[0,4]      251    41    7
(4,8]      219    29    5
(8,12]     190    27    5
(12,16]    193    36    3
(16,21]    256    28    4
(21,25]    187    28    4
(25,30]    253    38    6
(30,34]    205    19    4
(34,39]    216    26    6
(39,43]    183    25    6
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "ratio", print_option = "pandoc"))
```

```


Table: Ratio of observed to expected counts of (rows) bribes vs. (cols) extortion_victim

         no     yes      dk
---  ------  ------  ------
0     0.998   1.017   0.992
1     1.006   1.060   0.370
2     1.075   0.312   1.852
3     0.990   0.990   1.471
4     0.940   1.203   2.381
5+    1.039   0.665   1.316


Table: Ratio of observed to expected counts of (rows) bribe_victim vs. (cols) extortion_victim

          no     yes      dk
----  ------  ------  ------
no     0.996   1.029   0.992
yes    1.017   0.866   1.064
dk     1.068   0.505   1.000


Table: Ratio of observed to expected counts of (rows) size vs. (cols) extortion_victim

             no     yes      dk
-------  ------  ------  ------
Large     0.992   1.072   0.934
Medium    1.014   0.864   1.185
Small     0.985   1.049   1.375
Micro     1.008   1.020   0.530


Table: Ratio of observed to expected counts of (rows) subsector vs. (cols) extortion_victim

                     no     yes      dk
---------------  ------  ------  ------
Retail            1.005   0.821   1.829
Mining            1.006   0.561   3.333
Utilities         0.978   1.108   1.316
Construction      0.988   1.064   1.124
Manufacturing     1.003   1.078   0.413
Wholesale         0.959   1.171   1.739
Transport         0.989   0.873   2.222
Media             1.084   0.561   0.000
Finance           1.000   1.169   0.000
Real estate       1.016   0.631   2.500
Prof. services    0.886   1.772   1.316
Corporate         0.975   1.178   1.000
Maintenance       1.020   0.858   0.971
Education         1.036   0.910   0.000
Health            1.004   1.012   0.767
Leisure           0.958   1.263   1.250
HotelsRestBar     1.009   0.940   0.954
Other             1.161   0.000   0.000


Table: Ratio of observed to expected counts of (rows) yearsquant vs. (cols) extortion_victim

              no     yes      dk
--------  ------  ------  ------
[0,8]      0.989   1.067   1.087
(8,16]     0.980   1.168   0.881
(16,25]    1.015   0.930   0.789
(25,34]    1.013   0.914   0.952
(34,43]    1.003   0.929   1.299


Table: Ratio of observed to expected counts of (rows) years_deciles vs. (cols) extortion_victim

              no     yes      dk
--------  ------  ------  ------
[0,4]      0.975   1.154   1.171
(4,8]      1.005   0.965   0.988
(8,12]     0.994   1.024   1.126
(12,16]    0.966   1.306   0.647
(16,21]    1.032   0.818   0.694
(21,25]    0.992   1.076   0.913
(25,30]    0.989   1.077   1.010
(30,34]    1.044   0.701   0.877
(34,39]    1.011   0.882   1.210
(39,43]    0.993   0.983   1.402
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) bribes vs. (cols) extortion_victim

         no     yes     dk
---  ------  ------  -----
0     85.93   12.08   1.98
1     86.67   12.59   0.74
2     92.59    3.70   3.70
3     85.29   11.76   2.94
4     80.95   14.29   4.76
5+    89.47    7.89   2.63


Table: Row percentages of (rows) bribe_victim vs. (cols) extortion_victim

          no     yes     dk
----  ------  ------  -----
no     85.79   12.22   1.98
yes    87.59   10.28   2.13
dk     92.00    6.00   2.00


Table: Row percentages of (rows) size vs. (cols) extortion_victim

             no     yes     dk
-------  ------  ------  -----
Large     85.40   12.73   1.87
Medium    87.36   10.27   2.37
Small     84.79   12.46   2.75
Micro     86.82   12.12   1.06


Table: Row percentages of (rows) subsector vs. (cols) extortion_victim

                      no     yes     dk
---------------  -------  ------  -----
Retail             86.59    9.76   3.66
Mining             86.67    6.67   6.67
Utilities          84.21   13.16   2.63
Construction       85.11   12.64   2.25
Manufacturing      86.36   12.81   0.83
Wholesale          82.61   13.91   3.48
Transport          85.19   10.37   4.44
Media              93.33    6.67   0.00
Finance            86.11   13.89   0.00
Real estate        87.50    7.50   5.00
Prof. services     76.32   21.05   2.63
Corporate          84.00   14.00   2.00
Maintenance        87.86   10.19   1.94
Education          89.19   10.81   0.00
Health             86.45   12.02   1.53
Leisure            82.50   15.00   2.50
HotelsRestBar      86.92   11.17   1.91
Other             100.00    0.00   0.00


Table: Row percentages of (rows) yearsquant vs. (cols) extortion_victim

              no     yes     dk
--------  ------  ------  -----
[0,8]      85.14   12.68   2.17
(8,16]     84.36   13.88   1.76
(16,25]    87.38   11.05   1.58
(25,34]    87.24   10.86   1.90
(34,43]    86.36   11.04   2.60


Table: Row percentages of (rows) years_deciles vs. (cols) extortion_victim

              no     yes     dk
--------  ------  ------  -----
[0,4]      83.95   13.71   2.34
(4,8]      86.56   11.46   1.98
(8,12]     85.59   12.16   2.25
(12,16]    83.19   15.52   1.29
(16,21]    88.89    9.72   1.39
(21,25]    85.39   12.79   1.83
(25,30]    85.19   12.79   2.02
(30,34]    89.91    8.33   1.75
(34,39]    87.10   10.48   2.42
(39,43]    85.51   11.68   2.80
```

## Extortion distribution


```r
ivs <- c("bribes",
         "bribe_victim",
         "size",
         "subsector",
         "yearsquant",
         "years_deciles")

ext_p_batch_chsq <- batch_chisq(df = enve_final, DV = "extortions", IV = ivs)

kable(chisq_tb(ext_p_batch_chsq, stars = TRUE), "pandoc", 
      caption = "Chi-square tests between 'extortion counts' and a set of DV")
```



Table: Chi-square tests between 'extortion counts' and a set of DV

                               Chi-sq     Cramer's V   df   p.value     stars 
-----------------------------  ---------  -----------  ---  ----------  ------
extortions vs. bribes          14.73117   0.0343292    --   0.9255372         
extortions vs. bribe_victim    8.395162   0.040976     --   0.5757121         
extortions vs. size            16.81315   0.04734715   15   0.3301567         
extortions vs. subsector       67.79517   0.07364518   --   0.90005           
extortions vs. yearsquant      22.41818   0.04734784   --   0.3228386         
extortions vs. years_deciles   44.91095   0.0599406    --   0.4647676         

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) bribes vs. (cols) extortions

         0    1    2    3    4   5+
---  -----  ---  ---  ---  ---  ---
0     1950   99   44   29   21   75
1      118    6    1    2    3    5
2       52    1    0    0    0    1
3       30    1    1    0    0    2
4       18    1    0    0    1    1
5+      35    1    0    1    0    1


Table: Observed counts of (rows) bribe_victim vs. (cols) extortions

          0    1    2    3    4   5+
----  -----  ---  ---  ---  ---  ---
no     1903   98   42   29   21   75
yes     253   10    2    3    4   10
dk       47    1    2    0    0    0


Table: Observed counts of (rows) size vs. (cols) extortions

            0    1    2    3    4   5+
-------  ----  ---  ---  ---  ---  ---
Large     514   25   11    7    4   28
Medium    568   17    9   11    6   22
Small     541   33   13    7    5   19
Micro     580   34   13    7   10   16


Table: Observed counts of (rows) subsector vs. (cols) extortions

                    0    1    2    3    4   5+
---------------  ----  ---  ---  ---  ---  ---
Retail             74    2    0    2    0    4
Mining             28    1    0    0    0    1
Utilities          33    1    2    0    1    1
Construction      311   15    7    5    2   16
Manufacturing     422   21   13    7    8   13
Wholesale          99    9    3    2    0    2
Transport         121    8    1    0    2    3
Media              42    0    0    0    0    3
Finance            31    0    2    0    1    2
Real estate        37    1    0    0    0    2
Prof. services     30    1    2    1    0    4
Corporate          43    4    1    0    0    2
Maintenance       185   10    2    3    1    5
Education          33    1    0    1    0    2
Health            344   18    8    5    7    9
Leisure            34    2    0    1    0    3
HotelsRestBar     326   15    5    5    3   13
Other              10    0    0    0    0    0


Table: Observed counts of (rows) yearsquant vs. (cols) extortions

             0    1    2    3    4   5+
--------  ----  ---  ---  ---  ---  ---
[0,8]      482   25    9   13    7   16
(8,16]     391   25    8    8    5   17
(16,25]    451   22   14    4    2   14
(25,34]    468   20    9    1    5   22
(34,43]    411   17    6    6    6   16


Table: Observed counts of (rows) years_deciles vs. (cols) extortions

             0    1    2    3    4   5+
--------  ----  ---  ---  ---  ---  ---
[0,4]      258   11    7    6    6   11
(4,8]      224   14    2    7    1    5
(8,12]     195   10    4    3    3    7
(12,16]    196   15    4    5    2   10
(16,21]    260   12    6    2    1    7
(21,25]    191   10    8    2    1    7
(25,30]    259   11    6    0    3   18
(30,34]    209    9    3    1    2    4
(34,39]    222    9    4    3    2    8
(39,43]    189    8    2    3    4    8
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "ratio", print_option = "pandoc"))
```

```


Table: Ratio of observed to expected counts of (rows) bribes vs. (cols) extortions

          0       1       2       3       4      5+
---  ------  ------  ------  ------  ------  ------
0     0.998   1.024   1.078   1.021   0.947   0.995
1     0.992   1.019   0.403   1.157   2.222   1.089
2     1.093   0.425   0.000   0.000   0.000   0.545
3     1.001   0.675   1.598   0.000   0.000   1.730
4     0.973   1.092   0.000   0.000   4.762   1.401
5+    1.045   0.604   0.000   2.056   0.000   0.774


Table: Ratio of observed to expected counts of (rows) bribe_victim vs. (cols) extortions

           0       1       2       3       4      5+
----  ------  ------  ------  ------  ------  ------
no     0.996   1.037   1.053   1.045   0.969   1.017
yes    1.018   0.813   0.385   0.831   1.418   1.043
dk     1.067   0.459   2.174   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) size vs. (cols) extortions

              0       1       2       3       4      5+
-------  ------  ------  ------  ------  ------  ------
Large     0.990   0.974   1.015   0.928   0.679   1.398
Medium    1.018   0.616   0.773   1.358   0.948   1.022
Small     0.993   1.225   1.143   0.885   0.809   0.904
Micro     0.997   1.182   1.070   0.829   1.515   0.713


Table: Ratio of observed to expected counts of (rows) subsector vs. (cols) extortions

                      0       1       2       3       4      5+
---------------  ------  ------  ------  ------  ------  ------
Retail            1.024   0.559   0.000   1.905   0.000   1.435
Mining            1.059   0.765   0.000   0.000   0.000   0.980
Utilities         0.985   0.604   2.860   0.000   2.632   0.774
Construction      0.991   0.966   1.069   1.097   0.562   1.322
Manufacturing     0.989   0.995   1.460   1.130   1.653   0.790
Wholesale         0.977   1.795   1.418   1.359   0.000   0.512
Transport         1.017   1.359   0.403   0.000   1.481   0.654
Media             1.059   0.000   0.000   0.000   0.000   1.961
Finance           0.977   0.000   3.019   0.000   2.778   1.634
Real estate       1.050   0.573   0.000   0.000   0.000   1.471
Prof. services    0.896   0.604   2.860   2.056   0.000   3.096
Corporate         0.976   1.835   1.087   0.000   0.000   1.176
Maintenance       1.019   1.113   0.528   1.138   0.485   0.714
Education         1.012   0.620   0.000   2.111   0.000   1.590
Health            0.998   1.056   1.112   0.999   1.790   0.677
Leisure           0.965   1.147   0.000   1.953   0.000   2.206
HotelsRestBar     1.008   0.937   0.740   1.064   0.817   1.042
Other             1.135   0.000   0.000   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) yearsquant vs. (cols) extortions

               0       1       2       3       4      5+
--------  ------  ------  ------  ------  ------  ------
[0,8]      0.991   1.039   0.886   1.840   1.268   0.853
(8,16]     0.977   1.263   0.958   1.377   1.101   1.101
(16,25]    1.009   0.995   1.501   0.616   0.394   0.812
(25,34]    1.012   0.874   0.932   0.149   0.952   1.232
(34,43]    1.010   0.844   0.706   1.015   1.299   1.019


Table: Ratio of observed to expected counts of (rows) years_deciles vs. (cols) extortions

               0       1       2       3       4      5+
--------  ------  ------  ------  ------  ------  ------
[0,4]      0.979   0.844   1.272   1.568   2.007   1.082
(4,8]      1.005   1.269   0.430   2.162   0.395   0.581
(8,12]     0.997   1.033   0.979   1.056   1.351   0.927
(12,16]    0.959   1.483   0.937   1.684   0.862   1.268
(16,21]    1.024   0.956   1.132   0.543   0.347   0.715
(21,25]    0.990   1.047   1.985   0.713   0.457   0.940
(25,30]    0.990   0.849   1.098   0.000   1.010   1.783
(30,34]    1.040   0.905   0.715   0.343   0.877   0.516
(34,39]    1.016   0.832   0.877   0.945   0.806   0.949
(39,43]    1.002   0.857   0.508   1.095   1.869   1.100
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) bribes vs. (cols) extortions

          0      1      2      3      4     5+
---  ------  -----  -----  -----  -----  -----
0     87.92   4.46   1.98   1.31   0.95   3.38
1     87.41   4.44   0.74   1.48   2.22   3.70
2     96.30   1.85   0.00   0.00   0.00   1.85
3     88.24   2.94   2.94   0.00   0.00   5.88
4     85.71   4.76   0.00   0.00   4.76   4.76
5+    92.11   2.63   0.00   2.63   0.00   2.63


Table: Row percentages of (rows) bribe_victim vs. (cols) extortions

           0      1      2      3      4     5+
----  ------  -----  -----  -----  -----  -----
no     87.78   4.52   1.94   1.34   0.97   3.46
yes    89.72   3.55   0.71   1.06   1.42   3.55
dk     94.00   2.00   4.00   0.00   0.00   0.00


Table: Row percentages of (rows) size vs. (cols) extortions

              0      1      2      3      4     5+
-------  ------  -----  -----  -----  -----  -----
Large     87.27   4.24   1.87   1.19   0.68   4.75
Medium    89.73   2.69   1.42   1.74   0.95   3.48
Small     87.54   5.34   2.10   1.13   0.81   3.07
Micro     87.88   5.15   1.97   1.06   1.52   2.42


Table: Row percentages of (rows) subsector vs. (cols) extortions

                       0      1      2      3      4      5+
---------------  -------  -----  -----  -----  -----  ------
Retail             90.24   2.44   0.00   2.44   0.00    4.88
Mining             93.33   3.33   0.00   0.00   0.00    3.33
Utilities          86.84   2.63   5.26   0.00   2.63    2.63
Construction       87.36   4.21   1.97   1.40   0.56    4.49
Manufacturing      87.19   4.34   2.69   1.45   1.65    2.69
Wholesale          86.09   7.83   2.61   1.74   0.00    1.74
Transport          89.63   5.93   0.74   0.00   1.48    2.22
Media              93.33   0.00   0.00   0.00   0.00    6.67
Finance            86.11   0.00   5.56   0.00   2.78    5.56
Real estate        92.50   2.50   0.00   0.00   0.00    5.00
Prof. services     78.95   2.63   5.26   2.63   0.00   10.53
Corporate          86.00   8.00   2.00   0.00   0.00    4.00
Maintenance        89.81   4.85   0.97   1.46   0.49    2.43
Education          89.19   2.70   0.00   2.70   0.00    5.41
Health             87.98   4.60   2.05   1.28   1.79    2.30
Leisure            85.00   5.00   0.00   2.50   0.00    7.50
HotelsRestBar      88.83   4.09   1.36   1.36   0.82    3.54
Other             100.00   0.00   0.00   0.00   0.00    0.00


Table: Row percentages of (rows) yearsquant vs. (cols) extortions

               0      1      2      3      4     5+
--------  ------  -----  -----  -----  -----  -----
[0,8]      87.32   4.53   1.63   2.36   1.27   2.90
(8,16]     86.12   5.51   1.76   1.76   1.10   3.74
(16,25]    88.95   4.34   2.76   0.79   0.39   2.76
(25,34]    89.14   3.81   1.71   0.19   0.95   4.19
(34,43]    88.96   3.68   1.30   1.30   1.30   3.46


Table: Row percentages of (rows) years_deciles vs. (cols) extortions

               0      1      2      3      4     5+
--------  ------  -----  -----  -----  -----  -----
[0,4]      86.29   3.68   2.34   2.01   2.01   3.68
(4,8]      88.54   5.53   0.79   2.77   0.40   1.98
(8,12]     87.84   4.50   1.80   1.35   1.35   3.15
(12,16]    84.48   6.47   1.72   2.16   0.86   4.31
(16,21]    90.28   4.17   2.08   0.69   0.35   2.43
(21,25]    87.21   4.57   3.65   0.91   0.46   3.20
(25,30]    87.21   3.70   2.02   0.00   1.01   6.06
(30,34]    91.67   3.95   1.32   0.44   0.88   1.75
(34,39]    89.52   3.63   1.61   1.21   0.81   3.23
(39,43]    88.32   3.74   0.93   1.40   1.87   3.74
```

## Extortion vs Years

A brief EDA analysis of the years variable, using a facet plot of the bivariate relationship with extortion incidents.


```r
enve_test %>%
    select(extortions, years, yearsquant, years_deciles) -> gg_ext_years



years_scatterplots <- ggplot(gg_ext_years, aes(x = years, y = extortions))

years_scatterplots + geom_point() + theme_bw() + geom_smooth(method = "lm")
```

![plot of chunk years-plots](figure/years-plots-1.png)


```r
# Quintiles

years_scatterplots + 
    geom_point() + 
    theme_bw() + 
    facet_wrap(~ yearsquant, scales = "free_x") + 
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-1.png)

```r
# For deciles

years_scatterplots + 
    geom_point() + 
    theme_bw() + 
    facet_wrap(~ years_deciles, scales = "free_x") + 
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-2.png)

# State level

Provide a brief summary of state-level figures. Include:

- n business (n())
- n victims (extortion and bribery) (filter by, then n())
- n repeat victims (filter by)
- n incidents (sum(extortions))
- n repeat incidents (n incidents - n victims)
- Gini index per state (for all and for business) (lets see...)
- plot of lorenz curve per state (let see...)



```r
enve_final %>%
    group_by(CVE_ENT, NOM_ENT) %>%
    summarise(n = n(),
              incidents = sum(extortions)) -> state_incidents_df

enve_final %>%
    filter(extortion_victim == "yes") %>%
    group_by(NOM_ENT) %>%
    summarise(victims = n()) -> state_evics_df

enve_final %>%
    filter(bribe_victim == "yes") %>%
    group_by(NOM_ENT) %>%
    summarise(bribe_vics = n()) -> state_bvics_df

enve_final %>%
    filter(rep_extortion_victim == 1) %>%
    group_by(NOM_ENT) %>%
    summarise(rep_ext_vics = n()) -> state_rep_vics_df

state_incidents_df %>%
    full_join(state_evics_df) %>%
    full_join(state_rep_vics_df) %>%
    mutate(rep_inci = incidents - rep_ext_vics) %>%
    full_join(state_bvics_df) -> state_summary
```

```
Joining, by = "NOM_ENT"
Joining, by = "NOM_ENT"
Joining, by = "NOM_ENT"
```

```r
state_gini <- by(enve_final$extortions, enve_final$NOM_ENT, ineq::Gini)

state_gini_df <- data.frame(Gini = state_gini[1:32])

state_gini_df$NOM_ENT <- as.factor(rownames(state_gini_df))

state_gini_tests <- by(enve_final$extortions, enve_final$NOM_ENT, mc_gini_test)

state_mc_gini_l <- lapply(state_gini_tests[1:32], function(x) x$mc_mean)

state_mc_gini_df <- data.frame(`Poisson Gini`= unlist(state_mc_gini_l))

rownames(state_mc_gini_df) <- names(state_mc_gini_l)

state_mc_gini_df$NOM_ENT <- as.factor(names(state_mc_gini_l))

state_mc_gini_hyp <- sapply(state_gini_tests[1:32], 
                            function(x) if(x$mc_test == TRUE) "***" 
                                        else "")

unlist(state_mc_gini_hyp) -> state_mc_gini_df$test

state_summary %>%
    left_join(state_gini_df) %>%
    left_join(state_mc_gini_df) -> state_summary
```

```
Joining, by = "NOM_ENT"
Joining, by = "NOM_ENT"
```

```r
state_summary$Gini <- as.numeric(unname(state_summary$Gini))

kable(state_summary, format = "pandoc", digits = 3)
```



 CVE_ENT  NOM_ENT                              n   incidents   victims   rep_ext_vics   rep_inci   bribe_vics    Gini   Poisson.Gini  test 
--------  --------------------------------  ----  ----------  --------  -------------  ---------  -----------  ------  -------------  -----
       1  AGUASCALIENTES                      81          46        11              7         39           12   0.933          0.644  ***  
       2  BAJA CALIFORNIA                     70          42        12              9         33            4   0.895          0.632  ***  
       3  BAJA CALIFORNIA SUR                 72          42         6              3         39            5   0.972          0.637  ***  
       4  CAMPECHE                            57          16         4              3         13            9   0.956          0.782  ***  
       5  COAHUILA DE ZARAGOZA                81          62         9              6         56            7   0.953          0.581  ***  
       6  COLIMA                              70          44         6              4         40            9   0.966          0.620  ***  
       7  CHIAPAS                             71          83        12              8         75            6   0.914          0.486  ***  
       8  CHIHUAHUA                           78          51         6              3         48            8   0.967          0.613  ***  
       9  DISTRITO FEDERAL                    85          33        10              7         26           10   0.922          0.722  ***  
      10  DURANGO                             71          37        13              6         31           11   0.907          0.663  ***  
      11  GUANAJUATO                          95          48        10              7         41           10   0.945          0.669  ***  
      12  GUERRERO                            80          32         6              3         29            4   0.963          0.714  ***  
      13  HIDALGO                            100          48        13              9         39           12   0.939          0.679  ***  
      14  JALISCO                             95          24         9              4         20           12   0.950          0.798  ***  
      15  MEXICO                              79          63        12              9         54            9   0.925          0.569  ***  
      16  MICHOACAN DE OCAMPO                 72          50         7              7         43            5   0.949          0.599  ***  
      17  MORELOS                             71          31         8              5         26            7   0.942          0.697  ***  
      18  NAYARIT                             80          39         8              4         35           10   0.957          0.674  ***  
      19  NUEVO LEON                          87         112        16             12        100           11   0.924          0.466  ***  
      20  OAXACA                              83           7         5              2          5           16   0.950             --       
      21  PUEBLA                              84          68        12             10         58            8   0.922          0.568  ***  
      22  QUERETARO                           80          29         9              7         22            5   0.924          0.735  ***  
      23  QUINTANA ROO                        72          16         9              4         12            3   0.911          0.819  ***  
      24  SAN LUIS POTOSI                     79          33         7              6         27           10   0.947          0.706  ***  
      25  SINALOA                             75          77        12              8         69           11   0.915          0.514  ***  
      26  SONORA                              81          52         9              4         48           16   0.962          0.616  ***  
      27  TABASCO                             67          25         4              2         23            9   0.976          0.730  ***  
      28  TAMAULIPAS                          76          36        10              5         31            8   0.938          0.682  ***  
      29  TLAXCALA                            78          25        10              6         19            9   0.909          0.758  ***  
      30  VERACRUZ DE IGNACIO DE LA LLAVE     76          44         6              4         40           13   0.954          0.639  ***  
      31  YUCATAN                             79          25        13              6         19            7   0.889          0.759  ***  
      32  ZACATECAS                           75          83        13              8         75            6   0.932          0.499  ***  

```r
state_table <- by(enve_final$extortions, enve_final$NOM_ENT, victim_table)

lapply(seq_along(state_table), function(x) 
    {
    cap <- names(state_table)[x]
    final <- kable(state_table[[x]], format = "pandoc", digits = 3, 
                   caption = cap)
    return(final)
    }) -> state_dist_list

print_kables(state_dist_list)
```

```


Table: AGUASCALIENTES

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           70          --        --     86.420         --           --
      1            4           4        --      4.938     36.364        8.696
      2            2           4         2      2.469     18.182        8.696
      3            2           6         4      2.469     18.182       13.043
      8            1           8         7      1.235      9.091       17.391
      9            1           9         8      1.235      9.091       19.565
     15            1          15        14      1.235      9.091       32.609


Table: BAJA CALIFORNIA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           58          --        --     82.857         --           --
      1            3           3        --      4.286     25.000        7.143
      2            3           6         3      4.286     25.000       14.286
      3            2           6         4      2.857     16.667       14.286
      5            1           5         4      1.429      8.333       11.905
      6            1           6         5      1.429      8.333       14.286
      7            1           7         6      1.429      8.333       16.667
      9            1           9         8      1.429      8.333       21.429


Table: BAJA CALIFORNIA SUR

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           66          --        --     91.667         --           --
      1            3           3        --      4.167     50.000        7.143
      3            1           3         2      1.389     16.667        7.143
      4            1           4         3      1.389     16.667        9.524
     32            1          32        31      1.389     16.667       76.190


Table: CAMPECHE

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           53          --        --     92.982         --           --
      1            1           1        --      1.754         25         6.25
      2            1           2         1      1.754         25        12.50
      5            1           5         4      1.754         25        31.25
      8            1           8         7      1.754         25        50.00


Table: CHIAPAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           59          --        --     83.099         --           --
      1            4           4        --      5.634     33.333        4.819
      2            2           4         2      2.817     16.667        4.819
      6            1           6         5      1.408      8.333        7.229
     11            2          22        20      2.817     16.667       26.506
     13            1          13        12      1.408      8.333       15.663
     15            1          15        14      1.408      8.333       18.072
     19            1          19        18      1.408      8.333       22.892


Table: CHIHUAHUA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     92.308         --           --
      1            3           3        --      3.846     50.000        5.882
      4            1           4         3      1.282     16.667        7.843
     21            1          21        20      1.282     16.667       41.176
     23            1          23        22      1.282     16.667       45.098


Table: COAHUILA DE ZARAGOZA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     88.889         --           --
      1            3           3        --      3.704     33.333        4.839
      2            2           4         2      2.469     22.222        6.452
      3            1           3         2      1.235     11.111        4.839
     10            1          10         9      1.235     11.111       16.129
     20            1          20        19      1.235     11.111       32.258
     22            1          22        21      1.235     11.111       35.484


Table: COLIMA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           64          --        --     91.429         --           --
      1            2           2        --      2.857     33.333        4.545
      2            1           2         1      1.429     16.667        4.545
      3            1           3         2      1.429     16.667        6.818
     10            1          10         9      1.429     16.667       22.727
     27            1          27        26      1.429     16.667       61.364


Table: DISTRITO FEDERAL

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           75          --        --     88.235         --           --
      1            3           3        --      3.529         30        9.091
      2            1           2         1      1.176         10        6.061
      3            2           6         4      2.353         20       18.182
      4            1           4         3      1.176         10       12.121
      6            3          18        15      3.529         30       54.545


Table: DURANGO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           58          --        --     81.690         --           --
      1            7           7        --      9.859     53.846       18.919
      2            2           4         2      2.817     15.385       10.811
      3            1           3         2      1.408      7.692        8.108
      4            1           4         3      1.408      7.692       10.811
      5            1           5         4      1.408      7.692       13.514
     14            1          14        13      1.408      7.692       37.838


Table: GUANAJUATO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           85          --        --     89.474         --           --
      1            3           3        --      3.158         30        6.250
      2            2           4         2      2.105         20        8.333
      4            1           4         3      1.053         10        8.333
      5            1           5         4      1.053         10       10.417
      6            1           6         5      1.053         10       12.500
     12            1          12        11      1.053         10       25.000
     14            1          14        13      1.053         10       29.167


Table: GUERRERO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           74          --        --      92.50         --           --
      1            3           3        --       3.75     50.000        9.375
      5            1           5         4       1.25     16.667       15.625
      9            1           9         8       1.25     16.667       28.125
     15            1          15        14       1.25     16.667       46.875


Table: HIDALGO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           87          --        --         87         --           --
      1            4           4        --          4     30.769        8.333
      2            5          10         5          5     38.462       20.833
      3            2           6         4          2     15.385       12.500
      5            1           5         4          1      7.692       10.417
     23            1          23        22          1      7.692       47.917


Table: JALISCO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           86          --        --     90.526         --           --
      1            5           5        --      5.263     55.556       20.833
      2            2           4         2      2.105     22.222       16.667
      5            1           5         4      1.053     11.111       20.833
     10            1          10         9      1.053     11.111       41.667


Table: MEXICO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           67          --        --     84.810         --           --
      1            3           3        --      3.797     25.000        4.762
      2            2           4         2      2.532     16.667        6.349
      3            1           3         2      1.266      8.333        4.762
      4            2           8         6      2.532     16.667       12.698
      5            1           5         4      1.266      8.333        7.937
      7            1           7         6      1.266      8.333       11.111
     13            1          13        12      1.266      8.333       20.635
     20            1          20        19      1.266      8.333       31.746


Table: MICHOACAN DE OCAMPO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           65          --        --     90.278         --           --
      2            2           4        --      2.778     28.571            8
      4            3          12         9      4.167     42.857           24
      9            1           9         8      1.389     14.286           18
     25            1          25        24      1.389     14.286           50


Table: MORELOS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           63          --        --     88.732         --           --
      1            3           3        --      4.225       37.5        9.677
      2            1           2         1      1.408       12.5        6.452
      3            1           3         2      1.408       12.5        9.677
      4            1           4         3      1.408       12.5       12.903
      5            1           5         4      1.408       12.5       16.129
     14            1          14        13      1.408       12.5       45.161


Table: NAYARIT

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --      90.00         --           --
      1            4           4        --       5.00       50.0       10.256
      3            1           3         2       1.25       12.5        7.692
      4            1           4         3       1.25       12.5       10.256
      8            1           8         7       1.25       12.5       20.513
     20            1          20        19       1.25       12.5       51.282


Table: NUEVO LEON

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           71          --        --     81.609         --           --
      1            4           4        --      4.598      25.00        3.571
      2            1           2         1      1.149       6.25        1.786
      3            4          12         8      4.598      25.00       10.714
      4            2           8         6      2.299      12.50        7.143
      5            1           5         4      1.149       6.25        4.464
      7            1           7         6      1.149       6.25        6.250
     10            1          10         9      1.149       6.25        8.929
     32            2          64        62      2.299      12.50       57.143


Table: OAXACA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           78          --        --     93.976         --           --
      1            3           3        --      3.614         60       42.857
      2            2           4         2      2.410         40       57.143


Table: PUEBLA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     85.714         --           --
      1            2           2        --      2.381     16.667        2.941
      2            2           4         2      2.381     16.667        5.882
      4            3          12         9      3.571     25.000       17.647
      5            2          10         8      2.381     16.667       14.706
      7            1           7         6      1.190      8.333       10.294
     12            1          12        11      1.190      8.333       17.647
     21            1          21        20      1.190      8.333       30.882


Table: QUERETARO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           71          --        --      88.75         --           --
      1            2           2        --       2.50     22.222        6.897
      2            2           4         2       2.50     22.222       13.793
      3            1           3         2       1.25     11.111       10.345
      4            2           8         6       2.50     22.222       27.586
      5            1           5         4       1.25     11.111       17.241
      7            1           7         6       1.25     11.111       24.138


Table: QUINTANA ROO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           63          --        --     87.500         --           --
      1            5           5        --      6.944     55.556        31.25
      2            2           4         2      2.778     22.222        25.00
      3            1           3         2      1.389     11.111        18.75
      4            1           4         3      1.389     11.111        25.00


Table: SAN LUIS POTOSI

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     91.139         --           --
      1            1           1        --      1.266     14.286        3.030
      2            2           4         2      2.532     28.571       12.121
      4            1           4         3      1.266     14.286       12.121
      5            1           5         4      1.266     14.286       15.152
      6            1           6         5      1.266     14.286       18.182
     13            1          13        12      1.266     14.286       39.394


Table: SINALOA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           63          --        --     84.000         --           --
      1            4           4        --      5.333     33.333        5.195
      3            2           6         4      2.667     16.667        7.792
      4            1           4         3      1.333      8.333        5.195
      8            1           8         7      1.333      8.333       10.390
     12            1          12        11      1.333      8.333       15.584
     14            2          28        26      2.667     16.667       36.364
     15            1          15        14      1.333      8.333       19.481


Table: SONORA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     88.889         --           --
      1            5           5        --      6.173     55.556        9.615
      3            1           3         2      1.235     11.111        5.769
      5            1           5         4      1.235     11.111        9.615
      6            1           6         5      1.235     11.111       11.538
     33            1          33        32      1.235     11.111       63.462


Table: TABASCO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           63          --        --     94.030         --           --
      1            2           2        --      2.985         50            8
      3            1           3         2      1.493         25           12
     20            1          20        19      1.493         25           80


Table: TAMAULIPAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           66          --        --     86.842         --           --
      1            5           5        --      6.579         50       13.889
      2            2           4         2      2.632         20       11.111
      3            1           3         2      1.316         10        8.333
     12            2          24        22      2.632         20       66.667


Table: TLAXCALA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           68          --        --     87.179         --           --
      1            4           4        --      5.128         40           16
      3            4          12         8      5.128         40           48
      4            1           4         3      1.282         10           16
      5            1           5         4      1.282         10           20


Table: VERACRUZ DE IGNACIO DE LA LLAVE

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           70          --        --     92.105         --           --
      1            2           2        --      2.632     33.333        4.545
      5            1           5         4      1.316     16.667       11.364
      9            1           9         8      1.316     16.667       20.455
     13            1          13        12      1.316     16.667       29.545
     15            1          15        14      1.316     16.667       34.091


Table: YUCATAN

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           66          --        --     83.544         --           --
      1            7           7        --      8.861     53.846           28
      2            3           6         3      3.797     23.077           24
      3            1           3         2      1.266      7.692           12
      4            1           4         3      1.266      7.692           16
      5            1           5         4      1.266      7.692           20


Table: ZACATECAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           62          --        --     82.667         --           --
      1            5           5        --      6.667     38.462        6.024
      2            2           4         2      2.667     15.385        4.819
      3            1           3         2      1.333      7.692        3.614
      4            1           4         3      1.333      7.692        4.819
      5            1           5         4      1.333      7.692        6.024
     14            1          14        13      1.333      7.692       16.867
     19            1          19        18      1.333      7.692       22.892
     29            1          29        28      1.333      7.692       34.940
```

```r
victim_lorenz("extortions", data = enve_final, family = "poisson",
              reps = 500, by_var = "NOM_ABR")
```

![plot of chunk state-level-summary](figure/state-level-summary-1.png)

Compare the bribe variable from the `area_level` data set with the one from this state summary above


```r
bribe_mean <- mean(state_summary$bribe_vics)

state_summary %>%
    left_join(area_level) %>%
    dplyr::select(bribe_vics, log_bribe_vic) %>%
    mutate(log_bribe_vic2 = log(bribe_vics) - log(bribe_mean),
           dif = log_bribe_vic2 - log_bribe_vic) -> bribe_test
```

```
Joining, by = c("CVE_ENT", "NOM_ENT")
```

```
Adding missing grouping variables: `CVE_ENT`
```

```r
bribe_test$test <- round(bribe_test$dif, 2) == 0

kable(bribe_test, format = "pandoc")
```



 CVE_ENT   bribe_vics   log_bribe_vic   log_bribe_vic2          dif  test  
--------  -----------  --------------  ---------------  -----------  ------
       1           12      -0.1954920        0.3087355    0.5042275  FALSE 
       2            4      -0.1656391       -0.7898768   -0.6242378  FALSE 
       3            5      -0.1954920       -0.5667333   -0.3712412  FALSE 
       4            9      -0.0544134        0.0210534    0.0754668  FALSE 
       5            7      -0.5565054       -0.2302610    0.3262443  FALSE 
       6            9      -1.0529423        0.0210534    1.0739957  FALSE 
       7            6       0.3855379       -0.3844117   -0.7699496  FALSE 
       8            8      -0.2580124       -0.0967296    0.1612828  FALSE 
       9           10      -0.2262637        0.1264139    0.3526776  FALSE 
      10           11      -0.7475606        0.2217241    0.9692847  FALSE 
      11           10      -0.3961627        0.1264139    0.5225766  FALSE 
      12            4       0.4976552       -0.7898768   -1.2875320  FALSE 
      13           12      -0.4339030        0.3087355    0.7426385  FALSE 
      14           12      -0.1084806        0.3087355    0.4172161  FALSE 
      15            9       0.2782923        0.0210534   -0.2572389  FALSE 
      16            5       0.0215725       -0.5667333   -0.5883057  FALSE 
      17            7       0.4188743       -0.2302610   -0.6491353  FALSE 
      18           10      -0.1954920        0.1264139    0.3219059  FALSE 
      19           11      -0.2580124        0.2217241    0.4797365  FALSE 
      20           16       0.3855379        0.5964176    0.2108797  FALSE 
      21            8       0.3510517       -0.0967296   -0.4477813  FALSE 
      22            5      -0.6009571       -0.5667333    0.0342239  FALSE 
      23            3       0.3684434       -1.0775589   -1.4460023  FALSE 
      24           10      -0.7475606        0.1264139    0.8739745  FALSE 
      25           11      -0.1656391        0.2217241    0.3873632  FALSE 
      26           16      -0.6962673        0.5964176    1.2926849  FALSE 
      27            9      -0.3597951        0.0210534    0.3808485  FALSE 
      28            8       0.9231209       -0.0967296   -1.0198506  FALSE 
      29            9       0.4668835        0.0210534   -0.4458301  FALSE 
      30           13       0.1792014        0.3887782    0.2095768  FALSE 
      31            7       0.0921901       -0.2302610   -0.3224511  FALSE 
      32            6      -0.0031201       -0.3844117   -0.3812916  FALSE 

To do tomorrow:

- Move on to models
- Test
- Send

- Exclude subsectors with few observations!! 
- Remove package loading from here
- Then let's start adding the models, as including new state-level variables, would probably have to rerun all of them
- Also, will need to run ZIP and hurdle. should optimise the modelling strategy, as we know it can be extremely time consuming. (should probably exclude cols with NAs to avoid errors), will attempt all with glmmadmb.



```r
endtime <- proc.time()

endtime - starttime
```

```
   user  system elapsed 
317.801   6.217 619.944 
```
