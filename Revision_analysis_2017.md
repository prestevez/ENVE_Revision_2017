---
title: "Repeat extortion of Mexican businesses: Revision analysis"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "01/06/2017"
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
[1] "Mon Jun 12 03:06:41 2017"
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
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_3.4.0 magrittr_1.5   tools_3.4.0    stringi_1.1.5 
[5] knitr_1.16     stringr_1.2.0  evaluate_0.10 
```

```r
set.seed(42)
options(scipen=0)
```

## Load packages and functions

Install custom package, requires `devtools`.


```r
# devtools::install_github("prestevez/victim")
```

Next we load the packages that we will use.


```r
library(victim)
library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
library(texreg)
```

```
Version:  1.36.23
Date:     2017-03-03
Author:   Philip Leifeld (University of Glasgow)

Please cite the JSS article in your publications -- see citation("texreg").
```

```r
library(lme4)
```

```
Loading required package: Matrix
```

```r
library(glmmADMB)
```

```
Loading required package: MASS
```

```

Attaching package: 'glmmADMB'
```

```
The following object is masked from 'package:MASS':

    stepAIC
```

```
The following object is masked from 'package:stats':

    step
```

```r
library(classInt)
library(dplyr)
```

```

Attaching package: 'dplyr'
```

```
The following object is masked from 'package:MASS':

    select
```

```
The following objects are masked from 'package:stats':

    filter, lag
```

```
The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
```

```r
library(reshape2)
library(lmtest)
```

```
Loading required package: zoo
```

```

Attaching package: 'zoo'
```

```
The following objects are masked from 'package:base':

    as.Date, as.Date.numeric
```

```r
library(car)
```

```

Attaching package: 'car'
```

```
The following object is masked from 'package:dplyr':

    recode
```

```r
library(pscl)
```

```
Loading required package: lattice
```

```
Classes and Methods for R developed in the
```

```
Political Science Computational Laboratory
```

```
Department of Political Science
```

```
Stanford University
```

```
Simon Jackman
```

```
hurdle and zeroinfl functions by Achim Zeileis
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
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] pscl_1.4.9        lattice_0.20-35   car_2.1-4        
 [4] lmtest_0.9-35     zoo_1.8-0         reshape2_1.4.2   
 [7] dplyr_0.5.0       classInt_0.1-24   glmmADMB_0.8.3.3 
[10] MASS_7.3-47       lme4_1.1-13       Matrix_1.2-9     
[13] texreg_1.36.23    knitr_1.16        Cairo_1.5-9      
[16] ggplot2_2.2.1     foreign_0.8-67    victim_0.0.0.9000

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.11       compiler_3.4.0     nloptr_1.0.4      
 [4] R2admb_0.7.15      plyr_1.8.4         class_7.3-14      
 [7] tools_3.4.0        digest_0.6.12      evaluate_0.10     
[10] tibble_1.3.0       gtable_0.2.0       nlme_3.1-131      
[13] mgcv_1.8-17        DBI_0.6-1          parallel_3.4.0    
[16] SparseM_1.77       coda_0.19-1        e1071_1.6-8       
[19] stringr_1.2.0      MatrixModels_0.4-1 nnet_7.3-12       
[22] grid_3.4.0         R6_2.2.1           minqa_1.2.4       
[25] magrittr_1.5       scales_0.4.1       codetools_0.2-15  
[28] splines_3.4.0      pbkrtest_0.4-7     assertthat_0.2.0  
[31] colorspace_1.3-2   quantreg_5.33      stringi_1.1.5     
[34] lazyeval_0.2.0     munsell_0.4.3     
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
### To exclude the subsectors with very few observations

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

```r
nrow(enve_test)
```

```
[1] 2500
```

```r
enve_test %>%
    filter(subsector != "Utilities") %>%
    filter(subsector != "Corporate") -> enve_test_2

enve_test_2$subsector <- droplevels(enve_test_2$subsector)

levels(enve_test_2$subsector)
```

```
 [1] "Retail"         "Mining"         "Construction"   "Manufacturing" 
 [5] "Wholesale"      "Transport"      "Media"          "Finance"       
 [9] "Real estate"    "Prof. services" "Maintenance"    "Education"     
[13] "Health"         "Leisure"        "HotelsRestBar"  "Other"         
```

```r
nrow(enve_test_2)
```

```
[1] 2412
```

```r
enve_test <- enve_test_2

####

## Summary of final dataset
summary(enve_test)
```

```
    CVE_ENT        extortions      extortion_victim extortions_nas   
 Min.   : 1.00   Min.   : 0.0000   no :2079         Min.   : 0.0000  
 1st Qu.: 9.00   1st Qu.: 0.0000   yes: 285         1st Qu.: 0.0000  
 Median :16.00   Median : 0.0000   dk :  48         Median : 0.0000  
 Mean   :16.47   Mean   : 0.5688                    Mean   : 0.5804  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.: 0.0000  
 Max.   :32.00   Max.   :33.0000                    Max.   :33.0000  
                                                    NA's   :48       
 rep_extortion_victim     bribes        bribe_victim   bribes_nas     
 0:2231               Min.   : 0.0000   no :2090     Min.   : 0.0000  
 1: 181               1st Qu.: 0.0000   yes: 274     1st Qu.: 0.0000  
                      Median : 0.0000   dk :  48     Median : 0.0000  
                      Mean   : 0.2786                Mean   : 0.2843  
                      3rd Qu.: 0.0000                3rd Qu.: 0.0000  
                      Max.   :13.0000                Max.   :13.0000  
                                                     NA's   :48       
   CVE_UNICA          size     sector     tempsub      
 Min.   :    60   Large :568   C:776   Min.   :212410  
 1st Qu.: 10225   Medium:611   I:792   1st Qu.:365310  
 Median : 19849   Small :597   S:844   Median :510610  
 Mean   : 32109   Micro :636           Mean   :514704  
 3rd Qu.: 53297                        3rd Qu.:665310  
 Max.   :100000                        Max.   :812910  
                                                       
         subsector       years        yearsquant  years_deciles
 Manufacturing:484   Min.   : 0.0   [0,8]  :539   [0,4]  :293  
 Health       :391   1st Qu.:10.0   (8,16] :433   (25,30]:289  
 HotelsRestBar:367   Median :21.0   (16,25]:487   (16,21]:279  
 Construction :356   Mean   :20.9   (25,34]:510   (4,8]  :246  
 Maintenance  :206   3rd Qu.:31.0   (34,43]:443   (34,39]:241  
 Transport    :135   Max.   :43.0                 (30,34]:221  
 (Other)      :473                                (Other):843  
   Prevalence       Incidence     Concentration   log_bribe_vic    
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.0529  
 1st Qu.: 45.00   1st Qu.: 63.0   1st Qu.:1.340   1st Qu.:-0.3962  
 Median : 59.00   Median :103.0   Median :1.606   Median :-0.1656  
 Mean   : 69.83   Mean   :115.4   Mean   :1.658   Mean   :-0.1035  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.2783  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.9231  
                                                                   
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.49289   Mean   :  0.2984   Mean   :-0.6757  
 3rd Qu.: 0.16603   3rd Qu.:  8.8654   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.19996  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.02994   Mean   :-0.3723   Mean   :-0.2404   Mean   :-0.03879  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index           comp_index                       NOM_ENT    
 Min.   :-33.02134   Min.   :-22.4096   HIDALGO             :  98  
 1st Qu.: -4.29495   1st Qu.: -5.1176   GUANAJUATO          :  93  
 Median :  1.13880   Median :  1.3520   JALISCO             :  93  
 Mean   : -0.00842   Mean   :  0.1609   DISTRITO FEDERAL    :  82  
 3rd Qu.:  7.76001   3rd Qu.:  5.0020   NUEVO LEON          :  82  
 Max.   : 24.00707   Max.   : 20.1805   COAHUILA DE ZARAGOZA:  80  
                                        (Other)             :1884  
    NOM_ABR    
 HGO.   :  98  
 GTO.   :  93  
 JAL.   :  93  
 DF     :  82  
 NL     :  82  
 COAH.  :  80  
 (Other):1884  
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
 Min.   :    60   Min.   :10     Telephone    :1431   Remote   :1437  
 1st Qu.:  9352   1st Qu.:10     Internet     :   6   In person:  85  
 Median : 18014   Median :10     Street       :  62   Other    :  31  
 Mean   : 29764   Mean   :10     Premises     :  16   NA's     :1175  
 3rd Qu.: 47945   3rd Qu.:10     Cobro de piso:   7                   
 Max.   :100000   Max.   :10     Other        :  31                   
                  NA's   :1175   NA's         :1175                   
 typeTelephone     typeInternet      typeStreet      typePremises   
 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
 1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
 Median :1.0000   Median :0.0000   Median :0.0000   Median :0.0000  
 Mean   :0.9214   Mean   :0.0039   Mean   :0.0399   Mean   :0.0103  
 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
 NA's   :1175     NA's   :1175     NA's   :1175     NA's   :1175    
 typeCobro.de.piso   typeOther      simpRemote     simpIn.person   
 Min.   :0.0000    Min.   :0.00   Min.   :0.0000   Min.   :0.0000  
 1st Qu.:0.0000    1st Qu.:0.00   1st Qu.:1.0000   1st Qu.:0.0000  
 Median :0.0000    Median :0.00   Median :1.0000   Median :0.0000  
 Mean   :0.0045    Mean   :0.02   Mean   :0.9253   Mean   :0.0547  
 3rd Qu.:0.0000    3rd Qu.:0.00   3rd Qu.:1.0000   3rd Qu.:0.0000  
 Max.   :1.0000    Max.   :1.00   Max.   :1.0000   Max.   :1.0000  
 NA's   :1175      NA's   :1175   NA's   :1175     NA's   :1175    
   simpOther       CVE_ENT        extortions      extortion_victim
 Min.   :0.00   Min.   : 1.00   Min.   : 0.0000   no :2359        
 1st Qu.:0.00   1st Qu.: 9.00   1st Qu.: 0.0000   yes: 318        
 Median :0.00   Median :16.00   Median : 0.0000   dk :  51        
 Mean   :0.02   Mean   :16.53   Mean   : 0.5788                   
 3rd Qu.:0.00   3rd Qu.:24.00   3rd Qu.: 0.0000                   
 Max.   :1.00   Max.   :32.00   Max.   :33.0000                   
 NA's   :1175                                                     
 extortions_nas    rep_extortion_victim     bribes        bribe_victim
 Min.   : 0.0000   0:2527               Min.   : 0.0000   no :2352    
 1st Qu.: 0.0000   1: 201               1st Qu.: 0.0000   yes: 323    
 Median : 0.0000                        Median : 0.0000   dk :  53    
 Mean   : 0.5898                        Mean   : 0.2969               
 3rd Qu.: 0.0000                        3rd Qu.: 0.0000               
 Max.   :33.0000                        Max.   :13.0000               
 NA's   :51                                                           
   bribes_nas          size     sector     tempsub      
 Min.   : 0.0000   Large :646   C:883   Min.   :212410  
 1st Qu.: 0.0000   Medium:686   I:877   1st Qu.:366210  
 Median : 0.0000   Small :678   S:968   Median :510010  
 Mean   : 0.3028   Micro :718           Mean   :514770  
 3rd Qu.: 0.0000                        3rd Qu.:665310  
 Max.   :13.0000                        Max.   :812910  
 NA's   :53                                             
         subsector       years         yearsquant  years_deciles
 Manufacturing:561   Min.   : 0.00   [0,8]  :623   [0,4]  :342  
 Health       :444   1st Qu.: 9.00   (8,16] :494   (25,30]:318  
 HotelsRestBar:413   Median :21.00   (16,25]:546   (16,21]:309  
 Construction :395   Mean   :20.74   (25,34]:571   (4,8]  :281  
 Maintenance  :229   3rd Qu.:31.00   (34,43]:494   (34,39]:273  
 Transport    :155   Max.   :43.00                 (30,34]:253  
 (Other)      :531                                 (Other):952  
   Prevalence       Incidence     Concentration   log_bribe_vic    
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.0529  
 1st Qu.: 45.00   1st Qu.: 64.0   1st Qu.:1.340   1st Qu.:-0.3962  
 Median : 59.00   Median :103.0   Median :1.589   Median :-0.1656  
 Mean   : 69.83   Mean   :115.2   Mean   :1.653   Mean   :-0.1099  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.2783  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.9231  
                                                                   
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.46750   Mean   :  0.6495   Mean   :-0.6592  
 3rd Qu.: 0.16603   3rd Qu.: 11.6003   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.16721  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.05638   Mean   :-0.3454   Mean   :-0.2286   Mean   :-0.03787  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index         comp_index            NOM_ENT        NOM_ABR    
 Min.   :-33.021   Min.   :-22.410   JALISCO   : 118   JAL.   : 118  
 1st Qu.: -4.556   1st Qu.: -5.118   HIDALGO   : 109   HGO.   : 109  
 Median :  1.109   Median :  1.352   GUANAJUATO: 108   GTO.   : 108  
 Mean   : -0.132   Mean   :  0.157   OAXACA    : 102   OAX.   : 102  
 3rd Qu.:  7.760   3rd Qu.:  5.002   NUEVO LEON:  97   NL     :  97  
 Max.   : 24.007   Max.   : 20.181   CHIHUAHUA :  95   CHIH.  :  95  
                                     (Other)   :2099   (Other):2099  
```

```r
nrow(enve_incvic)
```

```
[1] 2728
```

```r
nrow(enve_test)
```

```
[1] 2412
```

```r
length(unique(enve_incvic$CVE_UNICA))
```

```
[1] 2412
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
[1] 2412
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
 Min.   : 1.00   Min.   : 0.0000   no :2079         Min.   : 0.0000  
 1st Qu.: 9.00   1st Qu.: 0.0000   yes: 285         1st Qu.: 0.0000  
 Median :16.00   Median : 0.0000   dk :  48         Median : 0.0000  
 Mean   :16.47   Mean   : 0.5688                    Mean   : 0.5804  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.: 0.0000  
 Max.   :32.00   Max.   :33.0000                    Max.   :33.0000  
                                                    NA's   :48       
 rep_extortion_victim     bribes        bribe_victim   bribes_nas     
 0:2231               Min.   : 0.0000   no :2090     Min.   : 0.0000  
 1: 181               1st Qu.: 0.0000   yes: 274     1st Qu.: 0.0000  
                      Median : 0.0000   dk :  48     Median : 0.0000  
                      Mean   : 0.2786                Mean   : 0.2843  
                      3rd Qu.: 0.0000                3rd Qu.: 0.0000  
                      Max.   :13.0000                Max.   :13.0000  
                                                     NA's   :48       
   CVE_UNICA          size     sector     tempsub      
 Min.   :    60   Large :568   C:776   Min.   :212410  
 1st Qu.: 10225   Medium:611   I:792   1st Qu.:365310  
 Median : 19849   Small :597   S:844   Median :510610  
 Mean   : 32109   Micro :636           Mean   :514704  
 3rd Qu.: 53297                        3rd Qu.:665310  
 Max.   :100000                        Max.   :812910  
                                                       
         subsector       years        yearsquant  years_deciles
 Manufacturing:484   Min.   : 0.0   [0,8]  :539   [0,4]  :293  
 Health       :391   1st Qu.:10.0   (8,16] :433   (25,30]:289  
 HotelsRestBar:367   Median :21.0   (16,25]:487   (16,21]:279  
 Construction :356   Mean   :20.9   (25,34]:510   (4,8]  :246  
 Maintenance  :206   3rd Qu.:31.0   (34,43]:443   (34,39]:241  
 Transport    :135   Max.   :43.0                 (30,34]:221  
 (Other)      :473                                (Other):843  
   Prevalence       Incidence     Concentration   log_bribe_vic    
 Min.   : 19.00   Min.   : 29.0   Min.   :1.163   Min.   :-1.0529  
 1st Qu.: 45.00   1st Qu.: 63.0   1st Qu.:1.340   1st Qu.:-0.3962  
 Median : 59.00   Median :103.0   Median :1.606   Median :-0.1656  
 Mean   : 69.83   Mean   :115.4   Mean   :1.658   Mean   :-0.1035  
 3rd Qu.: 83.00   3rd Qu.:144.0   3rd Qu.:1.820   3rd Qu.: 0.2783  
 Max.   :180.00   Max.   :286.0   Max.   :2.709   Max.   : 0.9231  
                                                                   
   log_deaths       rt_deaths_wpn      log_drug_crim    
 Min.   :-2.72796   Min.   :-37.1168   Min.   :-2.6561  
 1st Qu.:-1.19062   1st Qu.: -8.1226   1st Qu.:-1.5849  
 Median :-0.09826   Median :  3.7713   Median :-1.0359  
 Mean   :-0.49289   Mean   :  0.2984   Mean   :-0.6757  
 3rd Qu.: 0.16603   3rd Qu.:  8.8654   3rd Qu.: 0.1148  
 Max.   : 1.53441   Max.   : 27.5815   Max.   : 1.9593  
                                                        
  log_oc_crim        log_wpn_crim        log_pop           log_nbus       
 Min.   :-3.23988   Min.   :-2.8933   Min.   :-1.6674   Min.   :-0.50020  
 1st Qu.:-0.46729   1st Qu.:-0.8466   1st Qu.:-0.7611   1st Qu.:-0.19996  
 Median : 0.12742   Median :-0.2070   Median :-0.2470   Median :-0.04958  
 Mean   : 0.02994   Mean   :-0.3723   Mean   :-0.2404   Mean   :-0.03879  
 3rd Qu.: 0.74911   3rd Qu.: 0.4458   3rd Qu.: 0.2893   3rd Qu.: 0.08738  
 Max.   : 1.92491   Max.   : 1.0703   Max.   : 1.4868   Max.   : 0.63217  
                                                                          
   law_index           comp_index                       NOM_ENT    
 Min.   :-33.02134   Min.   :-22.4096   HIDALGO             :  98  
 1st Qu.: -4.29495   1st Qu.: -5.1176   GUANAJUATO          :  93  
 Median :  1.13880   Median :  1.3520   JALISCO             :  93  
 Mean   : -0.00842   Mean   :  0.1609   DISTRITO FEDERAL    :  82  
 3rd Qu.:  7.76001   3rd Qu.:  5.0020   NUEVO LEON          :  82  
 Max.   : 24.00707   Max.   : 20.1805   COAHUILA DE ZARAGOZA:  80  
                                        (Other)             :1884  
    NOM_ABR     typeTelephone     typeInternet        typeStreet    
 HGO.   :  98   Min.   :0.0000   Min.   :0.000000   Min.   :0.0000  
 GTO.   :  93   1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.0000  
 JAL.   :  93   Median :0.0000   Median :0.000000   Median :0.0000  
 DF     :  82   Mean   :0.5933   Mean   :0.002488   Mean   :0.0257  
 NL     :  82   3rd Qu.:1.0000   3rd Qu.:0.000000   3rd Qu.:0.0000  
 COAH.  :  80   Max.   :5.0000   Max.   :1.000000   Max.   :5.0000  
 (Other):1884                                                       
  typePremises      typeCobro.de.piso    typeOther         simpRemote    
 Min.   :0.000000   Min.   :0.000000   Min.   :0.00000   Min.   :0.0000  
 1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.0000  
 Median :0.000000   Median :0.000000   Median :0.00000   Median :0.0000  
 Mean   :0.006633   Mean   :0.002902   Mean   :0.01285   Mean   :0.5958  
 3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:1.0000  
 Max.   :5.000000   Max.   :3.000000   Max.   :4.00000   Max.   :5.0000  
                                                                         
 simpIn.person       simpOther         cap_count         propTel      
 Min.   :0.00000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
 1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000  
 Median :0.00000   Median :0.00000   Median :1.0000   Median :0.0000  
 Mean   :0.03524   Mean   :0.01285   Mean   :0.6439   Mean   :0.4732  
 3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000  
 Max.   :5.00000   Max.   :4.00000   Max.   :5.0000   Max.   :1.0000  
                                                                      
  propInternet        propStreet       propPremises     
 Min.   :0.000000   Min.   :0.00000   Min.   :0.000000  
 1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.000000  
 Median :0.000000   Median :0.00000   Median :0.000000  
 Mean   :0.002488   Mean   :0.02135   Mean   :0.003939  
 3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.000000  
 Max.   :1.000000   Max.   :1.00000   Max.   :1.000000  
                                                        
    propPiso          propOther         propRemote     propIn.person    
 Min.   :0.000000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
 1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000  
 Median :0.000000   Median :0.00000   Median :0.0000   Median :0.00000  
 Mean   :0.001797   Mean   :0.01005   Mean   :0.4757   Mean   :0.02709  
 3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.00000  
 Max.   :1.000000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000  
                                                                        
     estTel         estInternet   estStreet         estPremises       
 Min.   : 0.0000   Min.   :0    Min.   : 0.00000   Min.   :0.0000000  
 1st Qu.: 0.0000   1st Qu.:0    1st Qu.: 0.00000   1st Qu.:0.0000000  
 Median : 0.0000   Median :0    Median : 0.00000   Median :0.0000000  
 Mean   : 0.2794   Mean   :0    Mean   : 0.01202   Mean   :0.0008292  
 3rd Qu.: 0.0000   3rd Qu.:0    3rd Qu.: 0.00000   3rd Qu.:0.0000000  
 Max.   :33.0000   Max.   :0    Max.   :27.00000   Max.   :2.0000000  
                                                                      
    estPiso            estOther          estRemote      
 Min.   :0.000000   Min.   :0.000000   Min.   : 0.0000  
 1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.: 0.0000  
 Median :0.000000   Median :0.000000   Median : 0.0000  
 Mean   :0.001658   Mean   :0.003317   Mean   : 0.2794  
 3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.: 0.0000  
 Max.   :4.000000   Max.   :4.000000   Max.   :33.0000  
                                                        
  estIn.person     
 Min.   : 0.00000  
 1st Qu.: 0.00000  
 Median : 0.00000  
 Mean   : 0.01451  
 3rd Qu.: 0.00000  
 Max.   :27.00000  
                   
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
      0         2127          --        --     88.184         --           --
      1          104         104        --      4.312     36.491        7.580
      2           43          86        43      1.783     15.088        6.268
      3           32          96        64      1.327     11.228        6.997
      4           24          96        72      0.995      8.421        6.997
      5           19          95        76      0.788      6.667        6.924
      6            8          48        40      0.332      2.807        3.499
      7            4          28        24      0.166      1.404        2.041
      8            4          32        28      0.166      1.404        2.332
      9            5          45        40      0.207      1.754        3.280
     10            4          40        36      0.166      1.404        2.915
     11            2          22        20      0.083      0.702        1.603
     12            5          60        55      0.207      1.754        4.373
     13            4          52        48      0.166      1.404        3.790
     14            5          70        65      0.207      1.754        5.102
     15            4          60        56      0.166      1.404        4.373
     19            2          38        36      0.083      0.702        2.770
     20            4          80        76      0.166      1.404        5.831
     21            2          42        40      0.083      0.702        3.061
     22            1          22        21      0.041      0.351        1.603
     23            2          46        44      0.083      0.702        3.353
     25            1          25        24      0.041      0.351        1.822
     27            1          27        26      0.041      0.351        1.968
     29            1          29        28      0.041      0.351        2.114
     32            3          96        93      0.124      1.053        6.997
     33            1          33        32      0.041      0.351        2.405

$extortions_nas


Table: Victimisation distribution of extortions_nas.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2079          --        --     87.944         --           --
      1          104         104        --      4.399     36.491        7.580
      2           43          86        43      1.819     15.088        6.268
      3           32          96        64      1.354     11.228        6.997
      4           24          96        72      1.015      8.421        6.997
      5           19          95        76      0.804      6.667        6.924
      6            8          48        40      0.338      2.807        3.499
      7            4          28        24      0.169      1.404        2.041
      8            4          32        28      0.169      1.404        2.332
      9            5          45        40      0.212      1.754        3.280
     10            4          40        36      0.169      1.404        2.915
     11            2          22        20      0.085      0.702        1.603
     12            5          60        55      0.212      1.754        4.373
     13            4          52        48      0.169      1.404        3.790
     14            5          70        65      0.212      1.754        5.102
     15            4          60        56      0.169      1.404        4.373
     19            2          38        36      0.085      0.702        2.770
     20            4          80        76      0.169      1.404        5.831
     21            2          42        40      0.085      0.702        3.061
     22            1          22        21      0.042      0.351        1.603
     23            2          46        44      0.085      0.702        3.353
     25            1          25        24      0.042      0.351        1.822
     27            1          27        26      0.042      0.351        1.968
     29            1          29        28      0.042      0.351        2.114
     32            3          96        93      0.127      1.053        6.997
     33            1          33        32      0.042      0.351        2.405
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
      0       100.000        --          --
      1        11.816   100.000     100.000
      2         7.504    63.509      92.420
      3         5.721    48.421      86.152
      4         4.395    37.193      79.155
      5         3.400    28.772      72.157
      6         2.612    22.105      65.233
      7         2.280    19.298      61.735
      8         2.114    17.895      59.694
      9         1.949    16.491      57.362
     10         1.741    14.737      54.082
     11         1.575    13.333      51.166
     12         1.493    12.632      49.563
     13         1.285    10.877      45.190
     14         1.119     9.474      41.399
     15         0.912     7.719      36.297
     19         0.746     6.316      31.924
     20         0.663     5.614      29.155
     21         0.498     4.211      23.324
     22         0.415     3.509      20.262
     23         0.373     3.158      18.659
     25         0.290     2.456      15.306
     27         0.249     2.105      13.484
     29         0.207     1.754      11.516
     32         0.166     1.404       9.402
     33         0.041     0.351       2.405

$extortions_nas


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1        12.056   100.000     100.000
      2         7.657    63.509      92.420
      3         5.838    48.421      86.152
      4         4.484    37.193      79.155
      5         3.469    28.772      72.157
      6         2.665    22.105      65.233
      7         2.327    19.298      61.735
      8         2.157    17.895      59.694
      9         1.988    16.491      57.362
     10         1.777    14.737      54.082
     11         1.607    13.333      51.166
     12         1.523    12.632      49.563
     13         1.311    10.877      45.190
     14         1.142     9.474      41.399
     15         0.931     7.719      36.297
     19         0.761     6.316      31.924
     20         0.677     5.614      29.155
     21         0.508     4.211      23.324
     22         0.423     3.509      20.262
     23         0.381     3.158      18.659
     25         0.296     2.456      15.306
     27         0.254     2.105      13.484
     29         0.212     1.754      11.516
     32         0.169     1.404       9.402
     33         0.042     0.351       2.405
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
                               0.9481401 

$mc_mean
Monte Carlo mean 
       0.6462623 

$mc_confint
  MC 2.5%  MC 97.5% 
0.6290460 0.6632319 

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
                               0.9481401 

$mc_mean
Monte Carlo mean 
       0.9462634 

$mc_confint
  MC 2.5%  MC 97.5% 
0.9387800 0.9533805 

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

               Mean   Variance   var/mu          I   p-value     df   95% Chi-sq  stars 
-----------  ------  ---------  -------  ---------  --------  -----  -----------  ------
extortions    0.569      6.971   12.256   29548.01         0   2411     2526.345  ***   

```r
my_ks_test("extortions", enve_final)
```

```

	One-sample Kolmogorov-Smirnov test

data:  extortions vs. poisson
D^+ = 0.31548, p-value < 2.2e-16
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
D^+ = 0.0046153, p-value = 0.9023
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
X-squared = 10058, df = NA, p-value = 0.0004998
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
0             2127   1365.653
1              104    777.005
2               43    220.883
3               32     41.790
4               24      5.923
5+              82      0.746

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
X-squared = 2.1489, df = 5, p-value = 0.8282
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
0             2127   2127.639
1              104     97.412
2               43     47.193
3               32     29.491
4               24     20.796
5+              82     89.469

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
  0.049827816   0.569037979 
 (0.003764024) (0.054149999)
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
      0         1269          --        --     52.612         --           --
      1          957         957        --     39.677     83.727       66.876
      2          118         236       118      4.892     10.324       16.492
      3           45         135        90      1.866      3.937        9.434
      4           12          48        36      0.498      1.050        3.354
      5           11          55        44      0.456      0.962        3.843

$typeInternet


Table: Victimisation distribution of typeInternet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2406          --        --     99.751         --           --
      1            6           6        --      0.249        100          100

$typeStreet


Table: Victimisation distribution of typeStreet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2359          --        --     97.803         --           --
      1           48          48        --      1.990     90.566       77.419
      2            3           6         3      0.124      5.660        9.677
      3            1           3         2      0.041      1.887        4.839
      5            1           5         4      0.041      1.887        8.065

$typePremises


Table: Victimisation distribution of typePremises.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2402          --        --     99.585         --           --
      1            7           7        --      0.290         70        43.75
      2            2           4         2      0.083         20        25.00
      5            1           5         4      0.041         10        31.25

$typeCobro.de.piso


Table: Victimisation distribution of typeCobro.de.piso.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2407          --        --     99.793         --           --
      1            4           4        --      0.166         80       57.143
      3            1           3         2      0.041         20       42.857

$typeOther


Table: Victimisation distribution of typeOther.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2386          --        --     98.922         --           --
      1           23          23        --      0.954     88.462       74.194
      2            2           4         2      0.083      7.692       12.903
      4            1           4         3      0.041      3.846       12.903

$simpRemote


Table: Victimisation distribution of simpRemote.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         1263          --        --     52.363         --           --
      1          963         963        --     39.925     83.812       67.015
      2          118         236       118      4.892     10.270       16.423
      3           45         135        90      1.866      3.916        9.395
      4           12          48        36      0.498      1.044        3.340
      5           11          55        44      0.456      0.957        3.827

$simpIn.person


Table: Victimisation distribution of simpIn.person.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2344          --        --     97.181         --           --
      1           59          59        --      2.446     86.765       69.412
      2            5          10         5      0.207      7.353       11.765
      3            2           6         4      0.083      2.941        7.059
      5            2          10         8      0.083      2.941       11.765

$cap_count


Table: Victimisation distribution of cap_count.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         1175          --        --     48.715         --           --
      1         1035        1035        --     42.910     83.670       66.645
      2          128         256       128      5.307     10.348       16.484
      3           47         141        94      1.949      3.800        9.079
      4           14          56        42      0.580      1.132        3.606
      5           13          65        52      0.539      1.051        4.185
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
      0       100.000        --          --
      1        47.388   100.000     100.000
      2         7.711    16.273      33.124
      3         2.819     5.949      16.632
      4         0.954     2.012       7.198
      5         0.456     0.962       3.843

$typeInternet


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.249       100         100

$typeStreet


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         2.197   100.000     100.000
      2         0.207     9.434      22.581
      3         0.083     3.774      12.903
      5         0.041     1.887       8.065

$typePremises


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.415       100      100.00
      2         0.124        30       56.25
      5         0.041        10       31.25

$typeCobro.de.piso


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.207       100     100.000
      3         0.041        20      42.857

$typeOther


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         1.078   100.000     100.000
      2         0.124    11.538      25.806
      4         0.041     3.846      12.903

$simpRemote


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1        47.637   100.000     100.000
      2         7.711    16.188      32.985
      3         2.819     5.918      16.562
      4         0.954     2.002       7.168
      5         0.456     0.957       3.827

$simpIn.person


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         2.819   100.000     100.000
      2         0.373    13.235      30.588
      3         0.166     5.882      18.824
      5         0.083     2.941      11.765

$cap_count


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1        51.285   100.000     100.000
      2         8.375    16.330      33.355
      3         3.068     5.982      16.871
      4         1.119     2.183       7.791
      5         0.539     1.051       4.185
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
                                  0.6099403 

[[1]]$mc_mean
Monte Carlo mean 
       0.6372183 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6198517 0.6539911 

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
                                 0.9975124 

[[2]]$mc_mean
Monte Carlo mean 
             NaN 

[[2]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9954395 0.9991708 

[[2]]$reps
Replicates 
      2000 

[[2]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[2]]$plot
```

```
Warning: Removed 6 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-2.png)

```


[[3]]
[[3]]$DV
[1] "typeStreet"

[[3]]$stat
Observed Gini Coefficient for typeStreet 
                               0.9810089 

[[3]]$mc_mean
Monte Carlo mean 
       0.9749485 

[[3]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9684482 0.9809287 

[[3]]$reps
Replicates 
      2000 

[[3]]$mc_test
Alternative Hypothesis 
                  TRUE 

[[3]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-3.png)

```


[[4]]
[[4]]$DV
[1] "typePremises"

[[4]]$stat
Observed Gini Coefficient for typePremises 
                                 0.9970978 

[[4]]$mc_mean
Monte Carlo mean 
       0.9935279 

[[4]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9904216 0.9962687 

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
                                      0.9984009 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9946103 0.9991708 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 1 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-5.png)

```


[[6]]
[[6]]$DV
[1] "typeOther"

[[6]]$stat
Observed Gini Coefficient for typeOther 
                              0.9908121 

[[6]]$mc_mean
Monte Carlo mean 
       0.9873704 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9825871 0.9917081 

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
                               0.6076013 

[[7]]$mc_mean
Monte Carlo mean 
       0.6360962 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6189349 0.6535694 

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
                                   0.976934 

[[8]]$mc_mean
Monte Carlo mean 
       0.9660709 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9585087 0.9730393 

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
                              0.5789066 

[[9]]$mc_mean
Monte Carlo mean 
       0.6195217 

[[9]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6026049 0.6358270 

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
                                  0.6099403 

[[1]]$mc_mean
Monte Carlo mean 
       0.6406811 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6239083 0.6566031 

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
                                 0.9975124 

[[2]]$mc_mean
Monte Carlo mean 
             NaN 

[[2]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9954395 0.9991708 

[[2]]$reps
Replicates 
      2000 

[[2]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[2]]$plot
```

```
Warning: Removed 4 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-11.png)

```


[[3]]
[[3]]$DV
[1] "typeStreet"

[[3]]$stat
Observed Gini Coefficient for typeStreet 
                               0.9810089 

[[3]]$mc_mean
Monte Carlo mean 
       0.9807102 

[[3]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9750978 0.9857474 

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
                                 0.9970978 

[[4]]$mc_mean
Monte Carlo mean 
       0.9968283 

[[4]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9947024 0.9985904 

[[4]]$reps
Replicates 
      2000 

[[4]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[4]]$plot
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-13.png)

```


[[5]]
[[5]]$DV
[1] "typeCobro.de.piso"

[[5]]$stat
Observed Gini Coefficient for typeCobro.de.piso 
                                      0.9984009 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9965238 0.9995854 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 19 rows containing non-finite values (stat_density).
```

![plot of chunk capped-extortions-tests](figure/capped-extortions-tests-14.png)

```


[[6]]
[[6]]$DV
[1] "typeOther"

[[6]]$stat
Observed Gini Coefficient for typeOther 
                              0.9908121 

[[6]]$mc_mean
Monte Carlo mean 
       0.9906872 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9869381 0.9941760 

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
                               0.6076013 

[[7]]$mc_mean
Monte Carlo mean 
       0.6383798 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6221272 0.6556980 

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
                                   0.976934 

[[8]]$mc_mean
Monte Carlo mean 
       0.9765675 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9705190 0.9821966 

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
                              0.5789066 

[[9]]$mc_mean
Monte Carlo mean 
       0.6212528 

[[9]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6037885 0.6391865 

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
typeTelephone        0.593      0.602    1.015   2447.415     0.298   2411     2526.345        
typeInternet         0.002      0.002    0.998   2406.000     0.525   2411     2526.345        
typeStreet           0.026      0.038    1.491   3594.903     0.000   2411     2526.345  ***   
typePremises         0.007      0.017    2.494   6014.000     0.000   2411     2526.345  ***   
typeCobro.de.piso    0.003      0.005    1.855   4472.429     0.000   2411     2526.345  ***   
typeOther            0.013      0.019    1.504   3625.903     0.000   2411     2526.345  ***   
simpRemote           0.596      0.602    1.010   2435.292     0.360   2411     2526.345        
simpIn.person        0.035      0.060    1.695   4086.341     0.000   2411     2526.345  ***   
cap_count            0.644      0.630    0.979   2359.317     0.770   2411     2526.345        

```r
ks_test_batch(extortion_columns_capped, enve_final, print_option = "pandoc",
              family = "poisson")
```



Table: Kolmogorov-Smirnov Tests

                                 KS.Statistic   p-value  stars 
------------------------------  -------------  --------  ------
typeTelephone vs. poisson               0.042     0.000  ***   
typeInternet vs. poisson                0.000     1.000        
typeStreet vs. poisson                  0.004     0.939        
typePremises vs. poisson                0.002     0.971        
typeCobro.de.piso vs. poisson           0.001     0.997        
typeOther vs. poisson                   0.002     0.981        
simpRemote vs. poisson                  0.043     0.000  ***   
simpIn.person vs. poisson               0.007     0.801        
cap_count vs. poisson                   0.053     0.000  ***   

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
typeStreet vs. nbinom                  0.001     0.995        
typePremises vs. nbinom                0.000     1.000        
typeCobro.de.piso vs. nbinom           0.000     1.000        
typeOther vs. nbinom                   0.000     0.999        
simpRemote vs. nbinom                  0.044     0.000  ***   
simpIn.person vs. nbinom               0.002     0.986        
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
X-squared = 210.08, df = NA, p-value = 0.0004998


[[2]]

	Chi-squared test for given probabilities

data:  typeInternet vs. poisson
X-squared = 1.5055e-05, df = 1, p-value = 0.9969


[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeStreet vs. poisson
X-squared = 671.66, df = NA, p-value = 0.0004998


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typePremises vs. poisson
X-squared = 199.07, df = NA, p-value = 0.0004998


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeCobro.de.piso vs. poisson
X-squared = 104.53, df = NA, p-value = 0.004998


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeOther vs. poisson
X-squared = 681.85, df = NA, p-value = 0.0004998


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpRemote vs. poisson
X-squared = 215.38, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpIn.person vs. poisson
X-squared = 768.94, df = NA, p-value = 0.0004998


[[9]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  cap_count vs. poisson
X-squared = 237.27, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_cap_chisq_p), format = "pandoc", digits = 3)
```

                                Chi-sq        Cramer's V   df   p.value        stars 
------------------------------  ------------  -----------  ---  -------------  ------
typeTelephone vs. poisson       210.081       --           --   0.0004997501   ***   
typeInternet vs. poisson        1.50554e-05   --           1    0.9969041            
typeStreet vs. poisson          671.6594      --           --   0.0004997501   ***   
typePremises vs. poisson        199.0748      --           --   0.0004997501   ***   
typeCobro.de.piso vs. poisson   104.5348      --           --   0.004997501    **    
typeOther vs. poisson           681.8471      --           --   0.0004997501   ***   
simpRemote vs. poisson          215.3843      --           --   0.0004997501   ***   
simpIn.person vs. poisson       768.9397      --           --   0.0004997501   ***   
cap_count vs. poisson           237.2712      --           --   0.0004997501   ***   

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
0                1269   1331.721
1                 957    791.375
2                 118    234.749
3                  45     46.340
4                  12      6.896
5                  11      0.919


Table: Expected: Poisson

typeInternet    Freq   Exp.Freq
-------------  -----  ---------
0               2406    2405.99
1                  6       6.01


Table: Expected: Poisson

typeStreet    Freq   Exp.Freq
-----------  -----  ---------
0             2359   2350.640
1               48     60.576
2                3      0.778
3+               2      0.006


Table: Expected: Poisson

typePremises    Freq   Exp.Freq
-------------  -----  ---------
0               2402   2396.014
1                  7     15.941
2+                 3      0.045


Table: Expected: Poisson

typeCobro.de.piso    Freq   Exp.Freq
------------------  -----  ---------
0                    2407   2405.028
1                       4      6.962
3                       1      0.010


Table: Expected: Poisson

typeOther    Freq   Exp.Freq
----------  -----  ---------
0            2386   2381.341
1              23     30.449
2               2      0.209
4               1      0.002


Table: Expected: Poisson

simpRemote    Freq   Exp.Freq
-----------  -----  ---------
0             1263   1329.119
1              963    792.971
2              118    235.205
3               45     46.819
4               12      6.988
5               11      0.899


Table: Expected: Poisson

simpIn.person    Freq   Exp.Freq
--------------  -----  ---------
0                2344   2328.532
1                  59     81.976
2                   5      1.470
3+                  4      0.021


Table: Expected: Poisson

cap_count    Freq   Exp.Freq
----------  -----  ---------
0            1175   1268.119
1            1035    815.146
2             128    262.060
3              47     56.198
4              14      9.104
5              13      1.374
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
X-squared = 190.74, df = NA, p-value = 0.0004998


[[2]]

	Chi-squared test for given probabilities

data:  typeInternet vs. nbinom
X-squared = 0.0011112, df = 1, p-value = 0.9734


[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeStreet vs. nbinom
X-squared = 3.8707, df = NA, p-value = 0.3948


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typePremises vs. nbinom
X-squared = 0.12848, df = NA, p-value = 1


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeCobro.de.piso vs. nbinom
X-squared = 0.070053, df = NA, p-value = 1


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  typeOther vs. nbinom
X-squared = 0.55999, df = NA, p-value = 0.932


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpRemote vs. nbinom
X-squared = 196.75, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  simpIn.person vs. nbinom
X-squared = 4.4605, df = NA, p-value = 0.3178


[[9]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  cap_count vs. nbinom
X-squared = 234.81, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_cap_chisq_nb), format = "pandoc", digits = 3)
```

                               Chi-sq        Cramer's V   df   p.value        stars 
-----------------------------  ------------  -----------  ---  -------------  ------
typeTelephone vs. nbinom       190.7388      --           --   0.0004997501   ***   
typeInternet vs. nbinom        0.001111191   --           1    0.9734078            
typeStreet vs. nbinom          3.870659      --           --   0.3948026            
typePremises vs. nbinom        0.1284779     --           --   1                    
typeCobro.de.piso vs. nbinom   0.07005256    --           --   1                    
typeOther vs. nbinom           0.5599869     --           --   0.932034             
simpRemote vs. nbinom          196.7462      --           --   0.0004997501   ***   
simpIn.person vs. nbinom       4.460488      --           --   0.3178411            
cap_count vs. nbinom           234.8147      --           --   0.0004997501   ***   

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
0                1269   1338.028
1                 957    782.854
2                 118    234.501
3                  45     47.880
4                  12      7.625
5                  11      1.113


Table: Expected: negbin

typeInternet    Freq   Exp.Freq
-------------  -----  ---------
0               2406   2406.081
1                  6      5.919


Table: Expected: negbin

typeStreet    Freq   Exp.Freq
-----------  -----  ---------
0             2359   2359.242
1               48     45.304
2                3      6.123
3                1      1.071
5                1      0.260


Table: Expected: negbin

typePremises    Freq   Exp.Freq
-------------  -----  ---------
0               2402   2401.887
1                  7      6.770
2                  2      1.935
5                  1      1.409


Table: Expected: negbin

typeCobro.de.piso    Freq   Exp.Freq
------------------  -----  ---------
0                    2407   2406.890
1                       4      3.827
3                       1      1.282


Table: Expected: negbin

typeOther    Freq   Exp.Freq
----------  -----  ---------
0            2386   2386.072
1              23     21.971
2               2      3.188
4               1      0.769


Table: Expected: negbin

simpRemote    Freq   Exp.Freq
-----------  -----  ---------
0             1263   1333.639
1              963    786.398
2              118    235.538
3               45     47.960
4               12      7.390
5               11      1.075


Table: Expected: negbin

simpIn.person    Freq   Exp.Freq
--------------  -----  ---------
0                2344   2344.368
1                  59     54.748
2                   5      9.806
3                   2      2.268
5                   2      0.811


Table: Expected: negbin

cap_count    Freq   Exp.Freq
----------  -----  ---------
0            1175   1269.492
1            1035    812.260
2             128    262.160
3              47     57.245
4              14      9.417
5              13      1.426
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
      0         2276          --        --     94.362         --           --
      1           57          57        --      2.363     41.912        8.457
      2           17          34        17      0.705     12.500        5.045
      3           15          45        30      0.622     11.029        6.677
      4           11          44        33      0.456      8.088        6.528
      5            5          25        20      0.207      3.676        3.709
      6            3          18        15      0.124      2.206        2.671
      7            2          14        12      0.083      1.471        2.077
      8            1           8         7      0.041      0.735        1.187
      9            4          36        32      0.166      2.941        5.341
     10            1          10         9      0.041      0.735        1.484
     11            1          11        10      0.041      0.735        1.632
     12            2          24        22      0.083      1.471        3.561
     13            2          26        24      0.083      1.471        3.858
     14            2          28        26      0.083      1.471        4.154
     15            2          30        28      0.083      1.471        4.451
     19            2          38        36      0.083      1.471        5.638
     20            2          40        38      0.083      1.471        5.935
     21            2          42        40      0.083      1.471        6.231
     22            1          22        21      0.041      0.735        3.264
     25            1          25        24      0.041      0.735        3.709
     32            2          64        62      0.083      1.471        9.496
     33            1          33        32      0.041      0.735        4.896

$estInternet
NULL

$estStreet


Table: Victimisation distribution of estStreet.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2409          --        --     99.876         --           --
      1            2           2        --      0.083     66.667        6.897
     27            1          27        26      0.041     33.333       93.103

$estPremises


Table: Victimisation distribution of estPremises.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2411          --        --     99.959         --           --
      2            1           2        --      0.041        100          100

$estPiso


Table: Victimisation distribution of estPiso.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2411          --        --     99.959         --           --
      4            1           4        --      0.041        100          100

$estOther


Table: Victimisation distribution of estOther.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2407          --        --     99.793         --           --
      1            4           4        --      0.166         80           50
      4            1           4         3      0.041         20           50

$estRemote


Table: Victimisation distribution of estRemote.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2276          --        --     94.362         --           --
      1           57          57        --      2.363     41.912        8.457
      2           17          34        17      0.705     12.500        5.045
      3           15          45        30      0.622     11.029        6.677
      4           11          44        33      0.456      8.088        6.528
      5            5          25        20      0.207      3.676        3.709
      6            3          18        15      0.124      2.206        2.671
      7            2          14        12      0.083      1.471        2.077
      8            1           8         7      0.041      0.735        1.187
      9            4          36        32      0.166      2.941        5.341
     10            1          10         9      0.041      0.735        1.484
     11            1          11        10      0.041      0.735        1.632
     12            2          24        22      0.083      1.471        3.561
     13            2          26        24      0.083      1.471        3.858
     14            2          28        26      0.083      1.471        4.154
     15            2          30        28      0.083      1.471        4.451
     19            2          38        36      0.083      1.471        5.638
     20            2          40        38      0.083      1.471        5.935
     21            2          42        40      0.083      1.471        6.231
     22            1          22        21      0.041      0.735        3.264
     25            1          25        24      0.041      0.735        3.709
     32            2          64        62      0.083      1.471        9.496
     33            1          33        32      0.041      0.735        4.896

$estIn.person


Table: Victimisation distribution of estIn.person.

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0         2407          --        --     99.793         --           --
      1            2           2        --      0.083         40        5.714
      2            1           2         1      0.041         20        5.714
      4            1           4         3      0.041         20       11.429
     27            1          27        26      0.041         20       77.143
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
      0       100.000        --          --
      1         5.638   100.000     100.000
      2         3.275    58.088      91.543
      3         2.570    45.588      86.499
      4         1.949    34.559      79.822
      5         1.493    26.471      73.294
      6         1.285    22.794      69.585
      7         1.161    20.588      66.914
      8         1.078    19.118      64.837
      9         1.036    18.382      63.650
     10         0.871    15.441      58.309
     11         0.829    14.706      56.825
     12         0.788    13.971      55.193
     13         0.705    12.500      51.632
     14         0.622    11.029      47.774
     15         0.539     9.559      43.620
     19         0.456     8.088      39.169
     20         0.373     6.618      33.531
     21         0.290     5.147      27.596
     22         0.207     3.676      21.365
     25         0.166     2.941      18.101
     32         0.124     2.206      14.392
     33         0.041     0.735       4.896
NULL


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.124   100.000     100.000
     27         0.041    33.333      93.103


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      2         0.041       100         100


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      4         0.041       100         100


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.207       100         100
      4         0.041        20          50


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         5.638   100.000     100.000
      2         3.275    58.088      91.543
      3         2.570    45.588      86.499
      4         1.949    34.559      79.822
      5         1.493    26.471      73.294
      6         1.285    22.794      69.585
      7         1.161    20.588      66.914
      8         1.078    19.118      64.837
      9         1.036    18.382      63.650
     10         0.871    15.441      58.309
     11         0.829    14.706      56.825
     12         0.788    13.971      55.193
     13         0.705    12.500      51.632
     14         0.622    11.029      47.774
     15         0.539     9.559      43.620
     19         0.456     8.088      39.169
     20         0.373     6.618      33.531
     21         0.290     5.147      27.596
     22         0.207     3.676      21.365
     25         0.166     2.941      18.101
     32         0.124     2.206      14.392
     33         0.041     0.735       4.896


 Events   All targets   Victims   Incidents
-------  ------------  --------  ----------
      0       100.000        --          --
      1         0.207       100     100.000
      2         0.124        60      94.286
      4         0.083        40      88.571
     27         0.041        20      77.143
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
                            0.976768 

[[1]]$mc_mean
Monte Carlo mean 
       0.7831809 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.7675959 0.7988065 

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
                              0.9994996 

[[3]]$mc_mean
Monte Carlo mean 
       0.9881602 

[[3]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9834163 0.9921227 

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
                                0.9995854 

[[4]]$mc_mean
Monte Carlo mean 
             NaN 

[[4]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9979270 0.9995854 

[[4]]$reps
Replicates 
      2000 

[[4]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[4]]$plot
```

```
Warning: Removed 279 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-4.png)

```


[[5]]
[[5]]$DV
[1] "estPiso"

[[5]]$stat
Observed Gini Coefficient for estPiso 
                            0.9995854 

[[5]]$mc_mean
Monte Carlo mean 
             NaN 

[[5]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9966833 0.9995854 

[[5]]$reps
Replicates 
      2000 

[[5]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[5]]$plot
```

```
Warning: Removed 38 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-5.png)

```


[[6]]
[[6]]$DV
[1] "estOther"

[[6]]$stat
Observed Gini Coefficient for estOther 
                             0.9985489 

[[6]]$mc_mean
Monte Carlo mean 
       0.9966691 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9941957 0.9987562 

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
                               0.976768 

[[7]]$mc_mean
Monte Carlo mean 
       0.7831807 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.7671783 0.7996649 

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
                                   0.99923 

[[8]]$mc_mean
Monte Carlo mean 
       0.9857749 

[[8]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9809287 0.9904312 

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
                            0.976768 

[[1]]$mc_mean
Monte Carlo mean 
        0.974751 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9696153 0.9794803 

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
                             0.9985489 

[[6]]$mc_mean
Monte Carlo mean 
             NaN 

[[6]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9967963 0.9995854 

[[6]]$reps
Replicates 
      2000 

[[6]]$mc_test
Alternative Hypothesis 
                 FALSE 

[[6]]$plot
```

```
Warning: Removed 17 rows containing non-finite values (stat_density).
```

![plot of chunk estimated-extortions-tests](figure/estimated-extortions-tests-10.png)

```


[[7]]
[[7]]$DV
[1] "estRemote"

[[7]]$stat
Observed Gini Coefficient for estRemote 
                               0.976768 

[[7]]$mc_mean
Monte Carlo mean 
       0.9746571 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.9694264 0.9796342 

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
estTel          0.279      3.860   13.812   33301.56         0   2411     2526.345  ***   
estInternet     0.000      0.000       --         --        --   2411     2526.345  --    
estStreet       0.012      0.303   25.205   60770.03         0   2411     2526.345  ***   
estPremises     0.001      0.002    2.000    4822.00         0   2411     2526.345  ***   
estPiso         0.002      0.007    4.000    9644.00         0   2411     2526.345  ***   
estOther        0.003      0.008    2.498    6022.00         0   2411     2526.345  ***   
estRemote       0.279      3.860   13.812   33301.56         0   2411     2526.345  ***   
estIn.person    0.015      0.311   21.452   51719.63         0   2411     2526.345  ***   

```r
ks_test_batch(extortion_columns_estimated, enve_final, print_option = "pandoc",
              family = "poisson")
```



Table: Kolmogorov-Smirnov Tests

                            KS.Statistic   p-value  stars 
-------------------------  -------------  --------  ------
estTel vs. poisson                 0.187     0.000  ***   
estInternet vs. poisson            0.000     1.000        
estStreet vs. poisson              0.011     0.580        
estPremises vs. poisson            0.000     0.999        
estPiso vs. poisson                0.001     0.992        
estOther vs. poisson               0.001     0.992        
estRemote vs. poisson              0.187     0.000  ***   
estIn.person vs. poisson           0.012     0.484        

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
X-squared = 47861, df = NA, p-value = 0.0004998


[[2]]
NULL

[[3]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estStreet vs. poisson
X-squared = 28.61, df = NA, p-value = 0.0004998


[[4]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estPremises vs. poisson
X-squared = 0.53445, df = NA, p-value = 0.5317


[[5]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estPiso vs. poisson
X-squared = 2.3045, df = NA, p-value = 0.1439


[[6]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estOther vs. poisson
X-squared = 68.969, df = NA, p-value = 0.003998


[[7]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estRemote vs. poisson
X-squared = 43927, df = NA, p-value = 0.0004998


[[8]]

	Chi-squared test for given probabilities with simulated p-value
	(based on 2000 replicates)

data:  estIn.person vs. poisson
X-squared = 2029.3, df = NA, p-value = 0.0004998
```

```r
kable(chisq_tb(ext_est_chisq_p), format = "pandoc", digits = 3)
```

                           Chi-sq      Cramer's V   df   p.value        stars 
-------------------------  ----------  -----------  ---  -------------  ------
estTel vs. poisson         47860.63    --           --   0.0004997501   ***   
estStreet vs. poisson      28.60995    --           --   0.0004997501   ***   
estPremises vs. poisson    0.5344507   --           --   0.5317341            
estPiso vs. poisson        2.304543    --           --   0.143928             
estOther vs. poisson       68.96914    --           --   0.003998001    **    
estRemote vs. poisson      43926.69    --           --   0.0004997501   ***   
estIn.person vs. poisson   2029.269    --           --   0.0004997501   ***   

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
0         2276   1822.912
1           57    510.444
2           17     71.561
3           15      6.588
4           11      0.467
5+          36      0.028
[1] "skipping null objects"


Table: Expected: Poisson

estStreet    Freq   Exp.Freq
----------  -----  ---------
0            2409   2383.130
1               2     28.683
27              1      0.188


Table: Expected: Poisson

estPremises    Freq   Exp.Freq
------------  -----  ---------
0              2411   2409.955
2                 1      2.045


Table: Expected: Poisson

estPiso    Freq   Exp.Freq
--------  -----  ---------
0          2411   2407.946
4             1      4.054


Table: Expected: Poisson

estOther    Freq   Exp.Freq
---------  -----  ---------
0           2407   2404.005
1              4      7.980
4              1      0.014


Table: Expected: Poisson

estRemote    Freq   Exp.Freq
----------  -----  ---------
0            2276   1824.586
1              57    509.167
2              17     71.100
3              15      6.643
4              11      0.474
5+             36      0.030


Table: Expected: Poisson

estIn.person    Freq   Exp.Freq
-------------  -----  ---------
0               2407   2377.120
1                  2     34.621
2                  1      0.256
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
X-squared = 4.7391, df = 5, p-value = 0.4485


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
X-squared = 0.44096, df = NA, p-value = 0.8261


[[7]]

	Chi-squared test for given probabilities

data:  estRemote vs. nbinom
X-squared = 4.9612, df = 5, p-value = 0.4206


[[8]]
NULL
```

```r
kable(chisq_tb(ext_est_chisq_nb), format = "pandoc", digits = 3)
```

                       Chi-sq      Cramer's V   df   p.value     stars 
---------------------  ----------  -----------  ---  ----------  ------
estTel vs. nbinom      4.739056    --           5    0.4485487         
estOther vs. nbinom    0.4409586   --           --   0.826087          
estRemote vs. nbinom   4.961199    --           5    0.4206339         

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
0         2276   2276.215
1           57     47.053
2           17     22.105
3           15     13.738
4           11      9.751
5+          36     43.138
[1] "skipping null objects"
[1] "skipping null objects"
[1] "skipping null objects"
[1] "skipping null objects"


Table: Expected: nbinom

estOther    Freq   Exp.Freq
---------  -----  ---------
0           2407   2406.939
1              4      3.332
4              1      1.728


Table: Expected: nbinom

estRemote    Freq   Exp.Freq
----------  -----  ---------
0            2276   2276.208
1              57     46.864
2              17     22.220
3              15     13.745
4              11      9.643
5+             36     43.321
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
[1] 17.85723
```

```r
enve_final$iqv_long <- apply(enve_final[,cols], 1, iqv)

enve_final$iqv_long <- round(enve_final$iqv_long, 2)

summary(enve_final$iqv_long)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2735  0.0000 60.0000    1175 
```

```r
# now for simp cats

cols_simp <- c("simpRemote", "simpIn.person", "simpOther")

iqv_simp <- iqv(enve_final[,cols_simp])

iqv_simp
```

```
[1] 21.06224
```

```r
enve_final$iqv_simp <- apply(enve_final[,cols_simp], 1, iqv)

enve_final$iqv_simp <- round(enve_final$iqv_simp, 2)

summary(enve_final$iqv_simp)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3419  0.0000 75.0000    1175 
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
extortions vs. iqv_long   68.44094   0.135804     --   0.06396802         
extortions vs. iqv_simp   68.44094   0.135804     --   0.07596202         

```r
print_kables(chisq_list(batch_iqvs, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) iqv_long vs. (cols) extortions

            0    1    2    3    4   5+
------  -----  ---  ---  ---  ---  ---
0        1086   63   17   15   13   37
45          0    0    1    0    0    0
53.33       1    0    0    0    0    0
60          4    0    0    0    0    0


Table: Observed counts of (rows) iqv_simp vs. (cols) extortions

            0    1    2    3    4   5+
------  -----  ---  ---  ---  ---  ---
0        1086   63   17   15   13   37
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
0        1.000   1.005    0.949   1.005   1.005   1.005
45       0.000   0.000   68.722   0.000   0.000   0.000
53.33    1.134   0.000    0.000   0.000   0.000   0.000
60       1.134   0.000    0.000   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) iqv_simp vs. (cols) extortions

             0       1        2       3       4      5+
------  ------  ------  -------  ------  ------  ------
0        1.000   1.005    0.949   1.005   1.005   1.005
56.25    0.000   0.000   68.722   0.000   0.000   0.000
66.67    1.134   0.000    0.000   0.000   0.000   0.000
75       1.134   0.000    0.000   0.000   0.000   0.000
```

```r
print_kables(chisq_list(batch_iqvs, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) iqv_long vs. (cols) extortions

              0      1        2      3      4     5+
------  -------  -----  -------  -----  -----  -----
0         88.22   5.12     1.38   1.22   1.06   3.01
45         0.00   0.00   100.00   0.00   0.00   0.00
53.33    100.00   0.00     0.00   0.00   0.00   0.00
60       100.00   0.00     0.00   0.00   0.00   0.00


Table: Row percentages of (rows) iqv_simp vs. (cols) extortions

              0      1        2      3      4     5+
------  -------  -----  -------  -----  -----  -----
0         88.22   5.12     1.38   1.22   1.06   3.01
56.25      0.00   0.00   100.00   0.00   0.00   0.00
66.67    100.00   0.00     0.00   0.00   0.00   0.00
75       100.00   0.00     0.00   0.00   0.00   0.00
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
extortion_victim vs. bribes          7.202074   0.03863893   --   0.6751624         
extortion_victim vs. bribe_victim    3.475966   0.02684319   --   0.4862569         
extortion_victim vs. size            6.206986   0.03587044   6    0.4004075         
extortion_victim vs. subsector       28.4193    0.07675435   --   0.5312344         
extortion_victim vs. yearsquant      4.446167   0.03035912   8    0.8147945         
extortion_victim vs. years_deciles   11.24365   0.04827809   --   0.896052          

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) bribes vs. (cols) extortion_victim

        no   yes   dk
---  -----  ----  ---
0     1839   257   42
1      113    17    1
2       50     2    2
3       27     3    1
4       17     3    1
5+      33     3    1


Table: Observed counts of (rows) bribe_victim vs. (cols) extortion_victim

         no   yes   dk
----  -----  ----  ---
no     1794   254   42
yes     240    28    6
dk       45     3    0


Table: Observed counts of (rows) size vs. (cols) extortion_victim

           no   yes   dk
-------  ----  ----  ---
Large     487    71   10
Medium    532    64   15
Small     509    72   16
Micro     551    78    7


Table: Observed counts of (rows) subsector vs. (cols) extortion_victim

                   no   yes   dk
---------------  ----  ----  ---
Retail             71     8    3
Mining             26     2    2
Construction      303    45    8
Manufacturing     418    62    4
Wholesale          95    16    4
Transport         115    14    6
Media              42     3    0
Finance            31     5    0
Real estate        35     3    2
Prof. services     29     8    1
Maintenance       181    21    4
Education          33     4    0
Health            338    47    6
Leisure            33     6    1
HotelsRestBar     319    41    7
Other              10     0    0


Table: Observed counts of (rows) yearsquant vs. (cols) extortion_victim

            no   yes   dk
--------  ----  ----  ---
[0,8]      461    66   12
(8,16]     364    61    8
(16,25]    425    54    8
(25,34]    446    55    9
(34,43]    383    49   11


Table: Observed counts of (rows) years_deciles vs. (cols) extortion_victim

            no   yes   dk
--------  ----  ----  ---
[0,4]      248    38    7
(4,8]      213    28    5
(8,12]     182    26    5
(12,16]    182    35    3
(16,21]    247    28    4
(21,25]    178    26    4
(25,30]    246    38    5
(30,34]    200    17    4
(34,39]    209    26    6
(39,43]    174    23    5
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "ratio", print_option = "pandoc"))
```

```


Table: Ratio of observed to expected counts of (rows) bribes vs. (cols) extortion_victim

         no     yes      dk
---  ------  ------  ------
0     0.998   1.017   0.987
1     1.001   1.098   0.384
2     1.074   0.313   1.861
3     1.010   0.819   1.621
4     0.939   1.209   2.393
5+    1.035   0.686   1.358


Table: Ratio of observed to expected counts of (rows) bribe_victim vs. (cols) extortion_victim

          no     yes     dk
----  ------  ------  -----
no     0.996   1.029   1.01
yes    1.016   0.865   1.10
dk     1.088   0.529   0.00


Table: Ratio of observed to expected counts of (rows) size vs. (cols) extortion_victim

             no     yes      dk
-------  ------  ------  ------
Large     0.995   1.058   0.885
Medium    1.010   0.886   1.234
Small     0.989   1.021   1.347
Micro     1.005   1.038   0.553


Table: Ratio of observed to expected counts of (rows) subsector vs. (cols) extortion_victim

                     no     yes      dk
---------------  ------  ------  ------
Retail            1.005   0.826   1.838
Mining            1.005   0.564   3.350
Construction      0.987   1.070   1.129
Manufacturing     1.002   1.084   0.415
Wholesale         0.958   1.177   1.748
Transport         0.988   0.878   2.233
Media             1.083   0.564   0.000
Finance           0.999   1.175   0.000
Real estate       1.015   0.635   2.512
Prof. services    0.885   1.782   1.322
Maintenance       1.019   0.863   0.976
Education         1.035   0.915   0.000
Health            1.003   1.017   0.771
Leisure           0.957   1.269   1.256
HotelsRestBar     1.008   0.945   0.958
Other             1.160   0.000   0.000


Table: Ratio of observed to expected counts of (rows) yearsquant vs. (cols) extortion_victim

              no     yes      dk
--------  ------  ------  ------
[0,8]      0.992   1.036   1.119
(8,16]     0.975   1.192   0.928
(16,25]    1.012   0.938   0.825
(25,34]    1.015   0.913   0.887
(34,43]    1.003   0.936   1.248


Table: Ratio of observed to expected counts of (rows) years_deciles vs. (cols) extortion_victim

              no     yes      dk
--------  ------  ------  ------
[0,4]      0.982   1.098   1.201
(4,8]      1.005   0.963   1.021
(8,12]     0.991   1.033   1.180
(12,16]    0.960   1.346   0.685
(16,21]    1.027   0.849   0.720
(21,25]    0.993   1.058   0.966
(25,30]    0.988   1.113   0.869
(30,34]    1.050   0.651   0.910
(34,39]    1.006   0.913   1.251
(39,43]    0.999   0.964   1.244
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) bribes vs. (cols) extortion_victim

         no     yes     dk
---  ------  ------  -----
0     86.01   12.02   1.96
1     86.26   12.98   0.76
2     92.59    3.70   3.70
3     87.10    9.68   3.23
4     80.95   14.29   4.76
5+    89.19    8.11   2.70


Table: Row percentages of (rows) bribe_victim vs. (cols) extortion_victim

          no     yes     dk
----  ------  ------  -----
no     85.84   12.15   2.01
yes    87.59   10.22   2.19
dk     93.75    6.25   0.00


Table: Row percentages of (rows) size vs. (cols) extortion_victim

             no     yes     dk
-------  ------  ------  -----
Large     85.74   12.50   1.76
Medium    87.07   10.47   2.45
Small     85.26   12.06   2.68
Micro     86.64   12.26   1.10


Table: Row percentages of (rows) subsector vs. (cols) extortion_victim

                      no     yes     dk
---------------  -------  ------  -----
Retail             86.59    9.76   3.66
Mining             86.67    6.67   6.67
Construction       85.11   12.64   2.25
Manufacturing      86.36   12.81   0.83
Wholesale          82.61   13.91   3.48
Transport          85.19   10.37   4.44
Media              93.33    6.67   0.00
Finance            86.11   13.89   0.00
Real estate        87.50    7.50   5.00
Prof. services     76.32   21.05   2.63
Maintenance        87.86   10.19   1.94
Education          89.19   10.81   0.00
Health             86.45   12.02   1.53
Leisure            82.50   15.00   2.50
HotelsRestBar      86.92   11.17   1.91
Other             100.00    0.00   0.00


Table: Row percentages of (rows) yearsquant vs. (cols) extortion_victim

              no     yes     dk
--------  ------  ------  -----
[0,8]      85.53   12.24   2.23
(8,16]     84.06   14.09   1.85
(16,25]    87.27   11.09   1.64
(25,34]    87.45   10.78   1.76
(34,43]    86.46   11.06   2.48


Table: Row percentages of (rows) years_deciles vs. (cols) extortion_victim

              no     yes     dk
--------  ------  ------  -----
[0,4]      84.64   12.97   2.39
(4,8]      86.59   11.38   2.03
(8,12]     85.45   12.21   2.35
(12,16]    82.73   15.91   1.36
(16,21]    88.53   10.04   1.43
(21,25]    85.58   12.50   1.92
(25,30]    85.12   13.15   1.73
(30,34]    90.50    7.69   1.81
(34,39]    86.72   10.79   2.49
(39,43]    86.14   11.39   2.48
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
extortions vs. bribes          14.09462   0.03418637   --   0.946027          
extortions vs. bribe_victim    8.11865    0.04102402   --   0.6001999         
extortions vs. size            17.20452   0.04876093   15   0.3067862         
extortions vs. subsector       61.11313   0.07118584   --   0.8330835         
extortions vs. yearsquant      25.77896   0.0516909    --   0.1809095         
extortions vs. years_deciles   49.47846   0.06405226   --   0.3013493         

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "observed", print_option = "pandoc"))
```

```


Table: Observed counts of (rows) bribes vs. (cols) extortions

         0    1    2    3    4   5+
---  -----  ---  ---  ---  ---  ---
0     1881   94   41   29   20   73
1      114    6    1    2    3    5
2       52    1    0    0    0    1
3       28    1    1    0    0    1
4       18    1    0    0    1    1
5+      34    1    0    1    0    1


Table: Observed counts of (rows) bribe_victim vs. (cols) extortions

          0    1    2    3    4   5+
----  -----  ---  ---  ---  ---  ---
no     1836   93   39   29   20   73
yes     246   10    2    3    4    9
dk       45    1    2    0    0    0


Table: Observed counts of (rows) size vs. (cols) extortions

            0    1    2    3    4   5+
-------  ----  ---  ---  ---  ---  ---
Large     497   24    9    7    4   27
Medium    547   16    9   11    6   22
Small     525   32   12    7    4   17
Micro     558   32   13    7   10   16


Table: Observed counts of (rows) subsector vs. (cols) extortions

                    0    1    2    3    4   5+
---------------  ----  ---  ---  ---  ---  ---
Retail             74    2    0    2    0    4
Mining             28    1    0    0    0    1
Construction      311   15    7    5    2   16
Manufacturing     422   21   13    7    8   13
Wholesale          99    9    3    2    0    2
Transport         121    8    1    0    2    3
Media              42    0    0    0    0    3
Finance            31    0    2    0    1    2
Real estate        37    1    0    0    0    2
Prof. services     30    1    2    1    0    4
Maintenance       185   10    2    3    1    5
Education          33    1    0    1    0    2
Health            344   18    8    5    7    9
Leisure            34    2    0    1    0    3
HotelsRestBar     326   15    5    5    3   13
Other              10    0    0    0    0    0


Table: Observed counts of (rows) yearsquant vs. (cols) extortions

             0    1    2    3    4   5+
--------  ----  ---  ---  ---  ---  ---
[0,8]      473   25    7   13    7   14
(8,16]     372   23    8    8    5   17
(16,25]    433   21   14    4    1   14
(25,34]    455   19    8    1    5   22
(34,43]    394   16    6    6    6   15


Table: Observed counts of (rows) years_deciles vs. (cols) extortions

             0    1    2    3    4   5+
--------  ----  ---  ---  ---  ---  ---
[0,4]      255   11    6    6    6    9
(4,8]      218   14    1    7    1    5
(8,12]     187    9    4    3    3    7
(12,16]    185   14    4    5    2   10
(16,21]    251   12    6    2    1    7
(21,25]    182    9    8    2    0    7
(25,30]    251   11    6    0    3   18
(30,34]    204    8    2    1    2    4
(34,39]    215    9    4    3    2    8
(39,43]    179    7    2    3    4    7
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "ratio", print_option = "pandoc"))
```

```


Table: Ratio of observed to expected counts of (rows) bribes vs. (cols) extortions

          0       1       2       3       4      5+
---  ------  ------  ------  ------  ------  ------
0     0.998   1.020   1.076   1.022   0.940   1.004
1     0.987   1.062   0.428   1.151   2.302   1.123
2     1.092   0.429   0.000   0.000   0.000   0.545
3     1.024   0.748   1.809   0.000   0.000   0.949
4     0.972   1.104   0.000   0.000   4.786   1.401
5+    1.042   0.627   0.000   2.037   0.000   0.795


Table: Ratio of observed to expected counts of (rows) bribe_victim vs. (cols) extortions

           0       1       2       3       4      5+
----  ------  ------  ------  ------  ------  ------
no     0.996   1.032   1.047   1.046   0.962   1.027
yes    1.018   0.846   0.409   0.825   1.467   0.966
dk     1.063   0.483   2.337   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) size vs. (cols) extortions

              0       1       2       3       4      5+
-------  ------  ------  ------  ------  ------  ------
Large     0.992   0.980   0.889   0.929   0.708   1.398
Medium    1.015   0.607   0.826   1.357   0.987   1.059
Small     0.997   1.243   1.127   0.884   0.673   0.838
Micro     0.995   1.167   1.147   0.830   1.580   0.740


Table: Ratio of observed to expected counts of (rows) subsector vs. (cols) extortions

                      0       1       2       3       4      5+
---------------  ------  ------  ------  ------  ------  ------
Retail            1.023   0.566   0.000   1.838   0.000   1.435
Mining            1.058   0.773   0.000   0.000   0.000   0.980
Construction      0.991   0.977   1.103   1.059   0.565   1.322
Manufacturing     0.989   1.006   1.507   1.090   1.661   0.790
Wholesale         0.976   1.815   1.463   1.311   0.000   0.512
Transport         1.016   1.374   0.416   0.000   1.489   0.654
Media             1.058   0.000   0.000   0.000   0.000   1.961
Finance           0.976   0.000   3.116   0.000   2.792   1.634
Real estate       1.049   0.580   0.000   0.000   0.000   1.471
Prof. services    0.895   0.610   2.952   1.984   0.000   3.096
Maintenance       1.018   1.126   0.545   1.098   0.488   0.714
Education         1.011   0.627   0.000   2.037   0.000   1.590
Health            0.998   1.068   1.148   0.964   1.799   0.677
Leisure           0.964   1.160   0.000   1.884   0.000   2.206
HotelsRestBar     1.007   0.948   0.764   1.027   0.822   1.042
Other             1.134   0.000   0.000   0.000   0.000   0.000


Table: Ratio of observed to expected counts of (rows) yearsquant vs. (cols) extortions

               0       1       2       3       4      5+
--------  ------  ------  ------  ------  ------  ------
[0,8]      0.995   1.076   0.728   1.818   1.305   0.764
(8,16]     0.974   1.232   1.036   1.393   1.161   1.155
(16,25]    1.008   1.000   1.613   0.619   0.206   0.846
(25,34]    1.012   0.864   0.880   0.148   0.985   1.269
(34,43]    1.009   0.838   0.760   1.021   1.361   0.996


Table: Ratio of observed to expected counts of (rows) years_deciles vs. (cols) extortions

               0       1       2       3       4      5+
--------  ------  ------  ------  ------  ------  ------
[0,4]      0.987   0.871   1.149   1.544   2.058   0.904
(4,8]      1.005   1.320   0.228   2.145   0.409   0.598
(8,12]     0.996   0.980   1.053   1.062   1.415   0.967
(12,16]    0.954   1.476   1.020   1.713   0.914   1.337
(16,21]    1.020   0.998   1.206   0.540   0.360   0.738
(21,25]    0.992   1.004   2.157   0.725   0.000   0.990
(25,30]    0.985   0.883   1.165   0.000   1.043   1.832
(30,34]    1.047   0.840   0.508   0.341   0.910   0.532
(34,39]    1.012   0.866   0.931   0.938   0.834   0.976
(39,43]    1.005   0.804   0.555   1.119   1.990   1.019
```

```r
print_kables(chisq_list(ext_p_batch_chsq, option = "percent", print_option = "pandoc"))
```

```


Table: Row percentages of (rows) bribes vs. (cols) extortions

          0      1      2      3      4     5+
---  ------  -----  -----  -----  -----  -----
0     87.98   4.40   1.92   1.36   0.94   3.41
1     87.02   4.58   0.76   1.53   2.29   3.82
2     96.30   1.85   0.00   0.00   0.00   1.85
3     90.32   3.23   3.23   0.00   0.00   3.23
4     85.71   4.76   0.00   0.00   4.76   4.76
5+    91.89   2.70   0.00   2.70   0.00   2.70


Table: Row percentages of (rows) bribe_victim vs. (cols) extortions

           0      1      2      3      4     5+
----  ------  -----  -----  -----  -----  -----
no     87.85   4.45   1.87   1.39   0.96   3.49
yes    89.78   3.65   0.73   1.09   1.46   3.28
dk     93.75   2.08   4.17   0.00   0.00   0.00


Table: Row percentages of (rows) size vs. (cols) extortions

              0      1      2      3      4     5+
-------  ------  -----  -----  -----  -----  -----
Large     87.50   4.23   1.58   1.23   0.70   4.75
Medium    89.53   2.62   1.47   1.80   0.98   3.60
Small     87.94   5.36   2.01   1.17   0.67   2.85
Micro     87.74   5.03   2.04   1.10   1.57   2.52


Table: Row percentages of (rows) subsector vs. (cols) extortions

                       0      1      2      3      4      5+
---------------  -------  -----  -----  -----  -----  ------
Retail             90.24   2.44   0.00   2.44   0.00    4.88
Mining             93.33   3.33   0.00   0.00   0.00    3.33
Construction       87.36   4.21   1.97   1.40   0.56    4.49
Manufacturing      87.19   4.34   2.69   1.45   1.65    2.69
Wholesale          86.09   7.83   2.61   1.74   0.00    1.74
Transport          89.63   5.93   0.74   0.00   1.48    2.22
Media              93.33   0.00   0.00   0.00   0.00    6.67
Finance            86.11   0.00   5.56   0.00   2.78    5.56
Real estate        92.50   2.50   0.00   0.00   0.00    5.00
Prof. services     78.95   2.63   5.26   2.63   0.00   10.53
Maintenance        89.81   4.85   0.97   1.46   0.49    2.43
Education          89.19   2.70   0.00   2.70   0.00    5.41
Health             87.98   4.60   2.05   1.28   1.79    2.30
Leisure            85.00   5.00   0.00   2.50   0.00    7.50
HotelsRestBar      88.83   4.09   1.36   1.36   0.82    3.54
Other             100.00   0.00   0.00   0.00   0.00    0.00


Table: Row percentages of (rows) yearsquant vs. (cols) extortions

               0      1      2      3      4     5+
--------  ------  -----  -----  -----  -----  -----
[0,8]      87.76   4.64   1.30   2.41   1.30   2.60
(8,16]     85.91   5.31   1.85   1.85   1.15   3.93
(16,25]    88.91   4.31   2.87   0.82   0.21   2.87
(25,34]    89.22   3.73   1.57   0.20   0.98   4.31
(34,43]    88.94   3.61   1.35   1.35   1.35   3.39


Table: Row percentages of (rows) years_deciles vs. (cols) extortions

               0      1      2      3      4     5+
--------  ------  -----  -----  -----  -----  -----
[0,4]      87.03   3.75   2.05   2.05   2.05   3.07
(4,8]      88.62   5.69   0.41   2.85   0.41   2.03
(8,12]     87.79   4.23   1.88   1.41   1.41   3.29
(12,16]    84.09   6.36   1.82   2.27   0.91   4.55
(16,21]    89.96   4.30   2.15   0.72   0.36   2.51
(21,25]    87.50   4.33   3.85   0.96   0.00   3.37
(25,30]    86.85   3.81   2.08   0.00   1.04   6.23
(30,34]    92.31   3.62   0.90   0.45   0.90   1.81
(34,39]    89.21   3.73   1.66   1.24   0.83   3.32
(39,43]    88.61   3.47   0.99   1.49   1.98   3.47
```

## Extortion vs Years

A brief EDA analysis of the years variable, using a facet plot of the bivariate relationship with extortion incidents.


```r
enve_final %>%
    dplyr::select(extortions, years, yearsquant) %>%
    mutate("All years" = "All years")-> gg_ext_years1

melt(gg_ext_years1,
     id.vars = c("extortions", "years"),
     value.name = "yearsquant") -> gg_ext_years1
```

```
Warning: attributes are not identical across measure variables; they will
be dropped
```

```r
enve_final %>%
    dplyr::select(extortions, years, years_deciles) %>%
    mutate("All years" = "All years")-> gg_ext_years2

melt(gg_ext_years2,
     id.vars = c("extortions", "years"),
     value.name = "years_deciles") -> gg_ext_years2
```

```
Warning: attributes are not identical across measure variables; they will
be dropped
```

```r
cbind(gg_ext_years1, years_deciles = gg_ext_years2$years_deciles) -> gg_ext_years

gg_ext_years$yearsquant <- factor(gg_ext_years$yearsquant)

gg_ext_years$yearsquant <- relevel(gg_ext_years$yearsquant, ref = "All years")

gg_ext_years$yearsquant <- factor(gg_ext_years$yearsquant,
                                  levels = c("All years", levels(enve_final$yearsquant)))

gg_ext_years$years_deciles <- relevel(gg_ext_years$years_deciles, ref = "All years")

gg_ext_years$years_deciles <- factor(gg_ext_years$years_deciles,
                                  levels = c("All years", levels(enve_final$years_deciles)))

gg_ext_years <- dplyr::select(gg_ext_years, -variable)

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

```r
# Excluding the all business line

years_scatterplots <- ggplot(subset(gg_ext_years, yearsquant != "All years"),
                             aes(x = years, y = extortions))

years_scatterplots +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "lm", aes(colour = yearsquant))
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-3.png)

```r
## Only for victims

years_scatterplots <- ggplot(subset(gg_ext_years, yearsquant != "All years" &
                                        extortions > 0),
                             aes(x = years, y = extortions))

years_scatterplots +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "lm", aes(colour = yearsquant), se = FALSE)
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-4.png)

```r
years_scatterplots +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "lm", aes(colour = yearsquant), se = FALSE) +
    scale_x_sqrt()
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-5.png)

```r
# Only for victims with a grid

years_scatterplots <- ggplot(subset(gg_ext_years, extortions > 0),
                             aes(x = years, y = extortions))


# Quintiles

years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_wrap(~ yearsquant, scales = "free_x") +
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-6.png)

```r
# For deciles

years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_wrap(~ years_deciles, scales = "free_x") +
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-7.png)

```r
## wrap grid victims not a victim
gg_ext_years$victim <- ifelse(gg_ext_years$extortions > 0, "yes", "no")

gg_ext_years$pop <- "All businesses"

subset(gg_ext_years, victim == "yes") -> gg_ext_years_tmp

gg_ext_years_tmp$pop <- "Victims"

rbind(gg_ext_years, gg_ext_years_tmp) -> gg_ext_years

select(gg_ext_years, - victim) -> gg_ext_years

years_scatterplots <- ggplot(gg_ext_years,
                             aes(x = years, y = extortions))

years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_grid(pop ~ yearsquant, scales = "free_x") +
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-8.png)

```r
years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_grid(pop ~ years_deciles, scales = "free_x") +
    geom_smooth(method = "lm")
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-9.png)

```r
years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_grid(pop ~ . , scales = "free_x") +
    geom_smooth(method = "lm", aes(colour = yearsquant), se = FALSE)
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-10.png)

```r
years_scatterplots <- ggplot(subset(gg_ext_years, yearsquant != "All years"),
                             aes(x = years, y = extortions))

years_scatterplots +
    geom_point() +
    theme_bw() +
    facet_grid(pop ~ . , scales = "free_x") +
    geom_smooth(method = "lm", aes(colour = yearsquant), se = FALSE)
```

![plot of chunk years-plots-quintiles](figure/years-plots-quintiles-11.png)

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

state_gini_victims <- by(enve_final$extortions[enve_final$extortions > 0],
                         enve_final$NOM_ENT[enve_final$extortions > 0],
                         ineq::Gini)

state_gini_df$Gini_vic <- data.frame(state_gini_victims[1:32])[,1]

state_gini_df$NOM_ENT <- as.factor(rownames(state_gini_df))

state_gini_tests <- by(enve_final$extortions, enve_final$NOM_ENT, mc_gini_test)

state_mc_gini_l <- lapply(state_gini_tests[1:32], function(x) x$mc_mean)

state_mc_gini_df <- data.frame(P.Gini= unlist(state_mc_gini_l))

rownames(state_mc_gini_df) <- names(state_mc_gini_l)

state_mc_gini_df$NOM_ENT <- as.factor(names(state_mc_gini_l))

state_mc_gini_hyp <- sapply(state_gini_tests[1:32],
                            function(x) if(x$mc_test == TRUE) "***"
                                        else "")

unlist(state_mc_gini_hyp) -> state_mc_gini_df$test

#### now mc gini test for victims

state_gini_vic_tests <- by(enve_final$extortions[enve_final$extortions > 0],
                         enve_final$NOM_ENT[enve_final$extortions > 0],
                         mc_gini_test)

state_mc_gini_vic_l <- lapply(state_gini_vic_tests[1:32], function(x) x$mc_mean)

state_mc_gini_df$P.Gini.vic <- data.frame(unlist(state_mc_gini_vic_l))[,1]

state_mc_gini_vic_hyp <- sapply(state_gini_vic_tests[1:32],
                            function(x) if(x$mc_test == TRUE) "***"
                                        else "")

unlist(state_mc_gini_vic_hyp) -> state_mc_gini_df$vic.test

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
state_summary$Gini_vic <- as.numeric(unname(state_summary$Gini_vic))

kable(state_summary, format = "pandoc", digits = 3)
```



 CVE_ENT  NOM_ENT                             n   incidents   victims   rep_ext_vics   rep_inci   bribe_vics    Gini   Gini_vic   P.Gini  test    P.Gini.vic  vic.test 
--------  --------------------------------  ---  ----------  --------  -------------  ---------  -----------  ------  ---------  -------  -----  -----------  ---------
       1  AGUASCALIENTES                     78          46        11              7         39           12   0.930      0.506    0.635  ***          0.252  ***      
       2  BAJA CALIFORNIA                    68          42        12              9         33            4   0.892      0.389    0.626  ***          0.275           
       3  BAJA CALIFORNIA SUR                71          42         6              3         39            5   0.971      0.659    0.635  ***          0.179  ***      
       4  CAMPECHE                           54          16         4              3         13            9   0.954      0.375    0.771  ***          0.217           
       5  COAHUILA DE ZARAGOZA               80          62         9              6         56            6   0.952      0.573    0.576  ***          0.190  ***      
       6  COLIMA                             68          44         6              4         40            9   0.965      0.598    0.615  ***          0.173  ***      
       7  CHIAPAS                            67          83        12              8         75            5   0.909      0.491    0.475  ***          0.195  ***      
       8  CHIHUAHUA                          78          51         6              3         48            8   0.967      0.565    0.611  ***          0.161  ***      
       9  DISTRITO FEDERAL                   82          31         9              6         25           10   0.926      0.330    0.727  ***          0.270           
      10  DURANGO                            70          37        13              6         31           11   0.906      0.495    0.659  ***          0.309  ***      
      11  GUANAJUATO                         93          47         9              7         40           10   0.947      0.454    0.670  ***          0.218  ***      
      12  GUERRERO                           77          32         6              3         29            4   0.962      0.510    0.708  ***          0.205  ***      
      13  HIDALGO                            98          46        12              8         38           12   0.945      0.547    0.686  ***          0.262  ***      
      14  JALISCO                            93          24         9              4         20           10   0.949      0.472    0.793  ***          0.304           
      15  MEXICO                             77          63        12              9         54            8   0.923      0.504    0.563  ***          0.225  ***      
      16  MICHOACAN DE OCAMPO                69          50         7              7         43            5   0.947      0.474    0.592  ***          0.181  ***      
      17  MORELOS                            70          31         8              5         26            7   0.941      0.488    0.696  ***          0.248  ***      
      18  NAYARIT                            79          38         7              4         34           10   0.961      0.556    0.678  ***          0.206  ***      
      19  NUEVO LEON                         82         105        15             11         94           10   0.927      0.599    0.468  ***          0.197  ***      
      20  OAXACA                             79           5         4              1          4           16   0.957      0.150       --                  --           
      21  PUEBLA                             80          64        11              9         55            8   0.927      0.472    0.567  ***          0.211  ***      
      22  QUERETARO                          79          28         8              7         21            5   0.928      0.286    0.739  ***          0.267           
      23  QUINTANA ROO                       67          15         8              4         11            3   0.915      0.292    0.816  ***          0.356           
      24  SAN LUIS POTOSI                    77          33         7              6         27           10   0.946      0.407    0.702  ***          0.222  ***      
      25  SINALOA                            72          77        12              8         69           11   0.911      0.469    0.506  ***          0.204  ***      
      26  SONORA                             77          52         9              4         48           16   0.960      0.654    0.606  ***          0.210  ***      
      27  TABASCO                            64          25         4              2         23            9   0.974      0.590    0.718  ***          0.171  ***      
      28  TAMAULIPAS                         72          35         9              5         30            8   0.940      0.521    0.676  ***          0.251  ***      
      29  TLAXCALA                           75          25        10              6         19            9   0.906      0.292    0.751  ***          0.321           
      30  VERACRUZ DE IGNACIO DE LA LLAVE    72          29         5              3         26           12   0.961      0.441    0.714  ***          0.189  ***      
      31  YUCATAN                            73          25        13              6         19            7   0.880      0.326    0.746  ***          0.369           
      32  ZACATECAS                          71          69        12              7         62            5   0.937      0.627    0.526  ***          0.213  ***      

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
      0           67          --        --     85.897         --           --
      1            4           4        --      5.128     36.364        8.696
      2            2           4         2      2.564     18.182        8.696
      3            2           6         4      2.564     18.182       13.043
      8            1           8         7      1.282      9.091       17.391
      9            1           9         8      1.282      9.091       19.565
     15            1          15        14      1.282      9.091       32.609


Table: BAJA CALIFORNIA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           56          --        --     82.353         --           --
      1            3           3        --      4.412     25.000        7.143
      2            3           6         3      4.412     25.000       14.286
      3            2           6         4      2.941     16.667       14.286
      5            1           5         4      1.471      8.333       11.905
      6            1           6         5      1.471      8.333       14.286
      7            1           7         6      1.471      8.333       16.667
      9            1           9         8      1.471      8.333       21.429


Table: BAJA CALIFORNIA SUR

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           65          --        --     91.549         --           --
      1            3           3        --      4.225     50.000        7.143
      3            1           3         2      1.408     16.667        7.143
      4            1           4         3      1.408     16.667        9.524
     32            1          32        31      1.408     16.667       76.190


Table: CAMPECHE

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           50          --        --     92.593         --           --
      1            1           1        --      1.852         25         6.25
      2            1           2         1      1.852         25        12.50
      5            1           5         4      1.852         25        31.25
      8            1           8         7      1.852         25        50.00


Table: CHIAPAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           55          --        --     82.090         --           --
      1            4           4        --      5.970     33.333        4.819
      2            2           4         2      2.985     16.667        4.819
      6            1           6         5      1.493      8.333        7.229
     11            2          22        20      2.985     16.667       26.506
     13            1          13        12      1.493      8.333       15.663
     15            1          15        14      1.493      8.333       18.072
     19            1          19        18      1.493      8.333       22.892


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
      0           71          --        --      88.75         --           --
      1            3           3        --       3.75     33.333        4.839
      2            2           4         2       2.50     22.222        6.452
      3            1           3         2       1.25     11.111        4.839
     10            1          10         9       1.25     11.111       16.129
     20            1          20        19       1.25     11.111       32.258
     22            1          22        21       1.25     11.111       35.484


Table: COLIMA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           62          --        --     91.176         --           --
      1            2           2        --      2.941     33.333        4.545
      2            1           2         1      1.471     16.667        4.545
      3            1           3         2      1.471     16.667        6.818
     10            1          10         9      1.471     16.667       22.727
     27            1          27        26      1.471     16.667       61.364


Table: DISTRITO FEDERAL

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           73          --        --     89.024         --           --
      1            3           3        --      3.659     33.333        9.677
      3            2           6         4      2.439     22.222       19.355
      4            1           4         3      1.220     11.111       12.903
      6            3          18        15      3.659     33.333       58.065


Table: DURANGO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           57          --        --     81.429         --           --
      1            7           7        --     10.000     53.846       18.919
      2            2           4         2      2.857     15.385       10.811
      3            1           3         2      1.429      7.692        8.108
      4            1           4         3      1.429      7.692       10.811
      5            1           5         4      1.429      7.692       13.514
     14            1          14        13      1.429      7.692       37.838


Table: GUANAJUATO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           84          --        --     90.323         --           --
      1            2           2        --      2.151     22.222        4.255
      2            2           4         2      2.151     22.222        8.511
      4            1           4         3      1.075     11.111        8.511
      5            1           5         4      1.075     11.111       10.638
      6            1           6         5      1.075     11.111       12.766
     12            1          12        11      1.075     11.111       25.532
     14            1          14        13      1.075     11.111       29.787


Table: GUERRERO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           71          --        --     92.208         --           --
      1            3           3        --      3.896     50.000        9.375
      5            1           5         4      1.299     16.667       15.625
      9            1           9         8      1.299     16.667       28.125
     15            1          15        14      1.299     16.667       46.875


Table: HIDALGO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           86          --        --     87.755         --           --
      1            4           4        --      4.082     33.333        8.696
      2            4           8         4      4.082     33.333       17.391
      3            2           6         4      2.041     16.667       13.043
      5            1           5         4      1.020      8.333       10.870
     23            1          23        22      1.020      8.333       50.000


Table: JALISCO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           84          --        --     90.323         --           --
      1            5           5        --      5.376     55.556       20.833
      2            2           4         2      2.151     22.222       16.667
      5            1           5         4      1.075     11.111       20.833
     10            1          10         9      1.075     11.111       41.667


Table: MEXICO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           65          --        --     84.416         --           --
      1            3           3        --      3.896     25.000        4.762
      2            2           4         2      2.597     16.667        6.349
      3            1           3         2      1.299      8.333        4.762
      4            2           8         6      2.597     16.667       12.698
      5            1           5         4      1.299      8.333        7.937
      7            1           7         6      1.299      8.333       11.111
     13            1          13        12      1.299      8.333       20.635
     20            1          20        19      1.299      8.333       31.746


Table: MICHOACAN DE OCAMPO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           62          --        --     89.855         --           --
      2            2           4        --      2.899     28.571            8
      4            3          12         9      4.348     42.857           24
      9            1           9         8      1.449     14.286           18
     25            1          25        24      1.449     14.286           50


Table: MORELOS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           62          --        --     88.571         --           --
      1            3           3        --      4.286       37.5        9.677
      2            1           2         1      1.429       12.5        6.452
      3            1           3         2      1.429       12.5        9.677
      4            1           4         3      1.429       12.5       12.903
      5            1           5         4      1.429       12.5       16.129
     14            1          14        13      1.429       12.5       45.161


Table: NAYARIT

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           72          --        --     91.139         --           --
      1            3           3        --      3.797     42.857        7.895
      3            1           3         2      1.266     14.286        7.895
      4            1           4         3      1.266     14.286       10.526
      8            1           8         7      1.266     14.286       21.053
     20            1          20        19      1.266     14.286       52.632


Table: NUEVO LEON

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           67          --        --     81.707         --           --
      1            4           4        --      4.878     26.667        3.810
      2            1           2         1      1.220      6.667        1.905
      3            4          12         8      4.878     26.667       11.429
      4            2           8         6      2.439     13.333        7.619
      5            1           5         4      1.220      6.667        4.762
     10            1          10         9      1.220      6.667        9.524
     32            2          64        62      2.439     13.333       60.952


Table: OAXACA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           75          --        --     94.937         --           --
      1            3           3        --      3.797         75           60
      2            1           2         1      1.266         25           40


Table: PUEBLA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           69          --        --      86.25         --           --
      1            2           2        --       2.50     18.182        3.125
      2            2           4         2       2.50     18.182        6.250
      4            2           8         6       2.50     18.182       12.500
      5            2          10         8       2.50     18.182       15.625
      7            1           7         6       1.25      9.091       10.938
     12            1          12        11       1.25      9.091       18.750
     21            1          21        20       1.25      9.091       32.812


Table: QUERETARO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           71          --        --     89.873         --           --
      1            1           1        --      1.266       12.5        3.571
      2            2           4         2      2.532       25.0       14.286
      3            1           3         2      1.266       12.5       10.714
      4            2           8         6      2.532       25.0       28.571
      5            1           5         4      1.266       12.5       17.857
      7            1           7         6      1.266       12.5       25.000


Table: QUINTANA ROO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           59          --        --     88.060         --           --
      1            4           4        --      5.970       50.0       26.667
      2            2           4         2      2.985       25.0       26.667
      3            1           3         2      1.493       12.5       20.000
      4            1           4         3      1.493       12.5       26.667


Table: SAN LUIS POTOSI

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           70          --        --     90.909         --           --
      1            1           1        --      1.299     14.286        3.030
      2            2           4         2      2.597     28.571       12.121
      4            1           4         3      1.299     14.286       12.121
      5            1           5         4      1.299     14.286       15.152
      6            1           6         5      1.299     14.286       18.182
     13            1          13        12      1.299     14.286       39.394


Table: SINALOA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           60          --        --     83.333         --           --
      1            4           4        --      5.556     33.333        5.195
      3            2           6         4      2.778     16.667        7.792
      4            1           4         3      1.389      8.333        5.195
      8            1           8         7      1.389      8.333       10.390
     12            1          12        11      1.389      8.333       15.584
     14            2          28        26      2.778     16.667       36.364
     15            1          15        14      1.389      8.333       19.481


Table: SONORA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           68          --        --     88.312         --           --
      1            5           5        --      6.494     55.556        9.615
      3            1           3         2      1.299     11.111        5.769
      5            1           5         4      1.299     11.111        9.615
      6            1           6         5      1.299     11.111       11.538
     33            1          33        32      1.299     11.111       63.462


Table: TABASCO

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           60          --        --     93.750         --           --
      1            2           2        --      3.125         50            8
      3            1           3         2      1.562         25           12
     20            1          20        19      1.562         25           80


Table: TAMAULIPAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           63          --        --     87.500         --           --
      1            4           4        --      5.556     44.444       11.429
      2            2           4         2      2.778     22.222       11.429
      3            1           3         2      1.389     11.111        8.571
     12            2          24        22      2.778     22.222       68.571


Table: TLAXCALA

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           65          --        --     86.667         --           --
      1            4           4        --      5.333         40           16
      3            4          12         8      5.333         40           48
      4            1           4         3      1.333         10           16
      5            1           5         4      1.333         10           20


Table: VERACRUZ DE IGNACIO DE LA LLAVE

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           67          --        --     93.056         --           --
      1            2           2        --      2.778         40        6.897
      5            1           5         4      1.389         20       17.241
      9            1           9         8      1.389         20       31.034
     13            1          13        12      1.389         20       44.828


Table: YUCATAN

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           60          --        --     82.192         --           --
      1            7           7        --      9.589     53.846           28
      2            3           6         3      4.110     23.077           24
      3            1           3         2      1.370      7.692           12
      4            1           4         3      1.370      7.692           16
      5            1           5         4      1.370      7.692           20


Table: ZACATECAS

 Events   Prevalence   Incidence   Repeats   Target_%   Victim_%   Incident_%
-------  -----------  ----------  --------  ---------  ---------  -----------
      0           59          --        --     83.099         --           --
      1            5           5        --      7.042     41.667        7.246
      2            2           4         2      2.817     16.667        5.797
      3            1           3         2      1.408      8.333        4.348
      4            1           4         3      1.408      8.333        5.797
      5            1           5         4      1.408      8.333        7.246
     19            1          19        18      1.408      8.333       27.536
     29            1          29        28      1.408      8.333       42.029
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
       1           12      -0.1954920        0.3375144    0.5330065  FALSE 
       2            4      -0.1656391       -0.7610978   -0.5954588  FALSE 
       3            5      -0.1954920       -0.5379543   -0.3424623  FALSE 
       4            9      -0.0544134        0.0498324    0.1042458  FALSE 
       5            6      -0.5565054       -0.3556327    0.2008726  FALSE 
       6            9      -1.0529423        0.0498324    1.1027746  FALSE 
       7            5       0.3855379       -0.5379543   -0.9234922  FALSE 
       8            8      -0.2580124       -0.0679507    0.1900617  FALSE 
       9           10      -0.2262637        0.1551929    0.3814566  FALSE 
      10           11      -0.7475606        0.2505031    0.9980637  FALSE 
      11           10      -0.3961627        0.1551929    0.5513556  FALSE 
      12            4       0.4976552       -0.7610978   -1.2587530  FALSE 
      13           12      -0.4339030        0.3375144    0.7714175  FALSE 
      14           10      -0.1084806        0.1551929    0.2636735  FALSE 
      15            8       0.2782923       -0.0679507   -0.3462430  FALSE 
      16            5       0.0215725       -0.5379543   -0.5595268  FALSE 
      17            7       0.4188743       -0.2014821   -0.6203563  FALSE 
      18           10      -0.1954920        0.1551929    0.3506849  FALSE 
      19           10      -0.2580124        0.1551929    0.4132053  FALSE 
      20           16       0.3855379        0.6251965    0.2396587  FALSE 
      21            8       0.3510517       -0.0679507   -0.4190023  FALSE 
      22            5      -0.6009571       -0.5379543    0.0630028  FALSE 
      23            3       0.3684434       -1.0487799   -1.4172233  FALSE 
      24           10      -0.7475606        0.1551929    0.9027535  FALSE 
      25           11      -0.1656391        0.2505031    0.4161421  FALSE 
      26           16      -0.6962673        0.6251965    1.3214638  FALSE 
      27            9      -0.3597951        0.0498324    0.4096274  FALSE 
      28            8       0.9231209       -0.0679507   -0.9910716  FALSE 
      29            9       0.4668835        0.0498324   -0.4170511  FALSE 
      30           12       0.1792014        0.3375144    0.1583130  FALSE 
      31            7       0.0921901       -0.2014821   -0.2936721  FALSE 
      32            5      -0.0031201       -0.5379543   -0.5348342  FALSE 

# Models

There are two important changes to this modelling exercise. First, I am testing the influence of new state level variables that had not been previosly considered, as well as see if using years_deciles provides a better variable that yearsquant. Second, I will model extortion via two part models, using zero-inflated negative binomial and hurdle models. As the multilevel ZINB can only calculate a fixed zero-inflation factor (i.e. it is not poissbile to specify a separate equation for the zero-inflation part), I will focus on the hurdle models instead, testing hypotheses using these models.

## Model data frame

To avoid any problems with NA values that may lead to non-convergence, work with a data set onf only the relevant variables.


```r
enve_final %>%
    dplyr::select(extortions,
           bribes,
           yearsquant,
           years_deciles,
           subsector,
           size,
           log_deaths,
           log_bribe_vic,
           log_drug_crim,
           log_wpn_crim,
           law_index,
           comp_index,
           log_pop,
           log_nbus,
           state = NOM_ABR) -> enve_model

#### JUST FOR THIS TESTING EXERCISE, ADD MORE VICTIMS TO THE OTHER AND MEDIA CATEGORY

##### Comment out before sending #####

## enve_model$extortions[which(enve_model$subsector == "Other")] <- rpois(10,1)


## enve_model$extortions[which(enve_model$subsector == "Media")] <- rpois(45, 0.5)

##### Comment out before sending #####

##### Return to normal


enve_model$extortion_victim <- ifelse(enve_model$extortions == 0, 0, 1)

summary(enve_model)
```

```
   extortions          bribes          yearsquant  years_deciles
 Min.   : 0.0000   Min.   : 0.0000   [0,8]  :539   [0,4]  :293  
 1st Qu.: 0.0000   1st Qu.: 0.0000   (8,16] :433   (25,30]:289  
 Median : 0.0000   Median : 0.0000   (16,25]:487   (16,21]:279  
 Mean   : 0.5688   Mean   : 0.2786   (25,34]:510   (4,8]  :246  
 3rd Qu.: 0.0000   3rd Qu.: 0.0000   (34,43]:443   (34,39]:241  
 Max.   :33.0000   Max.   :13.0000                 (30,34]:221  
                                                   (Other):843  
         subsector       size       log_deaths       log_bribe_vic    
 Manufacturing:484   Large :568   Min.   :-2.72796   Min.   :-1.0529  
 Health       :391   Medium:611   1st Qu.:-1.19062   1st Qu.:-0.3962  
 HotelsRestBar:367   Small :597   Median :-0.09826   Median :-0.1656  
 Construction :356   Micro :636   Mean   :-0.49289   Mean   :-0.1035  
 Maintenance  :206                3rd Qu.: 0.16603   3rd Qu.: 0.2783  
 Transport    :135                Max.   : 1.53441   Max.   : 0.9231  
 (Other)      :473                                                    
 log_drug_crim      log_wpn_crim       law_index        
 Min.   :-2.6561   Min.   :-2.8933   Min.   :-33.02134  
 1st Qu.:-1.5849   1st Qu.:-0.8466   1st Qu.: -4.29495  
 Median :-1.0359   Median :-0.2070   Median :  1.13880  
 Mean   :-0.6757   Mean   :-0.3723   Mean   : -0.00842  
 3rd Qu.: 0.1148   3rd Qu.: 0.4458   3rd Qu.:  7.76001  
 Max.   : 1.9593   Max.   : 1.0703   Max.   : 24.00707  
                                                        
   comp_index          log_pop           log_nbus            state     
 Min.   :-22.4096   Min.   :-1.6674   Min.   :-0.50020   HGO.   :  98  
 1st Qu.: -5.1176   1st Qu.:-0.7611   1st Qu.:-0.19996   GTO.   :  93  
 Median :  1.3520   Median :-0.2470   Median :-0.04958   JAL.   :  93  
 Mean   :  0.1609   Mean   :-0.2404   Mean   :-0.03879   DF     :  82  
 3rd Qu.:  5.0020   3rd Qu.: 0.2893   3rd Qu.: 0.08738   NL     :  82  
 Max.   : 20.1805   Max.   : 1.4868   Max.   : 0.63217   COAH.  :  80  
                                                         (Other):1884  
 extortion_victim
 Min.   :0.0000  
 1st Qu.:0.0000  
 Median :0.0000  
 Mean   :0.1182  
 3rd Qu.:0.0000  
 Max.   :1.0000  
                 
```

```r
summary(factor(enve_model$extortion_victim))
```

```
   0    1 
2127  285 
```

```r
summary(factor(enve_final$extortion_victim))
```

```
  no  yes   dk 
2079  285   48 
```

```r
length(enve_model[is.na(enve_model)])
```

```
[1] 0
```


## Negative binomial

I will not test every single model as before, as we have done this before. Thus I will start with the "best model" from previous exercises, changing the state homicide rate for the log deaths and log pop variables instead. To minimise any syntax mistakes, I will work with formula objects and update them as I proceed.



```r
model_failsafe <- function(formula, data, family, multilevel = TRUE, ...)
{
    model <- glmmADMB::glmmadmb(formula = formula,
                                data = data,
                                family = family,
                                zeroInflation = FALSE, 
                                ...,
                                admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                                extra.args = "-ndi 60000")
    
    tc <- tryCatch(summary(model), error = function(e) e)
    
    if(is(tc, "error"))
    {
        if(isTRUE(multilevel))
        {
            if(family != "nbinom")
            {
                model <- lme4::glmer(formula = formula,
                                     data = data,
                                     family = family)
            }
            if(family == "nbinom")
            {
                model <- lme4::glmer.nb(formula = formula,
                                     data = data)
            }
        }
        else
        {
          if(family != "nbinom")
            {
                model <- glm(formula = formula,
                                     data = data,
                                     family = family)
            }
            if(family == "nbinom")
            {
                model <- MASS::glm.nb(formula = formula,
                                     data = data)
            }  
        }
        
    }
    else {print("The model was fitted correctly with glmmadmb")}
    
    return(model)
}
```



```r
count_formula <- as.formula("extortions ~
                            bribes +
                            yearsquant +
                            subsector +
                            size +
                            (1 | state)")
```



```r
admbControl <- glmmADMB::admbControl

mnb1 <- model_failsafe(formula = count_formula, 
                       data = enve_model, 
                       family = "nbinom")
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb1)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3112 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.28e-01   5.68e-01   -0.23    0.821  
bribes                  -1.01e-02   1.02e-01   -0.10    0.921  
yearsquant(8,16]         2.40e-01   3.17e-01    0.76    0.450  
yearsquant(16,25]       -1.11e-01   2.97e-01   -0.37    0.709  
yearsquant(25,34]        4.40e-01   3.02e-01    1.46    0.145  
yearsquant(34,43]        1.90e-01   3.12e-01    0.61    0.541  
subsectorMining         -1.49e+00   1.02e+00   -1.45    0.147  
subsectorConstruction   -9.62e-03   5.65e-01   -0.02    0.986  
subsectorManufacturing  -2.75e-01   5.49e-01   -0.50    0.616  
subsectorWholesale      -6.37e-01   6.80e-01   -0.94    0.349  
subsectorTransport      -6.15e-01   6.43e-01   -0.96    0.338  
subsectorMedia           1.43e-01   8.67e-01    0.17    0.869  
subsectorFinance        -5.20e-02   9.15e-01   -0.06    0.955  
subsectorReal estate    -1.38e+00   9.18e-01   -1.51    0.132  
subsectorProf. services  1.08e+00   8.81e-01    1.23    0.220  
subsectorMaintenance    -6.66e-01   6.08e-01   -1.10    0.273  
subsectorEducation       5.23e-03   9.01e-01    0.01    0.995  
subsectorHealth         -5.60e-01   5.64e-01   -0.99    0.321  
subsectorLeisure         2.38e-01   8.71e-01    0.27    0.784  
subsectorHotelsRestBar  -4.49e-01   5.65e-01   -0.80    0.427  
subsectorOther          -2.58e+01   1.48e+05    0.00    1.000  
sizeMedium              -1.84e-01   2.77e-01   -0.66    0.507  
sizeSmall               -4.70e-01   2.78e-01   -1.69    0.091 .
sizeMicro               -6.05e-01   2.76e-01   -2.19    0.028 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.042e-08 0.0001429

Negative binomial dispersion parameter: 0.053227 (std. err.: 0.004073)

Log-likelihood: -1529.98 
```

```r
get_glmmadmb(mnb1)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -1529.98

$df
[1] 26

$AIC
[1] 3111.96

$BIC
[1] 3262.454

$alpha
[1] 18.78746

$var_j
[1] 4.169356e-16

$ICC
[1] 2.219223e-17
```

```r
# vs Poisson

mp1 <- model_failsafe(count_formula, 
                      data = enve_model, 
                      family = "poisson")
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mp1)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 5661.1 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.19e+00   3.82e-01   -5.73    1e-08 ***
bribes                  -1.62e-01   5.97e-02   -2.71  0.00670 ** 
yearsquant(8,16]         3.34e-01   1.24e-01    2.69  0.00710 ** 
yearsquant(16,25]       -3.08e-01   1.37e-01   -2.25  0.02460 *  
yearsquant(25,34]       -1.26e-01   1.17e-01   -1.08  0.28218    
yearsquant(34,43]       -5.29e-02   1.32e-01   -0.40  0.68946    
subsectorMining          1.03e+00   5.05e-01    2.04  0.04136 *  
subsectorConstruction    9.01e-01   3.66e-01    2.46  0.01382 *  
subsectorManufacturing   8.31e-01   3.70e-01    2.25  0.02461 *  
subsectorWholesale       8.73e-01   4.06e-01    2.15  0.03162 *  
subsectorTransport       2.37e-01   4.28e-01    0.55  0.57996    
subsectorMedia           9.18e-01   4.69e-01    1.96  0.05039 .  
subsectorFinance         1.37e+00   4.54e-01    3.02  0.00249 ** 
subsectorReal estate     9.20e-01   4.69e-01    1.96  0.05010 .  
subsectorProf. services  1.51e+00   4.38e-01    3.44  0.00058 ***
subsectorMaintenance     7.44e-01   3.92e-01    1.90  0.05760 .  
subsectorEducation       7.87e-01   4.90e-01    1.61  0.10818    
subsectorHealth          9.78e-01   3.69e-01    2.65  0.00809 ** 
subsectorLeisure         1.52e+00   4.30e-01    3.52  0.00042 ***
subsectorHotelsRestBar   9.38e-01   3.74e-01    2.51  0.01214 *  
subsectorOther          -1.94e+01   1.55e+04    0.00  0.99900    
sizeMedium              -2.98e-01   1.23e-01   -2.42  0.01573 *  
sizeSmall               -1.94e-01   1.15e-01   -1.68  0.09310 .  
sizeMicro                5.08e-03   1.14e-01    0.04  0.96431    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance StdDev
(Intercept)   0.1849   0.43


Log-likelihood: -2805.56 
```

```r
get_glmmadmb(mp1)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -2805.56

$df
[1] 25

$AIC
[1] 5661.12

$BIC
[1] 5805.825

$alpha
numeric(0)

$var_j
[1] 0.0341991

$ICC
numeric(0)
```

```r
screenreg(list(mnb1, mp1), single.row = TRUE,
          custom.model.names = c("negbin", "poisson"))
```

```

========================================================================
                         negbin                  poisson                
------------------------------------------------------------------------
(Intercept)                 -0.13      (0.57)       -2.19     (0.38) ***
bribes                      -0.01      (0.10)       -0.16     (0.06) ** 
yearsquant(8,16]             0.24      (0.32)        0.33     (0.12) ** 
yearsquant(16,25]           -0.11      (0.30)       -0.31     (0.14) *  
yearsquant(25,34]            0.44      (0.30)       -0.13     (0.12)    
yearsquant(34,43]            0.19      (0.31)       -0.05     (0.13)    
subsectorMining             -1.49      (1.02)        1.03     (0.51) *  
subsectorConstruction       -0.01      (0.57)        0.90     (0.37) *  
subsectorManufacturing      -0.28      (0.55)        0.83     (0.37) *  
subsectorWholesale          -0.64      (0.68)        0.87     (0.41) *  
subsectorTransport          -0.62      (0.64)        0.24     (0.43)    
subsectorMedia               0.14      (0.87)        0.92     (0.47)    
subsectorFinance            -0.05      (0.91)        1.37     (0.45) ** 
subsectorReal estate        -1.38      (0.92)        0.92     (0.47)    
subsectorProf. services      1.08      (0.88)        1.51     (0.44) ***
subsectorMaintenance        -0.67      (0.61)        0.74     (0.39)    
subsectorEducation           0.01      (0.90)        0.79     (0.49)    
subsectorHealth             -0.56      (0.56)        0.98     (0.37) ** 
subsectorLeisure             0.24      (0.87)        1.52     (0.43) ***
subsectorHotelsRestBar      -0.45      (0.57)        0.94     (0.37) *  
subsectorOther             -25.77 (148200.00)      -19.43 (15481.00)    
sizeMedium                  -0.18      (0.28)       -0.30     (0.12) *  
sizeSmall                   -0.47      (0.28)       -0.19     (0.12)    
sizeMicro                   -0.60      (0.28) *      0.01     (0.11)    
------------------------------------------------------------------------
Variance: state              0.00                    0.18               
Dispersion: parameter        0.05                                       
Dispersion: SD               0.00                                       
AIC                       3111.96                 5661.12               
BIC                       3262.45                 5805.83               
Log Likelihood           -1529.98                -2805.56               
Num. obs.                 2412                    2412                  
Num. groups: state          32                      32                  
========================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(mp1, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  25 -2805.6                         
2  26 -1530.0  1 2551.2  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# vs single level nb

nb1_formula <- update(count_formula, . ~ . - (1 | state))

nb1 <- model_failsafe(nb1_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = FALSE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(nb1)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3110 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.28e-01   5.68e-01   -0.23    0.821  
bribes                  -1.01e-02   1.02e-01   -0.10    0.921  
yearsquant(8,16]         2.40e-01   3.17e-01    0.76    0.450  
yearsquant(16,25]       -1.11e-01   2.97e-01   -0.37    0.709  
yearsquant(25,34]        4.40e-01   3.02e-01    1.46    0.145  
yearsquant(34,43]        1.90e-01   3.12e-01    0.61    0.541  
subsectorMining         -1.49e+00   1.02e+00   -1.45    0.147  
subsectorConstruction   -9.58e-03   5.65e-01   -0.02    0.986  
subsectorManufacturing  -2.75e-01   5.49e-01   -0.50    0.616  
subsectorWholesale      -6.37e-01   6.80e-01   -0.94    0.349  
subsectorTransport      -6.15e-01   6.43e-01   -0.96    0.338  
subsectorMedia           1.43e-01   8.67e-01    0.17    0.869  
subsectorFinance        -5.20e-02   9.15e-01   -0.06    0.955  
subsectorReal estate    -1.38e+00   9.18e-01   -1.51    0.132  
subsectorProf. services  1.08e+00   8.81e-01    1.23    0.220  
subsectorMaintenance    -6.66e-01   6.08e-01   -1.10    0.273  
subsectorEducation       5.18e-03   9.01e-01    0.01    0.995  
subsectorHealth         -5.60e-01   5.64e-01   -0.99    0.321  
subsectorLeisure         2.38e-01   8.71e-01    0.27    0.784  
subsectorHotelsRestBar  -4.49e-01   5.65e-01   -0.79    0.427  
subsectorOther          -1.79e+01   2.97e+03   -0.01    0.995  
sizeMedium              -1.84e-01   2.77e-01   -0.66    0.507  
sizeSmall               -4.70e-01   2.78e-01   -1.69    0.091 .
sizeMicro               -6.05e-01   2.76e-01   -2.19    0.028 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.053155 (std. err.: 0.0040665)

Log-likelihood: -1529.98 
```

```r
get_glmmadmb(nb1)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size

$logLik
[1] -1529.98

$df
[1] 25

$AIC
[1] 3109.96

$BIC
[1] 3254.665

$alpha
[1] 18.81291
```

```r
screenreg(list(mnb1, nb1), single.row = TRUE,
          custom.model.names = c("multilevel nb", "single level nb"))
```

```

=====================================================================
                         multilevel nb           single level nb     
---------------------------------------------------------------------
(Intercept)                 -0.13      (0.57)       -0.13    (0.57)  
bribes                      -0.01      (0.10)       -0.01    (0.10)  
yearsquant(8,16]             0.24      (0.32)        0.24    (0.32)  
yearsquant(16,25]           -0.11      (0.30)       -0.11    (0.30)  
yearsquant(25,34]            0.44      (0.30)        0.44    (0.30)  
yearsquant(34,43]            0.19      (0.31)        0.19    (0.31)  
subsectorMining             -1.49      (1.02)       -1.49    (1.02)  
subsectorConstruction       -0.01      (0.57)       -0.01    (0.57)  
subsectorManufacturing      -0.28      (0.55)       -0.28    (0.55)  
subsectorWholesale          -0.64      (0.68)       -0.64    (0.68)  
subsectorTransport          -0.62      (0.64)       -0.62    (0.64)  
subsectorMedia               0.14      (0.87)        0.14    (0.87)  
subsectorFinance            -0.05      (0.91)       -0.05    (0.91)  
subsectorReal estate        -1.38      (0.92)       -1.38    (0.92)  
subsectorProf. services      1.08      (0.88)        1.08    (0.88)  
subsectorMaintenance        -0.67      (0.61)       -0.67    (0.61)  
subsectorEducation           0.01      (0.90)        0.01    (0.90)  
subsectorHealth             -0.56      (0.56)       -0.56    (0.56)  
subsectorLeisure             0.24      (0.87)        0.24    (0.87)  
subsectorHotelsRestBar      -0.45      (0.57)       -0.45    (0.57)  
subsectorOther             -25.77 (148200.00)      -17.89 (2970.80)  
sizeMedium                  -0.18      (0.28)       -0.18    (0.28)  
sizeSmall                   -0.47      (0.28)       -0.47    (0.28)  
sizeMicro                   -0.60      (0.28) *     -0.60    (0.28) *
---------------------------------------------------------------------
Variance: state              0.00                                    
Dispersion: parameter        0.05                    0.05            
Dispersion: SD               0.00                    0.00            
AIC                       3111.96                 3109.96            
BIC                       3262.45                 3254.67            
Log Likelihood           -1529.98                -1529.98            
Num. obs.                 2412                    2412               
Num. groups: state          32                                       
Num. groups:                                         1               
=====================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(nb1, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df LogLik Df Chisq Pr(>Chisq)
1  25  -1530                    
2  26  -1530  1     0          1
```

```r
vif(mnb1)
```

```
                   GVIF Df GVIF^(1/(2*Df))
bribes     3.712574e+15  1        60930898
yearsquant 0.000000e+00  4               0
subsector  0.000000e+00 15               0
size       0.000000e+00  3               0
```

```r
lm1 <- lm(update(count_formula, . ~ . - (1 | state)), data = enve_model)

summary(lm1)
```

```

Call:
lm(formula = update(count_formula, . ~ . - (1 | state)), data = enve_model)

Residuals:
   Min     1Q Median     3Q    Max 
-2.371 -0.663 -0.494 -0.319 32.511 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)   
(Intercept)              0.849048   0.326908   2.597  0.00946 **
bribes                  -0.007186   0.051068  -0.141  0.88811   
yearsquant(8,16]         0.122672   0.170594   0.719  0.47216   
yearsquant(16,25]       -0.039837   0.165657  -0.240  0.80998   
yearsquant(25,34]        0.208588   0.163325   1.277  0.20168   
yearsquant(34,43]        0.112467   0.170116   0.661  0.50860   
subsectorMining         -0.528027   0.563789  -0.937  0.34908   
subsectorConstruction   -0.025700   0.323138  -0.080  0.93661   
subsectorManufacturing  -0.132244   0.315104  -0.420  0.67475   
subsectorWholesale      -0.343471   0.381742  -0.900  0.36835   
subsectorTransport      -0.275448   0.370012  -0.744  0.45669   
subsectorMedia           0.231868   0.490160   0.473  0.63622   
subsectorFinance         0.032637   0.527630   0.062  0.95068   
subsectorReal estate    -0.454870   0.509572  -0.893  0.37213   
subsectorProf. services  1.399327   0.517920   2.702  0.00694 **
subsectorMaintenance    -0.324296   0.344765  -0.941  0.34699   
subsectorEducation       0.017759   0.523218   0.034  0.97293   
subsectorHealth         -0.282666   0.320766  -0.881  0.37829   
subsectorLeisure         0.568707   0.509172   1.117  0.26414   
subsectorHotelsRestBar  -0.179296   0.322730  -0.556  0.57856   
subsectorOther          -0.652141   0.883782  -0.738  0.46065   
sizeMedium              -0.189721   0.154112  -1.231  0.21842   
sizeSmall               -0.287743   0.155168  -1.854  0.06381 . 
sizeMicro               -0.358420   0.153087  -2.341  0.01930 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.636 on 2388 degrees of freedom
Multiple R-squared:  0.01258,	Adjusted R-squared:  0.003069 
F-statistic: 1.323 on 23 and 2388 DF,  p-value: 0.1392
```

```r
vif(lm1)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.008956  1        1.004468
yearsquant 1.035164  4        1.004329
subsector  1.047281 15        1.001541
size       1.020606  3        1.003405
```

```r
mnb1_drop <- drop1(mnb1, test = "Chisq")
```

```
Warning in extractAIC.glmmadmb(object, scale, k = k, ...): ignored explicit
specification of scale
```

```
Error in glmmADMB::glmmadmb(formula = extortions ~ yearsquant + subsector + : must specify 'family' as a character string
```

```r
mnb1_drop
```

```
Error in eval(expr, envir, enclos): object 'mnb1_drop' not found
```

```r
# NB null, multi and one level

nb_null_formula <- update(count_formula, . ~ 1)

nb_null <- model_failsafe(nb_null_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = FALSE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(nb_null)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3089.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.049824 (std. err.: 0.0037658)

Log-likelihood: -1542.76 
```

```r
get_glmmadmb(nb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1542.76

$df
[1] 2

$AIC
[1] 3089.52

$BIC
[1] 3101.096

$alpha
[1] 20.07065
```

```r
lrtest(nb_null, nb1)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2  25 -1530.0 23 25.56     0.3221
```

```r
lrtest(nb_null, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2  26 -1530.0 24 25.56     0.3759
```

```r
mnb_null_formula <- update(count_formula, . ~ (1 | state))

mnb_null <- model_failsafe(mnb_null_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb_null)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3091.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.665e-07 0.0005162

Negative binomial dispersion parameter: 0.049891 (std. err.: 0.0037718)

Log-likelihood: -1542.76 
```

```r
get_glmmadmb(mnb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1542.76

$df
[1] 3

$AIC
[1] 3091.52

$BIC
[1] 3108.885

$alpha
[1] 20.0437

$var_j
[1] 7.100093e-14

$ICC
[1] 3.542307e-15
```

```r
lrtest(nb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2   3 -1542.8  1     0          1
```

```r
lrtest(mnb_null, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   3 -1542.8                    
2  26 -1530.0 23 25.56     0.3221
```


```r
mnb2_formula <- update(count_formula, . ~ . - yearsquant + years_deciles)

mnb2 <- model_failsafe(mnb2_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb2)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3112 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)              3.27e-01   6.32e-01    0.52    0.605  
bribes                  -4.57e-02   1.01e-01   -0.45    0.650  
subsectorMining         -2.11e+00   1.05e+00   -2.01    0.044 *
subsectorConstruction   -3.69e-01   5.79e-01   -0.64    0.524  
subsectorManufacturing  -6.49e-01   5.65e-01   -1.15    0.250  
subsectorWholesale      -1.07e+00   6.96e-01   -1.54    0.124  
subsectorTransport      -1.09e+00   6.61e-01   -1.65    0.099 .
subsectorMedia          -3.67e-01   8.96e-01   -0.41    0.682  
subsectorFinance        -4.74e-01   9.33e-01   -0.51    0.611  
subsectorReal estate    -1.94e+00   9.47e-01   -2.05    0.041 *
subsectorProf. services  6.50e-01   8.95e-01    0.73    0.468  
subsectorMaintenance    -1.10e+00   6.20e-01   -1.77    0.076 .
subsectorEducation      -7.30e-01   9.21e-01   -0.79    0.428  
subsectorHealth         -1.03e+00   5.87e-01   -1.76    0.079 .
subsectorLeisure        -4.48e-01   8.93e-01   -0.50    0.616  
subsectorHotelsRestBar  -8.49e-01   5.82e-01   -1.46    0.144  
subsectorOther          -2.31e+01   3.39e+04    0.00    0.999  
sizeMedium              -1.83e-01   2.80e-01   -0.65    0.513  
sizeSmall               -4.09e-01   2.82e-01   -1.45    0.147  
sizeMicro               -4.88e-01   2.82e-01   -1.73    0.084 .
years_deciles(4,8]      -2.07e-01   4.11e-01   -0.50    0.614  
years_deciles(8,12]      6.24e-02   4.28e-01    0.15    0.884  
years_deciles(12,16]     2.78e-01   4.44e-01    0.63    0.531  
years_deciles(16,21]    -2.70e-01   4.01e-01   -0.67    0.502  
years_deciles(21,25]    -1.61e-01   4.28e-01   -0.38    0.706  
years_deciles(25,30]     8.04e-01   4.03e-01    2.00    0.046 *
years_deciles(30,34]    -6.31e-01   4.41e-01   -1.43    0.152  
years_deciles(34,39]     6.38e-02   4.16e-01    0.15    0.878  
years_deciles(39,43]     2.18e-01   4.39e-01    0.50    0.620  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 4.677e-09 6.839e-05

Negative binomial dispersion parameter: 0.054598 (std. err.: 0.0041964)

Log-likelihood: -1525 
```

```r
get_glmmadmb(mnb2)
```

```
$model
extortions ~ bribes + subsector + size + (1 | state) + years_deciles

$logLik
[1] -1525

$df
[1] 31

$AIC
[1] 3112

$BIC
[1] 3291.435

$alpha
[1] 18.31569

$var_j
[1] 2.187152e-17

$ICC
[1] 1.194141e-18
```

```r
screenreg(list(mnb1, mnb2), single.row = TRUE,
          custom.model.names = c("year qunitiles", "year deciles"))
```

```

======================================================================
                         year qunitiles          year deciles         
----------------------------------------------------------------------
(Intercept)                 -0.13      (0.57)        0.33     (0.63)  
bribes                      -0.01      (0.10)       -0.05     (0.10)  
yearsquant(8,16]             0.24      (0.32)                         
yearsquant(16,25]           -0.11      (0.30)                         
yearsquant(25,34]            0.44      (0.30)                         
yearsquant(34,43]            0.19      (0.31)                         
subsectorMining             -1.49      (1.02)       -2.11     (1.05) *
subsectorConstruction       -0.01      (0.57)       -0.37     (0.58)  
subsectorManufacturing      -0.28      (0.55)       -0.65     (0.57)  
subsectorWholesale          -0.64      (0.68)       -1.07     (0.70)  
subsectorTransport          -0.62      (0.64)       -1.09     (0.66)  
subsectorMedia               0.14      (0.87)       -0.37     (0.90)  
subsectorFinance            -0.05      (0.91)       -0.47     (0.93)  
subsectorReal estate        -1.38      (0.92)       -1.94     (0.95) *
subsectorProf. services      1.08      (0.88)        0.65     (0.90)  
subsectorMaintenance        -0.67      (0.61)       -1.10     (0.62)  
subsectorEducation           0.01      (0.90)       -0.73     (0.92)  
subsectorHealth             -0.56      (0.56)       -1.03     (0.59)  
subsectorLeisure             0.24      (0.87)       -0.45     (0.89)  
subsectorHotelsRestBar      -0.45      (0.57)       -0.85     (0.58)  
subsectorOther             -25.77 (148200.00)      -23.08 (33858.00)  
sizeMedium                  -0.18      (0.28)       -0.18     (0.28)  
sizeSmall                   -0.47      (0.28)       -0.41     (0.28)  
sizeMicro                   -0.60      (0.28) *     -0.49     (0.28)  
years_deciles(4,8]                                  -0.21     (0.41)  
years_deciles(8,12]                                  0.06     (0.43)  
years_deciles(12,16]                                 0.28     (0.44)  
years_deciles(16,21]                                -0.27     (0.40)  
years_deciles(21,25]                                -0.16     (0.43)  
years_deciles(25,30]                                 0.80     (0.40) *
years_deciles(30,34]                                -0.63     (0.44)  
years_deciles(34,39]                                 0.06     (0.42)  
years_deciles(39,43]                                 0.22     (0.44)  
----------------------------------------------------------------------
Variance: state              0.00                    0.00             
Dispersion: parameter        0.05                    0.05             
Dispersion: SD               0.00                    0.00             
AIC                       3111.96                 3112.00             
BIC                       3262.45                 3291.43             
Log Likelihood           -1529.98                -1525.00             
Num. obs.                 2412                    2412                
Num. groups: state          32                      32                
======================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

Now start introducing the state-level variables. First, log deaths, then the saturated model, then test them. Also run excluding all business-level and compare.


```r
mnb3_formula <- update(count_formula, . ~ . + log_deaths + log_pop)

mnb3 <-  model_failsafe(mnb3_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb3)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3113.8 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.18e-01   5.64e-01   -0.21    0.835  
bribes                  -1.47e-03   1.04e-01   -0.01    0.989  
yearsquant(8,16]         2.08e-01   3.18e-01    0.65    0.513  
yearsquant(16,25]       -1.06e-01   2.99e-01   -0.36    0.723  
yearsquant(25,34]        4.16e-01   3.04e-01    1.37    0.171  
yearsquant(34,43]        1.42e-01   3.13e-01    0.45    0.652  
subsectorMining         -1.31e+00   1.03e+00   -1.27    0.204  
subsectorConstruction    5.83e-02   5.65e-01    0.10    0.918  
subsectorManufacturing  -2.36e-01   5.47e-01   -0.43    0.666  
subsectorWholesale      -4.76e-01   6.88e-01   -0.69    0.490  
subsectorTransport      -6.18e-01   6.43e-01   -0.96    0.337  
subsectorMedia           1.33e-01   8.65e-01    0.15    0.878  
subsectorFinance        -1.09e-03   9.10e-01    0.00    0.999  
subsectorReal estate    -1.19e+00   9.26e-01   -1.29    0.199  
subsectorProf. services  1.18e+00   8.81e-01    1.34    0.180  
subsectorMaintenance    -5.56e-01   6.13e-01   -0.91    0.364  
subsectorEducation       1.83e-01   9.12e-01    0.20    0.841  
subsectorHealth         -4.87e-01   5.64e-01   -0.86    0.388  
subsectorLeisure         4.52e-01   8.83e-01    0.51    0.609  
subsectorHotelsRestBar  -4.04e-01   5.64e-01   -0.72    0.474  
subsectorOther          -4.16e+01   5.52e+05    0.00    1.000  
sizeMedium              -1.83e-01   2.78e-01   -0.66    0.509  
sizeSmall               -4.64e-01   2.78e-01   -1.67    0.096 .
sizeMicro               -6.35e-01   2.77e-01   -2.30    0.022 *
log_deaths               1.06e-01   1.33e-01    0.80    0.423  
log_pop                  5.23e-02   2.04e-01    0.26    0.798  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.073e-09 4.553e-05

Negative binomial dispersion parameter: 0.053547 (std. err.: 0.0041035)

Log-likelihood: -1528.91 
```

```r
get_glmmadmb(mnb3)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop

$logLik
[1] -1528.91

$df
[1] 28

$AIC
[1] 3113.82

$BIC
[1] 3275.89

$alpha
[1] 18.67518

$var_j
[1] 4.298988e-18

$ICC
[1] 2.301979e-19
```

```r
lrtest(mnb1, mnb3)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  28 -1528.9  2  2.14      0.343
```

```r
vif(mnb3)
```

```
                    GVIF Df GVIF^(1/(2*Df))
bribes     -2.702160e+15  1             NaN
yearsquant  0.000000e+00  4               0
subsector   0.000000e+00 15               0
size        0.000000e+00  3               0
log_deaths -3.459264e+01  1             NaN
log_pop    -3.521769e+01  1             NaN
```

```r
lm2 <- update(lm1, . ~ . + log_deaths + log_pop)

vif(lm2)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.009134  1        1.004556
yearsquant 1.038465  4        1.004729
subsector  1.061046 15        1.001977
size       1.022852  3        1.003773
log_deaths 2.391883  1        1.546571
log_pop    2.390019  1        1.545969
```

```r
# drop pop

mnb4_formula <- update(count_formula, . ~ . + log_deaths)

mnb4 <- model_failsafe(mnb4_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb4)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3111.9 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.13e-01   5.64e-01   -0.20    0.841  
bribes                   7.03e-04   1.04e-01    0.01    0.995  
yearsquant(8,16]         2.12e-01   3.17e-01    0.67    0.503  
yearsquant(16,25]       -9.67e-02   2.97e-01   -0.33    0.745  
yearsquant(25,34]        4.12e-01   3.03e-01    1.36    0.175  
yearsquant(34,43]        1.42e-01   3.13e-01    0.45    0.650  
subsectorMining         -1.30e+00   1.03e+00   -1.27    0.205  
subsectorConstruction    5.38e-02   5.65e-01    0.10    0.924  
subsectorManufacturing  -2.43e-01   5.46e-01   -0.45    0.656  
subsectorWholesale      -4.75e-01   6.89e-01   -0.69    0.491  
subsectorTransport      -6.30e-01   6.41e-01   -0.98    0.326  
subsectorMedia           1.47e-01   8.63e-01    0.17    0.864  
subsectorFinance        -7.96e-04   9.10e-01    0.00    0.999  
subsectorReal estate    -1.18e+00   9.26e-01   -1.28    0.201  
subsectorProf. services  1.17e+00   8.79e-01    1.33    0.184  
subsectorMaintenance    -5.67e-01   6.12e-01   -0.93    0.354  
subsectorEducation       1.60e-01   9.07e-01    0.18    0.860  
subsectorHealth         -4.91e-01   5.64e-01   -0.87    0.384  
subsectorLeisure         4.42e-01   8.82e-01    0.50    0.617  
subsectorHotelsRestBar  -4.07e-01   5.64e-01   -0.72    0.470  
subsectorOther          -3.58e+01   3.61e+05    0.00    1.000  
sizeMedium              -1.91e-01   2.76e-01   -0.69    0.488  
sizeSmall               -4.64e-01   2.78e-01   -1.67    0.095 .
sizeMicro               -6.33e-01   2.76e-01   -2.29    0.022 *
log_deaths               1.31e-01   9.02e-02    1.46    0.146  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 3.169e-09 5.629e-05

Negative binomial dispersion parameter: 0.053544 (std. err.: 0.0041036)

Log-likelihood: -1528.94 
```

```r
get_glmmadmb(mnb4)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths

$logLik
[1] -1528.94

$df
[1] 27

$AIC
[1] 3111.88

$BIC
[1] 3268.162

$alpha
[1] 18.67623

$var_j
[1] 1.004003e-17

$ICC
[1] 5.375832e-19
```

```r
lrtest(mnb1, mnb4, mnb3)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  27 -1528.9  1  2.08     0.1492
3  28 -1528.9  1  0.06     0.8065
```

```r
vif(mnb4)
```

```
                    GVIF Df GVIF^(1/(2*Df))
bribes     -3.684763e+16  1             NaN
yearsquant  0.000000e+00  4    0.000000e+00
subsector   0.000000e+00 15    0.000000e+00
size        1.204290e-07  3    7.027300e-02
log_deaths  3.715894e-24  1    1.927665e-12
```

```r
lm3 <- update(lm2, . ~ . - log_pop)

vif(lm3)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.008962  1        1.004471
yearsquant 1.036456  4        1.004486
subsector  1.055233 15        1.001794
size       1.021152  3        1.003495
log_deaths 1.009375  1        1.004677
```

```r
# drop deaths

mnb5_formula <- update(count_formula, . ~ . + log_pop)

mnb5 <- model_failsafe(mnb5_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb5)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3112.5 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.35e-01   5.62e-01   -0.24    0.811  
bribes                  -1.09e-02   1.03e-01   -0.11    0.916  
yearsquant(8,16]         2.08e-01   3.18e-01    0.65    0.513  
yearsquant(16,25]       -1.33e-01   2.97e-01   -0.45    0.654  
yearsquant(25,34]        4.38e-01   3.02e-01    1.45    0.147  
yearsquant(34,43]        1.59e-01   3.13e-01    0.51    0.612  
subsectorMining         -1.38e+00   1.03e+00   -1.34    0.180  
subsectorConstruction    4.64e-02   5.66e-01    0.08    0.935  
subsectorManufacturing  -2.30e-01   5.47e-01   -0.42    0.675  
subsectorWholesale      -5.38e-01   6.82e-01   -0.79    0.430  
subsectorTransport      -5.83e-01   6.42e-01   -0.91    0.364  
subsectorMedia           9.98e-02   8.66e-01    0.12    0.908  
subsectorFinance        -2.10e-02   9.10e-01   -0.02    0.982  
subsectorReal estate    -1.28e+00   9.20e-01   -1.39    0.164  
subsectorProf. services  1.18e+00   8.82e-01    1.34    0.180  
subsectorMaintenance    -5.65e-01   6.13e-01   -0.92    0.357  
subsectorEducation       1.79e-01   9.14e-01    0.20    0.845  
subsectorHealth         -5.03e-01   5.64e-01   -0.89    0.372  
subsectorLeisure         3.96e-01   8.80e-01    0.45    0.653  
subsectorHotelsRestBar  -4.10e-01   5.64e-01   -0.73    0.467  
subsectorOther          -2.48e+01   1.07e+05    0.00    1.000  
sizeMedium              -1.62e-01   2.77e-01   -0.58    0.559  
sizeSmall               -4.65e-01   2.78e-01   -1.67    0.095 .
sizeMicro               -6.30e-01   2.77e-01   -2.28    0.023 *
log_pop                  1.71e-01   1.40e-01    1.23    0.221  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance  StdDev
(Intercept) 2.562e-08 0.00016

Negative binomial dispersion parameter: 0.053435 (std. err.: 0.0040919)

Log-likelihood: -1529.23 
```

```r
get_glmmadmb(mnb5)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop

$logLik
[1] -1529.23

$df
[1] 27

$AIC
[1] 3112.46

$BIC
[1] 3268.742

$alpha
[1] 18.71433

$var_j
[1] 6.561795e-16

$ICC
[1] 3.506295e-17
```

```r
lrtest(mnb1, mnb5, mnb3)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  27 -1529.2  1  1.50     0.2207
3  28 -1528.9  1  0.64     0.4237
```

```r
vif(mnb5)
```

```
                   GVIF Df GVIF^(1/(2*Df))
bribes     1.366215e+17  1    3.696235e+08
yearsquant 0.000000e+00  4    0.000000e+00
subsector  3.337094e+15 15    3.291892e+00
size       0.000000e+00  3    0.000000e+00
log_pop    0.000000e+00  1    0.000000e+00
```

```r
lm4 <- update(lm2, . ~ . - log_deaths)

vif(lm4)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.009064  1        1.004522
yearsquant 1.036222  4        1.004458
subsector  1.053635 15        1.001743
size       1.022115  3        1.003652
log_pop    1.008589  1        1.004285
```

```r
####
screenreg(list(negbin = mnb1,
               "deaths + pop" = mnb3,
               "deaths" = mnb4,
               "pop" = mnb5),
          single.row = TRUE)
```

```

=======================================================================================================================
                         negbin                  deaths + pop            deaths                  pop                   
-----------------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.13      (0.57)       -0.12      (0.56)       -0.11      (0.56)       -0.13      (0.56)  
bribes                      -0.01      (0.10)       -0.00      (0.10)        0.00      (0.10)       -0.01      (0.10)  
yearsquant(8,16]             0.24      (0.32)        0.21      (0.32)        0.21      (0.32)        0.21      (0.32)  
yearsquant(16,25]           -0.11      (0.30)       -0.11      (0.30)       -0.10      (0.30)       -0.13      (0.30)  
yearsquant(25,34]            0.44      (0.30)        0.42      (0.30)        0.41      (0.30)        0.44      (0.30)  
yearsquant(34,43]            0.19      (0.31)        0.14      (0.31)        0.14      (0.31)        0.16      (0.31)  
subsectorMining             -1.49      (1.02)       -1.31      (1.03)       -1.30      (1.03)       -1.38      (1.03)  
subsectorConstruction       -0.01      (0.57)        0.06      (0.56)        0.05      (0.56)        0.05      (0.57)  
subsectorManufacturing      -0.28      (0.55)       -0.24      (0.55)       -0.24      (0.55)       -0.23      (0.55)  
subsectorWholesale          -0.64      (0.68)       -0.48      (0.69)       -0.47      (0.69)       -0.54      (0.68)  
subsectorTransport          -0.62      (0.64)       -0.62      (0.64)       -0.63      (0.64)       -0.58      (0.64)  
subsectorMedia               0.14      (0.87)        0.13      (0.86)        0.15      (0.86)        0.10      (0.87)  
subsectorFinance            -0.05      (0.91)       -0.00      (0.91)       -0.00      (0.91)       -0.02      (0.91)  
subsectorReal estate        -1.38      (0.92)       -1.19      (0.93)       -1.18      (0.93)       -1.28      (0.92)  
subsectorProf. services      1.08      (0.88)        1.18      (0.88)        1.17      (0.88)        1.18      (0.88)  
subsectorMaintenance        -0.67      (0.61)       -0.56      (0.61)       -0.57      (0.61)       -0.56      (0.61)  
subsectorEducation           0.01      (0.90)        0.18      (0.91)        0.16      (0.91)        0.18      (0.91)  
subsectorHealth             -0.56      (0.56)       -0.49      (0.56)       -0.49      (0.56)       -0.50      (0.56)  
subsectorLeisure             0.24      (0.87)        0.45      (0.88)        0.44      (0.88)        0.40      (0.88)  
subsectorHotelsRestBar      -0.45      (0.57)       -0.40      (0.56)       -0.41      (0.56)       -0.41      (0.56)  
subsectorOther             -25.77 (148200.00)      -41.63 (552320.00)      -35.76 (361150.00)      -24.85 (106660.00)  
sizeMedium                  -0.18      (0.28)       -0.18      (0.28)       -0.19      (0.28)       -0.16      (0.28)  
sizeSmall                   -0.47      (0.28)       -0.46      (0.28)       -0.46      (0.28)       -0.46      (0.28)  
sizeMicro                   -0.60      (0.28) *     -0.63      (0.28) *     -0.63      (0.28) *     -0.63      (0.28) *
log_deaths                                           0.11      (0.13)        0.13      (0.09)                          
log_pop                                              0.05      (0.20)                                0.17      (0.14)  
-----------------------------------------------------------------------------------------------------------------------
Variance: state              0.00                    0.00                    0.00                    0.00              
Dispersion: parameter        0.05                    0.05                    0.05                    0.05              
Dispersion: SD               0.00                    0.00                    0.00                    0.00              
AIC                       3111.96                 3113.82                 3111.88                 3112.46              
BIC                       3262.45                 3275.89                 3268.16                 3268.74              
Log Likelihood           -1529.98                -1528.91                -1528.94                -1529.23              
Num. obs.                 2412                    2412                    2412                    2412                 
Num. groups: state          32                      32                      32                      32                 
=======================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```


```r
mnb6_formula <- update(count_formula, . ~ . + log_bribe_vic + log_nbus)

mnb6 <- model_failsafe(mnb6_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb6)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3115.9 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.15e-01   5.70e-01   -0.20    0.840  
bribes                  -1.07e-02   1.02e-01   -0.10    0.917  
yearsquant(8,16]         2.41e-01   3.18e-01    0.76    0.449  
yearsquant(16,25]       -1.17e-01   2.99e-01   -0.39    0.696  
yearsquant(25,34]        4.46e-01   3.04e-01    1.47    0.141  
yearsquant(34,43]        1.95e-01   3.12e-01    0.63    0.531  
subsectorMining         -1.52e+00   1.03e+00   -1.47    0.141  
subsectorConstruction   -3.15e-02   5.70e-01   -0.06    0.956  
subsectorManufacturing  -2.94e-01   5.52e-01   -0.53    0.595  
subsectorWholesale      -6.44e-01   6.81e-01   -0.95    0.344  
subsectorTransport      -6.04e-01   6.43e-01   -0.94    0.348  
subsectorMedia           1.22e-01   8.71e-01    0.14    0.888  
subsectorFinance        -3.94e-02   9.19e-01   -0.04    0.966  
subsectorReal estate    -1.40e+00   9.21e-01   -1.52    0.129  
subsectorProf. services  1.04e+00   8.90e-01    1.17    0.242  
subsectorMaintenance    -6.89e-01   6.12e-01   -1.13    0.260  
subsectorEducation       1.23e-02   9.01e-01    0.01    0.989  
subsectorHealth         -5.63e-01   5.65e-01   -1.00    0.319  
subsectorLeisure         2.32e-01   8.71e-01    0.27    0.790  
subsectorHotelsRestBar  -4.72e-01   5.70e-01   -0.83    0.408  
subsectorOther          -2.28e+01   3.42e+04    0.00    0.999  
sizeMedium              -1.84e-01   2.77e-01   -0.66    0.508  
sizeSmall               -4.66e-01   2.79e-01   -1.67    0.094 .
sizeMicro               -6.03e-01   2.77e-01   -2.17    0.030 *
log_bribe_vic           -3.42e-02   2.53e-01   -0.13    0.893  
log_nbus                 1.37e-01   4.40e-01    0.31    0.755  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance    StdDev
(Intercept)  2.5e-08 0.0001581

Negative binomial dispersion parameter: 0.053238 (std. err.: 0.0040741)

Log-likelihood: -1529.93 
```

```r
get_glmmadmb(mnb6)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus

$logLik
[1] -1529.93

$df
[1] 28

$AIC
[1] 3115.86

$BIC
[1] 3277.93

$alpha
[1] 18.78358

$var_j
[1] 6.2505e-16

$ICC
[1] 3.327641e-17
```

```r
vif(mnb6)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes        -6.813207e+14  1             NaN
yearsquant     0.000000e+00  4    0.000000e+00
subsector      4.705046e-05 15    7.173847e-01
size          -1.038084e-28  3             NaN
log_bribe_vic  2.676511e-10  1    1.636004e-05
log_nbus       2.673560e-09  1    5.170648e-05
```

```r
lm5 <- update(lm1, . ~ . + log_bribe_vic + log_nbus)
vif(lm5)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009044  1        1.004512
yearsquant    1.037419  4        1.004603
subsector     1.060412 15        1.001957
size          1.022274  3        1.003678
log_bribe_vic 1.183825  1        1.088037
log_nbus      1.184427  1        1.088314
```

```r
# drop nbus

mnb7_formula <- update(count_formula, . ~ . + log_bribe_vic)

mnb7 <- model_failsafe(mnb7_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb7)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3114 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.28e-01   5.69e-01   -0.23    0.821  
bribes                  -1.02e-02   1.02e-01   -0.10    0.921  
yearsquant(8,16]         2.40e-01   3.18e-01    0.76    0.450  
yearsquant(16,25]       -1.10e-01   2.99e-01   -0.37    0.712  
yearsquant(25,34]        4.40e-01   3.03e-01    1.45    0.147  
yearsquant(34,43]        1.91e-01   3.12e-01    0.61    0.541  
subsectorMining         -1.49e+00   1.03e+00   -1.45    0.148  
subsectorConstruction   -9.95e-03   5.66e-01   -0.02    0.986  
subsectorManufacturing  -2.76e-01   5.49e-01   -0.50    0.616  
subsectorWholesale      -6.37e-01   6.80e-01   -0.94    0.349  
subsectorTransport      -6.15e-01   6.43e-01   -0.96    0.338  
subsectorMedia           1.43e-01   8.68e-01    0.16    0.869  
subsectorFinance        -5.28e-02   9.18e-01   -0.06    0.954  
subsectorReal estate    -1.38e+00   9.20e-01   -1.50    0.133  
subsectorProf. services  1.08e+00   8.81e-01    1.23    0.220  
subsectorMaintenance    -6.66e-01   6.08e-01   -1.10    0.273  
subsectorEducation       5.18e-03   9.01e-01    0.01    0.995  
subsectorHealth         -5.60e-01   5.65e-01   -0.99    0.321  
subsectorLeisure         2.38e-01   8.71e-01    0.27    0.784  
subsectorHotelsRestBar  -4.49e-01   5.65e-01   -0.79    0.427  
subsectorOther          -2.61e+01   1.63e+05    0.00    1.000  
sizeMedium              -1.84e-01   2.77e-01   -0.66    0.507  
sizeSmall               -4.70e-01   2.78e-01   -1.69    0.091 .
sizeMicro               -6.05e-01   2.77e-01   -2.18    0.029 *
log_bribe_vic           -2.79e-03   2.32e-01   -0.01    0.990  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.263e-08 0.0001124

Negative binomial dispersion parameter: 0.053227 (std. err.: 0.0040731)

Log-likelihood: -1529.98 
```

```r
get_glmmadmb(mnb7)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic

$logLik
[1] -1529.98

$df
[1] 27

$AIC
[1] 3113.96

$BIC
[1] 3270.242

$alpha
[1] 18.78746

$var_j
[1] 1.595927e-16

$ICC
[1] 8.49464e-18
```

```r
vif(mnb7)
```

```
                      GVIF Df GVIF^(1/(2*Df))
bribes        1.570092e+15  1    3.962439e+07
yearsquant    2.812945e+13  4    4.798938e+01
subsector     0.000000e+00 15    0.000000e+00
size          0.000000e+00  3    0.000000e+00
log_bribe_vic 0.000000e+00  1    0.000000e+00
```

```r
lm6 <- update(lm5, . ~ . - log_nbus)
vif(lm5)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009044  1        1.004512
yearsquant    1.037419  4        1.004603
subsector     1.060412 15        1.001957
size          1.022274  3        1.003678
log_bribe_vic 1.183825  1        1.088037
log_nbus      1.184427  1        1.088314
```

```r
lrtest(mnb1, mnb7, mnb6)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  27 -1530.0  1   0.0     1.0000
3  28 -1529.9  1   0.1     0.7518
```

```r
# test vs business-level bribes

mnb8_formula <- update(count_formula, . ~ . + log_bribe_vic + log_nbus - bribes)

mnb8 <- model_failsafe(mnb8_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb8)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3113.9 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.13e-01   5.70e-01   -0.20    0.842  
yearsquant(8,16]         2.43e-01   3.17e-01    0.77    0.444  
yearsquant(16,25]       -1.16e-01   2.99e-01   -0.39    0.698  
yearsquant(25,34]        4.43e-01   3.02e-01    1.47    0.142  
yearsquant(34,43]        1.93e-01   3.11e-01    0.62    0.536  
subsectorMining         -1.52e+00   1.03e+00   -1.47    0.141  
subsectorConstruction   -3.69e-02   5.68e-01   -0.06    0.948  
subsectorManufacturing  -2.98e-01   5.51e-01   -0.54    0.589  
subsectorWholesale      -6.48e-01   6.80e-01   -0.95    0.341  
subsectorTransport      -6.05e-01   6.43e-01   -0.94    0.347  
subsectorMedia           1.20e-01   8.70e-01    0.14    0.891  
subsectorFinance        -4.31e-02   9.19e-01   -0.05    0.963  
subsectorReal estate    -1.40e+00   9.21e-01   -1.52    0.128  
subsectorProf. services  1.04e+00   8.89e-01    1.17    0.243  
subsectorMaintenance    -6.98e-01   6.06e-01   -1.15    0.249  
subsectorEducation       7.50e-03   9.00e-01    0.01    0.993  
subsectorHealth         -5.66e-01   5.64e-01   -1.00    0.316  
subsectorLeisure         2.33e-01   8.71e-01    0.27    0.789  
subsectorHotelsRestBar  -4.73e-01   5.70e-01   -0.83    0.407  
subsectorOther          -2.51e+01   1.04e+05    0.00    1.000  
sizeMedium              -1.83e-01   2.77e-01   -0.66    0.509  
sizeSmall               -4.64e-01   2.78e-01   -1.67    0.095 .
sizeMicro               -6.07e-01   2.75e-01   -2.21    0.027 *
log_bribe_vic           -3.31e-02   2.53e-01   -0.13    0.896  
log_nbus                 1.36e-01   4.40e-01    0.31    0.757  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.577e-08 0.0001256

Negative binomial dispersion parameter: 0.053237 (std. err.: 0.004074)

Log-likelihood: -1529.94 
```

```r
get_glmmadmb(mnb8)
```

```
$model
extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic + 
    log_nbus

$logLik
[1] -1529.94

$df
[1] 27

$AIC
[1] 3113.88

$BIC
[1] 3270.162

$alpha
[1] 18.78393

$var_j
[1] 2.48756e-16

$ICC
[1] 1.324302e-17
```

```r
vif(mnb8)
```

```
                       GVIF Df GVIF^(1/(2*Df))
yearsquant     0.000000e+00  4    0.000000e+00
subsector      0.000000e+00 15    0.000000e+00
size           1.568323e-26  3    5.003101e-05
log_bribe_vic  3.514034e-08  1    1.874576e-04
log_nbus      -4.795523e-08  1             NaN
```

```r
lrtest(mnb8, mnb6)
```

```
Likelihood ratio test

Model 1: extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic + 
    log_nbus
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  27 -1529.9                    
2  28 -1529.9  1  0.02     0.8875
```

```r
mnb9_formula <- update(mnb7_formula, . ~ . - bribes)

mnb9 <- model_failsafe(mnb9_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb9)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3112 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.27e-01   5.68e-01   -0.22    0.823  
yearsquant(8,16]         2.42e-01   3.17e-01    0.76    0.446  
yearsquant(16,25]       -1.10e-01   2.99e-01   -0.37    0.714  
yearsquant(25,34]        4.37e-01   3.01e-01    1.45    0.147  
yearsquant(34,43]        1.88e-01   3.11e-01    0.60    0.545  
subsectorMining         -1.49e+00   1.03e+00   -1.45    0.148  
subsectorConstruction   -1.50e-02   5.64e-01   -0.03    0.979  
subsectorManufacturing  -2.80e-01   5.48e-01   -0.51    0.609  
subsectorWholesale      -6.40e-01   6.79e-01   -0.94    0.346  
subsectorTransport      -6.16e-01   6.43e-01   -0.96    0.337  
subsectorMedia           1.40e-01   8.67e-01    0.16    0.872  
subsectorFinance        -5.63e-02   9.17e-01   -0.06    0.951  
subsectorReal estate    -1.38e+00   9.20e-01   -1.50    0.133  
subsectorProf. services  1.08e+00   8.80e-01    1.22    0.221  
subsectorMaintenance    -6.75e-01   6.01e-01   -1.12    0.262  
subsectorEducation       8.28e-04   9.00e-01    0.00    0.999  
subsectorHealth         -5.63e-01   5.64e-01   -1.00    0.318  
subsectorLeisure         2.39e-01   8.71e-01    0.27    0.784  
subsectorHotelsRestBar  -4.50e-01   5.65e-01   -0.80    0.426  
subsectorOther          -2.62e+01   1.55e+05    0.00    1.000  
sizeMedium              -1.83e-01   2.77e-01   -0.66    0.509  
sizeSmall               -4.68e-01   2.78e-01   -1.69    0.092 .
sizeMicro               -6.09e-01   2.75e-01   -2.22    0.027 *
log_bribe_vic           -1.90e-03   2.32e-01   -0.01    0.993  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.321e-08 0.0001149

Negative binomial dispersion parameter: 0.053226 (std. err.: 0.004073)

Log-likelihood: -1529.99 
```

```r
get_glmmadmb(mnb9)
```

```
$model
extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic

$logLik
[1] -1529.99

$df
[1] 26

$AIC
[1] 3111.98

$BIC
[1] 3262.474

$alpha
[1] 18.78781

$var_j
[1] 1.745834e-16

$ICC
[1] 9.292374e-18
```

```r
vif(mnb9)
```

```
                       GVIF Df GVIF^(1/(2*Df))
yearsquant     0.000000e+00  4    0.000000e+00
subsector     -1.406260e+06 15             NaN
size           1.646838e-25  3    7.403581e-05
log_bribe_vic -6.871433e-09  1             NaN
```

```r
mnb10_formula <- update(count_formula, . ~ . - bribes)

mnb10 <- model_failsafe(mnb10_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb10)
```

```
Error in summary(mnb10): object 'mnb10' not found
```

```r
get_glmmadmb(mnb10)
```

```
Error in get_glmmadmb(mnb10): object 'mnb10' not found
```

```r
vif(mnb10)
```

```
Error in vif(mnb10): object 'mnb10' not found
```

```r
lrtest(mnb10, mnb1, mnb7, mnb6)
```

```
Error in lrtest(mnb10, mnb1, mnb7, mnb6): object 'mnb10' not found
```

```r
screenreg(list("no bribes" = mnb10,
               "bus bribes" = mnb1,
               "b + s bribes" = mnb7,
               "bs_bribes + nbus" = mnb6),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb10' not found
```

```r
lrtest(mnb10, mnb9, mnb8, mnb6)
```

```
Error in lrtest(mnb10, mnb9, mnb8, mnb6): object 'mnb10' not found
```

```r
screenreg(list("no bribes" = mnb10,
               "state bribes" = mnb9,
               "s bribes + nbus" = mnb8,
               "bs_bribes + nbus" = mnb6),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb10' not found
```

```r
## Include with log_deaths and pop

mnb11_formula <- update(mnb3_formula, . ~ . + log_bribe_vic + log_nbus)

mnb11 <- model_failsafe(mnb11_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb11)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3117.1 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -9.87e-02   5.65e-01   -0.17    0.861  
bribes                  -3.94e-03   1.03e-01   -0.04    0.970  
yearsquant(8,16]         2.09e-01   3.18e-01    0.66    0.512  
yearsquant(16,25]       -1.15e-01   3.01e-01   -0.38    0.703  
yearsquant(25,34]        4.17e-01   3.05e-01    1.37    0.172  
yearsquant(34,43]        1.46e-01   3.13e-01    0.47    0.642  
subsectorMining         -1.40e+00   1.03e+00   -1.35    0.176  
subsectorConstruction    8.65e-03   5.67e-01    0.02    0.988  
subsectorManufacturing  -2.74e-01   5.49e-01   -0.50    0.617  
subsectorWholesale      -4.65e-01   6.89e-01   -0.67    0.500  
subsectorTransport      -5.77e-01   6.44e-01   -0.90    0.370  
subsectorMedia           6.26e-02   8.68e-01    0.07    0.943  
subsectorFinance         1.63e-02   9.14e-01    0.02    0.986  
subsectorReal estate    -1.17e+00   9.29e-01   -1.26    0.209  
subsectorProf. services  1.12e+00   8.86e-01    1.26    0.207  
subsectorMaintenance    -5.81e-01   6.14e-01   -0.95    0.344  
subsectorEducation       2.57e-01   9.16e-01    0.28    0.779  
subsectorHealth         -4.86e-01   5.64e-01   -0.86    0.388  
subsectorLeisure         4.92e-01   8.84e-01    0.56    0.578  
subsectorHotelsRestBar  -4.34e-01   5.67e-01   -0.77    0.444  
subsectorOther          -2.84e+01   4.84e+05    0.00    1.000  
sizeMedium              -1.85e-01   2.78e-01   -0.66    0.506  
sizeSmall               -4.47e-01   2.79e-01   -1.60    0.110  
sizeMicro               -6.28e-01   2.77e-01   -2.27    0.023 *
log_deaths               1.12e-01   1.33e-01    0.84    0.402  
log_pop                  9.19e-02   2.13e-01    0.43    0.667  
log_bribe_vic           -1.84e-01   2.71e-01   -0.68    0.496  
log_nbus                 3.47e-01   4.58e-01    0.76    0.450  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 5.572e-09 7.465e-05

Negative binomial dispersion parameter: 0.053649 (std. err.: 0.0041131)

Log-likelihood: -1528.55 
```

```r
get_glmmadmb(mnb11)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_bribe_vic + log_nbus

$logLik
[1] -1528.55

$df
[1] 30

$AIC
[1] 3117.1

$BIC
[1] 3290.746

$alpha
[1] 18.63968

$var_j
[1] 3.105053e-17

$ICC
[1] 1.66583e-18
```

```r
vif(mnb11)
```

```
              GVIF Df GVIF^(1/(2*Df))
bribes         Inf  1             Inf
yearsquant     NaN  4             NaN
subsector      NaN 15             NaN
size           Inf  3             Inf
log_deaths     NaN  1             NaN
log_pop        NaN  1             NaN
log_bribe_vic -Inf  1             NaN
log_nbus       NaN  1             NaN
```

```r
lm7 <- update(lm2, . ~ . + log_bribe_vic + log_nbus)
vif(lm7)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009295  1        1.004637
yearsquant    1.040845  4        1.005017
subsector     1.075917 15        1.002442
size          1.024307  3        1.004011
log_deaths    2.427339  1        1.557992
log_pop       2.472451  1        1.572403
log_bribe_vic 1.296283  1        1.138544
log_nbus      1.237432  1        1.112399
```

```r
mnb12_formula <- update(mnb11_formula,  . ~ . - log_nbus)

mnb12 <- model_failsafe(mnb12_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb12)
```

```
Error in summary(mnb12): object 'mnb12' not found
```

```r
get_glmmadmb(mnb12)
```

```
Error in get_glmmadmb(mnb12): object 'mnb12' not found
```

```r
vif(mnb12)
```

```
Error in vif(mnb12): object 'mnb12' not found
```

```r
lm8 <- update(lm7, . ~ . - log_nbus)
vif(lm8)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009289  1        1.004634
yearsquant    1.039902  4        1.004903
subsector     1.067480 15        1.002179
size          1.023239  3        1.003836
log_deaths    2.393137  1        1.546977
log_pop       2.470876  1        1.571902
log_bribe_vic 1.074755  1        1.036704
```

```r
lrtest(mnb3, mnb12, mnb11)
```

```
Error in lrtest.default(mnb3, mnb12, mnb11): object 'mnb12' not found
```

```r
screenreg(list("deaths + pop" = mnb3,
               "+ s bribes" = mnb12,
               "+ nbus" = mnb11),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb12' not found
```


```r
# firearms-related federal crimes

mnb13_formula <- update(count_formula, . ~ . + log_wpn_crim)

mnb13 <- model_failsafe(mnb13_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb13)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3113 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.32e-01   5.66e-01   -0.23    0.816  
bribes                  -5.54e-03   1.03e-01   -0.05    0.957  
yearsquant(8,16]         2.23e-01   3.17e-01    0.70    0.482  
yearsquant(16,25]       -9.68e-02   2.97e-01   -0.33    0.745  
yearsquant(25,34]        4.40e-01   3.02e-01    1.46    0.145  
yearsquant(34,43]        1.48e-01   3.14e-01    0.47    0.639  
subsectorMining         -1.41e+00   1.03e+00   -1.38    0.168  
subsectorConstruction    2.47e-02   5.65e-01    0.04    0.965  
subsectorManufacturing  -2.43e-01   5.48e-01   -0.44    0.657  
subsectorWholesale      -5.45e-01   6.86e-01   -0.79    0.427  
subsectorTransport      -6.10e-01   6.42e-01   -0.95    0.342  
subsectorMedia           1.52e-01   8.64e-01    0.18    0.861  
subsectorFinance        -2.79e-02   9.12e-01   -0.03    0.976  
subsectorReal estate    -1.29e+00   9.22e-01   -1.40    0.162  
subsectorProf. services  1.16e+00   8.83e-01    1.31    0.189  
subsectorMaintenance    -5.64e-01   6.17e-01   -0.91    0.361  
subsectorEducation       1.31e-01   9.10e-01    0.14    0.886  
subsectorHealth         -5.17e-01   5.64e-01   -0.92    0.360  
subsectorLeisure         3.53e-01   8.79e-01    0.40    0.688  
subsectorHotelsRestBar  -4.21e-01   5.65e-01   -0.75    0.456  
subsectorOther          -3.45e+01   2.22e+05    0.00    1.000  
sizeMedium              -1.85e-01   2.76e-01   -0.67    0.503  
sizeSmall               -4.65e-01   2.78e-01   -1.67    0.095 .
sizeMicro               -6.08e-01   2.76e-01   -2.20    0.028 *
log_wpn_crim             1.04e-01   1.06e-01    0.98    0.328  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 3.216e-09 5.671e-05

Negative binomial dispersion parameter: 0.053375 (std. err.: 0.0040876)

Log-likelihood: -1529.51 
```

```r
get_glmmadmb(mnb13)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim

$logLik
[1] -1529.51

$df
[1] 27

$AIC
[1] 3113.02

$BIC
[1] 3269.302

$alpha
[1] 18.73536

$var_j
[1] 1.034459e-17

$ICC
[1] 5.521423e-19
```

```r
vif(mnb13)
```

```
                      GVIF Df GVIF^(1/(2*Df))
bribes        4.885867e+08  1           22104
yearsquant    0.000000e+00  4               0
subsector     0.000000e+00 15               0
size          0.000000e+00  3               0
log_wpn_crim -2.777640e-08  1             NaN
```

```r
lrtest(mnb1, mnb13)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  27 -1529.5  1  0.94     0.3323
```

```r
mnb14_formula <- update(mnb4_formula, . ~ . + log_wpn_crim)

mnb14 <- model_failsafe(mnb14_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb14)
```

```
Error in summary(mnb14): object 'mnb14' not found
```

```r
get_glmmadmb(mnb14)
```

```
Error in get_glmmadmb(mnb14): object 'mnb14' not found
```

```r
vif(mnb14)
```

```
Error in vif(mnb14): object 'mnb14' not found
```

```r
vif(mnb14)^2
```

```
Error in vif(mnb14): object 'mnb14' not found
```

```r
lm9 <- update(lm3, . ~ . + log_wpn_crim)
vif(lm9)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.010911  1        1.005441
yearsquant   1.037375  4        1.004597
subsector    1.061700 15        1.001998
size         1.021668  3        1.003579
log_deaths   4.730597  1        2.174994
log_wpn_crim 4.738002  1        2.176695
```

```r
vif(lm9)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.021940   1        1.010911
yearsquant    1.076146  16        1.009215
subsector     1.127208 225        1.003999
size          1.043805   9        1.007171
log_deaths   22.378549   1        4.730597
log_wpn_crim 22.448667   1        4.738002
```

```r
mnb15_formula <- update(mnb3_formula, . ~ . + log_wpn_crim)

mnb15 <- model_failsafe(mnb15_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb15)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3115.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.00e-01   5.63e-01   -0.18    0.859  
bribes                   4.21e-04   1.03e-01    0.00    0.997  
yearsquant(8,16]         2.08e-01   3.18e-01    0.65    0.514  
yearsquant(16,25]       -1.16e-01   3.00e-01   -0.39    0.699  
yearsquant(25,34]        3.91e-01   3.06e-01    1.28    0.200  
yearsquant(34,43]        1.64e-01   3.16e-01    0.52    0.603  
subsectorMining         -1.25e+00   1.03e+00   -1.21    0.227  
subsectorConstruction    6.64e-02   5.65e-01    0.12    0.906  
subsectorManufacturing  -2.53e-01   5.47e-01   -0.46    0.643  
subsectorWholesale      -4.67e-01   6.89e-01   -0.68    0.498  
subsectorTransport      -6.37e-01   6.43e-01   -0.99    0.322  
subsectorMedia           1.19e-01   8.65e-01    0.14    0.890  
subsectorFinance         7.25e-03   9.10e-01    0.01    0.994  
subsectorReal estate    -1.15e+00   9.28e-01   -1.24    0.214  
subsectorProf. services  1.15e+00   8.81e-01    1.30    0.194  
subsectorMaintenance    -6.18e-01   6.18e-01   -1.00    0.318  
subsectorEducation       1.41e-01   9.14e-01    0.15    0.877  
subsectorHealth         -4.88e-01   5.64e-01   -0.87    0.387  
subsectorLeisure         4.64e-01   8.82e-01    0.53    0.599  
subsectorHotelsRestBar  -4.06e-01   5.64e-01   -0.72    0.471  
subsectorOther          -2.41e+01   6.94e+04    0.00    1.000  
sizeMedium              -1.87e-01   2.78e-01   -0.67    0.502  
sizeSmall               -4.67e-01   2.78e-01   -1.68    0.093 .
sizeMicro               -6.58e-01   2.78e-01   -2.37    0.018 *
log_deaths               2.22e-01   2.12e-01    1.05    0.295  
log_pop                  6.77e-02   2.07e-01    0.33    0.744  
log_wpn_crim            -1.61e-01   2.30e-01   -0.70    0.484  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 3.003e-08 0.0001733

Negative binomial dispersion parameter: 0.053611 (std. err.: 0.0041091)

Log-likelihood: -1528.66 
```

```r
get_glmmadmb(mnb15)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_wpn_crim

$logLik
[1] -1528.66

$df
[1] 29

$AIC
[1] 3115.32

$BIC
[1] 3283.178

$alpha
[1] 18.65289

$var_j
[1] 9.019811e-16

$ICC
[1] 4.835611e-17
```

```r
vif(mnb15)
```

```
                      GVIF Df GVIF^(1/(2*Df))
bribes        2.655396e+16  1    1.629539e+08
yearsquant   -4.015355e+12  4             NaN
subsector     0.000000e+00 15    0.000000e+00
size         -1.667511e-26  3             NaN
log_deaths    2.157953e-09  1    4.645378e-05
log_pop       1.494360e-09  1    3.865695e-05
log_wpn_crim -1.827175e-08  1             NaN
```

```r
vif(mnb15)^2
```

```
                     GVIF  Df GVIF^(1/(2*Df))
bribes       7.051128e+32   1    2.655396e+16
yearsquant   1.612307e+25  16             NaN
subsector    0.000000e+00 225    0.000000e+00
size         2.780591e-52   9             NaN
log_deaths   4.656763e-18   1    2.157953e-09
log_pop      2.233110e-18   1    1.494360e-09
log_wpn_crim 3.338568e-16   1             NaN
```

```r
lm10 <- update(lm2, . ~ . + log_wpn_crim)
vif(lm10)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.010928  1        1.005449
yearsquant   1.039671  4        1.004875
subsector    1.068417 15        1.002208
size         1.023843  3        1.003935
log_deaths   5.368257  1        2.316950
log_pop      2.494122  1        1.579279
log_wpn_crim 4.944377  1        2.223595
```

```r
vif(lm10)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.021976   1        1.010928
yearsquant    1.080917  16        1.009774
subsector     1.141515 225        1.004422
size          1.048255   9        1.007885
log_deaths   28.818182   1        5.368257
log_pop       6.220643   1        2.494122
log_wpn_crim 24.446859   1        4.944377
```

```r
lrtest(mnb4, mnb14, mnb15)
```

```
Error in lrtest.default(mnb4, mnb14, mnb15): object 'mnb14' not found
```

```r
lrtest(mnb3, mnb15)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  28 -1528.9                    
2  29 -1528.7  1   0.5     0.4795
```

```r
# droping deaths

mnb16_formula <- update(mnb15_formula, . ~ . - log_deaths)

mnb16 <- model_failsafe(mnb16_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb16)
```

```
Error in summary(mnb16): object 'mnb16' not found
```

```r
get_glmmadmb(mnb16)
```

```
Error in get_glmmadmb(mnb16): object 'mnb16' not found
```

```r
vif(mnb16)
```

```
Error in vif(mnb16): object 'mnb16' not found
```

```r
vif(mnb16)^2
```

```
Error in vif(mnb16): object 'mnb16' not found
```

```r
lm11 <- update(lm10, . ~ . - log_deaths)
vif(lm11)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.009551  1        1.004764
yearsquant   1.039203  4        1.004818
subsector    1.063355 15        1.002050
size         1.023783  3        1.003925
log_pop      2.197862  1        1.482519
log_wpn_crim 2.203018  1        1.484257
```

```r
vif(lm11)^2
```

```
                 GVIF  Df GVIF^(1/(2*Df))
bribes       1.019194   1        1.009551
yearsquant   1.079944  16        1.009660
subsector    1.130723 225        1.004104
size         1.048132   9        1.007866
log_pop      4.830595   1        2.197862
log_wpn_crim 4.853290   1        2.203018
```

```r
lrtest(mnb5 ,mnb16, mnb15)
```

```
Error in lrtest.default(mnb5, mnb16, mnb15): object 'mnb16' not found
```

```r
lrtest(mnb13, mnb16)
```

```
Error in lrtest.default(mnb13, mnb16): object 'mnb16' not found
```

```r
screenreg(list("No state" = mnb1,
               "deaths" = mnb4,
               "deaths +  pop" = mnb3,
               "wpns" = mnb13,
               "wpns + dth" = mnb14,
               "wpns + pop" = mnb16,
               "wpns + dth + pop" = mnb15),
          single.row = FALSE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb14' not found
```

```r
## include state bribes var

mnb17_formula <- update(mnb16_formula, . ~ . + log_bribe_vic)

mnb17 <- model_failsafe(mnb17_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb17)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3116.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.47e-01   5.63e-01   -0.26    0.794  
bribes                  -1.18e-02   1.03e-01   -0.11    0.909  
yearsquant(8,16]         2.13e-01   3.18e-01    0.67    0.504  
yearsquant(16,25]       -1.19e-01   3.00e-01   -0.40    0.691  
yearsquant(25,34]        4.25e-01   3.04e-01    1.40    0.161  
yearsquant(34,43]        1.54e-01   3.15e-01    0.49    0.625  
subsectorMining         -1.40e+00   1.03e+00   -1.36    0.174  
subsectorConstruction    4.21e-02   5.65e-01    0.07    0.941  
subsectorManufacturing  -2.32e-01   5.47e-01   -0.42    0.671  
subsectorWholesale      -5.24e-01   6.84e-01   -0.77    0.443  
subsectorTransport      -5.78e-01   6.42e-01   -0.90    0.368  
subsectorMedia           8.65e-02   8.67e-01    0.10    0.921  
subsectorFinance        -4.42e-02   9.12e-01   -0.05    0.961  
subsectorReal estate    -1.23e+00   9.27e-01   -1.33    0.184  
subsectorProf. services  1.20e+00   8.83e-01    1.36    0.173  
subsectorMaintenance    -5.44e-01   6.16e-01   -0.88    0.377  
subsectorEducation       2.05e-01   9.15e-01    0.22    0.823  
subsectorHealth         -5.05e-01   5.64e-01   -0.90    0.370  
subsectorLeisure         4.22e-01   8.83e-01    0.48    0.633  
subsectorHotelsRestBar  -3.94e-01   5.65e-01   -0.70    0.486  
subsectorOther          -2.27e+01   3.44e+04    0.00    0.999  
sizeMedium              -1.67e-01   2.78e-01   -0.60    0.547  
sizeSmall               -4.58e-01   2.79e-01   -1.64    0.101  
sizeMicro               -6.19e-01   2.78e-01   -2.23    0.026 *
log_pop                  1.77e-01   2.04e-01    0.87    0.385  
log_wpn_crim             1.53e-02   1.48e-01    0.10    0.918  
log_bribe_vic           -1.00e-01   2.45e-01   -0.41    0.682  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 3.909e-08 0.0001977

Negative binomial dispersion parameter: 0.053474 (std. err.: 0.0040963)

Log-likelihood: -1529.13 
```

```r
get_glmmadmb(mnb17)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic

$logLik
[1] -1529.13

$df
[1] 29

$AIC
[1] 3116.26

$BIC
[1] 3284.118

$alpha
[1] 18.70068

$var_j
[1] 1.527872e-15

$ICC
[1] 8.170141e-17
```

```r
vif(mnb17)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes         2.292427e+14  1        15140763
yearsquant     0.000000e+00  4               0
subsector      0.000000e+00 15               0
size          -6.241476e-29  3             NaN
log_pop       -3.882478e-10  1             NaN
log_wpn_crim  -3.091522e-10  1             NaN
log_bribe_vic -3.978079e-10  1             NaN
```

```r
mnb18_formula <- update(mnb17_formula, . ~ . + log_nbus)

mnb18 <- model_failsafe(mnb18_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb18)
```

```
Error in summary(mnb18): object 'mnb18' not found
```

```r
get_glmmadmb(mnb18)
```

```
Error in get_glmmadmb(mnb18): object 'mnb18' not found
```

```r
vif(mnb18)
```

```
Error in vif(mnb18): object 'mnb18' not found
```

```r
lm12 <- update(lm10, . ~ . + log_bribe_vic)

vif(lm12)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.010942  1        1.005456
yearsquant    1.041438  4        1.005088
subsector     1.074943 15        1.002412
size          1.024438  3        1.004032
log_deaths    5.478172  1        2.340550
log_pop       2.623375  1        1.619684
log_wpn_crim  5.169042  1        2.273553
log_bribe_vic 1.123591  1        1.059996
```

```r
vif(lm12)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.022004   1        1.010942
yearsquant     1.084593  16        1.010202
subsector      1.155503 225        1.004829
size           1.049473   9        1.008081
log_deaths    30.010369   1        5.478172
log_pop        6.882096   1        2.623375
log_wpn_crim  26.718993   1        5.169042
log_bribe_vic  1.262456   1        1.123591
```

```r
lm13 <- update(lm12, . ~ . + log_nbus)

vif(lm13)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.010949  1        1.005459
yearsquant    1.042389  4        1.005203
subsector     1.083443 15        1.002675
size          1.025507  3        1.004207
log_deaths    5.509910  1        2.347320
log_pop       2.624833  1        1.620133
log_wpn_crim  5.169119  1        2.273570
log_bribe_vic 1.344319  1        1.159448
log_nbus      1.237450  1        1.112407
```

```r
vif(lm13)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.022017   1        1.010949
yearsquant     1.086576  16        1.010433
subsector      1.173848 225        1.005357
size           1.051665   9        1.008431
log_deaths    30.359108   1        5.509910
log_pop        6.889746   1        2.624833
log_wpn_crim  26.719790   1        5.169119
log_bribe_vic  1.807194   1        1.344319
log_nbus       1.531283   1        1.237450
```

```r
lm14 <- update(lm13, . ~ . - log_deaths)

vif(lm14)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009660  1        1.004818
yearsquant    1.041863  4        1.005139
subsector     1.077207 15        1.002482
size          1.025434  3        1.004195
log_pop       2.390983  1        1.546280
log_wpn_crim  2.277207  1        1.509042
log_bribe_vic 1.309957  1        1.144533
log_nbus      1.230322  1        1.109199
```

```r
vif(lm14)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.019413   1        1.009660
yearsquant    1.085478  16        1.010305
subsector     1.160375 225        1.004970
size          1.051515   9        1.008407
log_pop       5.716799   1        2.390983
log_wpn_crim  5.185670   1        2.277207
log_bribe_vic 1.715987   1        1.309957
log_nbus      1.513693   1        1.230322
```

```r
lrtest(mnb16, mnb17, mnb18)
```

```
Error in lrtest(mnb16, mnb17, mnb18): object 'mnb16' not found
```

```r
mnb19_formula <- update(mnb17_formula, . ~ . - log_pop)

mnb19 <- model_failsafe(mnb19_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb19)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3115 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.34e-01   5.67e-01   -0.24    0.814  
bribes                  -5.93e-03   1.04e-01   -0.06    0.954  
yearsquant(8,16]         2.25e-01   3.18e-01    0.71    0.480  
yearsquant(16,25]       -9.37e-02   2.99e-01   -0.31    0.754  
yearsquant(25,34]        4.37e-01   3.04e-01    1.44    0.150  
yearsquant(34,43]        1.48e-01   3.14e-01    0.47    0.639  
subsectorMining         -1.42e+00   1.03e+00   -1.38    0.167  
subsectorConstruction    2.19e-02   5.66e-01    0.04    0.969  
subsectorManufacturing  -2.46e-01   5.49e-01   -0.45    0.654  
subsectorWholesale      -5.46e-01   6.86e-01   -0.80    0.427  
subsectorTransport      -6.10e-01   6.42e-01   -0.95    0.342  
subsectorMedia           1.48e-01   8.65e-01    0.17    0.864  
subsectorFinance        -3.51e-02   9.15e-01   -0.04    0.969  
subsectorReal estate    -1.28e+00   9.24e-01   -1.38    0.166  
subsectorProf. services  1.16e+00   8.83e-01    1.31    0.189  
subsectorMaintenance    -5.63e-01   6.17e-01   -0.91    0.361  
subsectorEducation       1.31e-01   9.10e-01    0.14    0.885  
subsectorHealth         -5.19e-01   5.65e-01   -0.92    0.358  
subsectorLeisure         3.54e-01   8.79e-01    0.40    0.688  
subsectorHotelsRestBar  -4.19e-01   5.65e-01   -0.74    0.458  
subsectorOther          -3.28e+01   9.41e+05    0.00    1.000  
sizeMedium              -1.87e-01   2.77e-01   -0.67    0.500  
sizeSmall               -4.64e-01   2.79e-01   -1.66    0.096 .
sizeMicro               -6.05e-01   2.78e-01   -2.18    0.029 *
log_wpn_crim             1.05e-01   1.07e-01    0.98    0.325  
log_bribe_vic           -2.34e-02   2.34e-01   -0.10    0.920  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 3.171e-09 5.631e-05

Negative binomial dispersion parameter: 0.053378 (std. err.: 0.0040881)

Log-likelihood: -1529.51 
```

```r
get_glmmadmb(mnb19)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic

$logLik
[1] -1529.51

$df
[1] 28

$AIC
[1] 3115.02

$BIC
[1] 3277.09

$alpha
[1] 18.73431

$var_j
[1] 1.005588e-17

$ICC
[1] 5.367625e-19
```

```r
lrtest(mnb19, mnb16)
```

```
Error in lrtest.default(mnb19, mnb16): object 'mnb16' not found
```

```r
screenreg(list("no state" = mnb1,
               "wpns + pop" = mnb16,
               "+ s bribes" = mnb17,
               "+ nbus" = mnb18,
               "wpns + s bribes" = mnb19),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb16' not found
```


```r
# drugs-related federal crimes

mnb20_formula <- update(mnb18_formula, . ~ . + log_drug_crim)

mnb20 <- model_failsafe(mnb20_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb20)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3119.8 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.21e-01   5.68e-01   -0.21    0.831  
bribes                  -1.30e-02   1.03e-01   -0.13    0.900  
yearsquant(8,16]         2.09e-01   3.18e-01    0.66    0.511  
yearsquant(16,25]       -1.35e-01   3.01e-01   -0.45    0.655  
yearsquant(25,34]        4.39e-01   3.05e-01    1.44    0.150  
yearsquant(34,43]        1.58e-01   3.17e-01    0.50    0.618  
subsectorMining         -1.46e+00   1.03e+00   -1.42    0.157  
subsectorConstruction    1.49e-03   5.68e-01    0.00    0.998  
subsectorManufacturing  -2.61e-01   5.49e-01   -0.48    0.634  
subsectorWholesale      -5.19e-01   6.85e-01   -0.76    0.449  
subsectorTransport      -5.46e-01   6.44e-01   -0.85    0.396  
subsectorMedia           3.92e-02   8.71e-01    0.05    0.964  
subsectorFinance        -9.27e-03   9.20e-01   -0.01    0.992  
subsectorReal estate    -1.25e+00   9.26e-01   -1.35    0.178  
subsectorProf. services  1.13e+00   8.88e-01    1.28    0.202  
subsectorMaintenance    -5.72e-01   6.16e-01   -0.93    0.353  
subsectorEducation       2.59e-01   9.18e-01    0.28    0.778  
subsectorHealth         -4.98e-01   5.64e-01   -0.88    0.377  
subsectorLeisure         4.41e-01   8.90e-01    0.49    0.621  
subsectorHotelsRestBar  -4.34e-01   5.68e-01   -0.76    0.445  
subsectorOther          -3.42e+01   7.09e+05    0.00    1.000  
sizeMedium              -1.65e-01   2.77e-01   -0.59    0.553  
sizeSmall               -4.48e-01   2.80e-01   -1.60    0.109  
sizeMicro               -6.19e-01   2.78e-01   -2.23    0.026 *
log_pop                  1.96e-01   2.06e-01    0.95    0.341  
log_wpn_crim             2.22e-02   1.79e-01    0.12    0.901  
log_bribe_vic           -1.82e-01   2.72e-01   -0.67    0.502  
log_nbus                 3.17e-01   4.70e-01    0.67    0.500  
log_drug_crim            1.48e-03   1.36e-01    0.01    0.991  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.452e-09 4.951e-05

Negative binomial dispersion parameter: 0.053536 (std. err.: 0.0041015)

Log-likelihood: -1528.89 
```

```r
get_glmmadmb(mnb20)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1528.89

$df
[1] 31

$AIC
[1] 3119.78

$BIC
[1] 3299.215

$alpha
[1] 18.67902

$var_j
[1] 6.010833e-18

$ICC
[1] 3.217959e-19
```

```r
vif(mnb20)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes         0.000000e+00  1       0.0000000
yearsquant     0.000000e+00  4       0.0000000
subsector     -8.233462e+07 15             NaN
size          -5.489237e-16  3             NaN
log_pop        1.077294e-01  1       0.3282216
log_wpn_crim   1.066035e-01  1       0.3265019
log_bribe_vic  1.063684e-01  1       0.3261416
log_nbus       1.069919e-01  1       0.3270962
log_drug_crim  1.069648e-01  1       0.3270548
```

```r
lm15 <- update(lm14, . ~ . + log_drug_crim)
vif(lm15)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009788  1        1.004882
yearsquant    1.043282  4        1.005310
subsector     1.082343 15        1.002641
size          1.027215  3        1.004485
log_pop       2.397691  1        1.548448
log_wpn_crim  3.147607  1        1.774150
log_bribe_vic 1.317880  1        1.147990
log_nbus      1.386734  1        1.177597
log_drug_crim 2.263375  1        1.504452
```

```r
vif(lm15)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.019671   1        1.009788
yearsquant    1.088437  16        1.010649
subsector     1.171467 225        1.005289
size          1.055170   9        1.008991
log_pop       5.748924   1        2.397691
log_wpn_crim  9.907430   1        3.147607
log_bribe_vic 1.736808   1        1.317880
log_nbus      1.923030   1        1.386734
log_drug_crim 5.122868   1        2.263375
```

```r
lrtest(mnb18, mnb20)
```

```
Error in lrtest(mnb18, mnb20): object 'mnb18' not found
```

```r
# drop log_wpn_crim

mnb21_formula <- update(mnb20_formula, . ~ . - log_wpn_crim)

mnb21 <- model_failsafe(mnb21_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb21)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3117.8 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.18e-01   5.67e-01   -0.21    0.836  
bribes                  -1.48e-02   1.02e-01   -0.14    0.885  
yearsquant(8,16]         2.09e-01   3.18e-01    0.66    0.512  
yearsquant(16,25]       -1.38e-01   3.00e-01   -0.46    0.644  
yearsquant(25,34]        4.39e-01   3.04e-01    1.44    0.149  
yearsquant(34,43]        1.59e-01   3.17e-01    0.50    0.616  
subsectorMining         -1.46e+00   1.03e+00   -1.42    0.157  
subsectorConstruction    2.98e-03   5.68e-01    0.01    0.996  
subsectorManufacturing  -2.61e-01   5.49e-01   -0.47    0.635  
subsectorWholesale      -5.23e-01   6.84e-01   -0.76    0.445  
subsectorTransport      -5.46e-01   6.44e-01   -0.85    0.397  
subsectorMedia           3.17e-02   8.69e-01    0.04    0.971  
subsectorFinance        -1.92e-02   9.16e-01   -0.02    0.983  
subsectorReal estate    -1.25e+00   9.26e-01   -1.35    0.177  
subsectorProf. services  1.13e+00   8.88e-01    1.27    0.203  
subsectorMaintenance    -5.77e-01   6.15e-01   -0.94    0.348  
subsectorEducation       2.55e-01   9.18e-01    0.28    0.781  
subsectorHealth         -4.99e-01   5.64e-01   -0.88    0.377  
subsectorLeisure         4.29e-01   8.85e-01    0.48    0.628  
subsectorHotelsRestBar  -4.35e-01   5.68e-01   -0.77    0.443  
subsectorOther          -3.17e+01   2.93e+05    0.00    1.000  
sizeMedium              -1.62e-01   2.77e-01   -0.59    0.558  
sizeSmall               -4.50e-01   2.80e-01   -1.61    0.108  
sizeMicro               -6.20e-01   2.78e-01   -2.23    0.026 *
log_pop                  2.11e-01   1.70e-01    1.24    0.214  
log_bribe_vic           -1.86e-01   2.69e-01   -0.69    0.489  
log_nbus                 3.21e-01   4.68e-01    0.69    0.493  
log_drug_crim            1.07e-02   1.13e-01    0.09    0.925  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance    StdDev
(Intercept) 3.38e-09 5.813e-05

Negative binomial dispersion parameter: 0.053532 (std. err.: 0.004101)

Log-likelihood: -1528.9 
```

```r
get_glmmadmb(mnb21)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1528.9

$df
[1] 30

$AIC
[1] 3117.8

$BIC
[1] 3291.446

$alpha
[1] 18.68042

$var_j
[1] 1.142102e-17

$ICC
[1] 6.113901e-19
```

```r
vif(mnb21)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes        -2.898483e+09  1             NaN
yearsquant     0.000000e+00  4        0.000000
subsector     -5.790328e+16 15             NaN
size          -3.268687e-07  3             NaN
log_pop        8.392481e+00  1        2.896978
log_bribe_vic  5.021776e+00  1        2.240932
log_nbus       8.391126e+00  1        2.896744
log_drug_crim -6.337517e-09  1             NaN
```

```r
vif(mnb21)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        8.401205e+18   1             NaN
yearsquant    0.000000e+00  16        0.000000
subsector     3.352790e+33 225             NaN
size          1.068431e-13   9             NaN
log_pop       7.043373e+01   1        8.392481
log_bribe_vic 2.521823e+01   1        5.021776
log_nbus      7.041099e+01   1        8.391126
log_drug_crim 4.016412e-17   1             NaN
```

```r
lm16 <- update(lm15, . ~ . - log_wpn_crim)
vif(lm16)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009229  1        1.004604
yearsquant    1.040001  4        1.004915
subsector     1.076619 15        1.002464
size          1.025910  3        1.004272
log_pop       1.550004  1        1.244992
log_bribe_vic 1.295382  1        1.138149
log_nbus      1.371962  1        1.171308
log_drug_crim 1.637489  1        1.279644
```

```r
vif(lm16)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.018542   1        1.009229
yearsquant    1.081601  16        1.009854
subsector     1.159109 225        1.004934
size          1.052490   9        1.008563
log_pop       2.402513   1        1.550004
log_bribe_vic 1.678015   1        1.295382
log_nbus      1.882280   1        1.371962
log_drug_crim 2.681372   1        1.637489
```

```r
lrtest(mnb21, mnb20)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus + log_drug_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  30 -1528.9                    
2  31 -1528.9  1  0.02     0.8875
```

```r
# drop log_drug_crim

mnb22_formula <- update(mnb21_formula, . ~ . -  log_pop)

mnb22 <- model_failsafe(mnb22_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb22)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3117.4 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -7.57e-02   5.73e-01   -0.13    0.895  
bribes                  -1.66e-02   1.02e-01   -0.16    0.871  
yearsquant(8,16]         2.31e-01   3.18e-01    0.73    0.467  
yearsquant(16,25]       -1.14e-01   2.99e-01   -0.38    0.704  
yearsquant(25,34]        4.58e-01   3.04e-01    1.51    0.132  
yearsquant(34,43]        1.56e-01   3.17e-01    0.49    0.622  
subsectorMining         -1.49e+00   1.03e+00   -1.44    0.149  
subsectorConstruction   -1.27e-02   5.70e-01   -0.02    0.982  
subsectorManufacturing  -2.77e-01   5.52e-01   -0.50    0.616  
subsectorWholesale      -5.97e-01   6.85e-01   -0.87    0.383  
subsectorTransport      -6.08e-01   6.43e-01   -0.95    0.344  
subsectorMedia           9.46e-02   8.70e-01    0.11    0.913  
subsectorFinance        -8.23e-02   9.20e-01   -0.09    0.929  
subsectorReal estate    -1.35e+00   9.23e-01   -1.46    0.144  
subsectorProf. services  1.06e+00   8.90e-01    1.19    0.235  
subsectorMaintenance    -6.44e-01   6.15e-01   -1.05    0.294  
subsectorEducation       8.30e-02   9.05e-01    0.09    0.927  
subsectorHealth         -5.30e-01   5.66e-01   -0.94    0.348  
subsectorLeisure         2.23e-01   8.70e-01    0.26    0.798  
subsectorHotelsRestBar  -4.82e-01   5.69e-01   -0.85    0.397  
subsectorOther          -2.38e+01   5.86e+04    0.00    1.000  
sizeMedium              -1.83e-01   2.77e-01   -0.66    0.508  
sizeSmall               -4.72e-01   2.79e-01   -1.69    0.090 .
sizeMicro               -5.97e-01   2.78e-01   -2.15    0.032 *
log_bribe_vic           -5.60e-02   2.52e-01   -0.22    0.824  
log_nbus                 2.51e-01   4.69e-01    0.53    0.593  
log_drug_crim            7.31e-02   1.02e-01    0.71    0.475  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.725e-08 0.0001313

Negative binomial dispersion parameter: 0.05332 (std. err.: 0.0040822)

Log-likelihood: -1529.68 
```

```r
get_glmmadmb(mnb22)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1529.68

$df
[1] 29

$AIC
[1] 3117.36

$BIC
[1] 3285.218

$alpha
[1] 18.75469

$var_j
[1] 2.974245e-16

$ICC
[1] 1.585868e-17
```

```r
mnb23_formula <- update(mnb22_formula, . ~ . - log_drug_crim)

mnb22 <- model_failsafe(mnb23_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
lrtest(mnb23, mnb22, mnb21)
```

```
Error in lrtest(mnb23, mnb22, mnb21): object 'mnb23' not found
```

```r
# include deaths

mnb24_formula <- update(mnb20_formula, . ~ . + log_deaths)

mnb24 <- model_failsafe(mnb24_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb24)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3120.4 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -7.02e-02   5.69e-01   -0.12    0.902  
bribes                  -4.23e-03   1.03e-01   -0.04    0.967  
yearsquant(8,16]         2.08e-01   3.19e-01    0.65    0.515  
yearsquant(16,25]       -1.23e-01   3.01e-01   -0.41    0.683  
yearsquant(25,34]        3.85e-01   3.08e-01    1.25    0.212  
yearsquant(34,43]        1.62e-01   3.18e-01    0.51    0.610  
subsectorMining         -1.33e+00   1.04e+00   -1.29    0.197  
subsectorConstruction    2.13e-02   5.68e-01    0.04    0.970  
subsectorManufacturing  -2.93e-01   5.49e-01   -0.53    0.594  
subsectorWholesale      -4.47e-01   6.91e-01   -0.65    0.518  
subsectorTransport      -6.02e-01   6.44e-01   -0.93    0.350  
subsectorMedia           3.31e-02   8.70e-01    0.04    0.970  
subsectorFinance        -3.62e-03   9.19e-01    0.00    0.997  
subsectorReal estate    -1.11e+00   9.32e-01   -1.19    0.236  
subsectorProf. services  1.08e+00   8.86e-01    1.22    0.221  
subsectorMaintenance    -6.47e-01   6.18e-01   -1.05    0.296  
subsectorEducation       2.15e-01   9.16e-01    0.24    0.814  
subsectorHealth         -4.85e-01   5.64e-01   -0.86    0.390  
subsectorLeisure         4.94e-01   8.90e-01    0.56    0.579  
subsectorHotelsRestBar  -4.34e-01   5.68e-01   -0.76    0.445  
subsectorOther          -2.45e+01   7.73e+04    0.00    1.000  
sizeMedium              -1.89e-01   2.78e-01   -0.68    0.497  
sizeSmall               -4.52e-01   2.80e-01   -1.62    0.106  
sizeMicro               -6.50e-01   2.78e-01   -2.34    0.019 *
log_pop                  1.17e-01   2.17e-01    0.54    0.588  
log_wpn_crim            -2.11e-01   2.67e-01   -0.79    0.429  
log_bribe_vic           -2.29e-01   2.74e-01   -0.84    0.403  
log_nbus                 3.65e-01   4.72e-01    0.77    0.440  
log_drug_crim            2.56e-02   1.36e-01    0.19    0.850  
log_deaths               2.50e-01   2.16e-01    1.16    0.246  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance   StdDev
(Intercept) 1.513e-08 0.000123

Negative binomial dispersion parameter: 0.053742 (std. err.: 0.0041216)

Log-likelihood: -1528.22 
```

```r
get_glmmadmb(mnb24)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    log_deaths

$logLik
[1] -1528.22

$df
[1] 32

$AIC
[1] 3120.44

$BIC
[1] 3305.663

$alpha
[1] 18.60742

$var_j
[1] 2.290682e-16

$ICC
[1] 1.231058e-17
```

```r
vif(mnb24)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes        -1.125704e+16  1             NaN
yearsquant    -1.935832e+14  4             NaN
subsector      5.082846e-27 15    1.329036e-01
size           5.531235e-25  3    9.060184e-05
log_pop        3.874510e-09  1    6.224556e-05
log_wpn_crim   3.926446e-08  1    1.981526e-04
log_bribe_vic -4.294059e-07  1             NaN
log_nbus      -1.032740e-06  1             NaN
log_drug_crim -8.604356e-08  1             NaN
log_deaths    -2.921194e-08  1             NaN
```

```r
vif(mnb24)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        1.267210e+32   1             NaN
yearsquant    3.747446e+28  16             NaN
subsector     2.583532e-53 225    1.766338e-02
size          3.059456e-49   9    8.208694e-09
log_pop       1.501183e-17   1    3.874510e-09
log_wpn_crim  1.541698e-15   1    3.926446e-08
log_bribe_vic 1.843894e-13   1             NaN
log_nbus      1.066553e-12   1             NaN
log_drug_crim 7.403494e-15   1             NaN
log_deaths    8.533376e-16   1             NaN
```

```r
lm17 <- update(lm15, . ~ . + log_deaths)
vif(lm17)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.011235  1        1.005602
yearsquant    1.043737  4        1.005365
subsector     1.089059 15        1.002848
size          1.027329  3        1.004504
log_pop       2.649337  1        1.627678
log_wpn_crim  6.615988  1        2.572156
log_bribe_vic 1.358311  1        1.165466
log_nbus      1.407953  1        1.186572
log_drug_crim 2.315932  1        1.521819
log_deaths    5.637853  1        2.374416
```

```r
vif(lm17)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.022595   1        1.011235
yearsquant     1.089388  16        1.010759
subsector      1.186049 225        1.005704
size           1.055404   9        1.009028
log_pop        7.018987   1        2.649337
log_wpn_crim  43.771304   1        6.615988
log_bribe_vic  1.845009   1        1.358311
log_nbus       1.982331   1        1.407953
log_drug_crim  5.363542   1        2.315932
log_deaths    31.785389   1        5.637853
```

```r
# no fed crimes

mnb25_formula <- update(mnb18_formula, . ~ . - log_wpn_crim)

mnb25 <- model_failsafe(mnb25_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb25)
```

```
Error in summary(mnb25): object 'mnb25' not found
```

```r
get_glmmadmb(mnb25)
```

```
Error in get_glmmadmb(mnb25): object 'mnb25' not found
```

```r
vif(mnb25)
```

```
Error in vif(mnb25): object 'mnb25' not found
```

```r
mnb26_formula <- update(mnb20_formula, . ~ . -  log_pop)

mnb26 <- model_failsafe(mnb26_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb26)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3118.7 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.10e-01   5.72e-01   -0.19     0.85  
bribes                  -6.47e-03   1.04e-01   -0.06     0.95  
yearsquant(8,16]         2.24e-01   3.18e-01    0.70     0.48  
yearsquant(16,25]       -1.04e-01   2.99e-01   -0.35     0.73  
yearsquant(25,34]        4.50e-01   3.05e-01    1.48     0.14  
yearsquant(34,43]        1.50e-01   3.16e-01    0.48     0.63  
subsectorMining         -1.48e+00   1.03e+00   -1.43     0.15  
subsectorConstruction   -1.29e-02   5.69e-01   -0.02     0.98  
subsectorManufacturing  -2.72e-01   5.51e-01   -0.49     0.62  
subsectorWholesale      -5.44e-01   6.87e-01   -0.79     0.43  
subsectorTransport      -5.86e-01   6.43e-01   -0.91     0.36  
subsectorMedia           1.12e-01   8.69e-01    0.13     0.90  
subsectorFinance        -5.57e-03   9.23e-01   -0.01     1.00  
subsectorReal estate    -1.30e+00   9.25e-01   -1.40     0.16  
subsectorProf. services  1.10e+00   8.89e-01    1.23     0.22  
subsectorMaintenance    -5.90e-01   6.18e-01   -0.95     0.34  
subsectorEducation       1.68e-01   9.12e-01    0.18     0.85  
subsectorHealth         -5.16e-01   5.65e-01   -0.91     0.36  
subsectorLeisure         3.60e-01   8.86e-01    0.41     0.68  
subsectorHotelsRestBar  -4.55e-01   5.69e-01   -0.80     0.42  
subsectorOther          -2.74e+01   4.36e+05    0.00     1.00  
sizeMedium              -1.87e-01   2.77e-01   -0.67     0.50  
sizeSmall               -4.56e-01   2.80e-01   -1.63     0.10  
sizeMicro               -6.03e-01   2.78e-01   -2.17     0.03 *
log_wpn_crim             1.19e-01   1.45e-01    0.82     0.41  
log_bribe_vic           -8.47e-02   2.57e-01   -0.33     0.74  
log_nbus                 2.65e-01   4.74e-01    0.56     0.58  
log_drug_crim            1.61e-03   1.34e-01    0.01     0.99  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 6.808e-09 8.251e-05

Negative binomial dispersion parameter: 0.053422 (std. err.: 0.004092)

Log-likelihood: -1529.34 
```

```r
get_glmmadmb(mnb26)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1529.34

$df
[1] 30

$AIC
[1] 3118.68

$BIC
[1] 3292.326

$alpha
[1] 18.71888

$var_j
[1] 4.634478e-17

$ICC
[1] 2.475831e-18
```

```r
lrtest(mnb25, mnb20)
```

```
Error in lrtest(mnb25, mnb20): object 'mnb25' not found
```

```r
screenreg(list("no fed" = mnb25,
               "- drug + wpn" = mnb18,
               "+ drug - wpn" = mnb21,
               "+ drug + wpn - pop " = mnb26,
               "+ drug + wpn + pop" = mnb20),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb25' not found
```


Controls: Competitive index

- Include both, drop 1, test
- both + homicides, test



```r
## controls: competitive index

mnb27_formula <- update(mnb20_formula, . ~ . + comp_index)

mnb27 <- model_failsafe(mnb27_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb27)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3121.5 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.17e-01   5.67e-01   -0.21    0.837  
bribes                  -1.59e-02   1.04e-01   -0.15    0.878  
yearsquant(8,16]         1.97e-01   3.18e-01    0.62    0.536  
yearsquant(16,25]       -1.45e-01   3.02e-01   -0.48    0.630  
yearsquant(25,34]        4.24e-01   3.06e-01    1.39    0.166  
yearsquant(34,43]        1.46e-01   3.17e-01    0.46    0.644  
subsectorMining         -1.47e+00   1.03e+00   -1.42    0.155  
subsectorConstruction    1.06e-02   5.68e-01    0.02    0.985  
subsectorManufacturing  -2.45e-01   5.50e-01   -0.44    0.656  
subsectorWholesale      -5.42e-01   6.85e-01   -0.79    0.429  
subsectorTransport      -5.48e-01   6.44e-01   -0.85    0.394  
subsectorMedia          -1.74e-02   8.77e-01   -0.02    0.984  
subsectorFinance         4.87e-03   9.20e-01    0.01    0.996  
subsectorReal estate    -1.23e+00   9.27e-01   -1.32    0.186  
subsectorProf. services  1.15e+00   8.88e-01    1.30    0.194  
subsectorMaintenance    -5.35e-01   6.21e-01   -0.86    0.390  
subsectorEducation       2.92e-01   9.21e-01    0.32    0.751  
subsectorHealth         -4.96e-01   5.64e-01   -0.88    0.379  
subsectorLeisure         4.27e-01   8.91e-01    0.48    0.632  
subsectorHotelsRestBar  -4.42e-01   5.68e-01   -0.78    0.437  
subsectorOther          -3.88e+01   9.93e+05    0.00    1.000  
sizeMedium              -1.62e-01   2.78e-01   -0.58    0.559  
sizeSmall               -4.52e-01   2.80e-01   -1.62    0.106  
sizeMicro               -6.13e-01   2.78e-01   -2.20    0.028 *
log_pop                  1.98e-01   2.07e-01    0.96    0.339  
log_wpn_crim             2.40e-02   1.79e-01    0.13    0.893  
log_bribe_vic           -1.41e-01   2.84e-01   -0.50    0.620  
log_nbus                 3.46e-01   4.74e-01    0.73    0.465  
log_drug_crim           -3.55e-03   1.35e-01   -0.03    0.979  
comp_index               6.78e-03   1.34e-02    0.51    0.613  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.073e-09 4.553e-05

Negative binomial dispersion parameter: 0.05357 (std. err.: 0.0041046)

Log-likelihood: -1528.76 
```

```r
get_glmmadmb(mnb27)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index

$logLik
[1] -1528.76

$df
[1] 32

$AIC
[1] 3121.52

$BIC
[1] 3306.743

$alpha
[1] 18.66716

$var_j
[1] 4.296085e-18

$ICC
[1] 2.301413e-19
```

```r
lrtest(mnb20, mnb27)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  31 -1528.9                    
2  32 -1528.8  1  0.26     0.6101
```

```r
vif(mnb27)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes         0.000000e+00  1        0.000000
yearsquant     0.000000e+00  4        0.000000
subsector      1.492294e+06 15        1.606183
size           0.000000e+00  3        0.000000
log_pop       -4.883336e+00  1             NaN
log_wpn_crim  -4.791055e+00  1             NaN
log_bribe_vic -4.780107e+00  1             NaN
log_nbus      -4.850388e+00  1             NaN
log_drug_crim -4.893352e+00  1             NaN
comp_index    -4.895763e+00  1             NaN
```

```r
vif(mnb27)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        0.000000e+00   1        0.000000
yearsquant    0.000000e+00  16        0.000000
subsector     2.226942e+12 225        2.579825
size          0.000000e+00   9        0.000000
log_pop       2.384697e+01   1             NaN
log_wpn_crim  2.295421e+01   1             NaN
log_bribe_vic 2.284942e+01   1             NaN
log_nbus      2.352627e+01   1             NaN
log_drug_crim 2.394489e+01   1             NaN
comp_index    2.396850e+01   1             NaN
```

```r
lm18 <- update(lm15, . ~ . + comp_index)
vif(lm18)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009863  1        1.004920
yearsquant    1.045319  4        1.005556
subsector     1.089369 15        1.002857
size          1.027921  3        1.004600
log_pop       2.407559  1        1.551631
log_wpn_crim  3.184361  1        1.784478
log_bribe_vic 1.504109  1        1.226421
log_nbus      1.387016  1        1.177717
log_drug_crim 2.347924  1        1.532294
comp_index    1.227277  1        1.107825
```

```r
vif(lm18)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.019824   1        1.009863
yearsquant     1.092692  16        1.011142
subsector      1.186725 225        1.005723
size           1.056621   9        1.009222
log_pop        5.796339   1        2.407559
log_wpn_crim  10.140154   1        3.184361
log_bribe_vic  2.262343   1        1.504109
log_nbus       1.923815   1        1.387016
log_drug_crim  5.512747   1        2.347924
comp_index     1.506209   1        1.227277
```

```r
# control: rule of law index

mnb28_formula <- update(mnb27_formula, . ~ . + law_index)

mnb28 <- model_failsafe(mnb28_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb28)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.38e-01   5.69e-01   -0.24    0.809  
bribes                  -2.20e-02   1.04e-01   -0.21    0.833  
yearsquant(8,16]         2.07e-01   3.19e-01    0.65    0.516  
yearsquant(16,25]       -1.29e-01   3.04e-01   -0.43    0.671  
yearsquant(25,34]        4.16e-01   3.06e-01    1.36    0.174  
yearsquant(34,43]        1.47e-01   3.18e-01    0.46    0.644  
subsectorMining         -1.45e+00   1.03e+00   -1.40    0.160  
subsectorConstruction    4.92e-02   5.73e-01    0.09    0.932  
subsectorManufacturing  -2.12e-01   5.54e-01   -0.38    0.702  
subsectorWholesale      -5.24e-01   6.85e-01   -0.76    0.444  
subsectorTransport      -5.14e-01   6.48e-01   -0.79    0.427  
subsectorMedia          -2.94e-02   8.76e-01   -0.03    0.973  
subsectorFinance         5.15e-02   9.26e-01    0.06    0.956  
subsectorReal estate    -1.17e+00   9.32e-01   -1.26    0.209  
subsectorProf. services  1.18e+00   8.90e-01    1.33    0.184  
subsectorMaintenance    -5.48e-01   6.21e-01   -0.88    0.377  
subsectorEducation       3.83e-01   9.39e-01    0.41    0.683  
subsectorHealth         -4.92e-01   5.64e-01   -0.87    0.383  
subsectorLeisure         4.13e-01   8.93e-01    0.46    0.644  
subsectorHotelsRestBar  -4.23e-01   5.69e-01   -0.74    0.458  
subsectorOther          -2.93e+01   3.81e+05    0.00    1.000  
sizeMedium              -1.59e-01   2.78e-01   -0.57    0.567  
sizeSmall               -4.50e-01   2.80e-01   -1.61    0.107  
sizeMicro               -6.30e-01   2.80e-01   -2.25    0.024 *
log_pop                  2.19e-01   2.12e-01    1.03    0.301  
log_wpn_crim            -4.30e-02   2.22e-01   -0.19    0.847  
log_bribe_vic           -2.32e-01   3.37e-01   -0.69    0.491  
log_nbus                 4.82e-01   5.47e-01    0.88    0.379  
log_drug_crim            3.08e-02   1.52e-01    0.20    0.839  
comp_index               6.95e-03   1.35e-02    0.52    0.605  
law_index               -5.89e-03   1.18e-02   -0.50    0.617  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 4.564e-09 6.756e-05

Negative binomial dispersion parameter: 0.053592 (std. err.: 0.0041058)

Log-likelihood: -1528.63 
```

```r
get_glmmadmb(mnb28)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1528.63

$df
[1] 33

$AIC
[1] 3123.26

$BIC
[1] 3314.271

$alpha
[1] 18.6595

$var_j
[1] 2.082918e-17

$ICC
[1] 1.116278e-18
```

```r
lrtest(mnb27, mnb28)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -1528.8                    
2  33 -1528.6  1  0.26     0.6101
```

```r
lrtest(mnb20, mnb28)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  31 -1528.9                    
2  33 -1528.6  2  0.52     0.7711
```

```r
vif(mnb28)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes         8.895148e+16  1       298247343
yearsquant    -1.992256e+07  4             NaN
subsector      0.000000e+00 15               0
size           0.000000e+00  3               0
log_pop        0.000000e+00  1               0
log_wpn_crim  -1.189904e-07  1             NaN
log_bribe_vic  0.000000e+00  1               0
log_nbus       0.000000e+00  1               0
log_drug_crim  0.000000e+00  1               0
comp_index     0.000000e+00  1               0
law_index      0.000000e+00  1               0
```

```r
vif(mnb28)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        7.912365e+33   1    8.895148e+16
yearsquant    3.969084e+14  16             NaN
subsector     0.000000e+00 225    0.000000e+00
size          0.000000e+00   9    0.000000e+00
log_pop       0.000000e+00   1    0.000000e+00
log_wpn_crim  1.415872e-14   1             NaN
log_bribe_vic 0.000000e+00   1    0.000000e+00
log_nbus      0.000000e+00   1    0.000000e+00
log_drug_crim 0.000000e+00   1    0.000000e+00
comp_index    0.000000e+00   1    0.000000e+00
law_index     0.000000e+00   1    0.000000e+00
```

```r
lm19 <- update(lm18, . ~ . + law_index)
vif(lm19)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.012208  1        1.006086
yearsquant    1.047652  4        1.005836
subsector     1.096233 15        1.003067
size          1.029090  3        1.004791
log_pop       2.417286  1        1.554762
log_wpn_crim  4.239396  1        2.058979
log_bribe_vic 1.919590  1        1.385493
log_nbus      1.679917  1        1.296116
log_drug_crim 2.715554  1        1.647894
comp_index    1.237157  1        1.112276
law_index     1.885176  1        1.373017
```

```r
vif(lm19)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.024565   1        1.012208
yearsquant     1.097574  16        1.011706
subsector      1.201727 225        1.006144
size           1.059026   9        1.009604
log_pop        5.843272   1        2.417286
log_wpn_crim  17.972478   1        4.239396
log_bribe_vic  3.684825   1        1.919590
log_nbus       2.822122   1        1.679917
log_drug_crim  7.374233   1        2.715554
comp_index     1.530558   1        1.237157
law_index      3.553889   1        1.885176
```

```r
## dropping comp_index and keeping law index

mnb29_formula <- update(mnb28_formula, . ~ . - comp_index)

mnb29 <- model_failsafe(mnb29_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb29)
```

```
Error in summary(mnb29): object 'mnb29' not found
```

```r
get_glmmadmb(mnb29)
```

```
Error in get_glmmadmb(mnb29): object 'mnb29' not found
```

```r
lrtest(mnb20, mnb29)
```

```
Error in lrtest.default(mnb20, mnb29): object 'mnb29' not found
```

```r
vif(mnb29)
```

```
Error in vif(mnb29): object 'mnb29' not found
```

```r
vif(mnb29)^2
```

```
Error in vif(mnb29): object 'mnb29' not found
```

```r
lm20 <- update(lm19, . ~ . - comp_index)
vif(lm20)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.012189  1        1.006076
yearsquant    1.045386  4        1.005564
subsector     1.088755 15        1.002839
size          1.028459  3        1.004688
log_pop       2.405676  1        1.551024
log_wpn_crim  4.159456  1        2.039474
log_bribe_vic 1.682013  1        1.296924
log_nbus      1.675678  1        1.294480
log_drug_crim 2.597360  1        1.611633
law_index     1.870121  1        1.367524
```

```r
vif(lm20)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.024527   1        1.012189
yearsquant     1.092832  16        1.011158
subsector      1.185387 225        1.005685
size           1.057727   9        1.009398
log_pop        5.787275   1        2.405676
log_wpn_crim  17.301071   1        4.159456
log_bribe_vic  2.829168   1        1.682013
log_nbus       2.807896   1        1.675678
log_drug_crim  6.746278   1        2.597360
law_index      3.497352   1        1.870121
```

```r
## Including deaths

mnb30_formula <- update(mnb28_formula, . ~ . + log_deaths)

mnb30 <- model_failsafe(mnb30_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb30)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -6.02e-02   5.72e-01   -0.11     0.92  
bribes                  -1.02e-02   1.04e-01   -0.10     0.92  
yearsquant(8,16]         1.90e-01   3.19e-01    0.59     0.55  
yearsquant(16,25]       -1.33e-01   3.03e-01   -0.44     0.66  
yearsquant(25,34]        3.46e-01   3.12e-01    1.11     0.27  
yearsquant(34,43]        1.42e-01   3.18e-01    0.45     0.66  
subsectorMining         -1.32e+00   1.03e+00   -1.28     0.20  
subsectorConstruction    5.60e-02   5.73e-01    0.10     0.92  
subsectorManufacturing  -2.58e-01   5.55e-01   -0.47     0.64  
subsectorWholesale      -4.69e-01   6.90e-01   -0.68     0.50  
subsectorTransport      -6.01e-01   6.50e-01   -0.92     0.36  
subsectorMedia          -7.40e-02   8.76e-01   -0.08     0.93  
subsectorFinance         4.41e-02   9.27e-01    0.05     0.96  
subsectorReal estate    -1.01e+00   9.39e-01   -1.08     0.28  
subsectorProf. services  1.12e+00   8.88e-01    1.27     0.21  
subsectorMaintenance    -6.04e-01   6.21e-01   -0.97     0.33  
subsectorEducation       3.01e-01   9.39e-01    0.32     0.75  
subsectorHealth         -4.76e-01   5.64e-01   -0.84     0.40  
subsectorLeisure         4.79e-01   8.92e-01    0.54     0.59  
subsectorHotelsRestBar  -4.40e-01   5.69e-01   -0.77     0.44  
subsectorOther          -3.75e+01   5.64e+05    0.00     1.00  
sizeMedium              -1.90e-01   2.78e-01   -0.68     0.49  
sizeSmall               -4.59e-01   2.79e-01   -1.64     0.10  
sizeMicro               -6.50e-01   2.79e-01   -2.33     0.02 *
log_pop                  1.18e-01   2.27e-01    0.52     0.60  
log_wpn_crim            -2.76e-01   2.85e-01   -0.97     0.33  
log_bribe_vic           -2.03e-01   3.33e-01   -0.61     0.54  
log_nbus                 4.76e-01   5.51e-01    0.86     0.39  
log_drug_crim            3.41e-02   1.49e-01    0.23     0.82  
comp_index               1.21e-02   1.40e-02    0.86     0.39  
law_index               -2.10e-03   1.20e-02   -0.18     0.86  
log_deaths               2.96e-01   2.31e-01    1.28     0.20  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.184e-09 4.674e-05

Negative binomial dispersion parameter: 0.053848 (std. err.: 0.0041309)

Log-likelihood: -1527.82 
```

```r
get_glmmadmb(mnb30)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths

$logLik
[1] -1527.82

$df
[1] 34

$AIC
[1] 3123.64

$BIC
[1] 3320.439

$alpha
[1] 18.57079

$var_j
[1] 4.771603e-18

$ICC
[1] 2.569413e-19
```

```r
lrtest(mnb28, mnb30)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths
  #Df  LogLik Df Chisq Pr(>Chisq)
1  33 -1528.6                    
2  34 -1527.8  1  1.62     0.2031
```

```r
vif(mnb30)
```

```
                      GVIF Df GVIF^(1/(2*Df))
bribes        1.239735e+07  1    3.520987e+03
yearsquant    0.000000e+00  4    0.000000e+00
subsector     0.000000e+00 15    0.000000e+00
size          5.933232e-02  3    6.245232e-01
log_pop       1.078507e-01  1    3.284063e-01
log_wpn_crim  1.072005e-01  1    3.274149e-01
log_bribe_vic 1.059027e-01  1    3.254269e-01
log_nbus      1.059389e-01  1    3.254826e-01
log_drug_crim 1.077033e-01  1    3.281818e-01
comp_index    1.093675e-01  1    3.307075e-01
law_index     5.720319e-02  1    2.391719e-01
log_deaths    3.839142e-10  1    1.959373e-05
```

```r
vif(mnb30)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        1.536944e+14   1    1.239735e+07
yearsquant    0.000000e+00  16    0.000000e+00
subsector     0.000000e+00 225    0.000000e+00
size          3.520324e-03   9    3.900292e-01
log_pop       1.163177e-02   1    1.078507e-01
log_wpn_crim  1.149195e-02   1    1.072005e-01
log_bribe_vic 1.121537e-02   1    1.059027e-01
log_nbus      1.122306e-02   1    1.059389e-01
log_drug_crim 1.160000e-02   1    1.077033e-01
comp_index    1.196125e-02   1    1.093675e-01
law_index     3.272204e-03   1    5.720319e-02
log_deaths    1.473901e-19   1    3.839142e-10
```

```r
lm21 <- update(lm19, . ~ . + log_deaths)
vif(lm21)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.012971  1        1.006465
yearsquant    1.048784  4        1.005972
subsector     1.100805 15        1.003207
size          1.029187  3        1.004806
log_pop       2.788567  1        1.669900
log_wpn_crim  6.906841  1        2.628087
log_bribe_vic 1.933923  1        1.390655
log_nbus      1.680738  1        1.296433
log_drug_crim 2.716880  1        1.648296
comp_index    1.356298  1        1.164602
law_index     2.100865  1        1.449436
log_deaths    6.728775  1        2.593988
```

```r
vif(lm21)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.026111   1        1.012971
yearsquant     1.099948  16        1.011979
subsector      1.211772 225        1.006423
size           1.059226   9        1.009636
log_pop        7.776104   1        2.788567
log_wpn_crim  47.704451   1        6.906841
log_bribe_vic  3.740056   1        1.933923
log_nbus       2.824881   1        1.680738
log_drug_crim  7.381434   1        2.716880
comp_index     1.839543   1        1.356298
law_index      4.413635   1        2.100865
log_deaths    45.276409   1        6.728775
```

```r
screenreg(list("no index" = mnb20,
               "comp" = mnb27,
               "law" = mnb29,
               "comp + law" = mnb28,
               "comp + law + deaths" = mnb30),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mnb29' not found
```

Final spec:
- business_level + sbribes, wpn, drug, nbus, pop, index1, index2
    - vif
- test: remove controls
- test: remove business_level vars (multi and single level)
- test: full, single and multilevel spec
- test: include deaths
- MNB vs MP


```r
mnb_final <- mnb28

mnb_final_formula <- mnb28_formula

summary(mnb_final)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.38e-01   5.69e-01   -0.24    0.809  
bribes                  -2.20e-02   1.04e-01   -0.21    0.833  
yearsquant(8,16]         2.07e-01   3.19e-01    0.65    0.516  
yearsquant(16,25]       -1.29e-01   3.04e-01   -0.43    0.671  
yearsquant(25,34]        4.16e-01   3.06e-01    1.36    0.174  
yearsquant(34,43]        1.47e-01   3.18e-01    0.46    0.644  
subsectorMining         -1.45e+00   1.03e+00   -1.40    0.160  
subsectorConstruction    4.92e-02   5.73e-01    0.09    0.932  
subsectorManufacturing  -2.12e-01   5.54e-01   -0.38    0.702  
subsectorWholesale      -5.24e-01   6.85e-01   -0.76    0.444  
subsectorTransport      -5.14e-01   6.48e-01   -0.79    0.427  
subsectorMedia          -2.94e-02   8.76e-01   -0.03    0.973  
subsectorFinance         5.15e-02   9.26e-01    0.06    0.956  
subsectorReal estate    -1.17e+00   9.32e-01   -1.26    0.209  
subsectorProf. services  1.18e+00   8.90e-01    1.33    0.184  
subsectorMaintenance    -5.48e-01   6.21e-01   -0.88    0.377  
subsectorEducation       3.83e-01   9.39e-01    0.41    0.683  
subsectorHealth         -4.92e-01   5.64e-01   -0.87    0.383  
subsectorLeisure         4.13e-01   8.93e-01    0.46    0.644  
subsectorHotelsRestBar  -4.23e-01   5.69e-01   -0.74    0.458  
subsectorOther          -2.93e+01   3.81e+05    0.00    1.000  
sizeMedium              -1.59e-01   2.78e-01   -0.57    0.567  
sizeSmall               -4.50e-01   2.80e-01   -1.61    0.107  
sizeMicro               -6.30e-01   2.80e-01   -2.25    0.024 *
log_pop                  2.19e-01   2.12e-01    1.03    0.301  
log_wpn_crim            -4.30e-02   2.22e-01   -0.19    0.847  
log_bribe_vic           -2.32e-01   3.37e-01   -0.69    0.491  
log_nbus                 4.82e-01   5.47e-01    0.88    0.379  
log_drug_crim            3.08e-02   1.52e-01    0.20    0.839  
comp_index               6.95e-03   1.35e-02    0.52    0.605  
law_index               -5.89e-03   1.18e-02   -0.50    0.617  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 4.564e-09 6.756e-05

Negative binomial dispersion parameter: 0.053592 (std. err.: 0.0041058)

Log-likelihood: -1528.63 
```

```r
get_glmmadmb(mnb_final)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1528.63

$df
[1] 33

$AIC
[1] 3123.26

$BIC
[1] 3314.271

$alpha
[1] 18.6595

$var_j
[1] 2.082918e-17

$ICC
[1] 1.116278e-18
```

```r
vif(mnb_final)
```

```
                       GVIF Df GVIF^(1/(2*Df))
bribes         8.895148e+16  1       298247343
yearsquant    -1.992256e+07  4             NaN
subsector      0.000000e+00 15               0
size           0.000000e+00  3               0
log_pop        0.000000e+00  1               0
log_wpn_crim  -1.189904e-07  1             NaN
log_bribe_vic  0.000000e+00  1               0
log_nbus       0.000000e+00  1               0
log_drug_crim  0.000000e+00  1               0
comp_index     0.000000e+00  1               0
law_index      0.000000e+00  1               0
```

```r
vif(mnb_final)^2
```

```
                      GVIF  Df GVIF^(1/(2*Df))
bribes        7.912365e+33   1    8.895148e+16
yearsquant    3.969084e+14  16             NaN
subsector     0.000000e+00 225    0.000000e+00
size          0.000000e+00   9    0.000000e+00
log_pop       0.000000e+00   1    0.000000e+00
log_wpn_crim  1.415872e-14   1             NaN
log_bribe_vic 0.000000e+00   1    0.000000e+00
log_nbus      0.000000e+00   1    0.000000e+00
log_drug_crim 0.000000e+00   1    0.000000e+00
comp_index    0.000000e+00   1    0.000000e+00
law_index     0.000000e+00   1    0.000000e+00
```

```r
# Test all state level vars
lrtest(mnb1, mnb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  26 -1530.0                    
2  33 -1528.6  7   2.7     0.9113
```

```r
# Test vs multilevel null

summary(mnb_null)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3091.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.665e-07 0.0005162

Negative binomial dispersion parameter: 0.049891 (std. err.: 0.0037718)

Log-likelihood: -1542.76 
```

```r
get_glmmadmb(mnb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1542.76

$df
[1] 3

$AIC
[1] 3091.52

$BIC
[1] 3108.885

$alpha
[1] 20.0437

$var_j
[1] 7.100093e-14

$ICC
[1] 3.542307e-15
```

```r
lrtest(mnb_null, mnb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   3 -1542.8                    
2  33 -1528.6 30 28.26     0.5567
```

```r
# test vs sigle-level full and null

nb_final_formula <- update(mnb_final_formula, . ~ . - (1 | state))

nb_final <- model_failsafe(nb_final_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(nb_final)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3121.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.38e-01   5.69e-01   -0.24    0.809  
bribes                  -2.20e-02   1.04e-01   -0.21    0.833  
yearsquant(8,16]         2.07e-01   3.19e-01    0.65    0.516  
yearsquant(16,25]       -1.29e-01   3.04e-01   -0.43    0.671  
yearsquant(25,34]        4.16e-01   3.06e-01    1.36    0.174  
yearsquant(34,43]        1.47e-01   3.18e-01    0.46    0.644  
subsectorMining         -1.45e+00   1.03e+00   -1.40    0.160  
subsectorConstruction    4.92e-02   5.73e-01    0.09    0.932  
subsectorManufacturing  -2.12e-01   5.54e-01   -0.38    0.702  
subsectorWholesale      -5.24e-01   6.85e-01   -0.76    0.444  
subsectorTransport      -5.14e-01   6.48e-01   -0.79    0.427  
subsectorMedia          -2.93e-02   8.76e-01   -0.03    0.973  
subsectorFinance         5.15e-02   9.26e-01    0.06    0.956  
subsectorReal estate    -1.17e+00   9.32e-01   -1.26    0.209  
subsectorProf. services  1.18e+00   8.90e-01    1.33    0.184  
subsectorMaintenance    -5.48e-01   6.21e-01   -0.88    0.377  
subsectorEducation       3.83e-01   9.39e-01    0.41    0.683  
subsectorHealth         -4.92e-01   5.64e-01   -0.87    0.383  
subsectorLeisure         4.13e-01   8.93e-01    0.46    0.644  
subsectorHotelsRestBar  -4.23e-01   5.69e-01   -0.74    0.458  
subsectorOther          -1.76e+01   2.61e+03   -0.01    0.995  
sizeMedium              -1.59e-01   2.78e-01   -0.57    0.567  
sizeSmall               -4.50e-01   2.80e-01   -1.61    0.107  
sizeMicro               -6.30e-01   2.80e-01   -2.25    0.024 *
log_pop                  2.19e-01   2.12e-01    1.03    0.301  
log_wpn_crim            -4.30e-02   2.22e-01   -0.19    0.847  
log_bribe_vic           -2.33e-01   3.37e-01   -0.69    0.491  
log_nbus                 4.82e-01   5.47e-01    0.88    0.379  
log_drug_crim            3.08e-02   1.52e-01    0.20    0.839  
comp_index               6.95e-03   1.35e-02    0.52    0.605  
law_index               -5.89e-03   1.18e-02   -0.50    0.616  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.053519 (std. err.: 0.0040993)

Log-likelihood: -1528.63 
```

```r
get_glmmadmb(nb_final)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1528.63

$df
[1] 32

$AIC
[1] 3121.26

$BIC
[1] 3306.483

$alpha
[1] 18.68495
```

```r
lrtest(nb_final, mnb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -1528.6                    
2  33 -1528.6  1     0          1
```

```r
summary(nb_null)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3089.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.049824 (std. err.: 0.0037658)

Log-likelihood: -1542.76 
```

```r
get_glmmadmb(nb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1542.76

$df
[1] 2

$AIC
[1] 3089.52

$BIC
[1] 3101.096

$alpha
[1] 20.07065
```

```r
lrtest(nb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2   3 -1542.8  1     0          1
```

```r
lrtest(nb_null, mnb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2  33 -1528.6 31 28.26     0.6077
```

```r
lrtest(nb_null, nb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2  32 -1528.6 30 28.26     0.5567
```

```r
# Only state vars

mnb_only_state_formula <- update(mnb_null_formula, . ~ . + log_pop + log_wpn_crim + log_drug_crim +
                             log_bribe_vic + log_nbus + comp_index + law_index)

mnb_only_state <- model_failsafe(mnb_only_state_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb_only_state)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3102.5 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.52103    0.11847   -4.40  1.1e-05 ***
log_pop        0.11870    0.20618    0.58     0.56    
log_wpn_crim   0.00526    0.21963    0.02     0.98    
log_drug_crim  0.05078    0.14172    0.36     0.72    
log_bribe_vic -0.26411    0.33360   -0.79     0.43    
log_nbus       0.53235    0.50082    1.06     0.29    
comp_index     0.00865    0.01304    0.66     0.51    
law_index     -0.00202    0.01060   -0.19     0.85    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.794e-08 0.0001339

Negative binomial dispersion parameter: 0.050279 (std. err.: 0.0038068)

Log-likelihood: -1541.25 
```

```r
get_glmmadmb(mnb_only_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -1541.25

$df
[1] 10

$AIC
[1] 3102.5

$BIC
[1] 3160.382

$alpha
[1] 19.88902

$var_j
[1] 3.21736e-16

$ICC
[1] 1.617656e-17
```

```r
nb_only_state_formula <- update(mnb_only_state_formula, . ~ . - (1 | state))

nb_only_state <- model_failsafe(nb_only_state_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = FALSE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(nb_only_state)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3100.5 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.52102    0.11847   -4.40  1.1e-05 ***
log_pop        0.11872    0.20618    0.58     0.56    
log_wpn_crim   0.00523    0.21963    0.02     0.98    
log_drug_crim  0.05081    0.14172    0.36     0.72    
log_bribe_vic -0.26424    0.33360   -0.79     0.43    
log_nbus       0.53249    0.50082    1.06     0.29    
comp_index     0.00865    0.01304    0.66     0.51    
law_index     -0.00203    0.01060   -0.19     0.85    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.050212 (std. err.: 0.0038007)

Log-likelihood: -1541.25 
```

```r
get_glmmadmb(nb_only_state)
```

```
$model
extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index

$logLik
[1] -1541.25

$df
[1] 9

$AIC
[1] 3100.5

$BIC
[1] 3152.594

$alpha
[1] 19.91556
```

```r
lrtest(nb_only_state, mnb_only_state)
```

```
Likelihood ratio test

Model 1: extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index
Model 2: extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   9 -1541.2                    
2  10 -1541.2  1     0          1
```

```r
vif(nb_only_state)
```

```
      log_pop  log_wpn_crim log_drug_crim log_bribe_vic      log_nbus 
     1.022845      1.042606      1.016935      1.027481      1.030969 
   comp_index     law_index 
     1.030346      1.028394 
```

```r
lm_only_state <- update(lm1, nb_only_state_formula)

vif(lm_only_state)
```

```
      log_pop  log_wpn_crim log_drug_crim log_bribe_vic      log_nbus 
     2.389957      4.193113      2.693174      1.896340      1.661691 
   comp_index     law_index 
     1.225665      1.862741 
```

```r
## No controls

mnb_noc_formula <- update(mnb_final_formula, . ~ . - log_pop - log_nbus - comp_index -
                      law_index)

mnb_noc <- model_failsafe(mnb_noc_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -ndi 60000' had
status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmADMB::glmmadmb(formula = formula, data = data, family = family, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mnb_noc)
```

```
Error in summary(mnb_noc): object 'mnb_noc' not found
```

```r
get_glmmadmb(mnb_noc)
```

```
Error in get_glmmadmb(mnb_noc): object 'mnb_noc' not found
```

```r
vif(mnb_noc)
```

```
Error in vif(mnb_noc): object 'mnb_noc' not found
```

```r
vif(mnb_noc)^2
```

```
Error in vif(mnb_noc): object 'mnb_noc' not found
```

```r
lm_noc <- update(lm1, update(mnb_noc_formula, . ~ . - (1 | state)))

vif(lm_noc)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.009756  1        1.004866
yearsquant    1.039845  4        1.004896
subsector     1.067621 15        1.002183
size          1.023590  3        1.003894
log_wpn_crim  2.011163  1        1.418155
log_bribe_vic 1.017012  1        1.008470
log_drug_crim 1.999303  1        1.413967
```

```r
screenreg(list("MNB" = mnb_final,
               "No state vars" = mnb1,
               "Only state vars" = mnb_only_state,
               "No controls" = mnb_noc,
               "Null" = mnb_null,
               "NB null" = nb_null))
```

```
Error in "list" %in% class(l)[1]: object 'mnb_noc' not found
```

```r
## Including deaths

mnb_f_deaths_formula <- update(mnb_final_formula, . ~ . + log_deaths)

mnb_f_deaths <- model_failsafe(mnb_f_deaths_formula,
                      data = enve_model,
                      family = "nbinom", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mnb_f_deaths)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -6.02e-02   5.72e-01   -0.11     0.92  
bribes                  -1.02e-02   1.04e-01   -0.10     0.92  
yearsquant(8,16]         1.90e-01   3.19e-01    0.59     0.55  
yearsquant(16,25]       -1.33e-01   3.03e-01   -0.44     0.66  
yearsquant(25,34]        3.46e-01   3.12e-01    1.11     0.27  
yearsquant(34,43]        1.42e-01   3.18e-01    0.45     0.66  
subsectorMining         -1.32e+00   1.03e+00   -1.28     0.20  
subsectorConstruction    5.60e-02   5.73e-01    0.10     0.92  
subsectorManufacturing  -2.58e-01   5.55e-01   -0.47     0.64  
subsectorWholesale      -4.69e-01   6.90e-01   -0.68     0.50  
subsectorTransport      -6.01e-01   6.50e-01   -0.92     0.36  
subsectorMedia          -7.40e-02   8.76e-01   -0.08     0.93  
subsectorFinance         4.41e-02   9.27e-01    0.05     0.96  
subsectorReal estate    -1.01e+00   9.39e-01   -1.08     0.28  
subsectorProf. services  1.12e+00   8.88e-01    1.27     0.21  
subsectorMaintenance    -6.04e-01   6.21e-01   -0.97     0.33  
subsectorEducation       3.01e-01   9.39e-01    0.32     0.75  
subsectorHealth         -4.76e-01   5.64e-01   -0.84     0.40  
subsectorLeisure         4.79e-01   8.92e-01    0.54     0.59  
subsectorHotelsRestBar  -4.40e-01   5.69e-01   -0.77     0.44  
subsectorOther          -3.75e+01   5.64e+05    0.00     1.00  
sizeMedium              -1.90e-01   2.78e-01   -0.68     0.49  
sizeSmall               -4.59e-01   2.79e-01   -1.64     0.10  
sizeMicro               -6.50e-01   2.79e-01   -2.33     0.02 *
log_pop                  1.18e-01   2.27e-01    0.52     0.60  
log_wpn_crim            -2.76e-01   2.85e-01   -0.97     0.33  
log_bribe_vic           -2.03e-01   3.33e-01   -0.61     0.54  
log_nbus                 4.76e-01   5.51e-01    0.86     0.39  
log_drug_crim            3.41e-02   1.49e-01    0.23     0.82  
comp_index               1.21e-02   1.40e-02    0.86     0.39  
law_index               -2.10e-03   1.20e-02   -0.18     0.86  
log_deaths               2.96e-01   2.31e-01    1.28     0.20  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.184e-09 4.674e-05

Negative binomial dispersion parameter: 0.053848 (std. err.: 0.0041309)

Log-likelihood: -1527.82 
```

```r
get_glmmadmb(mnb_f_deaths)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths

$logLik
[1] -1527.82

$df
[1] 34

$AIC
[1] 3123.64

$BIC
[1] 3320.439

$alpha
[1] 18.57079

$var_j
[1] 4.771603e-18

$ICC
[1] 2.569413e-19
```

```r
lrtest(mnb_final, mnb_f_deaths)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths
  #Df  LogLik Df Chisq Pr(>Chisq)
1  33 -1528.6                    
2  34 -1527.8  1  1.62     0.2031
```

```r
vif(mnb_f_deaths)
```

```
                      GVIF Df GVIF^(1/(2*Df))
bribes        1.239735e+07  1    3.520987e+03
yearsquant    0.000000e+00  4    0.000000e+00
subsector     0.000000e+00 15    0.000000e+00
size          5.933232e-02  3    6.245232e-01
log_pop       1.078507e-01  1    3.284063e-01
log_wpn_crim  1.072005e-01  1    3.274149e-01
log_bribe_vic 1.059027e-01  1    3.254269e-01
log_nbus      1.059389e-01  1    3.254826e-01
log_drug_crim 1.077033e-01  1    3.281818e-01
comp_index    1.093675e-01  1    3.307075e-01
law_index     5.720319e-02  1    2.391719e-01
log_deaths    3.839142e-10  1    1.959373e-05
```

```r
lm_f_deaths <- lm(update(nb_final_formula, . ~ . +  log_deaths),
                  data = enve_model)
vif(lm_f_deaths)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.012971  1        1.006465
yearsquant    1.048784  4        1.005972
subsector     1.100805 15        1.003207
size          1.029187  3        1.004806
log_pop       2.788567  1        1.669900
log_wpn_crim  6.906841  1        2.628087
log_bribe_vic 1.933923  1        1.390655
log_nbus      1.680738  1        1.296433
log_drug_crim 2.716880  1        1.648296
comp_index    1.356298  1        1.164602
law_index     2.100865  1        1.449436
log_deaths    6.728775  1        2.593988
```

```r
screenreg(list("MNB" = mnb_final,
          "With deaths" = mnb_f_deaths),
          single.row = TRUE)
```

```

=======================================================================
                         MNB                     With deaths           
-----------------------------------------------------------------------
(Intercept)                 -0.14      (0.57)       -0.06      (0.57)  
bribes                      -0.02      (0.10)       -0.01      (0.10)  
yearsquant(8,16]             0.21      (0.32)        0.19      (0.32)  
yearsquant(16,25]           -0.13      (0.30)       -0.13      (0.30)  
yearsquant(25,34]            0.42      (0.31)        0.35      (0.31)  
yearsquant(34,43]            0.15      (0.32)        0.14      (0.32)  
subsectorMining             -1.45      (1.03)       -1.32      (1.03)  
subsectorConstruction        0.05      (0.57)        0.06      (0.57)  
subsectorManufacturing      -0.21      (0.55)       -0.26      (0.55)  
subsectorWholesale          -0.52      (0.69)       -0.47      (0.69)  
subsectorTransport          -0.51      (0.65)       -0.60      (0.65)  
subsectorMedia              -0.03      (0.88)       -0.07      (0.88)  
subsectorFinance             0.05      (0.93)        0.04      (0.93)  
subsectorReal estate        -1.17      (0.93)       -1.01      (0.94)  
subsectorProf. services      1.18      (0.89)        1.12      (0.89)  
subsectorMaintenance        -0.55      (0.62)       -0.60      (0.62)  
subsectorEducation           0.38      (0.94)        0.30      (0.94)  
subsectorHealth             -0.49      (0.56)       -0.48      (0.56)  
subsectorLeisure             0.41      (0.89)        0.48      (0.89)  
subsectorHotelsRestBar      -0.42      (0.57)       -0.44      (0.57)  
subsectorOther             -29.25 (380660.00)      -37.48 (563740.00)  
sizeMedium                  -0.16      (0.28)       -0.19      (0.28)  
sizeSmall                   -0.45      (0.28)       -0.46      (0.28)  
sizeMicro                   -0.63      (0.28) *     -0.65      (0.28) *
log_pop                      0.22      (0.21)        0.12      (0.23)  
log_wpn_crim                -0.04      (0.22)       -0.28      (0.29)  
log_bribe_vic               -0.23      (0.34)       -0.20      (0.33)  
log_nbus                     0.48      (0.55)        0.48      (0.55)  
log_drug_crim                0.03      (0.15)        0.03      (0.15)  
comp_index                   0.01      (0.01)        0.01      (0.01)  
law_index                   -0.01      (0.01)       -0.00      (0.01)  
log_deaths                                           0.30      (0.23)  
-----------------------------------------------------------------------
Variance: state              0.00                    0.00              
Dispersion: parameter        0.05                    0.05              
Dispersion: SD               0.00                    0.00              
AIC                       3123.26                 3123.64              
BIC                       3314.27                 3320.44              
Log Likelihood           -1528.63                -1527.82              
Num. obs.                 2412                    2412                 
Num. groups: state          32                      32                 
=======================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Vs Poisson


mp_final <- model_failsafe(mnb_final_formula,
                      data = enve_model,
                      family = "poisson", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mp_final)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 5667.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.20e+00   3.84e-01   -5.73    1e-08 ***
bribes                  -1.60e-01   6.00e-02   -2.66  0.00777 ** 
yearsquant(8,16]         2.91e-01   1.14e-01    2.55  0.01073 *  
yearsquant(16,25]       -3.18e-01   1.37e-01   -2.32  0.02057 *  
yearsquant(25,34]       -1.08e-01   1.44e-01   -0.75  0.45165    
yearsquant(34,43]       -6.13e-02   1.32e-01   -0.46  0.64327    
subsectorMining          1.02e+00   5.05e-01    2.02  0.04343 *  
subsectorConstruction    9.11e-01   3.79e-01    2.40  0.01630 *  
subsectorManufacturing   8.18e-01   3.70e-01    2.21  0.02701 *  
subsectorWholesale       8.48e-01   4.05e-01    2.09  0.03647 *  
subsectorTransport       2.14e-01   4.28e-01    0.50  0.61693    
subsectorMedia           8.90e-01   4.69e-01    1.90  0.05786 .  
subsectorFinance         1.34e+00   4.52e-01    2.97  0.00301 ** 
subsectorReal estate     8.79e-01   4.70e-01    1.87  0.06137 .  
subsectorProf. services  1.51e+00   4.39e-01    3.44  0.00058 ***
subsectorMaintenance     7.22e-01   3.93e-01    1.84  0.06629 .  
subsectorEducation       7.78e-01   4.90e-01    1.59  0.11235    
subsectorHealth          9.56e-01   3.69e-01    2.59  0.00955 ** 
subsectorLeisure         1.48e+00   4.30e-01    3.45  0.00056 ***
subsectorHotelsRestBar   8.64e-01   3.64e-01    2.37  0.01757 *  
subsectorOther          -2.15e+01   4.52e+04    0.00  0.99962    
sizeMedium              -3.02e-01   1.24e-01   -2.44  0.01473 *  
sizeSmall               -2.18e-01   1.26e-01   -1.74  0.08262 .  
sizeMicro               -6.06e-04   1.14e-01   -0.01  0.99576    
log_pop                  1.69e-01   1.58e-01    1.07  0.28484    
log_wpn_crim            -1.31e-01   1.55e-01   -0.85  0.39711    
log_bribe_vic            8.97e-02   2.42e-01    0.37  0.71083    
log_nbus                -3.44e-01   4.11e-01   -0.84  0.40263    
log_drug_crim           -5.74e-02   1.12e-01   -0.51  0.60706    
comp_index               2.43e-02   1.03e-02    2.35  0.01860 *  
law_index               -4.69e-03   8.07e-03   -0.58  0.56110    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance StdDev
(Intercept)   0.1216 0.3487


Log-likelihood: -2801.65 
```

```r
get_glmmadmb(mp_final)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -2801.65

$df
[1] 32

$AIC
[1] 5667.3

$BIC
[1] 5852.523

$alpha
numeric(0)

$var_j
[1] 0.01478899

$ICC
numeric(0)
```

```r
lrtest(mp_final, mnb_final)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)    
1  32 -2801.7                        
2  33 -1528.6  1  2546  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
screenreg(list("MNB" = mnb_final, "M Poisson" = mp_final), single.row = TRUE)
```

```

========================================================================
                         MNB                     M Poisson              
------------------------------------------------------------------------
(Intercept)                 -0.14      (0.57)       -2.20     (0.38) ***
bribes                      -0.02      (0.10)       -0.16     (0.06) ** 
yearsquant(8,16]             0.21      (0.32)        0.29     (0.11) *  
yearsquant(16,25]           -0.13      (0.30)       -0.32     (0.14) *  
yearsquant(25,34]            0.42      (0.31)       -0.11     (0.14)    
yearsquant(34,43]            0.15      (0.32)       -0.06     (0.13)    
subsectorMining             -1.45      (1.03)        1.02     (0.50) *  
subsectorConstruction        0.05      (0.57)        0.91     (0.38) *  
subsectorManufacturing      -0.21      (0.55)        0.82     (0.37) *  
subsectorWholesale          -0.52      (0.69)        0.85     (0.41) *  
subsectorTransport          -0.51      (0.65)        0.21     (0.43)    
subsectorMedia              -0.03      (0.88)        0.89     (0.47)    
subsectorFinance             0.05      (0.93)        1.34     (0.45) ** 
subsectorReal estate        -1.17      (0.93)        0.88     (0.47)    
subsectorProf. services      1.18      (0.89)        1.51     (0.44) ***
subsectorMaintenance        -0.55      (0.62)        0.72     (0.39)    
subsectorEducation           0.38      (0.94)        0.78     (0.49)    
subsectorHealth             -0.49      (0.56)        0.96     (0.37) ** 
subsectorLeisure             0.41      (0.89)        1.48     (0.43) ***
subsectorHotelsRestBar      -0.42      (0.57)        0.86     (0.36) *  
subsectorOther             -29.25 (380660.00)      -21.55 (45168.00)    
sizeMedium                  -0.16      (0.28)       -0.30     (0.12) *  
sizeSmall                   -0.45      (0.28)       -0.22     (0.13)    
sizeMicro                   -0.63      (0.28) *     -0.00     (0.11)    
log_pop                      0.22      (0.21)        0.17     (0.16)    
log_wpn_crim                -0.04      (0.22)       -0.13     (0.15)    
log_bribe_vic               -0.23      (0.34)        0.09     (0.24)    
log_nbus                     0.48      (0.55)       -0.34     (0.41)    
log_drug_crim                0.03      (0.15)       -0.06     (0.11)    
comp_index                   0.01      (0.01)        0.02     (0.01) *  
law_index                   -0.01      (0.01)       -0.00     (0.01)    
------------------------------------------------------------------------
Variance: state              0.00                    0.12               
Dispersion: parameter        0.05                                       
Dispersion: SD               0.00                                       
AIC                       3123.26                 5667.30               
BIC                       3314.27                 5852.52               
Log Likelihood           -1528.63                -2801.65               
Num. obs.                 2412                    2412                  
Num. groups: state          32                      32                  
========================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

## Zero-inflated models

Multilevel zero inflated using glmmadmb
- fully specified: MZINB vs MNB vs MZIP
- null: MZINB vs MNB
- Full and null: MZINB vs ZINB
- No state: MZINB vs MNB
- No bus: MZINB vs MNB


```r
mzinb_full <- glmmadmb(mnb_final_formula, data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzinb_full)
```

```

Call:
glmmadmb(formula = mnb_final_formula, data = enve_model, family = "nbinom", 
    zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.05e+00   7.27e-01    1.44   0.1500   
bribes                   1.92e-03   1.06e-01    0.02   0.9856   
yearsquant(8,16]         1.66e-01   3.00e-01    0.55   0.5797   
yearsquant(16,25]       -1.13e-01   2.86e-01   -0.40   0.6920   
yearsquant(25,34]        5.02e-01   3.00e-01    1.67   0.0947 . 
yearsquant(34,43]        1.82e-01   3.04e-01    0.60   0.5494   
subsectorMining         -1.54e+00   9.95e-01   -1.54   0.1228   
subsectorConstruction   -2.15e-02   5.64e-01   -0.04   0.9696   
subsectorManufacturing  -3.37e-01   5.48e-01   -0.61   0.5387   
subsectorWholesale      -6.44e-01   6.59e-01   -0.98   0.3286   
subsectorTransport      -5.64e-01   6.28e-01   -0.90   0.3693   
subsectorMedia           1.42e-01   9.00e-01    0.16   0.8750   
subsectorFinance        -4.12e-02   8.77e-01   -0.05   0.9625   
subsectorReal estate    -1.32e+00   9.03e-01   -1.46   0.1434   
subsectorProf. services  9.89e-01   8.27e-01    1.20   0.2317   
subsectorMaintenance    -6.29e-01   6.07e-01   -1.04   0.3005   
subsectorEducation       3.82e-01   9.35e-01    0.41   0.6827   
subsectorHealth         -5.88e-01   5.52e-01   -1.06   0.2874   
subsectorLeisure         3.50e-01   8.45e-01    0.41   0.6790   
subsectorHotelsRestBar  -4.86e-01   5.56e-01   -0.87   0.3822   
subsectorOther          -1.71e+01   1.98e+03   -0.01   0.9931   
sizeMedium              -1.26e-01   2.67e-01   -0.47   0.6375   
sizeSmall               -4.75e-01   2.64e-01   -1.80   0.0725 . 
sizeMicro               -7.06e-01   2.67e-01   -2.64   0.0082 **
log_pop                  2.14e-01   2.01e-01    1.06   0.2870   
log_wpn_crim             5.77e-03   2.19e-01    0.03   0.9789   
log_bribe_vic           -2.47e-01   3.25e-01   -0.76   0.4469   
log_nbus                 4.72e-01   5.22e-01    0.90   0.3656   
log_drug_crim            7.27e-03   1.47e-01    0.05   0.9605   
comp_index               2.12e-03   1.34e-02    0.16   0.8746   
law_index               -4.38e-03   1.15e-02   -0.38   0.7029   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.515e-09 5.015e-05

Negative binomial dispersion parameter: 0.20966 (std. err.: 0.12758)
Zero-inflation: 0.67006  (std. err.:  0.13962 )

Log-likelihood: -1527.79 
```

```r
get_glmmadmb(mzinb_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1527.79

$df
[1] 34

$AIC
[1] 3123.58

$BIC
[1] 3320.379

$alpha
[1] 4.769627

$var_j
[1] 6.325728e-18

$ICC
[1] 1.326252e-18
```

```r
lrtest(mnb_final, mzinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  33 -1528.6                    
2  34 -1527.8  1  1.68     0.1949
```

```r
mzip_full <- glmmadmb(mnb_final_formula, data = enve_model,
                       family = "poisson", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzip_full)
```

```

Call:
glmmadmb(formula = mnb_final_formula, data = enve_model, family = "poisson", 
    zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3620.2 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              2.08e+00   2.05e-01   10.14  < 2e-16 ***
bribes                   5.01e-02   4.46e-02    1.12  0.26153    
yearsquant(8,16]         3.98e-02   1.22e-01    0.33  0.74423    
yearsquant(16,25]       -1.49e-01   1.21e-01   -1.23  0.21943    
yearsquant(25,34]        4.88e-02   1.20e-01    0.40  0.68556    
yearsquant(34,43]       -5.75e-02   1.20e-01   -0.48  0.63211    
subsectorMining         -5.07e-01   4.62e-01   -1.10  0.27213    
subsectorConstruction   -5.72e-01   1.84e-01   -3.10  0.00192 ** 
subsectorManufacturing  -1.21e+00   1.81e-01   -6.69  2.2e-11 ***
subsectorWholesale      -1.76e+00   2.66e-01   -6.62  3.5e-11 ***
subsectorTransport      -7.87e-01   2.16e-01   -3.65  0.00026 ***
subsectorMedia           6.18e-01   2.51e-01    2.46  0.01388 *  
subsectorFinance        -4.53e-01   2.70e-01   -1.68  0.09317 .  
subsectorReal estate    -8.24e-01   3.95e-01   -2.09  0.03690 *  
subsectorProf. services -1.28e-01   2.13e-01   -0.60  0.54925    
subsectorMaintenance    -1.04e+00   2.24e-01   -4.62  3.8e-06 ***
subsectorEducation      -1.76e+00   4.30e-01   -4.09  4.3e-05 ***
subsectorHealth         -1.03e+00   1.89e-01   -5.46  4.8e-08 ***
subsectorLeisure        -8.02e-02   2.93e-01   -0.27  0.78454    
subsectorHotelsRestBar  -7.16e-01   1.80e-01   -3.98  7.0e-05 ***
subsectorOther          -2.43e+01   5.90e+04    0.00  0.99967    
sizeMedium               2.32e-01   1.11e-01    2.10  0.03555 *  
sizeSmall               -9.10e-02   1.11e-01   -0.82  0.41045    
sizeMicro               -5.75e-01   1.18e-01   -4.87  1.1e-06 ***
log_pop                  1.70e-01   1.57e-01    1.08  0.28046    
log_wpn_crim             3.53e-01   1.63e-01    2.17  0.03023 *  
log_bribe_vic            4.23e-01   2.51e-01    1.68  0.09259 .  
log_nbus                -6.53e-01   4.24e-01   -1.54  0.12328    
log_drug_crim           -2.29e-01   1.17e-01   -1.96  0.05028 .  
comp_index              -1.25e-02   1.02e-02   -1.23  0.21797    
law_index                1.14e-02   8.37e-03    1.37  0.17222    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance StdDev
(Intercept)   0.1351 0.3676

Zero-inflation: 0.87243  (std. err.:  0.0075204 )

Log-likelihood: -1777.09 
```

```r
get_glmmadmb(mzip_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1777.09

$df
[1] 33

$AIC
[1] 3620.18

$BIC
[1] 3811.191

$alpha
numeric(0)

$var_j
[1] 0.01826012

$ICC
numeric(0)
```

```r
lrtest(mzip_full, mzinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)    
1  33 -1777.1                        
2  34 -1527.8  1 498.6  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
zinb_full_formula <- update(mnb_final_formula, . ~ . - (1 | state))

zinb_full <- glmmadmb(zinb_full_formula, data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(zinb_full)
```

```

Call:
glmmadmb(formula = zinb_full_formula, data = enve_model, family = "nbinom", 
    zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3121.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.05e+00   7.27e-01    1.44   0.1500   
bribes                   1.92e-03   1.06e-01    0.02   0.9856   
yearsquant(8,16]         1.66e-01   3.00e-01    0.55   0.5797   
yearsquant(16,25]       -1.13e-01   2.86e-01   -0.40   0.6920   
yearsquant(25,34]        5.02e-01   3.00e-01    1.67   0.0947 . 
yearsquant(34,43]        1.82e-01   3.04e-01    0.60   0.5494   
subsectorMining         -1.54e+00   9.95e-01   -1.54   0.1228   
subsectorConstruction   -2.15e-02   5.64e-01   -0.04   0.9696   
subsectorManufacturing  -3.37e-01   5.48e-01   -0.61   0.5387   
subsectorWholesale      -6.44e-01   6.59e-01   -0.98   0.3286   
subsectorTransport      -5.64e-01   6.28e-01   -0.90   0.3693   
subsectorMedia           1.42e-01   9.00e-01    0.16   0.8750   
subsectorFinance        -4.12e-02   8.77e-01   -0.05   0.9625   
subsectorReal estate    -1.32e+00   9.03e-01   -1.46   0.1434   
subsectorProf. services  9.89e-01   8.26e-01    1.20   0.2316   
subsectorMaintenance    -6.29e-01   6.07e-01   -1.04   0.3005   
subsectorEducation       3.82e-01   9.35e-01    0.41   0.6827   
subsectorHealth         -5.88e-01   5.52e-01   -1.06   0.2874   
subsectorLeisure         3.50e-01   8.45e-01    0.41   0.6790   
subsectorHotelsRestBar  -4.86e-01   5.56e-01   -0.87   0.3822   
subsectorOther          -1.71e+01   1.98e+03   -0.01   0.9931   
sizeMedium              -1.26e-01   2.67e-01   -0.47   0.6375   
sizeSmall               -4.75e-01   2.64e-01   -1.80   0.0725 . 
sizeMicro               -7.06e-01   2.67e-01   -2.64   0.0082 **
log_pop                  2.14e-01   2.01e-01    1.06   0.2870   
log_wpn_crim             5.77e-03   2.19e-01    0.03   0.9789   
log_bribe_vic           -2.47e-01   3.25e-01   -0.76   0.4469   
log_nbus                 4.72e-01   5.22e-01    0.90   0.3656   
log_drug_crim            7.27e-03   1.47e-01    0.05   0.9605   
comp_index               2.12e-03   1.34e-02    0.16   0.8746   
law_index               -4.38e-03   1.15e-02   -0.38   0.7029   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.20932 (std. err.: 0.12736)
Zero-inflation: 0.67006  (std. err.:  0.13962 )

Log-likelihood: -1527.79 
```

```r
get_glmmadmb(zinb_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1527.79

$df
[1] 33

$AIC
[1] 3121.58

$BIC
[1] 3312.591

$alpha
[1] 4.777374
```

```r
lrtest(zinb_full, mzinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  33 -1527.8                    
2  34 -1527.8  1     0          1
```

```r
screenreg(list("MNB" = mnb_final,
               "MZINB" = mzinb_full,
               "MZIP" = mzip_full,
               "ZINB" = zinb_full),
          single.row = TRUE)
```

```

========================================================================================================================
                           MNB                     MZINB                  MZIP                     ZINB                 
------------------------------------------------------------------------------------------------------------------------
(Intercept)                   -0.14      (0.57)        1.05    (0.73)         2.08     (0.21) ***      1.05    (0.73)   
bribes                        -0.02      (0.10)        0.00    (0.11)         0.05     (0.04)          0.00    (0.11)   
yearsquant(8,16]               0.21      (0.32)        0.17    (0.30)         0.04     (0.12)          0.17    (0.30)   
yearsquant(16,25]             -0.13      (0.30)       -0.11    (0.29)        -0.15     (0.12)         -0.11    (0.29)   
yearsquant(25,34]              0.42      (0.31)        0.50    (0.30)         0.05     (0.12)          0.50    (0.30)   
yearsquant(34,43]              0.15      (0.32)        0.18    (0.30)        -0.06     (0.12)          0.18    (0.30)   
subsectorMining               -1.45      (1.03)       -1.54    (1.00)        -0.51     (0.46)         -1.54    (1.00)   
subsectorConstruction          0.05      (0.57)       -0.02    (0.56)        -0.57     (0.18) **      -0.02    (0.56)   
subsectorManufacturing        -0.21      (0.55)       -0.34    (0.55)        -1.21     (0.18) ***     -0.34    (0.55)   
subsectorWholesale            -0.52      (0.69)       -0.64    (0.66)        -1.76     (0.27) ***     -0.64    (0.66)   
subsectorTransport            -0.51      (0.65)       -0.56    (0.63)        -0.79     (0.22) ***     -0.56    (0.63)   
subsectorMedia                -0.03      (0.88)        0.14    (0.90)         0.62     (0.25) *        0.14    (0.90)   
subsectorFinance               0.05      (0.93)       -0.04    (0.88)        -0.45     (0.27)         -0.04    (0.88)   
subsectorReal estate          -1.17      (0.93)       -1.32    (0.90)        -0.82     (0.39) *       -1.32    (0.90)   
subsectorProf. services        1.18      (0.89)        0.99    (0.83)        -0.13     (0.21)          0.99    (0.83)   
subsectorMaintenance          -0.55      (0.62)       -0.63    (0.61)        -1.04     (0.22) ***     -0.63    (0.61)   
subsectorEducation             0.38      (0.94)        0.38    (0.93)        -1.76     (0.43) ***      0.38    (0.93)   
subsectorHealth               -0.49      (0.56)       -0.59    (0.55)        -1.03     (0.19) ***     -0.59    (0.55)   
subsectorLeisure               0.41      (0.89)        0.35    (0.84)        -0.08     (0.29)          0.35    (0.84)   
subsectorHotelsRestBar        -0.42      (0.57)       -0.49    (0.56)        -0.72     (0.18) ***     -0.49    (0.56)   
subsectorOther               -29.25 (380660.00)      -17.08 (1982.30)       -24.32 (59037.00)        -17.07 (1975.60)   
sizeMedium                    -0.16      (0.28)       -0.13    (0.27)         0.23     (0.11) *       -0.13    (0.27)   
sizeSmall                     -0.45      (0.28)       -0.47    (0.26)        -0.09     (0.11)         -0.47    (0.26)   
sizeMicro                     -0.63      (0.28) *     -0.71    (0.27) **     -0.57     (0.12) ***     -0.71    (0.27) **
log_pop                        0.22      (0.21)        0.21    (0.20)         0.17     (0.16)          0.21    (0.20)   
log_wpn_crim                  -0.04      (0.22)        0.01    (0.22)         0.35     (0.16) *        0.01    (0.22)   
log_bribe_vic                 -0.23      (0.34)       -0.25    (0.32)         0.42     (0.25)         -0.25    (0.32)   
log_nbus                       0.48      (0.55)        0.47    (0.52)        -0.65     (0.42)          0.47    (0.52)   
log_drug_crim                  0.03      (0.15)        0.01    (0.15)        -0.23     (0.12)          0.01    (0.15)   
comp_index                     0.01      (0.01)        0.00    (0.01)        -0.01     (0.01)          0.00    (0.01)   
law_index                     -0.01      (0.01)       -0.00    (0.01)         0.01     (0.01)         -0.00    (0.01)   
------------------------------------------------------------------------------------------------------------------------
Variance: state                0.00                    0.00                   0.14                                      
Dispersion: parameter          0.05                    0.21                                            0.21             
Dispersion: SD                 0.00                    0.13                                            0.13             
AIC                         3123.26                 3123.58                3620.18                  3121.58             
BIC                         3314.27                 3320.38                3811.19                  3312.59             
Log Likelihood             -1528.63                -1527.79               -1777.09                 -1527.79             
Num. obs.                   2412                    2412                   2412                     2412                
Num. groups: state            32                      32                     32                                         
Zero inflation: parameter                              0.67                   0.87                     0.67             
Zero inflation: SD                                     0.14                   0.01                     0.14             
Num. groups:                                                                                           1                
========================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# Dropping state vars

mzinb_no_state <- glmmadmb(count_formula, data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzinb_no_state)
```

```

Call:
glmmadmb(formula = count_formula, data = enve_model, family = "nbinom", 
    zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3112.7 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)              9.55e-01   7.64e-01    1.25    0.211  
bribes                   5.23e-03   1.03e-01    0.05    0.959  
yearsquant(8,16]         2.01e-01   3.01e-01    0.67    0.503  
yearsquant(16,25]       -9.76e-02   2.82e-01   -0.35    0.729  
yearsquant(25,34]        5.04e-01   2.95e-01    1.71    0.088 .
yearsquant(34,43]        2.17e-01   2.99e-01    0.73    0.468  
subsectorMining         -1.57e+00   9.93e-01   -1.58    0.115  
subsectorConstruction   -5.26e-02   5.55e-01   -0.09    0.925  
subsectorManufacturing  -3.64e-01   5.42e-01   -0.67    0.502  
subsectorWholesale      -7.68e-01   6.61e-01   -1.16    0.246  
subsectorTransport      -6.69e-01   6.27e-01   -1.07    0.286  
subsectorMedia           2.77e-01   8.92e-01    0.31    0.756  
subsectorFinance        -1.39e-01   8.75e-01   -0.16    0.873  
subsectorReal estate    -1.50e+00   8.89e-01   -1.69    0.092 .
subsectorProf. services  9.25e-01   8.25e-01    1.12    0.262  
subsectorMaintenance    -7.36e-01   5.97e-01   -1.23    0.218  
subsectorEducation      -1.76e-02   8.83e-01   -0.02    0.984  
subsectorHealth         -6.47e-01   5.56e-01   -1.16    0.245  
subsectorLeisure         1.34e-01   8.31e-01    0.16    0.872  
subsectorHotelsRestBar  -5.10e-01   5.56e-01   -0.92    0.359  
subsectorOther          -1.93e+01   5.79e+03    0.00    0.997  
sizeMedium              -1.55e-01   2.68e-01   -0.58    0.564  
sizeSmall               -4.99e-01   2.65e-01   -1.88    0.060 .
sizeMicro               -6.63e-01   2.65e-01   -2.51    0.012 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.562e-09 5.062e-05

Negative binomial dispersion parameter: 0.18208 (std. err.: 0.11881)
Zero-inflation: 0.63769  (std. err.:  0.17138 )

Log-likelihood: -1529.33 
```

```r
get_glmmadmb(mzinb_no_state)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -1529.33

$df
[1] 27

$AIC
[1] 3112.66

$BIC
[1] 3268.942

$alpha
[1] 5.492091

$var_j
[1] 6.563844e-18

$ICC
[1] 1.195145e-18
```

```r
lrtest(mzinb_no_state, mzinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  27 -1529.3                    
2  34 -1527.8  7  3.08     0.8775
```

```r
# Dropping bus vars

mzinb_only_state <- glmmadmb(mnb_only_state_formula, data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzinb_only_state)
```

```

Call:
glmmadmb(formula = mnb_only_state_formula, data = enve_model, 
    family = "nbinom", zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3104.5 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.52104    0.11847   -4.40  1.1e-05 ***
log_pop        0.11877    0.20618    0.58     0.56    
log_wpn_crim   0.00524    0.21963    0.02     0.98    
log_drug_crim  0.05079    0.14172    0.36     0.72    
log_bribe_vic -0.26420    0.33360   -0.79     0.43    
log_nbus       0.53230    0.50082    1.06     0.29    
comp_index     0.00865    0.01304    0.66     0.51    
law_index     -0.00202    0.01060   -0.19     0.85    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 7.091e-09 8.421e-05

Negative binomial dispersion parameter: 0.050279 (std. err.: 0.0038068)
Zero-inflation: 1e-06  (std. err.:  2.4032e-06 )

Log-likelihood: -1541.25 
```

```r
get_glmmadmb(mzinb_only_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -1541.25

$df
[1] 11

$AIC
[1] 3104.5

$BIC
[1] 3168.17

$alpha
[1] 19.88902

$var_j
[1] 5.027661e-17

$ICC
[1] 2.527858e-18
```

```r
lrtest(mzinb_only_state, mzinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  11 -1541.2                    
2  34 -1527.8 23 26.92     0.2594
```

```r
screenreg(list("MZINB Full" = mzinb_full,
               "No state" = mzinb_no_state,
               "No bus" = mzinb_only_state),
          single.row = TRUE)
```

```

===========================================================================================
                           MZINB Full             No state              No bus             
-------------------------------------------------------------------------------------------
(Intercept)                    1.05    (0.73)         0.95    (0.76)       -0.52 (0.12) ***
bribes                         0.00    (0.11)         0.01    (0.10)                       
yearsquant(8,16]               0.17    (0.30)         0.20    (0.30)                       
yearsquant(16,25]             -0.11    (0.29)        -0.10    (0.28)                       
yearsquant(25,34]              0.50    (0.30)         0.50    (0.30)                       
yearsquant(34,43]              0.18    (0.30)         0.22    (0.30)                       
subsectorMining               -1.54    (1.00)        -1.57    (0.99)                       
subsectorConstruction         -0.02    (0.56)        -0.05    (0.56)                       
subsectorManufacturing        -0.34    (0.55)        -0.36    (0.54)                       
subsectorWholesale            -0.64    (0.66)        -0.77    (0.66)                       
subsectorTransport            -0.56    (0.63)        -0.67    (0.63)                       
subsectorMedia                 0.14    (0.90)         0.28    (0.89)                       
subsectorFinance              -0.04    (0.88)        -0.14    (0.87)                       
subsectorReal estate          -1.32    (0.90)        -1.50    (0.89)                       
subsectorProf. services        0.99    (0.83)         0.93    (0.83)                       
subsectorMaintenance          -0.63    (0.61)        -0.74    (0.60)                       
subsectorEducation             0.38    (0.93)        -0.02    (0.88)                       
subsectorHealth               -0.59    (0.55)        -0.65    (0.56)                       
subsectorLeisure               0.35    (0.84)         0.13    (0.83)                       
subsectorHotelsRestBar        -0.49    (0.56)        -0.51    (0.56)                       
subsectorOther               -17.08 (1982.30)       -19.27 (5794.50)                       
sizeMedium                    -0.13    (0.27)        -0.15    (0.27)                       
sizeSmall                     -0.47    (0.26)        -0.50    (0.26)                       
sizeMicro                     -0.71    (0.27) **     -0.66    (0.26) *                     
log_pop                        0.21    (0.20)                               0.12 (0.21)    
log_wpn_crim                   0.01    (0.22)                               0.01 (0.22)    
log_bribe_vic                 -0.25    (0.32)                              -0.26 (0.33)    
log_nbus                       0.47    (0.52)                               0.53 (0.50)    
log_drug_crim                  0.01    (0.15)                               0.05 (0.14)    
comp_index                     0.00    (0.01)                               0.01 (0.01)    
law_index                     -0.00    (0.01)                              -0.00 (0.01)    
-------------------------------------------------------------------------------------------
Variance: state                0.00                   0.00                  0.00           
Dispersion: parameter          0.21                   0.18                  0.05           
Dispersion: SD                 0.13                   0.12                  0.00           
Zero inflation: parameter      0.67                   0.64                  0.00           
Zero inflation: SD             0.14                   0.17                  0.00           
AIC                         3123.58                3112.66               3104.50           
BIC                         3320.38                3268.94               3168.17           
Log Likelihood             -1527.79               -1529.33              -1541.25           
Num. obs.                   2412                   2412                  2412              
Num. groups: state            32                     32                    32              
===========================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# include deaths

mzinb_deaths <- glmmadmb(update(mnb_final_formula, . ~ . + log_deaths), 
                         data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzinb_deaths)
```

```

Call:
glmmadmb(formula = update(mnb_final_formula, . ~ . + log_deaths), 
    data = enve_model, family = "nbinom", zeroInflation = TRUE, 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 3123.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.19e+00   7.06e-01    1.68   0.0928 . 
bribes                   1.93e-02   1.07e-01    0.18   0.8576   
yearsquant(8,16]         1.47e-01   2.97e-01    0.49   0.6206   
yearsquant(16,25]       -1.09e-01   2.84e-01   -0.39   0.7000   
yearsquant(25,34]        4.30e-01   3.04e-01    1.42   0.1562   
yearsquant(34,43]        1.81e-01   3.03e-01    0.60   0.5498   
subsectorMining         -1.40e+00   1.00e+00   -1.40   0.1628   
subsectorConstruction   -2.34e-02   5.62e-01   -0.04   0.9668   
subsectorManufacturing  -4.00e-01   5.48e-01   -0.73   0.4656   
subsectorWholesale      -5.94e-01   6.62e-01   -0.90   0.3696   
subsectorTransport      -6.72e-01   6.29e-01   -1.07   0.2856   
subsectorMedia           1.10e-01   9.00e-01    0.12   0.9025   
subsectorFinance        -6.13e-02   8.74e-01   -0.07   0.9440   
subsectorReal estate    -1.16e+00   9.10e-01   -1.27   0.2038   
subsectorProf. services  9.09e-01   8.16e-01    1.11   0.2657   
subsectorMaintenance    -7.00e-01   6.06e-01   -1.15   0.2481   
subsectorEducation       2.85e-01   9.30e-01    0.31   0.7593   
subsectorHealth         -5.82e-01   5.51e-01   -1.06   0.2910   
subsectorLeisure         4.14e-01   8.41e-01    0.49   0.6227   
subsectorHotelsRestBar  -5.08e-01   5.54e-01   -0.92   0.3591   
subsectorOther          -1.69e+01   1.71e+03   -0.01   0.9921   
sizeMedium              -1.58e-01   2.66e-01   -0.59   0.5530   
sizeSmall               -4.86e-01   2.63e-01   -1.85   0.0641 . 
sizeMicro               -7.36e-01   2.65e-01   -2.78   0.0054 **
log_pop                  1.05e-01   2.15e-01    0.49   0.6252   
log_wpn_crim            -2.37e-01   2.76e-01   -0.86   0.3902   
log_bribe_vic           -2.19e-01   3.19e-01   -0.69   0.4917   
log_nbus                 4.61e-01   5.24e-01    0.88   0.3787   
log_drug_crim            1.07e-02   1.43e-01    0.07   0.9405   
comp_index               6.94e-03   1.38e-02    0.50   0.6137   
law_index               -5.34e-04   1.16e-02   -0.05   0.9631   
log_deaths               3.10e-01   2.19e-01    1.42   0.1568   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.484e-09 4.984e-05

Negative binomial dispersion parameter: 0.22685 (std. err.: 0.12982)
Zero-inflation: 0.6861  (std. err.:  0.12188 )

Log-likelihood: -1526.81 
```

```r
get_glmmadmb(mzinb_deaths)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths

$logLik
[1] -1526.81

$df
[1] 35

$AIC
[1] 3123.62

$BIC
[1] 3326.207

$alpha
[1] 4.408199

$var_j
[1] 6.170256e-18

$ICC
[1] 1.399723e-18
```

```r
lrtest(mzinb_full, mzinb_deaths)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths
  #Df  LogLik Df Chisq Pr(>Chisq)
1  34 -1527.8                    
2  35 -1526.8  1  1.96     0.1615
```

```r
## Null models: MZINB vs MNB

mzinb_null <- glmmadmb(update(mnb_final_formula, . ~ 1 + (1 | state)), 
                         data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(mzinb_null)
```

```

Call:
glmmadmb(formula = update(mnb_final_formula, . ~ 1 + (1 | state)), 
    data = enve_model, family = "nbinom", zeroInflation = TRUE, 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 3093.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance   StdDev
(Intercept) 2.061e-09 4.54e-05

Negative binomial dispersion parameter: 0.049891 (std. err.: 0.0037719)
Zero-inflation: 1e-06  (std. err.:  5.0492e-08 )

Log-likelihood: -1542.75 
```

```r
get_glmmadmb(mzinb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1542.75

$df
[1] 4

$AIC
[1] 3093.5

$BIC
[1] 3116.653

$alpha
[1] 20.0437

$var_j
[1] 4.248958e-18

$ICC
[1] 2.119847e-19
```

```r
lrtest(mzinb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state)
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   4 -1542.8                    
2   3 -1542.8 -1  0.02     0.8875
```

```r
## Null models: MZINB vs ZINB

zinb_null <- glmmadmb(update(mnb_final_formula, . ~ 1), 
                         data = enve_model,
                       family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(zinb_null)
```

```

Call:
glmmadmb(formula = update(mnb_final_formula, . ~ 1), data = enve_model, 
    family = "nbinom", zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3091.5 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5642     0.0951   -5.93    3e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.049825 (std. err.: 0.0037671)
Zero-inflation: 2.7586e-06  (std. err.:  0.0017606 )

Log-likelihood: -1542.75 
```

```r
get_glmmadmb(zinb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1542.75

$df
[1] 3

$AIC
[1] 3091.5

$BIC
[1] 3108.865

$alpha
[1] 20.07025
```

```r
lrtest(zinb_null, mzinb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   3 -1542.8                    
2   4 -1542.8  1     0          1
```

```r
screenreg(list("MNB Null" = mnb_null,
               "MZINB Null" = mzinb_null,
               "ZINB Null" = zinb_null),
          single.row = TRUE)
```

```

========================================================================================
                           MNB Null             MZINB Null           ZINB Null          
----------------------------------------------------------------------------------------
(Intercept)                   -0.56 (0.10) ***     -0.56 (0.10) ***     -0.56 (0.10) ***
----------------------------------------------------------------------------------------
Variance: state                0.00                 0.00                                
Dispersion: parameter          0.05                 0.05                 0.05           
Dispersion: SD                 0.00                 0.00                 0.00           
AIC                         3091.52              3093.50              3091.50           
BIC                         3108.88              3116.65              3108.86           
Log Likelihood             -1542.76             -1542.75             -1542.75           
Num. obs.                   2412                 2412                 2412              
Num. groups: state            32                   32                                   
Zero inflation: parameter                           0.00                 0.00           
Zero inflation: SD                                  0.00                 0.00           
Num. groups:                                                             1              
========================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

Single level ZINB using glmmadmb
- fully specified: ZINB vs NB vs ZIP
- null: ZINB vs NB
- No state: ZINB vs NB
- No bus: ZINB vs NB


```r
lrtest(nb_final, zinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -1528.6                    
2  33 -1527.8  1  1.68     0.1949
```

```r
zip_full <- glmmadmb(update(mnb_final_formula, . ~ . - (1 | state)), 
                         data = enve_model,
                       family = "poisson", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")
    
summary(zip_full)
```

```

Call:
glmmadmb(formula = update(mnb_final_formula, . ~ . - (1 | state)), 
    data = enve_model, family = "poisson", zeroInflation = TRUE, 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 3644.1 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              2.33e+00   1.75e-01   13.31  < 2e-16 ***
bribes                   1.03e-01   3.87e-02    2.67  0.00760 ** 
yearsquant(8,16]         8.18e-02   1.17e-01    0.70  0.48590    
yearsquant(16,25]       -9.75e-02   1.27e-01   -0.77  0.44407    
yearsquant(25,34]        4.93e-01   1.15e-01    4.27  2.0e-05 ***
yearsquant(34,43]        1.59e-01   1.18e-01    1.34  0.18002    
subsectorMining         -1.08e+00   4.27e-01   -2.53  0.01141 *  
subsectorConstruction   -8.12e-01   1.67e-01   -4.85  1.2e-06 ***
subsectorManufacturing  -8.67e-01   1.68e-01   -5.17  2.4e-07 ***
subsectorWholesale      -1.82e+00   2.58e-01   -7.06  1.6e-12 ***
subsectorTransport      -7.65e-01   1.98e-01   -3.87  0.00011 ***
subsectorMedia          -1.46e-01   3.59e-01   -0.41  0.68419    
subsectorFinance        -3.45e-01   2.48e-01   -1.39  0.16414    
subsectorReal estate    -1.46e+00   3.74e-01   -3.89  9.8e-05 ***
subsectorProf. services  4.17e-01   1.81e-01    2.30  0.02162 *  
subsectorMaintenance    -9.61e-01   1.99e-01   -4.83  1.4e-06 ***
subsectorEducation      -1.84e+00   4.14e-01   -4.45  8.7e-06 ***
subsectorHealth         -1.10e+00   1.72e-01   -6.42  1.4e-10 ***
subsectorLeisure        -3.14e-01   2.81e-01   -1.12  0.26457    
subsectorHotelsRestBar  -6.79e-01   1.63e-01   -4.17  3.0e-05 ***
subsectorOther          -1.76e+01   2.39e+03   -0.01  0.99413    
sizeMedium              -6.36e-02   9.66e-02   -0.66  0.51020    
sizeSmall               -5.43e-01   1.05e-01   -5.17  2.4e-07 ***
sizeMicro               -8.53e-01   1.08e-01   -7.93  2.2e-15 ***
log_pop                  2.07e-01   7.13e-02    2.90  0.00372 ** 
log_wpn_crim             2.58e-01   7.92e-02    3.26  0.00113 ** 
log_bribe_vic           -4.63e-04   1.16e-01    0.00  0.99681    
log_nbus                 7.99e-02   1.94e-01    0.41  0.67980    
log_drug_crim           -1.08e-01   5.27e-02   -2.05  0.04077 *  
comp_index              -2.15e-02   4.65e-03   -4.62  3.8e-06 ***
law_index                4.80e-03   3.84e-03    1.25  0.21190    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Zero-inflation: 0.87542  (std. err.:  0.0072134 )

Log-likelihood: -1790.04 
```

```r
get_glmmadmb(zip_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -1790.04

$df
[1] 32

$AIC
[1] 3644.08

$BIC
[1] 3829.303

$alpha
numeric(0)
```

```r
lrtest(zip_full, zinb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)    
1  32 -1790.0                        
2  33 -1527.8  1 524.5  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
lrtest(nb_null, zinb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ 1
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1542.8                    
2   3 -1542.8  1  0.02     0.8875
```

```r
# no state

zinb_no_state <- glmmadmb(nb1_formula, data = enve_model,
                          family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(zinb_no_state)
```

```

Call:
glmmadmb(formula = nb1_formula, data = enve_model, family = "nbinom", 
    zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3110.7 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)              9.55e-01   7.64e-01    1.25    0.211  
bribes                   5.23e-03   1.03e-01    0.05    0.959  
yearsquant(8,16]         2.01e-01   3.01e-01    0.67    0.503  
yearsquant(16,25]       -9.76e-02   2.82e-01   -0.35    0.729  
yearsquant(25,34]        5.04e-01   2.95e-01    1.71    0.088 .
yearsquant(34,43]        2.17e-01   2.99e-01    0.73    0.468  
subsectorMining         -1.57e+00   9.93e-01   -1.58    0.115  
subsectorConstruction   -5.26e-02   5.55e-01   -0.09    0.925  
subsectorManufacturing  -3.64e-01   5.42e-01   -0.67    0.502  
subsectorWholesale      -7.68e-01   6.61e-01   -1.16    0.246  
subsectorTransport      -6.69e-01   6.27e-01   -1.07    0.286  
subsectorMedia           2.77e-01   8.92e-01    0.31    0.756  
subsectorFinance        -1.39e-01   8.75e-01   -0.16    0.873  
subsectorReal estate    -1.50e+00   8.89e-01   -1.69    0.092 .
subsectorProf. services  9.25e-01   8.25e-01    1.12    0.262  
subsectorMaintenance    -7.36e-01   5.97e-01   -1.23    0.218  
subsectorEducation      -1.76e-02   8.83e-01   -0.02    0.984  
subsectorHealth         -6.47e-01   5.56e-01   -1.16    0.245  
subsectorLeisure         1.34e-01   8.31e-01    0.16    0.872  
subsectorHotelsRestBar  -5.10e-01   5.56e-01   -0.92    0.359  
subsectorOther          -1.93e+01   5.95e+03    0.00    0.997  
sizeMedium              -1.55e-01   2.68e-01   -0.58    0.564  
sizeSmall               -4.99e-01   2.65e-01   -1.88    0.060 .
sizeMicro               -6.63e-01   2.65e-01   -2.51    0.012 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.18179 (std. err.: 0.1186)
Zero-inflation: 0.63769  (std. err.:  0.17138 )

Log-likelihood: -1529.33 
```

```r
get_glmmadmb(zinb_no_state)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size

$logLik
[1] -1529.33

$df
[1] 26

$AIC
[1] 3110.66

$BIC
[1] 3261.154

$alpha
[1] 5.500853
```

```r
lrtest(nb1, zinb_no_state)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size
Model 2: extortions ~ bribes + yearsquant + subsector + size
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -1530.0                    
2  26 -1529.3  1   1.3     0.2542
```

```r
# no business level measurements

zinb_only_state <- glmmadmb(nb_only_state_formula, data = enve_model,
                          family = "nbinom", zeroInflation = TRUE,
                       admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                       extra.args = "-ndi 60000")

summary(zinb_only_state)
```

```

Call:
glmmadmb(formula = nb_only_state_formula, data = enve_model, 
    family = "nbinom", zeroInflation = TRUE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 3102.5 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.52104    0.11849   -4.40  1.1e-05 ***
log_pop        0.11876    0.20618    0.58     0.56    
log_wpn_crim   0.00524    0.21963    0.02     0.98    
log_drug_crim  0.05079    0.14172    0.36     0.72    
log_bribe_vic -0.26418    0.33360   -0.79     0.43    
log_nbus       0.53226    0.50082    1.06     0.29    
comp_index     0.00865    0.01304    0.66     0.51    
law_index     -0.00202    0.01060   -0.19     0.85    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.050212 (std. err.: 0.0038026)
Zero-inflation: 3.1148e-06  (std. err.:  0.0021407 )

Log-likelihood: -1541.25 
```

```r
get_glmmadmb(zinb_only_state)
```

```
$model
extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index

$logLik
[1] -1541.25

$df
[1] 10

$AIC
[1] 3102.5

$BIC
[1] 3160.382

$alpha
[1] 19.91556
```

```r
lrtest(nb_only_state, zinb_only_state)
```

```
Likelihood ratio test

Model 1: extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index
Model 2: extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   9 -1541.2                    
2  10 -1541.2  1     0          1
```


Single level ZINB using pscl
- Different equation for zero inflation
- count: business level, zero: state level
- count: state level, zero: business level



```r
zi_formula <- as.formula(extortions ~
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index | 1)

zinb_pscl_simple_full <- zeroinfl(zi_formula, data = enve_model,
                                  dist = "negbin")
summary(zinb_pscl_simple_full)
```

```

Call:
zeroinfl(formula = zi_formula, data = enve_model, dist = "negbin")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-0.2444 -0.2357 -0.2313 -0.2243 14.3304 

Count model coefficients (negbin with log link):
                          Estimate Std. Error z value Pr(>|z|)   
(Intercept)               1.046364   0.726997   1.439  0.15007   
bribes                    0.001913   0.106226   0.018  0.98563   
yearsquant(8,16]          0.165909   0.299624   0.554  0.57977   
yearsquant(16,25]        -0.113125   0.285598  -0.396  0.69203   
yearsquant(25,34]         0.501900   0.300361   1.671  0.09472 . 
yearsquant(34,43]         0.181857   0.303706   0.599  0.54931   
subsectorMining          -1.535888   0.995333  -1.543  0.12281   
subsectorConstruction    -0.021291   0.564165  -0.038  0.96990   
subsectorManufacturing   -0.336824   0.548086  -0.615  0.53885   
subsectorWholesale       -0.643907   0.659275  -0.977  0.32872   
subsectorTransport       -0.564064   0.628449  -0.898  0.36943   
subsectorMedia            0.141933   0.900295   0.158  0.87473   
subsectorFinance         -0.041063   0.876785  -0.047  0.96265   
subsectorReal estate     -1.321084   0.902969  -1.463  0.14346   
subsectorProf. services   0.988791   0.826481   1.196  0.23155   
subsectorMaintenance     -0.628790   0.607425  -1.035  0.30059   
subsectorEducation        0.382226   0.934666   0.409  0.68258   
subsectorHealth          -0.587530   0.552401  -1.064  0.28751   
subsectorLeisure          0.349838   0.844866   0.414  0.67882   
subsectorHotelsRestBar   -0.485647   0.555945  -0.874  0.38236   
subsectorOther          -12.763549 229.147626  -0.056  0.95558   
sizeMedium               -0.125741   0.266852  -0.471  0.63750   
sizeSmall                -0.474629   0.264256  -1.796  0.07248 . 
sizeMicro                -0.705938   0.266934  -2.645  0.00818 **
log_pop                   0.214231   0.201204   1.065  0.28699   
log_wpn_crim              0.005772   0.218631   0.026  0.97894   
log_bribe_vic            -0.246929   0.324660  -0.761  0.44691   
log_nbus                  0.472379   0.522148   0.905  0.36563   
log_drug_crim             0.007272   0.146721   0.050  0.96047   
comp_index                0.002113   0.013401   0.158  0.87469   
law_index                -0.004383   0.011491  -0.381  0.70290   
Log(theta)               -1.563850   0.608421  -2.570  0.01016 * 

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)
(Intercept)   0.7085     0.6315   1.122    0.262
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.2093 
Number of iterations in BFGS optimization: 40 
Log-likelihood: -1528 on 33 Df
```

```r
lrtest(nb_final, zinb_pscl_simple_full)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmmadmb", updated model is of class "zeroinfl"
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | 1
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  32 -1528.6                     
2  33 -1527.8  1 1.6725     0.1959
```

```r
nb_mass_final <- glm.nb(nb_final_formula, data = enve_model)

summary(nb_mass_final)
```

```

Call:
glm.nb(formula = nb_final_formula, data = enve_model, init.theta = 0.05351940287, 
    link = log)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6859  -0.5261  -0.4920  -0.4517   2.4588  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.376e-01  5.670e-01  -0.243   0.8082  
bribes                  -2.198e-02  8.917e-02  -0.246   0.8053  
yearsquant(8,16]         2.075e-01  2.968e-01   0.699   0.4846  
yearsquant(16,25]       -1.291e-01  2.910e-01  -0.444   0.6573  
yearsquant(25,34]        4.161e-01  2.833e-01   1.469   0.1419  
yearsquant(34,43]        1.466e-01  2.962e-01   0.495   0.6206  
subsectorMining         -1.450e+00  1.031e+00  -1.406   0.1597  
subsectorConstruction    4.923e-02  5.523e-01   0.089   0.9290  
subsectorManufacturing  -2.121e-01  5.390e-01  -0.393   0.6940  
subsectorWholesale      -5.241e-01  6.593e-01  -0.795   0.4266  
subsectorTransport      -5.142e-01  6.384e-01  -0.806   0.4205  
subsectorMedia          -2.938e-02  8.372e-01  -0.035   0.9720  
subsectorFinance         5.147e-02  9.006e-01   0.057   0.9544  
subsectorReal estate    -1.170e+00  9.205e-01  -1.272   0.2035  
subsectorProf. services  1.183e+00  8.703e-01   1.359   0.1742  
subsectorMaintenance    -5.482e-01  5.935e-01  -0.924   0.3557  
subsectorEducation       3.831e-01  8.842e-01   0.433   0.6648  
subsectorHealth         -4.923e-01  5.506e-01  -0.894   0.3712  
subsectorLeisure         4.126e-01  8.625e-01   0.478   0.6324  
subsectorHotelsRestBar  -4.228e-01  5.534e-01  -0.764   0.4449  
subsectorOther          -3.654e+01  2.122e+07   0.000   1.0000  
sizeMedium              -1.593e-01  2.644e-01  -0.602   0.5469  
sizeSmall               -4.502e-01  2.688e-01  -1.675   0.0939 .
sizeMicro               -6.302e-01  2.661e-01  -2.369   0.0179 *
log_pop                  2.192e-01  1.968e-01   1.114   0.2654  
log_wpn_crim            -4.296e-02  1.900e-01  -0.226   0.8212  
log_bribe_vic           -2.324e-01  2.985e-01  -0.778   0.4363  
log_nbus                 4.815e-01  5.044e-01   0.955   0.3398  
log_drug_crim            3.085e-02  1.404e-01   0.220   0.8261  
comp_index               6.952e-03  1.221e-02   0.569   0.5691  
law_index               -5.889e-03  9.963e-03  -0.591   0.5545  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(0.0535) family taken to be 1)

    Null deviance: 729.09  on 2411  degrees of freedom
Residual deviance: 699.94  on 2381  degrees of freedom
AIC: 3121.3

Number of Fisher Scoring iterations: 1

              Theta:  0.05352 
          Std. Err.:  0.00410 

 2 x log-likelihood:  -3057.26800 
```

```r
lrtest(nb_mass_final, zinb_pscl_simple_full)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "negbin", updated model is of class "zeroinfl"
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | 1
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  32 -1528.6                     
2  33 -1527.8  1 1.6803     0.1949
```

```r
vuong(nb_mass_final, zinb_pscl_simple_full)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A  p-value
Raw                 -0.49595424 model2 > model1 0.309963
AIC-corrected        0.09437841 model1 > model2 0.462404
BIC-corrected        1.80286354 model1 > model2 0.035705
```

```r
zi_formula <- as.formula(extortions ~
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index |
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index)

zinb_pscl_full <- zeroinfl(zi_formula, data = enve_model,
                                  dist = "negbin")
```

```
Error in solve.default(as.matrix(fit$hessian)): system is computationally singular: reciprocal condition number = 7.14215e-44
```

```r
summary(zinb_pscl_full)
```

```
Error in summary(zinb_pscl_full): object 'zinb_pscl_full' not found
```

```r
lrtest(nb_final, zinb_pscl_full)
```

```
Error in lrtest.default(nb_final, zinb_pscl_full): object 'zinb_pscl_full' not found
```

```r
lrtest(zinb_pscl_simple_full ,zinb_pscl_full)
```

```
Error in lrtest.default(zinb_pscl_simple_full, zinb_pscl_full): object 'zinb_pscl_full' not found
```

```r
lrtest(nb_mass_final, zinb_pscl_full)
```

```
Error in lrtest.default(nb_mass_final, zinb_pscl_full): object 'zinb_pscl_full' not found
```

```r
vuong(nb_mass_final, zinb_pscl_full)
```

```
Error in vuong(nb_mass_final, zinb_pscl_full): object 'zinb_pscl_full' not found
```

```r
## Constant on either side of equation

zinb_pscl_nullc <- update(zinb_pscl_full, . ~ 1 | . )
```

```
Error in update(zinb_pscl_full, . ~ 1 | .): object 'zinb_pscl_full' not found
```

```r
summary(zinb_pscl_nullc)
```

```
Error in summary(zinb_pscl_nullc): object 'zinb_pscl_nullc' not found
```

```r
lrtest(zinb_pscl_nullc, zinb_pscl_full)
```

```
Error in lrtest(zinb_pscl_nullc, zinb_pscl_full): object 'zinb_pscl_nullc' not found
```

```r
vuong(zinb_pscl_nullc, zinb_pscl_full)
```

```
Error in vuong(zinb_pscl_nullc, zinb_pscl_full): object 'zinb_pscl_nullc' not found
```

```r
zinb_pscl_c_all_z_state <- update(zinb_pscl_full, . ~ . | . - bribes -
                             yearsquant -
                             subsector -
                             size )
```

```
Error in update(zinb_pscl_full, . ~ . | . - bribes - yearsquant - subsector - : object 'zinb_pscl_full' not found
```

```r
summary(zinb_pscl_c_all_z_state)
```

```
Error in summary(zinb_pscl_c_all_z_state): object 'zinb_pscl_c_all_z_state' not found
```

```r
lrtest(zinb_pscl_simple_full, zinb_pscl_c_all_z_state)
```

```
Error in lrtest.default(zinb_pscl_simple_full, zinb_pscl_c_all_z_state): object 'zinb_pscl_c_all_z_state' not found
```

```r
vuong(zinb_pscl_simple_full, zinb_pscl_c_all_z_state)
```

```
Error in vuong(zinb_pscl_simple_full, zinb_pscl_c_all_z_state): object 'zinb_pscl_c_all_z_state' not found
```

```r
zinb_pscl_c_state_l_z_all <- update(zinb_pscl_full, . ~ . - bribes -
                             yearsquant -
                             subsector -
                             size | . )
```

```
Error in update(zinb_pscl_full, . ~ . - bribes - yearsquant - subsector - : object 'zinb_pscl_full' not found
```

```r
summary(zinb_pscl_c_state_l_z_all)
```

```
Error in summary(zinb_pscl_c_state_l_z_all): object 'zinb_pscl_c_state_l_z_all' not found
```

```r
lrtest(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all)
```

```
Error in lrtest.default(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all): object 'zinb_pscl_c_state_l_z_all' not found
```

```r
vuong(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all)
```

```
Error in vuong(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all): object 'zinb_pscl_c_state_l_z_all' not found
```

```r
screenreg(list("ZINB Full" = zinb_pscl_full,
               "Count: Null" = zinb_pscl_nullc,
               "Zero: Null" = zinb_pscl_simple_full,
               "Zero: state" = zinb_pscl_c_all_z_state,
               "Count: state" = zinb_pscl_c_state_l_z_all),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'zinb_pscl_full' not found
```



## Hurdle models

First prepare data sets


```r
enve_trunc <- subset(enve_model, extortions > 0)
```

Multilevel Hurdle model using glmmadmb
- fully specified: MHNB vs MNB
- Full: MHNB vs HNB

Compare:
- Binary: state; Count: all vs. Full
- Binary: bus; Count: all vs. Full
- Binary: all; Count: bus vs. Full
- Binary: all; Count: state vs. Full



```r
binary_formula <- update(mnb_final_formula, extortion_victim ~ .)

# Full, equal variable, hurdle models

## Binary part first

mlogit_full <- model_failsafe(binary_formula, data = enve_model,
                        family = "binomial", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mlogit_full)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1794.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -2.03698    0.41416   -4.92  8.7e-07 ***
bribes                   -0.08012    0.07171   -1.12    0.264    
yearsquant(8,16]          0.16620    0.19217    0.86    0.387    
yearsquant(16,25]        -0.15750    0.19726   -0.80    0.425    
yearsquant(25,34]        -0.17361    0.19559   -0.89    0.375    
yearsquant(34,43]        -0.12664    0.20263   -0.62    0.532    
subsectorMining          -0.42876    0.82423   -0.52    0.603    
subsectorConstruction     0.27065    0.40634    0.67    0.505    
subsectorManufacturing    0.29557    0.39738    0.74    0.457    
subsectorWholesale        0.38996    0.46154    0.84    0.398    
subsectorTransport        0.02831    0.46944    0.06    0.952    
subsectorMedia           -0.48275    0.70651   -0.68    0.494    
subsectorFinance          0.41626    0.61160    0.68    0.496    
subsectorReal estate     -0.35440    0.71023   -0.50    0.618    
subsectorProf. services   0.90666    0.54843    1.65    0.098 .  
subsectorMaintenance      0.02076    0.43924    0.05    0.962    
subsectorEducation        0.08884    0.64992    0.14    0.891    
subsectorHealth           0.20113    0.40512    0.50    0.620    
subsectorLeisure          0.46179    0.58177    0.79    0.427    
subsectorHotelsRestBar    0.12410    0.40930    0.30    0.762    
subsectorOther          -11.71727  310.76000   -0.04    0.970    
sizeMedium               -0.24009    0.18453   -1.30    0.193    
sizeSmall                -0.07392    0.18023   -0.41    0.682    
sizeMicro                -0.06044    0.17733   -0.34    0.733    
log_pop                   0.08219    0.13154    0.62    0.532    
log_wpn_crim             -0.08686    0.12787   -0.68    0.497    
log_bribe_vic             0.09021    0.20395    0.44    0.658    
log_nbus                  0.05549    0.34501    0.16    0.872    
log_drug_crim             0.00677    0.09563    0.07    0.944    
comp_index                0.01397    0.00832    1.68    0.093 .  
law_index                -0.00255    0.00684   -0.37    0.710    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 6.678e-08 0.0002584


Log-likelihood: -865.143 
```

```r
get_glmmadmb(mlogit_full)
```

```
$model
extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index

$logLik
[1] -865.143

$df
[1] 32

$AIC
[1] 1794.286

$BIC
[1] 1979.509

$alpha
numeric(0)

$var_j
[1] 4.459702e-15

$ICC
numeric(0)
```

```r
## Count part

mtnb_full <- glmmadmb(mnb_final_formula, data = enve_trunc,
                      family = "truncnbinom",
                      admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                      extra.args = "-ndi 60000")
```

```
Warning in eval(substitute(expr), data, enclos = parent.frame()): sd.est
not defined for this family
```

```r
summary(mtnb_full)
```

```

Call:
glmmadmb(formula = mnb_final_formula, data = enve_trunc, family = "truncnbinom", 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 1358 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.28519    0.79201    1.62   0.1047   
bribes                   0.16463    0.15270    1.08   0.2810   
yearsquant(8,16]         0.02363    0.35219    0.07   0.9465   
yearsquant(16,25]       -0.09263    0.34966   -0.26   0.7911   
yearsquant(25,34]        0.72542    0.37368    1.94   0.0522 . 
yearsquant(34,43]        0.29004    0.38210    0.76   0.4478   
subsectorMining         -1.62832    1.39590   -1.17   0.2434   
subsectorConstruction   -0.33924    0.71085   -0.48   0.6332   
subsectorManufacturing  -0.89297    0.68955   -1.30   0.1953   
subsectorWholesale      -1.07813    0.84741   -1.27   0.2033   
subsectorTransport      -0.76396    0.79499   -0.96   0.3366   
subsectorMedia           0.85346    1.26640    0.67   0.5004   
subsectorFinance        -0.41933    1.03790   -0.40   0.6862   
subsectorReal estate    -1.65662    1.24040   -1.34   0.1817   
subsectorProf. services  0.37822    0.92212    0.41   0.6817   
subsectorMaintenance    -1.00344    0.77841   -1.29   0.1974   
subsectorEducation       0.21770    1.17110    0.19   0.8525   
subsectorHealth         -0.97355    0.70034   -1.39   0.1645   
subsectorLeisure         0.19694    0.98658    0.20   0.8418   
subsectorHotelsRestBar  -0.74874    0.70878   -1.06   0.2908   
sizeMedium               0.01452    0.32057    0.05   0.9639   
sizeSmall               -0.50381    0.32108   -1.57   0.1166   
sizeMicro               -0.95416    0.31573   -3.02   0.0025 **
log_pop                  0.14016    0.24384    0.57   0.5654   
log_wpn_crim             0.30115    0.27796    1.08   0.2786   
log_bribe_vic           -0.26862    0.41292   -0.65   0.5153   
log_nbus                 0.34186    0.61819    0.55   0.5803   
log_drug_crim           -0.13175    0.18020   -0.73   0.4647   
comp_index              -0.01620    0.01584   -1.02   0.3065   
law_index                0.00606    0.01414    0.43   0.6682   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=285, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 1.436e-08 0.0001198

Negative binomial dispersion parameter: 0.1974 (std. err.: 0.11235)

Log-likelihood: -646.979 
```

```r
get_glmmadmb(mtnb_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -646.979

$df
[1] 32

$AIC
[1] 1357.958

$BIC
[1] 1474.838

$alpha
[1] 5.065856

$var_j
[1] 2.06267e-16

$ICC
[1] 4.071711e-17
```

```r
### Compare joint IC with Full MNB and ZINB

compare_hurdle2other(mlogit_full, mtnb_full, mnb_final, mzinb_full)
```

```
          Hurdle   Negbin.      ZINB
logLik -1512.122 -1528.630 -1527.790
df        64.000    33.000    34.000
AIC     3152.244  3123.260  3123.580
BIC     3454.346  3314.271  3320.379
```

```r
screenreg(list("MNB" = mnb_final,
               "MZINB" = mzinb_full,
               "Logit" = mlogit_full,
               "Trunc. NB" = mtnb_full),
          single.row = TRUE)
```

```

=================================================================================================================
                           MNB                     MZINB                  Logit                 Trunc. NB        
-----------------------------------------------------------------------------------------------------------------
(Intercept)                   -0.14      (0.57)        1.05    (0.73)       -2.04   (0.41) ***     1.29 (0.79)   
bribes                        -0.02      (0.10)        0.00    (0.11)       -0.08   (0.07)         0.16 (0.15)   
yearsquant(8,16]               0.21      (0.32)        0.17    (0.30)        0.17   (0.19)         0.02 (0.35)   
yearsquant(16,25]             -0.13      (0.30)       -0.11    (0.29)       -0.16   (0.20)        -0.09 (0.35)   
yearsquant(25,34]              0.42      (0.31)        0.50    (0.30)       -0.17   (0.20)         0.73 (0.37)   
yearsquant(34,43]              0.15      (0.32)        0.18    (0.30)       -0.13   (0.20)         0.29 (0.38)   
subsectorMining               -1.45      (1.03)       -1.54    (1.00)       -0.43   (0.82)        -1.63 (1.40)   
subsectorConstruction          0.05      (0.57)       -0.02    (0.56)        0.27   (0.41)        -0.34 (0.71)   
subsectorManufacturing        -0.21      (0.55)       -0.34    (0.55)        0.30   (0.40)        -0.89 (0.69)   
subsectorWholesale            -0.52      (0.69)       -0.64    (0.66)        0.39   (0.46)        -1.08 (0.85)   
subsectorTransport            -0.51      (0.65)       -0.56    (0.63)        0.03   (0.47)        -0.76 (0.79)   
subsectorMedia                -0.03      (0.88)        0.14    (0.90)       -0.48   (0.71)         0.85 (1.27)   
subsectorFinance               0.05      (0.93)       -0.04    (0.88)        0.42   (0.61)        -0.42 (1.04)   
subsectorReal estate          -1.17      (0.93)       -1.32    (0.90)       -0.35   (0.71)        -1.66 (1.24)   
subsectorProf. services        1.18      (0.89)        0.99    (0.83)        0.91   (0.55)         0.38 (0.92)   
subsectorMaintenance          -0.55      (0.62)       -0.63    (0.61)        0.02   (0.44)        -1.00 (0.78)   
subsectorEducation             0.38      (0.94)        0.38    (0.93)        0.09   (0.65)         0.22 (1.17)   
subsectorHealth               -0.49      (0.56)       -0.59    (0.55)        0.20   (0.41)        -0.97 (0.70)   
subsectorLeisure               0.41      (0.89)        0.35    (0.84)        0.46   (0.58)         0.20 (0.99)   
subsectorHotelsRestBar        -0.42      (0.57)       -0.49    (0.56)        0.12   (0.41)        -0.75 (0.71)   
subsectorOther               -29.25 (380660.00)      -17.08 (1982.30)      -11.72 (310.76)                       
sizeMedium                    -0.16      (0.28)       -0.13    (0.27)       -0.24   (0.18)         0.01 (0.32)   
sizeSmall                     -0.45      (0.28)       -0.47    (0.26)       -0.07   (0.18)        -0.50 (0.32)   
sizeMicro                     -0.63      (0.28) *     -0.71    (0.27) **    -0.06   (0.18)        -0.95 (0.32) **
log_pop                        0.22      (0.21)        0.21    (0.20)        0.08   (0.13)         0.14 (0.24)   
log_wpn_crim                  -0.04      (0.22)        0.01    (0.22)       -0.09   (0.13)         0.30 (0.28)   
log_bribe_vic                 -0.23      (0.34)       -0.25    (0.32)        0.09   (0.20)        -0.27 (0.41)   
log_nbus                       0.48      (0.55)        0.47    (0.52)        0.06   (0.35)         0.34 (0.62)   
log_drug_crim                  0.03      (0.15)        0.01    (0.15)        0.01   (0.10)        -0.13 (0.18)   
comp_index                     0.01      (0.01)        0.00    (0.01)        0.01   (0.01)        -0.02 (0.02)   
law_index                     -0.01      (0.01)       -0.00    (0.01)       -0.00   (0.01)         0.01 (0.01)   
-----------------------------------------------------------------------------------------------------------------
Variance: state                0.00                    0.00                  0.00                  0.00          
Dispersion: parameter          0.05                    0.21                                        0.20          
Dispersion: SD                 0.00                    0.13                                        0.11          
AIC                         3123.26                 3123.58               1794.29               1357.96          
BIC                         3314.27                 3320.38               1979.51               1474.84          
Log Likelihood             -1528.63                -1527.79               -865.14               -646.98          
Num. obs.                   2412                    2412                  2412                   285             
Num. groups: state            32                      32                    32                    32             
Zero inflation: parameter                              0.67                                                      
Zero inflation: SD                                     0.14                                                      
=================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# Full, single level, equal equation hurdle

## Binary part first

logit_full_formula <- update(binary_formula, . ~ . - (1 | state))

logit_full <- model_failsafe(logit_full_formula, data = enve_model,
                        family = "binomial", multilevel = FALSE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(logit_full)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1792.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.04e+00   4.14e-01   -4.92  8.7e-07 ***
bribes                  -8.01e-02   7.17e-02   -1.12    0.264    
yearsquant(8,16]         1.66e-01   1.92e-01    0.87    0.387    
yearsquant(16,25]       -1.57e-01   1.97e-01   -0.80    0.425    
yearsquant(25,34]       -1.74e-01   1.96e-01   -0.89    0.375    
yearsquant(34,43]       -1.27e-01   2.03e-01   -0.62    0.532    
subsectorMining         -4.29e-01   8.24e-01   -0.52    0.603    
subsectorConstruction    2.71e-01   4.06e-01    0.67    0.505    
subsectorManufacturing   2.96e-01   3.97e-01    0.74    0.457    
subsectorWholesale       3.90e-01   4.62e-01    0.85    0.398    
subsectorTransport       2.83e-02   4.69e-01    0.06    0.952    
subsectorMedia          -4.83e-01   7.07e-01   -0.68    0.494    
subsectorFinance         4.16e-01   6.12e-01    0.68    0.496    
subsectorReal estate    -3.54e-01   7.10e-01   -0.50    0.618    
subsectorProf. services  9.07e-01   5.48e-01    1.65    0.098 .  
subsectorMaintenance     2.07e-02   4.39e-01    0.05    0.962    
subsectorEducation       8.89e-02   6.50e-01    0.14    0.891    
subsectorHealth          2.01e-01   4.05e-01    0.50    0.619    
subsectorLeisure         4.62e-01   5.82e-01    0.79    0.427    
subsectorHotelsRestBar   1.24e-01   4.09e-01    0.30    0.762    
subsectorOther          -1.52e+01   1.77e+03   -0.01    0.993    
sizeMedium              -2.40e-01   1.85e-01   -1.30    0.193    
sizeSmall               -7.39e-02   1.80e-01   -0.41    0.682    
sizeMicro               -6.04e-02   1.77e-01   -0.34    0.733    
log_pop                  8.22e-02   1.32e-01    0.62    0.532    
log_wpn_crim            -8.68e-02   1.28e-01   -0.68    0.497    
log_bribe_vic            9.02e-02   2.04e-01    0.44    0.658    
log_nbus                 5.55e-02   3.45e-01    0.16    0.872    
log_drug_crim            6.75e-03   9.56e-02    0.07    0.944    
comp_index               1.40e-02   8.32e-03    1.68    0.093 .  
law_index               -2.55e-03   6.84e-03   -0.37    0.710    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412

Log-likelihood: -865.143 
```

```r
get_glmmadmb(logit_full)
```

```
$model
extortion_victim ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -865.143

$df
[1] 31

$AIC
[1] 1792.286

$BIC
[1] 1971.721

$alpha
numeric(0)
```

```r
## Count part

tnb_full <- glmmadmb(update(mnb_final_formula, . ~ . - (1 | state)), 
                     data = enve_trunc,
                      family = "truncnbinom",
                      admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                      extra.args = "-ndi 60000")
```

```
Warning in eval(substitute(expr), data, enclos = parent.frame()): sd.est
not defined for this family
```

```r
summary(tnb_full)
```

```

Call:
glmmadmb(formula = update(mnb_final_formula, . ~ . - (1 | state)), 
    data = enve_trunc, family = "truncnbinom", admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1356 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.28514    0.79200    1.62   0.1047   
bribes                   0.16464    0.15270    1.08   0.2809   
yearsquant(8,16]         0.02359    0.35219    0.07   0.9466   
yearsquant(16,25]       -0.09267    0.34966   -0.27   0.7910   
yearsquant(25,34]        0.72540    0.37368    1.94   0.0522 . 
yearsquant(34,43]        0.29001    0.38210    0.76   0.4479   
subsectorMining         -1.62823    1.39590   -1.17   0.2434   
subsectorConstruction   -0.33914    0.71084   -0.48   0.6333   
subsectorManufacturing  -0.89290    0.68954   -1.29   0.1953   
subsectorWholesale      -1.07813    0.84739   -1.27   0.2033   
subsectorTransport      -0.76393    0.79497   -0.96   0.3366   
subsectorMedia           0.85359    1.26640    0.67   0.5003   
subsectorFinance        -0.41922    1.03790   -0.40   0.6863   
subsectorReal estate    -1.65648    1.24040   -1.34   0.1817   
subsectorProf. services  0.37828    0.92210    0.41   0.6816   
subsectorMaintenance    -1.00342    0.77839   -1.29   0.1974   
subsectorEducation       0.21774    1.17110    0.19   0.8525   
subsectorHealth         -0.97349    0.70033   -1.39   0.1645   
subsectorLeisure         0.19695    0.98655    0.20   0.8418   
subsectorHotelsRestBar  -0.74864    0.70876   -1.06   0.2908   
sizeMedium               0.01454    0.32058    0.05   0.9638   
sizeSmall               -0.50381    0.32108   -1.57   0.1166   
sizeMicro               -0.95415    0.31573   -3.02   0.0025 **
log_pop                  0.14014    0.24384    0.57   0.5655   
log_wpn_crim             0.30115    0.27797    1.08   0.2786   
log_bribe_vic           -0.26858    0.41292   -0.65   0.5154   
log_nbus                 0.34184    0.61819    0.55   0.5803   
log_drug_crim           -0.13175    0.18020   -0.73   0.4647   
comp_index              -0.01620    0.01584   -1.02   0.3066   
law_index                0.00606    0.01414    0.43   0.6682   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=285
Negative binomial dispersion parameter: 0.19709 (std. err.: 0.11216)

Log-likelihood: -646.979 
```

```r
get_glmmadmb(tnb_full)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index

$logLik
[1] -646.979

$df
[1] 31

$AIC
[1] 1355.958

$BIC
[1] 1469.185

$alpha
[1] 5.073824
```

```r
### Compare joint IC with Full NB and ZINB

compare_hurdle2other(logit_full, tnb_full, nb_final, zinb_full)
```

```
          Hurdle   Negbin.      ZINB
logLik -1512.122 -1528.630 -1527.790
df        62.000    32.000    33.000
AIC     3148.244  3121.260  3121.580
BIC     3440.906  3306.483  3312.591
```

```r
screenreg(list("NB" = nb_final,
               "ZINB" = zinb_full,
               "Logit" = logit_full,
               "Trunc. NB" = tnb_full),
          single.row = TRUE)
```

```

================================================================================================================
                           NB                    ZINB                   Logit                  Trunc. NB        
----------------------------------------------------------------------------------------------------------------
(Intercept)                   -0.14    (0.57)        1.05    (0.73)       -2.04    (0.41) ***     1.29 (0.79)   
bribes                        -0.02    (0.10)        0.00    (0.11)       -0.08    (0.07)         0.16 (0.15)   
yearsquant(8,16]               0.21    (0.32)        0.17    (0.30)        0.17    (0.19)         0.02 (0.35)   
yearsquant(16,25]             -0.13    (0.30)       -0.11    (0.29)       -0.16    (0.20)        -0.09 (0.35)   
yearsquant(25,34]              0.42    (0.31)        0.50    (0.30)       -0.17    (0.20)         0.73 (0.37)   
yearsquant(34,43]              0.15    (0.32)        0.18    (0.30)       -0.13    (0.20)         0.29 (0.38)   
subsectorMining               -1.45    (1.03)       -1.54    (1.00)       -0.43    (0.82)        -1.63 (1.40)   
subsectorConstruction          0.05    (0.57)       -0.02    (0.56)        0.27    (0.41)        -0.34 (0.71)   
subsectorManufacturing        -0.21    (0.55)       -0.34    (0.55)        0.30    (0.40)        -0.89 (0.69)   
subsectorWholesale            -0.52    (0.69)       -0.64    (0.66)        0.39    (0.46)        -1.08 (0.85)   
subsectorTransport            -0.51    (0.65)       -0.56    (0.63)        0.03    (0.47)        -0.76 (0.79)   
subsectorMedia                -0.03    (0.88)        0.14    (0.90)       -0.48    (0.71)         0.85 (1.27)   
subsectorFinance               0.05    (0.93)       -0.04    (0.88)        0.42    (0.61)        -0.42 (1.04)   
subsectorReal estate          -1.17    (0.93)       -1.32    (0.90)       -0.35    (0.71)        -1.66 (1.24)   
subsectorProf. services        1.18    (0.89)        0.99    (0.83)        0.91    (0.55)         0.38 (0.92)   
subsectorMaintenance          -0.55    (0.62)       -0.63    (0.61)        0.02    (0.44)        -1.00 (0.78)   
subsectorEducation             0.38    (0.94)        0.38    (0.93)        0.09    (0.65)         0.22 (1.17)   
subsectorHealth               -0.49    (0.56)       -0.59    (0.55)        0.20    (0.41)        -0.97 (0.70)   
subsectorLeisure               0.41    (0.89)        0.35    (0.84)        0.46    (0.58)         0.20 (0.99)   
subsectorHotelsRestBar        -0.42    (0.57)       -0.49    (0.56)        0.12    (0.41)        -0.75 (0.71)   
subsectorOther               -17.61 (2608.30)      -17.07 (1975.60)      -15.20 (1769.80)                       
sizeMedium                    -0.16    (0.28)       -0.13    (0.27)       -0.24    (0.18)         0.01 (0.32)   
sizeSmall                     -0.45    (0.28)       -0.47    (0.26)       -0.07    (0.18)        -0.50 (0.32)   
sizeMicro                     -0.63    (0.28) *     -0.71    (0.27) **    -0.06    (0.18)        -0.95 (0.32) **
log_pop                        0.22    (0.21)        0.21    (0.20)        0.08    (0.13)         0.14 (0.24)   
log_wpn_crim                  -0.04    (0.22)        0.01    (0.22)       -0.09    (0.13)         0.30 (0.28)   
log_bribe_vic                 -0.23    (0.34)       -0.25    (0.32)        0.09    (0.20)        -0.27 (0.41)   
log_nbus                       0.48    (0.55)        0.47    (0.52)        0.06    (0.35)         0.34 (0.62)   
log_drug_crim                  0.03    (0.15)        0.01    (0.15)        0.01    (0.10)        -0.13 (0.18)   
comp_index                     0.01    (0.01)        0.00    (0.01)        0.01    (0.01)        -0.02 (0.02)   
law_index                     -0.01    (0.01)       -0.00    (0.01)       -0.00    (0.01)         0.01 (0.01)   
----------------------------------------------------------------------------------------------------------------
Dispersion: parameter          0.05                  0.21                                         0.20          
Dispersion: SD                 0.00                  0.13                                         0.11          
AIC                         3121.26               3121.58               1792.29                1355.96          
BIC                         3306.48               3312.59               1971.72                1469.19          
Log Likelihood             -1528.63              -1527.79               -865.14                -646.98          
Num. obs.                   2412                  2412                  2412                    285             
Num. groups:                   1                     1                     1                      1             
Zero inflation: parameter                            0.67                                                       
Zero inflation: SD                                   0.14                                                       
================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Compare two hurdle modles: multilevel vs single level

hurdle_compare(joint_ic(logit_full, tnb_full),
               joint_ic(mlogit_full, mtnb_full),
               modnames = c("Single level", "Multilevel"))
```

```
       Single level Multilevel
logLik -1512.122    -1512.122 
df     62           64        
AIC    3148.244     3152.244  
BIC    3440.906     3454.346  
```

```r
# - Binary: state; Count: all vs. Full

mlogit_state_formula <- update(mnb_only_state_formula, extortion_victim ~ .)

mlogit_state <- model_failsafe(mlogit_state_formula, data = enve_model,
                         family = "binomial", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mlogit_state)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1766.6 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -2.00851    0.07924  -25.35   <2e-16 ***
log_pop        0.09366    0.13037    0.72     0.47    
log_wpn_crim  -0.10467    0.12664   -0.83     0.41    
log_drug_crim  0.02133    0.09519    0.22     0.82    
log_bribe_vic  0.07342    0.20184    0.36     0.72    
log_nbus       0.08736    0.34157    0.26     0.80    
comp_index     0.01240    0.00824    1.50     0.13    
law_index     -0.00288    0.00678   -0.42     0.67    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 9.686e-08 0.0003112


Log-likelihood: -874.323 
```

```r
get_glmmadmb(mlogit_state)
```

```
$model
extortion_victim ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -874.323

$df
[1] 9

$AIC
[1] 1766.646

$BIC
[1] 1818.74

$alpha
numeric(0)

$var_j
[1] 9.382053e-15

$ICC
numeric(0)
```

```r
lrtest(mlogit_state, mlogit_full)
```

```
Likelihood ratio test

Model 1: extortion_victim ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index
Model 2: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1   9 -874.32                    
2  32 -865.14 23 18.36     0.7377
```

```r
# - Binary: bus; Count: all vs. Full

mlogit_bus <- model_failsafe(update(count_formula, extortion_victim ~ .), 
                             data = enve_model,
                         family = "binomial", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mlogit_bus)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1784.2 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -2.0614     0.4098   -5.03  4.9e-07 ***
bribes                   -0.0823     0.0714   -1.15    0.249    
yearsquant(8,16]          0.1670     0.1918    0.87    0.384    
yearsquant(16,25]        -0.1492     0.1970   -0.76    0.449    
yearsquant(25,34]        -0.1612     0.1951   -0.83    0.409    
yearsquant(34,43]        -0.1185     0.2023   -0.59    0.558    
subsectorMining          -0.4396     0.8236   -0.53    0.594    
subsectorConstruction     0.2977     0.4058    0.73    0.463    
subsectorManufacturing    0.3026     0.3972    0.76    0.446    
subsectorWholesale        0.4060     0.4610    0.88    0.378    
subsectorTransport        0.0500     0.4688    0.11    0.915    
subsectorMedia           -0.4662     0.7061   -0.66    0.509    
subsectorFinance          0.4287     0.6106    0.70    0.483    
subsectorReal estate     -0.3194     0.7089   -0.45    0.652    
subsectorProf. services   0.9135     0.5468    1.67    0.095 .  
subsectorMaintenance      0.0349     0.4388    0.08    0.937    
subsectorEducation        0.1005     0.6494    0.15    0.877    
subsectorHealth           0.2237     0.4045    0.55    0.580    
subsectorLeisure          0.5028     0.5804    0.87    0.386    
subsectorHotelsRestBar    0.1382     0.4087    0.34    0.735    
subsectorOther          -11.9209   360.7200   -0.03    0.974    
sizeMedium               -0.2321     0.1843   -1.26    0.208    
sizeSmall                -0.0689     0.1800   -0.38    0.702    
sizeMicro                -0.0500     0.1770   -0.28    0.778    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance StdDev
(Intercept) 0.003124 0.0559


Log-likelihood: -867.103 
```

```r
get_glmmadmb(mlogit_bus)
```

```
$model
extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state)

$logLik
[1] -867.103

$df
[1] 25

$AIC
[1] 1784.206

$BIC
[1] 1928.911

$alpha
numeric(0)

$var_j
[1] 9.76125e-06

$ICC
numeric(0)
```

```r
lrtest(mlogit_bus, mlogit_full)
```

```
Likelihood ratio test

Model 1: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state)
Model 2: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -867.10                    
2  32 -865.14  7  3.92     0.7889
```

```r
# - Binary: all; Count: bus vs. Full

mtnb_bus <- glmmadmb(count_formula, data = enve_trunc,
                      family = "truncnbinom",
                      admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                      extra.args = "-ndi 60000")
```

```
Warning in eval(substitute(expr), data, enclos = parent.frame()): sd.est
not defined for this family
```

```r
summary(mtnb_bus)
```

```

Call:
glmmadmb(formula = count_formula, data = enve_trunc, family = "truncnbinom", 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 1349.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.17521    0.86417    1.36   0.1739   
bribes                   0.12890    0.14886    0.87   0.3865   
yearsquant(8,16]         0.07544    0.35857    0.21   0.8334   
yearsquant(16,25]       -0.08939    0.34751   -0.26   0.7970   
yearsquant(25,34]        0.69001    0.37312    1.85   0.0644 . 
yearsquant(34,43]        0.29316    0.37798    0.78   0.4380   
subsectorMining         -1.72441    1.41240   -1.22   0.2221   
subsectorConstruction   -0.26166    0.72017   -0.36   0.7164   
subsectorManufacturing  -0.82128    0.70489   -1.17   0.2440   
subsectorWholesale      -1.46454    0.84093   -1.74   0.0816 . 
subsectorTransport      -0.91817    0.81245   -1.13   0.2584   
subsectorMedia           0.75639    1.29350    0.58   0.5587   
subsectorFinance        -0.59996    1.05760   -0.57   0.5705   
subsectorReal estate    -1.86826    1.22160   -1.53   0.1262   
subsectorProf. services  0.39296    0.93505    0.42   0.6743   
subsectorMaintenance    -1.12107    0.78285   -1.43   0.1521   
subsectorEducation      -0.07175    1.14220   -0.06   0.9499   
subsectorHealth         -1.07338    0.72462   -1.48   0.1385   
subsectorLeisure        -0.32703    0.98437   -0.33   0.7397   
subsectorHotelsRestBar  -0.79534    0.72661   -1.09   0.2737   
sizeMedium               0.00876    0.33299    0.03   0.9790   
sizeSmall               -0.57556    0.32038   -1.80   0.0724 . 
sizeMicro               -0.95849    0.31976   -3.00   0.0027 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=285, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
            Variance    StdDev
(Intercept) 2.08e-09 4.561e-05

Negative binomial dispersion parameter: 0.15966 (std. err.: 0.10687)

Log-likelihood: -649.784 
```

```r
get_glmmadmb(mtnb_bus)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -649.784

$df
[1] 25

$AIC
[1] 1349.568

$BIC
[1] 1440.88

$alpha
[1] 6.26331

$var_j
[1] 4.3264e-18

$ICC
[1] 6.90753e-19
```

```r
lrtest(mtnb_bus, mtnb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -649.78                    
2  32 -646.98  7  5.61      0.586
```

```r
# - Binary: all; Count: state vs. Full

mtnb_state <- glmmadmb(mnb_only_state_formula, data = enve_trunc,
                      family = "truncnbinom",
                      admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                      extra.args = "-ndi 60000")
```

```
Warning in eval(substitute(expr), data, enclos = parent.frame()): sd.est
not defined for this family
```

```r
summary(mtnb_state)
```

```

Call:
glmmadmb(formula = mnb_only_state_formula, data = enve_trunc, 
    family = "truncnbinom", admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1347.8 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -2.43025    0.15766  -15.41   <2e-16 ***
log_pop       -0.00864    0.26828   -0.03     0.97    
log_wpn_crim   0.28184    0.29637    0.95     0.34    
log_drug_crim -0.00992    0.18798   -0.05     0.96    
log_bribe_vic -0.47997    0.45820   -1.05     0.29    
log_nbus       0.53020    0.65179    0.81     0.42    
comp_index    -0.00718    0.01728   -0.42     0.68    
law_index      0.00341    0.01405    0.24     0.81    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=285, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 4.633e-08 0.0002153

Negative binomial dispersion parameter: 0.006738 (std. err.: 0.00011993)

Log-likelihood: -663.889 
```

```r
get_glmmadmb(mtnb_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -663.889

$df
[1] 10

$AIC
[1] 1347.778

$BIC
[1] 1384.303

$alpha
[1] 148.412

$var_j
[1] 2.14684e-15

$ICC
[1] 1.44654e-17
```

```r
lrtest(mtnb_state, mtnb_full)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)  
1  10 -663.89                      
2  32 -646.98 22 33.82    0.05123 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Binary with deaths

mlogit_deaths_formula <- update(binary_formula, . ~ . + log_deaths)

mlogit_deaths <- model_failsafe(binary_formula, data = enve_model,
                        family = "binomial", multilevel = TRUE)
```

```
[1] "The model was fitted correctly with glmmadmb"
```

```r
summary(mlogit_deaths)
```

```

Call:
glmmADMB::glmmadmb(formula = formula, data = data, family = family, 
    zeroInflation = FALSE, admb.opts = admbControl(noinit = FALSE, 
        shess = FALSE), extra.args = "-ndi 60000")

AIC: 1794.3 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -2.03698    0.41416   -4.92  8.7e-07 ***
bribes                   -0.08012    0.07171   -1.12    0.264    
yearsquant(8,16]          0.16620    0.19217    0.86    0.387    
yearsquant(16,25]        -0.15750    0.19726   -0.80    0.425    
yearsquant(25,34]        -0.17361    0.19559   -0.89    0.375    
yearsquant(34,43]        -0.12664    0.20263   -0.62    0.532    
subsectorMining          -0.42876    0.82423   -0.52    0.603    
subsectorConstruction     0.27065    0.40634    0.67    0.505    
subsectorManufacturing    0.29557    0.39738    0.74    0.457    
subsectorWholesale        0.38996    0.46154    0.84    0.398    
subsectorTransport        0.02831    0.46944    0.06    0.952    
subsectorMedia           -0.48275    0.70651   -0.68    0.494    
subsectorFinance          0.41626    0.61160    0.68    0.496    
subsectorReal estate     -0.35440    0.71023   -0.50    0.618    
subsectorProf. services   0.90666    0.54843    1.65    0.098 .  
subsectorMaintenance      0.02076    0.43924    0.05    0.962    
subsectorEducation        0.08884    0.64992    0.14    0.891    
subsectorHealth           0.20113    0.40512    0.50    0.620    
subsectorLeisure          0.46179    0.58177    0.79    0.427    
subsectorHotelsRestBar    0.12410    0.40930    0.30    0.762    
subsectorOther          -11.71727  310.76000   -0.04    0.970    
sizeMedium               -0.24009    0.18453   -1.30    0.193    
sizeSmall                -0.07392    0.18023   -0.41    0.682    
sizeMicro                -0.06044    0.17733   -0.34    0.733    
log_pop                   0.08219    0.13154    0.62    0.532    
log_wpn_crim             -0.08686    0.12787   -0.68    0.497    
log_bribe_vic             0.09021    0.20395    0.44    0.658    
log_nbus                  0.05549    0.34501    0.16    0.872    
log_drug_crim             0.00677    0.09563    0.07    0.944    
comp_index                0.01397    0.00832    1.68    0.093 .  
law_index                -0.00255    0.00684   -0.37    0.710    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 6.678e-08 0.0002584


Log-likelihood: -865.143 
```

```r
get_glmmadmb(mlogit_deaths)
```

```
$model
extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index

$logLik
[1] -865.143

$df
[1] 32

$AIC
[1] 1794.286

$BIC
[1] 1979.509

$alpha
numeric(0)

$var_j
[1] 4.459702e-15

$ICC
numeric(0)
```

```r
lrtest(mlogit_full, mlogit_deaths)
```

```
Likelihood ratio test

Model 1: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
Model 2: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -865.14                    
2  32 -865.14  0     0          1
```

```r
# count with deaths

mtnb_deaths_formula <- update(mnb_final_formula, . ~ . + log_deaths)

mtnb_deaths <- glmmadmb(mtnb_deaths_formula, data = enve_trunc,
                      family = "truncnbinom",
                      admb.opts = admbControl(noinit = FALSE, shess=FALSE),
                      extra.args = "-ndi 60000")
```

```
Warning in eval(substitute(expr), data, enclos = parent.frame()): sd.est
not defined for this family
```

```r
summary(mtnb_deaths)
```

```

Call:
glmmadmb(formula = mtnb_deaths_formula, data = enve_trunc, family = "truncnbinom", 
    admb.opts = admbControl(noinit = FALSE, shess = FALSE), extra.args = "-ndi 60000")

AIC: 1358 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)   
(Intercept)               1.4588     0.7756    1.88   0.0600 . 
bribes                    0.1898     0.1520    1.25   0.2117   
yearsquant(8,16]         -0.0245     0.3499   -0.07   0.9441   
yearsquant(16,25]        -0.1009     0.3457   -0.29   0.7704   
yearsquant(25,34]         0.5970     0.3809    1.57   0.1170   
yearsquant(34,43]         0.2851     0.3807    0.75   0.4540   
subsectorMining          -1.3456     1.4158   -0.95   0.3419   
subsectorConstruction    -0.3418     0.7057   -0.48   0.6281   
subsectorManufacturing   -0.9926     0.6902   -1.44   0.1504   
subsectorWholesale       -0.9894     0.8568   -1.15   0.2482   
subsectorTransport       -0.9158     0.7961   -1.15   0.2500   
subsectorMedia            0.7914     1.2503    0.63   0.5267   
subsectorFinance         -0.4119     1.0381   -0.40   0.6915   
subsectorReal estate     -1.4402     1.2298   -1.17   0.2416   
subsectorProf. services   0.2779     0.9087    0.31   0.7598   
subsectorMaintenance     -1.1196     0.7761   -1.44   0.1491   
subsectorEducation        0.0732     1.1578    0.06   0.9496   
subsectorHealth          -0.9774     0.6971   -1.40   0.1609   
subsectorLeisure          0.3016     0.9821    0.31   0.7588   
subsectorHotelsRestBar   -0.7891     0.7050   -1.12   0.2630   
sizeMedium               -0.0299     0.3171   -0.09   0.9250   
sizeSmall                -0.5060     0.3187   -1.59   0.1123   
sizeMicro                -0.9836     0.3110   -3.16   0.0016 **
log_pop                   0.0017     0.2600    0.01   0.9948   
log_wpn_crim             -0.0166     0.3476   -0.05   0.9619   
log_bribe_vic            -0.2434     0.3985   -0.61   0.5413   
log_nbus                  0.3286     0.6197    0.53   0.5959   
log_drug_crim            -0.1221     0.1742   -0.70   0.4836   
comp_index               -0.0101     0.0161   -0.62   0.5321   
law_index                 0.0101     0.0140    0.72   0.4697   
log_deaths                0.3878     0.2722    1.43   0.1542   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=285, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 4.555e-09 6.749e-05

Negative binomial dispersion parameter: 0.21119 (std. err.: 0.11485)

Log-likelihood: -645.98 
```

```r
get_glmmadmb(mtnb_deaths)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths

$logLik
[1] -645.98

$df
[1] 33

$AIC
[1] 1357.96

$BIC
[1] 1478.492

$alpha
[1] 4.735073

$var_j
[1] 2.074802e-17

$ICC
[1] 4.381775e-18
```

```r
lrtest(mtnb_full, mtnb_deaths)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index + log_deaths
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -646.98                    
2  33 -645.98  1 1.998     0.1575
```

```r
# together with deaths

hurdle_compare(joint_ic(mlogit_full, mtnb_full),
               joint_ic(mlogit_deaths, mtnb_deaths),
               modnames = c("Full", "+ deaths"))
```

```
       Full      + deaths 
logLik -1512.122 -1511.123
df     64        65       
AIC    3152.244  3152.246 
BIC    3454.346  3458.001 
```

```r
compare_hurdle2other(mlogit_deaths, mtnb_deaths, mnb_f_deaths)
```

```
          Hurdle   Negbin.
logLik -1511.123 -1527.820
df        65.000    34.000
AIC     3152.246  3123.640
BIC     3458.001  3320.439
```



```r
update.hurdle <- function(object, new, ...)
{
    call <- object$call
    call$formula <- update(Formula::as.Formula(formula(object)), new)
    eval.parent(call)
}

zi_formula <- as.formula(extortions ~
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index | 1)

hurdle_pscl_simple_full <- hurdle(zi_formula, data = enve_model,
                                  dist = "negbin")
```

```
Error in solve.default(as.matrix(fit_count$hessian)): Lapack routine dgesv: system is exactly singular: U[21,21] = 0
```

```r
summary(hurdle_pscl_simple_full)
```

```
Error in summary(hurdle_pscl_simple_full): object 'hurdle_pscl_simple_full' not found
```

```r
lrtest(nb_final, hurdle_pscl_simple_full)
```

```
Error in lrtest.default(nb_final, hurdle_pscl_simple_full): object 'hurdle_pscl_simple_full' not found
```

```r
nb_mass_final <- glm.nb(nb_final_formula, data = enve_model)

summary(nb_mass_final)
```

```

Call:
glm.nb(formula = nb_final_formula, data = enve_model, init.theta = 0.05351940287, 
    link = log)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6859  -0.5261  -0.4920  -0.4517   2.4588  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -1.376e-01  5.670e-01  -0.243   0.8082  
bribes                  -2.198e-02  8.917e-02  -0.246   0.8053  
yearsquant(8,16]         2.075e-01  2.968e-01   0.699   0.4846  
yearsquant(16,25]       -1.291e-01  2.910e-01  -0.444   0.6573  
yearsquant(25,34]        4.161e-01  2.833e-01   1.469   0.1419  
yearsquant(34,43]        1.466e-01  2.962e-01   0.495   0.6206  
subsectorMining         -1.450e+00  1.031e+00  -1.406   0.1597  
subsectorConstruction    4.923e-02  5.523e-01   0.089   0.9290  
subsectorManufacturing  -2.121e-01  5.390e-01  -0.393   0.6940  
subsectorWholesale      -5.241e-01  6.593e-01  -0.795   0.4266  
subsectorTransport      -5.142e-01  6.384e-01  -0.806   0.4205  
subsectorMedia          -2.938e-02  8.372e-01  -0.035   0.9720  
subsectorFinance         5.147e-02  9.006e-01   0.057   0.9544  
subsectorReal estate    -1.170e+00  9.205e-01  -1.272   0.2035  
subsectorProf. services  1.183e+00  8.703e-01   1.359   0.1742  
subsectorMaintenance    -5.482e-01  5.935e-01  -0.924   0.3557  
subsectorEducation       3.831e-01  8.842e-01   0.433   0.6648  
subsectorHealth         -4.923e-01  5.506e-01  -0.894   0.3712  
subsectorLeisure         4.126e-01  8.625e-01   0.478   0.6324  
subsectorHotelsRestBar  -4.228e-01  5.534e-01  -0.764   0.4449  
subsectorOther          -3.654e+01  2.122e+07   0.000   1.0000  
sizeMedium              -1.593e-01  2.644e-01  -0.602   0.5469  
sizeSmall               -4.502e-01  2.688e-01  -1.675   0.0939 .
sizeMicro               -6.302e-01  2.661e-01  -2.369   0.0179 *
log_pop                  2.192e-01  1.968e-01   1.114   0.2654  
log_wpn_crim            -4.296e-02  1.900e-01  -0.226   0.8212  
log_bribe_vic           -2.324e-01  2.985e-01  -0.778   0.4363  
log_nbus                 4.815e-01  5.044e-01   0.955   0.3398  
log_drug_crim            3.085e-02  1.404e-01   0.220   0.8261  
comp_index               6.952e-03  1.221e-02   0.569   0.5691  
law_index               -5.889e-03  9.963e-03  -0.591   0.5545  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(0.0535) family taken to be 1)

    Null deviance: 729.09  on 2411  degrees of freedom
Residual deviance: 699.94  on 2381  degrees of freedom
AIC: 3121.3

Number of Fisher Scoring iterations: 1

              Theta:  0.05352 
          Std. Err.:  0.00410 

 2 x log-likelihood:  -3057.26800 
```

```r
lrtest(nb_mass_final, hurdle_pscl_simple_full)
```

```
Error in lrtest.default(nb_mass_final, hurdle_pscl_simple_full): object 'hurdle_pscl_simple_full' not found
```

```r
vuong(nb_mass_final, hurdle_pscl_simple_full)
```

```
Error in vuong(nb_mass_final, hurdle_pscl_simple_full): object 'hurdle_pscl_simple_full' not found
```

```r
zi_formula <- as.formula(extortions ~
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index |
                             bribes +
                             yearsquant +
                             subsector +
                             size +
                             log_pop +
                             log_wpn_crim +
                             log_bribe_vic +
                             log_nbus +
                             log_drug_crim +
                             comp_index +
                             law_index)

hurdle_pscl_full <- hurdle(zi_formula, data = enve_model,
                                  dist = "negbin")
```

```
Error in solve.default(as.matrix(fit_count$hessian)): Lapack routine dgesv: system is exactly singular: U[21,21] = 0
```

```r
summary(hurdle_pscl_full)
```

```
Error in summary(hurdle_pscl_full): object 'hurdle_pscl_full' not found
```

```r
lrtest(nb_final, hurdle_pscl_full)
```

```
Error in lrtest.default(nb_final, hurdle_pscl_full): object 'hurdle_pscl_full' not found
```

```r
lrtest(hurdle_pscl_simple_full, hurdle_pscl_full)
```

```
Error in lrtest(hurdle_pscl_simple_full, hurdle_pscl_full): object 'hurdle_pscl_simple_full' not found
```

```r
lrtest(nb_mass_final, hurdle_pscl_full)
```

```
Error in lrtest.default(nb_mass_final, hurdle_pscl_full): object 'hurdle_pscl_full' not found
```

```r
vuong(nb_mass_final, hurdle_pscl_full)
```

```
Error in vuong(nb_mass_final, hurdle_pscl_full): object 'hurdle_pscl_full' not found
```

```r
## Constant on either side of equation

hurdle_pscl_nullc <- update(hurdle_pscl_full, . ~ 1 | . )
```

```
Error in update(hurdle_pscl_full, . ~ 1 | .): object 'hurdle_pscl_full' not found
```

```r
summary(hurdle_pscl_nullc)
```

```
Error in summary(hurdle_pscl_nullc): object 'hurdle_pscl_nullc' not found
```

```r
lrtest(hurdle_pscl_nullc, hurdle_pscl_full)
```

```
Error in lrtest(hurdle_pscl_nullc, hurdle_pscl_full): object 'hurdle_pscl_nullc' not found
```

```r
vuong(hurdle_pscl_nullc, hurdle_pscl_full)
```

```
Error in vuong(hurdle_pscl_nullc, hurdle_pscl_full): object 'hurdle_pscl_nullc' not found
```

```r
hurdle_pscl_c_all_z_state <- update(hurdle_pscl_full, . ~ . | . - bribes -
                             yearsquant -
                             subsector -
                             size )
```

```
Error in update(hurdle_pscl_full, . ~ . | . - bribes - yearsquant - subsector - : object 'hurdle_pscl_full' not found
```

```r
summary(hurdle_pscl_c_all_z_state)
```

```
Error in summary(hurdle_pscl_c_all_z_state): object 'hurdle_pscl_c_all_z_state' not found
```

```r
lrtest(hurdle_pscl_simple_full, hurdle_pscl_c_all_z_state)
```

```
Error in lrtest(hurdle_pscl_simple_full, hurdle_pscl_c_all_z_state): object 'hurdle_pscl_simple_full' not found
```

```r
vuong(hurdle_pscl_simple_full, hurdle_pscl_c_all_z_state)
```

```
Error in vuong(hurdle_pscl_simple_full, hurdle_pscl_c_all_z_state): object 'hurdle_pscl_simple_full' not found
```

```r
hurdle_pscl_c_state_l_z_all <- update(hurdle_pscl_full, . ~ . - bribes -
                             yearsquant -
                             subsector -
                             size | . )
```

```
Error in update(hurdle_pscl_full, . ~ . - bribes - yearsquant - subsector - : object 'hurdle_pscl_full' not found
```

```r
summary(hurdle_pscl_c_state_l_z_all)
```

```
Error in summary(hurdle_pscl_c_state_l_z_all): object 'hurdle_pscl_c_state_l_z_all' not found
```

```r
lrtest(hurdle_pscl_simple_full, hurdle_pscl_c_state_l_z_all)
```

```
Error in lrtest(hurdle_pscl_simple_full, hurdle_pscl_c_state_l_z_all): object 'hurdle_pscl_simple_full' not found
```

```r
vuong(hurdle_pscl_simple_full, hurdle_pscl_c_state_l_z_all)
```

```
Error in vuong(hurdle_pscl_simple_full, hurdle_pscl_c_state_l_z_all): object 'hurdle_pscl_simple_full' not found
```

```r
screenreg(list("Hurdle Full" = hurdle_pscl_full,
               "Count: Null" = hurdle_pscl_nullc,
               "Logit: Null" = hurdle_pscl_simple_full,
               "Logit: state" = hurdle_pscl_c_all_z_state,
               "Count: state" = hurdle_pscl_c_state_l_z_all),
          single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'hurdle_pscl_full' not found
```

# The end


```r
endtime <- proc.time()

endtime - starttime
```

```
    user   system  elapsed 
8108.897  696.654 8243.554 
```
