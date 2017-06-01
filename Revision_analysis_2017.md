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
[1] "Thu Jun  1 01:30:12 2017"
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
 [1] pscl_1.4.9        lattice_0.20-35   MASS_7.3-47      
 [4] car_2.1-4         lmtest_0.9-35     zoo_1.8-0        
 [7] reshape2_1.4.2    dplyr_0.5.0       classInt_0.1-24  
[10] knitr_1.16        Cairo_1.5-9       ggplot2_2.2.1    
[13] foreign_0.8-67    victim_0.0.0.9000

loaded via a namespace (and not attached):
 [1] splines_3.4.0      colorspace_1.3-2   mgcv_1.8-17       
 [4] e1071_1.6-8        nloptr_1.0.4       withr_1.0.2       
 [7] DBI_0.6-1          plyr_1.8.4         stringr_1.2.0     
[10] MatrixModels_0.4-1 munsell_0.4.3      gtable_0.2.0      
[13] devtools_1.13.1    codetools_0.2-15   memoise_1.1.0     
[16] evaluate_0.10      labeling_0.3       SparseM_1.77      
[19] quantreg_5.33      pbkrtest_0.4-7     curl_2.6          
[22] parallel_3.4.0     class_7.3-14       dgof_1.2          
[25] highr_0.6          Rcpp_0.12.10       scales_0.4.1      
[28] ineq_0.2-13        lme4_1.1-13        digest_0.6.12     
[31] stringi_1.1.5      grid_3.4.0         tools_3.4.0       
[34] magrittr_1.5       lazyeval_0.2.0     tibble_1.3.0      
[37] Matrix_1.2-9       assertthat_0.2.0   minqa_1.2.4       
[40] httr_1.2.1         R6_2.2.1           nnet_7.3-12       
[43] nlme_3.1-131       git2r_0.18.0       compiler_3.4.0    
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
#library(lme4)
library(glmmADMB)
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
library(reshape2)
library(lmtest)
library(car)
library(pscl)

# make sure we use the dplyr select

select <- dplyr::select
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
       0.6462624 

$mc_confint
  MC 2.5%  MC 97.5% 
0.6287968 0.6632665 

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
0.9387268 0.9533721 

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
0             2127   1365.654
1              104    777.005
2               43    220.883
3               32     41.789
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
       0.6372182 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6198944 0.6539553 

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
0.9684494 0.9809287 

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
0.9900498 0.9962687 

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
        0.636096 

[[7]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6192131 0.6534250 

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
0.9585325 0.9730514 

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
       0.6195213 

[[9]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6021915 0.6356343 

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
       0.6406812 

[[1]]$mc_confint
  MC 2.5%  MC 97.5% 
0.6237463 0.6566420 

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
       0.9807104 

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
0.9947021 0.9985904 

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
Warning: Removed 18 rows containing non-finite values (stat_density).
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
0.9869381 0.9941795 

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
    select(extortions, years, yearsquant) %>%
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
    select(extortions, years, years_deciles) %>%
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

gg_ext_years <- select(gg_ext_years, -variable)

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
    select(extortions,
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

enve_model$extortions[which(enve_model$subsector == "Other")] <- rpois(10,1)


enve_model$extortions[which(enve_model$subsector == "Media")] <- rpois(45, 0.5)

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
 Mean   : 0.5651   Mean   : 0.2786   (25,34]:510   (4,8]  :246  
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
 Mean   :0.1281  
 3rd Qu.:0.0000  
 Max.   :1.0000  
                 
```

```r
summary(factor(enve_model$extortion_victim))
```

```
   0    1 
2103  309 
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
count_formula <- as.formula("extortions ~ 
                            bribes + 
                            yearsquant + 
                            subsector + 
                            size + 
                            (1 | state)")
```


```r
mnb1 <- glmmadmb(count_formula, data = enve_model, family = "nbinom", zeroInflation=FALSE)

summary(mnb1)
```

```

Call:
glmmadmb(formula = count_formula, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3247.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11181    0.53344   -0.21    0.834  
bribes                  -0.01377    0.09581   -0.14    0.886  
yearsquant(8,16]         0.16349    0.29371    0.56    0.578  
yearsquant(16,25]       -0.08372    0.27883   -0.30    0.764  
yearsquant(25,34]        0.42869    0.28252    1.52    0.129  
yearsquant(34,43]        0.20495    0.29258    0.70    0.484  
subsectorMining         -1.48222    0.96736   -1.53    0.125  
subsectorConstruction   -0.00565    0.53084   -0.01    0.992  
subsectorManufacturing  -0.26656    0.51538   -0.52    0.605  
subsectorWholesale      -0.62515    0.63925   -0.98    0.328  
subsectorTransport      -0.60032    0.60462   -0.99    0.321  
subsectorMedia          -0.35719    0.80355   -0.44    0.657  
subsectorFinance         0.00680    0.85954    0.01    0.994  
subsectorReal estate    -1.34661    0.86635   -1.55    0.120  
subsectorProf. services  1.07375    0.82593    1.30    0.194  
subsectorMaintenance    -0.65259    0.57174   -1.14    0.254  
subsectorEducation       0.02688    0.84629    0.03    0.975  
subsectorHealth         -0.53431    0.52972   -1.01    0.313  
subsectorLeisure         0.25187    0.81771    0.31    0.758  
subsectorHotelsRestBar  -0.42695    0.53081   -0.80    0.421  
subsectorOther           0.56957    1.40450    0.41    0.685  
sizeMedium              -0.24829    0.25851   -0.96    0.337  
sizeSmall               -0.49626    0.26067   -1.90    0.057 .
sizeMicro               -0.60290    0.25864   -2.33    0.020 *
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
(Intercept) 2.211e-09 4.702e-05

Negative binomial dispersion parameter: 0.060859 (std. err.: 0.0045385)

Log-likelihood: -1597.8 
```

```r
get_glmmadmb(mnb1)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -1597.8

$df
[1] 26

$AIC
[1] 3247.6

$BIC
[1] 3398.094

$alpha
[1] 16.43142

$var_j
[1] 4.889405e-18

$ICC
[1] 2.975643e-19
```

```r
# vs Poisson

mp1 <- glmmadmb(count_formula, data = enve_model, family = "poisson", zeroInflation=FALSE)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -noinit -shess'
had status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmadmb(count_formula, data = enve_model, family = "poisson", : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mp1)
```

```
Error in summary(mp1): object 'mp1' not found
```

```r
get_glmmadmb(mp1)
```

```
Error in get_glmmadmb(mp1): object 'mp1' not found
```

```r
screenreg(list(mnb1, mp1), single.row = TRUE,
          custom.model.names = c("negbin", "poisson"))
```

```
Error in "list" %in% class(l)[1]: object 'mp1' not found
```

```r
lrtest(mp1, mnb1)
```

```
Error in lrtest(mp1, mnb1): object 'mp1' not found
```

```r
# vs single level nb

nb1 <- update(mnb1, . ~ . - (1 | state))

summary(nb1)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size, data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3245.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11181    0.53345   -0.21    0.834  
bribes                  -0.01377    0.09581   -0.14    0.886  
yearsquant(8,16]         0.16349    0.29371    0.56    0.578  
yearsquant(16,25]       -0.08371    0.27883   -0.30    0.764  
yearsquant(25,34]        0.42869    0.28252    1.52    0.129  
yearsquant(34,43]        0.20496    0.29258    0.70    0.484  
subsectorMining         -1.48223    0.96736   -1.53    0.125  
subsectorConstruction   -0.00565    0.53084   -0.01    0.992  
subsectorManufacturing  -0.26657    0.51538   -0.52    0.605  
subsectorWholesale      -0.62516    0.63925   -0.98    0.328  
subsectorTransport      -0.60032    0.60463   -0.99    0.321  
subsectorMedia          -0.35720    0.80355   -0.44    0.657  
subsectorFinance         0.00675    0.85953    0.01    0.994  
subsectorReal estate    -1.34664    0.86635   -1.55    0.120  
subsectorProf. services  1.07374    0.82594    1.30    0.194  
subsectorMaintenance    -0.65261    0.57174   -1.14    0.254  
subsectorEducation       0.02686    0.84629    0.03    0.975  
subsectorHealth         -0.53432    0.52972   -1.01    0.313  
subsectorLeisure         0.25186    0.81771    0.31    0.758  
subsectorHotelsRestBar  -0.42695    0.53081   -0.80    0.421  
subsectorOther           0.56956    1.40450    0.41    0.685  
sizeMedium              -0.24829    0.25851   -0.96    0.337  
sizeSmall               -0.49627    0.26067   -1.90    0.057 .
sizeMicro               -0.60289    0.25864   -2.33    0.020 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.060775 (std. err.: 0.0045312)

Log-likelihood: -1597.8 
```

```r
get_glmmadmb(nb1)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size

$logLik
[1] -1597.8

$df
[1] 25

$AIC
[1] 3245.6

$BIC
[1] 3390.305

$alpha
[1] 16.45413
```

```r
screenreg(list(mnb1, nb1), single.row = TRUE,
          custom.model.names = c("multilevel nb", "single level nb"))
```

```

=============================================================
                         multilevel nb      single level nb  
-------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.11 (0.53)  
bribes                      -0.01 (0.10)       -0.01 (0.10)  
yearsquant(8,16]             0.16 (0.29)        0.16 (0.29)  
yearsquant(16,25]           -0.08 (0.28)       -0.08 (0.28)  
yearsquant(25,34]            0.43 (0.28)        0.43 (0.28)  
yearsquant(34,43]            0.20 (0.29)        0.20 (0.29)  
subsectorMining             -1.48 (0.97)       -1.48 (0.97)  
subsectorConstruction       -0.01 (0.53)       -0.01 (0.53)  
subsectorManufacturing      -0.27 (0.52)       -0.27 (0.52)  
subsectorWholesale          -0.63 (0.64)       -0.63 (0.64)  
subsectorTransport          -0.60 (0.60)       -0.60 (0.60)  
subsectorMedia              -0.36 (0.80)       -0.36 (0.80)  
subsectorFinance             0.01 (0.86)        0.01 (0.86)  
subsectorReal estate        -1.35 (0.87)       -1.35 (0.87)  
subsectorProf. services      1.07 (0.83)        1.07 (0.83)  
subsectorMaintenance        -0.65 (0.57)       -0.65 (0.57)  
subsectorEducation           0.03 (0.85)        0.03 (0.85)  
subsectorHealth             -0.53 (0.53)       -0.53 (0.53)  
subsectorLeisure             0.25 (0.82)        0.25 (0.82)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.43 (0.53)  
subsectorOther               0.57 (1.40)        0.57 (1.40)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)  
sizeSmall                   -0.50 (0.26)       -0.50 (0.26)  
sizeMicro                   -0.60 (0.26) *     -0.60 (0.26) *
-------------------------------------------------------------
Variance: state              0.00                            
Dispersion: parameter        0.06               0.06         
Dispersion: SD               0.00               0.00         
AIC                       3247.60            3245.60         
BIC                       3398.09            3390.31         
Log Likelihood           -1597.80           -1597.80         
Num. obs.                 2412               2412            
Num. groups: state          32                               
Num. groups:                                    1            
=============================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(nb1, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -1597.8                    
2  26 -1597.8  1     0          1
```

```r
vif(mnb1)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.168583  1        1.081010
yearsquant 1.489217  4        1.051041
subsector  1.693996 15        1.017725
size       1.340733  3        1.050083
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
-2.341 -0.659 -0.481 -0.310 32.519 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)   
(Intercept)              0.872619   0.316690   2.755  0.00591 **
bribes                  -0.006194   0.049471  -0.125  0.90038   
yearsquant(8,16]         0.063197   0.165262   0.382  0.70220   
yearsquant(16,25]       -0.030907   0.160479  -0.193  0.84729   
yearsquant(25,34]        0.206289   0.158220   1.304  0.19242   
yearsquant(34,43]        0.127875   0.164799   0.776  0.43786   
subsectorMining         -0.530860   0.546167  -0.972  0.33116   
subsectorConstruction   -0.022027   0.313038  -0.070  0.94391   
subsectorManufacturing  -0.128147   0.305254  -0.420  0.67467   
subsectorWholesale      -0.337798   0.369810  -0.913  0.36110   
subsectorTransport      -0.268970   0.358447  -0.750  0.45310   
subsectorMedia          -0.204088   0.474839  -0.430  0.66738   
subsectorFinance         0.034524   0.511138   0.068  0.94615   
subsectorReal estate    -0.446792   0.493645  -0.905  0.36551   
subsectorProf. services  1.404726   0.501732   2.800  0.00516 **
subsectorMaintenance    -0.319555   0.333989  -0.957  0.33877   
subsectorEducation       0.027084   0.506864   0.053  0.95739   
subsectorHealth         -0.276485   0.310740  -0.890  0.37368   
subsectorLeisure         0.577148   0.493257   1.170  0.24209   
subsectorHotelsRestBar  -0.171806   0.312643  -0.550  0.58269   
subsectorOther           0.450481   0.856158   0.526  0.59882   
sizeMedium              -0.243381   0.149295  -1.630  0.10319   
sizeSmall               -0.311054   0.150318  -2.069  0.03863 * 
sizeMicro               -0.370415   0.148302  -2.498  0.01257 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.554 on 2388 degrees of freedom
Multiple R-squared:  0.01305,	Adjusted R-squared:  0.003542 
F-statistic: 1.373 on 23 and 2388 DF,  p-value: 0.111
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
Warning in extractAIC.glmmadmb(nfit, scale, k = k, ...): ignored explicit
specification of scale

Warning in extractAIC.glmmadmb(nfit, scale, k = k, ...): ignored explicit
specification of scale

Warning in extractAIC.glmmadmb(nfit, scale, k = k, ...): ignored explicit
specification of scale

Warning in extractAIC.glmmadmb(nfit, scale, k = k, ...): ignored explicit
specification of scale
```

```r
mnb1_drop
```

```
Single term deletions

Model:
extortions ~ bribes + yearsquant + subsector + size + (1 | state)
           Df    AIC   LRT Pr(>Chi)  
<none>        3247.6                 
bribes      1 3245.6  0.02  0.88754  
yearsquant  4 3243.4  3.76  0.43946  
subsector  15 3234.5 16.86  0.32730  
size        3 3248.2  6.62  0.08505 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# NB null, multi and one level

nb_null <- update(mnb1, . ~ 1)

summary(nb_null)
```

```

Call:
glmmadmb(formula = extortions ~ 1, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3224.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.057159 (std. err.: 0.0042079)

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(nb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1610.33

$df
[1] 2

$AIC
[1] 3224.66

$BIC
[1] 3236.236

$alpha
[1] 17.49506
```

```r
lrtest(nb_null, nb1)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1610.3                    
2  25 -1597.8 23 25.06     0.3472
```

```r
lrtest(nb_null, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1610.3                    
2  26 -1597.8 24 25.06     0.4025
```

```r
mnb_null <- update(mnb1, . ~ 1 + (1 | state))

summary(mnb_null)
```

```

Call:
glmmadmb(formula = extortions ~ (1 | state), data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3226.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
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
(Intercept) 2.69e-07 0.0005187

Negative binomial dispersion parameter: 0.057237 (std. err.: 0.0042146)

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(mnb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1610.33

$df
[1] 3

$AIC
[1] 3226.66

$BIC
[1] 3244.025

$alpha
[1] 17.47122

$var_j
[1] 7.238252e-14

$ICC
[1] 4.142958e-15
```

```r
lrtest(nb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1610.3                    
2   3 -1610.3  1     0          1
```

```r
lrtest(mnb_null, mnb1)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   3 -1610.3                    
2  26 -1597.8 23 25.06     0.3472
```


```r
mnb2 <- update(mnb1, . ~ . - yearsquant + years_deciles)

summary(mnb2)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + subsector + size + (1 | 
    state) + years_deciles, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3246.9 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)               0.3337     0.5927    0.56    0.573  
bribes                   -0.0477     0.0942   -0.51    0.613  
subsectorMining          -2.0886     0.9906   -2.11    0.035 *
subsectorConstruction    -0.3715     0.5433   -0.68    0.494  
subsectorManufacturing   -0.6353     0.5300   -1.20    0.231  
subsectorWholesale       -1.0376     0.6540   -1.59    0.113  
subsectorTransport       -1.0209     0.6213   -1.64    0.100  
subsectorMedia           -0.7213     0.8081   -0.89    0.372  
subsectorFinance         -0.3482     0.8760   -0.40    0.691  
subsectorReal estate     -1.8889     0.8940   -2.11    0.035 *
subsectorProf. services   0.6537     0.8387    0.78    0.436  
subsectorMaintenance     -1.0565     0.5828   -1.81    0.070 .
subsectorEducation       -0.6430     0.8644   -0.74    0.457  
subsectorHealth          -0.9725     0.5506   -1.77    0.077 .
subsectorLeisure         -0.3842     0.8384   -0.46    0.647  
subsectorHotelsRestBar   -0.8337     0.5459   -1.53    0.127  
subsectorOther            0.2693     1.3928    0.19    0.847  
sizeMedium               -0.2540     0.2611   -0.97    0.331  
sizeSmall                -0.4472     0.2641   -1.69    0.090 .
sizeMicro                -0.5115     0.2629   -1.95    0.052 .
years_deciles(4,8]       -0.1928     0.3857   -0.50    0.617  
years_deciles(8,12]       0.1242     0.4010    0.31    0.757  
years_deciles(12,16]      0.0597     0.4039    0.15    0.883  
years_deciles(16,21]     -0.2430     0.3758   -0.65    0.518  
years_deciles(21,25]     -0.1208     0.4018   -0.30    0.764  
years_deciles(25,30]      0.7876     0.3764    2.09    0.036 *
years_deciles(30,34]     -0.6061     0.4133   -1.47    0.143  
years_deciles(34,39]      0.0994     0.3898    0.25    0.799  
years_deciles(39,43]      0.2124     0.4125    0.51    0.607  
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
(Intercept) 2.293e-09 4.788e-05

Negative binomial dispersion parameter: 0.062429 (std. err.: 0.0046764)

Log-likelihood: -1592.44 
```

```r
get_glmmadmb(mnb2)
```

```
$model
extortions ~ bribes + subsector + size + (1 | state) + years_deciles

$logLik
[1] -1592.44

$df
[1] 31

$AIC
[1] 3246.88

$BIC
[1] 3426.315

$alpha
[1] 16.0182

$var_j
[1] 5.255556e-18

$ICC
[1] 3.280991e-19
```

```r
screenreg(list(mnb1, mnb2), single.row = TRUE,
          custom.model.names = c("year qunitiles", "year deciles"))
```

```

=============================================================
                         year qunitiles     year deciles     
-------------------------------------------------------------
(Intercept)                 -0.11 (0.53)        0.33 (0.59)  
bribes                      -0.01 (0.10)       -0.05 (0.09)  
yearsquant(8,16]             0.16 (0.29)                     
yearsquant(16,25]           -0.08 (0.28)                     
yearsquant(25,34]            0.43 (0.28)                     
yearsquant(34,43]            0.20 (0.29)                     
subsectorMining             -1.48 (0.97)       -2.09 (0.99) *
subsectorConstruction       -0.01 (0.53)       -0.37 (0.54)  
subsectorManufacturing      -0.27 (0.52)       -0.64 (0.53)  
subsectorWholesale          -0.63 (0.64)       -1.04 (0.65)  
subsectorTransport          -0.60 (0.60)       -1.02 (0.62)  
subsectorMedia              -0.36 (0.80)       -0.72 (0.81)  
subsectorFinance             0.01 (0.86)       -0.35 (0.88)  
subsectorReal estate        -1.35 (0.87)       -1.89 (0.89) *
subsectorProf. services      1.07 (0.83)        0.65 (0.84)  
subsectorMaintenance        -0.65 (0.57)       -1.06 (0.58)  
subsectorEducation           0.03 (0.85)       -0.64 (0.86)  
subsectorHealth             -0.53 (0.53)       -0.97 (0.55)  
subsectorLeisure             0.25 (0.82)       -0.38 (0.84)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.83 (0.55)  
subsectorOther               0.57 (1.40)        0.27 (1.39)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)  
sizeSmall                   -0.50 (0.26)       -0.45 (0.26)  
sizeMicro                   -0.60 (0.26) *     -0.51 (0.26)  
years_deciles(4,8]                             -0.19 (0.39)  
years_deciles(8,12]                             0.12 (0.40)  
years_deciles(12,16]                            0.06 (0.40)  
years_deciles(16,21]                           -0.24 (0.38)  
years_deciles(21,25]                           -0.12 (0.40)  
years_deciles(25,30]                            0.79 (0.38) *
years_deciles(30,34]                           -0.61 (0.41)  
years_deciles(34,39]                            0.10 (0.39)  
years_deciles(39,43]                            0.21 (0.41)  
-------------------------------------------------------------
Variance: state              0.00               0.00         
Dispersion: parameter        0.06               0.06         
Dispersion: SD               0.00               0.00         
AIC                       3247.60            3246.88         
BIC                       3398.09            3426.31         
Log Likelihood           -1597.80           -1592.44         
Num. obs.                 2412               2412            
Num. groups: state          32                 32            
=============================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

Now start introducing the state-level variables. First, log deaths, then the saturated model, then test them. Also run excluding all business-level and compare.


```r
mnb3 <- update(mnb1, . ~ . + log_deaths + log_pop)

summary(mnb3)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths + log_pop, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3249.7 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.09825    0.53052   -0.19    0.853  
bribes                  -0.00459    0.09738   -0.05    0.962  
yearsquant(8,16]         0.14227    0.29423    0.48    0.629  
yearsquant(16,25]       -0.07380    0.28068   -0.26    0.793  
yearsquant(25,34]        0.40571    0.28431    1.43    0.154  
yearsquant(34,43]        0.16360    0.29397    0.56    0.578  
subsectorMining         -1.32646    0.97089   -1.37    0.172  
subsectorConstruction    0.04952    0.53112    0.09    0.926  
subsectorManufacturing  -0.23865    0.51452   -0.46    0.643  
subsectorWholesale      -0.48671    0.64719   -0.75    0.452  
subsectorTransport      -0.61442    0.60538   -1.01    0.310  
subsectorMedia          -0.27590    0.80423   -0.34    0.732  
subsectorFinance         0.04643    0.85588    0.05    0.957  
subsectorReal estate    -1.17753    0.87337   -1.35    0.178  
subsectorProf. services  1.14851    0.82663    1.39    0.165  
subsectorMaintenance    -0.56709    0.57656   -0.98    0.325  
subsectorEducation       0.16159    0.85514    0.19    0.850  
subsectorHealth         -0.47640    0.53000   -0.90    0.369  
subsectorLeisure         0.42544    0.82778    0.51    0.607  
subsectorHotelsRestBar  -0.39257    0.53012   -0.74    0.459  
subsectorOther           0.64781    1.40370    0.46    0.644  
sizeMedium              -0.25280    0.25913   -0.98    0.329  
sizeSmall               -0.49120    0.26092   -1.88    0.060 .
sizeMicro               -0.62899    0.25920   -2.43    0.015 *
log_deaths               0.11299    0.12426    0.91    0.363  
log_pop                  0.00595    0.19073    0.03    0.975  
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
(Intercept) 2.062e-09 4.541e-05

Negative binomial dispersion parameter: 0.061162 (std. err.: 0.0045671)

Log-likelihood: -1596.87 
```

```r
get_glmmadmb(mnb3)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop

$logLik
[1] -1596.87

$df
[1] 28

$AIC
[1] 3249.74

$BIC
[1] 3411.81

$alpha
[1] 16.35002

$var_j
[1] 4.250607e-18

$ICC
[1] 2.599756e-19
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
1  26 -1597.8                    
2  28 -1596.9  2  1.86     0.3946
```

```r
vif(mnb3)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.186456  1        1.089245
yearsquant 1.570754  4        1.058068
subsector  1.984645 15        1.023111
size       1.370686  3        1.053957
log_deaths 1.146093  1        1.070557
log_pop    1.088680  1        1.043398
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

mnb4 <- update(mnb3, . ~ . - log_pop)

summary(mnb4)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3247.7 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.09772    0.53034   -0.18    0.854  
bribes                  -0.00435    0.09707   -0.04    0.964  
yearsquant(8,16]         0.14279    0.29376    0.49    0.627  
yearsquant(16,25]       -0.07276    0.27871   -0.26    0.794  
yearsquant(25,34]        0.40517    0.28379    1.43    0.153  
yearsquant(34,43]        0.16362    0.29396    0.56    0.578  
subsectorMining         -1.32629    0.97086   -1.37    0.172  
subsectorConstruction    0.04900    0.53087    0.09    0.926  
subsectorManufacturing  -0.23950    0.51382   -0.47    0.641  
subsectorWholesale      -0.48668    0.64728   -0.75    0.452  
subsectorTransport      -0.61582    0.60368   -1.02    0.308  
subsectorMedia          -0.27694    0.80357   -0.34    0.730  
subsectorFinance         0.04647    0.85592    0.05    0.957  
subsectorReal estate    -1.17669    0.87299   -1.35    0.178  
subsectorProf. services  1.14692    0.82506    1.39    0.164  
subsectorMaintenance    -0.56838    0.57506   -0.99    0.323  
subsectorEducation       0.15911    0.85133    0.19    0.852  
subsectorHealth         -0.47689    0.52980   -0.90    0.368  
subsectorLeisure         0.42433    0.82700    0.51    0.608  
subsectorHotelsRestBar  -0.39300    0.52996   -0.74    0.458  
subsectorOther           0.64639    1.40290    0.46    0.645  
sizeMedium              -0.25360    0.25785   -0.98    0.325  
sizeSmall               -0.49121    0.26091   -1.88    0.060 .
sizeMicro               -0.62876    0.25909   -2.43    0.015 *
log_deaths               0.11584    0.08430    1.37    0.169  
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
(Intercept) 2.597e-09 5.096e-05

Negative binomial dispersion parameter: 0.061163 (std. err.: 0.0045671)

Log-likelihood: -1596.87 
```

```r
get_glmmadmb(mnb4)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths

$logLik
[1] -1596.87

$df
[1] 27

$AIC
[1] 3247.74

$BIC
[1] 3404.022

$alpha
[1] 16.34975

$var_j
[1] 6.745448e-18

$ICC
[1] 4.125718e-19
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
1  26 -1597.8                    
2  27 -1596.9  1  1.86     0.1726
3  28 -1596.9  1  0.00     1.0000
```

```r
vif(mnb4)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.180125  1        1.086335
yearsquant 1.512004  4        1.053038
subsector  1.918605 15        1.021958
size       1.350568  3        1.051363
log_deaths 1.144227  1        1.069685
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

mnb5 <- update(mnb3, . ~ . - log_deaths)

summary(mnb5)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3248.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.1174     0.5296   -0.22    0.825  
bribes                   -0.0143     0.0961   -0.15    0.881  
yearsquant(8,16]          0.1406     0.2944    0.48    0.633  
yearsquant(16,25]        -0.1005     0.2791   -0.36    0.719  
yearsquant(25,34]         0.4281     0.2827    1.51    0.130  
yearsquant(34,43]         0.1821     0.2935    0.62    0.535  
subsectorMining          -1.3991     0.9693   -1.44    0.149  
subsectorConstruction     0.0373     0.5317    0.07    0.944  
subsectorManufacturing   -0.2309     0.5148   -0.45    0.654  
subsectorWholesale       -0.5490     0.6415   -0.86    0.392  
subsectorTransport       -0.5761     0.6045   -0.95    0.341  
subsectorMedia           -0.2895     0.8047   -0.36    0.719  
subsectorFinance          0.0271     0.8565    0.03    0.975  
subsectorReal estate     -1.2706     0.8684   -1.46    0.143  
subsectorProf. services   1.1498     0.8275    1.39    0.165  
subsectorMaintenance     -0.5748     0.5762   -1.00    0.318  
subsectorEducation        0.1562     0.8567    0.18    0.855  
subsectorHealth          -0.4917     0.5300   -0.93    0.354  
subsectorLeisure          0.3688     0.8254    0.45    0.655  
subsectorHotelsRestBar   -0.3973     0.5306   -0.75    0.454  
subsectorOther            0.6455     1.4053    0.46    0.646  
sizeMedium               -0.2328     0.2587   -0.90    0.368  
sizeSmall                -0.4935     0.2610   -1.89    0.059 .
sizeMicro                -0.6225     0.2592   -2.40    0.016 *
log_pop                   0.1323     0.1300    1.02    0.309  
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
(Intercept) 2.062e-09 4.541e-05

Negative binomial dispersion parameter: 0.06101 (std. err.: 0.0045517)

Log-likelihood: -1597.28 
```

```r
get_glmmadmb(mnb5)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop

$logLik
[1] -1597.28

$df
[1] 27

$AIC
[1] 3248.56

$BIC
[1] 3404.842

$alpha
[1] 16.39076

$var_j
[1] 4.251844e-18

$ICC
[1] 2.59405e-19
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
1  26 -1597.8                    
2  27 -1597.3  1  1.04     0.3078
3  28 -1596.9  1  0.82     0.3652
```

```r
vif(mnb5)
```

```
               GVIF Df GVIF^(1/(2*Df))
bribes     1.169019  1        1.081212
yearsquant 1.511378  4        1.052984
subsector  1.819251 15        1.020148
size       1.355051  3        1.051944
log_pop    1.098517  1        1.048102
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

===================================================================================================
                         negbin             deaths + pop       deaths             pop              
---------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.10 (0.53)       -0.10 (0.53)       -0.12 (0.53)  
bribes                      -0.01 (0.10)       -0.00 (0.10)       -0.00 (0.10)       -0.01 (0.10)  
yearsquant(8,16]             0.16 (0.29)        0.14 (0.29)        0.14 (0.29)        0.14 (0.29)  
yearsquant(16,25]           -0.08 (0.28)       -0.07 (0.28)       -0.07 (0.28)       -0.10 (0.28)  
yearsquant(25,34]            0.43 (0.28)        0.41 (0.28)        0.41 (0.28)        0.43 (0.28)  
yearsquant(34,43]            0.20 (0.29)        0.16 (0.29)        0.16 (0.29)        0.18 (0.29)  
subsectorMining             -1.48 (0.97)       -1.33 (0.97)       -1.33 (0.97)       -1.40 (0.97)  
subsectorConstruction       -0.01 (0.53)        0.05 (0.53)        0.05 (0.53)        0.04 (0.53)  
subsectorManufacturing      -0.27 (0.52)       -0.24 (0.51)       -0.24 (0.51)       -0.23 (0.51)  
subsectorWholesale          -0.63 (0.64)       -0.49 (0.65)       -0.49 (0.65)       -0.55 (0.64)  
subsectorTransport          -0.60 (0.60)       -0.61 (0.61)       -0.62 (0.60)       -0.58 (0.60)  
subsectorMedia              -0.36 (0.80)       -0.28 (0.80)       -0.28 (0.80)       -0.29 (0.80)  
subsectorFinance             0.01 (0.86)        0.05 (0.86)        0.05 (0.86)        0.03 (0.86)  
subsectorReal estate        -1.35 (0.87)       -1.18 (0.87)       -1.18 (0.87)       -1.27 (0.87)  
subsectorProf. services      1.07 (0.83)        1.15 (0.83)        1.15 (0.83)        1.15 (0.83)  
subsectorMaintenance        -0.65 (0.57)       -0.57 (0.58)       -0.57 (0.58)       -0.57 (0.58)  
subsectorEducation           0.03 (0.85)        0.16 (0.86)        0.16 (0.85)        0.16 (0.86)  
subsectorHealth             -0.53 (0.53)       -0.48 (0.53)       -0.48 (0.53)       -0.49 (0.53)  
subsectorLeisure             0.25 (0.82)        0.43 (0.83)        0.42 (0.83)        0.37 (0.83)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.39 (0.53)       -0.39 (0.53)       -0.40 (0.53)  
subsectorOther               0.57 (1.40)        0.65 (1.40)        0.65 (1.40)        0.65 (1.41)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)       -0.25 (0.26)       -0.23 (0.26)  
sizeSmall                   -0.50 (0.26)       -0.49 (0.26)       -0.49 (0.26)       -0.49 (0.26)  
sizeMicro                   -0.60 (0.26) *     -0.63 (0.26) *     -0.63 (0.26) *     -0.62 (0.26) *
log_deaths                                      0.11 (0.12)        0.12 (0.08)                     
log_pop                                         0.01 (0.19)                           0.13 (0.13)  
---------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00         
Dispersion: parameter        0.06               0.06               0.06               0.06         
Dispersion: SD               0.00               0.00               0.00               0.00         
AIC                       3247.60            3249.74            3247.74            3248.56         
BIC                       3398.09            3411.81            3404.02            3404.84         
Log Likelihood           -1597.80           -1596.87           -1596.87           -1597.28         
Num. obs.                 2412               2412               2412               2412            
Num. groups: state          32                 32                 32                 32            
===================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```


```r
mnb6 <- update(mnb1, . ~ . + log_bribe_vic + log_nbus)

summary(mnb6)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_bribe_vic + log_nbus, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3251.5 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.1019     0.5353   -0.19    0.849  
bribes                   -0.0143     0.0957   -0.15    0.882  
yearsquant(8,16]          0.1641     0.2944    0.56    0.577  
yearsquant(16,25]        -0.0887     0.2808   -0.32    0.752  
yearsquant(25,34]         0.4335     0.2842    1.53    0.127  
yearsquant(34,43]         0.2086     0.2928    0.71    0.476  
subsectorMining          -1.5097     0.9761   -1.55    0.122  
subsectorConstruction    -0.0226     0.5352   -0.04    0.966  
subsectorManufacturing   -0.2800     0.5185   -0.54    0.589  
subsectorWholesale       -0.6302     0.6399   -0.98    0.325  
subsectorTransport       -0.5910     0.6055   -0.98    0.329  
subsectorMedia           -0.3670     0.8048   -0.46    0.648  
subsectorFinance          0.0192     0.8643    0.02    0.982  
subsectorReal estate     -1.3603     0.8693   -1.56    0.118  
subsectorProf. services   1.0439     0.8333    1.25    0.210  
subsectorMaintenance     -0.6700     0.5752   -1.16    0.244  
subsectorEducation        0.0329     0.8463    0.04    0.969  
subsectorHealth          -0.5356     0.5306   -1.01    0.313  
subsectorLeisure          0.2475     0.8176    0.30    0.762  
subsectorHotelsRestBar   -0.4443     0.5349   -0.83    0.406  
subsectorOther            0.5667     1.4047    0.40    0.687  
sizeMedium               -0.2490     0.2588   -0.96    0.336  
sizeSmall                -0.4939     0.2611   -1.89    0.059 .
sizeMicro                -0.6011     0.2599   -2.31    0.021 *
log_bribe_vic            -0.0262     0.2347   -0.11    0.911  
log_nbus                  0.1090     0.4085    0.27    0.790  
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
(Intercept) 2.344e-09 4.841e-05

Negative binomial dispersion parameter: 0.060868 (std. err.: 0.0045393)

Log-likelihood: -1597.76 
```

```r
get_glmmadmb(mnb6)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus

$logLik
[1] -1597.76

$df
[1] 28

$AIC
[1] 3251.52

$BIC
[1] 3413.59

$alpha
[1] 16.42899

$var_j
[1] 5.49293e-18

$ICC
[1] 3.343436e-19
```

```r
vif(mnb6)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.167531  1        1.080524
yearsquant    1.558505  4        1.057033
subsector     1.955180 15        1.022601
size          1.365224  3        1.053256
log_bribe_vic 1.088759  1        1.043436
log_nbus      1.123386  1        1.059899
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

mnb7 <- update(mnb1, . ~ . + log_bribe_vic)

summary(mnb7)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_bribe_vic, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3249.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11196    0.53377   -0.21    0.834  
bribes                  -0.01380    0.09587   -0.14    0.886  
yearsquant(8,16]         0.16365    0.29434    0.56    0.578  
yearsquant(16,25]       -0.08348    0.28015   -0.30    0.766  
yearsquant(25,34]        0.42847    0.28365    1.51    0.131  
yearsquant(34,43]        0.20498    0.29259    0.70    0.484  
subsectorMining         -1.48291    0.97079   -1.53    0.127  
subsectorConstruction   -0.00590    0.53162   -0.01    0.991  
subsectorManufacturing  -0.26679    0.51600   -0.52    0.605  
subsectorWholesale      -0.62528    0.63942   -0.98    0.328  
subsectorTransport      -0.60034    0.60463   -0.99    0.321  
subsectorMedia          -0.35743    0.80399   -0.44    0.657  
subsectorFinance         0.00617    0.86241    0.01    0.994  
subsectorReal estate    -1.34613    0.86806   -1.55    0.121  
subsectorProf. services  1.07383    0.82599    1.30    0.194  
subsectorMaintenance    -0.65259    0.57174   -1.14    0.254  
subsectorEducation       0.02688    0.84628    0.03    0.975  
subsectorHealth         -0.53455    0.53044   -1.01    0.314  
subsectorLeisure         0.25178    0.81773    0.31    0.758  
subsectorHotelsRestBar  -0.42682    0.53107   -0.80    0.422  
subsectorOther           0.56937    1.40480    0.41    0.685  
sizeMedium              -0.24840    0.25881   -0.96    0.337  
sizeSmall               -0.49616    0.26099   -1.90    0.057 .
sizeMicro               -0.60269    0.25982   -2.32    0.020 *
log_bribe_vic           -0.00185    0.21594   -0.01    0.993  
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
(Intercept) 2.249e-09 4.742e-05

Negative binomial dispersion parameter: 0.060859 (std. err.: 0.0045386)

Log-likelihood: -1597.8 
```

```r
get_glmmadmb(mnb7)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic

$logLik
[1] -1597.8

$df
[1] 27

$AIC
[1] 3249.6

$BIC
[1] 3405.882

$alpha
[1] 16.43142

$var_j
[1] 5.056202e-18

$ICC
[1] 3.077154e-19
```

```r
vif(mnb7)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.169025  1        1.081215
yearsquant    1.535540  4        1.055073
subsector     1.775599 15        1.019322
size          1.365413  3        1.053280
log_bribe_vic 1.090156  1        1.044105
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
1  26 -1597.8                    
2  27 -1597.8  1  0.00     1.0000
3  28 -1597.8  1  0.08     0.7773
```

```r
# test vs business-level bribes

mnb8 <- update(mnb6, . ~ . - bribes)

summary(mnb8)
```

```

Call:
glmmadmb(formula = extortions ~ yearsquant + subsector + size + 
    (1 | state) + log_bribe_vic + log_nbus, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3249.5 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.0998     0.5352   -0.19    0.852  
yearsquant(8,16]          0.1669     0.2938    0.57    0.570  
yearsquant(16,25]        -0.0875     0.2806   -0.31    0.755  
yearsquant(25,34]         0.4297     0.2829    1.52    0.129  
yearsquant(34,43]         0.2053     0.2921    0.70    0.482  
subsectorMining          -1.5087     0.9760   -1.55    0.122  
subsectorConstruction    -0.0295     0.5332   -0.06    0.956  
subsectorManufacturing   -0.2858     0.5171   -0.55    0.581  
subsectorWholesale       -0.6349     0.6391   -0.99    0.320  
subsectorTransport       -0.5927     0.6054   -0.98    0.328  
subsectorMedia           -0.3690     0.8047   -0.46    0.647  
subsectorFinance          0.0142     0.8637    0.02    0.987  
subsectorReal estate     -1.3610     0.8693   -1.57    0.117  
subsectorProf. services   1.0394     0.8328    1.25    0.212  
subsectorMaintenance     -0.6820     0.5693   -1.20    0.231  
subsectorEducation        0.0266     0.8452    0.03    0.975  
subsectorHealth          -0.5396     0.5299   -1.02    0.309  
subsectorLeisure          0.2476     0.8175    0.30    0.762  
subsectorHotelsRestBar   -0.4457     0.5349   -0.83    0.405  
subsectorOther            0.5660     1.4047    0.40    0.687  
sizeMedium               -0.2480     0.2587   -0.96    0.338  
sizeSmall                -0.4913     0.2606   -1.89    0.059 .
sizeMicro                -0.6064     0.2573   -2.36    0.018 *
log_bribe_vic            -0.0249     0.2344   -0.11    0.916  
log_nbus                  0.1078     0.4083    0.26    0.792  
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
(Intercept) 2.616e-09 5.115e-05

Negative binomial dispersion parameter: 0.060866 (std. err.: 0.0045391)

Log-likelihood: -1597.77 
```

```r
get_glmmadmb(mnb8)
```

```
$model
extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic + 
    log_nbus

$logLik
[1] -1597.77

$df
[1] 27

$AIC
[1] 3249.54

$BIC
[1] 3405.822

$alpha
[1] 16.42953

$var_j
[1] 6.846072e-18

$ICC
[1] 4.16693e-19
```

```r
vif(mnb8)
```

```
                  GVIF Df GVIF^(1/(2*Df))
yearsquant    1.492396  4        1.051321
subsector     1.842566 15        1.020581
size          1.296982  3        1.044293
log_bribe_vic 1.088435  1        1.043281
log_nbus      1.123163  1        1.059794
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
1  27 -1597.8                    
2  28 -1597.8  1  0.02     0.8875
```

```r
mnb9 <- update(mnb7, . ~ . - bribes)

summary(mnb9)
```

```

Call:
glmmadmb(formula = extortions ~ yearsquant + subsector + size + 
    (1 | state) + log_bribe_vic, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3247.6 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.109893   0.533690   -0.21    0.837  
yearsquant(8,16]         0.166311   0.293740    0.57    0.571  
yearsquant(16,25]       -0.082393   0.280030   -0.29    0.769  
yearsquant(25,34]        0.424735   0.282360    1.50    0.133  
yearsquant(34,43]        0.201772   0.291830    0.69    0.489  
subsectorMining         -1.482157   0.970700   -1.53    0.127  
subsectorConstruction   -0.012652   0.529500   -0.02    0.981  
subsectorManufacturing  -0.272493   0.514490   -0.53    0.596  
subsectorWholesale      -0.629825   0.638650   -0.99    0.324  
subsectorTransport      -0.601825   0.604570   -1.00    0.320  
subsectorMedia          -0.359393   0.803940   -0.45    0.655  
subsectorFinance         0.001560   0.861880    0.00    0.999  
subsectorReal estate    -1.346943   0.867970   -1.55    0.121  
subsectorProf. services  1.069089   0.825350    1.30    0.195  
subsectorMaintenance    -0.664517   0.565640   -1.17    0.240  
subsectorEducation       0.020869   0.845200    0.02    0.980  
subsectorHealth         -0.538380   0.529830   -1.02    0.310  
subsectorLeisure         0.251899   0.817710    0.31    0.758  
subsectorHotelsRestBar  -0.428360   0.530960   -0.81    0.420  
subsectorOther           0.568670   1.404800    0.40    0.686  
sizeMedium              -0.247406   0.258750   -0.96    0.339  
sizeSmall               -0.493591   0.260390   -1.90    0.058 .
sizeMicro               -0.607786   0.257310   -2.36    0.018 *
log_bribe_vic           -0.000796   0.215700    0.00    0.997  
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
(Intercept) 2.123e-09 4.607e-05

Negative binomial dispersion parameter: 0.060857 (std. err.: 0.0045384)

Log-likelihood: -1597.81 
```

```r
get_glmmadmb(mnb9)
```

```
$model
extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic

$logLik
[1] -1597.81

$df
[1] 26

$AIC
[1] 3247.62

$BIC
[1] 3398.114

$alpha
[1] 16.43196

$var_j
[1] 4.506704e-18

$ICC
[1] 2.742645e-19
```

```r
vif(mnb9)
```

```
                  GVIF Df GVIF^(1/(2*Df))
yearsquant    1.469733  4        1.049312
subsector     1.669571 15        1.017232
size          1.296927  3        1.044286
log_bribe_vic 1.089813  1        1.043941
```

```r
mnb10 <- update(mnb1, . ~ . - bribes)

summary(mnb10)
```

```

Call:
glmmadmb(formula = extortions ~ yearsquant + subsector + size + 
    (1 | state), data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3245.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.10982    0.53339   -0.21    0.837  
yearsquant(8,16]         0.16623    0.29307    0.57    0.571  
yearsquant(16,25]       -0.08250    0.27868   -0.30    0.767  
yearsquant(25,34]        0.42482    0.28115    1.51    0.131  
yearsquant(34,43]        0.20176    0.29183    0.69    0.489  
subsectorMining         -1.48185    0.96728   -1.53    0.126  
subsectorConstruction   -0.01254    0.52861   -0.02    0.981  
subsectorManufacturing  -0.27240    0.51380   -0.53    0.596  
subsectorWholesale      -0.62978    0.63845   -0.99    0.324  
subsectorTransport      -0.60183    0.60456   -1.00    0.320  
subsectorMedia          -0.35929    0.80348   -0.45    0.655  
subsectorFinance         0.00183    0.85891    0.00    0.998  
subsectorReal estate    -1.34715    0.86627   -1.56    0.120  
subsectorProf. services  1.06905    0.82531    1.30    0.195  
subsectorMaintenance    -0.66450    0.56563   -1.17    0.240  
subsectorEducation       0.02087    0.84520    0.02    0.980  
subsectorHealth         -0.53827    0.52905   -1.02    0.309  
subsectorLeisure         0.25193    0.81768    0.31    0.758  
subsectorHotelsRestBar  -0.42842    0.53071   -0.81    0.420  
subsectorOther           0.56875    1.40460    0.40    0.686  
sizeMedium              -0.24737    0.25846   -0.96    0.339  
sizeSmall               -0.49364    0.26005   -1.90    0.058 .
sizeMicro               -0.60788    0.25621   -2.37    0.018 *
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
(Intercept) 2.062e-09 4.54e-05

Negative binomial dispersion parameter: 0.060857 (std. err.: 0.0045384)

Log-likelihood: -1597.81 
```

```r
get_glmmadmb(mnb10)
```

```
$model
extortions ~ yearsquant + subsector + size + (1 | state)

$logLik
[1] -1597.81

$df
[1] 25

$AIC
[1] 3245.62

$BIC
[1] 3390.325

$alpha
[1] 16.43196

$var_j
[1] 4.250195e-18

$ICC
[1] 2.586541e-19
```

```r
vif(mnb10)
```

```
               GVIF Df GVIF^(1/(2*Df))
yearsquant 1.421248  4        1.044922
subsector  1.591117 15        1.015602
size       1.274339  3        1.041232
```

```r
lrtest(mnb10, mnb1, mnb7, mnb6)
```

```
Likelihood ratio test

Model 1: extortions ~ yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic
Model 4: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -1597.8                    
2  26 -1597.8  1  0.02     0.8875
3  27 -1597.8  1  0.00     1.0000
4  28 -1597.8  1  0.08     0.7773
```

```r
screenreg(list("no bribes" = mnb10, 
               "bus bribes" = mnb1, 
               "b + s bribes" = mnb7,
               "bs_bribes + nbus" = mnb6), 
          single.row = TRUE)
```

```

===================================================================================================
                         no bribes          bus bribes         b + s bribes       bs_bribes + nbus 
---------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.11 (0.53)       -0.11 (0.53)       -0.10 (0.54)  
yearsquant(8,16]             0.17 (0.29)        0.16 (0.29)        0.16 (0.29)        0.16 (0.29)  
yearsquant(16,25]           -0.08 (0.28)       -0.08 (0.28)       -0.08 (0.28)       -0.09 (0.28)  
yearsquant(25,34]            0.42 (0.28)        0.43 (0.28)        0.43 (0.28)        0.43 (0.28)  
yearsquant(34,43]            0.20 (0.29)        0.20 (0.29)        0.20 (0.29)        0.21 (0.29)  
subsectorMining             -1.48 (0.97)       -1.48 (0.97)       -1.48 (0.97)       -1.51 (0.98)  
subsectorConstruction       -0.01 (0.53)       -0.01 (0.53)       -0.01 (0.53)       -0.02 (0.54)  
subsectorManufacturing      -0.27 (0.51)       -0.27 (0.52)       -0.27 (0.52)       -0.28 (0.52)  
subsectorWholesale          -0.63 (0.64)       -0.63 (0.64)       -0.63 (0.64)       -0.63 (0.64)  
subsectorTransport          -0.60 (0.60)       -0.60 (0.60)       -0.60 (0.60)       -0.59 (0.61)  
subsectorMedia              -0.36 (0.80)       -0.36 (0.80)       -0.36 (0.80)       -0.37 (0.80)  
subsectorFinance             0.00 (0.86)        0.01 (0.86)        0.01 (0.86)        0.02 (0.86)  
subsectorReal estate        -1.35 (0.87)       -1.35 (0.87)       -1.35 (0.87)       -1.36 (0.87)  
subsectorProf. services      1.07 (0.83)        1.07 (0.83)        1.07 (0.83)        1.04 (0.83)  
subsectorMaintenance        -0.66 (0.57)       -0.65 (0.57)       -0.65 (0.57)       -0.67 (0.58)  
subsectorEducation           0.02 (0.85)        0.03 (0.85)        0.03 (0.85)        0.03 (0.85)  
subsectorHealth             -0.54 (0.53)       -0.53 (0.53)       -0.53 (0.53)       -0.54 (0.53)  
subsectorLeisure             0.25 (0.82)        0.25 (0.82)        0.25 (0.82)        0.25 (0.82)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.43 (0.53)       -0.43 (0.53)       -0.44 (0.53)  
subsectorOther               0.57 (1.40)        0.57 (1.40)        0.57 (1.40)        0.57 (1.40)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)       -0.25 (0.26)       -0.25 (0.26)  
sizeSmall                   -0.49 (0.26)       -0.50 (0.26)       -0.50 (0.26)       -0.49 (0.26)  
sizeMicro                   -0.61 (0.26) *     -0.60 (0.26) *     -0.60 (0.26) *     -0.60 (0.26) *
bribes                                         -0.01 (0.10)       -0.01 (0.10)       -0.01 (0.10)  
log_bribe_vic                                                     -0.00 (0.22)       -0.03 (0.23)  
log_nbus                                                                              0.11 (0.41)  
---------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00         
Dispersion: parameter        0.06               0.06               0.06               0.06         
Dispersion: SD               0.00               0.00               0.00               0.00         
AIC                       3245.62            3247.60            3249.60            3251.52         
BIC                       3390.33            3398.09            3405.88            3413.59         
Log Likelihood           -1597.81           -1597.80           -1597.80           -1597.76         
Num. obs.                 2412               2412               2412               2412            
Num. groups: state          32                 32                 32                 32            
===================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(mnb10, mnb9, mnb8, mnb6)
```

```
Likelihood ratio test

Model 1: extortions ~ yearsquant + subsector + size + (1 | state)
Model 2: extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic
Model 3: extortions ~ yearsquant + subsector + size + (1 | state) + log_bribe_vic + 
    log_nbus
Model 4: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -1597.8                    
2  26 -1597.8  1  0.00     1.0000
3  27 -1597.8  1  0.08     0.7773
4  28 -1597.8  1  0.02     0.8875
```

```r
screenreg(list("no bribes" = mnb10, 
               "state bribes" = mnb9, 
               "s bribes + nbus" = mnb8,
               "bs_bribes + nbus" = mnb6), 
          single.row = TRUE)
```

```

===================================================================================================
                         no bribes          state bribes       s bribes + nbus    bs_bribes + nbus 
---------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.11 (0.53)       -0.10 (0.54)       -0.10 (0.54)  
yearsquant(8,16]             0.17 (0.29)        0.17 (0.29)        0.17 (0.29)        0.16 (0.29)  
yearsquant(16,25]           -0.08 (0.28)       -0.08 (0.28)       -0.09 (0.28)       -0.09 (0.28)  
yearsquant(25,34]            0.42 (0.28)        0.42 (0.28)        0.43 (0.28)        0.43 (0.28)  
yearsquant(34,43]            0.20 (0.29)        0.20 (0.29)        0.21 (0.29)        0.21 (0.29)  
subsectorMining             -1.48 (0.97)       -1.48 (0.97)       -1.51 (0.98)       -1.51 (0.98)  
subsectorConstruction       -0.01 (0.53)       -0.01 (0.53)       -0.03 (0.53)       -0.02 (0.54)  
subsectorManufacturing      -0.27 (0.51)       -0.27 (0.51)       -0.29 (0.52)       -0.28 (0.52)  
subsectorWholesale          -0.63 (0.64)       -0.63 (0.64)       -0.63 (0.64)       -0.63 (0.64)  
subsectorTransport          -0.60 (0.60)       -0.60 (0.60)       -0.59 (0.61)       -0.59 (0.61)  
subsectorMedia              -0.36 (0.80)       -0.36 (0.80)       -0.37 (0.80)       -0.37 (0.80)  
subsectorFinance             0.00 (0.86)        0.00 (0.86)        0.01 (0.86)        0.02 (0.86)  
subsectorReal estate        -1.35 (0.87)       -1.35 (0.87)       -1.36 (0.87)       -1.36 (0.87)  
subsectorProf. services      1.07 (0.83)        1.07 (0.83)        1.04 (0.83)        1.04 (0.83)  
subsectorMaintenance        -0.66 (0.57)       -0.66 (0.57)       -0.68 (0.57)       -0.67 (0.58)  
subsectorEducation           0.02 (0.85)        0.02 (0.85)        0.03 (0.85)        0.03 (0.85)  
subsectorHealth             -0.54 (0.53)       -0.54 (0.53)       -0.54 (0.53)       -0.54 (0.53)  
subsectorLeisure             0.25 (0.82)        0.25 (0.82)        0.25 (0.82)        0.25 (0.82)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.43 (0.53)       -0.45 (0.53)       -0.44 (0.53)  
subsectorOther               0.57 (1.40)        0.57 (1.40)        0.57 (1.40)        0.57 (1.40)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)       -0.25 (0.26)       -0.25 (0.26)  
sizeSmall                   -0.49 (0.26)       -0.49 (0.26)       -0.49 (0.26)       -0.49 (0.26)  
sizeMicro                   -0.61 (0.26) *     -0.61 (0.26) *     -0.61 (0.26) *     -0.60 (0.26) *
log_bribe_vic                                  -0.00 (0.22)       -0.02 (0.23)       -0.03 (0.23)  
log_nbus                                                           0.11 (0.41)        0.11 (0.41)  
bribes                                                                               -0.01 (0.10)  
---------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00         
Dispersion: parameter        0.06               0.06               0.06               0.06         
Dispersion: SD               0.00               0.00               0.00               0.00         
AIC                       3245.62            3247.62            3249.54            3251.52         
BIC                       3390.33            3398.11            3405.82            3413.59         
Log Likelihood           -1597.81           -1597.81           -1597.77           -1597.76         
Num. obs.                 2412               2412               2412               2412            
Num. groups: state          32                 32                 32                 32            
===================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Include with log_deaths and pop

mnb11 <- update(mnb3, . ~ . + log_bribe_vic + log_nbus)

summary(mnb11)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths + log_pop + log_bribe_vic + 
    log_nbus, data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3253.2 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.08249    0.53207   -0.16    0.877  
bribes                  -0.00635    0.09698   -0.07    0.948  
yearsquant(8,16]         0.14408    0.29468    0.49    0.625  
yearsquant(16,25]       -0.08028    0.28211   -0.28    0.776  
yearsquant(25,34]        0.40801    0.28567    1.43    0.153  
yearsquant(34,43]        0.16578    0.29382    0.56    0.573  
subsectorMining         -1.39625    0.97525   -1.43    0.152  
subsectorConstruction    0.01109    0.53316    0.02    0.983  
subsectorManufacturing  -0.26778    0.51622   -0.52    0.604  
subsectorWholesale      -0.47746    0.64773   -0.74    0.461  
subsectorTransport      -0.58333    0.60637   -0.96    0.336  
subsectorMedia          -0.28572    0.80433   -0.36    0.722  
subsectorFinance         0.06528    0.86048    0.08    0.940  
subsectorReal estate    -1.16359    0.87601   -1.33    0.184  
subsectorProf. services  1.09902    0.83159    1.32    0.186  
subsectorMaintenance    -0.58746    0.57775   -1.02    0.309  
subsectorEducation       0.21616    0.85835    0.25    0.801  
subsectorHealth         -0.47419    0.53001   -0.89    0.371  
subsectorLeisure         0.45326    0.82857    0.55    0.584  
subsectorHotelsRestBar  -0.41800    0.53311   -0.78    0.433  
subsectorOther           0.65903    1.40330    0.47    0.639  
sizeMedium              -0.25573    0.25900   -0.99    0.323  
sizeSmall               -0.48043    0.26166   -1.84    0.066 .
sizeMicro               -0.62366    0.25980   -2.40    0.016 *
log_deaths               0.11859    0.12507    0.95    0.343  
log_pop                  0.03401    0.19852    0.17    0.864  
log_bribe_vic           -0.13888    0.25153   -0.55    0.581  
log_nbus                 0.27771    0.42581    0.65    0.514  
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
(Intercept) 2.093e-09 4.574e-05

Negative binomial dispersion parameter: 0.061241 (std. err.: 0.0045744)

Log-likelihood: -1596.61 
```

```r
get_glmmadmb(mnb11)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_bribe_vic + log_nbus

$logLik
[1] -1596.61

$df
[1] 30

$AIC
[1] 3253.22

$BIC
[1] 3426.866

$alpha
[1] 16.32893

$var_j
[1] 4.378975e-18

$ICC
[1] 2.681728e-19
```

```r
vif(mnb11)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.179929  1        1.086245
yearsquant    1.619165  4        1.062090
subsector     2.326655 15        1.028548
size          1.386601  3        1.055987
log_deaths    1.150140  1        1.072446
log_pop       1.100125  1        1.048868
log_bribe_vic 1.096134  1        1.046964
log_nbus      1.129653  1        1.062851
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
mnb12 <- update(mnb11,  . ~ . - log_nbus)

summary(mnb12)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths + log_pop + log_bribe_vic, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3251.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.10576    0.53110   -0.20    0.842  
bribes                  -0.00579    0.09737   -0.06    0.953  
yearsquant(8,16]         0.14614    0.29454    0.50    0.620  
yearsquant(16,25]       -0.06739    0.28157   -0.24    0.811  
yearsquant(25,34]        0.39811    0.28546    1.39    0.163  
yearsquant(34,43]        0.16234    0.29393    0.55    0.581  
subsectorMining         -1.34461    0.97264   -1.38    0.167  
subsectorConstruction    0.04475    0.53125    0.08    0.933  
subsectorManufacturing  -0.24220    0.51467   -0.47    0.638  
subsectorWholesale      -0.48432    0.64730   -0.75    0.454  
subsectorTransport      -0.61051    0.60553   -1.01    0.313  
subsectorMedia          -0.27693    0.80427   -0.34    0.731  
subsectorFinance         0.02784    0.85803    0.03    0.974  
subsectorReal estate    -1.15403    0.87662   -1.32    0.188  
subsectorProf. services  1.15925    0.82736    1.40    0.161  
subsectorMaintenance    -0.55984    0.57689   -0.97    0.332  
subsectorEducation       0.17419    0.85609    0.20    0.839  
subsectorHealth         -0.48045    0.53002   -0.91    0.365  
subsectorLeisure         0.43586    0.82864    0.53    0.599  
subsectorHotelsRestBar  -0.38334    0.53105   -0.72    0.470  
subsectorOther           0.64916    1.40360    0.46    0.644  
sizeMedium              -0.25470    0.25914   -0.98    0.326  
sizeSmall               -0.48697    0.26138   -1.86    0.062 .
sizeMicro               -0.62299    0.25989   -2.40    0.017 *
log_deaths               0.11044    0.12433    0.89    0.374  
log_pop                  0.02129    0.19756    0.11    0.914  
log_bribe_vic           -0.06702    0.22655   -0.30    0.767  
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
(Intercept) 2.087e-09 4.568e-05

Negative binomial dispersion parameter: 0.061182 (std. err.: 0.0045693)

Log-likelihood: -1596.82 
```

```r
get_glmmadmb(mnb12)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_bribe_vic

$logLik
[1] -1596.82

$df
[1] 29

$AIC
[1] 3251.64

$BIC
[1] 3419.498

$alpha
[1] 16.34468

$var_j
[1] 4.355569e-18

$ICC
[1] 2.664824e-19
```

```r
vif(mnb12)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.184391  1        1.088297
yearsquant    1.606283  4        1.061030
subsector     2.108754 15        1.025182
size          1.386047  3        1.055917
log_deaths    1.146682  1        1.070833
log_pop       1.095507  1        1.046665
log_bribe_vic 1.100067  1        1.048841
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
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_bribe_vic
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  28 -1596.9                    
2  29 -1596.8  1  0.10     0.7518
3  30 -1596.6  1  0.42     0.5169
```

```r
screenreg(list("deaths + pop" = mnb3,
               "+ s bribes" = mnb12, 
               "+ nbus" = mnb11),
          single.row = TRUE)
```

```

================================================================================
                         deaths + pop       + s bribes         + nbus           
--------------------------------------------------------------------------------
(Intercept)                 -0.10 (0.53)       -0.11 (0.53)       -0.08 (0.53)  
bribes                      -0.00 (0.10)       -0.01 (0.10)       -0.01 (0.10)  
yearsquant(8,16]             0.14 (0.29)        0.15 (0.29)        0.14 (0.29)  
yearsquant(16,25]           -0.07 (0.28)       -0.07 (0.28)       -0.08 (0.28)  
yearsquant(25,34]            0.41 (0.28)        0.40 (0.29)        0.41 (0.29)  
yearsquant(34,43]            0.16 (0.29)        0.16 (0.29)        0.17 (0.29)  
subsectorMining             -1.33 (0.97)       -1.34 (0.97)       -1.40 (0.98)  
subsectorConstruction        0.05 (0.53)        0.04 (0.53)        0.01 (0.53)  
subsectorManufacturing      -0.24 (0.51)       -0.24 (0.51)       -0.27 (0.52)  
subsectorWholesale          -0.49 (0.65)       -0.48 (0.65)       -0.48 (0.65)  
subsectorTransport          -0.61 (0.61)       -0.61 (0.61)       -0.58 (0.61)  
subsectorMedia              -0.28 (0.80)       -0.28 (0.80)       -0.29 (0.80)  
subsectorFinance             0.05 (0.86)        0.03 (0.86)        0.07 (0.86)  
subsectorReal estate        -1.18 (0.87)       -1.15 (0.88)       -1.16 (0.88)  
subsectorProf. services      1.15 (0.83)        1.16 (0.83)        1.10 (0.83)  
subsectorMaintenance        -0.57 (0.58)       -0.56 (0.58)       -0.59 (0.58)  
subsectorEducation           0.16 (0.86)        0.17 (0.86)        0.22 (0.86)  
subsectorHealth             -0.48 (0.53)       -0.48 (0.53)       -0.47 (0.53)  
subsectorLeisure             0.43 (0.83)        0.44 (0.83)        0.45 (0.83)  
subsectorHotelsRestBar      -0.39 (0.53)       -0.38 (0.53)       -0.42 (0.53)  
subsectorOther               0.65 (1.40)        0.65 (1.40)        0.66 (1.40)  
sizeMedium                  -0.25 (0.26)       -0.25 (0.26)       -0.26 (0.26)  
sizeSmall                   -0.49 (0.26)       -0.49 (0.26)       -0.48 (0.26)  
sizeMicro                   -0.63 (0.26) *     -0.62 (0.26) *     -0.62 (0.26) *
log_deaths                   0.11 (0.12)        0.11 (0.12)        0.12 (0.13)  
log_pop                      0.01 (0.19)        0.02 (0.20)        0.03 (0.20)  
log_bribe_vic                                  -0.07 (0.23)       -0.14 (0.25)  
log_nbus                                                           0.28 (0.43)  
--------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00         
Dispersion: parameter        0.06               0.06               0.06         
Dispersion: SD               0.00               0.00               0.00         
AIC                       3249.74            3251.64            3253.22         
BIC                       3411.81            3419.50            3426.87         
Log Likelihood           -1596.87           -1596.82           -1596.61         
Num. obs.                 2412               2412               2412            
Num. groups: state          32                 32                 32            
================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```


```r
# firearms-related federal crimes

mnb13 <- update(mnb1, . ~ . + log_wpn_crim)

summary(mnb13)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_wpn_crim, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3248.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11399    0.53188   -0.21    0.830  
bribes                  -0.00983    0.09686   -0.10    0.919  
yearsquant(8,16]         0.15133    0.29377    0.52    0.606  
yearsquant(16,25]       -0.07158    0.27904   -0.26    0.798  
yearsquant(25,34]        0.42958    0.28299    1.52    0.129  
yearsquant(34,43]        0.16865    0.29504    0.57    0.568  
subsectorMining         -1.42253    0.96836   -1.47    0.142  
subsectorConstruction    0.02301    0.53114    0.04    0.965  
subsectorManufacturing  -0.23992    0.51524   -0.47    0.641  
subsectorWholesale      -0.54887    0.64480   -0.85    0.395  
subsectorTransport      -0.59743    0.60415   -0.99    0.323  
subsectorMedia          -0.30591    0.80460   -0.38    0.704  
subsectorFinance         0.02233    0.85731    0.03    0.979  
subsectorReal estate    -1.26955    0.86945   -1.46    0.144  
subsectorProf. services  1.13758    0.82789    1.37    0.169  
subsectorMaintenance    -0.56778    0.57988   -0.98    0.328  
subsectorEducation       0.13097    0.85406    0.15    0.878  
subsectorHealth         -0.49970    0.53013   -0.94    0.346  
subsectorLeisure         0.34544    0.82453    0.42    0.675  
subsectorHotelsRestBar  -0.40450    0.53055   -0.76    0.446  
subsectorOther           0.62120    1.40440    0.44    0.658  
sizeMedium              -0.24849    0.25814   -0.96    0.336  
sizeSmall               -0.49244    0.26091   -1.89    0.059 .
sizeMicro               -0.60624    0.25885   -2.34    0.019 *
log_wpn_crim             0.08881    0.09933    0.89    0.371  
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
(Intercept) 2.097e-09 4.58e-05

Negative binomial dispersion parameter: 0.060994 (std. err.: 0.0045515)

Log-likelihood: -1597.4 
```

```r
get_glmmadmb(mnb13)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim

$logLik
[1] -1597.4

$df
[1] 27

$AIC
[1] 3248.8

$BIC
[1] 3405.082

$alpha
[1] 16.39506

$var_j
[1] 4.399506e-18

$ICC
[1] 2.683435e-19
```

```r
vif(mnb13)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.179737  1        1.086157
yearsquant   1.525435  4        1.054203
subsector    1.901557 15        1.021654
size         1.346064  3        1.050778
log_wpn_crim 1.156253  1        1.075292
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
1  26 -1597.8                    
2  27 -1597.4  1   0.8     0.3711
```

```r
mnb14 <- update(mnb4, . ~ . + log_wpn_crim)

summary(mnb14)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths + log_wpn_crim, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3249.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.0814     0.5303   -0.15    0.878  
bribes                   -0.0022     0.0965   -0.02    0.982  
yearsquant(8,16]          0.1432     0.2938    0.49    0.626  
yearsquant(16,25]        -0.0812     0.2789   -0.29    0.771  
yearsquant(25,34]         0.3814     0.2857    1.33    0.182  
yearsquant(34,43]         0.1850     0.2961    0.63    0.532  
subsectorMining          -1.2722     0.9738   -1.31    0.191  
subsectorConstruction     0.0557     0.5307    0.11    0.916  
subsectorManufacturing   -0.2560     0.5141   -0.50    0.619  
subsectorWholesale       -0.4787     0.6479   -0.74    0.460  
subsectorTransport       -0.6358     0.6041   -1.05    0.293  
subsectorMedia           -0.2831     0.8034   -0.35    0.725  
subsectorFinance          0.0592     0.8562    0.07    0.945  
subsectorReal estate     -1.1391     0.8748   -1.30    0.193  
subsectorProf. services   1.1125     0.8259    1.35    0.178  
subsectorMaintenance     -0.6260     0.5805   -1.08    0.281  
subsectorEducation        0.1155     0.8536    0.14    0.892  
subsectorHealth          -0.4784     0.5300   -0.90    0.367  
subsectorLeisure          0.4339     0.8265    0.52    0.600  
subsectorHotelsRestBar   -0.3960     0.5300   -0.75    0.455  
subsectorOther            0.6338     1.4023    0.45    0.651  
sizeMedium               -0.2587     0.2579   -1.00    0.316  
sizeSmall                -0.4935     0.2608   -1.89    0.058 .
sizeMicro                -0.6488     0.2602   -2.49    0.013 *
log_deaths                0.2277     0.1826    1.25    0.212  
log_wpn_crim             -0.1471     0.2128   -0.69    0.489  
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
(Intercept) 1.903e-08 0.000138

Negative binomial dispersion parameter: 0.061229 (std. err.: 0.0045728)

Log-likelihood: -1596.63 
```

```r
get_glmmadmb(mnb14)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_wpn_crim

$logLik
[1] -1596.63

$df
[1] 28

$AIC
[1] 3249.26

$BIC
[1] 3411.33

$alpha
[1] 16.33213

$var_j
[1] 3.622932e-16

$ICC
[1] 2.218285e-17
```

```r
vif(mnb14)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.173933  1        1.083482
yearsquant   1.574937  4        1.058420
subsector    2.136334 15        1.025626
size         1.361544  3        1.052782
log_deaths   1.152513  1        1.073551
log_wpn_crim 1.193257  1        1.092363
```

```r
vif(mnb14)^2
```

```
                 GVIF  Df GVIF^(1/(2*Df))
bribes       1.378120   1        1.173933
yearsquant   2.480428  16        1.120252
subsector    4.563923 225        1.051908
size         1.853801   9        1.108351
log_deaths   1.328286   1        1.152513
log_wpn_crim 1.423862   1        1.193257
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
mnb15 <- update(mnb3, . ~ . + log_wpn_crim)

summary(mnb15)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_deaths + log_pop + log_wpn_crim, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3251.2 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.08285    0.53015   -0.16    0.876  
bribes                  -0.00304    0.09675   -0.03    0.975  
yearsquant(8,16]         0.14130    0.29432    0.48    0.631  
yearsquant(16,25]       -0.08503    0.28098   -0.30    0.762  
yearsquant(25,34]        0.38298    0.28603    1.34    0.181  
yearsquant(34,43]        0.18528    0.29611    0.63    0.532  
subsectorMining         -1.27240    0.97365   -1.31    0.191  
subsectorConstruction    0.05750    0.53088    0.11    0.914  
subsectorManufacturing  -0.25353    0.51447   -0.49    0.622  
subsectorWholesale      -0.47883    0.64753   -0.74    0.460  
subsectorTransport      -0.63131    0.60544   -1.04    0.297  
subsectorMedia          -0.27982    0.80379   -0.35    0.728  
subsectorFinance         0.05881    0.85601    0.07    0.945  
subsectorReal estate    -1.14178    0.87504   -1.30    0.192  
subsectorProf. services  1.11735    0.82695    1.35    0.177  
subsectorMaintenance    -0.62255    0.58123   -1.07    0.284  
subsectorEducation       0.12338    0.85676    0.14    0.885  
subsectorHealth         -0.47683    0.53005   -0.90    0.368  
subsectorLeisure         0.43771    0.82722    0.53    0.597  
subsectorHotelsRestBar  -0.39456    0.53012   -0.74    0.457  
subsectorOther           0.63860    1.40300    0.46    0.649  
sizeMedium              -0.25594    0.25912   -0.99    0.323  
sizeSmall               -0.49356    0.26084   -1.89    0.058 .
sizeMicro               -0.64995    0.26043   -2.50    0.013 *
log_deaths               0.21947    0.19689    1.11    0.265  
log_pop                  0.02147    0.19326    0.11    0.912  
log_wpn_crim            -0.14980    0.21430   -0.70    0.485  
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
(Intercept) 2.198e-09 4.688e-05

Negative binomial dispersion parameter: 0.061228 (std. err.: 0.0045726)

Log-likelihood: -1596.62 
```

```r
get_glmmadmb(mnb15)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_wpn_crim

$logLik
[1] -1596.62

$df
[1] 29

$AIC
[1] 3251.24

$BIC
[1] 3419.098

$alpha
[1] 16.3324

$var_j
[1] 4.830764e-18

$ICC
[1] 2.95778e-19
```

```r
vif(mnb15)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.179936  1        1.086249
yearsquant   1.631036  4        1.063060
subsector    2.194002 15        1.026537
size         1.382389  3        1.055452
log_deaths   1.153266  1        1.073902
log_pop      1.100082  1        1.048848
log_wpn_crim 1.186293  1        1.089171
```

```r
vif(mnb15)^2
```

```
                 GVIF  Df GVIF^(1/(2*Df))
bribes       1.392249   1        1.179936
yearsquant   2.660279  16        1.130097
subsector    4.813643 225        1.053778
size         1.910999   9        1.113978
log_deaths   1.330022   1        1.153266
log_pop      1.210181   1        1.100082
log_wpn_crim 1.407290   1        1.186293
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
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_wpn_crim
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  27 -1596.9                    
2  28 -1596.6  1  0.48     0.4884
3  29 -1596.6  1  0.02     0.8875
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
1  28 -1596.9                    
2  29 -1596.6  1   0.5     0.4795
```

```r
# droping deaths

mnb16 <- update(mnb15, . ~ . - log_deaths)

summary(mnb16)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3250.5 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.1169     0.5299   -0.22    0.825  
bribes                   -0.0126     0.0967   -0.13    0.897  
yearsquant(8,16]          0.1413     0.2944    0.48    0.631  
yearsquant(16,25]        -0.0918     0.2812   -0.33    0.744  
yearsquant(25,34]         0.4285     0.2829    1.51    0.130  
yearsquant(34,43]         0.1730     0.2954    0.59    0.558  
subsectorMining          -1.3951     0.9692   -1.44    0.150  
subsectorConstruction     0.0385     0.5316    0.07    0.942  
subsectorManufacturing   -0.2287     0.5149   -0.44    0.657  
subsectorWholesale       -0.5366     0.6437   -0.83    0.404  
subsectorTransport       -0.5806     0.6047   -0.96    0.337  
subsectorMedia           -0.2849     0.8049   -0.35    0.723  
subsectorFinance          0.0290     0.8564    0.03    0.973  
subsectorReal estate     -1.2577     0.8696   -1.45    0.148  
subsectorProf. services   1.1575     0.8280    1.40    0.162  
subsectorMaintenance     -0.5597     0.5793   -0.97    0.334  
subsectorEducation        0.1670     0.8573    0.19    0.846  
subsectorHealth          -0.4879     0.5301   -0.92    0.357  
subsectorLeisure          0.3790     0.8265    0.46    0.647  
subsectorHotelsRestBar   -0.3956     0.5305   -0.75    0.456  
subsectorOther            0.6482     1.4051    0.46    0.645  
sizeMedium               -0.2366     0.2590   -0.91    0.361  
sizeSmall                -0.4925     0.2610   -1.89    0.059 .
sizeMicro                -0.6191     0.2596   -2.38    0.017 *
log_pop                   0.1004     0.1787    0.56    0.574  
log_wpn_crim              0.0357     0.1374    0.26    0.795  
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
(Intercept) 2.088e-09 4.569e-05

Negative binomial dispersion parameter: 0.061027 (std. err.: 0.0045537)

Log-likelihood: -1597.25 
```

```r
get_glmmadmb(mnb16)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim

$logLik
[1] -1597.25

$df
[1] 28

$AIC
[1] 3250.5

$BIC
[1] 3412.57

$alpha
[1] 16.38619

$var_j
[1] 4.358074e-18

$ICC
[1] 2.659602e-19
```

```r
vif(mnb16)
```

```
                 GVIF Df GVIF^(1/(2*Df))
bribes       1.180368  1        1.086447
yearsquant   1.567442  4        1.057789
subsector    1.940653 15        1.022347
size         1.370529  3        1.053937
log_pop      1.115375  1        1.056113
log_wpn_crim 1.131954  1        1.063933
```

```r
vif(mnb16)^2
```

```
                 GVIF  Df GVIF^(1/(2*Df))
bribes       1.393269   1        1.180368
yearsquant   2.456874  16        1.118917
subsector    3.766133 225        1.045193
size         1.878349   9        1.110783
log_pop      1.244062   1        1.115375
log_wpn_crim 1.281320   1        1.131954
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
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_deaths + log_pop + log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  27 -1597.3                    
2  28 -1597.2  1  0.06     0.8065
3  29 -1596.6  1  1.26     0.2617
```

```r
lrtest(mnb13, mnb16)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  27 -1597.4                    
2  28 -1597.2  1   0.3     0.5839
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

====================================================================================================================
                         No state    deaths      deaths +  pop  wpns        wpns + dth  wpns + pop  wpns + dth + pop
--------------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.11       -0.10       -0.10          -0.11       -0.08       -0.12       -0.08        
                            (0.53)      (0.53)      (0.53)         (0.53)      (0.53)      (0.53)      (0.53)       
bribes                      -0.01       -0.00       -0.00          -0.01       -0.00       -0.01       -0.00        
                            (0.10)      (0.10)      (0.10)         (0.10)      (0.10)      (0.10)      (0.10)       
yearsquant(8,16]             0.16        0.14        0.14           0.15        0.14        0.14        0.14        
                            (0.29)      (0.29)      (0.29)         (0.29)      (0.29)      (0.29)      (0.29)       
yearsquant(16,25]           -0.08       -0.07       -0.07          -0.07       -0.08       -0.09       -0.09        
                            (0.28)      (0.28)      (0.28)         (0.28)      (0.28)      (0.28)      (0.28)       
yearsquant(25,34]            0.43        0.41        0.41           0.43        0.38        0.43        0.38        
                            (0.28)      (0.28)      (0.28)         (0.28)      (0.29)      (0.28)      (0.29)       
yearsquant(34,43]            0.20        0.16        0.16           0.17        0.19        0.17        0.19        
                            (0.29)      (0.29)      (0.29)         (0.30)      (0.30)      (0.30)      (0.30)       
subsectorMining             -1.48       -1.33       -1.33          -1.42       -1.27       -1.40       -1.27        
                            (0.97)      (0.97)      (0.97)         (0.97)      (0.97)      (0.97)      (0.97)       
subsectorConstruction       -0.01        0.05        0.05           0.02        0.06        0.04        0.06        
                            (0.53)      (0.53)      (0.53)         (0.53)      (0.53)      (0.53)      (0.53)       
subsectorManufacturing      -0.27       -0.24       -0.24          -0.24       -0.26       -0.23       -0.25        
                            (0.52)      (0.51)      (0.51)         (0.52)      (0.51)      (0.51)      (0.51)       
subsectorWholesale          -0.63       -0.49       -0.49          -0.55       -0.48       -0.54       -0.48        
                            (0.64)      (0.65)      (0.65)         (0.64)      (0.65)      (0.64)      (0.65)       
subsectorTransport          -0.60       -0.62       -0.61          -0.60       -0.64       -0.58       -0.63        
                            (0.60)      (0.60)      (0.61)         (0.60)      (0.60)      (0.60)      (0.61)       
subsectorMedia              -0.36       -0.28       -0.28          -0.31       -0.28       -0.28       -0.28        
                            (0.80)      (0.80)      (0.80)         (0.80)      (0.80)      (0.80)      (0.80)       
subsectorFinance             0.01        0.05        0.05           0.02        0.06        0.03        0.06        
                            (0.86)      (0.86)      (0.86)         (0.86)      (0.86)      (0.86)      (0.86)       
subsectorReal estate        -1.35       -1.18       -1.18          -1.27       -1.14       -1.26       -1.14        
                            (0.87)      (0.87)      (0.87)         (0.87)      (0.87)      (0.87)      (0.88)       
subsectorProf. services      1.07        1.15        1.15           1.14        1.11        1.16        1.12        
                            (0.83)      (0.83)      (0.83)         (0.83)      (0.83)      (0.83)      (0.83)       
subsectorMaintenance        -0.65       -0.57       -0.57          -0.57       -0.63       -0.56       -0.62        
                            (0.57)      (0.58)      (0.58)         (0.58)      (0.58)      (0.58)      (0.58)       
subsectorEducation           0.03        0.16        0.16           0.13        0.12        0.17        0.12        
                            (0.85)      (0.85)      (0.86)         (0.85)      (0.85)      (0.86)      (0.86)       
subsectorHealth             -0.53       -0.48       -0.48          -0.50       -0.48       -0.49       -0.48        
                            (0.53)      (0.53)      (0.53)         (0.53)      (0.53)      (0.53)      (0.53)       
subsectorLeisure             0.25        0.42        0.43           0.35        0.43        0.38        0.44        
                            (0.82)      (0.83)      (0.83)         (0.82)      (0.83)      (0.83)      (0.83)       
subsectorHotelsRestBar      -0.43       -0.39       -0.39          -0.40       -0.40       -0.40       -0.39        
                            (0.53)      (0.53)      (0.53)         (0.53)      (0.53)      (0.53)      (0.53)       
subsectorOther               0.57        0.65        0.65           0.62        0.63        0.65        0.64        
                            (1.40)      (1.40)      (1.40)         (1.40)      (1.40)      (1.41)      (1.40)       
sizeMedium                  -0.25       -0.25       -0.25          -0.25       -0.26       -0.24       -0.26        
                            (0.26)      (0.26)      (0.26)         (0.26)      (0.26)      (0.26)      (0.26)       
sizeSmall                   -0.50       -0.49       -0.49          -0.49       -0.49       -0.49       -0.49        
                            (0.26)      (0.26)      (0.26)         (0.26)      (0.26)      (0.26)      (0.26)       
sizeMicro                   -0.60 *     -0.63 *     -0.63 *        -0.61 *     -0.65 *     -0.62 *     -0.65 *      
                            (0.26)      (0.26)      (0.26)         (0.26)      (0.26)      (0.26)      (0.26)       
log_deaths                               0.12        0.11                       0.23                    0.22        
                                        (0.08)      (0.12)                     (0.18)                  (0.20)       
log_pop                                              0.01                                   0.10        0.02        
                                                    (0.19)                                 (0.18)      (0.19)       
log_wpn_crim                                                        0.09       -0.15        0.04       -0.15        
                                                                   (0.10)      (0.21)      (0.14)      (0.21)       
--------------------------------------------------------------------------------------------------------------------
Variance: state              0.00        0.00        0.00           0.00        0.00        0.00        0.00        
Dispersion: parameter        0.06        0.06        0.06           0.06        0.06        0.06        0.06        
Dispersion: SD               0.00        0.00        0.00           0.00        0.00        0.00        0.00        
AIC                       3247.60     3247.74     3249.74        3248.80     3249.26     3250.50     3251.24        
BIC                       3398.09     3404.02     3411.81        3405.08     3411.33     3412.57     3419.10        
Log Likelihood           -1597.80    -1596.87    -1596.87       -1597.40    -1596.63    -1597.25    -1596.62        
Num. obs.                 2412        2412        2412           2412        2412        2412        2412           
Num. groups: state          32          32          32             32          32          32          32           
====================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## include state bribes var

mnb17 <- update(mnb16, . ~ . + log_bribe_vic)

summary(mnb17)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3252.4 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12546    0.53042   -0.24    0.813  
bribes                  -0.01406    0.09663   -0.15    0.884  
yearsquant(8,16]         0.14526    0.29468    0.49    0.622  
yearsquant(16,25]       -0.08633    0.28176   -0.31    0.759  
yearsquant(25,34]        0.41978    0.28427    1.48    0.140  
yearsquant(34,43]        0.17383    0.29539    0.59    0.556  
subsectorMining         -1.41245    0.97074   -1.46    0.146  
subsectorConstruction    0.03435    0.53168    0.06    0.948  
subsectorManufacturing  -0.23235    0.51499   -0.45    0.652  
subsectorWholesale      -0.53438    0.64382   -0.83    0.407  
subsectorTransport      -0.57583    0.60481   -0.95    0.341  
subsectorMedia          -0.28562    0.80493   -0.35    0.723  
subsectorFinance         0.00994    0.85842    0.01    0.991  
subsectorReal estate    -1.23172    0.87358   -1.41    0.159  
subsectorProf. services  1.16807    0.82860    1.41    0.159  
subsectorMaintenance    -0.55381    0.57927   -0.96    0.339  
subsectorEducation       0.17904    0.85806    0.21    0.835  
subsectorHealth         -0.49176    0.53005   -0.93    0.354  
subsectorLeisure         0.38984    0.82735    0.47    0.638  
subsectorHotelsRestBar  -0.38550    0.53147   -0.73    0.468  
subsectorOther           0.65035    1.40500    0.46    0.643  
sizeMedium              -0.23813    0.25901   -0.92    0.358  
sizeSmall               -0.48822    0.26148   -1.87    0.062 .
sizeMicro               -0.61347    0.26012   -2.36    0.018 *
log_pop                  0.12120    0.19049    0.64    0.525  
log_wpn_crim             0.02762    0.13928    0.20    0.843  
log_bribe_vic           -0.07117    0.22917   -0.31    0.756  
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
(Intercept) 2.075e-09 4.555e-05

Negative binomial dispersion parameter: 0.061048 (std. err.: 0.004556)

Log-likelihood: -1597.2 
```

```r
get_glmmadmb(mnb17)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic

$logLik
[1] -1597.2

$df
[1] 29

$AIC
[1] 3252.4

$BIC
[1] 3420.258

$alpha
[1] 16.38055

$var_j
[1] 4.305625e-18

$ICC
[1] 2.628498e-19
```

```r
vif(mnb17)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.178128  1        1.085416
yearsquant    1.601049  4        1.060597
subsector     2.043695 15        1.024111
size          1.381818  3        1.055379
log_pop       1.118234  1        1.057466
log_wpn_crim  1.130253  1        1.063133
log_bribe_vic 1.087246  1        1.042711
```

```r
mnb18 <- update(mnb17, . ~ . + log_nbus)

summary(mnb18)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus, data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3254.1 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.10624    0.53122   -0.20    0.841  
bribes                  -0.01495    0.09628   -0.16    0.877  
yearsquant(8,16]         0.14318    0.29477    0.49    0.627  
yearsquant(16,25]       -0.09772    0.28232   -0.35    0.729  
yearsquant(25,34]        0.43016    0.28467    1.51    0.131  
yearsquant(34,43]        0.17681    0.29518    0.60    0.549  
subsectorMining         -1.45999    0.97407   -1.50    0.134  
subsectorConstruction    0.00351    0.53378    0.01    0.995  
subsectorManufacturing  -0.25356    0.51631   -0.49    0.623  
subsectorWholesale      -0.52937    0.64416   -0.82    0.411  
subsectorTransport      -0.55027    0.60591   -0.91    0.364  
subsectorMedia          -0.29226    0.80505   -0.36    0.717  
subsectorFinance         0.04142    0.86050    0.05    0.962  
subsectorReal estate    -1.24386    0.87320   -1.42    0.154  
subsectorProf. services  1.11560    0.83265    1.34    0.180  
subsectorMaintenance    -0.57497    0.57976   -0.99    0.321  
subsectorEducation       0.21913    0.86076    0.25    0.799  
subsectorHealth         -0.48555    0.53010   -0.92    0.360  
subsectorLeisure         0.40391    0.82731    0.49    0.625  
subsectorHotelsRestBar  -0.41611    0.53352   -0.78    0.435  
subsectorOther           0.66191    1.40510    0.47    0.638  
sizeMedium              -0.23801    0.25886   -0.92    0.358  
sizeSmall               -0.48256    0.26174   -1.84    0.065 .
sizeMicro               -0.61297    0.26002   -2.36    0.018 *
log_pop                  0.13475    0.19215    0.70    0.483  
log_wpn_crim             0.03447    0.14032    0.25    0.806  
log_bribe_vic           -0.13323    0.25274   -0.53    0.598  
log_nbus                 0.24579    0.42337    0.58    0.562  
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
(Intercept) 2.103e-09 4.586e-05

Negative binomial dispersion parameter: 0.061093 (std. err.: 0.0045597)

Log-likelihood: -1597.03 
```

```r
get_glmmadmb(mnb18)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus

$logLik
[1] -1597.03

$df
[1] 30

$AIC
[1] 3254.06

$BIC
[1] 3427.706

$alpha
[1] 16.36849

$var_j
[1] 4.422188e-18

$ICC
[1] 2.701648e-19
```

```r
vif(mnb18)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.174325  1        1.083663
yearsquant    1.616482  4        1.061870
subsector     2.252835 15        1.027443
size          1.381143  3        1.055293
log_pop       1.127632  1        1.061900
log_wpn_crim  1.127268  1        1.061729
log_bribe_vic 1.084610  1        1.041446
log_nbus      1.133448  1        1.064635
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
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus
  #Df  LogLik Df Chisq Pr(>Chisq)
1  28 -1597.2                    
2  29 -1597.2  1  0.10     0.7518
3  30 -1597.0  1  0.34     0.5598
```

```r
mnb19 <- update(mnb17, . ~ . - log_pop)

summary(mnb19)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_wpn_crim + log_bribe_vic, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3250.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.1157     0.5323   -0.22     0.83  
bribes                   -0.0101     0.0969   -0.10     0.92  
yearsquant(8,16]          0.1529     0.2943    0.52     0.60  
yearsquant(16,25]        -0.0690     0.2806   -0.25     0.81  
yearsquant(25,34]         0.4273     0.2842    1.50     0.13  
yearsquant(34,43]         0.1685     0.2950    0.57     0.57  
subsectorMining          -1.4293     0.9713   -1.47     0.14  
subsectorConstruction     0.0207     0.5317    0.04     0.97  
subsectorManufacturing   -0.2419     0.5157   -0.47     0.64  
subsectorWholesale       -0.5494     0.6449   -0.85     0.39  
subsectorTransport       -0.5974     0.6041   -0.99     0.32  
subsectorMedia           -0.3077     0.8049   -0.38     0.70  
subsectorFinance          0.0163     0.8600    0.02     0.98  
subsectorReal estate     -1.2638     0.8718   -1.45     0.15  
subsectorProf. services   1.1390     0.8280    1.38     0.17  
subsectorMaintenance     -0.5670     0.5799   -0.98     0.33  
subsectorEducation        0.1319     0.8540    0.15     0.88  
subsectorHealth          -0.5018     0.5306   -0.95     0.34  
subsectorLeisure          0.3459     0.8245    0.42     0.67  
subsectorHotelsRestBar   -0.4028     0.5309   -0.76     0.45  
subsectorOther            0.6197     1.4045    0.44     0.66  
sizeMedium               -0.2496     0.2584   -0.97     0.33  
sizeSmall                -0.4913     0.2612   -1.88     0.06 .
sizeMicro                -0.6040     0.2600   -2.32     0.02 *
log_wpn_crim              0.0895     0.0996    0.90     0.37  
log_bribe_vic            -0.0194     0.2173   -0.09     0.93  
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
(Intercept) 2.081e-09 4.562e-05

Negative binomial dispersion parameter: 0.060997 (std. err.: 0.004552)

Log-likelihood: -1597.4 
```

```r
get_glmmadmb(mnb19)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic

$logLik
[1] -1597.4

$df
[1] 28

$AIC
[1] 3250.8

$BIC
[1] 3412.87

$alpha
[1] 16.39425

$var_j
[1] 4.330145e-18

$ICC
[1] 2.641258e-19
```

```r
lrtest(mnb19, mnb16)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim
  #Df  LogLik Df Chisq Pr(>Chisq)    
1  28 -1597.4                        
2  28 -1597.2  0   0.3  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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

======================================================================================================================
                         no state           wpns + pop         + s bribes         + nbus             wpns + s bribes  
----------------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.12 (0.53)       -0.13 (0.53)       -0.11 (0.53)       -0.12 (0.53)  
bribes                      -0.01 (0.10)       -0.01 (0.10)       -0.01 (0.10)       -0.01 (0.10)       -0.01 (0.10)  
yearsquant(8,16]             0.16 (0.29)        0.14 (0.29)        0.15 (0.29)        0.14 (0.29)        0.15 (0.29)  
yearsquant(16,25]           -0.08 (0.28)       -0.09 (0.28)       -0.09 (0.28)       -0.10 (0.28)       -0.07 (0.28)  
yearsquant(25,34]            0.43 (0.28)        0.43 (0.28)        0.42 (0.28)        0.43 (0.28)        0.43 (0.28)  
yearsquant(34,43]            0.20 (0.29)        0.17 (0.30)        0.17 (0.30)        0.18 (0.30)        0.17 (0.30)  
subsectorMining             -1.48 (0.97)       -1.40 (0.97)       -1.41 (0.97)       -1.46 (0.97)       -1.43 (0.97)  
subsectorConstruction       -0.01 (0.53)        0.04 (0.53)        0.03 (0.53)        0.00 (0.53)        0.02 (0.53)  
subsectorManufacturing      -0.27 (0.52)       -0.23 (0.51)       -0.23 (0.51)       -0.25 (0.52)       -0.24 (0.52)  
subsectorWholesale          -0.63 (0.64)       -0.54 (0.64)       -0.53 (0.64)       -0.53 (0.64)       -0.55 (0.64)  
subsectorTransport          -0.60 (0.60)       -0.58 (0.60)       -0.58 (0.60)       -0.55 (0.61)       -0.60 (0.60)  
subsectorMedia              -0.36 (0.80)       -0.28 (0.80)       -0.29 (0.80)       -0.29 (0.81)       -0.31 (0.80)  
subsectorFinance             0.01 (0.86)        0.03 (0.86)        0.01 (0.86)        0.04 (0.86)        0.02 (0.86)  
subsectorReal estate        -1.35 (0.87)       -1.26 (0.87)       -1.23 (0.87)       -1.24 (0.87)       -1.26 (0.87)  
subsectorProf. services      1.07 (0.83)        1.16 (0.83)        1.17 (0.83)        1.12 (0.83)        1.14 (0.83)  
subsectorMaintenance        -0.65 (0.57)       -0.56 (0.58)       -0.55 (0.58)       -0.57 (0.58)       -0.57 (0.58)  
subsectorEducation           0.03 (0.85)        0.17 (0.86)        0.18 (0.86)        0.22 (0.86)        0.13 (0.85)  
subsectorHealth             -0.53 (0.53)       -0.49 (0.53)       -0.49 (0.53)       -0.49 (0.53)       -0.50 (0.53)  
subsectorLeisure             0.25 (0.82)        0.38 (0.83)        0.39 (0.83)        0.40 (0.83)        0.35 (0.82)  
subsectorHotelsRestBar      -0.43 (0.53)       -0.40 (0.53)       -0.39 (0.53)       -0.42 (0.53)       -0.40 (0.53)  
subsectorOther               0.57 (1.40)        0.65 (1.41)        0.65 (1.40)        0.66 (1.41)        0.62 (1.40)  
sizeMedium                  -0.25 (0.26)       -0.24 (0.26)       -0.24 (0.26)       -0.24 (0.26)       -0.25 (0.26)  
sizeSmall                   -0.50 (0.26)       -0.49 (0.26)       -0.49 (0.26)       -0.48 (0.26)       -0.49 (0.26)  
sizeMicro                   -0.60 (0.26) *     -0.62 (0.26) *     -0.61 (0.26) *     -0.61 (0.26) *     -0.60 (0.26) *
log_pop                                         0.10 (0.18)        0.12 (0.19)        0.13 (0.19)                     
log_wpn_crim                                    0.04 (0.14)        0.03 (0.14)        0.03 (0.14)        0.09 (0.10)  
log_bribe_vic                                                     -0.07 (0.23)       -0.13 (0.25)       -0.02 (0.22)  
log_nbus                                                                              0.25 (0.42)                     
----------------------------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00               0.00         
Dispersion: parameter        0.06               0.06               0.06               0.06               0.06         
Dispersion: SD               0.00               0.00               0.00               0.00               0.00         
AIC                       3247.60            3250.50            3252.40            3254.06            3250.80         
BIC                       3398.09            3412.57            3420.26            3427.71            3412.87         
Log Likelihood           -1597.80           -1597.25           -1597.20           -1597.03           -1597.40         
Num. obs.                 2412               2412               2412               2412               2412            
Num. groups: state          32                 32                 32                 32                 32            
======================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```


```r
# drugs-related federal crimes

mnb20 <- update(mnb18, . ~ . + log_drug_crim)

summary(mnb20)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3256 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11166    0.53477   -0.21    0.835  
bribes                  -0.01385    0.09716   -0.14    0.887  
yearsquant(8,16]         0.14384    0.29485    0.49    0.626  
yearsquant(16,25]       -0.09700    0.28242   -0.34    0.731  
yearsquant(25,34]        0.42879    0.28510    1.50    0.133  
yearsquant(34,43]        0.17982    0.29722    0.61    0.545  
subsectorMining         -1.46126    0.97418   -1.50    0.134  
subsectorConstruction    0.00179    0.53413    0.00    0.997  
subsectorManufacturing  -0.25490    0.51653   -0.49    0.622  
subsectorWholesale      -0.53015    0.64412   -0.82    0.410  
subsectorTransport      -0.54879    0.60614   -0.91    0.365  
subsectorMedia          -0.29216    0.80507   -0.36    0.717  
subsectorFinance         0.05005    0.86633    0.06    0.954  
subsectorReal estate    -1.24507    0.87343   -1.43    0.154  
subsectorProf. services  1.11675    0.83262    1.34    0.180  
subsectorMaintenance    -0.57543    0.57988   -0.99    0.321  
subsectorEducation       0.21834    0.86099    0.25    0.800  
subsectorHealth         -0.48743    0.53062   -0.92    0.358  
subsectorLeisure         0.41312    0.83423    0.50    0.620  
subsectorHotelsRestBar  -0.41369    0.53429   -0.77    0.439  
subsectorOther           0.66373    1.40520    0.47    0.637  
sizeMedium              -0.23862    0.25893   -0.92    0.357  
sizeSmall               -0.48117    0.26227   -1.83    0.067 .
sizeMicro               -0.61378    0.26018   -2.36    0.018 *
log_pop                  0.13479    0.19223    0.70    0.483  
log_wpn_crim             0.04224    0.16661    0.25    0.800  
log_bribe_vic           -0.13290    0.25309   -0.53    0.599  
log_nbus                 0.23599    0.43834    0.54    0.590  
log_drug_crim           -0.01084    0.12572   -0.09    0.931  
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
(Intercept) 2.08e-09 4.561e-05

Negative binomial dispersion parameter: 0.061094 (std. err.: 0.0045599)

Log-likelihood: -1597.02 
```

```r
get_glmmadmb(mnb20)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1597.02

$df
[1] 31

$AIC
[1] 3256.04

$BIC
[1] 3435.475

$alpha
[1] 16.36822

$var_j
[1] 4.3264e-18

$ICC
[1] 2.643171e-19
```

```r
vif(mnb20)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.202470  1        1.096572
yearsquant    1.671935  4        1.066357
subsector     2.443187 15        1.030225
size          1.391507  3        1.056609
log_pop       1.129681  1        1.062864
log_wpn_crim  1.128579  1        1.062346
log_bribe_vic 1.085933  1        1.042081
log_nbus      1.142112  1        1.068697
log_drug_crim 1.145025  1        1.070059
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
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
  #Df LogLik Df Chisq Pr(>Chisq)
1  30  -1597                    
2  31  -1597  1  0.02     0.8875
```

```r
# drop log_wpn_crim

mnb21 <- update(mnb20, . ~ . - log_wpn_crim)

summary(mnb21)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_bribe_vic + log_nbus + 
    log_drug_crim, data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3254.1 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.10536    0.53415   -0.20    0.844  
bribes                  -0.01713    0.09598   -0.18    0.858  
yearsquant(8,16]         0.14270    0.29490    0.48    0.628  
yearsquant(16,25]       -0.10406    0.28104   -0.37    0.711  
yearsquant(25,34]        0.42913    0.28498    1.51    0.132  
yearsquant(34,43]        0.18241    0.29720    0.61    0.539  
subsectorMining         -1.46280    0.97420   -1.50    0.133  
subsectorConstruction    0.00416    0.53416    0.01    0.994  
subsectorManufacturing  -0.25417    0.51651   -0.49    0.623  
subsectorWholesale      -0.53822    0.64319   -0.84    0.403  
subsectorTransport      -0.54763    0.60615   -0.90    0.366  
subsectorMedia          -0.29528    0.80496   -0.37    0.714  
subsectorFinance         0.03142    0.86305    0.04    0.971  
subsectorReal estate    -1.24984    0.87329   -1.43    0.152  
subsectorProf. services  1.11212    0.83258    1.34    0.182  
subsectorMaintenance    -0.58483    0.57842   -1.01    0.312  
subsectorEducation       0.21157    0.86059    0.25    0.806  
subsectorHealth         -0.48785    0.53053   -0.92    0.358  
subsectorLeisure         0.39177    0.82979    0.47    0.637  
subsectorHotelsRestBar  -0.41636    0.53433   -0.78    0.436  
subsectorOther           0.65903    1.40520    0.47    0.639  
sizeMedium              -0.23460    0.25856   -0.91    0.364  
sizeSmall               -0.48384    0.26200   -1.85    0.065 .
sizeMicro               -0.61448    0.26009   -2.36    0.018 *
log_pop                  0.16270    0.15730    1.03    0.301  
log_bribe_vic           -0.14044    0.25023   -0.56    0.575  
log_nbus                 0.24333    0.43599    0.56    0.577  
log_drug_crim            0.00638    0.10564    0.06    0.952  
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
(Intercept) 1.882e-08 0.0001372

Negative binomial dispersion parameter: 0.06108 (std. err.: 0.0045583)

Log-likelihood: -1597.06 
```

```r
get_glmmadmb(mnb21)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1597.06

$df
[1] 30

$AIC
[1] 3254.12

$BIC
[1] 3427.766

$alpha
[1] 16.37197

$var_j
[1] 3.540795e-16

$ICC
[1] 2.162718e-17
```

```r
vif(mnb21)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.170181  1        1.081749
yearsquant    1.640644  4        1.063841
subsector     2.308781 15        1.028283
size          1.376858  3        1.054747
log_pop       1.126437  1        1.061337
log_bribe_vic 1.098590  1        1.048137
log_nbus      1.142025  1        1.068656
log_drug_crim 1.151997  1        1.073311
```

```r
vif(mnb21)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.369324   1        1.170181
yearsquant    2.691711  16        1.131758
subsector     5.330471 225        1.057366
size          1.895738   9        1.112491
log_pop       1.268860   1        1.126437
log_bribe_vic 1.206901   1        1.098590
log_nbus      1.304222   1        1.142025
log_drug_crim 1.327098   1        1.151997
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
1  30 -1597.1                    
2  31 -1597.0  1  0.08     0.7773
```

```r
# drop log_drug_crim

mnb22 <- update(mnb21, . ~ . -  log_pop)

summary(mnb22)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_bribe_vic + log_nbus + log_drug_crim, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3253.2 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.07138    0.53818   -0.13    0.894  
bribes                  -0.01843    0.09606   -0.19    0.848  
yearsquant(8,16]         0.15716    0.29461    0.53    0.594  
yearsquant(16,25]       -0.08692    0.28078   -0.31    0.757  
yearsquant(25,34]        0.44222    0.28481    1.55    0.120  
yearsquant(34,43]        0.17960    0.29704    0.60    0.545  
subsectorMining         -1.49055    0.97598   -1.53    0.127  
subsectorConstruction   -0.00994    0.53545   -0.02    0.985  
subsectorManufacturing  -0.26865    0.51854   -0.52    0.604  
subsectorWholesale      -0.59691    0.64289   -0.93    0.353  
subsectorTransport      -0.59438    0.60543   -0.98    0.326  
subsectorMedia          -0.34988    0.80478   -0.43    0.664  
subsectorFinance        -0.01693    0.86557   -0.02    0.984  
subsectorReal estate    -1.32259    0.87066   -1.52    0.129  
subsectorProf. services  1.05272    0.83344    1.26    0.207  
subsectorMaintenance    -0.63804    0.57721   -1.11    0.269  
subsectorEducation       0.08491    0.85008    0.10    0.920  
subsectorHealth         -0.51268    0.53137   -0.96    0.335  
subsectorLeisure         0.23920    0.81708    0.29    0.770  
subsectorHotelsRestBar  -0.45347    0.53443   -0.85    0.396  
subsectorOther           0.57673    1.40400    0.41    0.681  
sizeMedium              -0.24769    0.25863   -0.96    0.338  
sizeSmall               -0.49816    0.26120   -1.91    0.056 .
sizeMicro               -0.59735    0.26008   -2.30    0.022 *
log_bribe_vic           -0.04346    0.23457   -0.19    0.853  
log_nbus                 0.19601    0.43661    0.45    0.653  
log_drug_crim            0.05469    0.09513    0.57    0.565  
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
(Intercept) 6.089e-09 7.803e-05

Negative binomial dispersion parameter: 0.060926 (std. err.: 0.0045451)

Log-likelihood: -1597.6 
```

```r
get_glmmadmb(mnb22)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1597.6

$df
[1] 29

$AIC
[1] 3253.2

$BIC
[1] 3421.058

$alpha
[1] 16.41335

$var_j
[1] 3.707349e-17

$ICC
[1] 2.258739e-18
```

```r
mnb23 <- update(mnb22, . ~ . - log_drug_crim)

lrtest(mnb23, mnb22, mnb21)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_bribe_vic + log_nbus + log_drug_crim
Model 3: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus + log_drug_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  28 -1597.8                    
2  29 -1597.6  1  0.32     0.5716
3  30 -1597.1  1  1.08     0.2987
```

```r
# include deaths 

mnb24 <- update(mnb20, . ~ . + log_deaths)
summary(mnb24)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + log_deaths, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3256.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.06338    0.53572   -0.12    0.906  
bribes                  -0.00579    0.09682   -0.06    0.952  
yearsquant(8,16]         0.14387    0.29496    0.49    0.626  
yearsquant(16,25]       -0.08979    0.28241   -0.32    0.751  
yearsquant(25,34]        0.37812    0.28839    1.31    0.190  
yearsquant(34,43]        0.18521    0.29784    0.62    0.534  
subsectorMining         -1.34285    0.97721   -1.37    0.169  
subsectorConstruction    0.01989    0.53326    0.04    0.970  
subsectorManufacturing  -0.28570    0.51652   -0.55    0.580  
subsectorWholesale      -0.46428    0.64878   -0.72    0.474  
subsectorTransport      -0.60277    0.60656   -0.99    0.320  
subsectorMedia          -0.29007    0.80379   -0.36    0.718  
subsectorFinance         0.05931    0.86607    0.07    0.945  
subsectorReal estate    -1.10758    0.87902   -1.26    0.208  
subsectorProf. services  1.07077    0.83129    1.29    0.198  
subsectorMaintenance    -0.64621    0.58163   -1.11    0.267  
subsectorEducation       0.17957    0.85916    0.21    0.834  
subsectorHealth         -0.47520    0.53047   -0.90    0.370  
subsectorLeisure         0.46552    0.83423    0.56    0.577  
subsectorHotelsRestBar  -0.41562    0.53420   -0.78    0.437  
subsectorOther           0.64643    1.40240    0.46    0.645  
sizeMedium              -0.25996    0.25903   -1.00    0.316  
sizeSmall               -0.48252    0.26216   -1.84    0.066 .
sizeMicro               -0.64363    0.26052   -2.47    0.013 *
log_pop                  0.05966    0.20225    0.29    0.768  
log_wpn_crim            -0.18132    0.24794   -0.73    0.465  
log_bribe_vic           -0.17970    0.25527   -0.70    0.481  
log_nbus                 0.28392    0.44046    0.64    0.519  
log_drug_crim            0.01156    0.12577    0.09    0.927  
log_deaths               0.24129    0.20091    1.20    0.230  
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
(Intercept) 2.078e-09 4.558e-05

Negative binomial dispersion parameter: 0.06133 (std. err.: 0.0045821)

Log-likelihood: -1596.31 
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
[1] -1596.31

$df
[1] 32

$AIC
[1] 3256.62

$BIC
[1] 3441.843

$alpha
[1] 16.30523

$var_j
[1] 4.316422e-18

$ICC
[1] 2.647261e-19
```

```r
vif(mnb24)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.193690  1        1.092561
yearsquant    1.731324  4        1.071019
subsector     2.768668 15        1.034528
size          1.398497  3        1.057492
log_pop       1.131419  1        1.063682
log_wpn_crim  1.128701  1        1.062403
log_bribe_vic 1.087856  1        1.043003
log_nbus      1.139197  1        1.067332
log_drug_crim 1.147050  1        1.071004
log_deaths    1.182213  1        1.087296
```

```r
vif(mnb24)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.424896   1        1.193690
yearsquant    2.997482  16        1.147082
subsector     7.665523 225        1.070249
size          1.955793   9        1.118288
log_pop       1.280110   1        1.131419
log_wpn_crim  1.273966   1        1.128701
log_bribe_vic 1.183430   1        1.087856
log_nbus      1.297769   1        1.139197
log_drug_crim 1.315724   1        1.147050
log_deaths    1.397628   1        1.182213
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

mnb25 <- update(mnb18, . ~ . - log_wpn_crim)
summary(mnb25)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_bribe_vic + log_nbus, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3252.1 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)              -0.1089     0.5308   -0.21    0.837  
bribes                   -0.0167     0.0957   -0.17    0.861  
yearsquant(8,16]          0.1430     0.2948    0.49    0.628  
yearsquant(16,25]        -0.1045     0.2809   -0.37    0.710  
yearsquant(25,34]         0.4280     0.2844    1.51    0.132  
yearsquant(34,43]         0.1853     0.2933    0.63    0.528  
subsectorMining          -1.4640     0.9740   -1.50    0.133  
subsectorConstruction     0.0031     0.5338    0.01    0.995  
subsectorManufacturing   -0.2551     0.5162   -0.49    0.621  
subsectorWholesale       -0.5401     0.6423   -0.84    0.400  
subsectorTransport       -0.5462     0.6057   -0.90    0.367  
subsectorMedia           -0.2957     0.8049   -0.37    0.713  
subsectorFinance          0.0357     0.8601    0.04    0.967  
subsectorReal estate     -1.2515     0.8729   -1.43    0.152  
subsectorProf. services   1.1125     0.8325    1.34    0.181  
subsectorMaintenance     -0.5867     0.5776   -1.02    0.310  
subsectorEducation        0.2099     0.8603    0.24    0.807  
subsectorHealth          -0.4895     0.5299   -0.92    0.356  
subsectorLeisure          0.3962     0.8266    0.48    0.632  
subsectorHotelsRestBar   -0.4147     0.5337   -0.78    0.437  
subsectorOther            0.6599     1.4051    0.47    0.639  
sizeMedium               -0.2345     0.2586   -0.91    0.364  
sizeSmall                -0.4831     0.2617   -1.85    0.065 .
sizeMicro                -0.6153     0.2597   -2.37    0.018 *
log_pop                   0.1670     0.1403    1.19    0.234  
log_bribe_vic            -0.1413     0.2499   -0.57    0.572  
log_nbus                  0.2364     0.4201    0.56    0.574  
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
(Intercept) 8.02e-09 8.956e-05

Negative binomial dispersion parameter: 0.061079 (std. err.: 0.0045581)

Log-likelihood: -1597.06 
```

```r
get_glmmadmb(mnb25)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus

$logLik
[1] -1597.06

$df
[1] 29

$AIC
[1] 3252.12

$BIC
[1] 3419.978

$alpha
[1] 16.37224

$var_j
[1] 6.432682e-17

$ICC
[1] 3.929018e-18
```

```r
vif(mnb25)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.162297  1        1.078099
yearsquant    1.571073  4        1.058095
subsector     2.139148 15        1.025671
size          1.368188  3        1.053637
log_pop       1.114197  1        1.055555
log_bribe_vic 1.096901  1        1.047330
log_nbus      1.135798  1        1.065738
```

```r
mnb26 <- update(mnb20, . ~ . -  log_pop)

summary(mnb26)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim, data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3254.5 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.10353    0.53745   -0.19    0.847  
bribes                  -0.00941    0.09747   -0.10    0.923  
yearsquant(8,16]         0.15302    0.29444    0.52    0.603  
yearsquant(16,25]       -0.07646    0.28099   -0.27    0.786  
yearsquant(25,34]        0.43571    0.28517    1.53    0.127  
yearsquant(34,43]        0.17370    0.29679    0.59    0.558  
subsectorMining         -1.47316    0.97517   -1.51    0.131  
subsectorConstruction   -0.00824    0.53450   -0.02    0.988  
subsectorManufacturing  -0.26292    0.51758   -0.51    0.611  
subsectorWholesale      -0.54774    0.64523   -0.85    0.396  
subsectorTransport      -0.57605    0.60532   -0.95    0.341  
subsectorMedia          -0.31712    0.80503   -0.39    0.694  
subsectorFinance         0.05316    0.86852    0.06    0.951  
subsectorReal estate    -1.27831    0.87192   -1.47    0.143  
subsectorProf. services  1.09166    0.83309    1.31    0.190  
subsectorMaintenance    -0.58762    0.58092   -1.01    0.312  
subsectorEducation       0.16011    0.85595    0.19    0.852  
subsectorHealth         -0.49972    0.53118   -0.94    0.347  
subsectorLeisure         0.36163    0.83087    0.44    0.663  
subsectorHotelsRestBar  -0.42849    0.53442   -0.80    0.423  
subsectorOther           0.62607    1.40440    0.45    0.656  
sizeMedium              -0.25133    0.25832   -0.97    0.331  
sizeSmall               -0.48489    0.26206   -1.85    0.064 .
sizeMicro               -0.60300    0.26007   -2.32    0.020 *
log_wpn_crim             0.10967    0.13513    0.81    0.417  
log_bribe_vic           -0.06750    0.23856   -0.28    0.777  
log_nbus                 0.20289    0.43976    0.46    0.645  
log_drug_crim           -0.01088    0.12502   -0.09    0.931  
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
(Intercept) 3.845e-08 0.0001961

Negative binomial dispersion parameter: 0.061034 (std. err.: 0.0045552)

Log-likelihood: -1597.27 
```

```r
get_glmmadmb(mnb26)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim

$logLik
[1] -1597.27

$df
[1] 30

$AIC
[1] 3254.54

$BIC
[1] 3428.186

$alpha
[1] 16.38431

$var_j
[1] 1.478479e-15

$ICC
[1] 9.023751e-17
```

```r
lrtest(mnb25, mnb20)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_bribe_vic + log_nbus
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
  #Df  LogLik Df Chisq Pr(>Chisq)
1  29 -1597.1                    
2  31 -1597.0  2  0.08     0.9608
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

=========================================================================================================================
                         no fed             - drug + wpn       + drug - wpn       + drug + wpn - pop   + drug + wpn + pop
-------------------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.11 (0.53)       -0.11 (0.53)       -0.10 (0.54)         -0.11 (0.53)   
bribes                      -0.02 (0.10)       -0.01 (0.10)       -0.02 (0.10)       -0.01 (0.10)         -0.01 (0.10)   
yearsquant(8,16]             0.14 (0.29)        0.14 (0.29)        0.14 (0.29)        0.15 (0.29)          0.14 (0.29)   
yearsquant(16,25]           -0.10 (0.28)       -0.10 (0.28)       -0.10 (0.28)       -0.08 (0.28)         -0.10 (0.28)   
yearsquant(25,34]            0.43 (0.28)        0.43 (0.28)        0.43 (0.28)        0.44 (0.29)          0.43 (0.29)   
yearsquant(34,43]            0.19 (0.29)        0.18 (0.30)        0.18 (0.30)        0.17 (0.30)          0.18 (0.30)   
subsectorMining             -1.46 (0.97)       -1.46 (0.97)       -1.46 (0.97)       -1.47 (0.98)         -1.46 (0.97)   
subsectorConstruction        0.00 (0.53)        0.00 (0.53)        0.00 (0.53)       -0.01 (0.53)          0.00 (0.53)   
subsectorManufacturing      -0.26 (0.52)       -0.25 (0.52)       -0.25 (0.52)       -0.26 (0.52)         -0.25 (0.52)   
subsectorWholesale          -0.54 (0.64)       -0.53 (0.64)       -0.54 (0.64)       -0.55 (0.65)         -0.53 (0.64)   
subsectorTransport          -0.55 (0.61)       -0.55 (0.61)       -0.55 (0.61)       -0.58 (0.61)         -0.55 (0.61)   
subsectorMedia              -0.30 (0.80)       -0.29 (0.81)       -0.30 (0.80)       -0.32 (0.81)         -0.29 (0.81)   
subsectorFinance             0.04 (0.86)        0.04 (0.86)        0.03 (0.86)        0.05 (0.87)          0.05 (0.87)   
subsectorReal estate        -1.25 (0.87)       -1.24 (0.87)       -1.25 (0.87)       -1.28 (0.87)         -1.25 (0.87)   
subsectorProf. services      1.11 (0.83)        1.12 (0.83)        1.11 (0.83)        1.09 (0.83)          1.12 (0.83)   
subsectorMaintenance        -0.59 (0.58)       -0.57 (0.58)       -0.58 (0.58)       -0.59 (0.58)         -0.58 (0.58)   
subsectorEducation           0.21 (0.86)        0.22 (0.86)        0.21 (0.86)        0.16 (0.86)          0.22 (0.86)   
subsectorHealth             -0.49 (0.53)       -0.49 (0.53)       -0.49 (0.53)       -0.50 (0.53)         -0.49 (0.53)   
subsectorLeisure             0.40 (0.83)        0.40 (0.83)        0.39 (0.83)        0.36 (0.83)          0.41 (0.83)   
subsectorHotelsRestBar      -0.41 (0.53)       -0.42 (0.53)       -0.42 (0.53)       -0.43 (0.53)         -0.41 (0.53)   
subsectorOther               0.66 (1.41)        0.66 (1.41)        0.66 (1.41)        0.63 (1.40)          0.66 (1.41)   
sizeMedium                  -0.23 (0.26)       -0.24 (0.26)       -0.23 (0.26)       -0.25 (0.26)         -0.24 (0.26)   
sizeSmall                   -0.48 (0.26)       -0.48 (0.26)       -0.48 (0.26)       -0.48 (0.26)         -0.48 (0.26)   
sizeMicro                   -0.62 (0.26) *     -0.61 (0.26) *     -0.61 (0.26) *     -0.60 (0.26) *       -0.61 (0.26) * 
log_pop                      0.17 (0.14)        0.13 (0.19)        0.16 (0.16)                             0.13 (0.19)   
log_bribe_vic               -0.14 (0.25)       -0.13 (0.25)       -0.14 (0.25)       -0.07 (0.24)         -0.13 (0.25)   
log_nbus                     0.24 (0.42)        0.25 (0.42)        0.24 (0.44)        0.20 (0.44)          0.24 (0.44)   
log_wpn_crim                                    0.03 (0.14)                           0.11 (0.14)          0.04 (0.17)   
log_drug_crim                                                      0.01 (0.11)       -0.01 (0.13)         -0.01 (0.13)   
-------------------------------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00                 0.00          
Dispersion: parameter        0.06               0.06               0.06               0.06                 0.06          
Dispersion: SD               0.00               0.00               0.00               0.00                 0.00          
AIC                       3252.12            3254.06            3254.12            3254.54              3256.04          
BIC                       3419.98            3427.71            3427.77            3428.19              3435.47          
Log Likelihood           -1597.06           -1597.03           -1597.06           -1597.27             -1597.02          
Num. obs.                 2412               2412               2412               2412                 2412             
Num. groups: state          32                 32                 32                 32                   32             
=========================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```


Controls: Competitive index

- Include both, drop 1, test
- both + homicides, test



```r
## controls: competitive index

mnb27 <- update(mnb20, . ~ . + comp_index)

summary(mnb27)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3258 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11019    0.53468   -0.21    0.837  
bribes                  -0.01469    0.09732   -0.15    0.880  
yearsquant(8,16]         0.14031    0.29517    0.48    0.635  
yearsquant(16,25]       -0.10037    0.28288   -0.35    0.723  
yearsquant(25,34]        0.42385    0.28624    1.48    0.139  
yearsquant(34,43]        0.17475    0.29817    0.59    0.558  
subsectorMining         -1.46479    0.97452   -1.50    0.133  
subsectorConstruction    0.00456    0.53430    0.01    0.993  
subsectorManufacturing  -0.24945    0.51730   -0.48    0.630  
subsectorWholesale      -0.53866    0.64516   -0.83    0.404  
subsectorTransport      -0.55005    0.60617   -0.91    0.364  
subsectorMedia          -0.28814    0.80521   -0.36    0.720  
subsectorFinance         0.05492    0.86689    0.06    0.949  
subsectorReal estate    -1.23758    0.87405   -1.42    0.157  
subsectorProf. services  1.12459    0.83358    1.35    0.177  
subsectorMaintenance    -0.56208    0.58390   -0.96    0.336  
subsectorEducation       0.22993    0.86324    0.27    0.790  
subsectorHealth         -0.48699    0.53064   -0.92    0.359  
subsectorLeisure         0.40675    0.83503    0.49    0.626  
subsectorHotelsRestBar  -0.41694    0.53440   -0.78    0.435  
subsectorOther           0.64875    1.40710    0.46    0.645  
sizeMedium              -0.23746    0.25908   -0.92    0.359  
sizeSmall               -0.48186    0.26218   -1.84    0.066 .
sizeMicro               -0.61187    0.26040   -2.35    0.019 *
log_pop                  0.13535    0.19268    0.70    0.482  
log_wpn_crim             0.04296    0.16668    0.26    0.797  
log_bribe_vic           -0.11775    0.26414   -0.45    0.656  
log_nbus                 0.24728    0.44220    0.56    0.576  
log_drug_crim           -0.01260    0.12576   -0.10    0.920  
comp_index               0.00253    0.01260    0.20    0.841  
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
(Intercept) 3.169e-08 0.000178

Negative binomial dispersion parameter: 0.0611 (std. err.: 0.0045603)

Log-likelihood: -1597 
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
[1] -1597

$df
[1] 32

$AIC
[1] 3258

$BIC
[1] 3443.223

$alpha
[1] 16.36661

$var_j
[1] 1.004193e-15

$ICC
[1] 6.135618e-17
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
  #Df LogLik Df Chisq Pr(>Chisq)
1  31  -1597                    
2  32  -1597  1  0.04     0.8415
```

```r
vif(mnb27)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.208169  1        1.099167
yearsquant    1.680587  4        1.067045
subsector     2.663611 15        1.033195
size          1.400300  3        1.057719
log_pop       1.137765  1        1.066661
log_wpn_crim  1.130538  1        1.063267
log_bribe_vic 1.087968  1        1.043057
log_nbus      1.154266  1        1.074368
log_drug_crim 1.164628  1        1.079179
comp_index    1.133882  1        1.064839
```

```r
vif(mnb27)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.459671   1        1.208169
yearsquant    2.824374  16        1.138585
subsector     7.094823 225        1.067492
size          1.960841   9        1.118769
log_pop       1.294509   1        1.137765
log_wpn_crim  1.278116   1        1.130538
log_bribe_vic 1.183674   1        1.087968
log_nbus      1.332330   1        1.154266
log_drug_crim 1.356358   1        1.164628
comp_index    1.285689   1        1.133882
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

mnb28 <- update(mnb27, . ~ . + law_index)

summary(mnb28)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3259.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12540    0.53633   -0.23    0.815  
bribes                  -0.01917    0.09757   -0.20    0.844  
yearsquant(8,16]         0.14876    0.29612    0.50    0.615  
yearsquant(16,25]       -0.08699    0.28498   -0.31    0.760  
yearsquant(25,34]        0.41857    0.28644    1.46    0.144  
yearsquant(34,43]        0.17589    0.29844    0.59    0.556  
subsectorMining         -1.45152    0.97526   -1.49    0.137  
subsectorConstruction    0.03164    0.53825    0.06    0.953  
subsectorManufacturing  -0.22572    0.52020   -0.43    0.664  
subsectorWholesale      -0.52600    0.64538   -0.82    0.415  
subsectorTransport      -0.52603    0.60946   -0.86    0.388  
subsectorMedia          -0.28255    0.80534   -0.35    0.726  
subsectorFinance         0.09109    0.87221    0.10    0.917  
subsectorReal estate    -1.19610    0.87932   -1.36    0.174  
subsectorProf. services  1.14363    0.83473    1.37    0.171  
subsectorMaintenance    -0.57262    0.58375   -0.98    0.327  
subsectorEducation       0.29655    0.87863    0.34    0.736  
subsectorHealth         -0.48439    0.53048   -0.91    0.361  
subsectorLeisure         0.39324    0.83643    0.47    0.638  
subsectorHotelsRestBar  -0.40358    0.53557   -0.75    0.451  
subsectorOther           0.65432    1.40760    0.46    0.642  
sizeMedium              -0.23687    0.25934   -0.91    0.361  
sizeSmall               -0.47995    0.26220   -1.83    0.067 .
sizeMicro               -0.62528    0.26240   -2.38    0.017 *
log_pop                  0.15008    0.19633    0.76    0.445  
log_wpn_crim            -0.00677    0.20603   -0.03    0.974  
log_bribe_vic           -0.18605    0.31241   -0.60    0.551  
log_nbus                 0.34860    0.50804    0.69    0.493  
log_drug_crim            0.01343    0.14108    0.10    0.924  
comp_index               0.00262    0.01263    0.21    0.835  
law_index               -0.00446    0.01093   -0.41    0.683  
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
(Intercept) 4.784e-09 6.917e-05

Negative binomial dispersion parameter: 0.061114 (std. err.: 0.004561)

Log-likelihood: -1596.92 
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
[1] -1596.92

$df
[1] 33

$AIC
[1] 3259.84

$BIC
[1] 3450.851

$alpha
[1] 16.36286

$var_j
[1] 2.288953e-17

$ICC
[1] 1.398871e-18
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
1  32 -1597.0                    
2  33 -1596.9  1  0.16     0.6892
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
1  31 -1597.0                    
2  33 -1596.9  2   0.2     0.9048
```

```r
vif(mnb28)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.231000  1        1.109504
yearsquant    1.736916  4        1.071451
subsector     3.030298 15        1.037647
size          1.427052  3        1.061060
log_pop       1.143256  1        1.069231
log_wpn_crim  1.138033  1        1.066786
log_bribe_vic 1.103142  1        1.050306
log_nbus      1.152708  1        1.073642
log_drug_crim 1.172239  1        1.082700
comp_index    1.147999  1        1.071447
law_index     1.291190  1        1.136305
```

```r
vif(mnb28)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.515360   1        1.231000
yearsquant    3.016876  16        1.148007
subsector     9.182706 225        1.076711
size          2.036478   9        1.125849
log_pop       1.307033   1        1.143256
log_wpn_crim  1.295119   1        1.138033
log_bribe_vic 1.216922   1        1.103142
log_nbus      1.328736   1        1.152708
log_drug_crim 1.374143   1        1.172239
comp_index    1.317901   1        1.147999
law_index     1.667171   1        1.291190
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

mnb29 <- update(mnb28, . ~ . - comp_index)

summary(mnb29)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3257.9 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12691    0.53644   -0.24    0.813  
bribes                  -0.01825    0.09740   -0.19    0.851  
yearsquant(8,16]         0.15239    0.29582    0.52    0.606  
yearsquant(16,25]       -0.08352    0.28455   -0.29    0.769  
yearsquant(25,34]        0.42385    0.28522    1.49    0.137  
yearsquant(34,43]        0.18139    0.29742    0.61    0.542  
subsectorMining         -1.44793    0.97490   -1.49    0.137  
subsectorConstruction    0.02845    0.53801    0.05    0.958  
subsectorManufacturing  -0.23149    0.51945   -0.45    0.656  
subsectorWholesale      -0.51726    0.64437   -0.80    0.422  
subsectorTransport      -0.52503    0.60942   -0.86    0.389  
subsectorMedia          -0.28684    0.80519   -0.36    0.722  
subsectorFinance         0.08557    0.87152    0.10    0.922  
subsectorReal estate    -1.20441    0.87860   -1.37    0.170  
subsectorProf. services  1.13512    0.83367    1.36    0.173  
subsectorMaintenance    -0.58621    0.57989   -1.01    0.312  
subsectorEducation       0.28383    0.87609    0.32    0.746  
subsectorHealth         -0.48492    0.53046   -0.91    0.361  
subsectorLeisure         0.39987    0.83564    0.48    0.632  
subsectorHotelsRestBar  -0.40032    0.53548   -0.75    0.455  
subsectorOther           0.66943    1.40560    0.48    0.634  
sizeMedium              -0.23808    0.25919   -0.92    0.358  
sizeSmall               -0.47923    0.26229   -1.83    0.068 .
sizeMicro               -0.62709    0.26219   -2.39    0.017 *
log_pop                  0.14907    0.19569    0.76    0.446  
log_wpn_crim            -0.00685    0.20565   -0.03    0.973  
log_bribe_vic           -0.20078    0.30375   -0.66    0.509  
log_nbus                 0.33639    0.50495    0.67    0.505  
log_drug_crim            0.01491    0.14103    0.11    0.916  
law_index               -0.00442    0.01092   -0.40    0.686  
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

Negative binomial dispersion parameter: 0.061109 (std. err.: 0.0045606)

Log-likelihood: -1596.94 
```

```r
get_glmmadmb(mnb29)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    law_index

$logLik
[1] -1596.94

$df
[1] 32

$AIC
[1] 3257.88

$BIC
[1] 3443.103

$alpha
[1] 16.3642

$var_j
[1] 4.248958e-18

$ICC
[1] 2.596496e-19
```

```r
lrtest(mnb20, mnb29)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim
Model 2: extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    law_index
  #Df  LogLik Df Chisq Pr(>Chisq)
1  31 -1597.0                    
2  32 -1596.9  1  0.16     0.6892
```

```r
vif(mnb29)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.224873  1        1.106740
yearsquant    1.726898  4        1.070677
subsector     2.778237 15        1.034647
size          1.418103  3        1.059948
log_pop       1.133944  1        1.064868
log_wpn_crim  1.135378  1        1.065541
log_bribe_vic 1.098916  1        1.048292
log_nbus      1.142107  1        1.068694
log_drug_crim 1.150676  1        1.072696
law_index     1.277274  1        1.130165
```

```r
vif(mnb29)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.500315   1        1.224873
yearsquant    2.982175  16        1.146348
subsector     7.718600 225        1.070495
size          2.011017   9        1.123490
log_pop       1.285828   1        1.133944
log_wpn_crim  1.289083   1        1.135378
log_bribe_vic 1.207617   1        1.098916
log_nbus      1.304408   1        1.142107
log_drug_crim 1.324056   1        1.150676
law_index     1.631429   1        1.277274
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

mnb30 <- update(mnb28, . ~ . + log_deaths)

summary(mnb30)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index + log_deaths, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3260.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.05715    0.53833   -0.11    0.915  
bribes                  -0.00847    0.09770   -0.09    0.931  
yearsquant(8,16]         0.13604    0.29581    0.46    0.646  
yearsquant(16,25]       -0.09482    0.28433   -0.33    0.739  
yearsquant(25,34]        0.35701    0.29107    1.23    0.220  
yearsquant(34,43]        0.17089    0.29876    0.57    0.567  
subsectorMining         -1.33898    0.97707   -1.37    0.171  
subsectorConstruction    0.03666    0.53773    0.07    0.946  
subsectorManufacturing  -0.26814    0.52116   -0.51    0.607  
subsectorWholesale      -0.47877    0.64901   -0.74    0.461  
subsectorTransport      -0.60425    0.61172   -0.99    0.323  
subsectorMedia          -0.27518    0.80396   -0.34    0.732  
subsectorFinance         0.08535    0.87303    0.10    0.922  
subsectorReal estate    -1.05793    0.88509   -1.20    0.232  
subsectorProf. services  1.09331    0.83337    1.31    0.190  
subsectorMaintenance    -0.62109    0.58388   -1.06    0.287  
subsectorEducation       0.22564    0.87876    0.26    0.797  
subsectorHealth         -0.47115    0.53057   -0.89    0.375  
subsectorLeisure         0.45203    0.83608    0.54    0.589  
subsectorHotelsRestBar  -0.42170    0.53546   -0.79    0.431  
subsectorOther           0.60460    1.40480    0.43    0.667  
sizeMedium              -0.25985    0.25941   -1.00    0.316  
sizeSmall               -0.48418    0.26193   -1.85    0.065 .
sizeMicro               -0.64493    0.26192   -2.46    0.014 *
log_pop                  0.05847    0.21019    0.28    0.781  
log_wpn_crim            -0.21672    0.26428   -0.82    0.412  
log_bribe_vic           -0.16226    0.30892   -0.53    0.599  
log_nbus                 0.34695    0.51101    0.68    0.497  
log_drug_crim            0.01575    0.13905    0.11    0.910  
comp_index               0.00717    0.01308    0.55    0.584  
law_index               -0.00108    0.01114   -0.10    0.923  
log_deaths               0.26810    0.21486    1.25    0.212  
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
(Intercept) 2.132e-09 4.617e-05

Negative binomial dispersion parameter: 0.061373 (std. err.: 0.0045858)

Log-likelihood: -1596.15 
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
[1] -1596.15

$df
[1] 34

$AIC
[1] 3260.3

$BIC
[1] 3457.099

$alpha
[1] 16.29381

$var_j
[1] 4.544571e-18

$ICC
[1] 2.78914e-19
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
1  33 -1596.9                    
2  34 -1596.2  1  1.54     0.2146
```

```r
vif(mnb30)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.231634  1        1.109790
yearsquant    1.786366  4        1.075217
subsector     3.437494 15        1.042017
size          1.428236  3        1.061207
log_pop       1.147198  1        1.071073
log_wpn_crim  1.138453  1        1.066983
log_bribe_vic 1.102173  1        1.049844
log_nbus      1.151191  1        1.072936
log_drug_crim 1.173889  1        1.083461
comp_index    1.149899  1        1.072333
law_index     1.292437  1        1.136854
log_deaths    1.167607  1        1.080559
```

```r
vif(mnb30)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.516922   1        1.231634
yearsquant     3.191102  16        1.156093
subsector     11.816363 225        1.085799
size           2.039859   9        1.126160
log_pop        1.316064   1        1.147198
log_wpn_crim   1.296076   1        1.138453
log_bribe_vic  1.214785   1        1.102173
log_nbus       1.325240   1        1.151191
log_drug_crim  1.378015   1        1.173889
comp_index     1.322267   1        1.149899
law_index      1.670392   1        1.292437
log_deaths     1.363306   1        1.167607
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

========================================================================================================================
                         no index           comp               law                comp + law         comp + law + deaths
------------------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.11 (0.53)       -0.11 (0.53)       -0.13 (0.54)       -0.13 (0.54)       -0.06 (0.54)    
bribes                      -0.01 (0.10)       -0.01 (0.10)       -0.02 (0.10)       -0.02 (0.10)       -0.01 (0.10)    
yearsquant(8,16]             0.14 (0.29)        0.14 (0.30)        0.15 (0.30)        0.15 (0.30)        0.14 (0.30)    
yearsquant(16,25]           -0.10 (0.28)       -0.10 (0.28)       -0.08 (0.28)       -0.09 (0.28)       -0.09 (0.28)    
yearsquant(25,34]            0.43 (0.29)        0.42 (0.29)        0.42 (0.29)        0.42 (0.29)        0.36 (0.29)    
yearsquant(34,43]            0.18 (0.30)        0.17 (0.30)        0.18 (0.30)        0.18 (0.30)        0.17 (0.30)    
subsectorMining             -1.46 (0.97)       -1.46 (0.97)       -1.45 (0.97)       -1.45 (0.98)       -1.34 (0.98)    
subsectorConstruction        0.00 (0.53)        0.00 (0.53)        0.03 (0.54)        0.03 (0.54)        0.04 (0.54)    
subsectorManufacturing      -0.25 (0.52)       -0.25 (0.52)       -0.23 (0.52)       -0.23 (0.52)       -0.27 (0.52)    
subsectorWholesale          -0.53 (0.64)       -0.54 (0.65)       -0.52 (0.64)       -0.53 (0.65)       -0.48 (0.65)    
subsectorTransport          -0.55 (0.61)       -0.55 (0.61)       -0.53 (0.61)       -0.53 (0.61)       -0.60 (0.61)    
subsectorMedia              -0.29 (0.81)       -0.29 (0.81)       -0.29 (0.81)       -0.28 (0.81)       -0.28 (0.80)    
subsectorFinance             0.05 (0.87)        0.05 (0.87)        0.09 (0.87)        0.09 (0.87)        0.09 (0.87)    
subsectorReal estate        -1.25 (0.87)       -1.24 (0.87)       -1.20 (0.88)       -1.20 (0.88)       -1.06 (0.89)    
subsectorProf. services      1.12 (0.83)        1.12 (0.83)        1.14 (0.83)        1.14 (0.83)        1.09 (0.83)    
subsectorMaintenance        -0.58 (0.58)       -0.56 (0.58)       -0.59 (0.58)       -0.57 (0.58)       -0.62 (0.58)    
subsectorEducation           0.22 (0.86)        0.23 (0.86)        0.28 (0.88)        0.30 (0.88)        0.23 (0.88)    
subsectorHealth             -0.49 (0.53)       -0.49 (0.53)       -0.48 (0.53)       -0.48 (0.53)       -0.47 (0.53)    
subsectorLeisure             0.41 (0.83)        0.41 (0.84)        0.40 (0.84)        0.39 (0.84)        0.45 (0.84)    
subsectorHotelsRestBar      -0.41 (0.53)       -0.42 (0.53)       -0.40 (0.54)       -0.40 (0.54)       -0.42 (0.54)    
subsectorOther               0.66 (1.41)        0.65 (1.41)        0.67 (1.41)        0.65 (1.41)        0.60 (1.40)    
sizeMedium                  -0.24 (0.26)       -0.24 (0.26)       -0.24 (0.26)       -0.24 (0.26)       -0.26 (0.26)    
sizeSmall                   -0.48 (0.26)       -0.48 (0.26)       -0.48 (0.26)       -0.48 (0.26)       -0.48 (0.26)    
sizeMicro                   -0.61 (0.26) *     -0.61 (0.26) *     -0.63 (0.26) *     -0.63 (0.26) *     -0.64 (0.26) *  
log_pop                      0.13 (0.19)        0.14 (0.19)        0.15 (0.20)        0.15 (0.20)        0.06 (0.21)    
log_wpn_crim                 0.04 (0.17)        0.04 (0.17)       -0.01 (0.21)       -0.01 (0.21)       -0.22 (0.26)    
log_bribe_vic               -0.13 (0.25)       -0.12 (0.26)       -0.20 (0.30)       -0.19 (0.31)       -0.16 (0.31)    
log_nbus                     0.24 (0.44)        0.25 (0.44)        0.34 (0.50)        0.35 (0.51)        0.35 (0.51)    
log_drug_crim               -0.01 (0.13)       -0.01 (0.13)        0.01 (0.14)        0.01 (0.14)        0.02 (0.14)    
comp_index                                      0.00 (0.01)                           0.00 (0.01)        0.01 (0.01)    
law_index                                                         -0.00 (0.01)       -0.00 (0.01)       -0.00 (0.01)    
log_deaths                                                                                               0.27 (0.21)    
------------------------------------------------------------------------------------------------------------------------
Variance: state              0.00               0.00               0.00               0.00               0.00           
Dispersion: parameter        0.06               0.06               0.06               0.06               0.06           
Dispersion: SD               0.00               0.00               0.00               0.00               0.00           
AIC                       3256.04            3258.00            3257.88            3259.84            3260.30           
BIC                       3435.47            3443.22            3443.10            3450.85            3457.10           
Log Likelihood           -1597.02           -1597.00           -1596.94           -1596.92           -1596.15           
Num. obs.                 2412               2412               2412               2412               2412              
Num. groups: state          32                 32                 32                 32                 32              
========================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
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

summary(mnb_final)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3259.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12540    0.53633   -0.23    0.815  
bribes                  -0.01917    0.09757   -0.20    0.844  
yearsquant(8,16]         0.14876    0.29612    0.50    0.615  
yearsquant(16,25]       -0.08699    0.28498   -0.31    0.760  
yearsquant(25,34]        0.41857    0.28644    1.46    0.144  
yearsquant(34,43]        0.17589    0.29844    0.59    0.556  
subsectorMining         -1.45152    0.97526   -1.49    0.137  
subsectorConstruction    0.03164    0.53825    0.06    0.953  
subsectorManufacturing  -0.22572    0.52020   -0.43    0.664  
subsectorWholesale      -0.52600    0.64538   -0.82    0.415  
subsectorTransport      -0.52603    0.60946   -0.86    0.388  
subsectorMedia          -0.28255    0.80534   -0.35    0.726  
subsectorFinance         0.09109    0.87221    0.10    0.917  
subsectorReal estate    -1.19610    0.87932   -1.36    0.174  
subsectorProf. services  1.14363    0.83473    1.37    0.171  
subsectorMaintenance    -0.57262    0.58375   -0.98    0.327  
subsectorEducation       0.29655    0.87863    0.34    0.736  
subsectorHealth         -0.48439    0.53048   -0.91    0.361  
subsectorLeisure         0.39324    0.83643    0.47    0.638  
subsectorHotelsRestBar  -0.40358    0.53557   -0.75    0.451  
subsectorOther           0.65432    1.40760    0.46    0.642  
sizeMedium              -0.23687    0.25934   -0.91    0.361  
sizeSmall               -0.47995    0.26220   -1.83    0.067 .
sizeMicro               -0.62528    0.26240   -2.38    0.017 *
log_pop                  0.15008    0.19633    0.76    0.445  
log_wpn_crim            -0.00677    0.20603   -0.03    0.974  
log_bribe_vic           -0.18605    0.31241   -0.60    0.551  
log_nbus                 0.34860    0.50804    0.69    0.493  
log_drug_crim            0.01343    0.14108    0.10    0.924  
comp_index               0.00262    0.01263    0.21    0.835  
law_index               -0.00446    0.01093   -0.41    0.683  
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
(Intercept) 4.784e-09 6.917e-05

Negative binomial dispersion parameter: 0.061114 (std. err.: 0.004561)

Log-likelihood: -1596.92 
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
[1] -1596.92

$df
[1] 33

$AIC
[1] 3259.84

$BIC
[1] 3450.851

$alpha
[1] 16.36286

$var_j
[1] 2.288953e-17

$ICC
[1] 1.398871e-18
```

```r
vif(mnb_final)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.231000  1        1.109504
yearsquant    1.736916  4        1.071451
subsector     3.030298 15        1.037647
size          1.427052  3        1.061060
log_pop       1.143256  1        1.069231
log_wpn_crim  1.138033  1        1.066786
log_bribe_vic 1.103142  1        1.050306
log_nbus      1.152708  1        1.073642
log_drug_crim 1.172239  1        1.082700
comp_index    1.147999  1        1.071447
law_index     1.291190  1        1.136305
```

```r
vif(mnb_final)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.515360   1        1.231000
yearsquant    3.016876  16        1.148007
subsector     9.182706 225        1.076711
size          2.036478   9        1.125849
log_pop       1.307033   1        1.143256
log_wpn_crim  1.295119   1        1.138033
log_bribe_vic 1.216922   1        1.103142
log_nbus      1.328736   1        1.152708
log_drug_crim 1.374143   1        1.172239
comp_index    1.317901   1        1.147999
law_index     1.667171   1        1.291190
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
1  26 -1597.8                    
2  33 -1596.9  7  1.76     0.9719
```

```r
# Test vs multilevel null

mnb_null <- update(mnb_final, . ~ 1 + (1 | state))

summary(mnb_null)
```

```

Call:
glmmadmb(formula = extortions ~ (1 | state), data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3226.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
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
(Intercept) 2.69e-07 0.0005187

Negative binomial dispersion parameter: 0.057237 (std. err.: 0.0042146)

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(mnb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1610.33

$df
[1] 3

$AIC
[1] 3226.66

$BIC
[1] 3244.025

$alpha
[1] 17.47122

$var_j
[1] 7.238252e-14

$ICC
[1] 4.142958e-15
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
1   3 -1610.3                    
2  33 -1596.9 30 26.82     0.6327
```

```r
# test vs sigle-level full and null

nb_final <- update(mnb_final, . ~ . - (1 | state))

summary(nb_final)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3257.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12540    0.53633   -0.23    0.815  
bribes                  -0.01916    0.09757   -0.20    0.844  
yearsquant(8,16]         0.14875    0.29612    0.50    0.615  
yearsquant(16,25]       -0.08702    0.28498   -0.31    0.760  
yearsquant(25,34]        0.41855    0.28644    1.46    0.144  
yearsquant(34,43]        0.17589    0.29844    0.59    0.556  
subsectorMining         -1.45146    0.97527   -1.49    0.137  
subsectorConstruction    0.03166    0.53825    0.06    0.953  
subsectorManufacturing  -0.22574    0.52020   -0.43    0.664  
subsectorWholesale      -0.52600    0.64538   -0.82    0.415  
subsectorTransport      -0.52600    0.60946   -0.86    0.388  
subsectorMedia          -0.28248    0.80535   -0.35    0.726  
subsectorFinance         0.09110    0.87221    0.10    0.917  
subsectorReal estate    -1.19609    0.87932   -1.36    0.174  
subsectorProf. services  1.14362    0.83472    1.37    0.171  
subsectorMaintenance    -0.57261    0.58375   -0.98    0.327  
subsectorEducation       0.29661    0.87863    0.34    0.736  
subsectorHealth         -0.48439    0.53048   -0.91    0.361  
subsectorLeisure         0.39330    0.83644    0.47    0.638  
subsectorHotelsRestBar  -0.40358    0.53557   -0.75    0.451  
subsectorOther           0.65438    1.40760    0.46    0.642  
sizeMedium              -0.23684    0.25934   -0.91    0.361  
sizeSmall               -0.47993    0.26220   -1.83    0.067 .
sizeMicro               -0.62530    0.26240   -2.38    0.017 *
log_pop                  0.15010    0.19633    0.76    0.445  
log_wpn_crim            -0.00677    0.20603   -0.03    0.974  
log_bribe_vic           -0.18604    0.31241   -0.60    0.552  
log_nbus                 0.34861    0.50805    0.69    0.493  
log_drug_crim            0.01342    0.14108    0.10    0.924  
comp_index               0.00262    0.01263    0.21    0.835  
law_index               -0.00446    0.01093   -0.41    0.683  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.06103 (std. err.: 0.0045536)

Log-likelihood: -1596.92 
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
[1] -1596.92

$df
[1] 32

$AIC
[1] 3257.84

$BIC
[1] 3443.063

$alpha
[1] 16.38538
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
1  32 -1596.9                    
2  33 -1596.9  1     0          1
```

```r
nb_null <- update(nb_final, . ~ 1)

summary(nb_null)
```

```

Call:
glmmadmb(formula = extortions ~ 1, data = enve_model, family = "nbinom", 
    zeroInflation = FALSE)

AIC: 3224.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.057159 (std. err.: 0.0042079)

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(nb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1610.33

$df
[1] 2

$AIC
[1] 3224.66

$BIC
[1] 3236.236

$alpha
[1] 17.49506
```

```r
lrtest(nb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   2 -1610.3                    
2   3 -1610.3  1     0          1
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
1   2 -1610.3                    
2  33 -1596.9 31 26.82     0.6812
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
1   2 -1610.3                    
2  32 -1596.9 30 26.82     0.6327
```

```r
# Only state vars

mnb_only_state <- update(mnb_null, . ~ . + log_pop + log_wpn_crim + log_drug_crim +
                             log_bribe_vic + log_nbus + comp_index + law_index)

summary(mnb_only_state)
```

```

Call:
glmmadmb(formula = extortions ~ (1 | state) + log_pop + log_wpn_crim + 
    log_drug_crim + log_bribe_vic + log_nbus + comp_index + law_index, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3238.9 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.53520    0.11138   -4.81  1.5e-06 ***
log_pop        0.05452    0.19363    0.28     0.78    
log_wpn_crim   0.03145    0.20431    0.15     0.88    
log_drug_crim  0.03644    0.13234    0.28     0.78    
log_bribe_vic -0.21118    0.30950   -0.68     0.50    
log_nbus       0.40979    0.46996    0.87     0.38    
comp_index     0.00435    0.01249    0.35     0.73    
law_index     -0.00139    0.00991   -0.14     0.89    
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
(Intercept) 4.601e-09 6.783e-05

Negative binomial dispersion parameter: 0.057487 (std. err.: 0.0042366)

Log-likelihood: -1609.43 
```

```r
get_glmmadmb(mnb_only_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -1609.43

$df
[1] 10

$AIC
[1] 3238.86

$BIC
[1] 3296.742

$alpha
[1] 17.39524

$var_j
[1] 2.117012e-17

$ICC
[1] 1.217007e-18
```

```r
nb_only_state <- update(mnb_only_state, . ~ . - (1 | state))

summary(nb_only_state)
```

```

Call:
glmmadmb(formula = extortions ~ log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = FALSE)

AIC: 3236.9 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.53520    0.11138   -4.81  1.5e-06 ***
log_pop        0.05453    0.19363    0.28     0.78    
log_wpn_crim   0.03144    0.20431    0.15     0.88    
log_drug_crim  0.03644    0.13234    0.28     0.78    
log_bribe_vic -0.21114    0.30950   -0.68     0.50    
log_nbus       0.40977    0.46995    0.87     0.38    
comp_index     0.00435    0.01249    0.35     0.73    
law_index     -0.00139    0.00991   -0.14     0.89    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.057408 (std. err.: 0.0042298)

Log-likelihood: -1609.43 
```

```r
get_glmmadmb(nb_only_state)
```

```
$model
extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index

$logLik
[1] -1609.43

$df
[1] 9

$AIC
[1] 3236.86

$BIC
[1] 3288.954

$alpha
[1] 17.41918
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
1   9 -1609.4                    
2  10 -1609.4  1     0          1
```

```r
vif(nb_only_state)
```

```
      log_pop  log_wpn_crim log_drug_crim log_bribe_vic      log_nbus 
     1.027587      1.040173      1.019299      1.025737      1.039642 
   comp_index     law_index 
     1.040072      1.025460 
```

```r
lm_only_state <- update(lm1, formula(nb_only_state))

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

mnb_noc <- update(mnb_final, . ~ . - log_pop - log_nbus - comp_index -
                      law_index)

summary(mnb_noc)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_wpn_crim + log_bribe_vic + log_drug_crim, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3252.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12730    0.53474   -0.24    0.812  
bribes                  -0.00758    0.09768   -0.08    0.938  
yearsquant(8,16]         0.15420    0.29437    0.52    0.600  
yearsquant(16,25]       -0.06835    0.28055   -0.24    0.808  
yearsquant(25,34]        0.42527    0.28434    1.50    0.135  
yearsquant(34,43]        0.17573    0.29690    0.59    0.554  
subsectorMining         -1.43637    0.97188   -1.48    0.139  
subsectorConstruction    0.01428    0.53258    0.03    0.979  
subsectorManufacturing  -0.24643    0.51617   -0.48    0.633  
subsectorWholesale      -0.55047    0.64474   -0.85    0.393  
subsectorTransport      -0.59143    0.60472   -0.98    0.328  
subsectorMedia          -0.30831    0.80491   -0.38    0.702  
subsectorFinance         0.04025    0.86750    0.05    0.963  
subsectorReal estate    -1.26857    0.87225   -1.45    0.146  
subsectorProf. services  1.13681    0.82781    1.37    0.170  
subsectorMaintenance    -0.56978    0.58023   -0.98    0.326  
subsectorEducation       0.13266    0.85431    0.16    0.877  
subsectorHealth         -0.50553    0.53107   -0.95    0.341  
subsectorLeisure         0.36791    0.83106    0.44    0.658  
subsectorHotelsRestBar  -0.39958    0.53115   -0.75    0.452  
subsectorOther           0.62488    1.40470    0.44    0.656  
sizeMedium              -0.25072    0.25844   -0.97    0.332  
sizeSmall               -0.48741    0.26196   -1.86    0.063 .
sizeMicro               -0.60548    0.26004   -2.33    0.020 *
log_wpn_crim             0.10898    0.13551    0.80    0.421  
log_bribe_vic           -0.02338    0.21867   -0.11    0.915  
log_drug_crim           -0.02568    0.12157   -0.21    0.833  
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
(Intercept) 2.064e-09 4.543e-05

Negative binomial dispersion parameter: 0.061004 (std. err.: 0.0045527)

Log-likelihood: -1597.38 
```

```r
get_glmmadmb(mnb_noc)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state) + 
    log_wpn_crim + log_bribe_vic + log_drug_crim

$logLik
[1] -1597.38

$df
[1] 29

$AIC
[1] 3252.76

$BIC
[1] 3420.618

$alpha
[1] 16.39237

$var_j
[1] 4.258445e-18

$ICC
[1] 2.597822e-19
```

```r
vif(mnb_noc)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.205041  1        1.097744
yearsquant    1.619594  4        1.062125
subsector     2.184883 15        1.026394
size          1.381051  3        1.055281
log_wpn_crim  1.155632  1        1.075003
log_bribe_vic 1.096388  1        1.047086
log_drug_crim 1.132151  1        1.064026
```

```r
vif(mnb_noc)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.452124   1        1.205041
yearsquant    2.623085  16        1.128110
subsector     4.773714 225        1.053485
size          1.907301   9        1.113619
log_wpn_crim  1.335486   1        1.155632
log_bribe_vic 1.202067   1        1.096388
log_drug_crim 1.281767   1        1.132151
```

```r
lm_noc <- update(lm1, update(formula(mnb_noc), . ~ . - (1 | state)))

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
vif(lm_noc)^2
```

```
                  GVIF  Df GVIF^(1/(2*Df))
bribes        1.019607   1        1.009756
yearsquant    1.081277  16        1.009816
subsector     1.139815 225        1.004372
size          1.047736   9        1.007802
log_wpn_crim  4.044778   1        2.011163
log_bribe_vic 1.034314   1        1.017012
log_drug_crim 3.997212   1        1.999303
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

============================================================================================================
                         MNB         No state vars  Only state vars  No controls  Null          NB null     
------------------------------------------------------------------------------------------------------------
(Intercept)                 -0.13       -0.11          -0.54 ***        -0.13        -0.57 ***     -0.57 ***
                            (0.54)      (0.53)         (0.11)           (0.53)       (0.09)        (0.09)   
bribes                      -0.02       -0.01                           -0.01                               
                            (0.10)      (0.10)                          (0.10)                              
yearsquant(8,16]             0.15        0.16                            0.15                               
                            (0.30)      (0.29)                          (0.29)                              
yearsquant(16,25]           -0.09       -0.08                           -0.07                               
                            (0.28)      (0.28)                          (0.28)                              
yearsquant(25,34]            0.42        0.43                            0.43                               
                            (0.29)      (0.28)                          (0.28)                              
yearsquant(34,43]            0.18        0.20                            0.18                               
                            (0.30)      (0.29)                          (0.30)                              
subsectorMining             -1.45       -1.48                           -1.44                               
                            (0.98)      (0.97)                          (0.97)                              
subsectorConstruction        0.03       -0.01                            0.01                               
                            (0.54)      (0.53)                          (0.53)                              
subsectorManufacturing      -0.23       -0.27                           -0.25                               
                            (0.52)      (0.52)                          (0.52)                              
subsectorWholesale          -0.53       -0.63                           -0.55                               
                            (0.65)      (0.64)                          (0.64)                              
subsectorTransport          -0.53       -0.60                           -0.59                               
                            (0.61)      (0.60)                          (0.60)                              
subsectorMedia              -0.28       -0.36                           -0.31                               
                            (0.81)      (0.80)                          (0.80)                              
subsectorFinance             0.09        0.01                            0.04                               
                            (0.87)      (0.86)                          (0.87)                              
subsectorReal estate        -1.20       -1.35                           -1.27                               
                            (0.88)      (0.87)                          (0.87)                              
subsectorProf. services      1.14        1.07                            1.14                               
                            (0.83)      (0.83)                          (0.83)                              
subsectorMaintenance        -0.57       -0.65                           -0.57                               
                            (0.58)      (0.57)                          (0.58)                              
subsectorEducation           0.30        0.03                            0.13                               
                            (0.88)      (0.85)                          (0.85)                              
subsectorHealth             -0.48       -0.53                           -0.51                               
                            (0.53)      (0.53)                          (0.53)                              
subsectorLeisure             0.39        0.25                            0.37                               
                            (0.84)      (0.82)                          (0.83)                              
subsectorHotelsRestBar      -0.40       -0.43                           -0.40                               
                            (0.54)      (0.53)                          (0.53)                              
subsectorOther               0.65        0.57                            0.62                               
                            (1.41)      (1.40)                          (1.40)                              
sizeMedium                  -0.24       -0.25                           -0.25                               
                            (0.26)      (0.26)                          (0.26)                              
sizeSmall                   -0.48       -0.50                           -0.49                               
                            (0.26)      (0.26)                          (0.26)                              
sizeMicro                   -0.63 *     -0.60 *                         -0.61 *                             
                            (0.26)      (0.26)                          (0.26)                              
log_pop                      0.15                       0.05                                                
                            (0.20)                     (0.19)                                               
log_wpn_crim                -0.01                       0.03             0.11                               
                            (0.21)                     (0.20)           (0.14)                              
log_bribe_vic               -0.19                      -0.21            -0.02                               
                            (0.31)                     (0.31)           (0.22)                              
log_nbus                     0.35                       0.41                                                
                            (0.51)                     (0.47)                                               
log_drug_crim                0.01                       0.04            -0.03                               
                            (0.14)                     (0.13)           (0.12)                              
comp_index                   0.00                       0.00                                                
                            (0.01)                     (0.01)                                               
law_index                   -0.00                      -0.00                                                
                            (0.01)                     (0.01)                                               
------------------------------------------------------------------------------------------------------------
Variance: state              0.00        0.00           0.00             0.00         0.00                  
Dispersion: parameter        0.06        0.06           0.06             0.06         0.06          0.06    
Dispersion: SD               0.00        0.00           0.00             0.00         0.00          0.00    
AIC                       3259.84     3247.60        3238.86          3252.76      3226.66       3224.66    
BIC                       3450.85     3398.09        3296.74          3420.62      3244.02       3236.24    
Log Likelihood           -1596.92    -1597.80       -1609.43         -1597.38     -1610.33      -1610.33    
Num. obs.                 2412        2412           2412             2412         2412          2412       
Num. groups: state          32          32             32               32           32                     
Num. groups:                                                                                        1       
============================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Including deaths

mnb_f_deaths <- update(mnb_final, . ~ . + log_deaths)
summary(mnb_f_deaths)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index + log_deaths, 
    data = enve_model, family = "nbinom", zeroInflation = FALSE)

AIC: 3260.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.05715    0.53833   -0.11    0.915  
bribes                  -0.00847    0.09770   -0.09    0.931  
yearsquant(8,16]         0.13604    0.29581    0.46    0.646  
yearsquant(16,25]       -0.09482    0.28433   -0.33    0.739  
yearsquant(25,34]        0.35701    0.29107    1.23    0.220  
yearsquant(34,43]        0.17089    0.29876    0.57    0.567  
subsectorMining         -1.33898    0.97707   -1.37    0.171  
subsectorConstruction    0.03666    0.53773    0.07    0.946  
subsectorManufacturing  -0.26814    0.52116   -0.51    0.607  
subsectorWholesale      -0.47877    0.64901   -0.74    0.461  
subsectorTransport      -0.60425    0.61172   -0.99    0.323  
subsectorMedia          -0.27518    0.80396   -0.34    0.732  
subsectorFinance         0.08535    0.87303    0.10    0.922  
subsectorReal estate    -1.05793    0.88509   -1.20    0.232  
subsectorProf. services  1.09331    0.83337    1.31    0.190  
subsectorMaintenance    -0.62109    0.58388   -1.06    0.287  
subsectorEducation       0.22564    0.87876    0.26    0.797  
subsectorHealth         -0.47115    0.53057   -0.89    0.375  
subsectorLeisure         0.45203    0.83608    0.54    0.589  
subsectorHotelsRestBar  -0.42170    0.53546   -0.79    0.431  
subsectorOther           0.60460    1.40480    0.43    0.667  
sizeMedium              -0.25985    0.25941   -1.00    0.316  
sizeSmall               -0.48418    0.26193   -1.85    0.065 .
sizeMicro               -0.64493    0.26192   -2.46    0.014 *
log_pop                  0.05847    0.21019    0.28    0.781  
log_wpn_crim            -0.21672    0.26428   -0.82    0.412  
log_bribe_vic           -0.16226    0.30892   -0.53    0.599  
log_nbus                 0.34695    0.51101    0.68    0.497  
log_drug_crim            0.01575    0.13905    0.11    0.910  
comp_index               0.00717    0.01308    0.55    0.584  
law_index               -0.00108    0.01114   -0.10    0.923  
log_deaths               0.26810    0.21486    1.25    0.212  
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
(Intercept) 2.132e-09 4.617e-05

Negative binomial dispersion parameter: 0.061373 (std. err.: 0.0045858)

Log-likelihood: -1596.15 
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
[1] -1596.15

$df
[1] 34

$AIC
[1] 3260.3

$BIC
[1] 3457.099

$alpha
[1] 16.29381

$var_j
[1] 4.544571e-18

$ICC
[1] 2.78914e-19
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
1  33 -1596.9                    
2  34 -1596.2  1  1.54     0.2146
```

```r
vif(mnb_f_deaths)
```

```
                  GVIF Df GVIF^(1/(2*Df))
bribes        1.231634  1        1.109790
yearsquant    1.786366  4        1.075217
subsector     3.437494 15        1.042017
size          1.428236  3        1.061207
log_pop       1.147198  1        1.071073
log_wpn_crim  1.138453  1        1.066983
log_bribe_vic 1.102173  1        1.049844
log_nbus      1.151191  1        1.072936
log_drug_crim 1.173889  1        1.083461
comp_index    1.149899  1        1.072333
law_index     1.292437  1        1.136854
log_deaths    1.167607  1        1.080559
```

```r
vif(mnb_f_deaths)^2
```

```
                   GVIF  Df GVIF^(1/(2*Df))
bribes         1.516922   1        1.231634
yearsquant     3.191102  16        1.156093
subsector     11.816363 225        1.085799
size           2.039859   9        1.126160
log_pop        1.316064   1        1.147198
log_wpn_crim   1.296076   1        1.138453
log_bribe_vic  1.214785   1        1.102173
log_nbus       1.325240   1        1.151191
log_drug_crim  1.378015   1        1.173889
comp_index     1.322267   1        1.149899
law_index      1.670392   1        1.292437
log_deaths     1.363306   1        1.167607
```

```r
lm_f_deaths <- lm(update(formula(nb_final), . ~ . +  log_deaths), 
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
vif(lm_f_deaths)^2
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
screenreg(list("MNB" = mnb_final,
          "With deaths" = mnb_f_deaths),
          single.row = TRUE)
```

```

=============================================================
                         MNB                With deaths      
-------------------------------------------------------------
(Intercept)                 -0.13 (0.54)       -0.06 (0.54)  
bribes                      -0.02 (0.10)       -0.01 (0.10)  
yearsquant(8,16]             0.15 (0.30)        0.14 (0.30)  
yearsquant(16,25]           -0.09 (0.28)       -0.09 (0.28)  
yearsquant(25,34]            0.42 (0.29)        0.36 (0.29)  
yearsquant(34,43]            0.18 (0.30)        0.17 (0.30)  
subsectorMining             -1.45 (0.98)       -1.34 (0.98)  
subsectorConstruction        0.03 (0.54)        0.04 (0.54)  
subsectorManufacturing      -0.23 (0.52)       -0.27 (0.52)  
subsectorWholesale          -0.53 (0.65)       -0.48 (0.65)  
subsectorTransport          -0.53 (0.61)       -0.60 (0.61)  
subsectorMedia              -0.28 (0.81)       -0.28 (0.80)  
subsectorFinance             0.09 (0.87)        0.09 (0.87)  
subsectorReal estate        -1.20 (0.88)       -1.06 (0.89)  
subsectorProf. services      1.14 (0.83)        1.09 (0.83)  
subsectorMaintenance        -0.57 (0.58)       -0.62 (0.58)  
subsectorEducation           0.30 (0.88)        0.23 (0.88)  
subsectorHealth             -0.48 (0.53)       -0.47 (0.53)  
subsectorLeisure             0.39 (0.84)        0.45 (0.84)  
subsectorHotelsRestBar      -0.40 (0.54)       -0.42 (0.54)  
subsectorOther               0.65 (1.41)        0.60 (1.40)  
sizeMedium                  -0.24 (0.26)       -0.26 (0.26)  
sizeSmall                   -0.48 (0.26)       -0.48 (0.26)  
sizeMicro                   -0.63 (0.26) *     -0.64 (0.26) *
log_pop                      0.15 (0.20)        0.06 (0.21)  
log_wpn_crim                -0.01 (0.21)       -0.22 (0.26)  
log_bribe_vic               -0.19 (0.31)       -0.16 (0.31)  
log_nbus                     0.35 (0.51)        0.35 (0.51)  
log_drug_crim                0.01 (0.14)        0.02 (0.14)  
comp_index                   0.00 (0.01)        0.01 (0.01)  
law_index                   -0.00 (0.01)       -0.00 (0.01)  
log_deaths                                      0.27 (0.21)  
-------------------------------------------------------------
Variance: state              0.00               0.00         
Dispersion: parameter        0.06               0.06         
Dispersion: SD               0.00               0.00         
AIC                       3259.84            3260.30         
BIC                       3450.85            3457.10         
Log Likelihood           -1596.92           -1596.15         
Num. obs.                 2412               2412            
Num. groups: state          32                 32            
=============================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Vs Poisson

mp_final <- glmmadmb(formula(mnb_final), family = "poisson", zeroInflation = FALSE,
                     data = enve_model)
```

```
Warning: running command './glmmadmb -maxfn 500 -maxph 5 -noinit -shess'
had status 1
```

```
Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative
```

```
Error in glmmadmb(formula(mnb_final), family = "poisson", zeroInflation = FALSE, : The function maximizer failed (couldn't find parameter file) Troubleshooting steps include (1) run with 'save.dir' set and inspect output files; (2) change run parameters: see '?admbControl';(3) re-run with debug=TRUE for more information on failure mode
```

```r
summary(mp_final)
```

```
Error in summary(mp_final): object 'mp_final' not found
```

```r
get_glmmadmb(mp_final)
```

```
Error in get_glmmadmb(mp_final): object 'mp_final' not found
```

```r
lrtest(mp_final, mnb_final)
```

```
Error in lrtest(mp_final, mnb_final): object 'mp_final' not found
```

```r
screenreg(list("MNB" = mnb_final, "M Poisson" = mp_final), single.row = TRUE)
```

```
Error in "list" %in% class(l)[1]: object 'mp_final' not found
```

## Zero-inflated models

Multilevel zero inflated using glmmadmb
- fully specified: MZINB vs MNB vs MZIP
- null: MZINB vs MNB
- Full and null: MZINB vs ZINB
- No state: MZINB vs MNB
- No bus: MZINB vs MNB


```r
mzinb_full <- glmmadmb(formula(mnb_final), data = enve_model,
                       family = "nbinom", zeroInflation = TRUE)

summary(mzinb_full)
```

```

Call:
glmmadmb(formula = formula(mnb_final), data = enve_model, family = "nbinom", 
    zeroInflation = TRUE)

AIC: 3261.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12540    0.53633   -0.23    0.815  
bribes                  -0.01916    0.09757   -0.20    0.844  
yearsquant(8,16]         0.14875    0.29612    0.50    0.615  
yearsquant(16,25]       -0.08703    0.28498   -0.31    0.760  
yearsquant(25,34]        0.41854    0.28644    1.46    0.144  
yearsquant(34,43]        0.17586    0.29845    0.59    0.556  
subsectorMining         -1.45144    0.97527   -1.49    0.137  
subsectorConstruction    0.03165    0.53825    0.06    0.953  
subsectorManufacturing  -0.22574    0.52020   -0.43    0.664  
subsectorWholesale      -0.52602    0.64538   -0.82    0.415  
subsectorTransport      -0.52599    0.60946   -0.86    0.388  
subsectorMedia          -0.28247    0.80535   -0.35    0.726  
subsectorFinance         0.09113    0.87222    0.10    0.917  
subsectorReal estate    -1.19608    0.87932   -1.36    0.174  
subsectorProf. services  1.14363    0.83472    1.37    0.171  
subsectorMaintenance    -0.57262    0.58375   -0.98    0.327  
subsectorEducation       0.29662    0.87864    0.34    0.736  
subsectorHealth         -0.48442    0.53048   -0.91    0.361  
subsectorLeisure         0.39331    0.83644    0.47    0.638  
subsectorHotelsRestBar  -0.40357    0.53557   -0.75    0.451  
subsectorOther           0.65439    1.40760    0.46    0.642  
sizeMedium              -0.23685    0.25935   -0.91    0.361  
sizeSmall               -0.47992    0.26220   -1.83    0.067 .
sizeMicro               -0.62530    0.26240   -2.38    0.017 *
log_pop                  0.15010    0.19633    0.76    0.445  
log_wpn_crim            -0.00677    0.20603   -0.03    0.974  
log_bribe_vic           -0.18602    0.31241   -0.60    0.552  
log_nbus                 0.34860    0.50805    0.69    0.493  
log_drug_crim            0.01341    0.14108    0.10    0.924  
comp_index               0.00262    0.01263    0.21    0.835  
law_index               -0.00446    0.01093   -0.41    0.683  
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
(Intercept) 3.02e-08 0.0001738

Negative binomial dispersion parameter: 0.061114 (std. err.: 0.0045611)
Zero-inflation: 1e-06  (std. err.:  3.6308e-07 )

Log-likelihood: -1596.92 
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
[1] -1596.92

$df
[1] 34

$AIC
[1] 3261.84

$BIC
[1] 3458.639

$alpha
[1] 16.36286

$var_j
[1] 9.1204e-16

$ICC
[1] 5.573841e-17
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
1  33 -1596.9                    
2  34 -1596.9  1     0          1
```

```r
mzip_full <- glmmadmb(formula(mnb_final), data = enve_model,
                       family = "poisson", zeroInflation = TRUE)

summary(mzip_full)
```

```

Call:
glmmadmb(formula = formula(mnb_final), data = enve_model, family = "poisson", 
    zeroInflation = TRUE)

AIC: 3754.1 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              2.06016    0.20214   10.19  < 2e-16 ***
bribes                   0.05340    0.04520    1.18  0.23735    
yearsquant(8,16]         0.02323    0.11701    0.20  0.84264    
yearsquant(16,25]       -0.10228    0.11977   -0.85  0.39313    
yearsquant(25,34]        0.05947    0.11995    0.50  0.62002    
yearsquant(34,43]        0.02656    0.12016    0.22  0.82505    
subsectorMining         -0.45589    0.45452   -1.00  0.31586    
subsectorConstruction   -0.61327    0.18370   -3.34  0.00084 ***
subsectorManufacturing  -1.22553    0.18074   -6.78  1.2e-11 ***
subsectorWholesale      -1.74658    0.26787   -6.52  7.0e-11 ***
subsectorTransport      -0.75356    0.21390   -3.52  0.00043 ***
subsectorMedia          -1.96067    0.27136   -7.23  5.0e-13 ***
subsectorFinance        -0.34263    0.27053   -1.27  0.20533    
subsectorReal estate    -0.80092    0.39796   -2.01  0.04416 *  
subsectorProf. services -0.12528    0.21313   -0.59  0.55665    
subsectorMaintenance    -1.01310    0.22420   -4.52  6.2e-06 ***
subsectorEducation      -1.70045    0.43517   -3.91  9.3e-05 ***
subsectorHealth         -1.01779    0.18639   -5.46  4.7e-08 ***
subsectorLeisure        -0.12786    0.29379   -0.44  0.66342    
subsectorHotelsRestBar  -0.73225    0.17845   -4.10  4.1e-05 ***
subsectorOther          -1.36462    0.34735   -3.93  8.5e-05 ***
sizeMedium               0.19695    0.10706    1.84  0.06582 .  
sizeSmall               -0.07378    0.10741   -0.69  0.49214    
sizeMicro               -0.47524    0.11448   -4.15  3.3e-05 ***
log_pop                  0.20573    0.15105    1.36  0.17319    
log_wpn_crim             0.30004    0.15600    1.92  0.05444 .  
log_bribe_vic            0.39605    0.24023    1.65  0.09923 .  
log_nbus                -0.57852    0.40773   -1.42  0.15594    
log_drug_crim           -0.20620    0.11196   -1.84  0.06551 .  
comp_index              -0.01342    0.00978   -1.37  0.17014    
law_index                0.01100    0.00805    1.37  0.17162    
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
(Intercept)   0.1225   0.35

Zero-inflation: 0.86201  (std. err.:  0.0077677 )

Log-likelihood: -1844.05 
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
[1] -1844.05

$df
[1] 33

$AIC
[1] 3754.1

$BIC
[1] 3945.111

$alpha
numeric(0)

$var_j
[1] 0.01501115

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
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  33 -1844.0                         
2  34 -1596.9  1 494.26  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
zinb_full <- update(mzinb_full, . ~ . - (1 | state))

summary(zinb_full)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "nbinom", zeroInflation = TRUE)

AIC: 3259.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.12540    0.53633   -0.23    0.815  
bribes                  -0.01916    0.09757   -0.20    0.844  
yearsquant(8,16]         0.14875    0.29612    0.50    0.615  
yearsquant(16,25]       -0.08703    0.28498   -0.31    0.760  
yearsquant(25,34]        0.41855    0.28644    1.46    0.144  
yearsquant(34,43]        0.17586    0.29845    0.59    0.556  
subsectorMining         -1.45145    0.97527   -1.49    0.137  
subsectorConstruction    0.03165    0.53825    0.06    0.953  
subsectorManufacturing  -0.22574    0.52020   -0.43    0.664  
subsectorWholesale      -0.52603    0.64538   -0.82    0.415  
subsectorTransport      -0.52600    0.60946   -0.86    0.388  
subsectorMedia          -0.28247    0.80535   -0.35    0.726  
subsectorFinance         0.09112    0.87221    0.10    0.917  
subsectorReal estate    -1.19608    0.87932   -1.36    0.174  
subsectorProf. services  1.14363    0.83472    1.37    0.171  
subsectorMaintenance    -0.57262    0.58375   -0.98    0.327  
subsectorEducation       0.29661    0.87864    0.34    0.736  
subsectorHealth         -0.48442    0.53048   -0.91    0.361  
subsectorLeisure         0.39331    0.83644    0.47    0.638  
subsectorHotelsRestBar  -0.40357    0.53557   -0.75    0.451  
subsectorOther           0.65438    1.40760    0.46    0.642  
sizeMedium              -0.23685    0.25935   -0.91    0.361  
sizeSmall               -0.47992    0.26220   -1.83    0.067 .
sizeMicro               -0.62530    0.26240   -2.38    0.017 *
log_pop                  0.15010    0.19633    0.76    0.445  
log_wpn_crim            -0.00676    0.20603   -0.03    0.974  
log_bribe_vic           -0.18602    0.31241   -0.60    0.552  
log_nbus                 0.34861    0.50805    0.69    0.493  
log_drug_crim            0.01341    0.14108    0.10    0.924  
comp_index               0.00262    0.01263    0.21    0.835  
law_index               -0.00446    0.01093   -0.41    0.683  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.06103 (std. err.: 0.004557)
Zero-inflation: 3.5341e-06  (std. err.:  0.0025321 )

Log-likelihood: -1596.92 
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
[1] -1596.92

$df
[1] 33

$AIC
[1] 3259.84

$BIC
[1] 3450.851

$alpha
[1] 16.38538
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
1  33 -1596.9                    
2  34 -1596.9  1     0          1
```

```r
screenreg(list("MNB" = mnb_final,
               "MZINB" = mzinb_full,
               "MZIP" = mzip_full,
               "ZINB" = zinb_full),
          single.row = TRUE)
```

```

=======================================================================================================
                           MNB                MZINB              MZIP                 ZINB             
-------------------------------------------------------------------------------------------------------
(Intercept)                   -0.13 (0.54)       -0.13 (0.54)        2.06 (0.20) ***     -0.13 (0.54)  
bribes                        -0.02 (0.10)       -0.02 (0.10)        0.05 (0.05)         -0.02 (0.10)  
yearsquant(8,16]               0.15 (0.30)        0.15 (0.30)        0.02 (0.12)          0.15 (0.30)  
yearsquant(16,25]             -0.09 (0.28)       -0.09 (0.28)       -0.10 (0.12)         -0.09 (0.28)  
yearsquant(25,34]              0.42 (0.29)        0.42 (0.29)        0.06 (0.12)          0.42 (0.29)  
yearsquant(34,43]              0.18 (0.30)        0.18 (0.30)        0.03 (0.12)          0.18 (0.30)  
subsectorMining               -1.45 (0.98)       -1.45 (0.98)       -0.46 (0.45)         -1.45 (0.98)  
subsectorConstruction          0.03 (0.54)        0.03 (0.54)       -0.61 (0.18) ***      0.03 (0.54)  
subsectorManufacturing        -0.23 (0.52)       -0.23 (0.52)       -1.23 (0.18) ***     -0.23 (0.52)  
subsectorWholesale            -0.53 (0.65)       -0.53 (0.65)       -1.75 (0.27) ***     -0.53 (0.65)  
subsectorTransport            -0.53 (0.61)       -0.53 (0.61)       -0.75 (0.21) ***     -0.53 (0.61)  
subsectorMedia                -0.28 (0.81)       -0.28 (0.81)       -1.96 (0.27) ***     -0.28 (0.81)  
subsectorFinance               0.09 (0.87)        0.09 (0.87)       -0.34 (0.27)          0.09 (0.87)  
subsectorReal estate          -1.20 (0.88)       -1.20 (0.88)       -0.80 (0.40) *       -1.20 (0.88)  
subsectorProf. services        1.14 (0.83)        1.14 (0.83)       -0.13 (0.21)          1.14 (0.83)  
subsectorMaintenance          -0.57 (0.58)       -0.57 (0.58)       -1.01 (0.22) ***     -0.57 (0.58)  
subsectorEducation             0.30 (0.88)        0.30 (0.88)       -1.70 (0.44) ***      0.30 (0.88)  
subsectorHealth               -0.48 (0.53)       -0.48 (0.53)       -1.02 (0.19) ***     -0.48 (0.53)  
subsectorLeisure               0.39 (0.84)        0.39 (0.84)       -0.13 (0.29)          0.39 (0.84)  
subsectorHotelsRestBar        -0.40 (0.54)       -0.40 (0.54)       -0.73 (0.18) ***     -0.40 (0.54)  
subsectorOther                 0.65 (1.41)        0.65 (1.41)       -1.36 (0.35) ***      0.65 (1.41)  
sizeMedium                    -0.24 (0.26)       -0.24 (0.26)        0.20 (0.11)         -0.24 (0.26)  
sizeSmall                     -0.48 (0.26)       -0.48 (0.26)       -0.07 (0.11)         -0.48 (0.26)  
sizeMicro                     -0.63 (0.26) *     -0.63 (0.26) *     -0.48 (0.11) ***     -0.63 (0.26) *
log_pop                        0.15 (0.20)        0.15 (0.20)        0.21 (0.15)          0.15 (0.20)  
log_wpn_crim                  -0.01 (0.21)       -0.01 (0.21)        0.30 (0.16)         -0.01 (0.21)  
log_bribe_vic                 -0.19 (0.31)       -0.19 (0.31)        0.40 (0.24)         -0.19 (0.31)  
log_nbus                       0.35 (0.51)        0.35 (0.51)       -0.58 (0.41)          0.35 (0.51)  
log_drug_crim                  0.01 (0.14)        0.01 (0.14)       -0.21 (0.11)          0.01 (0.14)  
comp_index                     0.00 (0.01)        0.00 (0.01)       -0.01 (0.01)          0.00 (0.01)  
law_index                     -0.00 (0.01)       -0.00 (0.01)        0.01 (0.01)         -0.00 (0.01)  
-------------------------------------------------------------------------------------------------------
Variance: state                0.00               0.00               0.12                              
Dispersion: parameter          0.06               0.06                                    0.06         
Dispersion: SD                 0.00               0.00                                    0.00         
AIC                         3259.84            3261.84            3754.10              3259.84         
BIC                         3450.85            3458.64            3945.11              3450.85         
Log Likelihood             -1596.92           -1596.92           -1844.05             -1596.92         
Num. obs.                   2412               2412               2412                 2412            
Num. groups: state            32                 32                 32                                 
Zero inflation: parameter                         0.00               0.86                 0.00         
Zero inflation: SD                                0.00               0.01                 0.00         
Num. groups:                                                                              1            
=======================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# Dropping state vars

mzinb_no_state <- glmmadmb(formula(mnb1), data = enve_model,
                       family = "nbinom", zeroInflation = TRUE)

summary(mzinb_no_state)
```

```

Call:
glmmadmb(formula = formula(mnb1), data = enve_model, family = "nbinom", 
    zeroInflation = TRUE)

AIC: 3249.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11177    0.53345   -0.21    0.834  
bribes                  -0.01377    0.09581   -0.14    0.886  
yearsquant(8,16]         0.16349    0.29371    0.56    0.578  
yearsquant(16,25]       -0.08373    0.27883   -0.30    0.764  
yearsquant(25,34]        0.42868    0.28252    1.52    0.129  
yearsquant(34,43]        0.20492    0.29258    0.70    0.484  
subsectorMining         -1.48226    0.96736   -1.53    0.125  
subsectorConstruction   -0.00569    0.53084   -0.01    0.991  
subsectorManufacturing  -0.26660    0.51539   -0.52    0.605  
subsectorWholesale      -0.62522    0.63926   -0.98    0.328  
subsectorTransport      -0.60035    0.60463   -0.99    0.321  
subsectorMedia          -0.35723    0.80355   -0.44    0.657  
subsectorFinance         0.00676    0.85954    0.01    0.994  
subsectorReal estate    -1.34663    0.86635   -1.55    0.120  
subsectorProf. services  1.07371    0.82593    1.30    0.194  
subsectorMaintenance    -0.65263    0.57175   -1.14    0.254  
subsectorEducation       0.02686    0.84630    0.03    0.975  
subsectorHealth         -0.53437    0.52973   -1.01    0.313  
subsectorLeisure         0.25183    0.81771    0.31    0.758  
subsectorHotelsRestBar  -0.42698    0.53082   -0.80    0.421  
subsectorOther           0.56951    1.40450    0.41    0.685  
sizeMedium              -0.24832    0.25851   -0.96    0.337  
sizeSmall               -0.49627    0.26068   -1.90    0.057 .
sizeMicro               -0.60290    0.25864   -2.33    0.020 *
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

Negative binomial dispersion parameter: 0.06086 (std. err.: 0.0045386)
Zero-inflation: 1.0005e-06  (std. err.:  4.4851e-05 )

Log-likelihood: -1597.8 
```

```r
get_glmmadmb(mzinb_no_state)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -1597.8

$df
[1] 27

$AIC
[1] 3249.6

$BIC
[1] 3405.882

$alpha
[1] 16.43115

$var_j
[1] 4.248545e-18

$ICC
[1] 2.585665e-19
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
1  27 -1597.8                    
2  34 -1596.9  7  1.76     0.9719
```

```r
# Dropping bus vars

mzinb_only_state <- glmmadmb(formula(mnb_only_state), data = enve_model,
                       family = "nbinom", zeroInflation = TRUE)

summary(mzinb_only_state)
```

```

Call:
glmmadmb(formula = formula(mnb_only_state), data = enve_model, 
    family = "nbinom", zeroInflation = TRUE)

AIC: 3240.9 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.53522    0.11138   -4.81  1.5e-06 ***
log_pop        0.05453    0.19364    0.28     0.78    
log_wpn_crim   0.03145    0.20431    0.15     0.88    
log_drug_crim  0.03643    0.13234    0.28     0.78    
log_bribe_vic -0.21114    0.30950   -0.68     0.50    
log_nbus       0.40978    0.46996    0.87     0.38    
comp_index     0.00435    0.01249    0.35     0.73    
law_index     -0.00139    0.00991   -0.14     0.89    
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
(Intercept) 2.062e-09 4.541e-05

Negative binomial dispersion parameter: 0.057487 (std. err.: 0.0042367)
Zero-inflation: 1e-06  (std. err.:  4.7609e-07 )

Log-likelihood: -1609.43 
```

```r
get_glmmadmb(mzinb_only_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -1609.43

$df
[1] 11

$AIC
[1] 3240.86

$BIC
[1] 3304.53

$alpha
[1] 17.39524

$var_j
[1] 4.251019e-18

$ICC
[1] 2.443783e-19
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
1  11 -1609.4                    
2  34 -1596.9 23 25.02     0.3493
```

```r
screenreg(list("MZINB Full" = mzinb_full,
               "No state" = mzinb_no_state,
               "No bus" = mzinb_only_state), 
          single.row = TRUE)
```

```

====================================================================================
                           MZINB Full         No state           No bus             
------------------------------------------------------------------------------------
(Intercept)                   -0.13 (0.54)       -0.11 (0.53)       -0.54 (0.11) ***
bribes                        -0.02 (0.10)       -0.01 (0.10)                       
yearsquant(8,16]               0.15 (0.30)        0.16 (0.29)                       
yearsquant(16,25]             -0.09 (0.28)       -0.08 (0.28)                       
yearsquant(25,34]              0.42 (0.29)        0.43 (0.28)                       
yearsquant(34,43]              0.18 (0.30)        0.20 (0.29)                       
subsectorMining               -1.45 (0.98)       -1.48 (0.97)                       
subsectorConstruction          0.03 (0.54)       -0.01 (0.53)                       
subsectorManufacturing        -0.23 (0.52)       -0.27 (0.52)                       
subsectorWholesale            -0.53 (0.65)       -0.63 (0.64)                       
subsectorTransport            -0.53 (0.61)       -0.60 (0.60)                       
subsectorMedia                -0.28 (0.81)       -0.36 (0.80)                       
subsectorFinance               0.09 (0.87)        0.01 (0.86)                       
subsectorReal estate          -1.20 (0.88)       -1.35 (0.87)                       
subsectorProf. services        1.14 (0.83)        1.07 (0.83)                       
subsectorMaintenance          -0.57 (0.58)       -0.65 (0.57)                       
subsectorEducation             0.30 (0.88)        0.03 (0.85)                       
subsectorHealth               -0.48 (0.53)       -0.53 (0.53)                       
subsectorLeisure               0.39 (0.84)        0.25 (0.82)                       
subsectorHotelsRestBar        -0.40 (0.54)       -0.43 (0.53)                       
subsectorOther                 0.65 (1.41)        0.57 (1.40)                       
sizeMedium                    -0.24 (0.26)       -0.25 (0.26)                       
sizeSmall                     -0.48 (0.26)       -0.50 (0.26)                       
sizeMicro                     -0.63 (0.26) *     -0.60 (0.26) *                     
log_pop                        0.15 (0.20)                           0.05 (0.19)    
log_wpn_crim                  -0.01 (0.21)                           0.03 (0.20)    
log_bribe_vic                 -0.19 (0.31)                          -0.21 (0.31)    
log_nbus                       0.35 (0.51)                           0.41 (0.47)    
log_drug_crim                  0.01 (0.14)                           0.04 (0.13)    
comp_index                     0.00 (0.01)                           0.00 (0.01)    
law_index                     -0.00 (0.01)                          -0.00 (0.01)    
------------------------------------------------------------------------------------
Variance: state                0.00               0.00               0.00           
Dispersion: parameter          0.06               0.06               0.06           
Dispersion: SD                 0.00               0.00               0.00           
Zero inflation: parameter      0.00               0.00               0.00           
Zero inflation: SD             0.00               0.00               0.00           
AIC                         3261.84            3249.60            3240.86           
BIC                         3458.64            3405.88            3304.53           
Log Likelihood             -1596.92           -1597.80           -1609.43           
Num. obs.                   2412               2412               2412              
Num. groups: state            32                 32                 32              
====================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# include deaths

mzinb_deaths<- update(mzinb_full, . ~ . + log_deaths)

summary(mzinb_deaths)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index + log_deaths, 
    data = enve_model, family = "nbinom", zeroInflation = TRUE)

AIC: 3262.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.05715    0.53833   -0.11    0.915  
bribes                  -0.00847    0.09770   -0.09    0.931  
yearsquant(8,16]         0.13604    0.29581    0.46    0.646  
yearsquant(16,25]       -0.09482    0.28433   -0.33    0.739  
yearsquant(25,34]        0.35701    0.29107    1.23    0.220  
yearsquant(34,43]        0.17087    0.29876    0.57    0.567  
subsectorMining         -1.33898    0.97707   -1.37    0.171  
subsectorConstruction    0.03665    0.53773    0.07    0.946  
subsectorManufacturing  -0.26814    0.52117   -0.51    0.607  
subsectorWholesale      -0.47881    0.64901   -0.74    0.461  
subsectorTransport      -0.60426    0.61172   -0.99    0.323  
subsectorMedia          -0.27519    0.80395   -0.34    0.732  
subsectorFinance         0.08536    0.87304    0.10    0.922  
subsectorReal estate    -1.05793    0.88509   -1.20    0.232  
subsectorProf. services  1.09332    0.83337    1.31    0.190  
subsectorMaintenance    -0.62110    0.58388   -1.06    0.287  
subsectorEducation       0.22563    0.87876    0.26    0.797  
subsectorHealth         -0.47118    0.53057   -0.89    0.375  
subsectorLeisure         0.45202    0.83609    0.54    0.589  
subsectorHotelsRestBar  -0.42170    0.53546   -0.79    0.431  
subsectorOther           0.60460    1.40480    0.43    0.667  
sizeMedium              -0.25986    0.25942   -1.00    0.316  
sizeSmall               -0.48418    0.26193   -1.85    0.065 .
sizeMicro               -0.64493    0.26192   -2.46    0.014 *
log_pop                  0.05847    0.21019    0.28    0.781  
log_wpn_crim            -0.21672    0.26428   -0.82    0.412  
log_bribe_vic           -0.16225    0.30892   -0.53    0.599  
log_nbus                 0.34694    0.51101    0.68    0.497  
log_drug_crim            0.01575    0.13905    0.11    0.910  
comp_index               0.00717    0.01308    0.55    0.584  
law_index               -0.00108    0.01114   -0.10    0.923  
log_deaths               0.26809    0.21486    1.25    0.212  
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

Negative binomial dispersion parameter: 0.061374 (std. err.: 0.0045859)
Zero-inflation: 1e-06  (std. err.:  3.8019e-06 )

Log-likelihood: -1596.15 
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
[1] -1596.15

$df
[1] 35

$AIC
[1] 3262.3

$BIC
[1] 3464.887

$alpha
[1] 16.29354

$var_j
[1] 4.248545e-18

$ICC
[1] 2.607502e-19
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
1  34 -1596.9                    
2  35 -1596.2  1  1.54     0.2146
```

```r
## Null models: MZINB vs MNB

mzinb_null <- update(mzinb_full, . ~ 1 + (1 | state))

summary(mzinb_null)
```

```

Call:
glmmadmb(formula = extortions ~ (1 | state), data = enve_model, 
    family = "nbinom", zeroInflation = TRUE)

AIC: 3228.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
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

Negative binomial dispersion parameter: 0.057237 (std. err.: 0.0042147)
Zero-inflation: 1e-06  (std. err.:  7.5849e-07 )

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(mzinb_null)
```

```
$model
extortions ~ (1 | state)

$logLik
[1] -1610.33

$df
[1] 4

$AIC
[1] 3228.66

$BIC
[1] 3251.813

$alpha
[1] 17.47122

$var_j
[1] 4.248545e-18

$ICC
[1] 2.43174e-19
```

```r
lrtest(mzinb_null, mnb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state)
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   4 -1610.3                    
2   3 -1610.3 -1     0          1
```

```r
## Null models: MZINB vs ZINB

zinb_null <- update(zinb_full, . ~ 1)

summary(zinb_null)
```

```

Call:
glmmadmb(formula = extortions ~ 1, data = enve_model, family = "nbinom", 
    zeroInflation = TRUE)

AIC: 3226.7 

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.5708     0.0894   -6.39  1.7e-10 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.057159 (std. err.: 0.0042081)
Zero-inflation: 1.6702e-06  (std. err.:  0.00066856 )

Log-likelihood: -1610.33 
```

```r
get_glmmadmb(zinb_null)
```

```
$model
extortions ~ 1

$logLik
[1] -1610.33

$df
[1] 3

$AIC
[1] 3226.66

$BIC
[1] 3244.025

$alpha
[1] 17.49506
```

```r
lrtest(zinb_null, mzinb_null)
```

```
Likelihood ratio test

Model 1: extortions ~ 1
Model 2: extortions ~ (1 | state)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   3 -1610.3                    
2   4 -1610.3  1     0          1
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
(Intercept)                   -0.57 (0.09) ***     -0.57 (0.09) ***     -0.57 (0.09) ***
----------------------------------------------------------------------------------------
Variance: state                0.00                 0.00                                
Dispersion: parameter          0.06                 0.06                 0.06           
Dispersion: SD                 0.00                 0.00                 0.00           
AIC                         3226.66              3228.66              3226.66           
BIC                         3244.02              3251.81              3244.02           
Log Likelihood             -1610.33             -1610.33             -1610.33           
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
1  32 -1596.9                    
2  33 -1596.9  1     0          1
```

```r
zip_full <- update(mzip_full, . ~ . - (1 | state))
summary(zip_full)
```

```

Call:
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "poisson", zeroInflation = TRUE)

AIC: 3762.8 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              2.31219    0.17154   13.48  < 2e-16 ***
bribes                   0.10215    0.03900    2.62  0.00881 ** 
yearsquant(8,16]         0.08433    0.11388    0.74  0.45896    
yearsquant(16,25]       -0.08178    0.12325   -0.66  0.50699    
yearsquant(25,34]        0.49371    0.11290    4.37  1.2e-05 ***
yearsquant(34,43]        0.18399    0.11509    1.60  0.10990    
subsectorMining         -1.05955    0.43125   -2.46  0.01401 *  
subsectorConstruction   -0.80929    0.16747   -4.83  1.3e-06 ***
subsectorManufacturing  -0.86320    0.16592   -5.20  2.0e-07 ***
subsectorWholesale      -1.83381    0.25971   -7.06  1.7e-12 ***
subsectorTransport      -0.75446    0.19797   -3.81  0.00014 ***
subsectorMedia          -2.03952    0.25833   -7.90  2.9e-15 ***
subsectorFinance        -0.33504    0.24828   -1.35  0.17719    
subsectorReal estate    -1.45616    0.37827   -3.85  0.00012 ***
subsectorProf. services  0.40935    0.18137    2.26  0.02401 *  
subsectorMaintenance    -0.96536    0.20131   -4.80  1.6e-06 ***
subsectorEducation      -1.84160    0.41869   -4.40  1.1e-05 ***
subsectorHealth         -1.09467    0.17240   -6.35  2.2e-10 ***
subsectorLeisure        -0.32083    0.28156   -1.14  0.25450    
subsectorHotelsRestBar  -0.68046    0.16291   -4.18  3.0e-05 ***
subsectorOther          -1.17902    0.33639   -3.50  0.00046 ***
sizeMedium              -0.05439    0.09402   -0.58  0.56291    
sizeSmall               -0.54350    0.10372   -5.24  1.6e-07 ***
sizeMicro               -0.81745    0.10477   -7.80  6.1e-15 ***
log_pop                  0.20445    0.07040    2.90  0.00369 ** 
log_wpn_crim             0.24224    0.07829    3.09  0.00197 ** 
log_bribe_vic            0.00386    0.11400    0.03  0.97299    
log_nbus                 0.09000    0.19003    0.47  0.63578    
log_drug_crim           -0.09767    0.05221   -1.87  0.06137 .  
comp_index              -0.02197    0.00464   -4.73  2.2e-06 ***
law_index                0.00475    0.00380    1.25  0.21153    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Zero-inflation: 0.86403  (std. err.:  0.007502 )

Log-likelihood: -1849.42 
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
[1] -1849.42

$df
[1] 32

$AIC
[1] 3762.84

$BIC
[1] 3948.063

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
1  32 -1849.4                        
2  33 -1596.9  1   505  < 2.2e-16 ***
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
1   2 -1610.3                    
2   3 -1610.3  1     0          1
```

```r
# no state

zinb_no_state <- glmmadmb(formula(nb1), data = enve_model,
                          family = "nbinom", zeroInflation = TRUE)

summary(zinb_no_state)
```

```

Call:
glmmadmb(formula = formula(nb1), data = enve_model, family = "nbinom", 
    zeroInflation = TRUE)

AIC: 3247.6 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.11178    0.53345   -0.21    0.834  
bribes                  -0.01377    0.09582   -0.14    0.886  
yearsquant(8,16]         0.16350    0.29371    0.56    0.578  
yearsquant(16,25]       -0.08373    0.27883   -0.30    0.764  
yearsquant(25,34]        0.42867    0.28252    1.52    0.129  
yearsquant(34,43]        0.20493    0.29258    0.70    0.484  
subsectorMining         -1.48225    0.96736   -1.53    0.125  
subsectorConstruction   -0.00568    0.53084   -0.01    0.991  
subsectorManufacturing  -0.26659    0.51539   -0.52    0.605  
subsectorWholesale      -0.62520    0.63926   -0.98    0.328  
subsectorTransport      -0.60034    0.60463   -0.99    0.321  
subsectorMedia          -0.35721    0.80355   -0.44    0.657  
subsectorFinance         0.00676    0.85954    0.01    0.994  
subsectorReal estate    -1.34663    0.86635   -1.55    0.120  
subsectorProf. services  1.07373    0.82594    1.30    0.194  
subsectorMaintenance    -0.65263    0.57175   -1.14    0.254  
subsectorEducation       0.02686    0.84630    0.03    0.975  
subsectorHealth         -0.53436    0.52972   -1.01    0.313  
subsectorLeisure         0.25184    0.81771    0.31    0.758  
subsectorHotelsRestBar  -0.42697    0.53081   -0.80    0.421  
subsectorOther           0.56956    1.40460    0.41    0.685  
sizeMedium              -0.24831    0.25851   -0.96    0.337  
sizeSmall               -0.49626    0.26068   -1.90    0.057 .
sizeMicro               -0.60290    0.25864   -2.33    0.020 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.060776 (std. err.: 0.0045332)
Zero-inflation: 2.966e-06  (std. err.:  0.0019629 )

Log-likelihood: -1597.8 
```

```r
get_glmmadmb(zinb_no_state)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size

$logLik
[1] -1597.8

$df
[1] 26

$AIC
[1] 3247.6

$BIC
[1] 3398.094

$alpha
[1] 16.45386
```

```r
lrtest(nb1, zinb_no_state)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size
Model 2: extortions ~ bribes + yearsquant + subsector + size
  #Df  LogLik Df Chisq Pr(>Chisq)
1  25 -1597.8                    
2  26 -1597.8  1     0          1
```

```r
# no business level measurements

zinb_only_state <- glmmadmb(formula(nb_only_state), data = enve_model,
                          family = "nbinom", zeroInflation = TRUE)

summary(zinb_only_state)
```

```

Call:
glmmadmb(formula = formula(nb_only_state), data = enve_model, 
    family = "nbinom", zeroInflation = TRUE)

AIC: 3238.9 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.53522    0.11138   -4.81  1.5e-06 ***
log_pop        0.05453    0.19364    0.28     0.78    
log_wpn_crim   0.03145    0.20431    0.15     0.88    
log_drug_crim  0.03643    0.13234    0.28     0.78    
log_bribe_vic -0.21114    0.30950   -0.68     0.50    
log_nbus       0.40978    0.46996    0.87     0.38    
comp_index     0.00435    0.01249    0.35     0.73    
law_index     -0.00139    0.00991   -0.14     0.89    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412
Negative binomial dispersion parameter: 0.057408 (std. err.: 0.0042301)
Zero-inflation: 1.7183e-06  (std. err.:  0.00072091 )

Log-likelihood: -1609.43 
```

```r
get_glmmadmb(zinb_only_state)
```

```
$model
extortions ~ log_pop + log_wpn_crim + log_drug_crim + log_bribe_vic + 
    log_nbus + comp_index + law_index

$logLik
[1] -1609.43

$df
[1] 10

$AIC
[1] 3238.86

$BIC
[1] 3296.742

$alpha
[1] 17.41918
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
1   9 -1609.4                    
2  10 -1609.4  1     0          1
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
-0.2450 -0.2360 -0.2321 -0.2262 15.1984 

Count model coefficients (negbin with log link):
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -0.125391   0.536329  -0.234   0.8151    
bribes                  -0.019159   0.097572  -0.196   0.8443    
yearsquant(8,16]         0.148751   0.296122   0.502   0.6154    
yearsquant(16,25]       -0.087024   0.284982  -0.305   0.7601    
yearsquant(25,34]        0.418553   0.286444   1.461   0.1440    
yearsquant(34,43]        0.175883   0.298444   0.589   0.5556    
subsectorMining         -1.451455   0.975269  -1.488   0.1367    
subsectorConstruction    0.031653   0.538254   0.059   0.9531    
subsectorManufacturing  -0.225745   0.520197  -0.434   0.6643    
subsectorWholesale      -0.526014   0.645376  -0.815   0.4150    
subsectorTransport      -0.526003   0.609458  -0.863   0.3881    
subsectorMedia          -0.282488   0.805354  -0.351   0.7258    
subsectorFinance         0.091097   0.872215   0.104   0.9168    
subsectorReal estate    -1.196104   0.879322  -1.360   0.1737    
subsectorProf. services  1.143617   0.834723   1.370   0.1707    
subsectorMaintenance    -0.572614   0.583754  -0.981   0.3266    
subsectorEducation       0.296607   0.878634   0.338   0.7357    
subsectorHealth         -0.484394   0.530479  -0.913   0.3612    
subsectorLeisure         0.393299   0.836436   0.470   0.6382    
subsectorHotelsRestBar  -0.403581   0.535573  -0.754   0.4511    
subsectorOther           0.654395   1.407642   0.465   0.6420    
sizeMedium              -0.236837   0.259345  -0.913   0.3611    
sizeSmall               -0.479925   0.262199  -1.830   0.0672 .  
sizeMicro               -0.625298   0.262403  -2.383   0.0172 *  
log_pop                  0.150102   0.196331   0.765   0.4445    
log_wpn_crim            -0.006765   0.206034  -0.033   0.9738    
log_bribe_vic           -0.186036   0.312411  -0.595   0.5515    
log_nbus                 0.348618   0.508047   0.686   0.4926    
log_drug_crim            0.013416   0.141082   0.095   0.9242    
comp_index               0.002624   0.012632   0.208   0.8354    
law_index               -0.004455   0.010927  -0.408   0.6835    
Log(theta)              -2.796398   0.074620 -37.475   <2e-16 ***

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)
(Intercept)   -14.96    2818.90  -0.005    0.996
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.061 
Number of iterations in BFGS optimization: 114 
Log-likelihood: -1597 on 33 Df
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
1  32 -1596.9                     
2  33 -1596.9  1 0.0022     0.9626
```

```r
nb_mass_final <- glm.nb(formula(nb_final), data = enve_model)

summary(nb_mass_final)
```

```

Call:
glm.nb(formula = formula(nb_final), data = enve_model, init.theta = 0.06102952067, 
    link = log)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7096  -0.5456  -0.5113  -0.4718   2.6458  

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -0.125389   0.532978  -0.235   0.8140  
bribes                  -0.019169   0.084013  -0.228   0.8195  
yearsquant(8,16]         0.148750   0.279628   0.532   0.5948  
yearsquant(16,25]       -0.087018   0.273442  -0.318   0.7503  
yearsquant(25,34]        0.418531   0.266486   1.571   0.1163  
yearsquant(34,43]        0.175883   0.278660   0.631   0.5279  
subsectorMining         -1.451476   0.976485  -1.486   0.1372  
subsectorConstruction    0.031669   0.519462   0.061   0.9514  
subsectorManufacturing  -0.225722   0.507025  -0.445   0.6562  
subsectorWholesale      -0.525990   0.620881  -0.847   0.3969  
subsectorTransport      -0.525981   0.601217  -0.875   0.3816  
subsectorMedia          -0.282480   0.793873  -0.356   0.7220  
subsectorFinance         0.091142   0.845980   0.108   0.9142  
subsectorReal estate    -1.196093   0.872870  -1.370   0.1706  
subsectorProf. services  1.143628   0.817360   1.399   0.1618  
subsectorMaintenance    -0.572629   0.559000  -1.024   0.3057  
subsectorEducation       0.296592   0.832274   0.356   0.7216  
subsectorHealth         -0.484384   0.517990  -0.935   0.3497  
subsectorLeisure         0.393291   0.811035   0.485   0.6277  
subsectorHotelsRestBar  -0.403559   0.520555  -0.775   0.4382  
subsectorOther           0.654382   1.398954   0.468   0.6400  
sizeMedium              -0.236834   0.248777  -0.952   0.3411  
sizeSmall               -0.479926   0.252489  -1.901   0.0573 .
sizeMicro               -0.625318   0.249932  -2.502   0.0124 *
log_pop                  0.150101   0.185378   0.810   0.4181  
log_wpn_crim            -0.006752   0.179123  -0.038   0.9699  
log_bribe_vic           -0.186025   0.280858  -0.662   0.5077  
log_nbus                 0.348606   0.474514   0.735   0.4625  
log_drug_crim            0.013415   0.132263   0.101   0.9192  
comp_index               0.002625   0.011492   0.228   0.8193  
law_index               -0.004454   0.009379  -0.475   0.6349  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(0.061) family taken to be 1)

    Null deviance: 786.26  on 2411  degrees of freedom
Residual deviance: 758.65  on 2381  degrees of freedom
AIC: 3257.8

Number of Fisher Scoring iterations: 1

              Theta:  0.06103 
          Std. Err.:  0.00455 

 2 x log-likelihood:  -3193.84200 
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
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -1596.9                    
2  33 -1596.9  1     0     0.9997
```

```r
vuong(nb_mass_final, zinb_pscl_simple_full)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A p-value
Raw                1.352919e-04 model1 > model2 0.49995
AIC-corrected      2.173296e+03 model1 > model2 < 2e-16
BIC-corrected      8.463044e+03 model1 > model2 < 2e-16
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
summary(zinb_pscl_full)
```

```

Call:
zeroinfl(formula = zi_formula, data = enve_model, dist = "negbin")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-0.6056 -0.2722 -0.2423 -0.2085 16.8596 

Count model coefficients (negbin with log link):
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              1.638451   0.575517   2.847 0.004414 ** 
bribes                   0.167207   0.125647   1.331 0.183265    
yearsquant(8,16]         0.191575   0.293369   0.653 0.513745    
yearsquant(16,25]        0.095509   0.283367   0.337 0.736080    
yearsquant(25,34]        0.769833   0.312039   2.467 0.013621 *  
yearsquant(34,43]        0.373065   0.307472   1.213 0.225004    
subsectorMining         -1.566550   1.153592  -1.358 0.174472    
subsectorConstruction   -0.373025   0.588811  -0.634 0.526392    
subsectorManufacturing  -0.892316   0.571612  -1.561 0.118512    
subsectorWholesale      -1.007794   0.685203  -1.471 0.141346    
subsectorTransport      -0.757979   0.658155  -1.152 0.249456    
subsectorMedia          -2.194366   0.633895  -3.462 0.000537 ***
subsectorFinance        -0.420015   0.855869  -0.491 0.623605    
subsectorReal estate    -2.046556   1.023744  -1.999 0.045599 *  
subsectorProf. services  0.427781   0.751710   0.569 0.569303    
subsectorMaintenance    -0.918135   0.644925  -1.424 0.154553    
subsectorEducation       0.032294   0.963840   0.034 0.973271    
subsectorHealth         -0.962624   0.577355  -1.667 0.095455 .  
subsectorLeisure         0.280954   0.819416   0.343 0.731695    
subsectorHotelsRestBar  -0.641298   0.581006  -1.104 0.269692    
subsectorOther          -0.946528   0.794959  -1.191 0.233786    
sizeMedium              -0.042364   0.248961  -0.170 0.864881    
sizeSmall               -0.468260   0.259269  -1.806 0.070907 .  
sizeMicro               -0.923749   0.254937  -3.623 0.000291 ***
log_pop                  0.113482   0.195999   0.579 0.562596    
log_wpn_crim             0.289812   0.233444   1.241 0.214436    
log_bribe_vic           -0.169608   0.331936  -0.511 0.609374    
log_nbus                 0.111387   0.509007   0.219 0.826781    
log_drug_crim           -0.144382   0.149781  -0.964 0.335067    
comp_index              -0.020636   0.012960  -1.592 0.111327    
law_index                0.005087   0.011977   0.425 0.671021    
Log(theta)              -0.810382   0.231244  -3.504 0.000458 ***

Zero-inflation model coefficients (binomial with logit link):
                          Estimate Std. Error z value Pr(>|z|)   
(Intercept)              1.539e+00  4.983e-01   3.088  0.00201 **
bribes                   1.443e-01  8.364e-02   1.725  0.08460 . 
yearsquant(8,16]        -5.802e-02  2.702e-01  -0.215  0.82996   
yearsquant(16,25]        2.461e-01  2.662e-01   0.924  0.35532   
yearsquant(25,34]        5.073e-01  2.635e-01   1.925  0.05418 . 
yearsquant(34,43]        3.134e-01  2.737e-01   1.145  0.25211   
subsectorMining         -1.335e-01  1.059e+00  -0.126  0.89966   
subsectorConstruction   -4.131e-01  4.717e-01  -0.876  0.38113   
subsectorManufacturing  -6.655e-01  4.728e-01  -1.408  0.15927   
subsectorWholesale      -8.110e-01  5.822e-01  -1.393  0.16360   
subsectorTransport      -2.676e-01  5.558e-01  -0.481  0.63021   
subsectorMedia          -1.929e+01  2.251e+03  -0.009  0.99316   
subsectorFinance        -6.002e-01  7.484e-01  -0.802  0.42253   
subsectorReal estate    -7.383e-01  1.191e+00  -0.620  0.53532   
subsectorProf. services -8.327e-01  6.406e-01  -1.300  0.19369   
subsectorMaintenance    -3.163e-01  5.256e-01  -0.602  0.54736   
subsectorEducation      -1.244e-01  7.443e-01  -0.167  0.86730   
subsectorHealth         -5.606e-01  4.782e-01  -1.172  0.24111   
subsectorLeisure        -3.866e-01  6.686e-01  -0.578  0.56309   
subsectorHotelsRestBar  -3.300e-01  4.746e-01  -0.695  0.48683   
subsectorOther          -2.208e+01  1.769e+04  -0.001  0.99900   
sizeMedium               2.751e-01  2.215e-01   1.242  0.21410   
sizeSmall               -7.022e-02  2.313e-01  -0.304  0.76146   
sizeMicro               -3.339e-01  2.480e-01  -1.346  0.17818   
log_pop                  5.575e-03  1.754e-01   0.032  0.97465   
log_wpn_crim             2.192e-01  1.863e-01   1.177  0.23922   
log_bribe_vic           -1.861e-01  2.749e-01  -0.677  0.49826   
log_nbus                 6.441e-03  4.492e-01   0.014  0.98856   
log_drug_crim           -7.666e-02  1.282e-01  -0.598  0.54975   
comp_index              -2.379e-02  1.122e-02  -2.120  0.03403 * 
law_index                5.410e-03  9.477e-03   0.571  0.56808   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.4447 
Number of iterations in BFGS optimization: 83 
Log-likelihood: -1556 on 63 Df
```

```r
lrtest(nb_final, zinb_pscl_full)
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
    comp_index + law_index | bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  32 -1596.9                         
2  63 -1555.6 31 82.555  1.434e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
lrtest(zinb_pscl_simple_full ,zinb_pscl_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  33 -1596.9                         
2  63 -1555.6 30 82.558  8.418e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
lrtest(nb_mass_final, zinb_pscl_full)
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
    comp_index + law_index | bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  32 -1596.9                         
2  63 -1555.6 31 82.558  1.433e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
vuong(nb_mass_final, zinb_pscl_full)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A    p-value
Raw                   -4.443355 model2 > model1 4.4283e-06
AIC-corrected         -1.106432 model2 > model1    0.13427
BIC-corrected          8.550975 model1 > model2 < 2.22e-16
```

```r
## Constant on either side of equation

zinb_pscl_nullc <- update(zinb_pscl_full, . ~ 1 | . )

summary(zinb_pscl_nullc)
```

```

Call:
zeroinfl(formula = extortions ~ 1 | bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index, data = enve_model, dist = "negbin")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-0.4292 -0.2456 -0.2252 -0.2009 15.8689 

Count model coefficients (negbin with log link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   0.4990     0.1601   3.116  0.00183 ** 
Log(theta)   -1.5730     0.2037  -7.723 1.14e-14 ***

Zero-inflation model coefficients (binomial with logit link):
                          Estimate Std. Error z value Pr(>|z|)
(Intercept)              7.383e-01  5.630e-01   1.311    0.190
bribes                   9.976e-02  8.614e-02   1.158    0.247
yearsquant(8,16]        -1.800e-01  2.699e-01  -0.667    0.505
yearsquant(16,25]        2.224e-01  2.629e-01   0.846    0.398
yearsquant(25,34]        2.137e-01  2.589e-01   0.826    0.409
yearsquant(34,43]        1.736e-01  2.711e-01   0.640    0.522
subsectorMining          5.238e-01  9.647e-01   0.543    0.587
subsectorConstruction   -3.588e-01  5.123e-01  -0.700    0.484
subsectorManufacturing  -4.045e-01  5.014e-01  -0.807    0.420
subsectorWholesale      -5.239e-01  6.035e-01  -0.868    0.385
subsectorTransport      -1.914e-02  5.861e-01  -0.033    0.974
subsectorMedia          -2.371e+01  3.818e+04  -0.001    1.000
subsectorFinance        -5.790e-01  8.343e-01  -0.694    0.488
subsectorReal estate     4.015e-01  8.552e-01   0.470    0.639
subsectorProf. services -1.304e+00  8.755e-01  -1.490    0.136
subsectorMaintenance    -1.193e-02  5.472e-01  -0.022    0.983
subsectorEducation      -1.592e-01  8.321e-01  -0.191    0.848
subsectorHealth         -2.477e-01  5.084e-01  -0.487    0.626
subsectorLeisure        -5.914e-01  7.922e-01  -0.747    0.455
subsectorHotelsRestBar  -1.549e-01  5.115e-01  -0.303    0.762
subsectorOther          -1.784e+01  2.284e+03  -0.008    0.994
sizeMedium               3.564e-01  2.464e-01   1.446    0.148
sizeSmall                1.157e-01  2.452e-01   0.472    0.637
sizeMicro                1.056e-01  2.422e-01   0.436    0.663
log_pop                 -5.976e-02  1.798e-01  -0.332    0.740
log_wpn_crim             9.562e-02  1.750e-01   0.546    0.585
log_bribe_vic           -1.495e-01  2.789e-01  -0.536    0.592
log_nbus                -3.418e-02  4.600e-01  -0.074    0.941
log_drug_crim           -1.749e-03  1.266e-01  -0.014    0.989
comp_index              -1.780e-02  1.151e-02  -1.547    0.122
law_index                3.338e-03  9.269e-03   0.360    0.719
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.2074 
Number of iterations in BFGS optimization: 57 
Log-likelihood: -1586 on 33 Df
```

```r
lrtest(zinb_pscl_nullc, zinb_pscl_full)
```

```
Likelihood ratio test

Model 1: extortions ~ 1 | bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  33 -1586.2                         
2  63 -1555.6 30 61.202   0.000657 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
vuong(zinb_pscl_nullc, zinb_pscl_full)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A    p-value
Raw                  -3.9105314 model2 > model1 4.6047e-05
AIC-corrected        -0.0767689 model2 > model1     0.4694
BIC-corrected        11.0185454 model1 > model2 < 2.22e-16
```

```r
zinb_pscl_c_all_z_state <- update(zinb_pscl_full, . ~ . | . - bribes - 
                             yearsquant - 
                             subsector -
                             size )

summary(zinb_pscl_c_all_z_state)
```

```

Call:
zeroinfl(formula = extortions ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index, data = enve_model, 
    dist = "negbin")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-0.2658 -0.2487 -0.2383 -0.2098 14.8844 

Count model coefficients (negbin with log link):
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              0.108720   0.549532   0.198   0.8432    
bribes                   0.015303   0.102224   0.150   0.8810    
yearsquant(8,16]         0.155316   0.295803   0.525   0.5995    
yearsquant(16,25]       -0.093829   0.282684  -0.332   0.7399    
yearsquant(25,34]        0.459539   0.284145   1.617   0.1058    
yearsquant(34,43]        0.169406   0.299462   0.566   0.5716    
subsectorMining         -1.338063   0.980572  -1.365   0.1724    
subsectorConstruction   -0.030976   0.540338  -0.057   0.9543    
subsectorManufacturing  -0.304443   0.521628  -0.584   0.5595    
subsectorWholesale      -0.423191   0.646703  -0.654   0.5129    
subsectorTransport      -0.576835   0.609709  -0.946   0.3441    
subsectorMedia          -0.347786   0.787176  -0.442   0.6586    
subsectorFinance         0.011654   0.853668   0.014   0.9891    
subsectorReal estate    -1.286046   0.872699  -1.474   0.1406    
subsectorProf. services  1.106185   0.835525   1.324   0.1855    
subsectorMaintenance    -0.577588   0.587242  -0.984   0.3253    
subsectorEducation       0.262317   0.881225   0.298   0.7660    
subsectorHealth         -0.488750   0.530878  -0.921   0.3572    
subsectorLeisure         0.587255   0.837786   0.701   0.4833    
subsectorHotelsRestBar  -0.408400   0.536961  -0.761   0.4469    
subsectorOther           0.698593   1.337019   0.523   0.6013    
sizeMedium              -0.247919   0.256448  -0.967   0.3337    
sizeSmall               -0.470175   0.262342  -1.792   0.0731 .  
sizeMicro               -0.667430   0.261489  -2.552   0.0107 *  
log_pop                  0.149141   0.221216   0.674   0.5002    
log_wpn_crim             0.189090   0.232352   0.814   0.4158    
log_bribe_vic           -0.108660   0.355823  -0.305   0.7601    
log_nbus                 0.233968   0.566869   0.413   0.6798    
log_drug_crim           -0.025191   0.154568  -0.163   0.8705    
comp_index              -0.014764   0.014506  -1.018   0.3088    
law_index                0.001184   0.012482   0.095   0.9244    
Log(theta)              -2.615687   0.123300 -21.214   <2e-16 ***

Zero-inflation model coefficients (binomial with logit link):
              Estimate Std. Error z value Pr(>|z|)  
(Intercept)   -2.57659    1.44855  -1.779   0.0753 .
log_pop        1.06187    1.07603   0.987   0.3237  
log_wpn_crim   2.20203    1.44528   1.524   0.1276  
log_bribe_vic -2.01239    1.63069  -1.234   0.2172  
log_nbus       2.02348    2.41482   0.838   0.4021  
log_drug_crim -0.18031    0.60695  -0.297   0.7664  
comp_index    -0.15353    0.08660  -1.773   0.0762 .
law_index      0.02522    0.04230   0.596   0.5510  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.0731 
Number of iterations in BFGS optimization: 83 
Log-likelihood: -1591 on 40 Df
```

```r
lrtest(zinb_pscl_simple_full, zinb_pscl_c_all_z_state)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | 1
Model 2: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  33 -1596.9                     
2  40 -1591.5  7 10.916     0.1423
```

```r
vuong(zinb_pscl_simple_full, zinb_pscl_c_all_z_state)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A   p-value
Raw                  -1.6878591 model2 > model1  0.045719
AIC-corrected         0.4768483 model1 > model2  0.316735
BIC-corrected         6.7417407 model1 > model2 7.825e-12
```

```r
zinb_pscl_c_state_l_z_all <- update(zinb_pscl_full, . ~ . - bribes - 
                             yearsquant - 
                             subsector -
                             size | . )

summary(zinb_pscl_c_state_l_z_all)
```

```

Call:
zeroinfl(formula = extortions ~ log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index | bribes + 
    yearsquant + subsector + size + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index, data = enve_model, 
    dist = "negbin")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-0.4381 -0.2489 -0.2254 -0.1978 16.5225 

Count model coefficients (negbin with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)    0.551800   0.176215   3.131  0.00174 ** 
log_pop       -0.014651   0.214098  -0.068  0.94544    
log_wpn_crim   0.238199   0.232327   1.025  0.30523    
log_bribe_vic -0.373073   0.357704  -1.043  0.29696    
log_nbus       0.367162   0.521175   0.704  0.48113    
log_drug_crim -0.015777   0.143856  -0.110  0.91267    
comp_index    -0.012323   0.014312  -0.861  0.38924    
law_index      0.003649   0.011224   0.325  0.74508    
Log(theta)    -1.559338   0.207327  -7.521 5.43e-14 ***

Zero-inflation model coefficients (binomial with logit link):
                          Estimate Std. Error z value Pr(>|z|)  
(Intercept)              7.669e-01  5.635e-01   1.361   0.1735  
bribes                   1.011e-01  8.638e-02   1.171   0.2418  
yearsquant(8,16]        -1.652e-01  2.704e-01  -0.611   0.5413  
yearsquant(16,25]        2.298e-01  2.653e-01   0.866   0.3865  
yearsquant(25,34]        2.245e-01  2.609e-01   0.861   0.3895  
yearsquant(34,43]        1.899e-01  2.729e-01   0.696   0.4866  
subsectorMining          4.988e-01  9.591e-01   0.520   0.6030  
subsectorConstruction   -4.011e-01  5.145e-01  -0.780   0.4357  
subsectorManufacturing  -4.424e-01  5.031e-01  -0.879   0.3792  
subsectorWholesale      -5.343e-01  6.033e-01  -0.886   0.3758  
subsectorTransport      -3.080e-02  5.873e-01  -0.052   0.9582  
subsectorMedia          -1.791e+01  2.223e+03  -0.008   0.9936  
subsectorFinance        -6.209e-01  8.495e-01  -0.731   0.4648  
subsectorReal estate     3.915e-01  8.637e-01   0.453   0.6503  
subsectorProf. services -1.283e+00  8.498e-01  -1.510   0.1310  
subsectorMaintenance    -4.952e-03  5.464e-01  -0.009   0.9928  
subsectorEducation      -1.537e-01  8.286e-01  -0.186   0.8528  
subsectorHealth         -2.551e-01  5.080e-01  -0.502   0.6156  
subsectorLeisure        -5.371e-01  7.803e-01  -0.688   0.4913  
subsectorHotelsRestBar  -1.497e-01  5.101e-01  -0.293   0.7692  
subsectorOther          -1.907e+01  4.500e+03  -0.004   0.9966  
sizeMedium               3.618e-01  2.480e-01   1.459   0.1447  
sizeSmall                1.241e-01  2.464e-01   0.504   0.6146  
sizeMicro                1.006e-01  2.436e-01   0.413   0.6796  
log_pop                 -5.072e-02  2.163e-01  -0.234   0.8146  
log_wpn_crim             2.214e-01  2.204e-01   1.004   0.3152  
log_bribe_vic           -3.453e-01  3.405e-01  -1.014   0.3105  
log_nbus                 1.590e-01  5.310e-01   0.299   0.7646  
log_drug_crim           -1.358e-02  1.457e-01  -0.093   0.9258  
comp_index              -2.404e-02  1.374e-02  -1.749   0.0802 .
law_index                5.536e-03  1.093e-02   0.506   0.6125  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.2103 
Number of iterations in BFGS optimization: 63 
Log-likelihood: -1583 on 40 Df
```

```r
lrtest(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + log_pop + 
    log_wpn_crim + log_bribe_vic + log_nbus + log_drug_crim + 
    comp_index + law_index | 1
Model 2: extortions ~ log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index | bribes + yearsquant + 
    subsector + size + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  33 -1596.9                         
2  40 -1583.5  7 26.885  0.0003495 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
vuong(zinb_pscl_simple_full, zinb_pscl_c_state_l_z_all)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A  p-value
Raw                  -1.6109495 model2 > model1 0.053595
AIC-corrected        -0.7720795 model2 > model1 0.220034
BIC-corrected         1.6556991 model1 > model2 0.048891
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

===========================================================================================================================================================
                                      ZINB Full                Count: Null              Zero: Null              Zero: state          Count: state          
-----------------------------------------------------------------------------------------------------------------------------------------------------------
Count model: (Intercept)                  1.64     (0.58) **       0.50     (0.16) **      -0.13    (0.54)          0.11 (0.55)          0.55    (0.18) ** 
Count model: bribes                       0.17     (0.13)                                  -0.02    (0.10)          0.02 (0.10)                            
Count model: yearsquant(8,16]             0.19     (0.29)                                   0.15    (0.30)          0.16 (0.30)                            
Count model: yearsquant(16,25]            0.10     (0.28)                                  -0.09    (0.28)         -0.09 (0.28)                            
Count model: yearsquant(25,34]            0.77     (0.31) *                                 0.42    (0.29)          0.46 (0.28)                            
Count model: yearsquant(34,43]            0.37     (0.31)                                   0.18    (0.30)          0.17 (0.30)                            
Count model: subsectorMining             -1.57     (1.15)                                  -1.45    (0.98)         -1.34 (0.98)                            
Count model: subsectorConstruction       -0.37     (0.59)                                   0.03    (0.54)         -0.03 (0.54)                            
Count model: subsectorManufacturing      -0.89     (0.57)                                  -0.23    (0.52)         -0.30 (0.52)                            
Count model: subsectorWholesale          -1.01     (0.69)                                  -0.53    (0.65)         -0.42 (0.65)                            
Count model: subsectorTransport          -0.76     (0.66)                                  -0.53    (0.61)         -0.58 (0.61)                            
Count model: subsectorMedia              -2.19     (0.63) ***                              -0.28    (0.81)         -0.35 (0.79)                            
Count model: subsectorFinance            -0.42     (0.86)                                   0.09    (0.87)          0.01 (0.85)                            
Count model: subsectorReal estate        -2.05     (1.02) *                                -1.20    (0.88)         -1.29 (0.87)                            
Count model: subsectorProf. services      0.43     (0.75)                                   1.14    (0.83)          1.11 (0.84)                            
Count model: subsectorMaintenance        -0.92     (0.64)                                  -0.57    (0.58)         -0.58 (0.59)                            
Count model: subsectorEducation           0.03     (0.96)                                   0.30    (0.88)          0.26 (0.88)                            
Count model: subsectorHealth             -0.96     (0.58)                                  -0.48    (0.53)         -0.49 (0.53)                            
Count model: subsectorLeisure             0.28     (0.82)                                   0.39    (0.84)          0.59 (0.84)                            
Count model: subsectorHotelsRestBar      -0.64     (0.58)                                  -0.40    (0.54)         -0.41 (0.54)                            
Count model: subsectorOther              -0.95     (0.79)                                   0.65    (1.41)          0.70 (1.34)                            
Count model: sizeMedium                  -0.04     (0.25)                                  -0.24    (0.26)         -0.25 (0.26)                            
Count model: sizeSmall                   -0.47     (0.26)                                  -0.48    (0.26)         -0.47 (0.26)                            
Count model: sizeMicro                   -0.92     (0.25) ***                              -0.63    (0.26) *       -0.67 (0.26) *                          
Count model: log_pop                      0.11     (0.20)                                   0.15    (0.20)          0.15 (0.22)         -0.01    (0.21)    
Count model: log_wpn_crim                 0.29     (0.23)                                  -0.01    (0.21)          0.19 (0.23)          0.24    (0.23)    
Count model: log_bribe_vic               -0.17     (0.33)                                  -0.19    (0.31)         -0.11 (0.36)         -0.37    (0.36)    
Count model: log_nbus                     0.11     (0.51)                                   0.35    (0.51)          0.23 (0.57)          0.37    (0.52)    
Count model: log_drug_crim               -0.14     (0.15)                                   0.01    (0.14)         -0.03 (0.15)         -0.02    (0.14)    
Count model: comp_index                  -0.02     (0.01)                                   0.00    (0.01)         -0.01 (0.01)         -0.01    (0.01)    
Count model: law_index                    0.01     (0.01)                                  -0.00    (0.01)          0.00 (0.01)          0.00    (0.01)    
Count model: Log(theta)                  -0.81     (0.23) ***     -1.57     (0.20) ***     -2.80    (0.07) ***     -2.62 (0.12) ***     -1.56    (0.21) ***
Zero model: (Intercept)                   1.54     (0.50) **       0.74     (0.56)        -14.96 (2818.90)         -2.58 (1.45)          0.77    (0.56)    
Zero model: bribes                        0.14     (0.08)          0.10     (0.09)                                                       0.10    (0.09)    
Zero model: yearsquant(8,16]             -0.06     (0.27)         -0.18     (0.27)                                                      -0.17    (0.27)    
Zero model: yearsquant(16,25]             0.25     (0.27)          0.22     (0.26)                                                       0.23    (0.27)    
Zero model: yearsquant(25,34]             0.51     (0.26)          0.21     (0.26)                                                       0.22    (0.26)    
Zero model: yearsquant(34,43]             0.31     (0.27)          0.17     (0.27)                                                       0.19    (0.27)    
Zero model: subsectorMining              -0.13     (1.06)          0.52     (0.96)                                                       0.50    (0.96)    
Zero model: subsectorConstruction        -0.41     (0.47)         -0.36     (0.51)                                                      -0.40    (0.51)    
Zero model: subsectorManufacturing       -0.67     (0.47)         -0.40     (0.50)                                                      -0.44    (0.50)    
Zero model: subsectorWholesale           -0.81     (0.58)         -0.52     (0.60)                                                      -0.53    (0.60)    
Zero model: subsectorTransport           -0.27     (0.56)         -0.02     (0.59)                                                      -0.03    (0.59)    
Zero model: subsectorMedia              -19.29  (2251.12)        -23.71 (38180.16)                                                     -17.91 (2223.26)    
Zero model: subsectorFinance             -0.60     (0.75)         -0.58     (0.83)                                                      -0.62    (0.85)    
Zero model: subsectorReal estate         -0.74     (1.19)          0.40     (0.86)                                                       0.39    (0.86)    
Zero model: subsectorProf. services      -0.83     (0.64)         -1.30     (0.88)                                                      -1.28    (0.85)    
Zero model: subsectorMaintenance         -0.32     (0.53)         -0.01     (0.55)                                                      -0.00    (0.55)    
Zero model: subsectorEducation           -0.12     (0.74)         -0.16     (0.83)                                                      -0.15    (0.83)    
Zero model: subsectorHealth              -0.56     (0.48)         -0.25     (0.51)                                                      -0.26    (0.51)    
Zero model: subsectorLeisure             -0.39     (0.67)         -0.59     (0.79)                                                      -0.54    (0.78)    
Zero model: subsectorHotelsRestBar       -0.33     (0.47)         -0.15     (0.51)                                                      -0.15    (0.51)    
Zero model: subsectorOther              -22.08 (17693.61)        -17.84  (2283.97)                                                     -19.07 (4499.76)    
Zero model: sizeMedium                    0.28     (0.22)          0.36     (0.25)                                                       0.36    (0.25)    
Zero model: sizeSmall                    -0.07     (0.23)          0.12     (0.25)                                                       0.12    (0.25)    
Zero model: sizeMicro                    -0.33     (0.25)          0.11     (0.24)                                                       0.10    (0.24)    
Zero model: log_pop                       0.01     (0.18)         -0.06     (0.18)                                  1.06 (1.08)         -0.05    (0.22)    
Zero model: log_wpn_crim                  0.22     (0.19)          0.10     (0.18)                                  2.20 (1.45)          0.22    (0.22)    
Zero model: log_bribe_vic                -0.19     (0.27)         -0.15     (0.28)                                 -2.01 (1.63)         -0.35    (0.34)    
Zero model: log_nbus                      0.01     (0.45)         -0.03     (0.46)                                  2.02 (2.41)          0.16    (0.53)    
Zero model: log_drug_crim                -0.08     (0.13)         -0.00     (0.13)                                 -0.18 (0.61)         -0.01    (0.15)    
Zero model: comp_index                   -0.02     (0.01) *       -0.02     (0.01)                                 -0.15 (0.09)         -0.02    (0.01)    
Zero model: law_index                     0.01     (0.01)          0.00     (0.01)                                  0.03 (0.04)          0.01    (0.01)    
-----------------------------------------------------------------------------------------------------------------------------------------------------------
AIC                                    3237.28                  3238.49                  3259.84                 3262.93              3246.96              
Log Likelihood                        -1555.64                 -1586.24                 -1596.92                -1591.46             -1583.48              
Num. obs.                              2412                     2412                     2412                    2412                 2412                 
===========================================================================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
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
binary_formula <- update(formula(mnb_final), extortion_victim ~ .)

# Full, equal variable, hurdle models

## Binary part first

mlogit_full <- glmmadmb(binary_formula, data = enve_model, 
                        family = "binomial")

summary(mlogit_full)
```

```

Call:
glmmadmb(formula = binary_formula, data = enve_model, family = "binomial")

AIC: 1846.4 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.063647   0.412850   -5.00  5.8e-07 ***
bribes                  -0.076429   0.071065   -1.08  0.28216    
yearsquant(8,16]         0.211578   0.189160    1.12  0.26335    
yearsquant(16,25]       -0.091222   0.193460   -0.47  0.63726    
yearsquant(25,34]       -0.130677   0.194230   -0.67  0.50108    
yearsquant(34,43]       -0.094047   0.198950   -0.47  0.63641    
subsectorMining         -0.426944   0.824270   -0.52  0.60448    
subsectorConstruction    0.275851   0.406300    0.68  0.49718    
subsectorManufacturing   0.297657   0.397360    0.75  0.45380    
subsectorWholesale       0.399030   0.461510    0.86  0.38725    
subsectorTransport       0.031678   0.469370    0.07  0.94619    
subsectorMedia           1.949729   0.481220    4.05  5.1e-05 ***
subsectorFinance         0.411272   0.611480    0.67  0.50121    
subsectorReal estate    -0.350246   0.709890   -0.49  0.62174    
subsectorProf. services  0.910412   0.548430    1.66  0.09691 .  
subsectorMaintenance     0.024418   0.439190    0.06  0.95566    
subsectorEducation       0.094362   0.649770    0.15  0.88453    
subsectorHealth          0.203938   0.405070    0.50  0.61464    
subsectorLeisure         0.470966   0.581490    0.81  0.41798    
subsectorHotelsRestBar   0.130574   0.409230    0.32  0.74967    
subsectorOther           2.940026   0.788470    3.73  0.00019 ***
sizeMedium              -0.271750   0.180010   -1.51  0.13114    
sizeSmall               -0.122287   0.176840   -0.69  0.48924    
sizeMicro               -0.086857   0.172870   -0.50  0.61536    
log_pop                  0.036072   0.129950    0.28  0.78133    
log_wpn_crim            -0.073851   0.126460   -0.58  0.55923    
log_bribe_vic            0.104648   0.199310    0.53  0.59955    
log_nbus                -0.025739   0.338070   -0.08  0.93931    
log_drug_crim           -0.000967   0.093804   -0.01  0.99177    
comp_index               0.012680   0.008190    1.55  0.12153    
law_index               -0.003756   0.006676   -0.56  0.57374    
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
(Intercept) 1.008e-07 0.0003175


Log-likelihood: -891.18 
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
[1] -891.18

$df
[1] 32

$AIC
[1] 1846.36

$BIC
[1] 2031.583

$alpha
numeric(0)

$var_j
[1] 1.015862e-14

$ICC
numeric(0)
```

```r
## Count part

mtnb_full <- glmmadmb(formula(mnb_final), data = enve_trunc,
                      family = "truncnbinom")
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
glmmadmb(formula = formula(mnb_final), data = enve_trunc, family = "truncnbinom")

AIC: 1371.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              1.24054    0.80908    1.53   0.1252    
bribes                   0.16114    0.15205    1.06   0.2892    
yearsquant(8,16]         0.08348    0.34840    0.24   0.8106    
yearsquant(16,25]       -0.04118    0.34496   -0.12   0.9050    
yearsquant(25,34]        0.73870    0.36765    2.01   0.0445 *  
yearsquant(34,43]        0.36100    0.37604    0.96   0.3370    
subsectorMining         -1.63240    1.40450   -1.16   0.2451    
subsectorConstruction   -0.40366    0.71572   -0.56   0.5728    
subsectorManufacturing  -0.93155    0.69547   -1.34   0.1804    
subsectorWholesale      -1.09973    0.85280   -1.29   0.1972    
subsectorTransport      -0.79543    0.80045   -0.99   0.3204    
subsectorMedia          -4.20615    0.95735   -4.39  1.1e-05 ***
subsectorFinance        -0.46490    1.04560   -0.44   0.6566    
subsectorReal estate    -1.69775    1.24630   -1.36   0.1731    
subsectorProf. services  0.28295    0.92879    0.30   0.7606    
subsectorMaintenance    -1.08100    0.78409   -1.38   0.1680    
subsectorEducation       0.14352    1.17560    0.12   0.9028    
subsectorHealth         -1.01710    0.70545   -1.44   0.1494    
subsectorLeisure         0.15122    0.99395    0.15   0.8791    
subsectorHotelsRestBar  -0.79107    0.71505   -1.11   0.2686    
subsectorOther          -2.15965    1.02410   -2.11   0.0350 *  
sizeMedium               0.00414    0.31324    0.01   0.9895    
sizeSmall               -0.46520    0.31605   -1.47   0.1410    
sizeMicro               -0.90886    0.31135   -2.92   0.0035 ** 
log_pop                  0.13336    0.23928    0.56   0.5773    
log_wpn_crim             0.26577    0.27534    0.97   0.3344    
log_bribe_vic           -0.27293    0.40211   -0.68   0.4973    
log_nbus                 0.37293    0.60758    0.61   0.5393    
log_drug_crim           -0.11246    0.17668   -0.64   0.5244    
comp_index              -0.01873    0.01580   -1.19   0.2357    
law_index                0.00693    0.01392    0.50   0.6183    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=309, state=32 
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

Negative binomial dispersion parameter: 0.18761 (std. err.: 0.11196)

Log-likelihood: -652.663 
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
[1] -652.663

$df
[1] 33

$AIC
[1] 1371.326

$BIC
[1] 1494.526

$alpha
[1] 5.330206

$var_j
[1] 4.248958e-18

$ICC
[1] 7.97147e-19
```

```r
### Compare joint IC with Full MNB and ZINB

compare_hurdle2other(mlogit_full, mtnb_full, mnb_final, mzinb_full)
```

```
          Hurdle   Negbin.      ZINB
logLik -1543.843 -1596.920 -1596.920
df        65.000    33.000    34.000
AIC     3217.686  3259.840  3261.840
BIC     3526.109  3450.851  3458.639
```

```r
screenreg(list("MNB" = mnb_final,
               "MZINB" = mzinb_full,
               "Logit" = mlogit_full, 
               "Trunc. NB" = mtnb_full), 
          single.row = TRUE)
```

```

=======================================================================================================
                           MNB                MZINB              Logit               Trunc. NB         
-------------------------------------------------------------------------------------------------------
(Intercept)                   -0.13 (0.54)       -0.13 (0.54)      -2.06 (0.41) ***     1.24 (0.81)    
bribes                        -0.02 (0.10)       -0.02 (0.10)      -0.08 (0.07)         0.16 (0.15)    
yearsquant(8,16]               0.15 (0.30)        0.15 (0.30)       0.21 (0.19)         0.08 (0.35)    
yearsquant(16,25]             -0.09 (0.28)       -0.09 (0.28)      -0.09 (0.19)        -0.04 (0.34)    
yearsquant(25,34]              0.42 (0.29)        0.42 (0.29)      -0.13 (0.19)         0.74 (0.37) *  
yearsquant(34,43]              0.18 (0.30)        0.18 (0.30)      -0.09 (0.20)         0.36 (0.38)    
subsectorMining               -1.45 (0.98)       -1.45 (0.98)      -0.43 (0.82)        -1.63 (1.40)    
subsectorConstruction          0.03 (0.54)        0.03 (0.54)       0.28 (0.41)        -0.40 (0.72)    
subsectorManufacturing        -0.23 (0.52)       -0.23 (0.52)       0.30 (0.40)        -0.93 (0.70)    
subsectorWholesale            -0.53 (0.65)       -0.53 (0.65)       0.40 (0.46)        -1.10 (0.85)    
subsectorTransport            -0.53 (0.61)       -0.53 (0.61)       0.03 (0.47)        -0.80 (0.80)    
subsectorMedia                -0.28 (0.81)       -0.28 (0.81)       1.95 (0.48) ***    -4.21 (0.96) ***
subsectorFinance               0.09 (0.87)        0.09 (0.87)       0.41 (0.61)        -0.46 (1.05)    
subsectorReal estate          -1.20 (0.88)       -1.20 (0.88)      -0.35 (0.71)        -1.70 (1.25)    
subsectorProf. services        1.14 (0.83)        1.14 (0.83)       0.91 (0.55)         0.28 (0.93)    
subsectorMaintenance          -0.57 (0.58)       -0.57 (0.58)       0.02 (0.44)        -1.08 (0.78)    
subsectorEducation             0.30 (0.88)        0.30 (0.88)       0.09 (0.65)         0.14 (1.18)    
subsectorHealth               -0.48 (0.53)       -0.48 (0.53)       0.20 (0.41)        -1.02 (0.71)    
subsectorLeisure               0.39 (0.84)        0.39 (0.84)       0.47 (0.58)         0.15 (0.99)    
subsectorHotelsRestBar        -0.40 (0.54)       -0.40 (0.54)       0.13 (0.41)        -0.79 (0.72)    
subsectorOther                 0.65 (1.41)        0.65 (1.41)       2.94 (0.79) ***    -2.16 (1.02) *  
sizeMedium                    -0.24 (0.26)       -0.24 (0.26)      -0.27 (0.18)         0.00 (0.31)    
sizeSmall                     -0.48 (0.26)       -0.48 (0.26)      -0.12 (0.18)        -0.47 (0.32)    
sizeMicro                     -0.63 (0.26) *     -0.63 (0.26) *    -0.09 (0.17)        -0.91 (0.31) ** 
log_pop                        0.15 (0.20)        0.15 (0.20)       0.04 (0.13)         0.13 (0.24)    
log_wpn_crim                  -0.01 (0.21)       -0.01 (0.21)      -0.07 (0.13)         0.27 (0.28)    
log_bribe_vic                 -0.19 (0.31)       -0.19 (0.31)       0.10 (0.20)        -0.27 (0.40)    
log_nbus                       0.35 (0.51)        0.35 (0.51)      -0.03 (0.34)         0.37 (0.61)    
log_drug_crim                  0.01 (0.14)        0.01 (0.14)      -0.00 (0.09)        -0.11 (0.18)    
comp_index                     0.00 (0.01)        0.00 (0.01)       0.01 (0.01)        -0.02 (0.02)    
law_index                     -0.00 (0.01)       -0.00 (0.01)      -0.00 (0.01)         0.01 (0.01)    
-------------------------------------------------------------------------------------------------------
Variance: state                0.00               0.00              0.00                0.00           
Dispersion: parameter          0.06               0.06                                  0.19           
Dispersion: SD                 0.00               0.00                                  0.11           
AIC                         3259.84            3261.84           1846.36             1371.33           
BIC                         3450.85            3458.64           2031.58             1494.53           
Log Likelihood             -1596.92           -1596.92           -891.18             -652.66           
Num. obs.                   2412               2412              2412                 309              
Num. groups: state            32                 32                32                  32              
Zero inflation: parameter                         0.00                                                 
Zero inflation: SD                                0.00                                                 
=======================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
# Full, single level, equal equation hurdle

## Binary part first

logit_full <- update(mlogit_full, . ~ . - (1 | state))

summary(logit_full)
```

```

Call:
glmmadmb(formula = extortion_victim ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index, data = enve_model, 
    family = "binomial")

AIC: 1844.4 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.063711   0.412850   -5.00  5.8e-07 ***
bribes                  -0.076443   0.071067   -1.08  0.28209    
yearsquant(8,16]         0.211609   0.189160    1.12  0.26328    
yearsquant(16,25]       -0.091205   0.193460   -0.47  0.63733    
yearsquant(25,34]       -0.130652   0.194230   -0.67  0.50116    
yearsquant(34,43]       -0.094032   0.198950   -0.47  0.63647    
subsectorMining         -0.426637   0.824190   -0.52  0.60471    
subsectorConstruction    0.275864   0.406310    0.68  0.49717    
subsectorManufacturing   0.297673   0.397370    0.75  0.45379    
subsectorWholesale       0.399088   0.461520    0.86  0.38719    
subsectorTransport       0.031753   0.469370    0.07  0.94606    
subsectorMedia           1.949788   0.481220    4.05  5.1e-05 ***
subsectorFinance         0.411329   0.611480    0.67  0.50115    
subsectorReal estate    -0.350335   0.709920   -0.49  0.62167    
subsectorProf. services  0.910431   0.548430    1.66  0.09690 .  
subsectorMaintenance     0.024469   0.439190    0.06  0.95557    
subsectorEducation       0.094419   0.649770    0.15  0.88446    
subsectorHealth          0.203989   0.405070    0.50  0.61455    
subsectorLeisure         0.470949   0.581500    0.81  0.41801    
subsectorHotelsRestBar   0.130623   0.409230    0.32  0.74958    
subsectorOther           2.940211   0.788490    3.73  0.00019 ***
sizeMedium              -0.271724   0.180010   -1.51  0.13117    
sizeSmall               -0.122270   0.176840   -0.69  0.48930    
sizeMicro               -0.086840   0.172870   -0.50  0.61543    
log_pop                  0.036065   0.129950    0.28  0.78137    
log_wpn_crim            -0.073838   0.126450   -0.58  0.55927    
log_bribe_vic            0.104665   0.199310    0.53  0.59949    
log_nbus                -0.025784   0.338070   -0.08  0.93920    
log_drug_crim           -0.000979   0.093803   -0.01  0.99167    
comp_index               0.012679   0.008190    1.55  0.12157    
law_index               -0.003755   0.006676   -0.56  0.57381    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=2412

Log-likelihood: -891.18 
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
[1] -891.18

$df
[1] 31

$AIC
[1] 1844.36

$BIC
[1] 2023.795

$alpha
numeric(0)
```

```r
## Count part

tnb_full <- update(mtnb_full, . ~ . - (1 | state))
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
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index, data = enve_trunc, 
    family = "truncnbinom")

AIC: 1369.3 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              1.24054    0.80908    1.53   0.1252    
bribes                   0.16114    0.15205    1.06   0.2892    
yearsquant(8,16]         0.08348    0.34840    0.24   0.8106    
yearsquant(16,25]       -0.04118    0.34496   -0.12   0.9050    
yearsquant(25,34]        0.73870    0.36765    2.01   0.0445 *  
yearsquant(34,43]        0.36100    0.37604    0.96   0.3370    
subsectorMining         -1.63240    1.40450   -1.16   0.2451    
subsectorConstruction   -0.40366    0.71572   -0.56   0.5728    
subsectorManufacturing  -0.93155    0.69547   -1.34   0.1804    
subsectorWholesale      -1.09973    0.85280   -1.29   0.1972    
subsectorTransport      -0.79543    0.80045   -0.99   0.3204    
subsectorMedia          -4.20615    0.95735   -4.39  1.1e-05 ***
subsectorFinance        -0.46490    1.04560   -0.44   0.6566    
subsectorReal estate    -1.69775    1.24630   -1.36   0.1731    
subsectorProf. services  0.28295    0.92879    0.30   0.7606    
subsectorMaintenance    -1.08100    0.78409   -1.38   0.1680    
subsectorEducation       0.14352    1.17560    0.12   0.9028    
subsectorHealth         -1.01710    0.70545   -1.44   0.1494    
subsectorLeisure         0.15122    0.99395    0.15   0.8791    
subsectorHotelsRestBar  -0.79107    0.71505   -1.11   0.2686    
subsectorOther          -2.15965    1.02410   -2.11   0.0350 *  
sizeMedium               0.00414    0.31324    0.01   0.9895    
sizeSmall               -0.46520    0.31605   -1.47   0.1410    
sizeMicro               -0.90886    0.31135   -2.92   0.0035 ** 
log_pop                  0.13336    0.23928    0.56   0.5773    
log_wpn_crim             0.26577    0.27534    0.97   0.3344    
log_bribe_vic           -0.27293    0.40211   -0.68   0.4973    
log_nbus                 0.37293    0.60758    0.61   0.5393    
log_drug_crim           -0.11246    0.17668   -0.64   0.5244    
comp_index              -0.01873    0.01580   -1.19   0.2357    
law_index                0.00693    0.01392    0.50   0.6183    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=309
Negative binomial dispersion parameter: 0.18731 (std. err.: 0.11176)

Log-likelihood: -652.663 
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
[1] -652.663

$df
[1] 32

$AIC
[1] 1369.326

$BIC
[1] 1488.793

$alpha
[1] 5.338743
```

```r
### Compare joint IC with Full NB and ZINB

compare_hurdle2other(logit_full, tnb_full, nb_final, zinb_full)
```

```
          Hurdle   Negbin.      ZINB
logLik -1543.843 -1596.920 -1596.920
df        63.000    32.000    33.000
AIC     3213.686  3257.840  3259.840
BIC     3512.587  3443.063  3450.851
```

```r
screenreg(list("NB" = nb_final,
               "ZINB" = zinb_full,
               "Logit" = logit_full, 
               "Trunc. NB" = tnb_full), 
          single.row = TRUE)
```

```

=======================================================================================================
                           NB                 ZINB               Logit               Trunc. NB         
-------------------------------------------------------------------------------------------------------
(Intercept)                   -0.13 (0.54)       -0.13 (0.54)      -2.06 (0.41) ***     1.24 (0.81)    
bribes                        -0.02 (0.10)       -0.02 (0.10)      -0.08 (0.07)         0.16 (0.15)    
yearsquant(8,16]               0.15 (0.30)        0.15 (0.30)       0.21 (0.19)         0.08 (0.35)    
yearsquant(16,25]             -0.09 (0.28)       -0.09 (0.28)      -0.09 (0.19)        -0.04 (0.34)    
yearsquant(25,34]              0.42 (0.29)        0.42 (0.29)      -0.13 (0.19)         0.74 (0.37) *  
yearsquant(34,43]              0.18 (0.30)        0.18 (0.30)      -0.09 (0.20)         0.36 (0.38)    
subsectorMining               -1.45 (0.98)       -1.45 (0.98)      -0.43 (0.82)        -1.63 (1.40)    
subsectorConstruction          0.03 (0.54)        0.03 (0.54)       0.28 (0.41)        -0.40 (0.72)    
subsectorManufacturing        -0.23 (0.52)       -0.23 (0.52)       0.30 (0.40)        -0.93 (0.70)    
subsectorWholesale            -0.53 (0.65)       -0.53 (0.65)       0.40 (0.46)        -1.10 (0.85)    
subsectorTransport            -0.53 (0.61)       -0.53 (0.61)       0.03 (0.47)        -0.80 (0.80)    
subsectorMedia                -0.28 (0.81)       -0.28 (0.81)       1.95 (0.48) ***    -4.21 (0.96) ***
subsectorFinance               0.09 (0.87)        0.09 (0.87)       0.41 (0.61)        -0.46 (1.05)    
subsectorReal estate          -1.20 (0.88)       -1.20 (0.88)      -0.35 (0.71)        -1.70 (1.25)    
subsectorProf. services        1.14 (0.83)        1.14 (0.83)       0.91 (0.55)         0.28 (0.93)    
subsectorMaintenance          -0.57 (0.58)       -0.57 (0.58)       0.02 (0.44)        -1.08 (0.78)    
subsectorEducation             0.30 (0.88)        0.30 (0.88)       0.09 (0.65)         0.14 (1.18)    
subsectorHealth               -0.48 (0.53)       -0.48 (0.53)       0.20 (0.41)        -1.02 (0.71)    
subsectorLeisure               0.39 (0.84)        0.39 (0.84)       0.47 (0.58)         0.15 (0.99)    
subsectorHotelsRestBar        -0.40 (0.54)       -0.40 (0.54)       0.13 (0.41)        -0.79 (0.72)    
subsectorOther                 0.65 (1.41)        0.65 (1.41)       2.94 (0.79) ***    -2.16 (1.02) *  
sizeMedium                    -0.24 (0.26)       -0.24 (0.26)      -0.27 (0.18)         0.00 (0.31)    
sizeSmall                     -0.48 (0.26)       -0.48 (0.26)      -0.12 (0.18)        -0.47 (0.32)    
sizeMicro                     -0.63 (0.26) *     -0.63 (0.26) *    -0.09 (0.17)        -0.91 (0.31) ** 
log_pop                        0.15 (0.20)        0.15 (0.20)       0.04 (0.13)         0.13 (0.24)    
log_wpn_crim                  -0.01 (0.21)       -0.01 (0.21)      -0.07 (0.13)         0.27 (0.28)    
log_bribe_vic                 -0.19 (0.31)       -0.19 (0.31)       0.10 (0.20)        -0.27 (0.40)    
log_nbus                       0.35 (0.51)        0.35 (0.51)      -0.03 (0.34)         0.37 (0.61)    
log_drug_crim                  0.01 (0.14)        0.01 (0.14)      -0.00 (0.09)        -0.11 (0.18)    
comp_index                     0.00 (0.01)        0.00 (0.01)       0.01 (0.01)        -0.02 (0.02)    
law_index                     -0.00 (0.01)       -0.00 (0.01)      -0.00 (0.01)         0.01 (0.01)    
-------------------------------------------------------------------------------------------------------
Dispersion: parameter          0.06               0.06                                  0.19           
Dispersion: SD                 0.00               0.00                                  0.11           
AIC                         3257.84            3259.84           1844.36             1369.33           
BIC                         3443.06            3450.85           2023.79             1488.79           
Log Likelihood             -1596.92           -1596.92           -891.18             -652.66           
Num. obs.                   2412               2412              2412                 309              
Num. groups:                   1                  1                 1                   1              
Zero inflation: parameter                         0.00                                                 
Zero inflation: SD                                0.00                                                 
=======================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
## Compare two hurdle modles: multilevel vs single level

hurdle_compare(joint_ic(logit_full, tnib_full), 
               joint_ic(mlogit_full, mtnb_full),
               modnames = c("Single level", "Multilevel"))
```

```
Error in joint_ic(logit_full, tnib_full): object 'tnib_full' not found
```

```r
# - Binary: state; Count: all vs. Full

mlogit_state <- glmmadmb(formula(mnb_only_state), data = enve_model,
                         family = "binomial")
summary(mlogit_state)
```

```

Call:
glmmadmb(formula = formula(mnb_only_state), data = enve_model, 
    family = "binomial")

AIC: 992.1 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -2.92876    0.11678  -25.08   <2e-16 ***
log_pop       -0.10281    0.19243   -0.53     0.59    
log_wpn_crim  -0.13641    0.18418   -0.74     0.46    
log_drug_crim  0.08413    0.13868    0.61     0.54    
log_bribe_vic  0.21223    0.29381    0.72     0.47    
log_nbus      -0.06556    0.50260   -0.13     0.90    
comp_index     0.00967    0.01212    0.80     0.42    
law_index     -0.00654    0.00976   -0.67     0.50    
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
(Intercept) 6.205e-08 0.0002491


Log-likelihood: -487.056 
```

```r
get_glmmadmb(mlogit_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -487.056

$df
[1] 9

$AIC
[1] 992.112

$BIC
[1] 1044.206

$alpha
numeric(0)

$var_j
[1] 3.850078e-15

$ICC
numeric(0)
```

```r
lrtest(mlogit_state, mlogit_full)
```

```
Likelihood ratio test

Model 1: extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index
Model 2: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1   9 -487.06                         
2  32 -891.18 23 808.25  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# - Binary: bus; Count: all vs. Full

mlogit_bus <- glmmadmb(formula(mnb1), data = enve_model,
                         family = "binomial")
summary(mlogit_bus)
```

```

Call:
glmmadmb(formula = formula(mnb1), data = enve_model, family = "binomial")

AIC: 955.4 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -3.6096     0.7617   -4.74  2.1e-06 ***
bribes                   -0.0605     0.1102   -0.55   0.5830    
yearsquant(8,16]          0.2295     0.2833    0.81   0.4178    
yearsquant(16,25]         0.0135     0.2903    0.05   0.9630    
yearsquant(25,34]        -0.1613     0.3014   -0.54   0.5926    
yearsquant(34,43]        -0.2038     0.3104   -0.66   0.5114    
subsectorMining           0.3135     1.2465    0.25   0.8014    
subsectorConstruction     0.5798     0.7640    0.76   0.4479    
subsectorManufacturing    0.5973     0.7509    0.80   0.4264    
subsectorWholesale        1.2373     0.7978    1.55   0.1209    
subsectorTransport        0.9419     0.8056    1.17   0.2423    
subsectorMedia            3.2268     0.7840    4.12  3.9e-05 ***
subsectorFinance         -9.8081   144.6600   -0.07   0.9459    
subsectorReal estate      0.0201     1.2430    0.02   0.9871    
subsectorProf. services   0.0608     1.2430    0.05   0.9610    
subsectorMaintenance      0.7005     0.7875    0.89   0.3737    
subsectorEducation        0.1705     1.2438    0.14   0.8910    
subsectorHealth           0.6672     0.7570    0.88   0.3781    
subsectorLeisure          0.7712     1.0220    0.75   0.4505    
subsectorHotelsRestBar    0.5374     0.7646    0.70   0.4821    
subsectorOther            2.7208     0.9975    2.73   0.0064 ** 
sizeMedium               -0.5658     0.3019   -1.87   0.0609 .  
sizeSmall                 0.1141     0.2628    0.43   0.6640    
sizeMicro                 0.1108     0.2586    0.43   0.6684    
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
(Intercept) 7.131e-08 0.000267


Log-likelihood: -452.724 
```

```r
get_glmmadmb(mlogit_bus)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -452.724

$df
[1] 25

$AIC
[1] 955.448

$BIC
[1] 1100.153

$alpha
numeric(0)

$var_j
[1] 5.085116e-15

$ICC
numeric(0)
```

```r
lrtest(mlogit_bus, mlogit_full)
```

```
Likelihood ratio test

Model 1: extortions ~ bribes + yearsquant + subsector + size + (1 | state)
Model 2: extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  25 -452.72                         
2  32 -891.18  7 876.91  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# - Binary: all; Count: bus vs. Full

mtnb_bus <- glmmadmb(formula(mnb1), data = enve_trunc,
                      family = "truncnbinom")
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
glmmadmb(formula = formula(mnb1), data = enve_trunc, family = "truncnbinom")

AIC: 1362.9 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              1.07851    0.89783    1.20   0.2297    
bribes                   0.13082    0.15020    0.87   0.3838    
yearsquant(8,16]         0.11695    0.35403    0.33   0.7411    
yearsquant(16,25]       -0.02766    0.34455   -0.08   0.9360    
yearsquant(25,34]        0.68160    0.36848    1.85   0.0643 .  
yearsquant(34,43]        0.35758    0.37477    0.95   0.3400    
subsectorMining         -1.68100    1.42480   -1.18   0.2381    
subsectorConstruction   -0.29301    0.72860   -0.40   0.6876    
subsectorManufacturing  -0.83780    0.71307   -1.17   0.2400    
subsectorWholesale      -1.47172    0.84762   -1.74   0.0825 .  
subsectorTransport      -0.93099    0.82153   -1.13   0.2571    
subsectorMedia          -4.24779    0.96577   -4.40  1.1e-05 ***
subsectorFinance        -0.60708    1.06810   -0.57   0.5698    
subsectorReal estate    -1.82642    1.23320   -1.48   0.1386    
subsectorProf. services  0.34952    0.94616    0.37   0.7118    
subsectorMaintenance    -1.14010    0.79216   -1.44   0.1501    
subsectorEducation      -0.06513    1.15550   -0.06   0.9550    
subsectorHealth         -1.10635    0.73213   -1.51   0.1308    
subsectorLeisure        -0.31700    0.99607   -0.32   0.7503    
subsectorHotelsRestBar  -0.81098    0.73407   -1.10   0.2693    
subsectorOther          -2.46136    1.03660   -2.37   0.0176 *  
sizeMedium               0.00958    0.32629    0.03   0.9766    
sizeSmall               -0.51562    0.31814   -1.62   0.1051    
sizeMicro               -0.91711    0.31520   -2.91   0.0036 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=309, state=32 
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

Negative binomial dispersion parameter: 0.14772 (std. err.: 0.10617)

Log-likelihood: -655.448 
```

```r
get_glmmadmb(mtnb_bus)
```

```
$model
extortions ~ bribes + yearsquant + subsector + size + (1 | state)

$logLik
[1] -655.448

$df
[1] 26

$AIC
[1] 1362.896

$BIC
[1] 1459.963

$alpha
[1] 6.769564

$var_j
[1] 4.249782e-18

$ICC
[1] 6.277778e-19
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
1  26 -655.45                    
2  33 -652.66  7  5.57     0.5908
```

```r
# - Binary: all; Count: state vs. Full

mtnb_state <- glmmadmb(formula(mnb_only_state), data = enve_trunc,
                      family = "truncnbinom")
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
glmmadmb(formula = formula(mnb_only_state), data = enve_trunc, 
    family = "truncnbinom")

AIC: 1386.5 

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -2.57432    0.14902  -17.27   <2e-16 ***
log_pop       -0.01656    0.25300   -0.07     0.95    
log_wpn_crim   0.29841    0.27866    1.07     0.28    
log_drug_crim -0.04118    0.17649   -0.23     0.82    
log_bribe_vic -0.45499    0.42893   -1.06     0.29    
log_nbus       0.43438    0.62255    0.70     0.49    
comp_index    -0.01542    0.01691   -0.91     0.36    
law_index      0.00647    0.01321    0.49     0.62    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=309, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.047e-08 0.0001431

Negative binomial dispersion parameter: 0.0067379 (std. err.: 1.798e-05)

Log-likelihood: -683.252 
```

```r
get_glmmadmb(mtnb_state)
```

```
$model
extortions ~ (1 | state) + log_pop + log_wpn_crim + log_drug_crim + 
    log_bribe_vic + log_nbus + comp_index + law_index

$logLik
[1] -683.252

$df
[1] 10

$AIC
[1] 1386.504

$BIC
[1] 1423.837

$alpha
[1] 148.4142

$var_j
[1] 4.191028e-16

$ICC
[1] 2.823873e-18
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
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1  10 -683.25                         
2  33 -652.66 23 61.178  2.577e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Binary with deaths

mlogit_deaths <- update(mlogit_full, . ~ . + log_deaths)
summary(mlogit_deaths)
```

```

Call:
glmmadmb(formula = extortion_victim ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index + log_deaths, 
    data = enve_model, family = "binomial")

AIC: 1848.2 

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -2.072761   0.413690   -5.01  5.4e-07 ***
bribes                  -0.076874   0.071027   -1.08  0.27911    
yearsquant(8,16]         0.211062   0.189180    1.12  0.26457    
yearsquant(16,25]       -0.090321   0.193470   -0.47  0.64061    
yearsquant(25,34]       -0.129219   0.194280   -0.67  0.50598    
yearsquant(34,43]       -0.092745   0.198970   -0.47  0.64113    
subsectorMining         -0.422638   0.824390   -0.51  0.60818    
subsectorConstruction    0.275827   0.406310    0.68  0.49723    
subsectorManufacturing   0.299678   0.397420    0.75  0.45081    
subsectorWholesale       0.401774   0.461600    0.87  0.38408    
subsectorTransport       0.032369   0.469390    0.07  0.94502    
subsectorMedia           1.953765   0.481390    4.06  4.9e-05 ***
subsectorFinance         0.413211   0.611540    0.68  0.49924    
subsectorReal estate    -0.353725   0.709970   -0.50  0.61833    
subsectorProf. services  0.909688   0.548500    1.66  0.09722 .  
subsectorMaintenance     0.025650   0.439220    0.06  0.95343    
subsectorEducation       0.094496   0.649830    0.15  0.88438    
subsectorHealth          0.203760   0.405080    0.50  0.61496    
subsectorLeisure         0.471306   0.581450    0.81  0.41761    
subsectorHotelsRestBar   0.131575   0.409260    0.32  0.74784    
subsectorOther           2.952473   0.789230    3.74  0.00018 ***
sizeMedium              -0.271870   0.180010   -1.51  0.13097    
sizeSmall               -0.122067   0.176860   -0.69  0.49007    
sizeMicro               -0.086155   0.172870   -0.50  0.61821    
log_pop                  0.055449   0.140760    0.39  0.69364    
log_wpn_crim            -0.037011   0.162210   -0.23  0.81952    
log_bribe_vic            0.099043   0.199650    0.50  0.61983    
log_nbus                -0.023699   0.338620   -0.07  0.94420    
log_drug_crim           -0.000532   0.093572   -0.01  0.99546    
comp_index               0.011750   0.008580    1.37  0.17086    
law_index               -0.004521   0.007000   -0.65  0.51842    
log_deaths              -0.051913   0.143680   -0.36  0.71787    
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
(Intercept) 5.71e-08 0.000239


Log-likelihood: -891.115 
```

```r
get_glmmadmb(mlogit_deaths)
```

```
$model
extortion_victim ~ bribes + yearsquant + subsector + size + (1 | 
    state) + log_pop + log_wpn_crim + log_bribe_vic + log_nbus + 
    log_drug_crim + comp_index + law_index + log_deaths

$logLik
[1] -891.115

$df
[1] 33

$AIC
[1] 1848.23

$BIC
[1] 2039.241

$alpha
numeric(0)

$var_j
[1] 3.26041e-15

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
    log_drug_crim + comp_index + law_index + log_deaths
  #Df  LogLik Df Chisq Pr(>Chisq)
1  32 -891.18                    
2  33 -891.12  1  0.13     0.7184
```

```r
# count with deaths

mtnb_deaths <- update(mtnb_full, . ~ . + log_deaths)
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
glmmadmb(formula = extortions ~ bribes + yearsquant + subsector + 
    size + (1 | state) + log_pop + log_wpn_crim + log_bribe_vic + 
    log_nbus + log_drug_crim + comp_index + law_index + log_deaths, 
    data = enve_trunc, family = "truncnbinom")

AIC: 1371.4 

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)              1.40903    0.79182    1.78   0.0752 .  
bribes                   0.18502    0.15134    1.22   0.2215    
yearsquant(8,16]         0.03823    0.34604    0.11   0.9120    
yearsquant(16,25]       -0.06082    0.34098   -0.18   0.8584    
yearsquant(25,34]        0.61795    0.37491    1.65   0.0993 .  
yearsquant(34,43]        0.36400    0.37548    0.97   0.3323    
subsectorMining         -1.37475    1.42340   -0.97   0.3341    
subsectorConstruction   -0.41199    0.71144   -0.58   0.5625    
subsectorManufacturing  -1.03563    0.69752   -1.48   0.1376    
subsectorWholesale      -1.02553    0.86103   -1.19   0.2336    
subsectorTransport      -0.93904    0.80180   -1.17   0.2415    
subsectorMedia          -4.19070    0.95111   -4.41  1.1e-05 ***
subsectorFinance        -0.45673    1.04620   -0.44   0.6624    
subsectorReal estate    -1.49681    1.23730   -1.21   0.2264    
subsectorProf. services  0.18076    0.91697    0.20   0.8437    
subsectorMaintenance    -1.20180    0.78348   -1.53   0.1250    
subsectorEducation       0.00605    1.16460    0.01   0.9959    
subsectorHealth         -1.03053    0.70298   -1.47   0.1427    
subsectorLeisure         0.25351    0.99039    0.26   0.7980    
subsectorHotelsRestBar  -0.83699    0.71227   -1.18   0.2400    
subsectorOther          -2.23485    1.01670   -2.20   0.0279 *  
sizeMedium              -0.03135    0.30999   -0.10   0.9194    
sizeSmall               -0.45812    0.31416   -1.46   0.1448    
sizeMicro               -0.93967    0.30668   -3.06   0.0022 ** 
log_pop                  0.00670    0.25422    0.03   0.9790    
log_wpn_crim            -0.03729    0.34391   -0.11   0.9137    
log_bribe_vic           -0.26215    0.38844   -0.67   0.4998    
log_nbus                 0.37535    0.60909    0.62   0.5377    
log_drug_crim           -0.10794    0.17141   -0.63   0.5289    
comp_index              -0.01307    0.01608   -0.81   0.4161    
law_index                0.01055    0.01372    0.77   0.4421    
log_deaths               0.36932    0.26623    1.39   0.1654    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number of observations: total=309, state=32 
Random effect variance(s):
```

```
Warning in .local(x, sigma, ...): 'sigma' and 'rdig' arguments are present
for compatibility only: ignored
```

```
Group=state
             Variance    StdDev
(Intercept) 2.075e-09 4.555e-05

Negative binomial dispersion parameter: 0.20032 (std. err.: 0.11421)

Log-likelihood: -651.715 
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
[1] -651.715

$df
[1] 34

$AIC
[1] 1371.43

$BIC
[1] 1498.364

$alpha
[1] 4.992013

$var_j
[1] 4.30604e-18

$ICC
[1] 8.625859e-19
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
1  33 -652.66                    
2  34 -651.72  1 1.896     0.1685
```

```r
# together with deaths

hurdle_compare(joint_ic(mlogit_full, mtnb_full), 
               joint_ic(mlogit_deaths, mtnb_deaths),
               modnames = c("Full", "+ deaths"))
```

```
       Full      + deaths
logLik -1543.843 -1542.83
df     65        67      
AIC    3217.686  3219.66 
BIC    3526.109  3537.605
```

```r
compare_hurdle2other(mlogit_deaths, mtnb_deaths, mnb_f_deaths)
```

```
          Hurdle   Negbin.
logLik -1542.830 -1596.150
df        67.000    34.000
AIC     3219.660  3260.300
BIC     3537.605  3457.099
```

# Before sending for real

- Remove package loading from here
- Remove subsector fake data added for testing only



```r
endtime <- proc.time()

endtime - starttime
```

```
    user   system  elapsed 
7209.405  575.218 7378.592 
```
