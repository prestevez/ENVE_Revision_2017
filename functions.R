my.chisq.test <- function(anftable, ...)
{
    # A function to run a chi-square test on a contingency table
    # It tests if there is a warning with the standard test
    # if there is, it performs the test under simulation instead
    # calculates Cramers V too.
    
    tc <- tryCatch(chisq.test(anftable), warning = function(x) x)
    
    simul <- FALSE
    
    if(is(tc, "warning"))
    {
        simul <- TRUE
    } 
    
    test <- chisq.test(anftable, simulate.p.value = simul, ...)
    
    CV <- sqrt(test$statistic / 
                   (sum(anftable) * 
                   (min(ncol(anftable), nrow(anftable)) - 1)))
    
    CV <- as.numeric(CV)
    
    return(list(chisqtest = test, CV=CV))
    
}


format.ftables.tst <- function(ftables_list)
{
    # Takes a list of chisq tests from the many.ftables.tst() function
    # and sumarises the results as a dataframe
    
    require(dplyr)
    
    results.matrix <- matrix(ncol=4, nrow = length(ftables_list))
    rownames(results.matrix) <- names(ftables_list)
    colnames(results.matrix) <- c("Chisq", "df", "p.value", "CV")
    
    for(i in 1:length(ftables_list))
    {
        results.matrix[i,1] <- ftables_list[[i]]$chisqtest$statistic
        results.matrix[i,2] <- ftables_list[[i]]$chisqtest$parameter
        results.matrix[i,3] <- ftables_list[[i]]$chisqtest$p.value
        results.matrix[i,4] <- ftables_list[[i]]$CV
    }
    
    results.df <- as.data.frame(results.matrix)
    
    results.df %>%
        mutate(s=make_stars(pval=p.value)) -> results.df
    rownames(results.df) <- names(ftables_list)
    
    return(results.df)
}

make_stars <- function(pval=pval)
{
    # prints significance stars for every p-vlaue of numeric vector pval
    ifelse(pval < .001, "***", ifelse(pval < .01, "**", ifelse(pval < .05, "*", "")))
}


print_freqtst <- function(formated_freqtest_df, 
                          result = c("none", "markdown", "latex", "html", "pandoc"), 
                          digits = 3,
                          scientific = TRUE,
                          DV)
{
    # prints pretty table from format.ftables.tst()
    require(knitr)
    
    legend <- ": Chi-square tests of association. df = NA indicates simulated p-value."
    
    cap <- paste(DV, legend, sep = "")
    
    if (result[1] %in% c("html", "markdown", "latex", "pandoc")) 
    {
        return(kable(formated_freqtest_df, 
                     format = result[1], 
                     caption = cap, 
                     row.names = TRUE,
                     digits = digits))
    }
    else
    {
        return(format(formated_freqtest_df, digits = digits, scientific = scientific))
    }
}


many.ftables.tst <- function(df, DV, IV, 
                             print.special = c("none", "markdown", "latex", "html", "pandoc"),
                             ...)
{
    # takes a data frame, a DV and a vector of 
    # at least one IV, and generates a series of ftables
    # stops if the class of the IV or the DV is not factor
    
    if(class(df[,DV]) != "factor") stop("ERROR: DV is not a factor")
    
    results <- list()
    
    for(i in 1:length(IV))
    {
        if(class(df[,IV[i]]) != "factor") stop("ERROR: IV is not a factor")
        
        freqtable <- ftable(df[,IV[i]],  df[,DV])
        
        names(attr(freqtable, which = "col.vars")) <- DV
        names(attr(freqtable, which = "row.vars")) <- IV[i]
        
        chsqtst <- my.chisq.test(freqtable, ...)
        
        results[[IV[i]]] <- chsqtst
        
    }
    
    results.df <- format.ftables.tst(results)
    
    final <- print_freqtst(results.df, result = print.special[1], DV = DV)
    
    return(final)
    
}
