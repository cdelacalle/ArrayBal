# library dependencies

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(ggpubr)
library(broom)
library(plotly)
library(bit64)
library(GGally)
library(reshape2)
library(ggforce)

# user defined functions
                                        
cor3 <- function(df1, df2) {
  
  #function based on cor2 function of https://gist.github.com/talegari
  #This function handles the comparison of numeric variables (PCs)
  #with numeric or factor variables.
  
  stopifnot(inherits(df1, "data.frame"))
  stopifnot(
    vapply(df1, class, FUN.VALUE = character(1)) %in%
      c("integer"
        , "numeric"
        , "factor"
        , "character")
  )
  
  stopifnot(inherits(df2, "data.frame"))
  stopifnot(
    vapply(df2, class, FUN.VALUE = character(1)) %in%
      c("integer"
        , "numeric"
        , "factor"
        , "character")
  )
  
  cor_fun <- function(pos_df1, pos_df2) {
    # both are numeric
    if (class(df1[[pos_df1]]) %in% c("integer", "numeric") &&
        class(df2[[pos_df2]]) %in% c("integer", "numeric")) {
      res <- stats::cor.test(df1[[pos_df1]], 
                             df2[[pos_df2]], 
                             use = "pairwise.complete.obs")
      r <- res[['estimate']]
    }
    
    # one is numeric and other is a factor/character
    if (class(df1[[pos_df1]]) %in% c("integer", "numeric") &&
        class(df2[[pos_df2]]) %in% c("factor", "character")) {
      res <- stats::lm(df1[[pos_df1]] ~ as.factor(df2[[pos_df2]]))
      r <- sqrt(summary(res)[["r.squared"]])
    }
    
    if (class(df2[[pos_df2]]) %in% c("integer", "numeric") &&
        class(df1[[pos_df1]]) %in% c("factor", "character")) {
      res <- stats::lm(df2[[pos_df2]] ~ as.factor(df1[[pos_df1]]))
      r <- sqrt(summary(res)[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df1[[pos_df1]]) %in% c("factor", "character") &&
       class(df2[[pos_df2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df1[[pos_df1]], df2[[pos_df2]], simulate.p.value = TRUE)
    }
    return(r)
  }
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat_r <- outer(seq_len(ncol(df1))
                     , seq_len(ncol(df2))
                     , function(x, y) (cor_fun(x, y)))
  
  rownames(corrmat_r) <- colnames(df1)
  colnames(corrmat_r) <- colnames(df2)
 
  return(corrmat_r)
} # variable correlation function with p
cor3p <- function(df1, df2) {
  
  #function based on cor2 function of https://gist.github.com/talegari
  #This function handles the comparison of numeric variables (PCs)
  #with numeric or factor variables.
  
  stopifnot(inherits(df1, "data.frame"))
  stopifnot(
    vapply(df1, class, FUN.VALUE = character(1)) %in%
      c("integer"
        , "numeric"
        , "factor"
        , "character")
  )
  
  stopifnot(inherits(df2, "data.frame"))
  stopifnot(
    vapply(df2, class, FUN.VALUE = character(1)) %in%
      c("integer"
        , "numeric"
        , "factor"
        , "character")
  )
  
  cor_fun <- function(pos_df1, pos_df2) {
    # both are numeric
    if (class(df1[[pos_df1]]) %in% c("integer", "numeric") &&
        class(df2[[pos_df2]]) %in% c("integer", "numeric")) {
      res <- stats::cor.test(df1[[pos_df1]], 
                             df2[[pos_df2]], 
                             use = "pairwise.complete.obs")
      p <- res[['p.value']]
    }
    
    # one is numeric and other is a factor/character
    if (class(df1[[pos_df1]]) %in% c("integer", "numeric") &&
        class(df2[[pos_df2]]) %in% c("factor", "character")) {
      res <- stats::lm(df1[[pos_df1]] ~ as.factor(df2[[pos_df2]]))
      p <- broom::glance(res)$p.value
    }
    
    if (class(df2[[pos_df2]]) %in% c("integer", "numeric") &&
        class(df1[[pos_df1]]) %in% c("factor", "character")) {
      res <- stats::lm(df2[[pos_df2]] ~ as.factor(df1[[pos_df1]]))
      p <- glance(res)$p.value
    }
    
    # both are factor/character
    if(class(df1[[pos_df1]]) %in% c("factor", "character") &&
       class(df2[[pos_df2]]) %in% c("factor", "character")){
      p <- chisq.test(df1[[pos_df1]], df2[[pos_df2]], simulate.p.value = TRUE)[['p.value']] %>% as.numeric()
    }
    
    return(p)
  }
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  
  corrmat_p <- outer(seq_len(ncol(df1))
                     , seq_len(ncol(df2))
                     , function(x, y)
                       cor_fun(x, y))
  
  rownames(corrmat_p) <- colnames(df1)
  colnames(corrmat_p) <- colnames(df2)
  return(corrmat_p)
  
} # variable correlation function with p
cor3f <- function(df1, df2, namevar) {
  
  all <- list()
  df1 <- as.data.frame(df1); rownames(df1) <- NULL 
  df1 <- df1 %>% tibble::column_to_rownames(namevar)
  df2 <- as.data.frame(df2); rownames(df2) <- NULL 
  df2 <- df2 %>% tibble::column_to_rownames(namevar)
  
  cors <- cor3(df1, df2)
  ps <- cor3p(df1, df2)
  
  if (df1 == df2) {
  cors[upper.tri(cors)] <- NA
  ps[upper.tri(ps)] <- NA
  }
  
  cors <- cors %>%
    reshape2::melt() %>%
    dplyr::rename(coefficient = value) %>%
    dplyr::mutate(abs_coef = abs(coefficient))
  
  ps <- ps %>%
    reshape2::melt() %>%
    dplyr::rename(p = value) %>%
    dplyr::mutate(p_signif = signif(p, 2),
                  p_round = round(p, 1), 
                  log_pval = -log(p_signif),
                  log_pval = ifelse(log_pval == Inf, min((reshape2::melt(ps))$value %>% 
                                                           tibble() %>% dplyr::filter(.!=0), na.rm = TRUE), p_signif),
                  log_pval = -log(p_signif)) %>%
    dplyr::select(p, p_round, p_signif, log_pval)
  
  varinfo <- data.frame(
    Variable = colnames(df1),
    Type = unlist(lapply(df1, class)))
  
  all[[1]] <- cbind(cors, ps) %>%
    dplyr::arrange(Var2, Var1) %>%
    left_join(varinfo %>% dplyr::rename(Var1 = Variable, Type1 = Type), by = 'Var1') %>%
    left_join(varinfo %>% dplyr::rename(Var2 = Variable, Type2 = Type), by = 'Var2') %>%
    dplyr::mutate(varinfo = ifelse(Var2 == 'array', 'numeric', 'factor'))
  all[[2]] <- all[[1]] %>%
    dplyr::select(Var1:coefficient, p, Type1, Type2) %>%
    dplyr::filter(!Var1==Var2) %>%
    dplyr::rename(Corr = 3, Pval = 4) %>% 
    dplyr::mutate(Coefficient = 
                    ifelse(Type1 %in% c("integer", "numeric") & 
                             Type2 %in% c("integer", "numeric"), "Pearson's R",
                           ifelse(Type1 %in% c("integer", "numeric") & 
                                    Type2 %in% c("factor", "character"), 'R Squared',
                                  ifelse(Type1 %in% c("factor", "character") & 
                                           Type2 %in% c("integer", "numeric"), 'R Squared',
                                         ifelse(Type1 %in% c("factor", "character") & 
                                                  Type2 %in% c("factor", "character"), "Cramér's V", 'nada')))),
                  Corr = round(Corr, 3),
                  Pval = signif(Pval, 2)) %>%
    na.omit()
  
  return(all)
  
} # variable correlation messages
