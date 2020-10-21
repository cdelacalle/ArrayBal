library(shiny)
library(shinyWidgets)
library(ggplot2)

score <- function(df_reorder, df2) {
  wscores = cor3(df_reorder,df2)
  for (i in 1:length(input$percentage_select_var)) {
    wscores[input$percentage_select_var[i],] = wscores[input$percentage_select_var[i],]*(eval(parse(text = paste0("input$percentage_select_var", i))))
    wscores[,1] = wscores[,1]*(eval(parse(text = "input$slider_array")))
    wscores[,2] = wscores[,2]*(eval(parse(text = "input$slider_slide")))
  }
    return(mean(abs(wscores)))
}

cor3 = function(df1, df2) {
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
      r <- stats::cor(df1[[pos_df1]]
                      , df2[[pos_df2]]
                      , use = "pairwise.complete.obs")
    }
    
    # one is numeric and other is a factor/character
    if (class(df1[[pos_df1]]) %in% c("integer", "numeric") &&
        class(df2[[pos_df2]]) %in% c("factor", "character")) {
      r <- sqrt(summary(stats::lm(df1[[pos_df1]] ~ as.factor(df2[[pos_df2]])))[["r.squared"]])
    }
    
    if (class(df2[[pos_df2]]) %in% c("integer", "numeric") &&
        class(df1[[pos_df1]]) %in% c("factor", "character")) {
      r <- sqrt(summary(stats::lm(df2[[pos_df2]] ~ as.factor(df1[[pos_df1]])))[["r.squared"]])
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
  corrmat <- outer(seq_len(ncol(df1))
                   , seq_len(ncol(df2))
                   , function(x, y)
                     cor_fun(x, y))
  
  rownames(corrmat) <- colnames(df1)
  colnames(corrmat) <- colnames(df2)
  
  return(corrmat)
}
