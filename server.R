source("functions.R")

server <- function(input, output, session) {
  
  score <- function(df_reorder, df2) {
    wscores = cor3(df_reorder,df2)
      if (input$array_or_seq == 'Microarray') {
        for (i in 1:length(input$percentage_select_var)) {
      wscores[input$percentage_select_var[i],] = 
        wscores[input$percentage_select_var[i],]*(eval(parse(text = paste0("input$percentage_select_var", i))))
      wscores[,1] = wscores[,1]*(eval(parse(text = "input$slider_array")))
      wscores[,2] = wscores[,2]*(eval(parse(text = "input$slider_slide")))
    }}
      if (input$array_or_seq == 'NGS') {
        for (i in 1:length(input$percentage_select_var)) {
        wscores[input$percentage_select_var[i],] = 
          wscores[input$percentage_select_var[i],]*(eval(parse(text = paste0("input$percentage_select_var", i))))
      }}
    return(mean(abs(wscores)))
  } # score function
  
  # Load button only shows if file is uploaded
  
  output$ui_button_input_load = renderUI({
    if (!is.null(input$fileinput_input$datapath))
      return(actionButton("button_input_load", "Load Data"))
    else
      return()
  })
  
  # Enable load button every time input file is updated
  
  observeEvent(input$fileinput_input, shinyjs::enable("button_input_load"))
  
  # Read pdata sheet from csv
  
  rval_sheet = eventReactive(input$button_input_load, {
    
    # Check if updated file is .csv
    print(tools::file_ext(input$fileinput_input$datapath))
    shiny::validate(need(tools::file_ext(input$fileinput_input$datapath) == "csv", "File extension should be .csv"))
    shinyjs::disable("button_input_load") # disable the load button to avoid multiple clicks
    sheet = data.table::fread(input$fileinput_input$datapath, data.table = F)
    
    # We check if pdata format is correct
    # This prevents app crashes when updated zip is not correct
    
    shiny::validate(
      need(
        anyDuplicated(colnames(sheet)) == 0,
        "Repeated variable names are not allowed. Please, modify your sample sheet."
      )
    )
    colnames(sheet) = make.names(colnames(sheet)) # fix possible not-valid colnames
    sheet
  })
  
  # Filter pdata sheet based on selected samples
  
  rval_sheet_filt = eventReactive(input$button_input_next, 
                                    rval_sheet()[rval_sheet()[, input$select_input_samplenamevar] %in% input$selected_samples,])
  
  observeEvent(input$button_input_load, {
    updateSelectInput(
      session,
      "select_input_samplenamevar",
      label = "Select sample names:",
      choices = colnames(rval_sheet())
      )
    observeEvent(input$select_input_samplenamevar, {
      updateSelectInput(
        session,
        "select_input_responsevar",
        label = "Select dependent variable:",
        choices = colnames(rval_sheet())[colnames(rval_sheet()) != input$select_input_samplenamevar]
        )
      observeEvent(input$select_input_responsevar, {
        shinyWidgets::updatePickerInput(
          session,
          "select_input_covars",
          label = "Select covariates of interest:",
          choices = colnames(rval_sheet())[!colnames(rval_sheet()) %in% 
                                             c(input$select_input_samplenamevar, input$select_input_responsevar)]
          )
        shinyjs::enable("button_input_next") #Enable button continue
        })
      })
  })
    
  
  # The checkbox of samples to process is updated when samplenamevar changes
  
  observeEvent({
    input$select_input_samplenamevar
    input$select_input_responsevar
  },
  shinyWidgets::updatePickerInput(
    session,
    "selected_samples",
    label = "Select samples to process:",
    selected = rval_sheet()[,input$select_input_samplenamevar],
    choices = rval_sheet()[,input$select_input_samplenamevar],
    choicesOpt = list(subtext = paste0("(", rval_sheet()[,input$select_input_responsevar], ')'))
  ))
  
  # When samples selected are changed, continue button is enabled again
  
  observeEvent(input$selected_samples, shinyjs::enable("button_input_next"))
  
  # Update label of Continue button after pushing continuebutton and selecting variables or samples
  
  value <- reactiveValues(value = FALSE)
  
  observeEvent({input$button_input_next}, 
    { value$value <- TRUE
    })
  
  observeEvent({input$selected_samples}, 
    { if (value$value == TRUE) updateActionButton(session, "button_input_next", label = "Reload")
  })
  
  # The dataframe is rendered
  
  output$samples_table =  DT::renderDT(
    rval_sheet(),
    rownames = FALSE,
    selection = "single",
    style = "bootstrap",
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,
      columnDefs = list(list(
        targets = match("Basename",colnames(rval_sheet())) - 1, visible = FALSE
      ))
    )
  )
  
  # Change tab with Continue button
  
  observeEvent(input$button_input_next, {
    updateTabsetPanel(session, "tabset1",
                      selected = "cor_tab")})
  
  # Correlations amongs Variables of the pdata
  
  output$corr_plot <- renderUI({
    validate(need(expr = length(unique(rval_sheet_filt()[,input$select_input_samplenamevar])) == 
                    length(rval_sheet_filt()[,input$select_input_samplenamevar]), 
                  message = 'Select sample names with unique IDs'))

    corr_variables <- (cor3f(rval_sheet_filt(), rval_sheet_filt(), input$select_input_samplenamevar))[[1]]
    corr_variables_na <- na.omit(corr_variables)
   
    output = tagList(
    renderUI({
      cor_text <- c()
      for (i in 1:nrow(corr_variables_na)) {
        if (corr_variables_na[i,]$Var1 != corr_variables_na[i,]$Var2) {
          if (corr_variables_na[i,]$Var1 == input$select_input_responsevar | 
              corr_variables_na[i,]$Var2 == input$select_input_responsevar) {
          if (corr_variables_na[i,]$p < 0.05 & corr_variables_na[i,]$p > 0.01) {
            cor_text[i] <- 
              paste0('Variables <b>', corr_variables_na[i,]$Var1, '</b> and <b>', corr_variables_na[i,]$Var2, 
                     '</b> are <font color=\"#FFD604\"><b> significantly </b></font> correlated!',
                   " (R = ", round(corr_variables_na[i,]$coefficient, 2), ', p = ', corr_variables_na[i,]$p_signif, ')')
          } else if (corr_variables_na[i,]$p < 0.01 & corr_variables_na[i,]$p > 0.001) {
            cor_text[i] <- 
              paste0('Variables <b>', corr_variables_na[i,]$Var1, '</b> and <b>', corr_variables_na[i,]$Var2, 
                     '</b> are <font color=\"#FF5804\"><b> strongly </b></font> correlated!',
                       " (R = ", round(corr_variables_na[i,]$coefficient, 2), ', p = ', corr_variables_na[i,]$p_signif, ')')
          } else if (corr_variables_na[i,]$p < 0.001) {
            cor_text[i] <- 
              paste0('Variables <b>', corr_variables_na[i,]$Var1, '</b> and <b>', corr_variables_na[i,]$Var2, 
                     '</b> are <font color=\"#FF0404\"><b> extremely </b></font> correlated!',
                   " (R = ", round(corr_variables_na[i,]$coefficient, 2), ', p = ', corr_variables_na[i,]$p_signif, ')')
          }}}
      }
      cor_text <- na.omit(cor_text)
      HTML(paste0(cor_text, '<br>'))
    }), 
    renderPlotly(
      ggplotly(
        ggscatter(corr_variables, 'Var2', 'Var1', size = 'abs_coef', legend = 'right', 
                  xlab = '', ylab = '', color = 'coefficient') +
          scale_x_discrete(limits = unique(corr_variables$Var1)) +
          scale_y_discrete(limits = unique(corr_variables$Var1)) +
          geom_text(data = corr_variables %>% dplyr::filter(abs_coef < 0.7), 
                    aes_string(x = 'Var2', y = 'Var1', label = 'p_round'), color = 'black', size = 3) +
          geom_text(data = corr_variables %>% dplyr::filter(abs_coef > 0.7), 
                    aes_string(x = 'Var2', y = 'Var1', label = 'p_round'), color = 'white', size = 4) +
          scale_color_gradientn(colors = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "white",
                                           "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"), limits = c(-1,1)) +
          theme_bw() +
          rotate_x_text(angle = 45), tooltip = 'coefficient') %>% config(displayModeBar = F)
    ),
    DT::renderDT(
      (cor3f(rval_sheet_filt(), rval_sheet_filt(), input$select_input_samplenamevar))[[2]],
      extensions = "Buttons",
      rownames = FALSE,
      style = "bootstrap",
      selection = "single",
      options = list(
        dom = '<"top"l>rt<"bottom"Bip><"clear">',
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        buttons = list(list(extend='csv',
                            filename = 'var_correlations'),
                       list(extend='excel',
                            filename = 'var_correlations'),
                       list(extend='pdf', title = 'Variable Correlations and P-Values',
                            filename= 'var_correlations'))
        )))
    output
  })
  
  # Correlations amongs selected variables and plots
  
  output$cor_stat_plot <- renderPlot({
    vars <- unique(c(input$select_input_responsevar, input$select_input_covars))
    validate(need(length(vars) > 1, 'Select at least 1 covariate of interest!'))
        if (input$coloring_variable == 'none') {
          GGally::ggpairs(rval_sheet_filt()[,vars], cardinality_threshold = 100) + 
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.text.y = element_text(angle = 30, vjust = 0.5))
          } else {
            GGally::ggpairs(rval_sheet_filt()[,vars], 
                            ggplot2::aes_string(colour=input$coloring_variable, cardinality_threshold = 100)) + 
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.text.y = element_text(angle = 30, vjust = 0.5))
      }
  })
  
  # Change the coloring variable in the ggpairs plot

  output$variable_color <- renderUI({
    vars <- c(input$select_input_responsevar, input$select_input_covars)
    selectInput('coloring_variable', "Pick coloring variable", choices = c('none', vars), selected = NULL)
  })
  
  # Display different options (correlation amongs variables)
  
  output$diff_outputs <- renderUI({
    if (is.null(input$cor_numeric))
      return()
    switch(
      input$cor_numeric,
      "Correlation tests" = uiOutput(outputId = 'corr_plot') %>% shinycssloaders::withSpinner(color = 'red', type = 6),
      "Correlation plot" = plotOutput(outputId = 'cor_stat_plot') %>% shinycssloaders::withSpinner(color = 'red', type = 6))
  })
  
  # Create variable sliders based on selected response and covariates
  
  output$vars_percentage <- renderUI({
    validate(need(length(input$select_input_responsevar) > 0, 'Select a dependent variable!'))
    if (length(input$select_input_covars) > 0) {
    vars <- unique(c(input$select_input_responsevar, input$select_input_covars))
    } else {
      vars <- input$select_input_responsevar
    } 
  
        l <- list()
        l[[1]] <- sliderInput(paste0("percentage_select_var", 1), 
                              paste0(vars[1], ' (dependent var.)'), 
                              min = 0, max = 1, value = 0)
      

    if (length(vars) > 1) {
      for (i in 2:length(vars)) {
        l[[i]] <- sliderInput(paste0("percentage_select_var", i), 
                              vars[i], 
                              min = 0, max = 1, value = 0)
      }}
    return(tagList(l))
  })
  
  # Weighting of variables (should equal to 1)
  
  observeEvent(
    lapply(paste0("input$percentage_select_var", 
                  seq_len(length(unique(c(input$select_input_responsevar, input$select_input_covars))))), 
                      function(x) {eval(parse(text = x))}), {
    ind_val = unlist(lapply(paste0("input$percentage_select_var", 
                                   seq_len(length(unique(c(input$select_input_responsevar, input$select_input_covars))))),
                            function(x) {eval(parse(text = x)) 
                              }))
    suma = sum(ind_val)
    resto = abs(1 - suma) / (length(unique(c(input$select_input_responsevar, input$select_input_covars)))-1)
    print(paste0('suma = ', suma))
    print(paste0('resto = ', resto))
    
    # last_trigger_number = as.numeric(gsub(".*?([0-9]+).*", "\\1", input$changed)
    # resto = (suma - 1) / ( length(input$select_input_interestvar) - last_trigger_number)
    # ind_check = ind_val >= rest
    # if(suma > 1){
    #  if(last_trigger_number+1 <= length(input$select_input_interestvar)) {
    #   for(z in seq(last_trigger_number+1, length(input$select_input_interestvar))){
    #     print(eval(parse(text=paste0("input$percentage_select_var", z))) - resto)
    #     updateSliderInput(session,paste0("percentage_select_var", z),
    #                       value = eval(parse(text=paste0("input$percentage_select_var", z))) - resto)}
    
    if (suma > 1) {
      for (z in 1:length(unique(c(input$select_input_responsevar, input$select_input_covars)))) {
        print(eval(parse(text = paste0("input$percentage_select_var", z))) - resto)
        updateSliderInput(session, paste0("percentage_select_var", z),
                          value = eval(parse(text = paste0("input$percentage_select_var", z))) - resto)
    }}
})
  
  # Weighting button options
  
  observeEvent(input$recommended, {
    vars <- unique(c(input$select_input_responsevar, input$select_input_covars))
    if (length(vars) > 1) {
    updateSliderInput(session, paste0("percentage_select_var", 1), value = 0.5)
    for (i in 2:length(vars)) {
      updateSliderInput(session, paste0("percentage_select_var", i), value = 0.5/(length(vars)-1))}}
    if (length(vars) == 1) {
      updateSliderInput(session, paste0("percentage_select_var", 1), value = 1)
    }})
  
  observeEvent(input$evenly, {
    vars <- unique(c(input$select_input_responsevar, input$select_input_covars))
    for (i in 1:length(vars)) {
      updateSliderInput(session, paste0("percentage_select_var", i), value = 1/length(vars))}})
  
  # Slide and Array Sliders should add to 1
  
  observeEvent(c(input$slider_slide, input$slider_array), {
    suma = input$slider_array + input$slider_slide 
    resto = abs(1-suma) / 2
    print(resto)
    if (suma > 1) {
      for (i in c("slider_slide", "slider_array"))
      updateSliderInput(session, inputId = i, value = eval(parse(text = paste0("input$", i))) - resto)
    }
  })
  
  # Generate plot with array design
  
  rval_array = eventReactive(eventExpr = input$button_plot_array, {
                               validate(need(input$array_nrow > 0, 'Minumum rows = 1'),
                                        need(input$array_ncol > 0, 'Minimum columns = 1'))
                               rows = input$array_nrow
                               cols = input$array_ncol
                               
                               if (cols == 1) {
                                 array_design = data.frame(rows = rep(rep(1:rows), cols), columns = rep(c(1:cols), each = rows),
                                                           label = paste0('Array ', 1:rows))
                               } else {
                                 array_design = data.frame(rows = rep(rep(1:rows), cols), columns = rep(c(1:cols), each = rows),
                                                           label = paste0('Array ', 1:rows, '.', rep(c(1:cols), each = rows)))
                               }
                               
                               ggplot2::ggplot(data = array_design) +
                                 geom_tile(aes(x = columns, y = rows, fill = label), color = 'black', size = 1) +
                                 geom_text(aes(x = columns, y = rows, label = label)) +
                                 scale_x_continuous(breaks = 1:cols) +
                                 ggplot2::ggtitle('Slide:') +
                                 scale_y_reverse(breaks = 1:rows) +
                                 xlab('') + ylab('') +
                                 theme(
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   plot.title = element_text(hjust = 0.5, vjust = -1, size = 20),
                                   panel.background = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank(), 
                                   legend.position = "none"
                                 )
                             })
  
  rval_ngs = eventReactive(input$button_plot_array,
                             {
                               validate(need(input$ngs_samplexrun > 0, 'Minumum samples per run = 1'))
                               rows = input$ngs_samplexrun
                               cols = 1
                            
                               ngs_design = data.frame(rows = rep(rep(1:rows), cols), columns = rep(c(1:cols), each = rows),
                                                           label = paste0('Sample ', 1:rows))
                               
                               ggplot2::ggplot(data = ngs_design) +
                                 geom_tile(aes(x = columns, y = rows, fill = label), color = 'black', size = 1, height=.8) +
                                 geom_text(aes(x = columns, y = rows, label = label)) +
                                 scale_x_continuous(breaks = 1:cols) +
                                 ggplot2::ggtitle('Run:') +
                                 scale_y_reverse(breaks = 1:rows) +
                                 xlab('') + ylab('') +
                                 theme(
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   plot.title = element_text(hjust = 0.5, vjust = -1, size = 20),
                                   panel.background = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank(), 
                                   legend.position = "none"
                                 )
                             })
  
  observeEvent(input$button_plot_array, {
    output$array_design <- renderUI({
    output = tagList(
      div(style="display: inline-block;vertical-align:top; margin-top: 5px;", 
          renderPlot(width = 200, height = 500, rval_array())),
      div(style="display: inline-block;vertical-align:top; width: 20px;",
          HTML("<br>")),
      div(style="display: inline-block;vertical-align:top; margin-top: 15px; margin-left: 15px; width: 230px;", 
          renderText('Weight balancing of Slide/Array:'),
          div(style="display: inline-block;vertical-align:top; height: 10px;", HTML("<br>")),
          sliderInput("slider_slide", "Slide", min = 0, max = 1, value = 0.7),
          sliderInput("slider_array", "Array", min = 0, max = 1, value = 0.3))
      )
    output
  })
  })
  
  output$ngs_design <- renderPlot(width = 200, height = 500, rval_ngs())
  
  # Change tab with Continue button 2
  
  observeEvent(input$button_array_next, {
    updateTabsetPanel(session, "navbar_shiny",
                      selected = "samp_bal_panel")})
  
  # Correlation reactive
  
  rval_cor = eventReactive(input$button_run, {
    validate(need(length(input$select_input_responsevar) > 0, 'Select a dependent variable!'))
    ind_val = unlist(lapply(paste0("input$percentage_select_var", 
                                   seq_len(length(unique(c(input$select_input_responsevar, input$select_input_covars))))),
                            function(x) {eval(parse(text = x)) 
                            }))
    suma = sum(ind_val)
    validate(need(suma > 0, 'Weight your variables!'))
    
    vars <- c(input$select_input_responsevar, input$select_input_covars)
    
    if (input$array_or_seq == 'Microarray') {
    n_samples = nrow(rval_sheet_filt())
    n_rows = input$array_nrow
    n_cols = input$array_ncol
    n_arrays = ceiling(n_samples/(n_rows*n_cols))
    
    if (n_cols == 1) {
      array_design = data.frame(array = as.factor(rep(rep(rep(1:n_rows), n_cols), n_arrays)[1:n_samples]), 
                                slide = as.factor(rep(c(1:n_arrays), each = n_rows*n_cols))[1:n_samples])
    } else {
      array_design = data.frame(array = as.factor(rep(rep(rep(1:n_rows), n_cols), n_arrays)[1:n_samples]), 
                                slide_cols = as.factor(rep(rep(c(1:n_cols), each = n_rows), n_arrays)[1:n_samples]), 
                                slide_rows = as.factor(rep(c(1:n_arrays), each = n_rows*n_cols))[1:n_samples])
    }
    
    df <- as.data.frame(rval_sheet_filt())
    rownames(df) <- NULL
    df0 = df %>% as.data.frame() %>%
      tibble::column_to_rownames(input$select_input_samplenamevar) 
    df1 = df0 %>% dplyr::select(vars) %>% as.data.frame()
    df2 = array_design

    points = Inf
    points_list = list()
    
    for(i in 1:input$select_permutations) {
      df_reorder = df1[sample(nrow(df1)),,drop=FALSE]
      points_list[[i]] = points
      if (score(df_reorder,df2) < points){
        points = score(df_reorder,df2)
        df_final = as.data.frame(df_reorder)
      }
      }
    
    df3 <- df0 %>% tibble::rownames_to_column(input$select_input_samplenamevar) %>% 
      dplyr::select(-vars) %>% as.data.frame()
    
    definitivo <- cbind(df_final, df2) %>% 
      tibble::rownames_to_column(input$select_input_samplenamevar) %>%
      dplyr::left_join(df3, by = input$select_input_samplenamevar) %>%
      tibble::column_to_rownames(input$select_input_samplenamevar)
    
    results_rval_cor <- list()
    results_rval_cor[[1]] <- cor3(df_final, df2)
    results_rval_cor[[2]] <- definitivo %>% dplyr::select(array, slide, everything())
    results_rval_cor[[3]] <- points_list
    results_rval_cor[[4]] <- cor3p(df_final, df2)
    } 
    if (input$array_or_seq == 'NGS') {
      n_samples = nrow(rval_sheet_filt())
      n_rows = input$ngs_samplexrun
      n_cols = 1
      n_runs = ceiling(n_samples/(n_rows*n_cols))
      
      array_design = data.frame(run = as.factor(rep(c(1:n_runs), each = n_rows*n_cols))[1:n_samples])
      
      df <- as.data.frame(rval_sheet_filt())
      rownames(df) <- NULL
      df0 = df %>% as.data.frame() %>%
        tibble::column_to_rownames(input$select_input_samplenamevar)
      df1 = df0 %>% dplyr::select(vars) %>% as.data.frame()
      df2 = array_design %>% as.data.frame()
      
      points = Inf
      points_list = list()
      
      for(i in 1:input$select_permutations) {
        df_reorder = df1[sample(nrow(df1)),,drop=FALSE]
        points_list[[i]] = points
        if (score(df_reorder,df2) < points){
          points = score(df_reorder,df2)
          df_final = as.data.frame(df_reorder)
        }
      }
      
      df3 <- df0 %>% tibble::rownames_to_column(input$select_input_samplenamevar) %>% 
        dplyr::select(-vars) %>% as.data.frame()
      
      definitivo <- cbind(df_final, df2) %>% 
        tibble::rownames_to_column(input$select_input_samplenamevar) %>%
        dplyr::left_join(df3, by = input$select_input_samplenamevar) %>%
        tibble::column_to_rownames(input$select_input_samplenamevar)
      
      results_rval_cor <- list()
      results_rval_cor[[1]] <- cor3(df_final, df2)
      results_rval_cor[[2]] <- definitivo %>% dplyr::select(run, everything())
      results_rval_cor[[3]] <- points_list
      results_rval_cor[[4]] <- cor3p(df_final, df2)
      }
      return(results_rval_cor)
  })
  
  # Output sample order results

  output$results_table <- DT::renderDT(
    rval_cor()[[2]],
    rownames = TRUE,
    selection = "single",
    style = "bootstrap",
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,
      columnDefs = list(list(
        targets = match("Basename",colnames(rval_cor()[[2]])) - 1, visible = FALSE
      ))
  ))
  
  # Output sample order results (coefficients)
  
  output$correlation_table <- renderUI({
   
      varinfo <- data.frame(
        Variable = colnames(rval_sheet_filt()),
        Type = unlist(lapply(rval_sheet_filt(), class)))
    
      cors <- rval_cor()[[1]] %>%
        as.data.frame() %>%
        tibble::rownames_to_column('Var1') %>%
        tidyr::gather(Var2, coefficient, -Var1) %>%
        dplyr::mutate(abs_coef = abs(coefficient))

      ps <- rval_cor()[[4]] %>%
        as.data.frame() %>%
        tibble::rownames_to_column('Var1') %>%
        tidyr::gather(Var2, p, -Var1) %>%
        dplyr::mutate(p_plot = round(p, 1),
                      p_signif = signif(p, 2)) %>%
        dplyr::select(p, p_plot, p_signif)

      corr_variables_exp <- na.omit(cbind(cors, ps)) %>%
        left_join(varinfo %>% dplyr::rename(Var1 = Variable, Type1 = Type), by = 'Var1') %>%
        dplyr::arrange(Var1, Var2) %>%
        dplyr::mutate(Corr = round(coefficient, 3), 
                      Pval = p_signif, 
                      Type2 = ifelse(Var2 == 'array', 'numeric', 'factor'),
                      Coefficient = ifelse(Type1 %in% c("integer", "numeric") & Type2 %in% c("integer", "numeric"), "Pearson's R",
                                           ifelse(Type1 %in% c("integer", "numeric") & 
                                                    Type2 %in% c("factor", "character"), 'R Squared',
                                                  ifelse(Type1 %in% c("factor", "character") & 
                                                           Type2 %in% c("integer", "numeric"), 'R Squared',
                                                         ifelse(Type1 %in% c("factor", "character") & 
                                                                  Type2 %in% c("factor", "character"), "Cramér's V", 'nada')))))
      
    output = tagList(
      renderUI({
        cor_text <- c()
        for (i in 1:nrow(corr_variables_exp)) {
              if (corr_variables_exp[i,]$p < 0.05 & corr_variables_exp[i,]$p > 0.01) {
                cor_text[i] <- 
                  paste0('Variables <b>', corr_variables_exp[i,]$Var1, '</b> and <b>', corr_variables_exp[i,]$Var2, 
                         '</b> are <font color=\"#FFD604\"><b> significantly </b></font> correlated!',
                         " (R = ", round(corr_variables_exp[i,]$coefficient, 2), ', p = ', corr_variables_exp[i,]$p_signif, ')')
              } else if (corr_variables_exp[i,]$p < 0.01 & corr_variables_exp[i,]$p > 0.001) {
                cor_text[i] <- 
                  paste0('Variables <b>', corr_variables_exp[i,]$Var1, '</b> and <b>', corr_variables_exp[i,]$Var2, 
                         '</b> are <font color=\"#FF5804\"><b> strongly </b></font> correlated!',
                         " (R = ", round(corr_variables_exp[i,]$coefficient, 2), ', p = ', corr_variables_exp[i,]$p_signif, ')')
              } else if (corr_variables_exp[i,]$p < 0.001) {
                cor_text[i] <- 
                  paste0('Variables <b>', corr_variables_exp[i,]$Var1, '</b> and <b>', corr_variables_exp[i,]$Var2, 
                         '</b> are <font color=\"#FF0404\"><b> extremely </b></font> correlated!',
                         " (R = ", round(corr_variables_exp[i,]$coefficient, 2), ', p = ', corr_variables_exp[i,]$p_signif, ')')
              }
        }
        if (any(corr_variables_exp$p < 0.05)) {
          cor_text <- append(cor_text, paste0('<b>Run more permutations!</b>'))
        }
        cor_text <- na.omit(cor_text)
        HTML(paste0(cor_text, '<br>'))
      }), 
      renderPlotly(
        ggplotly(
              ggscatter(corr_variables_exp, 'Var2', 'Var1', size = 'abs_coef', legend = 'right',
                        xlab = '', ylab = '', color = 'coefficient') +
              border(linetype = 'solid') +
              geom_text(aes_string(x = 'Var2', y = 'Var1', label = 'p_plot'), color = 'black') +
              scale_color_gradientn(colors = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                                               "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"),
                                    limits = c(0,1)) +
              scale_y_discrete(breaks = c(input$select_input_responsevar, input$select_input_covars)) +
              theme_bw(),
              tooltip = 'coefficient') %>% config(displayModeBar = F)  %>% layout(width = 300)
      ),
      DT::renderDT(
        corr_variables_exp %>% 
          dplyr::select(Var1, Var2, Corr, Pval, Type1, Type2, Coefficient) %>%
          arrange(Pval),
        extensions = "Buttons",
        rownames = FALSE,
        style = "bootstrap",
        selection = "single",
        options = list(
          dom = '<"top"l>rt<"bottom"Bip><"clear">',
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          buttons = list(list(extend='csv',
                              filename = 'design_correlations'),
                         list(extend='excel',
                              filename = 'design_correlations'),
                         list(extend='pdf', title = 'Design Correlations and P-Values',
                              filename= 'design_correlations'))
        )))
    output
  })

  # output$correlation_table = renderPlotly({
  #   cors <- rval_cor()[[1]] %>%
  #     as.data.frame() %>%
  #     tibble::rownames_to_column('var1') %>%
  #     tidyr::gather(var2, coefficient, -var1) %>%
  #     dplyr::mutate(abs_coef = abs(coefficient))
  #   
  #   ps <- rval_cor()[[4]] %>%
  #     as.data.frame() %>%
  #     tibble::rownames_to_column('var1') %>%
  #     tidyr::gather(var2, p, -var1) %>%
  #     dplyr::mutate(p = round(p, 1)) %>%
  #     dplyr::select(p)
  #   
  #   all <- na.omit(cbind(cors, ps)) %>%
  #     dplyr::arrange(var1, var2)
  #   
  #   ggplotly(
  #     ggscatter(all, 'var2', 'var1', size = 'abs_coef', legend = 'right', 
  #               xlab = '', ylab = '', color = 'coefficient') +
  #     border(linetype = 'solid') +
  #     geom_text(aes_string(x = 'var2', y = 'var1', label = 'p'), color = 'black') +
  #     scale_color_gradientn(colors = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
  #                                      "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"), 
  #                           limits = c(0,1)) +
  #     scale_y_discrete(breaks = c(input$select_input_responsevar, input$select_input_covars)) +
  #     theme_bw(),
  #     tooltip = 'coefficient') %>% config(displayModeBar = F)  %>% layout(width = 300)
  #       })

  # Select plotting variables
  
  output$plot_variable <- renderUI({
    selectInput(inputId = 'plot_variable_select', label = 'Select plotting variable:', 
                choices = colnames(rval_sheet_filt()), selected = input$select_input_responsevar)
  })  
  
  
  # Plot with experimental design
  
  output$results_plot_microarray = renderPlot({
    rows = input$array_nrow
    cols = input$array_ncol
    
    plot <- rval_cor()[[2]] %>% tibble::rownames_to_column(input$select_input_samplenamevar)
    
    plot$array <- as.numeric(plot$array)
    plot$slide <- paste0('Slide ', as.numeric(plot$slide)) 
    plot$slide <- factor(plot$slide, levels = unique(plot$slide[order(nchar(plot$slide), plot$slide)]))
    
    ggplot2::ggplot(data = plot) +
      geom_tile(aes_string(x = 1, y = 'array', fill = input$plot_variable_select), color = 'black', size = 1) +
      geom_text(aes(x = 1, y = array, label = rownames(rval_cor()[[2]]))) +
      scale_x_continuous(breaks = 1:cols) +
      xlab('') + ylab('') +
      scale_y_reverse(breaks = 1:rows) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 10),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18)) +
      facet_grid(~slide)
  })
  
  output$results_plot_ngs = renderPlot({
    
  rows = input$ngs_samplexrun
  cols = 1
  n_runs = ceiling(nrow(rval_cor()[[2]])/(rows*cols))
  
  plot <- rval_cor()[[2]] %>% tibble::rownames_to_column(input$select_input_samplenamevar)
  plot$run <- paste0('Run ', as.numeric(plot$run))
  plot$run <- factor(plot$run, levels = unique(plot$run[order(nchar(plot$run), plot$run)]))
  plot$position <- as.numeric(paste0(rep(rep(rep(1:rows), 1), n_runs)[1:nrow(rval_cor()[[2]])]))
  
  ggplot2::ggplot(data = plot) +
    geom_tile(aes_string(x = 1, y = 'position', fill = input$plot_variable_select), color = 'black', size = 1, height = 0.8) +
    geom_text(aes(x = 1, y = position, label = rownames(rval_cor()[[2]]))) +
    scale_x_continuous(breaks = 1:cols) +
    xlab('') + ylab('') +
    scale_y_reverse(breaks = 1:rows) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 10),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 18)) +
    facet_grid(~run)
  })
  
  output$diff_results_plot = renderUI({
    switch(
      input$array_or_seq,
      "Microarray" = plotOutput(outputId = 'results_plot_microarray'),
      "NGS" = plotOutput(outputId = 'results_plot_ngs') %>% 
        shinycssloaders::withSpinner(color = 'red', type = 6))
  })
  
  # Plot with the optimization of the balanced design
  
  output$optimization_plot = renderPlot({
    
    plot <- t(as.data.frame((rval_cor()[[3]]))) %>%
      as.data.frame() %>%
      dplyr::rename(score = 1) %>%
      dplyr::mutate(score = log(1/score))
    
    plot <- plot %>%
      dplyr::mutate(permutation = 1:nrow(plot), 
                    score = ifelse(score == -Inf, 0, score))
    
    print(plot)
    
    slope = format(summary(lm(
      formula = score ~ permutation, data = plot))[['coefficients']][2,1], 
      scientific = TRUE, digits = 3)
    
    perm_plot <- ggscatter(plot, 'permutation', 'score', color = 'red',
              ylab = '-log(1/Balancing score)', xlab = 'Permutations (n)', 
              add = 'reg.line', size = 1.5,
              add.params = list(color = "black", size = 0.75),
              title = 'Optimization score') +
      annotate(geom = 'text', label = paste0("B = ",slope), color = 'black',size = 5, 
               x = mean((plot$permutation)*6/4), 
               y = mean((plot$score))*1/6) +
      geom_line(color = 'red', method = 'loess', se = TRUE, fullrange = TRUE, size = 1) +
      font("xlab", size = 16) +
      font("ylab", size = 16)
    
    plot2 <- plot %>% 
      group_by(score) %>%
      summarise(n_perms = n()) %>%
      dplyr::mutate(score_imp = row_number(),
                    score_diff = score - lag(score, default = first(score)))
    
    print(plot2)
    
    imp_plot <- ggscatter(plot2, 'score_imp', 'score_diff', color = 'red',
                       ylab = 'Score increase', xlab = 'Score steps', size = 1, 
                       title = 'Score improvement Plot') %>%
      ggpar(xticks.by = 1) +
      geom_line(color = 'darkred', method = 'loess', se = TRUE) +
      font("xlab", size = 16) +
      font("ylab", size = 16) +
      ggforce::facet_zoom(ylim = c(0,0.4))
    
    cos_plot <- ggscatter(plot2[-nrow(plot2),], 'score_imp', 'n_perms', color = 'red',
              ylab = 'Permutations (n)', xlab = 'Score improvements', size = 1, 
              title = 'Optimization cost Plot') %>%
      ggpar(xticks.by = 1) +
      geom_line(color = 'darkred', method = 'loess', se = TRUE) +
      font("xlab", size = 16) +
      font("ylab", size = 16) 
    
    ggarrange(perm_plot, imp_plot, cos_plot, nrow = 1, widths = c(1,2.5,1))
    
  })

  # Download results
  
  output$download_sample_order = downloadHandler(
    filename = "Balanced_Samples.csv", 
    content = function(file) {
      rval_cor()[[2]] %>%
        tibble::rownames_to_column(input$select_input_samplenamevar) %>% 
        write.csv(file, row.names = F)
    })
  }
  
  # Download sample data
  


