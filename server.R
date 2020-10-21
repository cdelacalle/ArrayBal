server <- function(input, output, session) {
  
  # Load button only shows if file is uploaded
  
  output$ui_button_input_load = renderUI({
    if (!is.null(input$fileinput_input$datapath))
      return(actionButton("button_input_load", "Load Data"))
    else
      return()
  })
  
  # Enable load button every time file input is updated
  
  observeEvent(input$fileinput_input, shinyjs::enable("button_input_load"))
  
  # Read pdata sheet from csv
  
  rval_sheet = eventReactive(input$button_input_load, {
    #Check if updated file is .csv
    print(tools::file_ext(input$fileinput_input$datapath))
    
    shiny::validate(need(tools::file_ext(input$fileinput_input$datapath) == "csv", "File extension should be .csv"))
    shinyjs::disable("button_input_load") #disable the load button to avoid multiple clicks
    
    sheet = data.table::fread(input$fileinput_input$datapath, data.table = F)
    
    #We check if sheet is correct
    #This is to prevent app crashes when zip updated is not correct
    shiny::validate(
      need(
        anyDuplicated(colnames(sheet)) == 0,
        "Repeated variable names are not allowed. Please, modify your sample sheet."
      )
    )

    colnames(sheet) = make.names(colnames(sheet)) # fix possible not-valid colnames

    sheet
  })
  
  observeEvent(input$button_input_load, {
    
    updateSelectInput(
      session,
      "select_input_samplenamevar",
      label = "Select Sample Names:",
      choices = colnames(rval_sheet())
    )
    
    updatePickerInput(
      session,
      "select_input_interestvar",
      label = "Select Variable(s) of Interest (max 5):",
      choices = colnames(rval_sheet()) 
    )

    shinyjs::enable("button_input_next") #Enable button continue
  })
  
  # The checkbox of samples to process is updated when samplenamevar changes
  
  observeEvent({
    input$select_input_samplenamevar
  },
  
  updatePickerInput(
    session,
    "selected_samples",
    label = "Select Samples to Process:",
    selected = rval_sheet()[,input$select_input_samplenamevar],
    choices = rval_sheet()[,input$select_input_samplenamevar]
  ))
  
  # Change tab with Continue button
  
  observeEvent(input$button_input_next, {
    updateTabsetPanel(session, "tabset1",
                      selected = "weight_tab")})
  
  # Weighting of variables
  
  output$vars_percentage <- renderUI({
    l <- list()
    for (i in 1:length(input$select_input_interestvar)) {
      l[[i]] <- sliderInput(paste0("percentage_select_var", i), 
                            input$select_input_interestvar[i], 
                            min = 0, max = 1, value = 0)
      }
    return(tagList(l))
  })
  
  observeEvent(lapply(paste0("input$percentage_select_var", seq_len(length(input$select_input_interestvar))), 
                      function(x) {eval(parse(text = x))}), {
    ind_val = unlist(lapply(paste0("input$percentage_select_var", seq_len(length(input$select_input_interestvar))),
                            function(x) {eval(parse(text = x)) 
                              }))
    suma = sum(ind_val)
    last_trigger_number = as.numeric(gsub(".*?([0-9]+).*", "\\1", input$changed))
    
    # resto = (suma - 1) / ( length(input$select_input_interestvar) - last_trigger_number)
    resto = abs(1 - suma) / (length(input$select_input_interestvar)-1)
    # ind_check = ind_val >= rest
    print(suma)
    print(resto)
    
    # if(suma > 1){
    #  if(last_trigger_number+1 <= length(input$select_input_interestvar)) {
    #   for(z in seq(last_trigger_number+1, length(input$select_input_interestvar))){
    #     print(eval(parse(text=paste0("input$percentage_select_var", z))) - resto)
    #     updateSliderInput(session,paste0("percentage_select_var", z),
    #                       value = eval(parse(text=paste0("input$percentage_select_var", z))) - resto)}
    
    if (suma > 1) {
      for (z in 1:length(input$select_input_interestvar)) {
        print(eval(parse(text = paste0("input$percentage_select_var", z))) - resto)
        updateSliderInput(session, paste0("percentage_select_var", z),
                          value = eval(parse(text = paste0("input$percentage_select_var", z))) - resto)
    }}
})
  
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
  
  #when samples selected are changed, continue button is enabled again
  observeEvent(input$selected_samples, shinyjs::enable("button_input_next"))
  
  #The dataframe is rendered
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
  
  #Generate plot 
  
  rval_array = eventReactive(input$button_plot_array,
                             {
                               validate(need(input$array_nrow > 0, 'Minumum rows = 1'),
                                        need(input$array_ncol > 0, 'Minimum columns = 1'))
                               rows = input$array_nrow
                               cols = input$array_ncol
                               array_design = data.frame(rows = rep(rep(1:rows), cols), columns = rep(c(1:cols), each = rows))
                               ggplot2::ggplot(data = array_design) +
                                 geom_tile(aes(x = columns, y = rows), color = 'black', fill = 'white', size = 1) +
                                 scale_x_continuous(breaks = 1:cols) +
                                 xlab('') + ylab('') +
                                 ggplot2::ggtitle('Array design') +
                                 scale_y_reverse(breaks = 1:rows) +
                                 theme(
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   plot.title = element_text(hjust = 0.5, vjust = -1, size = 20),
                                   panel.background = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.ticks.y = element_blank()
                                 )
                             })
  
  # Change tab with Continue button 2
  
  observeEvent(input$button_array_next, {
    updateTabsetPanel(session, "tabset1",
                      selected = "tab_permutations")})

  output$array_design <- renderPlot(width = 200, height = 500, rval_array())
  
  # Correlation function
  
  rval_cor = eventReactive(input$button_run, {
    
    n_rows = input$array_nrow
    n_cols = input$array_ncol
    n_arrays = nrow(rval_sheet())/(n_rows*n_cols)
    
    if (n_cols == 1) {
    array_design = data.frame(array = as.factor(rep(rep(rep(1:n_rows), n_cols), n_arrays)), 
                              slide = as.factor(rep(c(1:n_arrays), each = n_rows*n_cols)))
    } else {
    array_design = data.frame(array = as.factor(rep(rep(rep(1:n_rows), n_cols), n_arrays)), 
                              slide_cols = as.factor(rep(rep(c(1:n_cols), each = n_rows), n_arrays)), 
                              slide_rows = as.factor(rep(c(1:n_arrays), each = n_rows*n_cols)))
    }
    
    df1 = rval_sheet() %>% as.data.frame() %>% tibble::column_to_rownames(input$select_input_samplenamevar)
    df2 = array_design

    points = Inf
    
    score <- function(df_reorder, df2) {
      wscores = cor3(df_reorder,df2)
      for (i in 1:length(input$percentage_select_var)) {
        wscores[input$percentage_select_var[i],] = wscores[input$percentage_select_var[i],]*(eval(parse(text = paste0("input$percentage_select_var", i))))
        wscores[,1] = wscores[,1]*(eval(parse(text = "input$slider_array")))
        wscores[,2] = wscores[,2]*(eval(parse(text = "input$slider_slide")))
      }
      return(mean(abs(wscores)))
    }
    
    for(i in 1:input$select_permutations) {
      df_reorder = df1[sample(nrow(df1)),]
      if (score(df_reorder,df2) < points){
        points = score(df_reorder,df2)
        df_final = df_reorder
      }
      }
    
    definitivo <- cbind(df_final, df2)
    
    results_rval_cor <- list()
    results_rval_cor[[1]] <- cor3(df_final, df2)
    results_rval_cor[[2]] <- definitivo %>% dplyr::select(array, slide, everything())
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
  
  # Output sample order results
  
  output$correlation_table = DT::renderDT(
      round(rval_cor()[[1]], 3),
      rownames = TRUE,
      selection = "single",
      style = "bootstrap",
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(
          targets = match("Basename",colnames(rval_cor()[[1]])) - 1, visible = FALSE
        ))))
  
  # Select plotting variables
  
  output$plot_variable <- renderUI({
    selectInput(inputId = 'plot_variable_select', label = 'Select plotting variable', 
                choices = colnames(rval_sheet()), selected = 'group')
  })  
  
  
  # Plot with experimental design
  
  output$results_plot = renderPlot({
    rows = input$array_nrow
    cols = input$array_ncol
    
    plot <- rval_cor()[[2]] %>% tibble::rownames_to_column('name')
    
    plot$array <- as.numeric(plot$array)
    plot$slide <- as.numeric(plot$slide)
    
    ggplot2::ggplot(data = plot) +
      geom_tile(aes_string(x = 1, y = 'array', fill = input$plot_variable_select), color = 'black', size = 1) +
      geom_text(aes(x = 1, y = array, label = rownames(rval_cor()[[2]]))) +
      scale_x_continuous(breaks = 1:cols) +
      xlab('') + ylab('') +
      ggplot2::ggtitle('Experiment design') +
      scale_y_reverse(breaks = 1:rows) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 20),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
      facet_grid(~slide)
  })
  }



