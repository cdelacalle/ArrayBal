`%>%` = magrittr::`%>%`

ui <- navbarPage('Balance my array',
    id = "navbar_shiny",
    theme = shinythemes::shinytheme("simplex") ,
    tabPanel("Input pdata",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput(
            inputId = "fileinput_input",
            "Upload pdata (csv):",
            multiple = FALSE,
            accept = c("application/csv",
                       "application/tsv"),
          ),
          
          uiOutput("ui_button_input_load"),
          h3(),
          
          conditionalPanel(
            "typeof output.samples_table != 'undefined'",
            
            selectInput("select_input_samplenamevar", "", c()),
            h3(),
            
            pickerInput(
              inputId = "select_input_interestvar",
              label = "",
              choices = c(),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            ),
            h3(),
            
            pickerInput(
              inputId = "selected_samples",
              label = "",
              choices = c(),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            ),
            h3(),
            actionButton("button_input_next", "Continue")
          )
        ),
        mainPanel(
          tabsetPanel(id = 'tabset1', type = 'tabs',
                      tabPanel('Table', value = 'table_tab',
                               DT::DTOutput("samples_table") %>% shinycssloaders::withSpinner(color = 'green', type = 5)),
                      tabPanel("Variable Weighting", value = 'weight_tab', 
                               tableOutput("values"),
                               uiOutput("vars_percentage")),
                      tabPanel("Slide/Array Weighting", value = 'weight_tab_array',
                               tableOutput("values_array"),
                               sliderInput("slider_slide", "Slide", min = 0, max = 1, value = 0),
                               sliderInput("slider_array", "Array", min = 0, max = 1, value = 0)))
        )
      )
    ),
    tabPanel("Array design",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 numericInput("array_nrow", "Array rows", value = 8),
                 h3(), 
                 numericInput("array_ncol", "Array columns", value = 1),
                 h3(), 
                 actionButton("button_plot_array", "Plot Array"),
                 h3(),
                 actionButton("button_array_next", "Continue"),
                 h3()),
               mainPanel(
                 plotOutput(outputId = 'array_design')
               ))
             ),
    tabPanel('Sample balancing',
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderInput("select_permutations", label = 'Nº of permutations', min = 1, max = 5000, value = 2000, step = 10),
                 actionButton("button_run", "Run!"),
                 h3()),
               mainPanel(
                 tabsetPanel(id = 'tabset2', type = 'tabs', 
                             tabPanel('Sample Order', value = 'results_tab',
                                      DT::DTOutput("results_table") %>% shinycssloaders::withSpinner(color = 'green', type = 5)),
                             tabPanel('Correlations', value = 'correlations_tab',
                                      DT::DTOutput("correlation_table") %>% shinycssloaders::withSpinner(color = 'green', type = 5)),
                             tabPanel('Experiment', value = 'results_plot_tab',
                                      uiOutput("plot_variable"),
                                      plotOutput('results_plot')))
             ))))