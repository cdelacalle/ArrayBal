source("functions.R")

ui <- navbarPage(
  title = div(style = "padding: 1px 0px; width: '100%'",
              img(
                src = "balance_adn.svg",
                width = 40,
                height = 20),
              "ArrayBal v.0"),
  windowTitle = 'ArrayBal',
    id = "navbar_shiny",
    theme = shinythemes::shinytheme("simplex"),
    tabPanel("Input data",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput(
            inputId = "fileinput_input",
            "Upload data (csv):",
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
            
            selectInput("select_input_responsevar", "", c()),
            h3(),
            
            shinyWidgets::pickerInput(
              inputId = "select_input_covars",
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
            
            shinyWidgets::pickerInput(
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
            actionButton("button_input_next", "Continue"),
            h3()
          )
        ),
        mainPanel(
          tabsetPanel(id = 'tabset1', type = 'tabs',
                      tabPanel('Table', value = 'table_tab',
                               DT::DTOutput("samples_table") %>% shinycssloaders::withSpinner(color = 'red', type = 6)),
                      tabPanel('Var. Correlations', value = 'cor_tab',
                               selectInput('cor_numeric', '', 
                                           choices = c('Correlation tests', 'Correlation plot'), 
                                           selected = 'Correlation tests'),
                               conditionalPanel("input.cor_numeric == 'Correlation plot'",
                                                uiOutput("variable_color")),
                               uiOutput("diff_outputs")),
                      tabPanel("Var. Weighting", value = 'weight_tab', 
                               div(style="display: inline-block;vertical-align:top; margin-top: 5px; width: 275px;",
                                   uiOutput("vars_percentage")),
                               div(style="display: inline-block;vertical-align:top; width: 10px;",
                                   HTML("<br>")),
                               div(style="display: inline-block;vertical-align:top; margin-top: 30px; width: 100px;",
                                   actionButton('recommended', 'Recommended')),
                               div(style="display: inline-block;vertical-align:top; width: 20px;",
                                   HTML("<br>")),
                               div(style="display: inline-block;vertical-align:top; margin-top: 30px; width: 70px;",
                                   actionButton('evenly', 'Evenly'))))
          )
      )
    ),
    tabPanel("Design",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput('array_or_seq', 'Select Microarray / NGS', 
                             choices = c('Microarray', 'NGS'), selected = 'Microarray'),
                 conditionalPanel("input.array_or_seq == 'Microarray'",
                                  numericInput("array_nrow", "Array rows", value = 8),
                                  h3(),
                                  numericInput("array_ncol", "Array columns", value = 1),
                                  h3()),
                 conditionalPanel("input.array_or_seq == 'NGS'",
                                  numericInput("ngs_samplexrun", "Samples per run", value = 8),
                                  h3()),
                 actionButton("button_plot_array", "Plot Design"),
                 h3(),
                 actionButton("button_array_next", "Continue"),
                 h3()),
               mainPanel(
                 conditionalPanel("input.array_or_seq == 'Microarray'",
                                  uiOutput(outputId = 'array_design')),
                 conditionalPanel("input.array_or_seq == 'NGS'",
                                  plotOutput(outputId = 'ngs_design'))
               ))
             ),
    tabPanel('Sample balancing', value = 'samp_bal_panel',
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderInput("select_permutations", label = 'Nº of permutations', min = 0, max = 25000, value = 2000, step = 100),
                 actionButton("button_run", "Run!"),
                 h3()),
               mainPanel(
                 tabsetPanel(id = 'tabset2', type = 'tabs', 
                             tabPanel('Sample order', value = 'results_tab',
                                      div(style="display: inline-block;vertical-align:top; margin-top: 5px; width: 275px;"),
                                      DT::DTOutput("results_table") %>% shinycssloaders::withSpinner(color = 'red', type = 6)),
                             tabPanel('Design correlations', value = 'correlations_tab',
                                      div(style="display: inline-block;vertical-align:top; margin-top: 5px; width: 275px;"),
                                      uiOutput("correlation_table") %>% shinycssloaders::withSpinner(color = 'red', type = 6)),
                             tabPanel('Experiment design', value = 'results_plot_tab',
                                      div(style="display: inline-block;vertical-align:top; margin-top: 5px; width: 275px;"),
                                      uiOutput("plot_variable"),
                                      uiOutput('diff_results_plot') %>% shinycssloaders::withSpinner(color = 'red', type = 6)),
                             tabPanel('Optimization', value = 'optimization_tab',
                                      div(style="display: inline-block;vertical-align:top; margin-top: 5px; width: 275px;"),
                                      fluidRow(column(12,plotOutput('optimization_plot') %>% shinycssloaders::withSpinner(color = 'red', type = 6)))))
             ))),
    tabPanel('Results',
             h5('Variable Correlations'),
             downloadButton(''),
             h5('Balanced Design'),
             downloadButton('download_sample_order'),
             h5('Design Correlations'),
             downloadButton(''),
             h5('Full Report (HTML)'),
             downloadButton('')),
             tabPanel('About',
             tabsetPanel(id = 'tabset3', type = 'tabs',
                         tabPanel('Overview',
                                  tags$h2('ArrayBal: Microarray experimental design Balancing optimizer'),
                                  div(img(src="balance_adn_name.svg", width="12%")),
             tags$h2('Scope'),
             tags$p(HTML('ArrayBal is a web application that optimizes microarray or NGS experimental designs for multivariate cohort studies.')),
             tags$h2("Approach"),
             tags$p(HTML('ArrayBal optimizes an ideal experimental design by thoroughly balancing all desired variables:')),
             img(src='array_bad_designs.svg', width="35%"),
             h5('It does so by performing random samples ordering followed by correlation of variables with position in slide and array, 
                minimizing potential confounding effects.'),
             h5('Starting from a .csv table with individual detailed information, the workflow of the application includes the following steps:'),
             tags$ol(
               tags$li(h5('Selection of the variables to balance.')),
               tags$li(h5('Descriptive analysis of the relations among variables, including:')),
               tags$ul(
                  tags$li(h5('Inferential association tests among all variables.')),
                  tags$li(h5('Correlation plots among numeric variables.'))),
               tags$li(h5('Weight of the selected variables by balancing preference.')),
               tags$li(h5('Weight of the slide and array variables by balancing preference.')),
               tags$li(h5('Personalization of the microarray design.')),
               tags$li(h5('Optimization of the ideal sample order with balanced covariates:')),
               tags$ul(
                 tags$li(h5('Table with the order samples.')),
                 tags$li(h5('Table with covariate correlation with array design.')),
                 tags$li(h5('Visualization of the ordered samples in a plot.')),
                 tags$li(h5('Information about the optimization calculation.'))),
               tags$li(h5('Export of the table with the sample order.')),
               tags$li(h5('Generation of a .html report with all the previous information.')))),
             tabPanel('Help', 
                      tags$h2("Manual"),
                      tags$p(HTML('A user manual in pdf can be downloaded <a href="user_manual.pdf">here</a>. Sample datasets to test the app are provided below.')),
                      tags$h2("Sample data"),
                      a(href="sampledat1.csv", "Arthritis Dataset", download=NA, target="_blank"),
                      div(style = "margin-top: 5px"),
                      a(href="sampledat2.csv", "Cancer Dataset", download=NA, target="_blank")
             ),
             tabPanel('Credits', 
                      tags$h2("Datasets"),
                      tags$p(HTML('Sample data are completely made up and thus do not contain any personal data.')),
                      tags$h2("R Packages"),
                      tags$p(HTML("Interface programming and data wrangling was perfomed using <a href=\"http://shiny.rstudio.com/\">Shiny</a> and <a href=\"https://www.tidyverse.org/\">tidyverse</a>.")),
                      tags$p(HTML("Plots were generated using <a href=\"https://ggplot2.tidyverse.org\">ggplot2</a> and <a href=\"https://cran.r-project.org/web/packages/ggpubr/index.html\">ggpubr</a>.")),
                      tags$p(HTML("Interface concept and design by <a href=\"https://www.bio-graphics.es/?lang=es\">bio-graphics</a>."))
                      )
             )))
