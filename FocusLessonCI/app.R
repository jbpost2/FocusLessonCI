library(shiny)
library(shinydashboard)
library(DT)
library(DBI)
library(callr)
library(RSQLite)
library(plotly)
library(shinyalert)
#library(data.table)
#plotrix, tidyverse, and tidycensus read in via helpers.R

source("helpers.R")

# #get big sample for the first part that is common to everyone
# my_sample <- get_sample(var = names(variable_info),
#                        ages = 18:30, state = "all", year = 2022)
# #convert everything to factors...
# my_sample2 <- my_sample %>%
#   mutate(HHLfac = factor(as.character(HHL), labels = HHLvals, levels = names(HHLvals)),
#          HHLANPfac = factor(as.character(HHLANP), labels = HHLANPvals, levels = names(HHLANPvals)),
#          LANPfac = factor(as.character(LANP), labels = LANPvals, levels = names(LANPvals)),
#          LANXfac = factor(as.character(LANX), labels = LANXvals, levels = names(LANXvals)),
#          FLANPfac = factor(as.character(FLANP), labels = FLANPvals, levels = names(FLANPvals)),
#          FLANXPfac = factor(as.character(FLANXP), labels = FLANXPvals, levels = names(FLANXPvals)),
#          FFSPfac = factor(as.character(FFSP), labels = FFSPvals, levels = names(FFSPvals)),
#          WAOBfac = factor(as.character(WAOB), labels = WAOBvals, levels = names(WAOBvals)),
#          FERfac = factor(as.character(FER), labels = FERvals, levels = names(FERvals)),
#          SCHLfac = factor(as.character(SCHL), labels = SCHLvals, levels = names(SCHLvals)),
#          SCHfac = factor(as.character(SCH), labels = SCHvals, levels = names(SCHvals)),
#          FSCHPfac = factor(as.character(FSCHP), labels = FSCHPvals, levels = names(FSCHPvals)),
#          RAC1Pfac = factor(as.character(RAC1P), labels = RAC1Pvals, levels = names(RAC1Pvals)),
#          STfac = factor(as.character(ST), labels = state_names, levels = names(state_names))
#          )
# saveRDS(my_sample2, file = 'my_sample.rds')

my_sample <- readRDS("my_sample.rds")

#truth_p <- pull(my_sample, "FFSP") |> as.numeric() |> mean()
  
ui <- dashboardPage(
  # withMathJax(),
  dashboardHeader(title = "Confidence Interval Activity", disable = FALSE),
  dashboardSidebar(disable = FALSE,
                   sidebarMenu(
                     menuItem("Understanding Confidence", tabName = "first", icon = icon("archive"))
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first",
              titlePanel("Understanding Confidence"),
              sidebarLayout(
                sidebarPanel(
                  strong("Choose the data table to interact with:"),
                  textInput("db", NULL),
                  strong("Give your name or group name:"),
                  textInput("group_name", NULL),
                  p("We'll look at confidence intervals for p = P(SNAP Recipient)"),
                  strong("Choose a method for constructing your confidence interval:"),
                  radioButtons("ci_type", 
                               label = NULL, 
                               choices = c("Basic Parametric" = "basic", "Score" = "score", "Bootstrap" = "bootstrap"), 
                               selected = "basic"),
                  p("Choose a confidence level:"),
                  sliderInput("conf", label = NULL, min = 0.5, max = 0.99, step = 0.01, value = 0.95),
                  p("Choose a sample size: (a sample size of at least 200 is recommended)"),
                  numericInput("sample_size", label = NULL, min = 1, max = 400000, value = 300),
                  strong("Obtain a sample and construct a confidence interval by clicking the button below."),
                  br(),
                  actionButton("sample", "Get a Sample!"),
                  br(),
                  "Data comes from the US Census ACS Survey (Public Use Microdata Sample)"
                ),
                # Show a plot of the generated CIs
                mainPanel(
                  fluidRow(
                    #actionButton("update_plot", label = "Check for new data and refresh plot"),
                    plotlyOutput("CI_plots")
                  ),
                  br(),
                  fluidRow(
                    box(
                      title = "Information about your current dataset",
                      dataTableOutput("data_set_info"),
                      width = 6),
                    box(title = "Information about the above CIs",
                        uiOutput("info"),
                      width = 6
                    )
                  )
                )
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observeEvent(input$sample, {
    my_db <- input$db
    alpha <- 1-input$conf
    n <- input$sample_size
    gname <- input$group_name
    #get the subset
    my_subset <- my_sample[sample(1:nrow(my_sample), size = n, replace = FALSE), ]
    y <- sum(my_subset$FFSPfac == "Yes")
    truth <- mean(my_sample$FFSPfac == "Yes")
    #add the username and any restrictions (none here)
    data_df <- data.frame(y = y, n = n, truth = truth, group = gname)  
    
    #add it to the db
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if(my_db == ""){
      shinyalert(title = "Oh no!", "You must supply a name for the data table you want to work with!", type = "error")
    } else if(DBI::dbIsValid(con) && !DBI::dbExistsTable(con, my_db)){
      DBI::dbWriteTable(con, my_db, data_df)
    } else if(DBI::dbIsValid(con)){
      #grab data, check that sample size is the same (later restrictions)
      current_df <- dbGetQuery(con, paste0("select * from ", my_db))
      if(current_df$n[1] == n){
        DBI::dbWriteTable(con, my_db, data_df, append = TRUE)
      } else {
        shinyalert(title = "Oh no!", paste0("The data table ", my_db, " already has data in it with a sample size of ", current_df$n[1], ". We can't add data with a different sample size."), type = "error")
      }
    }
  })
  
  #plot the CI(s)
  output$CI_plots <- renderPlotly({
    #grab the data
    input$sample
    #input$update_plot
    my_db <- input$db    
    type <- input$ci_type
    alpha <- 1- input$conf

    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      ci_df <- add_ci(data_df, type = type, alpha = alpha)
      plot_CI(ci_df)
    } else {
      NULL
    }
  })

  output$info <- renderUI({
    #grab the data
    input$sample
    #input$update_plot
    my_db <- input$db    
    type <- input$ci_type
    alpha <- 1- input$conf
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      ci_df <- add_ci(data_df, type = type, alpha = alpha)
      
      prop <- mean(ci_df$lower < ci_df$truth & ci_df$upper > ci_df$truth)
      tagList(p(paste0("The true proportion of people aged 18 to 30 receiving SNAP benefits in the US is ", round(ci_df$truth[1], 4), ". \nFrom the set of confidence intervals above, ", 100*round(prop, 4), "% of the intervals contain this population value.")))
    } else {
      tagList(HTML("<ul><li>Select a data table in the top left box.</li><li>Place your name or group name in the box below.</li><li>Choose the type of interval you want to create, sample size (must match other samples in the data table), confidence level, and sample size.</li><li>Then click 'Get a Sample!' to draw a sample and create a confidence interval for the p = P(SNAP recipient)!</li></ul>"))
    }

  })
  
  
  output$data_set_info <- renderDataTable({
    #grab the data
    input$sample
    #input$update_plot
    my_db <- input$db    
    type <- input$ci_type
    alpha <- 1- input$conf
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      ci_df <- add_ci(data_df, type = type, alpha = alpha)
      ci_df %>% 
        mutate(p_hat = round(y/n, 4)) %>% 
        select(group, y, p_hat, lower, upper, col) %>% 
        mutate(lower = round(lower, 4), upper = round(upper, 4)) %>%
        DT::datatable(options = list(
          columnDefs = list(list(targets = 5, visible = FALSE))
        ), rownames = FALSE) %>%
        formatStyle(columns = c("lower", "upper"),
                    valueColumns = "col",
                    target = "cell",
                    backgroundColor = styleEqual(c("Black", "Red"), c("springgreen", "tomato")))
    } else {
      NULL
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server, onStart = function() {
  onStop(function() {
    DBI::dbDisconnect(con)
  })
})
