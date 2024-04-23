library(shiny)
library(shinydashboard)
library(DT)
library(DBI)
library(callr)
library(RSQLite)
library(plotly)
library(shinyalert)
library(shinyjs)
#library(data.table)
#plotrix, tidyverse, and tidycensus read in via helpers.R

source("helpers.R")

# #get big sample for the first part that is common to everyone
# my_sample <- get_sample(var = names(variable_info),
#                        ages = 15:100, state = "all", year = 2022)
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
                     menuItem("Understanding Confidence", tabName = "first", icon = icon("archive")),
                     menuItem("Investigating Data", tabName = "second", icon = icon("archive"))
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
              ),
              tabItem(tabName = "second",
                      titlePanel("Investigating the Data"),
                      fluidPage(
                        fluidRow(
                          p("Information about the data we are investigating and the purpose of what we'll be doing..."),
                          br(),
                          actionButton('reset', "Reset This Exercise")
                        ),
                        br(),
                        fluidRow(
                          box(title = "How wide do you want your 95% confidence interval to be?",
                            fluidRow(
                              column(9, 
                                sliderInput("width", NULL, value = 0.05, min = 0.01, max = 0.5)
                              ), 
                              column(3, 
                                actionButton("sub_width", "Go!")
                              )
                            ),
                            fluidRow(
                              column(12, uiOutput("sample_size_value"))
                            )
                          )
                        ),
                        uiOutput("sub_pop_ui"),
                        uiOutput("generate_sample"),
                        fluidRow(
                            box(title = "Enter your sample proportion (to 4 decimal places):",
                                fluidRow(
                                  column(9, 
                                         numericInput("proportion", NULL, value = NULL, min = 0, max = 1)
                                  ), 
                                  column(3, 
                                         actionButton("submit_proportion", "Submit!")
                                  )
                                ),
                                fluidRow(
                                  column(12, uiOutput("prop_information"))
                                )
                            )
                        ),
                        fluidRow(
                          box(title = "Enter the (estimated) margin of error for your sample proportion (to 4 decimal places):",
                              fluidRow(
                                column(9, 
                                       numericInput("MOE", NULL, value = NULL, min = 0)
                                ), 
                                column(3, 
                                       actionButton("submit_MOE", "Submit!")
                                )
                              ),
                              fluidRow(
                                column(12, uiOutput("MOE_information"))
                              )
                          )
                        ),
                        fluidRow(
                          box(title = "Which type of confidence interval do you want to create?",
                              fluidRow(
                                column(12, 
                                  radioButtons('type_of_ci_2nd', 
                                             label = NULL,
                                             choices = c("Parametric", "Bootstrap"), 
                                             inline = TRUE)
                                )
                              ),
                              fluidRow(
                                column(12, uiOutput("CI_choice"))
                              ),
                              conditionalPanel("input.type_of_ci_2nd == 'Bootstrap'",
                                fluidRow(
                                  plotlyOutput("boot_graph")
                                )
                              )
                          )
                        ),
                        fluidRow(
                          box(title = "Enter the lower and upper bounds for a 95% confidence interval (to 4 decimal places):",
                              fluidRow(
                                column(4, 
                                       numericInput("lower", NULL, value = NULL, min = 0)
                                ), 
                                column(4, 
                                       numericInput("upper", NULL, value = NULL, min = 0)
                                ),
                                column(3, 
                                       actionButton("submit_ci", "Submit!")
                                )
                              ),
                              fluidRow(
                                column(12, uiOutput("CI_information"))
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
  
  ##################################################################
  #1st tab stuff
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
  
  ##################################################################
  #2nd tab stuff
  dynamic_ui <- reactiveValues(show_subset = FALSE,
                               show_proportion = FALSE,
                               show_MOE = FALSE,
                               show_CI = FALSE)
  
  second_tab_info <- reactiveValues(message = NULL, 
                                 subset_max = 5000, 
                                 full_subset = NULL,
                                 subset = NULL,
                                 y = NULL,
                                 phat = NULL,
                                 phat_correct = FALSE,
                                 truth = NULL,
                                 MOE = NULL,
                                 MOE_correct = FALSE,
                                 CIlower = NULL,
                                 CIlower_correct = FALSE,
                                 CIupper = NULL,
                                 CIupper_correct = FALSE,
                                 CIboot_values = NULL,
                                 sample_info_message = NULL)

  observeEvent(input$reset, {
    second_tab_info$message <- NULL
    dynamic_ui$show_subset <- FALSE
    dynamic_ui$show_proportion <- FALSE
    dynamic_ui$show_MOE <- FALSE
    dynamic_ui$show_CI <- FALSE
    second_tab_info$sample_info_message <- NULL
  })
  
  observeEvent(input$sub_width, {
    second_tab_info$message <- tagList(p(paste0("The sample size required to obtain an interval of width ", 
                                         input$width, 
                                         " is ", 
                                         ceiling(1.96^2/input$width^2), 
                                         ".")))
    dynamic_ui$show_subset <- TRUE
    
  })
  
  output$sample_size_value <- renderUI({
    input$sub_width
    
    second_tab_info$message
  })
  
  output$sub_pop_ui <- renderUI({
    if(dynamic_ui$show_subset){
      fluidRow(
        box(title = "What (sub) population do you want to investigate?",
            radioButtons('hhl',
                         label = "Household Language", 
                         choices = c("Include All", unname(HHLvals)[c(4)], "Not English Only"), 
                         inline = TRUE), #need to remember to group others!
            # radioButtons('waob', 
            #              label = "World Area of Birth", 
            #              choices = c("Include All", "US state", "Others"), 
            #              inline = TRUE, 
            #              selected = c("Include All")),
            radioButtons('fer', 
                         label = "Gave Birth to a Child within past 12 months?", 
                         choices = c("Include All", unname(FERvals)[2:3]), 
                         inline = TRUE),
            radioButtons('fschp', 
                         label = "Current Student?", 
                         choices = c("Include All", unname(FSCHPvals)), 
                         selected = "Include All", 
                         inline = TRUE)#,
            # selectizeInput('state', "State", choices = c("Include All", unname(state_names)), selected = "Include All")
        )
      )
    } 
  })
  
  output$generate_sample <- renderUI({
    if(dynamic_ui$show_subset){
      fluidRow(
        box(title = "Generate a sample from your population:",
            id = "get_sample",
            fluidRow(
              column(9, 
                     sliderInput("sample_size_2", "Sample Size", min = 50, max = 1000, value = 300)
              ), 
              column(3, 
                     actionButton("sample_it", "Get Sample!")
              )
            ),
            fluidRow(
              column(12, uiOutput("sample_information"))
            )
        )
      )
    }
  })
  
  observeEvent(input$sample_it, {
    n <- input$sample_size_2
    hhl <- input$hhl
    waob <- input$waob
    fer <- input$fer
    fschp <- input$fschp
    state <- input$state

    if(hhl == "Include All"){
      hhl <- HHLvals
    } else if(hhl == "Not English Only"){
      hhl <- HHLvals[-4]
    }
    # if (waob == "Include All"){
    #   waob <- WAOBvals2
    # } else if(waob == "Others"){
    #   waob <- WAOBvals2[-2]
    # }
    if (fer == "Include All"){
      fer <- FERvals
    } else if(fer == "No"){
      fer <- FERvals[1:2]
    }
    if (fschp == "Include All"){
      fschp <- FSCHPvals
    }
    # if (state == "Include All"){
    #   state <- state_names
    # }


    #get the subset
    full_subset <- my_sample %>%
      filter(HHL %in% names(HHLvals[which(HHLvals %in% hhl)]),
             #WAOB %in% names(WAOBvals2[which(WAOBvals2 %in% waob)]),
             FER %in% names(FERvals[which(FERvals %in% fer)]),
             FSCHP %in% names(FSCHPvals[which(FSCHPvals %in% fschp)]))
    second_tab_info$subset_max <- nrow(full_subset)
    second_tab_info$full_subset <- full_subset
  })

  observe({
    updateSliderInput(session, 
                      "sample_size_2", 
                      max = min(1000, second_tab_info$subset_max), 
                      value = min(input$sample_size_2, second_tab_info$subset_max))
  })

  observeEvent(input$sample_it, {
    
    my_subset <- second_tab_info$full_subset[sample(1:nrow(second_tab_info$full_subset), size = input$sample_size_2, replace = TRUE), ]
    second_tab_info$subset <- my_subset
    y <- sum(my_subset$FFSPfac == "Yes")
    truth <- mean(my_sample$FFSPfac == "Yes")
    phat <- y/input$sample_size_2
    second_tab_info$y <- y
    second_tab_info$phat <- phat
    second_tab_info$truth <- truth
    second_tab_info$MOE <- sqrt(phat*(1-phat)/input$sample_size_2)
    
    if(input$hhl == "Include All"){
      hhl_message <- "any language"
    } else if (input$hhl == "English Only"){
      hhl_message <- "English only"
    } else {
      hhl_message <- "multiple language"
    }
    
    if(input$fer == "Include All"){
      fer_message <- "those that have and have not given birth in the past 12 months"
    } else if(input$fer == "No") {
      fer_message <- "those that have not given birth in the past 12 months"
    } else {
      fer_message <- "those that have given birth in the past 12 months"
    }
    
    if(input$fer == "Include All"){
      fschp_message <- "both current students and non students"
    } else if(input$fer == "No") {
      fschp_message <- "non students"
    } else {
      fschp_message <- "students"
    }

    second_tab_info$sample_info_message <- tagList(p(paste0("You sampled from the population consisting of ", 
                                                            hhl_message, 
                                                            " households, ",
                                                            fer_message,
                                                            ", and ",
                                                            fschp_message,
                                                            ". The number of people using SNAP benefits from your sample of ",
                                                            input$sample_size_2, 
                                                            " is ",
                                                            second_tab_info$y, 
                                                            ".")))
  })

  output$sample_information <- renderUI({
      second_tab_info$sample_info_message
  })
  
  observeEvent(input$submit_proportion, {
    if (round(input$proportion, 3) == round(second_tab_info$phat, 3)){
      second_tab_info$phat_correct <- TRUE
    } else {
      second_tab_info$phat_correct <- FALSE
    }
  })
  
  output$prop_information <- renderUI({
    if(second_tab_info$phat_correct){
      tagList(p("Correct!"))
    } else {
      tagList(p("Incorrect. Remember the sample proportion is the number of successes divided by the sample size."))
    }
  })
  
  observeEvent(input$submit_MOE, {
    if (round(input$MOE, 3) == round(second_tab_info$MOE, 3)){
      second_tab_info$MOE_correct <- TRUE
    } else {
      second_tab_info$MOE_correct <- FALSE
    }
  })
  
  output$MOE_information <- renderUI({
    if(second_tab_info$MOE_correct){
      tagList(p("Correct!"))
    } else {
      tagList(p("Incorrect. Remember the (estimated) standard error of your sample proportion is the square root of phat*(1-phat)/n."))
    }
  })
  
  observeEvent(ignoreInit = TRUE, list(input$sample_it, input$type_of_ci_2nd), {
    second_tab_info$subset <- my_subset
    
    if (input$type_of_ci_2nd == "Parametric"){
      second_tab_info$CIlower <- second_tab_info$phat - qnorm(0.975) * second_tab_info$MOE
      second_tab_info$CIupper <- second_tab_info$phat + qnorm(0.975) * second_tab_info$MOE
    } else if (input$type_of_ci_2nd == "Bootstrap") {
      phats <- rbinom(10000, size = input$sample_size_2, prob = second_tab_info$phat)/input$sample_size_2
      second_tab_info$CIboot_values <- phats
      second_tab_info$CIlower <- quantile(phats, c(0.015, 0.035))
      second_tab_info$CIupper <- quantile(phats, c(0.965, 0.985))
    }
  })
  
  output$boot_graph <- renderPlotly({
    
    if(input$type_of_ci_2nd == "Parametric"){
      NULL
    } else if (input$type_of_ci_2nd == "Bootstrap"){
      
      xs <- c(1:999)/1000 + 0.0005
      
      my_plot_data <- tibble(phat = second_tab_info$CIboot_values) %>%
        arrange(phat) %>%
        mutate(Quantile = findInterval(phat, quantile(phat, xs))/1000)

      g <- ggplot(my_plot_data, aes(x = phat)) + 
        geom_histogram(bins = 50, fill = "black", aes(group = Quantile))
    
      ggplotly(g, tooltip = c("x", "group"))
    }
  })
  
  observeEvent(input$submit_ci, {
    if(input$type_of_ci_2nd == "Parametric"){
      if (round(input$lower, 3) == round(second_tab_info$CIlower, 3)){
        second_tab_info$CIlower_correct <- TRUE
      } else {
        second_tab_info$CIlower_correct <- FALSE
      }
      if (round(input$upper, 3) == round(second_tab_info$CIupper, 3)){
        second_tab_info$CIupper_correct <- TRUE
      } else {
        second_tab_info$CIupper_correct <- FALSE
      }
    } else if (input$type_of_ci_2nd == "Bootstrap"){
      if ((input$lower <= second_tab_info$CIlower[2]) & (input$lower >= second_tab_info$CIlower[1])){
        second_tab_info$CIlower_correct <- TRUE
      } else {
        second_tab_info$CIlower_correct <- FALSE
      }
      if ((input$upper <= second_tab_info$CIupper[2]) & (input$upper >= second_tab_info$CIupper[1])){      
        second_tab_info$CIupper_correct <- TRUE
      } else {
        second_tab_info$CIupper_correct <- FALSE
      }
    }
    })
  
  output$CI_information <- renderUI({
    if (input$type_of_ci_2nd == "Parametric"){
      if(second_tab_info$CIupper_correct & second_tab_info$CIlower_correct){
        tagList(p("Correct!"))
      } else {
        tagList(p("Incorrect. Remember the formula for a 95% CI is (phat - 1.96*SE, phat + 1.96*SE)."))
      }
    } else if (input$type_of_ci_2nd == "Bootstrap"){
      if(second_tab_info$CIupper_correct & second_tab_info$CIlower_correct){
        tagList(p("Correct!"))
      } else {
        tagList(p("Incorrect. Remember the 95% CI corresponds to the 0.025 and 0.975 quantile of the bootstrap distribution."))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server, onStart = function() {
  onStop(function() {
    DBI::dbDisconnect(con)
  })
})


#fig = px.histogram(df, x="total_bill", histnorm='percent') fig = go.Figure(data=[go.Histogram(x=df['total_bill'],histnorm='percent')])