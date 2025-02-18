library(shiny)
library(shinydashboard)
library(DT)
library(DBI)
library(callr)
library(RSQLite)
library(plotly)
library(shinyalert)
library(shinyjs)
library(varhandle)
#library(data.table)
#plotrix, tidyverse, and tidycensus read in via helpers.R

source("helpers.R")

# # #get big sample for the first part that is common to everyone
# my_sample <- get_sample(var = names(variable_info),
#                        ages = 15:100, state = "all", year = 2022)
# #convert everything to factors...
# my_sample2 <- my_sample %>%
#   mutate(HHLfac = factor(as.character(HHL), labels = HHLvals, levels = names(HHLvals)),
#          HHLANPfac = factor(as.character(HHLANP), labels = HHLANPvals, levels = names(HHLANPvals)),
#          LANPfac = factor(as.character(LANP), labels = LANPvals, levels = names(LANPvals)),
#          LANXfac = factor(as.character(LANX), labels = LANXvals, levels = names(LANXvals)),
#          FSfac = factor(as.character(FS), labels = FSvals, levels = names(FSvals)),
#          WAOBfac = factor(as.character(WAOB), labels = WAOBvals, levels = names(WAOBvals)),
#          FERfac = factor(as.character(FER), labels = FERvals, levels = names(FERvals)),
#          SCHLfac = factor(as.character(SCHL), labels = SCHLvals, levels = names(SCHLvals)),
#          SCHfac = factor(as.character(SCH), labels = SCHvals, levels = names(SCHvals)),
#          RAC1Pfac = factor(as.character(RAC1P), labels = RAC1Pvals, levels = names(RAC1Pvals)),
#          STfac = factor(as.character(ST), labels = state_names, levels = names(state_names)),
#          AGEPnum = as.numeric(AGEP)
#          ) 
# saveRDS(my_sample2, file = 'my_sample.rds')

my_sample <- readRDS("my_sample.rds")

#truth_p <- pull(my_sample, "FFSP") |> as.numeric() |> mean()
  
ui <- dashboardPage(
  #withMathJax(),
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
                  strong("Choose the data table to interact with (only letters please - no spaces or special characters!):"),
                  textInput("db", NULL),
                  strong("Group Name:"),
                  textInput("group_name", NULL),
                  p("We'll look at confidence intervals for p = P(SNAP Recipient)"),
                  strong("Choose a method for constructing your confidence interval:"),
                  radioButtons("ci_type", 
                               label = NULL, 
                               choices = c("Basic Parametric" = "basic", "Score" = "score", "Bootstrap" = "bootstrap"), 
                               selected = "basic"),
                  p("Choose a confidence level:"),
                  sliderInput("conf", label = NULL, min = 0.5, max = 0.99, step = 0.01, value = 0.95),
                  p("Choose a sample size: (a sample size of at least 50 is recommended)"),
                  numericInput("sample_size", label = NULL, min = 20, max = 200000, value = 300),
                  strong("Obtain a sample and construct a confidence interval by clicking the button below."),
                  br(),
                  actionButton("sample", "Get a Sample!"),
                  br(),
                  "Data includes individuals in the US from age 15 to age 100.",
                  br(),
                  "Data courtesy: U.S. Census Bureau. (2022). American Community Survey Public Use Microdata Sample. U.S. Department of Commerce. Retrieved September 12, 2024, from https://data.census.gov/"
                ),
                # Show a plot of the generated CIs
                mainPanel(
                  fluidRow(
                    #actionButton("update_plot", label = "Check for new data and refresh plot"),
                      box(title = "Information about the CIs below.",
                          uiOutput("info"),
                          width = 12
                      )
                    ),
                  fluidRow(
                    plotlyOutput("CI_plots")
                  ),
                  br(),
                  fluidRow(
                    column(8, 
                       box(
                         title = "Information about your current data table",
                         dataTableOutput("data_set_info"),
                         width = 12)
                      ),
                    column(4, 
                       box(
                         title = "Referesh Plot, Data Table, and Information",
                         actionButton("refresh", "Check!"),
                         width = 12
                       )
                    )
                    )
                  )
                )
              ),
              tabItem(tabName = "second",
                      titlePanel("Investigating the Data"),
                      fluidPage(
                        fluidRow(
                          p("In this section of the application you can investigate confidence intervals for p = P(SNAP Recipient) for different (sub)populations. Select your group of interest below and your sample size, then click the button to obtain a sample!"),
                          br(),
                          actionButton('reset', "Reset This Exercise")
                        ),
                        br(),
                        #fluidRow(
                        #   box(title = "Roughly how wide do you want your 95% confidence interval to be?",
                        #       width = 8,
                        #     fluidRow(
                        #       column(9, 
                        #         sliderInput("width", NULL, value = 0.1, min = 0.05, max = 0.5)
                        #       ), 
                        #       column(3, 
                        #         actionButton("sub_width", "Go!")
                        #       )
                        #     ),
                        #     fluidRow(
                        #       column(12, uiOutput("sample_size_value"))
                        #     )
                        #   )
                        # ),
                        uiOutput("sub_pop_ui"),
                        uiOutput("generate_sample"),
                        uiOutput("sub_proportion"),
                        uiOutput("sub_MOE"),
                        uiOutput("select_ci_type"),
                        uiOutput("sub_ci")
                      )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  ##################################################################
  #1st tab stuff
  
  CI_data_frame <- reactiveValues(CIdf = NULL)
  
  observeEvent(input$sample, {
    my_db <- tolower(input$db)
    alpha <- 1-input$conf
    n <- input$sample_size
    gname <- input$group_name
    #first get the full, large data set using the PWGTP as a weight (number of people at that value)
    #my_sample[rep(seq_along(my_sample$PWGTP), my_sample$PWGTP), ] #takes a long time...
    #do a weighted sample instead.. there could be some issues with this but there is a lot of data so I think it will be fine
    #get the subset
    index <- sample(1:nrow(my_sample), size = n, replace = TRUE, prob = my_sample$PWGTP/sum(my_sample$PWGTP))
    my_subset <- my_sample[index, ]
    y <- sum(my_subset$FSfac == "Yes")
    truth <- sum((my_sample$FSfac == "Yes") * my_sample$PWGTP)/sum(my_sample$PWGTP)
    #add the username and any restrictions (none here)
    data_df <- data.frame(y = y, n = n, truth = truth, group = gname)  
    
    #add it to the db
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if(my_db == "" | !grepl('[[:alpha:]]', substr(my_db, 1, 1)) | grepl(' ', my_db)){#grepl("[^\\p{L} ]| ", my_db, perl = TRUE)
      shinyalert(title = "Oh no!", "You must supply a name for the data table you want to work with! The name should start with a letter and not contain any special symbols (other than an underscore).", type = "error")
    } else if(check.numeric(my_db)) {
      shinyalert(title = "Table names must not be numeric.", type = "error")
    } else if (check.numeric(gname) | (tolower(gname) %in% lexicon::profanity_alvarez)){
      shinyalert(title = "Either your group name is blank, numeric, or there may be a curse word present in your name (this can sometimes be flagged by inocuous words). Please try a different group name.", type = "error")
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
    input$refresh
    #input$update_plot
    my_db <- tolower(input$db)
    type <- input$ci_type
    alpha <- 1- input$conf

    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      CI_data_frame$CIdf <- add_ci(data_df, type = type, alpha = alpha)
      ci_df <- CI_data_frame$CIdf
      plot_CI(ci_df)
    } else {
      NULL
    }
  })

  output$info <- renderUI({
    #grab the data
    input$sample
    input$refresh
    #input$update_plot
    my_db <- tolower(input$db)
    #type <- input$ci_type
    #alpha <- 1- input$conf
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      #data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      #ci_df <- add_ci(data_df, type = type, alpha = alpha)
      ci_df <- CI_data_frame$CIdf
      prop <- mean(ci_df$lower < ci_df$truth & ci_df$upper > ci_df$truth)
      tagList(p(paste0("The true proportion of people aged 15 to 100 receiving SNAP benefits in the US is about ", round(ci_df$truth[1], 4), ". \nFrom the set of confidence intervals below, ", 100*round(prop, 4), "% of the intervals contain this population value.")))
    } else {
      tagList(HTML("<ul><li>Select a data table in the top left box.</li><li>Place your name or group name in the box below.</li><li>Choose the type of interval you want to create, sample size (must match other samples in the data table), and confidence level.</li><li>Then click 'Get a Sample!' to draw a sample and create a confidence interval for the p = P(SNAP recipient)!</li></ul>"))
    }

  })
  
  
  output$data_set_info <- renderDataTable({
    #grab the data
    input$sample
    input$refresh
    #input$update_plot
    my_db <- tolower(input$db)    
    #type <- input$ci_type
    #alpha <- 1- input$conf
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "class_data.sqlite")
    on.exit(DBI::dbDisconnect(con))
    if (my_db != "" && DBI::dbExistsTable(con, my_db)){
      #data_df <- dbGetQuery(con, paste0("select * from ", my_db))
      #now add the appropriate CI to the plot
      ci_df <- CI_data_frame$CIdf
      #ci_df <- add_ci(data_df, type = type, alpha = alpha)
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
  dynamic_ui <- reactiveValues(#show_subset = FALSE,
                               show_subset = TRUE,
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
                                 sample_info_message = NULL,
                                 phat_formula = FALSE,
                                 MOE_formula = FALSE,
                                 CI_formula = FALSE)

  observeEvent(input$reset, {
    second_tab_info$message <- NULL
    #dynamic_ui$show_subset <- FALSE
    dynamic_ui$show_subset <- TRUE
    dynamic_ui$show_proportion <- FALSE
    dynamic_ui$show_MOE <- FALSE
    dynamic_ui$show_CI <- FALSE
    second_tab_info$sample_info_message <- NULL
    second_tab_info$phat_formula <- FALSE
  })
  
  # observeEvent(input$sub_width, {
  #   second_tab_info$message <- tagList(p(paste0("The sample size required to obtain an interval of width ", 
  #                                        input$width, 
  #                                        " is ", 
  #                                        ceiling(1.96^2/input$width^2), 
  #                                        ".")))
  #   dynamic_ui$show_subset <- TRUE
  #   
  # })
  
  # output$sample_size_value <- renderUI({
  #   input$sub_width
  #   
  #   second_tab_info$message
  # })
  
  output$sub_pop_ui <- renderUI({
    if(dynamic_ui$show_subset){
      fluidRow(
        box(title = "What (sub) population do you want to investigate?",
            width = 8,
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
            radioButtons('sch', 
                         label = "Current Student?", 
                         choices = c("Include All", "Yes", "No (not in last 3 months)"), 
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
            width = 8,
            id = "get_sample",
            fluidRow(
              column(9, 
                     sliderInput("sample_size_2", "Sample Size", min = 200, max = 5000, value = 500)
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
    #waob <- input$waob
    fer <- input$fer
    sch <- input$sch
    #state <- input$state

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
    if (sch == "Include All"){
      sch <- SCHvals
    } else if(sch == "Yes"){
      sch <- SCHvals[c(2,4)]
    } else if(sch == "No (not in last 3 months)"){
      sch <- SCHvals[3]
    }
    # if (state == "Include All"){
    #   state <- state_names
    # }


    #get the subset
    full_subset <- my_sample %>%
      filter(HHL %in% names(HHLvals[which(HHLvals %in% hhl)]),
             #WAOB %in% names(WAOBvals2[which(WAOBvals2 %in% waob)]),
             FER %in% names(FERvals[which(FERvals %in% fer)]),
             SCH %in% names(SCHvals[which(SCHvals %in% sch)]))
    second_tab_info$subset_max <- nrow(full_subset)
    second_tab_info$full_subset <- full_subset
    
    dynamic_ui$show_proportion <- TRUE
    dynamic_ui$show_MOE <- FALSE
    dynamic_ui$show_CI <- FALSE
    second_tab_info$phat_correct <- FALSE
  })

  observeEvent(ignoreInit = TRUE, list(input$hhl, input$fer, input$sch), {
    n <- input$sample_size_2
    hhl <- input$hhl
    fer <- input$fer
    sch <- input$sch

    if(hhl == "Include All"){
      hhl <- HHLvals
    } else if(hhl == "Not English Only"){
      hhl <- HHLvals[-4]
    }
    if (fer == "Include All"){
      fer <- FERvals
    } else if(fer == "No"){
      fer <- FERvals[1:2]
    }
    if (sch == "Include All"){
      sch <- SCHvals
    } else if(sch == "Yes"){
      sch <- SCHvals[c(2,4)]
    } else if(sch == "No (not in last 3 months)"){
      sch <- SCHvals[3]
    }
    
    #get the subset
    full_subset <- my_sample %>%
      filter(HHL %in% names(HHLvals[which(HHLvals %in% hhl)]),
             FER %in% names(FERvals[which(FERvals %in% fer)]),
             SCH %in% names(SCHvals[which(SCHvals %in% sch)]))
    rows <- sum(full_subset$PWGTP)
    
    
    updateSliderInput(session, 
                      "sample_size_2", 
                      max = 5000, 
                      value = min(input$sample_size_2, rows))
  })

  observeEvent(input$sample_it, {
    
    full_subset <- second_tab_info$full_subset
    my_subset <- full_subset[sample(1:nrow(full_subset), 
                                     size = input$sample_size_2, 
                                    replace = TRUE, 
                                    prob = full_subset$PWGTP/sum(full_subset$PWGTP)), ]
    second_tab_info$subset <- my_subset
    y <- sum(my_subset$FSfac == "Yes")
    truth <- sum((full_subset$FSfac == "Yes") * full_subset$PWGTP)/sum(full_subset$PWGTP)
    phat <- y/input$sample_size_2
    second_tab_info$y <- y
    second_tab_info$phat <- phat
    second_tab_info$truth <- truth
    second_tab_info$MOE <- sqrt(round(phat, 4)*(1-round(phat, 4))/input$sample_size_2)
    
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
    
    if(input$sch == "Include All"){
      sch_message <- "both current students and non students"
    } else if(input$sch == "Yes") {
      sch_message <- "students in private, public or home school"
    } else {
      sch_message <- "non-students"
    }

    second_tab_info$sample_info_message <- tagList(p(paste0("You sampled from the population consisting of ", 
                                                            hhl_message, 
                                                            " households, ",
                                                            fer_message,
                                                            ", and ",
                                                            sch_message,
                                                            ". The number of people using SNAP benefits from your sample of ",
                                                            input$sample_size_2, 
                                                            " is ",
                                                            second_tab_info$y, 
                                                            ".")))
    
    second_tab_info$phat_formula <- FALSE
    second_tab_info$MOE_formula <- FALSE
    second_tab_info$CI_formula <- FALSE
  })

  output$sample_information <- renderUI({
      second_tab_info$sample_info_message
  })
  
  output$sub_proportion <- renderUI({
    if(dynamic_ui$show_proportion){
      fluidRow(
        box(title = "Enter your sample proportion (to four decimal places):",
            width = 8,
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
      )
    }
  })
  
  observeEvent(input$submit_proportion, {
    if(!is.numeric(input$proportion)){
      shinyalert(title = "Oh no!", "You must supply a number between 0 and 1!", type = "error")
    } else {
      if (round(input$proportion, 4) == round(second_tab_info$phat, 4)){
        second_tab_info$phat_correct <- TRUE
        dynamic_ui$show_MOE <- TRUE
      } else {
        second_tab_info$phat_correct <- FALSE
        dynamic_ui$show_MOE <- FALSE
        second_tab_info$phat_formula <- TRUE
      }
    }
  })
  
  output$prop_information <- renderUI({
    if(second_tab_info$phat_correct){
      tagList(p("Correct!"))
    } else if (second_tab_info$phat_formula){
      tagList(p("Incorrect. Remember the sample proportion is the number of successes divided by the sample size."))
    } else {
      #empty message
    }
  })
  
  output$sub_MOE <- renderUI({
    if(dynamic_ui$show_MOE){
      fluidRow(
        box(title = "Use the sample proportion rounded to four decimal places for all further calculations.\n
            Enter the (estimated) standard error of your sample proportion (to four decimal places):",
            width = 8,
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
      )
    }
  })
  
  observeEvent(input$submit_MOE, {
    if(!is.numeric(input$MOE)){
      shinyalert(title = "Oh no!", "You must supply a positive number.", type = "error")
    } else {
      if (round(input$MOE, 4) == round(second_tab_info$MOE, 4)){
        second_tab_info$MOE_correct <- TRUE
        dynamic_ui$show_CI <- TRUE
      } else {
        second_tab_info$MOE_correct <- FALSE
        dynamic_ui$show_CI <- FALSE
        second_tab_info$MOE_formula <- TRUE
      }
    }
  })
  
  output$MOE_information <- renderUI({
    if(second_tab_info$MOE_correct){
      tagList(p("Correct!"))
    } else if(second_tab_info$MOE_formula){
      tagList(p(withMathJax("$$\\text{Incorrect. Remember the (estimated) standard error of your sample proportion is }\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$")))
    } else{
      #blank on purpose
    }
  })
  
  output$select_ci_type <- renderUI({
    if(dynamic_ui$show_CI){
      fluidRow(
        box(title = "Which type of confidence interval do you want to create?",
            width = 8,
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
      )
    }
  })
  
  observeEvent(input$type_of_ci_2nd, {

    if (input$type_of_ci_2nd == "Parametric"){
      second_tab_info$CIlower <- round(second_tab_info$phat, 4) - 1.96 * round(second_tab_info$MOE, 4)
      second_tab_info$CIupper <- round(second_tab_info$phat, 4) + 1.96 * round(second_tab_info$MOE, 4)
    } else if (input$type_of_ci_2nd == "Bootstrap") {
      phats <- rbinom(10000, size = input$sample_size_2, prob = second_tab_info$phat)/input$sample_size_2
      second_tab_info$CIboot_values <- round(phats, 4)
    }
  })
  
  output$boot_graph <- renderPlotly({

    if(input$type_of_ci_2nd == "Parametric"){
      NULL
    } else if (input$type_of_ci_2nd == "Bootstrap"){

      xs <- c(1:999)/1000 + 0.0005

      my_plot_data <- tibble(phat = second_tab_info$CIboot_values) %>%
        arrange(phat) %>%
        mutate(Quantile = findInterval(phat, quantile(phat, xs, type = 4))/1000)

      g <- ggplot(my_plot_data, aes(x = phat)) +
        geom_histogram(bins = 50, fill = "black", aes(group = Quantile))

      g_plotly <- ggplotly(g, tooltip = c("x", "group"))
      
      #determine which bars are non-zero
      bars_shown <- g_plotly$x$data[[1]]$y > 0 
      #just get x values where y > 0
      x_shown <- g_plotly$x$data[[1]]$x[bars_shown]
      #grab the quantile value that corresponds to the closest one to the quantile of interest
      #first, get the quantiles from the text element...
      quants <- lapply(X = g_plotly$x$data[[1]]$text[bars_shown],
             FUN = sub,
             pattern = '.*: ',
             replacement = '') |>
        unlist() |>
        as.numeric()
          
#      second_tab_info$CIlower <- quantile(phats, c(0.015, 0.035))
#      second_tab_info$CIupper <- quantile(phats, c(0.965, 0.985))
      
      second_tab_info$CIlower <- x_shown[which.min(abs(quants - 0.025))]
      second_tab_info$CIupper <- x_shown[which.min(abs(quants - 0.975))]
      g_plotly
    }
  })
  
  output$sub_ci <- renderUI({
    if(dynamic_ui$show_CI){
      if (!is.null(input$type_of_ci_2nd) && input$type_of_ci_2nd == "Parametric"){
        title_text = "Enter the lower and upper bounds for a 95% confidence interval for p (to 4 decimal places). Use 1.96 for the multiplier and use the previous values rounded to four decimal places in your calcuations."
      } else {
        title_text = "Use the graph above to approximate a 95% confidence interval for p. Use the closest value to the quantile you want and round to four decimal places."
      }
      
      fluidRow(
        box(title = title_text,
            width = 8,
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
    }
  })
  
  observeEvent(input$submit_ci, {
    if(is.numeric(input$lower) & is.numeric(input$upper)){
      if(input$type_of_ci_2nd == "Parametric"){
        if ((round(input$lower, 3) <= round(second_tab_info$CIlower, 3) + 0.001) & (round(input$lower, 3) >= round(second_tab_info$CIlower, 3) - 0.001)){
          second_tab_info$CIlower_correct <- TRUE
        } else {
          second_tab_info$CIlower_correct <- FALSE
          second_tab_info$CI_formula <- TRUE
        }
        if ((round(input$upper, 3) <= round(second_tab_info$CIupper, 3) + 0.001) & (round(input$upper, 3) >= round(second_tab_info$CIupper, 3) - 0.001)){
          second_tab_info$CIupper_correct <- TRUE
        } else {
          second_tab_info$CIupper_correct <- FALSE
          second_tab_info$CI_formula <- TRUE
        }
      } else if (input$type_of_ci_2nd == "Bootstrap"){
        if ((input$lower <= second_tab_info$CIlower+0.002) & (input$lower >= second_tab_info$CIlower-0.002)){
          second_tab_info$CIlower_correct <- TRUE
        } else {
          second_tab_info$CIlower_correct <- FALSE
          second_tab_info$CI_formula <- TRUE
        }
        if ((input$upper <= second_tab_info$CIupper+0.002) & (input$upper >= second_tab_info$CIupper-0.002)){
          second_tab_info$CIupper_correct <- TRUE
        } else {
          second_tab_info$CIupper_correct <- FALSE
          second_tab_info$CI_formula <- TRUE
        }
      }
    } else {
      shinyalert(title = "Oh no!", "You must supply numbers in each box!", type = "error")
    }
    })
  
  output$CI_information <- renderUI({
    if (input$type_of_ci_2nd == "Parametric"){
      if(second_tab_info$CIupper_correct & second_tab_info$CIlower_correct){
        tagList(p(paste("Correct! Note: The true proportion for this subset of the population is approximately ", round(second_tab_info$truth, 4))))
      } else if (second_tab_info$CI_formula) {
        tagList(p(withMathJax("$$\\text{Incorrect. Remember the formula for a 95 percent CI is }\\left(\\hat{p} - 1.96*\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}, \\hat{p} + 1.96*\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\right)\\text{.}$$")))
      } else {
        #blank on purpose
      }
    } else if (input$type_of_ci_2nd == "Bootstrap"){
      if(second_tab_info$CIupper_correct & second_tab_info$CIlower_correct){
        tagList(p(paste("Correct! Note: The true proportion for this subset of the population is approximately ", round(second_tab_info$truth, 4))))
      } else if(second_tab_info$CI_formula) {
        tagList(p("Incorrect. Remember the 95% CI corresponds to the 0.025 and 0.975 quantile of the bootstrap distribution. These may not appear on the graph above so try to use the closest quantiles to 0.025 and 0.975."))
      } else {
        #blank on purpose
      }
    }
  })
  
  #if anything has changed above, reset the 'correct' messages to FALSE
  observeEvent(input$sample_it, {
    second_tab_info$CIlower_correct <- FALSE
    second_tab_info$CIlower_correct <- FALSE
    second_tab_info$CI_formula <- FALSE
    second_tab_info$MOE_correct <- FALSE
    second_tab_info$MOE_formula <- FALSE
    second_tab_info$phat_correct <- FALSE
    second_tab_info$phat_formula <- FALSE
    dynamic_ui$show_MOE <- FALSE
    second_tab_info$message <- NULL
    dynamic_ui$show_subset <- TRUE
    dynamic_ui$show_proportion <- TRUE
    dynamic_ui$show_MOE <- FALSE
    dynamic_ui$show_CI <- FALSE
  })
}

# Run the application
shinyApp(ui = ui, server = server, onStart = function() {
  onStop(function() {
    DBI::dbDisconnect(con)
  })
})


#fig = px.histogram(df, x="total_bill", histnorm='percent') fig = go.Figure(data=[go.Histogram(x=df['total_bill'],histnorm='percent')])