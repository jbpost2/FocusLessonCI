library(DT)
library(DBI)
library(shiny)
library(callr)
library(RSQLite)
library(datasets)
library(data.table)

# create dummy data -------------------------------------------------------
bgprocess <- r_bg(func = function(){
  con <- DBI::dbConnect(RSQLite::SQLite(), "iris_db.sqlite")
  on.exit(dbDisconnect(con))
  if(DBI::dbIsValid(con) && !DBI::dbExistsTable(con, "iris")){
    DBI::dbWriteTable(con, "iris", datasets::iris[1,])
  }
  for (i in 2:NROW(iris)){
    Sys.sleep(4)
    print("Updating DB")
    DBI::dbWriteTable(con, "iris", datasets::iris[i,], append = TRUE)
    if(i == NROW(datasets::iris)){
      print("Finished updating DB")
    }
  }
})

# bgprocess$is_alive()
print(paste("bgprocess pid:", bgprocess$get_pid()))


# shiny app ---------------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "iris_db.sqlite")
row_count <- 0L
previous_row_count <- 0L
display_data <- reactiveVal(NULL)

ui <- fluidPage(DTOutput("my_db_data"))

db_data_chunk <- reactivePoll(
  intervalMillis = 1000L, # check for a db update every second
  session = NULL,
  checkFunc = function() {
    print(paste("Running checkFunc:", Sys.time()))
    if(DBI::dbIsValid(con) && dbExistsTable(con, "iris")){
      row_count <<- dbGetQuery(con, "select count(*) from iris")[[1]]
    } else {
      0L
    }
  },
  valueFunc = function() {
    if(DBI::dbIsValid(con) && dbExistsTable(con, "iris")){
      print(paste("Running valueFunc: Updating display_data | Current row count:", row_count))
      DT <- setDT(dbGetQuery(con, sprintf("select * from iris LIMIT %s OFFSET %s", row_count-previous_row_count, previous_row_count)))
      previous_row_count <<- row_count
      DT
    } else {
      NULL
    }
  }
)

server <- function(input, output, session) {
  observeEvent(db_data_chunk(), {
    if(is.null(display_data())){
      display_data(db_data_chunk())
    } else {
      display_data(rbindlist(list(display_data(), db_data_chunk())))
    }
  })
  # check ?dataTableProxy() and ?replaceData() to avoid re-rendering the table
  output$my_db_data <- renderDT({req(display_data())}, server = FALSE)
}

shinyApp(ui, server, onStart = function() {
  onStop(function() {
    dbDisconnect(con)
  })
})