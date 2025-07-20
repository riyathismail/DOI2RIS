library(shiny)
library(rcrossref)

ui <- fluidPage(
  titlePanel("DOI to RIS Converter"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("doiInput", "Enter DOIs:", value = "", rows = 10),
      actionButton("submit", "Submit"),
      downloadButton("downloadRIS", "Download RIS File")
    ),
    mainPanel(
      textOutput("report"),
      tags$hr(),
      HTML("Developed by: Dr. MIM. Riyath, 
           Department of Accountancy and Finance,
           Faculty of Management and Commerce, 
           South Eastern University of Sri Lanka. <br>Email: <a href='mailto:riyath@seu.ac.lk'>riyath@seu.ac.lk</a>")
    )
  )
)

server <- function(input, output, session) {
  # Use reactive values for counters
  counters <- reactiveValues(success = 0, failure = 0)
  
  observeEvent(input$submit, {
    # Reset counters each time the button is pressed
    counters$success <- 0
    counters$failure <- 0
    
    dois <- unlist(strsplit(input$doiInput, "[\r\n]+"))
    citations <- list()
    
    withProgress(message = 'Fetching citations...', value = 0, {
      for (i in seq_along(dois)) {
        setProgress(message = paste("Processing DOI", i, "of", length(dois)),
                    detail = paste("DOI:", dois[i]),
                    value = i/length(dois))
        
        tryCatch({
          citation <- cr_cn(dois[i], format = "ris")
          citations[[i]] <- citation
          counters$success <- counters$success + 1
        }, error = function(e) {
          counters$failure <- counters$failure + 1
          citations[[i]] <- NA
        })
      }
    })
    
    output$report <- renderText({
      paste("Successfully collected:", counters$success, "DOIs. Failed to collect:", counters$failure, "DOIs.")
    })
    
    output$downloadRIS <- downloadHandler(
      filename = function() {
        paste0("citations-", Sys.Date(), ".ris")
      },
      content = function(file) {
        writeLines(unlist(citations), file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
