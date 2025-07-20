library(shiny)
library(rcrossref)

ui <- fluidPage(
  titlePanel("DOI to RIS Converter with Abstracts & Keywords"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("doiInput", "Enter DOIs:", value = "", rows = 10),
      checkboxInput("includeAbstracts", "Include Abstracts", value = TRUE),
      checkboxInput("includeKeywords", "Include Keywords", value = TRUE),
      actionButton("submit", "Submit"),
      downloadButton("downloadRIS", "Download RIS File"),
      tags$hr(),
      helpText("Enter one DOI per line. Enable options to fetch abstracts and/or keywords along with citations.")
    ),
    mainPanel(
      textOutput("report"),
      verbatimTextOutput("detailedReport"),
      tags$hr(),
      HTML("Developed by: Dr. MIM. Riyath, 
           Department of Accountancy and Finance,
           Faculty of Management and Commerce, 
           South Eastern University of Sri Lanka. <br>Email: <a href='mailto:riyath@seu.ac.lk'>riyath@seu.ac.lk</a>")
    )
  )
)

server <- function(input, output, session) {
  # Use reactive values for counters and detailed tracking
  counters <- reactiveValues(
    success = 0, 
    failure = 0, 
    abstracts_found = 0, 
    abstracts_missing = 0,
    keywords_found = 0,
    keywords_missing = 0,
    details = character(0)
  )
  
  # Store citations for download
  citations_data <- reactiveVal(list())
  
  observeEvent(input$submit, {
    # Reset counters and data each time the button is pressed
    counters$success <- 0
    counters$failure <- 0
    counters$abstracts_found <- 0
    counters$abstracts_missing <- 0
    counters$keywords_found <- 0
    counters$keywords_missing <- 0
    counters$details <- character(0)
    
    dois <- unlist(strsplit(input$doiInput, "[\r\n]+"))
    # Remove empty lines and trim whitespace
    dois <- trimws(dois[dois != ""])
    
    if (length(dois) == 0) {
      showNotification("Please enter at least one DOI", type = "warning")
      return()
    }
    
    citations <- list()
    
    withProgress(message = 'Fetching citations, abstracts, and keywords...', value = 0, {
      for (i in seq_along(dois)) {
        current_doi <- dois[i]
        setProgress(
          message = paste("Processing DOI", i, "of", length(dois)),
          detail = paste("DOI:", current_doi),
          value = i/length(dois)
        )
        
        citation_result <- tryCatch({
          # Fetch the RIS citation
          citation <- cr_cn(current_doi, format = "ris")
          
          # Initialize result
          final_citation <- citation
          abstract_status <- "No abstract requested"
          keywords_status <- "No keywords requested"
          
          # If abstracts or keywords are requested, try to fetch them
          if (input$includeAbstracts || input$includeKeywords) {
            metadata_result <- tryCatch({
              # Fetch detailed work information
              work_details <- cr_works(dois = current_doi)
              
              abstract_result <- "No abstract requested"
              keywords_result <- "No keywords requested"
              
              if (!is.null(work_details$data) && nrow(work_details$data) > 0) {
                
                # Handle abstracts
                if (input$includeAbstracts) {
                  abstract <- work_details$data$abstract[1]
                  
                  if (!is.null(abstract) && !is.na(abstract) && nzchar(trimws(abstract))) {
                    # Clean the abstract (remove HTML tags if present)
                    clean_abstract <- gsub("<[^>]*>", "", abstract)
                    clean_abstract <- gsub("\\s+", " ", clean_abstract)
                    clean_abstract <- trimws(clean_abstract)
                    
                    # Append the abstract to the citation using proper RIS format
                    final_citation <- paste(final_citation, sprintf("AB  - %s", clean_abstract), sep = "\n")
                    counters$abstracts_found <- counters$abstracts_found + 1
                    abstract_result <- "Abstract found"
                  } else {
                    counters$abstracts_missing <- counters$abstracts_missing + 1
                    abstract_result <- "Abstract not available"
                  }
                }
                
                # Handle keywords
                if (input$includeKeywords) {
                  # Keywords might be in different fields, try multiple approaches
                  keywords <- NULL
                  
                  # Try the subject field first (most common for keywords)
                  if (!is.null(work_details$data$subject)) {
                    subjects <- work_details$data$subject[[1]]
                    if (!is.null(subjects) && length(subjects) > 0) {
                      keywords <- subjects
                    }
                  }
                  
                  # If no subjects, try categories
                  if (is.null(keywords) && !is.null(work_details$data$category)) {
                    categories <- work_details$data$category[[1]]
                    if (!is.null(categories) && length(categories) > 0) {
                      # Extract category names if they exist
                      if (is.data.frame(categories) && "name" %in% names(categories)) {
                        keywords <- categories$name
                      } else if (is.character(categories)) {
                        keywords <- categories
                      }
                    }
                  }
                  
                  if (!is.null(keywords) && length(keywords) > 0) {
                    # Clean and format keywords
                    clean_keywords <- trimws(keywords)
                    clean_keywords <- clean_keywords[clean_keywords != "" & !is.na(clean_keywords)]
                    
                    if (length(clean_keywords) > 0) {
                      # Add each keyword as a separate KW line (RIS format)
                      for (keyword in clean_keywords) {
                        final_citation <- paste(final_citation, sprintf("KW  - %s", keyword), sep = "\n")
                      }
                      counters$keywords_found <- counters$keywords_found + 1
                      keywords_result <- paste("Keywords found:", length(clean_keywords))
                    } else {
                      counters$keywords_missing <- counters$keywords_missing + 1
                      keywords_result <- "Keywords not available"
                    }
                  } else {
                    counters$keywords_missing <- counters$keywords_missing + 1
                    keywords_result <- "Keywords not available"
                  }
                }
                
              } else {
                if (input$includeAbstracts) {
                  counters$abstracts_missing <- counters$abstracts_missing + 1
                  abstract_result <- "No work details found"
                }
                if (input$includeKeywords) {
                  counters$keywords_missing <- counters$keywords_missing + 1
                  keywords_result <- "No work details found"
                }
              }
              
              list(abstract = abstract_result, keywords = keywords_result)
              
            }, error = function(e) {
              if (input$includeAbstracts) {
                counters$abstracts_missing <- counters$abstracts_missing + 1
              }
              if (input$includeKeywords) {
                counters$keywords_missing <- counters$keywords_missing + 1
              }
              list(
                abstract = if (input$includeAbstracts) paste("Abstract fetch error:", e$message) else "No abstract requested",
                keywords = if (input$includeKeywords) paste("Keywords fetch error:", e$message) else "No keywords requested"
              )
            })
            
            abstract_status <- metadata_result$abstract
            keywords_status <- metadata_result$keywords
          }
          
          # Update success counter and details
          counters$success <- counters$success + 1
          detail_parts <- c("✓", current_doi)
          if (input$includeAbstracts) detail_parts <- c(detail_parts, "-", abstract_status)
          if (input$includeKeywords) detail_parts <- c(detail_parts, "-", keywords_status)
          counters$details <- c(counters$details, paste(detail_parts, collapse = " "))
          
          final_citation
          
        }, error = function(e) {
          counters$failure <- counters$failure + 1
          counters$details <- c(counters$details, 
                                paste("✗", current_doi, "- Citation fetch failed:", e$message))
          NA
        })
        
        citations[[i]] <- citation_result
      }
    })
    
    # Store citations for download
    citations_data(citations)
    
    # Update main report
    output$report <- renderText({
      report_text <- paste("Processing completed!", 
                           paste("Successfully collected:", counters$success, "citations."),
                           paste("Failed to collect:", counters$failure, "citations."),
                           sep = "\n")
      
      if (input$includeAbstracts) {
        report_text <- paste(report_text,
                             paste("Abstracts found:", counters$abstracts_found),
                             paste("Abstracts missing:", counters$abstracts_missing),
                             sep = "\n")
      }
      
      if (input$includeKeywords) {
        report_text <- paste(report_text,
                             paste("Keywords found:", counters$keywords_found),
                             paste("Keywords missing:", counters$keywords_missing),
                             sep = "\n")
      }
      
      report_text
    })
    
    # Update detailed report
    output$detailedReport <- renderText({
      paste(counters$details, collapse = "\n")
    })
    
    # Show completion notification
    showNotification(
      paste("Processing completed!", counters$success, "successful,", counters$failure, "failed"),
      type = if (counters$failure == 0) "message" else "warning"
    )
  })
  
  # Download handler
  output$downloadRIS <- downloadHandler(
    filename = function() {
      parts <- c("citations")
      if (input$includeAbstracts) parts <- c(parts, "abstracts")
      if (input$includeKeywords) parts <- c(parts, "keywords")
      paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".ris")
    },
    content = function(file) {
      citations <- citations_data()
      if (length(citations) > 0) {
        # Remove NA values
        valid_citations <- citations[!is.na(citations)]
        if (length(valid_citations) > 0) {
          writeLines(unlist(valid_citations), file)
        } else {
          writeLines("# No valid citations to export", file)
        }
      } else {
        writeLines("# No citations processed yet", file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)