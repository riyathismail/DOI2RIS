library(shiny)
library(rcrossref)
library(openxlsx)  # For Excel export
library(dplyr)     # For data manipulation

# Function to parse RIS format and extract fields for Excel
parse_ris_to_dataframe <- function(ris_text, doi) {
  if (is.na(ris_text) || is.null(ris_text) || ris_text == "") {
    # Return empty row with DOI and failed status
    return(data.frame(
      DOI = doi,
      Title = NA,
      Authors = NA,
      Journal = NA,
      Year = NA,
      Volume = NA,
      Issue = NA,
      Pages = NA,
      Publisher = NA,
      URL = paste0("https://doi.org/", doi),
      Type = NA,
      ISSN = NA,
      ISBN = NA,
      Language = NA,
      Abstract = NA,
      Keywords = NA,
      Published_Print = NA,
      Published_Online = NA,
      Status = "Failed to fetch citation",
      stringsAsFactors = FALSE
    ))
  }
  
  # Split RIS into lines
  lines <- strsplit(ris_text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  # Initialize fields
  fields <- list(
    DOI = doi,
    Title = NA,
    Authors = character(0),
    Journal = NA,
    Year = NA,
    Volume = NA,
    Issue = NA,
    Pages = NA,
    Publisher = NA,
    URL = paste0("https://doi.org/", doi),
    Type = NA,
    ISSN = NA,
    ISBN = NA,
    Language = NA,
    Abstract = NA,
    Keywords = character(0),
    Published_Print = NA,
    Published_Online = NA,
    Status = "Success"
  )
  
  # Parse each line
  for (line in lines) {
    if (grepl("^[A-Z0-9]{2,4}\\s*-\\s*", line)) {
      tag <- sub("^([A-Z0-9]{2,4})\\s*-\\s*(.*)$", "\\1", line)
      value <- sub("^([A-Z0-9]{2,4})\\s*-\\s*(.*)$", "\\2", line)
      value <- trimws(value)
      
      if (value != "") {
        switch(tag,
               "TI" = fields$Title <- value,
               "T1" = if (is.na(fields$Title)) fields$Title <- value,
               "AU" = fields$Authors <- c(fields$Authors, value),
               "A1" = fields$Authors <- c(fields$Authors, value),
               "JO" = fields$Journal <- value,
               "JF" = if (is.na(fields$Journal)) fields$Journal <- value,
               "T2" = if (is.na(fields$Journal)) fields$Journal <- value,
               "PY" = {
                 # Extract year from date
                 year_match <- regexpr("\\b(19|20)\\d{2}\\b", value)
                 if (year_match > 0) {
                   fields$Year <- as.numeric(regmatches(value, year_match))
                 }
               },
               "Y1" = {
                 if (is.na(fields$Year)) {
                   year_match <- regexpr("\\b(19|20)\\d{2}\\b", value)
                   if (year_match > 0) {
                     fields$Year <- as.numeric(regmatches(value, year_match))
                   }
                 }
               },
               "DA" = {
                 if (is.na(fields$Year)) {
                   year_match <- regexpr("\\b(19|20)\\d{2}\\b", value)
                   if (year_match > 0) {
                     fields$Year <- as.numeric(regmatches(value, year_match))
                   }
                 }
                 fields$Published_Print <- value
               },
               "VL" = fields$Volume <- value,
               "IS" = fields$Issue <- value,
               "SP" = {
                 if (is.na(fields$Pages)) {
                   fields$Pages <- value
                 } else {
                   fields$Pages <- paste0(value, "-", fields$Pages)
                 }
               },
               "EP" = {
                 if (is.na(fields$Pages)) {
                   fields$Pages <- value
                 } else if (!grepl("-", fields$Pages)) {
                   fields$Pages <- paste0(fields$Pages, "-", value)
                 }
               },
               "PB" = fields$Publisher <- value,
               "SN" = {
                 if (is.na(fields$ISSN)) {
                   fields$ISSN <- value
                 } else {
                   fields$ISSN <- paste(fields$ISSN, value, sep = "; ")
                 }
               },
               "BN" = fields$ISBN <- value,
               "LA" = fields$Language <- value,
               "AB" = fields$Abstract <- value,
               "N2" = if (is.na(fields$Abstract)) fields$Abstract <- value,
               "KW" = fields$Keywords <- c(fields$Keywords, value),
               "TY" = fields$Type <- value,
               "UR" = if (fields$URL == paste0("https://doi.org/", doi)) fields$URL <- value
        )
      }
    }
  }
  
  # Convert to data frame format
  result <- data.frame(
    DOI = fields$DOI,
    Title = ifelse(is.na(fields$Title), NA, fields$Title),
    Authors = ifelse(length(fields$Authors) > 0, paste(fields$Authors, collapse = "; "), NA),
    Journal = ifelse(is.na(fields$Journal), NA, fields$Journal),
    Year = ifelse(is.na(fields$Year), NA, fields$Year),
    Volume = ifelse(is.na(fields$Volume), NA, fields$Volume),
    Issue = ifelse(is.na(fields$Issue), NA, fields$Issue),
    Pages = ifelse(is.na(fields$Pages), NA, fields$Pages),
    Publisher = ifelse(is.na(fields$Publisher), NA, fields$Publisher),
    URL = fields$URL,
    Type = ifelse(is.na(fields$Type), NA, fields$Type),
    ISSN = ifelse(is.na(fields$ISSN), NA, fields$ISSN),
    ISBN = ifelse(is.na(fields$ISBN), NA, fields$ISBN),
    Language = ifelse(is.na(fields$Language), NA, fields$Language),
    Abstract = ifelse(is.na(fields$Abstract), NA, fields$Abstract),
    Keywords = ifelse(length(fields$Keywords) > 0, paste(fields$Keywords, collapse = "; "), NA),
    Published_Print = ifelse(is.na(fields$Published_Print), NA, fields$Published_Print),
    Published_Online = NA, # RIS doesn't typically separate online vs print dates
    Status = fields$Status,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

ui <- fluidPage(
  titlePanel("DOI to Citation Converter with Excel Export"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("doiInput", "Enter DOIs:", value = "", rows = 10),
      checkboxInput("includeAbstracts", "Include Abstracts", value = TRUE),
      checkboxInput("includeKeywords", "Include Keywords", value = TRUE),
      actionButton("submit", "Submit"),
      tags$hr(),
      h4("Download Options:"),
      downloadButton("downloadRIS", "Download RIS File", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadExcel", "Download Excel File", class = "btn-success"),
      tags$hr(),
      helpText("Enter one DOI per line. Enable options to fetch abstracts and/or keywords along with citations.")
    ),
    mainPanel(
      textOutput("report"),
      verbatimTextOutput("detailedReport"),
      tags$hr(),
      # Preview table
      h4("Preview of Collected Data:"),
      DT::dataTableOutput("previewTable"),
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
  # Store structured data for Excel export
  structured_data <- reactiveVal(data.frame())
  
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
    excel_data <- data.frame()
    
    withProgress(message = 'Fetching citations...', value = 0, {
      for (i in seq_along(dois)) {
        current_doi <- dois[i]
        setProgress(
          message = paste("Processing DOI", i, "of", length(dois)),
          detail = paste("DOI:", current_doi),
          value = i/length(dois)
        )
        
        citation_result <- tryCatch({
          # Fetch the RIS citation (single source of truth)
          base_citation <- cr_cn(current_doi, format = "ris")
          
          # Parse RIS to get structured data for Excel
          excel_row <- parse_ris_to_dataframe(base_citation, current_doi)
          
          # Initialize enhanced citation with base
          enhanced_citation <- base_citation
          abstract_status <- "No abstract requested"
          keywords_status <- "No keywords requested"
          
          # Handle abstracts and keywords by trying to fetch additional data
          if (input$includeAbstracts || input$includeKeywords) {
            additional_data <- tryCatch({
              cr_works(dois = current_doi)
            }, error = function(e) NULL)
            
            if (!is.null(additional_data) && !is.null(additional_data$data) && nrow(additional_data$data) > 0) {
              data <- additional_data$data[1, ]
              
              # Handle abstracts
              if (input$includeAbstracts && (is.na(excel_row$Abstract) || excel_row$Abstract == "")) {
                abstract <- data$abstract[1]
                if (!is.null(abstract) && !is.na(abstract) && nzchar(trimws(abstract))) {
                  clean_abstract <- gsub("<[^>]*>", "", abstract)
                  clean_abstract <- gsub("\\s+", " ", clean_abstract)
                  clean_abstract <- trimws(clean_abstract)
                  
                  # Add to RIS citation
                  enhanced_citation <- paste(enhanced_citation, sprintf("AB  - %s", clean_abstract), sep = "\n")
                  # Update Excel data
                  excel_row$Abstract <- clean_abstract
                  counters$abstracts_found <- counters$abstracts_found + 1
                  abstract_status <- "Abstract found"
                } else {
                  counters$abstracts_missing <- counters$abstracts_missing + 1
                  abstract_status <- "Abstract not available"
                  if (is.na(excel_row$Abstract)) excel_row$Abstract <- "Not available"
                }
              } else if (input$includeAbstracts && !is.na(excel_row$Abstract) && excel_row$Abstract != "") {
                counters$abstracts_found <- counters$abstracts_found + 1
                abstract_status <- "Abstract found in RIS"
              } else if (input$includeAbstracts) {
                counters$abstracts_missing <- counters$abstracts_missing + 1
                abstract_status <- "Abstract not available"
                excel_row$Abstract <- "Not available"
              }
              
              # Handle keywords
              if (input$includeKeywords && (is.na(excel_row$Keywords) || excel_row$Keywords == "")) {
                keywords <- NULL
                
                # Try the subject field first
                if (!is.null(data$subject)) {
                  subjects <- data$subject[[1]]
                  if (!is.null(subjects) && length(subjects) > 0) {
                    keywords <- subjects
                  }
                }
                
                # If no subjects, try categories
                if (is.null(keywords) && !is.null(data$category)) {
                  categories <- data$category[[1]]
                  if (!is.null(categories) && length(categories) > 0) {
                    if (is.data.frame(categories) && "name" %in% names(categories)) {
                      keywords <- categories$name
                    } else if (is.character(categories)) {
                      keywords <- categories
                    }
                  }
                }
                
                if (!is.null(keywords) && length(keywords) > 0) {
                  clean_keywords <- trimws(keywords)
                  clean_keywords <- clean_keywords[clean_keywords != "" & !is.na(clean_keywords)]
                  
                  if (length(clean_keywords) > 0) {
                    # Add to RIS format
                    for (keyword in clean_keywords) {
                      enhanced_citation <- paste(enhanced_citation, sprintf("KW  - %s", keyword), sep = "\n")
                    }
                    # Update Excel data
                    excel_row$Keywords <- paste(clean_keywords, collapse = "; ")
                    counters$keywords_found <- counters$keywords_found + 1
                    keywords_status <- paste("Keywords found:", length(clean_keywords))
                  } else {
                    counters$keywords_missing <- counters$keywords_missing + 1
                    keywords_status <- "Keywords not available"
                    if (is.na(excel_row$Keywords)) excel_row$Keywords <- "Not available"
                  }
                } else {
                  counters$keywords_missing <- counters$keywords_missing + 1
                  keywords_status <- "Keywords not available"
                  if (is.na(excel_row$Keywords)) excel_row$Keywords <- "Not available"
                }
              } else if (input$includeKeywords && !is.na(excel_row$Keywords) && excel_row$Keywords != "") {
                counters$keywords_found <- counters$keywords_found + 1
                keywords_status <- "Keywords found in RIS"
              } else if (input$includeKeywords) {
                counters$keywords_missing <- counters$keywords_missing + 1
                keywords_status <- "Keywords not available"
                excel_row$Keywords <- "Not available"
              }
            } else {
              # If additional data fetch fails, use what we have from RIS
              if (input$includeAbstracts) {
                if (!is.na(excel_row$Abstract) && excel_row$Abstract != "") {
                  counters$abstracts_found <- counters$abstracts_found + 1
                  abstract_status <- "Abstract found in RIS"
                } else {
                  counters$abstracts_missing <- counters$abstracts_missing + 1
                  abstract_status <- "Abstract not available"
                  excel_row$Abstract <- "Not available"
                }
              }
              
              if (input$includeKeywords) {
                if (!is.na(excel_row$Keywords) && excel_row$Keywords != "") {
                  counters$keywords_found <- counters$keywords_found + 1
                  keywords_status <- "Keywords found in RIS"
                } else {
                  counters$keywords_missing <- counters$keywords_missing + 1
                  keywords_status <- "Keywords not available"
                  excel_row$Keywords <- "Not available"
                }
              }
            }
          }
          
          # Add row to Excel data
          if (nrow(excel_data) == 0) {
            excel_data <- excel_row
          } else {
            excel_data <- rbind(excel_data, excel_row)
          }
          
          # Update success counter and details
          counters$success <- counters$success + 1
          detail_parts <- c("✓", current_doi)
          if (input$includeAbstracts) detail_parts <- c(detail_parts, "-", abstract_status)
          if (input$includeKeywords) detail_parts <- c(detail_parts, "-", keywords_status)
          counters$details <- c(counters$details, paste(detail_parts, collapse = " "))
          
          enhanced_citation
          
        }, error = function(e) {
          counters$failure <- counters$failure + 1
          counters$details <- c(counters$details, 
                                paste("✗", current_doi, "- Citation fetch failed:", e$message))
          
          # Add failed row to Excel data
          failed_row <- data.frame(
            DOI = current_doi,
            Title = NA,
            Authors = NA,
            Journal = NA,
            Year = NA,
            Volume = NA,
            Issue = NA,
            Pages = NA,
            Publisher = NA,
            URL = paste0("https://doi.org/", current_doi),
            Type = NA,
            ISSN = NA,
            ISBN = NA,
            Language = NA,
            Abstract = NA,
            Keywords = NA,
            Published_Print = NA,
            Published_Online = NA,
            Status = paste("Failed:", e$message),
            stringsAsFactors = FALSE
          )
          if (nrow(excel_data) == 0) {
            excel_data <- failed_row
          } else {
            excel_data <- rbind(excel_data, failed_row)
          }
          
          NA
        })
        
        citations[[i]] <- citation_result
      }
    })
    
    # Store data for downloads
    citations_data(citations)
    structured_data(excel_data)
    
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
    
    # Update preview table
    output$previewTable <- DT::renderDataTable({
      if (nrow(excel_data) > 0) {
        DT::datatable(excel_data, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
      }
    })
    
    # Show completion notification
    showNotification(
      paste("Processing completed!", counters$success, "successful,", counters$failure, "failed"),
      type = if (counters$failure == 0) "message" else "warning"
    )
  })
  
  # RIS Download handler
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
  
  # Excel Download handler - Export preview data directly
  output$downloadExcel <- downloadHandler(
    filename = function() {
      parts <- c("citations")
      if (input$includeAbstracts) parts <- c(parts, "abstracts")
      if (input$includeKeywords) parts <- c(parts, "keywords")
      paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Get the exact same data that's shown in the preview table
      data <- structured_data()
      
      if (nrow(data) > 0) {
        # Create workbook
        wb <- createWorkbook()
        addWorksheet(wb, "Citations")
        
        # Write the preview data directly to Excel
        writeData(wb, "Citations", data)
        
        # Format headers
        headerStyle <- createStyle(
          fontSize = 12,
          fontColour = "white",
          halign = "center",
          fgFill = "#4F81BD",
          border = "TopBottom",
          borderColour = "black"
        )
        addStyle(wb, "Citations", headerStyle, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
        
        # Auto-size columns (but limit width for readability)
        setColWidths(wb, "Citations", cols = 1:ncol(data), widths = "auto")
        
        # Set specific widths for text-heavy columns
        if ("Abstract" %in% names(data)) {
          abstract_col <- which(names(data) == "Abstract")
          setColWidths(wb, "Citations", cols = abstract_col, widths = 50)
        }
        if ("Keywords" %in% names(data)) {
          keywords_col <- which(names(data) == "Keywords")  
          setColWidths(wb, "Citations", cols = keywords_col, widths = 30)
        }
        if ("Title" %in% names(data)) {
          title_col <- which(names(data) == "Title")
          setColWidths(wb, "Citations", cols = title_col, widths = 40)
        }
        if ("Authors" %in% names(data)) {
          authors_col <- which(names(data) == "Authors")
          setColWidths(wb, "Citations", cols = authors_col, widths = 30)
        }
        
        # Enable text wrapping for long text columns
        wrapStyle <- createStyle(wrapText = TRUE, valign = "top")
        text_columns <- c("Title", "Authors", "Abstract", "Keywords")
        for (col_name in text_columns) {
          if (col_name %in% names(data)) {
            col_index <- which(names(data) == col_name)
            addStyle(wb, "Citations", wrapStyle, rows = 2:(nrow(data)+1), cols = col_index, gridExpand = TRUE)
          }
        }
        
        # Add borders to all cells
        borderStyle <- createStyle(border = "TopBottomLeftRight", borderColour = "black")
        addStyle(wb, "Citations", borderStyle, rows = 1:(nrow(data)+1), cols = 1:ncol(data), gridExpand = TRUE)
        
        # Save workbook
        saveWorkbook(wb, file, overwrite = TRUE)
        
      } else {
        # Create empty workbook with message
        wb <- createWorkbook()
        addWorksheet(wb, "Citations")
        writeData(wb, "Citations", data.frame(Message = "No citations processed yet - Click Submit first"))
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)