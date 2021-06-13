# Creates UI to document the literature review process
library(shiny)

# Define UI for data extraction application
dataExtract_ui_mod <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # Application title
      titlePanel("Data extraction form"),
      sidebarLayout(
        sidebarPanel(
          h2("Meta-data"),
          verbatimTextOutput(ns("meta")),
          fluidRow(column(6, div(style = "text-align:center", actionButton(ns("reset_btn"), "Reset", class="btn-warning"))),
                   column(6, div(style = "text-align:center", downloadButton(ns("download_btn")))))
        ),
        mainPanel(
          fluidRow(
            div(style = "display:inline-block", actionButton(ns("back_btn"), "Back", icon = shiny::icon("angle-left"))),
            div(style = "display:inline-block", uiOutput(ns("status"))),
            div(style = "display:inline-block", actionButton(ns("next_btn"), "Next", icon = shiny::icon("angle-right")))
          ),
          br(),
          fluidRow(
            # Group the questions in two columns RQ2 or RQ3
            column(6,
                   # Simplification Type
                   textAreaInput(ns("simplificationtype"), "Simplification Type"),
                   # Simulation software
                   textAreaInput(ns("simsoftware"), "Simulation Software"),
                   # Study type
                   textAreaInput(ns("studytype"), "Study Type"),
                   # System type
                   textAreaInput(ns("systemtype"), "System Type"),
                   # Application area
                   textAreaInput(ns("apparea"), "Application Area"),
                   # Size and complexity
                   textAreaInput(ns("complexity"), "Size and complexity"),
                   # Motivation
                   textAreaInput(ns("motivation"), "Motivation")),
            column(6,
                   # Complexity measure
                   textAreaInput(ns("complexitymeasure"), "Complexity Measure"),
                   # Validation measure
                   textAreaInput(ns("validationmeasure"), "Validation Measure"),
                   # Efficacy measure
                   textAreaInput(ns("efficacymeasure"), "Efficacy Measure"),
                   hr(),
                   textAreaInput(ns("additional_notes"), "Additional Notes"),
                   actionButton(ns("save_btn"), "Save", class = "btn-primary")
            ))
        )
      )
    ))
}

# Define server logic
dataExtract_server_mod <- function(id, bibliography, data_form) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      extraction_data <- reactiveValues("data" = data_form, current_text = 1)

      title_change <- reactive({
        paste("Status:", extraction_data$current_text, "of", nrow(bibliography))

      })

      fields <- c("simplificationtype" = "",
                  "simsoftware" = "",
                  "studytype" = "",
                  "systemtype" = "",
                  "apparea" = "",
                  "complexity" = "",
                  "motivation" = "",
                  "complexitymeasure" = "",
                  "validationmeasure" = "",
                  "efficacymeasure" = "",
                  "additional_notes" = "")

      create_empty_frame <- function() {
        # Take label from the original dataset, will allow joining later.
        data.frame("label" = bibliography$label,
                   as.list(fields))
      }

      # Get new data when current_text changes
      observeEvent(extraction_data$current_text,  {
        for (type in names(fields)) {
          updateTextAreaInput(session, type, value = extraction_data$data[extraction_data$current_text,type])
        }
        output$status <- renderText({title_change()})
      })

      # Decrease current_text
      observeEvent(input$back_btn, {
        if (extraction_data$current_text > 1) {
          extraction_data$current_text <- extraction_data$current_text - 1
        } else if (extraction_data$current_text == 1) {
          extraction_data$current_text <- nrow(extraction_data$data)
        }
      })

      # Increase current_text
      observeEvent(input$next_btn, {
        if (extraction_data$current_text < nrow(extraction_data$data)) {
          extraction_data$current_text <- extraction_data$current_text + 1
        } else if (extraction_data$current_text == nrow(extraction_data$data)) {
          extraction_data$current_text <- 1
        }
      })

      # Save the entered information, both in the data frame and to disk for redundancy.
      observeEvent(input$save_btn, {
        # Save values to the data frame
        for (type in names(fields)) {
          extraction_data$data[extraction_data$current_text,type] <- input[[type]]
        }
        saveRDS(extraction_data$data, file = paste0("saved_progress_", Sys.Date(),".RData"), compress = "xz")
      })

      # Modal dialog to ensure that resetting isn't done in error.
      observeEvent(input$reset_btn, {
        # Reset
        showModal(modalDialog(
          title = "Somewhat important message",
          textInput("validation", "Reset validation",
                    placeholder = 'Input reset here to reset the database'),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        ))
      })

      observeEvent(input$ok, {
        # Check that data object exists and is data frame.
        if (!is.null(input$validation) && nzchar(input$validation) && input$validation =="reset") {
          extraction_data$data <- create_empty_frame()
        }
        removeModal()
      })

      # Download the resulting data as csv
      output$download_btn <- downloadHandler(
        filename = function() {
          paste("ExtractedData-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(extraction_data$data, file, row.names = FALSE)
        }
      )

      # Prints the identifying information about the article.
      output$meta <-  renderText({paste(str_to_title(bibliography$title[extraction_data$current_text]),
                                        bibliography$author[extraction_data$current_text],
                                        bibliography$year[extraction_data$current_text],
                                        bibliography$journal[extraction_data$current_text], sep = "\n")})

      session$onSessionEnded(function() {
        stopApp(data_form)
      })
    })
}

extract_data_shiny <- function(data) {
  print(shinyApp(dataExtract_ui_mod("dataextract"),
                 function(input, output, session) dataExtract_server_mod("dataextract", data, data)))
}
