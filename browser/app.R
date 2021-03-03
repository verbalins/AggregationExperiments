# Shiny application used to extract data from selected literature.
# Base for literature review.
#
library(shiny)
library(RefManageR)
library(revtools)
library(stringr)

bibliography <- read_bibliography("../output/LiteratureReview.bib")

# Fields to be used for input
# Added additional_notes as a way to enter additional information
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
    data_form <- data.frame("label" = bibliography$label,
                            as.list(fields))
}

if (file.exists("saved_progress.RData")) {
    data_form <- readRDS("saved_progress.RData")
    if (nrow(data_form) != nrow(bibliography)) {
        data_form <- create_empty_frame()
    }
} else {
    data_form <- create_empty_frame()
}

# Define UI for data extraction application
ui <- fluidPage(

    # Application title
    titlePanel("Data extraction form"),
    sidebarLayout(
        sidebarPanel(
            h2("Meta-data"),
            verbatimTextOutput("meta"),
            fluidRow(column(6, div(style = "text-align:center", actionButton("reset_btn", "Reset", class="btn-warning"))),
                     column(6, div(style = "text-align:center", downloadButton("download_btn"))))
        ),
        mainPanel(
            fluidRow(
                div(style = "display:inline-block", actionButton("back_btn", "Back", icon = shiny::icon("angle-left"))),
                div(style = "display:inline-block", uiOutput("status")),
                div(style = "display:inline-block", actionButton("next_btn", "Next", icon = shiny::icon("angle-right")))
            ),
            br(),
            fluidRow(
            # Group the questions in two columns RQ2 or RQ3
            column(6,
                # Simplification Type
                textAreaInput("simplificationtype", "Simplification Type"),
                # Simulation software
                textAreaInput("simsoftware", "Simulation Software"),
                # Study type
                textAreaInput("studytype", "Study Type"),
                # System type
                textAreaInput("systemtype", "System Type"),
                # Application area
                textAreaInput("apparea", "Application Area"),
                # Size and complexity
                textAreaInput("complexity", "Size and complexity"),
                # Motivation
                textAreaInput("motivation", "Motivation")),
            column(6,
                # Complexity measure
                textAreaInput("complexitymeasure", "Complexity Measure"),
                # Validation measure
                textAreaInput("validationmeasure", "Validation Measure"),
                # Efficacy measure
                textAreaInput("efficacymeasure", "Efficacy Measure"),
                hr(),
                textAreaInput("additional_notes", "Additional Notes"),
                actionButton("save_btn", "Save", class = "btn-primary")
            ))
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    extraction_data <- reactiveValues("data" = data_form, current_text = 1)

    title_change <- reactive({
        paste("Status:", extraction_data$current_text, "of", nrow(bibliography))

    })

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
        saveRDS(extraction_data$data, file = "saved_progress.RData", compress = "xz")
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
}

# Run the application
shinyApp(ui = ui, server = server)
