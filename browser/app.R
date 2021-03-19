# Shiny application used to extract data from selected literature.
# Base for literature review.
#
library(shiny)
library(RefManageR)
library(revtools)
library(stringr)
source("../R/shinyModules.R")

ui <- dataExtract_ui_mod("dataextract")

server <- function(input, output, session) {
    # Two stages:
    # first for evaluating full-text:
    bib_file <- "fulltext"
    # second for further data collection on included papers:
    #bib_file <- "final_fulltext"

    bibliography <- read_bibliography(paste0("../output/",bib_file,".bib"))

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
        data.frame("label" = bibliography$label,
                   as.list(fields))
    }

    if (file.exists(paste0("saved_progress_",bib_file,".RData"))) {
        data_form <- readRDS(paste0("saved_progress_",bib_file,".RData"))
        if (nrow(data_form) != nrow(bibliography)) {
            data_form <- create_empty_frame()
        }
    } else {
        data_form <- create_empty_frame()
    }

    dataExtract_server_mod("dataextract", bibliography, data_form)
}

# Run the application
shinyApp(ui, server)
