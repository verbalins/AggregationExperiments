library(revtools)
library(tidyverse)
library(fulltext)
library(RefManageR)

wos <- read_bibliography("data/bib/wos-search.bib") %>% filter(year > 2017)
scopus <- read_bibliography("data/bib/scopus-search.bib") %>% filter(year > 2017)
ieee <- read_bibliography("data/bib/ieee-search.csv") %>% filter("Publication Year" > 2017)

# Import known references and apply some fixes
vanderzee <- read_bibliography("data/bib/vanderzee.bib") %>%
  mutate(title = str_to_title(title),
         year = as.character(year),
         filename="data/bib/vanderzee.bib")

vanderzee[grep("Yuan", vanderzee$label),]$doi <- "10.1109/WSC.2014.7020096" # Missing values
vanderzee[grep("Yuan", vanderzee$label),]$type <- "InProceedings"
vanderzee[grep("Yuan", vanderzee$label),]$title <- "Towards a semiconductor supply chain simulation library (SCSC-SIMLIB)"

vanderzee[grep("Pidd1998", vanderzee$label),]$title <- vanderzee[grep("Pidd1998", vanderzee$label),]$journal
vanderzee[grep("Pidd1998", vanderzee$label),]$journal <- NA_character_
vanderzee[grep("Pidd1998", vanderzee$label),]$type <- "BOOK"

vanderzee[grep("Law1991", vanderzee$label),]$title <- vanderzee[grep("Law1991", vanderzee$label),]$journal
vanderzee[grep("Law1991", vanderzee$label),]$journal <- NA_character_
vanderzee[grep("Law1991", vanderzee$label),]$type <- "BOOK"

vanderzee[grep("Robinson2004", vanderzee$label),]$title <- vanderzee[grep("Robinson2004", vanderzee$label),]$journal
vanderzee[grep("Robinson2004", vanderzee$label),]$journal <- NA_character_
vanderzee[grep("Robinson2004", vanderzee$label),]$type <- "BOOK"

vanderzee[grep("Zeigler1976", vanderzee$label),]$journal <- NA_character_
vanderzee[grep("Zeigler1976", vanderzee$label),]$type <- "BOOK"

vanderzee[grep("Pegden1995", vanderzee$label),]$title <- vanderzee[grep("Pegden1995", vanderzee$label),]$journal
vanderzee[grep("Pegden1995", vanderzee$label),]$journal <- NA_character_
vanderzee[grep("Pegden1995", vanderzee$label),]$type <- "BOOK"

# Import the references from the database search
sources <- read_bibliography(list.files(path="data/bib/", pattern = "*search.bib|*search.csv", full.names = TRUE)) %>%
  mutate(title = stringr::str_to_title(if_else(!is.na(title), title, document_title)),
         doi = if_else(!is.na(DOI), DOI, doi),
         year = if_else(!is.na(publication_year), as.character(publication_year), year),
         issn = if_else(!is.na(ISSN), ISSN, issn),
         journal = if_else(!is.na(journal), journal, publication_title)) %>%
  select(-DOI, -publication_year, -document_title, -ISSN, -publication_title) %>%
  filter(year > 2017, year < 2021)

sources <- full_join(sources, vanderzee)

stopifnot({ # Need year and title on all documents
  all(!is.na(sources$title));
  all(!is.na(sources$year))
})

matches <- find_duplicates(sources,
                           match_variable = "title",
                           group_variables = c("doi","year"),
                           to_lower = TRUE,
                           remove_punctuation = TRUE)

unique_references <- extract_unique_references(sources, matches)

deduplicated_sources <- screen_duplicates(unique_references)

titles_screened <- revtools::screen_titles(deduplicated_sources)

selected_abs <- revtools::screen_abstracts(titles_screened)

excluded_titles <- titles_screened %>% dplyr::filter(screened_titles == "excluded") %>% nrow()
excluded_abstracts <- selected_abs %>% dplyr::filter(screened_titles != "excluded", screened_abstracts == "excluded") %>% nrow()

# For import to Mendeley and the R app, formats better than other packages
fulltext <- selected_abs %>% dplyr::filter(screened_abstracts == "selected")
export_to_bib(fulltext, "fulltext")

# Save intermediary
#save(wos, scopus, ieee, vanderzee, sources, unique_references, deduplicated_sources, titles_screened, selected_abs, fulltext, file="output/LiteratureReview_BeforeFulltext.RData", compress = "xz")

# Extract data with shiny::runApp('browser'), creates browser/saved_progress.RData as intermediary.
# Mark excluded articles with EXCLUDE in "additional_notes" field

# Load data extracted from full-text reads
#extracted_data <- readRDS("browser/saved_progress.RData") # For intermediate
extracted_data <- readr::read_csv("output/ExtractedData-2021-03-19.csv") # For final

# Remove excluded articles
full_text_excluded <- extracted_data[grep("EXCLUDE", extracted_data$additional_notes),]
extracted_data_included <- extracted_data[grep("EXCLUDE", extracted_data$additional_notes, invert = TRUE),]
full_text_wrong_type <- length(grep("E02", full_text_excluded$additional_notes))
full_text_missing <- length(grep("E04", full_text_excluded$additional_notes))
full_text_not_relevant <- length(grep("E05", full_text_excluded$additional_notes))

final_fulltext <- right_join(fulltext, extracted_data_included, by = "label") # Combine the data for further processing
final_fulltext$author[grep("Rijnen", final_fulltext$author)] <- "Rijnen, D. and Rhuggenaath, J. and Costa, P. R. d. O. d. and Zhang, Y."
final_fulltext$author[grep("Vlker", final_fulltext$author)] <- "Völker, S. and Gmilkowsky, P."
final_fulltext$type <- toupper(final_fulltext$type)
final_fulltext <- assign_unique_labels(final_fulltext)

export_to_bib(final_fulltext, "final_fulltext")

# Include snowball and manual searches
manual <- read_bibliography("data/manual.bib") %>%
  mutate(type = "CONFERENCE",
         journal = booktitle)
wsc2020 <- manual %>% filter(grepl("2020 Winter", booktitle)) %>% nrow()
snowball <- nrow(manual) - wsc2020

# Combine database and manual search
included <- full_join(final_fulltext, manual)

# Data to answer RQ1
publication_type <- included %>%
  group_by(type, cut(as.numeric(year), breaks=c(0,2017,2020), labels=c(2017,2020))) %>%
  summarise(n = n())

# Delimited by " and " between authors and ", " between first and last name
allauthors <- str_split(str_split(included$author, " and ") %>% unlist(), ", ", simplify = TRUE)
firstauthors <- str_split(str_split(included$author, " and ", n = 2, simplify = TRUE)[,1], ", ", simplify = TRUE)

mostpublished_first <- data.frame(lastname = firstauthors[,1], firstname = substring(firstauthors[,2], 1, 1)) %>%
  group_by(lastname, firstname) %>%
  count() %>%
  arrange(desc(n))

mostpublished_all <- data.frame(lastname = allauthors[,1], firstname = substring(allauthors[,2], 1, 1)) %>%
  group_by(lastname, firstname) %>%
  count() %>%
  arrange(desc(n))

venues <- included %>%
  mutate(journal = if_else(grepl("Winter Simulation", journal), "Winter Simulation Conference", journal)) %>%
  group_by(journal,
           cut(as.numeric(year), breaks=c(0,2017,2020), labels=c(2017,2020))) %>%
  summarise(n = n()) %>%
  arrange(desc(n), journal)

#keywords <- included$author_keywords & included$keywords

# RQ2
included %>% select(label, simsoftware, apparea) %>% filter(grepl("plant", tolower(simsoftware)))

# Save intermediary
#save(wos, scopus, ieee, vanderzee, sources, unique_references, deduplicated_sources, titles_screened, selected_abs, fulltext, extracted_data, full_text_excluded, extracted_data_included, final_fulltext, manual, included, file="output/LiteratureReview_AfterFulltext.RData", compress = "xz")

# Create .data file (key = value, where value is nrow(df))
prismakeys <- list("scopus" = scopus,
             "wos" = wos,
             "ieee" = ieee,
             "other_included" = vanderzee,
             "total" = sources,
             "duplicates" = deduplicated_sources,
             "excluded_titles" = excluded_titles,
             "excluded_abstracts" = excluded_abstracts,
             "excluded" = excluded_titles + excluded_abstracts,
             "full_text" = fulltext,
             "full_text_excluded" = full_text_excluded,
             "full_text_wrong_type" = full_text_wrong_type,
             "full_text_missing" = full_text_missing,
             "full_text_not_relevant" = full_text_not_relevant,
             "wsc2020" = wsc2020,
             "snowball" = snowball,
             "pre2018" = included %>% filter(year <= 2017) %>% nrow(),
             "after2017" = included %>% filter(year > 2017) %>% nrow(),
             "manual" = wsc2020 + snowball,
             "final" = final_fulltext,
             "included" = nrow(final_fulltext) + nrow(manual),
             "semiconductor" = length(grep("Semi", included$apparea)))

# Create the file
write_lines(
  map2(names(prismakeys),
       prismakeys,
       function(x, y) paste(x, ifelse(is.data.frame(y), nrow(y), y),sep = " = ")),
  file = "output/PrismaFlowChartData.data")

export_to_bib <- function(data, filename) {
  row.names(data) <- data$label
  WriteBib(as.BibEntry(data %>%
                         mutate(bibtype = type,
                                booktitle = if_else(!is.na(journal), journal, note))),
           file = paste0("output/",filename,".bib"))
}

assign_unique_labels <- function(data) {
  data$label <- map2_chr(data$author,
                         data$year,
                         function(x, y)
                           paste0(stringr::str_replace_all(stringr::str_split(x, ",", simplify = TRUE)[1], " ", ""), y))
  num_duplicate <- data %>% group_by(label) %>% summarize(n = n()) %>% filter(n > 1) %>% arrange(desc(n))
  if (nrow(num_duplicate) > 0) {
    # Go through each duplicate label and add a letter
    for (dup in num_duplicate$label) {
      m <- regexpr(dup, data$label)
      regmatches(data$label,m) <- paste0(regmatches(data$label,m), letters[1:length(m)])
    }
  }
  data
}
