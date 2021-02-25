library(revtools)
library(tidyverse)
library(fulltext)
library(RefManageR)

wos <- read_bibliography("data/bib/wos-search.bib") %>% filter(year > 2017)
scopus <- read_bibliography("data/bib/scopus-search.bib") %>% filter(year > 2017)
ieee <- read_bibliography("data/bib/ieee-search.csv") %>% filter(publication_year > 2017)

# Import known references and apply some fixes
vanderzee <- read_bibliography("data/bib/vanderzee.bib") %>%
  mutate(title = str_to_title(title),
         year = as.character(year),
         filename="data/bib//vanderzee.bib")

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
excluded_abstracts <- selected_abs %>% dplyr::filter(screened_abstracts == "excluded") %>% nrow()

fulltext <- selected_abs %>% dplyr::filter(screened_abstracts == "selected")
row.names(fulltext) <- fulltext$label
WriteBib(as.BibEntry(fulltext %>%
                       mutate(bibtype = type,
                              booktitle = if_else(!is.na(journal), journal, note))),
         file = "LiteratureReview.bib")

# Create .data file (key = value, where value is nrow(df))
refs <- list("scopus" = scopus,
             "wos" = wos,
             "ieee" = ieee,
             "other_included" = vanderzee,
             "total" = sources,
             "duplicates" = deduplicated_sources,
             "excluded_titles" = excluded_titles,
             "excluded_abstracts" = excluded_abstracts,
             "excluded" = excluded_titles+excluded_abstracts,
             "full_text" = fulltext,
             "full_text_excluded" = 0,
             "final" = 0)

# Create the file
write_lines(
  map2(names(refs),
       refs,
       function(x, y) paste(x, ifelse(is.data.frame(y), nrow(y), y),sep = " = ")),
  file = "PrismaFlowChartData.data")
