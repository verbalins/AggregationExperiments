library(litsearchr)

rakedkeywords <- litsearchr::extract_terms(
  text = paste(included$title, included$abstract),
  method = "fakerake",
  min_freq = 2,
  ngrams = TRUE,
  min_n = 2,
  language = "English"
)

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = paste(included$keywords, included$author_keywords, included$keywords_plus),
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 1,
    language = "English"
  )

all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(included$title, included$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .80,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 20)

strengths <- igraph::strength(reducedgraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

cutoff_fig <- ggplot2::ggplot(term_strengths, ggplot2::aes(x=rank, y=strength, label=term)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

cutoff_change <- find_cutoff(reducedgraph, method="changepoint", knot_num=3)

cutoff_change

cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

# library(ggraph)
# ggraph(naivegraph, layout="stress") +
#   coord_fixed() +
#   expand_limits(x=c(-3, 3)) +
#   geom_edge_link(aes(alpha=weight)) +
#   geom_node_point(shape="circle filled", fill="white") +
#   geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
#   guides(edge_alpha=FALSE)
