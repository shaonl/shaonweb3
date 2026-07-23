# update_publications.R (v2)
# Pulls publications from ORCID, then uses each DOI to fetch accurate
# author/journal/year data from Crossref (the official source publishers
# report to). Writes everything to publications.bib for Quarto.
#
# Requires environment variables:
#   ORCID_CLIENT_ID
#   ORCID_CLIENT_SECRET

required_pkgs <- c("rorcid", "purrr", "dplyr", "stringr", "httr", "jsonlite")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(rorcid)
library(purrr)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)

my_orcid <- "0000-0001-9797-9566"

# ---- Authenticate with ORCID ----
orcid_auth()

# ---- Pull your works from ORCID ----
works <- orcid_works(my_orcid)
work_df <- works[[my_orcid]]$works

if (is.null(work_df) || nrow(work_df) == 0) {
  stop("No works returned from ORCID. Check your ORCID iD and credentials.")
}

safe_get <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
}

titles <- safe_get(work_df, "title.title.value")
types  <- safe_get(work_df, "type")

map_type <- function(t) {
  t <- tolower(as.character(t))
  case_when(
    str_detect(t, "book")                     ~ "textbook",
    str_detect(t, "journal-article")          ~ "articles",
    str_detect(t, "encyclopedia|dictionary")  ~ "encyclopedia",
    str_detect(t, "working-paper|preprint")   ~ "working_papers",
    TRUE                                      ~ "articles"
  )
}
categories <- map_type(types)

# ---- Extract ONLY the DOI (not other ID types like ISSN/ISBN) ----
extract_doi <- function(row_external_ids) {
  if (is.null(row_external_ids) || length(row_external_ids) == 0) return(NA_character_)
  tryCatch({
    ids <- row_external_ids
    doi_row <- ids[tolower(ids[["external-id-type"]]) == "doi", ]
    if (nrow(doi_row) == 0) return(NA_character_)
    doi_row[["external-id-value"]][1]
  }, error = function(e) NA_character_)
}

# external-ids column is often a list-column of data frames, one per work
if ("external-ids.external-id" %in% names(work_df)) {
  dois <- map_chr(work_df[["external-ids.external-id"]], extract_doi)
} else {
  dois <- rep(NA_character_, nrow(work_df))
}

# ---- Look up accurate metadata from Crossref for each DOI ----
lookup_crossref <- function(doi) {
  if (is.na(doi) || doi == "") return(NULL)
  url <- paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE))
  resp <- tryCatch(
    GET(url, timeout(10), add_headers(`User-Agent` = "personal-website-script (mailto:lahiri2@illinois.edu)")),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  parsed <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  msg <- parsed$message
  
  authors <- "Lahiri, Shaon"  # fallback if Crossref has no author list
  if (!is.null(msg$author) && nrow(msg$author) > 0) {
    authors <- paste(
      paste0(msg$author$family, ", ", msg$author$given),
      collapse = " and "
    )
  }
  
  list(
    title   = if (!is.null(msg$title)) msg$title[[1]] else NA,
    authors = authors,
    year    = if (!is.null(msg$`published-print`$`date-parts`)) {
      msg$`published-print`$`date-parts`[[1]][1]
    } else if (!is.null(msg$published$`date-parts`)) {
      msg$published$`date-parts`[[1]][1]
    } else NA,
    journal = if (!is.null(msg$`container-title`) && length(msg$`container-title`) > 0) {
      msg$`container-title`[[1]]
    } else NA
  )
}

cat("Looking up", sum(!is.na(dois)), "DOIs on Crossref (this may take a minute)...\n")
crossref_data <- map(dois, function(d) {
  Sys.sleep(0.5)  # brief pause between requests so Crossref doesn't throttle us
  lookup_crossref(d)
})

# ---- Build final fields, preferring Crossref data when available, falling back to ORCID ----
final_title <- map2_chr(titles, crossref_data, function(x, y) {
  if (!is.null(y) && !is.na(y$title)) y$title else x
})

final_authors <- map_chr(crossref_data, function(x) {
  if (!is.null(x)) x$authors else "Lahiri, Shaon"
})

final_year <- map_chr(crossref_data, function(x) {
  if (!is.null(x) && !is.na(x$year)) as.character(x$year) else "n.d."
})

final_journal <- map_chr(crossref_data, function(x) {
  if (!is.null(x) && !is.na(x$journal)) x$journal else NA_character_
})

# Flag entries where no DOI was found / Crossref lookup failed, so you can spot-check them
missing_flags <- map_lgl(crossref_data, is.null)
if (any(missing_flags)) {
  cat("\nNote: could not find Crossref data for", sum(missing_flags), "entries — these will list only 'Lahiri, Shaon' as author. Titles:\n")
  print(titles[missing_flags])
}

# Only keep journal articles for the automated bibliography
keep <- categories == "articles"

titles <- titles[keep]
final_title <- final_title[keep]
final_authors <- final_authors[keep]
final_year <- final_year[keep]
final_journal <- final_journal[keep]
categories <- categories[keep]
dois <- dois[keep]

make_key <- function(year, title) {
  first_word <- str_extract(tolower(title), "^[a-z]+")
  paste0("lahiri", year, first_word)
}
keys <- make_key(final_year, final_title)

bib_entry <- function(key, title, author, year, journal, category, doi) {
  paste0(
    "@article{", key, ",\n",
    "  title    = {", ifelse(is.na(title), "Untitled", title), "},\n",
    "  author   = {", author, "},\n",
    "  year     = {", year, "},\n",
    if (!is.na(journal)) paste0("  journal  = {", journal, "},\n") else "",
    if (!is.na(doi)) paste0("  doi      = {", doi, "},\n") else "",
    "  keywords = {", category, "}\n",
    "}\n"
  )
}

# Sort everything by year, most recent first, before writing
sort_order <- order(final_year, decreasing = TRUE)

entries <- pmap_chr(
  list(
    keys[sort_order], final_title[sort_order], final_authors[sort_order],
    final_year[sort_order], final_journal[sort_order],
    categories[sort_order], dois[sort_order]
  ),
  bib_entry
)

writeLines(entries, "publications.bib")

writeLines(entries, "publications.bib")
cat("\nWrote", length(entries), "entries to publications.bib\n")