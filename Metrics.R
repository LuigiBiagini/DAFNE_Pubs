# =============================================================================
# TUSCIA - Scopus Serial Title API
# Annual CiteScore + Annual Percentile by year (2023-2025)
# (Tracker/current metrics kept separate; NOT used to fill annual values)
# =============================================================================

rm(list = ls(all.names = TRUE))

library(dplyr)
library(httr2)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)

api_key <- Sys.getenv("Elsevier_API")
api_key <- "a1c0b5c5f249c2d168dc5fbbd3ccc066"  # optional hard-code

pubs_path <- "~/DAFNE_Pubblicazioni/tuscia_pubs_all.rds"
out_path  <- "tuscia_journal_metrics_annual_2023_2025.rds"
years_target <- 2023:2025

if (identical(api_key, "") || is.null(api_key)) stop("API key mancante.")

# -----------------------------
# Helpers
# -----------------------------
normalize_issn <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, "\\s+", "")
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x) & x != ""
  needs_dash <- ok & !str_detect(x, "-") & nchar(x) == 8
  out[ok] <- x[ok]
  out[needs_dash] <- paste0(substr(x[needs_dash], 1, 4), "-", substr(x[needs_dash], 5, 8))
  out
}

get_field <- function(x, nm) {
  if (is.null(x) || !(is.list(x) || is.data.frame(x))) return(NA_character_)
  nms <- names(x)
  if (is.null(nms) || !nm %in% nms) return(NA_character_)
  val <- x[[nm]]
  if (is.null(val) || length(val) == 0) NA_character_ else as.character(val[1])
}

as_entry_list <- function(entry) {
  if (is.null(entry) || length(entry) == 0) return(NULL)
  if (is.data.frame(entry)) return(as.list(entry[1, , drop = FALSE]))
  if (is.list(entry)) return(entry[[1]])
  NULL
}

safe_num <- function(z) suppressWarnings(as.numeric(z))

get_citescore_year_rows <- function(x) {
  y <- x[["citeScoreYearInfoList"]][["citeScoreYearInfo"]]
  if (is.null(y)) return(list())
  if (is.data.frame(y)) return(split(y, seq_len(nrow(y))))
  if (is.list(y)) return(y)
  list()
}

# STRICT annual CiteScore extraction: only from year rows (not tracker/current)
# Robust: row$citeScore or any nested "citeScore" element inside the row.
get_annual_citescore_from_year_row <- function(row) {
  if (is.null(row) || !is.list(row)) return(NA_real_)
  
  cs0 <- safe_num(get_field(row, "citeScore"))
  if (!is.na(cs0)) return(cs0)
  
  v <- unlist(row, recursive = TRUE, use.names = TRUE)
  nm <- names(v)
  if (is.null(nm)) return(NA_real_)
  
  keep <- grepl("(^|\\.)citeScore($|\\b)", nm, ignore.case = TRUE)
  if (!any(keep)) return(NA_real_)
  
  cs <- suppressWarnings(as.numeric(v[keep]))
  cs <- cs[!is.na(cs)]
  if (!length(cs)) return(NA_real_)
  
  # If multiple, choose max (should be same; max is safer than "first")
  max(cs)
}

# Annual percentile extraction from year row (robust)
get_annual_percentile_from_year_row <- function(row) {
  if (is.null(row) || !is.list(row)) return(NA_real_)
  v <- unlist(row, recursive = TRUE, use.names = TRUE)
  nm <- names(v)
  if (is.null(nm)) return(NA_real_)
  
  keep <- grepl("percentile", nm, ignore.case = TRUE)
  if (!any(keep)) return(NA_real_)
  
  out <- suppressWarnings(as.numeric(v[keep]))
  out <- out[!is.na(out)]
  if (!length(out)) return(NA_real_)
  
  max(out)
}

# -----------------------------
# Serial Title call (no crash on 4xx/5xx)
# -----------------------------
serial_title_call <- function(issn_use, api_key, view = c("CITESCORE", "ENHANCED")) {
  view <- match.arg(view)
  url  <- paste0("https://api.elsevier.com/content/serial/title/issn/", issn_use)
  
  req <- request(url) |>
    req_headers(
      "X-ELS-APIKey" = api_key,
      "Accept"       = "application/json"
    ) |>
    req_url_query(view = view) |>
    req_error(is_error = \(resp) FALSE)
  
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  
  if (is.null(resp)) {
    return(list(ok = FALSE, status = NA_integer_, error = "Transport error (no response)", x = NULL))
  }
  
  st <- resp_status(resp)
  if (st != 200) {
    return(list(ok = FALSE, status = st, error = paste("HTTP", st, resp_status_desc(resp)), x = NULL))
  }
  
  json <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(json)) return(list(ok = FALSE, status = st, error = "JSON parse error", x = NULL))
  
  sm <- json[["serial-metadata-response"]]
  if (is.null(sm)) return(list(ok = FALSE, status = st, error = "Missing serial-metadata-response", x = NULL))
  
  x <- as_entry_list(sm[["entry"]])
  if (is.null(x)) return(list(ok = FALSE, status = st, error = "Missing entry", x = NULL))
  
  list(ok = TRUE, status = st, error = NA_character_, x = x)
}

# -----------------------------
# Main function
# -----------------------------
get_journal_metrics_annual_2023_2025 <- function(issn, eissn, journal_title, api_key, years_target = 2023:2025) {
  
  keys <- unique(na.omit(c(issn, eissn)))
  if (!length(keys)) return(list(data = NULL, err = NULL))
  
  attempts <- purrr::map(keys, function(k) {
    cs <- serial_title_call(k, api_key, view = "CITESCORE")
    rows <- if (isTRUE(cs$ok)) get_citescore_year_rows(cs$x) else list()
    yrs  <- suppressWarnings(as.integer(vapply(rows, function(r) get_field(r, "@year"), character(1))))
    n_in_window <- sum(!is.na(yrs) & yrs %in% years_target)
    list(k = k, cs = cs, rows = rows, n_in_window = n_in_window)
  })
  
  best <- attempts[[ which.max(vapply(attempts, `[[`, numeric(1), "n_in_window")) ]]
  
  if (!isTRUE(best$cs$ok)) {
    err <- tibble(
      journal_title = journal_title,
      issn_tried    = paste(keys, collapse = ";"),
      status        = best$cs$status,
      error         = best$cs$error
    )
    return(list(data = NULL, err = err))
  }
  
  x_cs <- best$cs$x
  rows <- best$rows
  
  issn_api  <- get_field(x_cs, "prism:issn")
  eissn_api <- get_field(x_cs, "prism:eIssn")
  
  # Keep tracker/current metrics as separate fields (do NOT treat as annual series)
  tracker_val  <- safe_num(get_field(x_cs, "citeScoreYearInfoList.citeScoreTracker"))
  tracker_year <- suppressWarnings(as.integer(get_field(x_cs, "citeScoreYearInfoList.citeScoreTrackerYear")))
  
  current_val  <- safe_num(get_field(x_cs, "citeScoreYearInfoList.citeScoreCurrentMetric"))
  current_year <- suppressWarnings(as.integer(get_field(x_cs, "citeScoreYearInfoList.citeScoreCurrentMetricYear")))
  
  if (!length(rows)) {
    err <- tibble(
      journal_title = journal_title,
      issn_tried    = best$k,
      status        = 200L,
      error         = "No citeScoreYearInfo rows in payload (annual series unavailable)"
    )
    return(list(data = NULL, err = err))
  }
  
  dat <- purrr::map_dfr(rows, function(r) {
    yr <- suppressWarnings(as.integer(get_field(r, "@year")))
    if (is.na(yr) || !(yr %in% years_target)) return(NULL)
    
    tibble(
      journal_title            = journal_title,
      issn_use                 = best$k,
      issn_api                 = issn_api,
      eissn_api                = eissn_api,
      year                     = yr,
      citescore_annual          = get_annual_citescore_from_year_row(r),
      percentile_annual         = get_annual_percentile_from_year_row(r),
      citescore_tracker         = tracker_val,
      citescore_tracker_year    = tracker_year,
      citescore_current_metric  = current_val,
      citescore_current_year    = current_year
    )
  })
  
  if (!nrow(dat)) {
    err <- tibble(
      journal_title = journal_title,
      issn_tried    = best$k,
      status        = 200L,
      error         = "No rows in requested year window (2023-2025)"
    )
    return(list(data = NULL, err = err))
  }
  
  list(data = dat, err = NULL)
}

# =============================================================================
# RUN
# =============================================================================
pubs_all <- readRDS(pubs_path)


journals <- pubs_all |>
  transmute(
    journal_title = `prism:publicationName`,
    issn  = normalize_issn(`prism:issn`),
    eissn = normalize_issn(`prism:eIssn`)
  ) |>
  filter(!is.na(journal_title), nzchar(journal_title)) |>
  filter(!is.na(issn) | !is.na(eissn)) |>
  distinct(journal_title, issn, eissn, .keep_all = TRUE)

res_list <- purrr::map(
  seq_len(nrow(journals)),
  function(i) {
    get_journal_metrics_annual_2023_2025(
      issn          = journals$issn[i],
      eissn         = journals$eissn[i],
      journal_title = journals$journal_title[i],
      api_key       = api_key,
      years_target  = years_target
    )
  },
  .progress = TRUE
)

metrics_long <- bind_rows(purrr::map(res_list, "data"))
errors_df    <- bind_rows(purrr::map(res_list, "err"))

metrics_wide <- if (nrow(metrics_long) > 0) {
  metrics_long |>
    arrange(journal_title, year) |>
    pivot_wider(
      id_cols = c(journal_title, issn_use, issn_api, eissn_api),
      names_from = year,
      values_from = c(citescore_annual, percentile_annual),
      names_glue = "{.value}_{year}"
    )
} else {
  tibble()
}

saveRDS(list(long = metrics_long, wide = metrics_wide, errors = errors_df), out_path)
cat("\n✅ Saved:", out_path, "\n")

# -----------------------------------------------------------------------------------------------------------------
pubs_all$coverDate <- as.Date(pubs_all$`prism:coverDate`)
pubs_all$year <- year(pubs_all$coverDate)

# helper: format eISSN as NNNN-NNNC (8 chars; last may be X)
add_issn_dash <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, "[^0-9Xx]", "")   # keep digits and X
  x <- str_to_upper(x)
  
  ifelse(
    !is.na(x) & nchar(x) == 8,
    paste0(str_sub(x, 1, 4), "-", str_sub(x, 5, 8)),
    NA_character_
  )
}

Pubs_2 <- pubs_all |>
  dplyr::select(
    tuscia_name, tuscia_surname,
    `prism:publicationName`, `prism:issn`, `prism:eIssn`,year,
    `author-count.$`
  ) |>
  mutate(
    eIssn = add_issn_dash(`prism:eIssn`),
    Issn = add_issn_dash(`prism:issn`),
  ) |>
  as.data.frame()





Pubs_2<-as.data.frame(Pubs_2)

metrics_restr<-as.data.frame(metrics_long|>
  dplyr::select(journal_title,issn_use, issn_api, year, citescore_annual, percentile_annual))
merge.data.frame(Pubs_2,metrics_restr, all.x=T,by.x = c("Issn","year"), by.y=c("issn_use","year"))
