# =============================================================================
# TUSCIA - Journal metrics from Scopus Serial Title API
# CiteScore + Percentile by year (2023-2025) with robust error handling
# =============================================================================

rm(list = ls(all.names = TRUE))

library(dplyr)
library(httr2)
library(jsonlite)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)

api_key <- Sys.getenv("Elsevier_API")
api_key <- "a1c0b5c5f249c2d168dc5fbbd3ccc066"  # optional

pubs_path <- "~/DAFNE_Pubblicazioni/tuscia_pubs_all.rds"
out_path  <- "tuscia_journal_metrics_2023_2025_long.rds"
years_target <- 2023:2025

if (identical(api_key, "") || is.null(api_key)) stop("API key mancante.")

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

get_percentile_from_year_row <- function(row) {
  if (is.null(row) || !is.list(row)) return(NA_real_)
  v <- unlist(row, recursive = TRUE, use.names = TRUE)
  nm <- names(v)
  keep <- grepl("percentile", nm, ignore.case = TRUE)
  if (!any(keep)) return(NA_real_)
  out <- suppressWarnings(as.numeric(v[keep]))
  out <- out[!is.na(out)]
  if (!length(out)) return(NA_real_)
  max(out)
}

# ---- Serial Title call with status capture (404 is expected for some ISSNs) ----
serial_title_call <- function(issn_use, api_key, view = c("CITESCORE", "ENHANCED")) {
  view <- match.arg(view)
  url  <- paste0("https://api.elsevier.com/content/serial/title/issn/", issn_use)
  
  req <- request(url) |>
    req_headers(
      "X-ELS-APIKey" = api_key,
      "Accept"       = "application/json"
    ) |>
    req_url_query(view = view) |>
    # IMPORTANT: don't turn 4xx/5xx into R errors; keep response object
    req_error(is_error = \(resp) FALSE)  # [web:72]
  
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  
  if (is.null(resp)) {
    return(list(ok = FALSE, status = NA_integer_, error = "Transport error (no response)", x = NULL))
  }
  
  st <- resp_status(resp)  # safe now because resp is httr2_response [web:56]
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


# ---- Return BOTH: data rows + an error row (if failed) ----
get_journal_citescore_percentile_2023_2025 <- function(issn, eissn, journal_title, api_key, years_target = 2023:2025) {
  
  keys <- unique(na.omit(c(issn, eissn)))
  if (!length(keys)) return(list(data = NULL, err = NULL))
  
  attempts <- purrr::map(keys, function(k) {
    cs <- serial_title_call(k, api_key, view = "CITESCORE")
    rows <- if (isTRUE(cs$ok)) get_citescore_year_rows(cs$x) else list()
    yrs  <- suppressWarnings(as.integer(vapply(rows, function(r) get_field(r, "@year"), character(1))))
    n_in_window <- sum(!is.na(yrs) & yrs %in% years_target)
    
    list(k = k, cs = cs, rows = rows, n_in_window = n_in_window)
  })
  
  # Choose key with most rows in the requested window; if all 0, still keep the "best"
  best <- attempts[[ which.max(vapply(attempts, `[[`, numeric(1), "n_in_window")) ]]
  
  # If API failed (e.g., 404 on both ISSN and eISSN), return an error record
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
  
  if (!length(rows)) {
    err <- tibble(
      journal_title = journal_title,
      issn_tried    = best$k,
      status        = 200L,
      error         = "No citeScoreYearInfo rows in payload"
    )
    return(list(data = NULL, err = err))
  }
  
  issn_api  <- get_field(x_cs, "prism:issn")
  eissn_api <- get_field(x_cs, "prism:eIssn")
  
  dat <- purrr::map_dfr(rows, function(r) {
    yr <- suppressWarnings(as.integer(get_field(r, "@year")))
    if (is.na(yr) || !(yr %in% years_target)) return(NULL)
    
    tibble(
      journal_title = journal_title,
      issn_use      = best$k,
      issn_api      = issn_api,
      eissn_api     = eissn_api,
      year          = yr,
      citescore     = safe_num(get_field(r, "citeScore")),
      percentile    = get_percentile_from_year_row(r)
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
    get_journal_citescore_percentile_2023_2025(
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

# Guard: pivot only if we actually have rows
if (nrow(metrics_long) > 0) {
  metrics_wide <- metrics_long |>
    arrange(journal_title, year) |>
    pivot_wider(
      id_cols = c(journal_title, issn_use, issn_api, eissn_api),
      names_from = year,
      values_from = c(citescore, percentile),
      names_glue = "{.value}_{year}"
    )
} else {
  metrics_wide <- tibble()
  message("⚠️ metrics_long is empty (all calls failed or no data for 2023-2025). See errors_df.")
}

saveRDS(list(long = metrics_long, wide = metrics_wide, errors = errors_df), out_path)
cat("\n✅ Saved:", out_path, "\n")


