# =============================================================================
# DIAGNOSI + FIX (più veloce, più robusto)
# =============================================================================

rm(list = ls(all.names = TRUE))

library(dplyr)
library(httr2)
library(jsonlite)
library(progress)
library(R.utils)
library(rscopus)
library(stringr)

api_key  <- Sys.getenv("Elsevier_API")
api_key  <- "a1c0b5c5f249c2d168dc5fbbd3ccc066"  # se vuoi forzare qui

affil_id <- 60050715
MIN_YEAR <- 2023
MAX_YEAR <- 2025

if (identical(api_key, "") || is.null(api_key)) {
  stop("API key mancante: imposta Elsevier_API in .Renviron oppure assegna api_key.")
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1]) && x[1] != "") x else y

# -------------------------------------------------------------------------
# STEP 1: TUSCIA AUTHORS (usando rscopus::author_search_by_affil)
# -------------------------------------------------------------------------
get_tuscia_authors <- function(affil_id, api_key, max_results = 5000) {
  # rscopus gestisce la paginazione interna con count/max_count.[web:73][web:79]
  res <- tryCatch({
    author_search_by_affil(
      affil_id = affil_id,
      api_key  = api_key,
      count    = 200,      # per pagina
      verbose  = FALSE
    )
  }, error = function(e) {
    message("Errore author_search_by_affil: ", e$message)
    return(NULL)
  })
  
  if (is.null(res) || is.null(res$entries) || length(res$entries) == 0) {
    message("Nessuna entry trovata per affil_id = ", affil_id)
    return(tibble())
  }
  
  # Converti le entries in data.frame con la funzione ufficiale.[web:79]
  g <- tryCatch({
    gen_entries_to_df(res$entries)
  }, error = function(e) {
    message("gen_entries_to_df() fallita: ", e$message)
    return(NULL)
  })
  
  if (is.null(g) || !"df" %in% names(g)) {
    message("gen_entries_to_df() non ha restituito $df")
    return(tibble())
  }
  
  df <- g$df
  
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    message("Data frame vuoto da gen_entries_to_df()")
    return(tibble())
  }
  
  # Nei vignette rscopus, la colonna ID autore si chiama di solito 'identifier' o simile.[web:79]
  # Controlla i nomi disponibili:
  # print(names(df))
  
  id_col <- c("identifier", "dc.identifier", "dc:identifier")
  id_col <- id_col[id_col %in% names(df)][1]
  
  if (is.na(id_col)) {
    stop("Non trovo la colonna con l'ID autore in df. Controlla names(df).")
  }
  
  # Pulizia dell'ID: rimuove prefissi 'AUTHOR_ID:' o 'AUTHOR-ID(...)'
  df <- df |>
    mutate(
      author_id = str_remove_all(.data[[id_col]], "AUTHOR_ID:|AUTHOR-ID\\(|\\)")
    )
  
  # Nome e cognome preferiti: adattare ai nomi effettivi delle colonne
  name_col    <- intersect(c("preferred_name.given_name", "preferred-name.given-name"), names(df))[1]
  surname_col <- intersect(c("preferred_name.surname", "preferred-name.surname"), names(df))[1]
  
  df <- df |>
    mutate(
      author_name    = if (!is.na(name_col)) .data[[name_col]]    else NA_character_,
      author_surname = if (!is.na(surname_col)) .data[[surname_col]] else NA_character_
    ) |>
    filter(!is.na(author_id), author_id != "") |>
    distinct(author_id, .keep_all = TRUE)
  
  df
}

cat("🔍 STEP 1: Tuscia authors\n")

# Elimina cache se esiste
cache_authors <- "tuscia_authors_cache.rds"
if (file.exists(cache_authors)) {
  file.remove(cache_authors)
  cat("⚠️  Cache eliminata\n")
}

cat("Scarico autori da Scopus API...\n")
tuscia_authors <- get_tuscia_authors(affil_id, api_key, max_results = 5000)

cat("Autori trovati:", nrow(tuscia_authors), "\n")
head(tuscia_authors[, c("author_id", "author_name", "author_surname")], 10)



################################################################################
################################################################################
################################################################################

# Salva nuova cache
if (nrow(tuscia_authors) > 0) {
  saveRDS(tuscia_authors, cache_authors)
  cat("✅ Nuova cache salvata\n")
}

cat("✅ Autori trovati:", nrow(tuscia_authors), "\n")

# Diagnostica se vuoto
if (nrow(tuscia_authors) == 0) {
  stop("❌ Nessun autore trovato! Possibili cause:\n",
       "  1. AF-ID errato (", affil_id, ")\n",
       "  2. API key non valida\n",
       "  3. Quota API esaurita\n",
       "  4. Errore di rete\n",
       "Controlla la risposta API manualmente su:\n",
       "https://api.elsevier.com/content/search/author?query=AF-ID(", affil_id, ")&apiKey=", api_key)
}

test_authors <- head(unique(tuscia_authors$author_id), 3000)
if (length(test_authors) == 0) stop("Nessun author_id trovato.")

cat("✅ Test authors (primi 3):\n")
print(test_authors)


# -------------------------------------------------------------------------
# STEP 2: FAST DOWNLOAD (solo scopus_search, view COMPLETE)
# -------------------------------------------------------------------------
get_author_pubs_fast <- function(author_id, api_key, min_year, max_year,
                                 count = 20000, max_count = 20000,
                                 wait_time = 0.2, timeout = 30) {
  cache_file <- paste0("pubs_", author_id, "_", min_year, "_", max_year, ".rds")
  if (file.exists(cache_file)) return(readRDS(cache_file))
  
  # Scopus query language: PUBYEAR AFT <year> / BEF <year>
  q <- paste0("AU-ID(", author_id, ") AND PUBYEAR AFT ", (min_year - 1),
              " AND PUBYEAR BEF ", (max_year + 1))
  
  res <- tryCatch({
    withTimeout({
      scopus_search(
        query     = q,
        api_key   = api_key,
        view      = "COMPLETE",
        count     = count,
        max_count = max_count,
        wait_time = wait_time,
        verbose   = FALSE
      )
    }, timeout = timeout, onTimeout = "error")
  }, error = function(e) NULL)
  
  if (is.null(res) || is.null(res$entries) || length(res$entries) == 0) return(NULL)
  
  df <- gen_entries_to_df(res$entries)$df
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df$AU_ID <- author_id
  saveRDS(df, cache_file)
  df
}

cat("\n📥 STEP 2: Downloading pubs for", length(test_authors), "authors...\n")
pb <- progress_bar$new(total = length(test_authors),
                       format = "[:bar] :percent | :current/:total (:eta)")

pubs_list <- vector("list", length(test_authors))
for (i in seq_along(test_authors)) {
  pubs_list[[i]] <- get_author_pubs_fast(test_authors[i], api_key, MIN_YEAR, MAX_YEAR)
  pb$tick()
}
pubs_all <- bind_rows(Filter(Negate(is.null), pubs_list))
cat("\n✅ Total publications:", nrow(pubs_all), "\n")

# -------------------------------------------------------------------------
# STEP 3: Add Tuscia name/surname + number of authors per paper
# -------------------------------------------------------------------------
pubs_all <- pubs_all |>
  left_join(tuscia_authors, by = c("AU_ID" = "author_id")) |>
  rename(tuscia_name = author_name, tuscia_surname = author_surname)

# Choose an author string column if present (often "dc:creator" in search results)
creator_col <- grep("dc:creator|creator", names(pubs_all), value = TRUE)[1] %||% NA_character_

pubs_all <- pubs_all |>
  mutate(
    n_authors = if (!is.na(creator_col)) {
      sapply(.data[[creator_col]], function(x) {
        x <- as.character(x)
        if (is.na(x) || x == "") return(NA_integer_)
        # many exports separate authors with comma; some with semicolon
        max(str_count(x, ";") + 1L, str_count(x, ",") + 1L, na.rm = TRUE)
      })
    } else {
      NA_integer_
    }
  )

cols_show <- intersect(
  c(
    "tuscia_name",                 # OK: exists
    "tuscia_surname",              # OK: exists
    "AU_ID",                       # OK: exists
    "dc:title",                    # TI -> dc:title
    "prism:coverDate",            # PY -> cover date
    "prism:doi",                  # DI -> prism:doi
    "author-count.$",             # OK: exists
    "subtypeDescription",         # OK: exists
    "prism:aggregationType",      # OK: exists
    "citedby-count",              # OK: exists
    "prism:publicationName"       # OK: exists
  ),
  names(pubs_all)
)

# If pubs_all is already a data.table
print(head(pubs_all[, ..cols_show]))

# Or, if you prefer to force it to data.frame:
print(head(as.data.frame(pubs_all)[, cols_show, drop = FALSE]))


# ---------------------------------------------------------------------------------------------

# Save
saveRDS(pubs_all, "tuscia_pubs_all.rds")
cat("\n💾 Saved: tuscia_pubs_all.rds\n")



# -----------------------------------------------------------------------------------------------------------------
journals <- pubs_all |>
  dplyr::filter(!is.na(`prism:publicationName`), nzchar(`prism:publicationName`)) |>
  dplyr::distinct(journal_title = `prism:publicationName`)





