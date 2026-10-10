source("R/classification.R")
root <- Sys.getenv("DP_DATA", "../dp_data")
out <- "output/comparative"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
manifest <- readr::read_csv(file.path(root, "output/analysis/manifest.csv"),
  show_col_types = FALSE
)
inputs <- new.env(parent = emptyenv())
read_input <- function(name) {
  entry <- manifest[manifest$table == name, ]
  stopifnot(nrow(entry) == 1L)
  path <- file.path(root, entry$path)
  inputs[[name]] <- check_hash(path, entry$sha256)
  arrow::read_parquet(path)
}
people <- read_input("analysis_phase_participants")
items <- read_input("analysis_items")
registry <- readr::read_csv("coding/adjudicated.csv", show_col_types = FALSE)
assert_unique(registry, c("poll_id", "item_id", "construct"))
stopifnot(all(registry$direction %in% c("supports", "challenges")))
source_manifest <- readr::read_csv(file.path(root, "metadata/source_files.csv"),
  show_col_types = FALSE
)
source_hash <- function(path) {
  expected <- unique(source_manifest$sha256[source_manifest$path == path])
  stopifnot(length(expected) == 1L)
  actual <- check_hash(file.path(root, path), expected)
  inputs[[path]] <- actual
  actual
}
parse_codes <- function(x) as.numeric(strsplit(x, "\\|", fixed = FALSE)[[1]])
all_rows <- list()
for (poll in unique(registry$poll_id)) {
  mapping <- registry[registry$poll_id == poll, ]
  namespace <- unique(mapping$source_dataset)
  stopifnot(length(namespace) == 1L)
  keep <- people$poll_id == poll &
    people$source_dataset == namespace &
    people$attendance_status == "attended"
  selected <- people[keep, ]
  assert_unique(selected, person_keys)
  path <- unique(mapping$source_path)
  stopifnot(length(path) == 1L)
  source_hash(path)
  if (grepl("\\.parquet$", path)) {
    raw <- arrow::read_parquet(file.path(root, path),
      col_select = unique(c(
        "source_row", mapping$id_column, mapping$source_column
      ))
    )
  } else if (grepl("\\.sav$", path)) {
    raw <- haven::read_sav(file.path(root, path))
    raw$source_row <- seq_len(nrow(raw))
  } else if (grepl("\\.dta$", path)) {
    raw <- haven::read_dta(file.path(root, path))
    raw$source_row <- seq_len(nrow(raw))
  } else if (grepl("\\.csv$", path)) {
    raw <- readr::read_csv(file.path(root, path),
      col_select = unique(c(mapping$id_column, mapping$source_column, "Time")),
      show_col_types = FALSE
    )
    raw$source_row <- seq_len(nrow(raw))
  } else {
    raw <- readr::read_tsv(file.path(root, path),
      col_select = unique(mapping$source_column),
      col_types = readr::cols(.default = readr::col_double()),
      show_col_types = FALSE
    )
    raw$source_row <- seq_len(nrow(raw))
  }
  assert_unique(raw, "source_row")
  index <- match(selected$source_row, raw$source_row)
  stopifnot(!anyNA(index))
  id_column <- unique(mapping$id_column)
  stopifnot(length(id_column) == 1L)
  raw_id <- raw[[id_column]][index]
  if (is.numeric(raw_id)) raw_id <- round(raw_id)
  stopifnot(identical(as.character(raw_id), selected$respondent_id))
  if (poll == "amr-2024") stopifnot(all(raw$Time[index] == 0))
  for (j in seq_len(nrow(mapping))) {
    m <- mapping[j, ]
    source_hash(m$dictionary_path)
    value <- as.numeric(raw[[m$source_column]][index])
    positive <- value %in% parse_codes(m$positive_codes)
    negative <- value %in% parse_codes(m$negative_codes)
    stopifnot(!any(positive & negative))
    side <- ifelse(positive, "positive", ifelse(negative, "negative", NA))
    relation <- classify_relation(rep(m$direction, length(side)), side)
    relation[is.na(side)] <- "cannot_classify"
    row <- selected[c(person_keys, "source_row", "small_group_id")]
    row$item_id <- m$item_id
    row$construct <- m$construct
    row$axis <- m$axis
    row$source_column <- m$source_column
    row$baseline_value <- value
    row$baseline_side <- side
    row$relation <- relation
    row$classification_reason <- ifelse(is.na(value), "baseline_missing",
      ifelse(is.na(side), "midpoint_or_nonresponse", "source_coded")
    )
    row$specification <- m$specification
    all_rows[[paste(poll, m$item_id, m$construct)]] <- row
  }
  keep <- items$poll_id == poll &
    !items$item_id %in% mapping$item_id[mapping$construct == "policy"]
  unmapped <- items[keep, ]
  for (item in unmapped$item_id) {
    row <- selected[c(person_keys, "source_row", "small_group_id")]
    row$item_id <- item
    row$construct <- "policy"
    row$axis <- row$source_column <- NA_character_
    row$baseline_value <- NA_real_
    row$baseline_side <- NA_character_
    row$relation <- "cannot_classify"
    row$classification_reason <- "item_not_adjudicated"
    row$specification <- "unclassified"
    all_rows[[paste(poll, item, "policy")]] <- row
  }
}
rows <- dplyr::bind_rows(all_rows)
assert_unique(rows, c(person_keys, "item_id", "construct"))
arrow::write_parquet(rows, file.path(out, "respondent_items.parquet"))
readr::write_csv(dplyr::count(
  rows, poll_id, construct, item_id,
  relation, classification_reason
), file.path(out, "classification_coverage.csv"))
jsonlite::write_json(
  list(
    inputs = as.list(inputs),
    coding_sha256 = digest::digest("coding/adjudicated.csv",
      file = TRUE, algo = "sha256"
    ),
    initial_coding_before_comparative_outcome_inspection = TRUE,
    post_outcome_source_correction = paste(
      "Vermont flawed item restricted to expanded sensitivity;",
      "see coding/adjudication.json"
    ),
    baseline_join = paste(
      "Canonical source_row joins original survey source_row within one",
      "poll and one source_dataset; historical source_people mapping",
      "and cor_sood respondents preserve original rows."
    ),
    classification_is_model_assisted = TRUE,
    human_validated = FALSE,
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ), file.path(out, "classification_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)
