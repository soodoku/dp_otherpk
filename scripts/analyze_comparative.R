source("R/classification.R")
source("R/analysis.R")
source("R/comparative.R")
root <- Sys.getenv("DP_DATA", "../dp_data")
out <- "output/comparative"
registry <- readr::read_csv("coding/adjudicated.csv", show_col_types = FALSE)
frozen <- jsonlite::read_json(file.path(out, "classification_manifest.json"))
check_hash("coding/adjudicated.csv", frozen$coding_sha256)
rows <- arrow::read_parquet(file.path(out, "respondent_items.parquet"))
manifest <- readr::read_csv(file.path(root, "output/analysis/manifest.csv"),
  show_col_types = FALSE
)
entry <- manifest[manifest$table == "analysis_phase_item_responses", ]
path <- file.path(root, entry$path)
response_hash <- check_hash(path, entry$sha256)
responses <- arrow::read_parquet(path)
keep <- responses$poll_id %in% registry$poll_id &
  responses$wave %in% c("t0", "t2")
responses <- responses[keep, ]
assert_unique(responses, c(person_keys, "item_id", "wave"))
results <- item_results <- supports <- flows <- diagnostics <- list()
paired_outputs <- list()
for (rule in c("explicit_dk", "canonical", "substantive_only")) {
  responses$score <- analysis_score(responses, rule)
  keys <- c(person_keys, "item_id")
  before <- responses[responses$wave == "t0", c(keys, "score")]
  after <- responses[responses$wave == "t2", c(keys, "score")]
  names(before)[names(before) == "score"] <- "pre"
  names(after)[names(after) == "score"] <- "post"
  paired <- dplyr::left_join(rows, before,
    by = keys, relationship = "many-to-one"
  ) |>
    dplyr::left_join(after, by = keys, relationship = "many-to-one")
  stopifnot(nrow(paired) == nrow(rows))
  paired$paired <- !is.na(paired$pre) & !is.na(paired$post)
  if (rule == "explicit_dk") paired_outputs[[rule]] <- paired
  for (construct in unique(paired$construct)) {
    for (poll in unique(paired$poll_id[paired$construct == construct])) {
      for (spec in c("primary", "expanded")) {
        keep <- paired$poll_id == poll & paired$construct == construct
        selected <- paired[keep, ]
        if (spec == "primary") {
          selected <- selected[selected$specification == "primary", ]
        }
        if (!nrow(selected)) next
        tag <- function(x) {
          x$poll_id <- rep(poll, nrow(x))
          x$construct <- rep(construct, nrow(x))
          x$specification <- rep(spec, nrow(x))
          x$missing_rule <- rep(rule, nrow(x))
          x
        }
        key <- paste(poll, construct, spec, rule, sep = ":")
        flows[[key]] <- tag(dplyr::count(selected, relation, paired))
        keep <- selected$paired & selected$relation != "cannot_classify"
        usable <- selected[keep, ]
        usable$cluster_id <- usable$small_group_id
        complete_groups <- nrow(usable) && !anyNA(usable$cluster_id) &&
          length(unique(usable$cluster_id)) >= 2L
        mode <- if (complete_groups) {
          "cluster"
        } else {
          "respondent"
        }
        fit <- comparative_estimates(usable,
          draws = 5000L, seed = 20261010L, resample = mode
        )
        fit$diagnostics$partial_cluster_coverage <-
          anyNA(usable$cluster_id) && any(!is.na(usable$cluster_id))
        fit$diagnostics$missing_cluster_respondents <- length(unique(
          usable$respondent_id[is.na(usable$cluster_id)]
        ))
        results[[key]] <- tag(fit$estimates)
        item_results[[key]] <- tag(fit$item_estimates)
        supports[[key]] <- tag(fit$support)
        diagnostics[[key]] <- tag(as.data.frame(fit$diagnostics))
      }
    }
  }
}
tables <- list(
  estimates = results, item_estimates = item_results,
  support = supports, sample_flow = flows, diagnostics = diagnostics
)
for (name in names(tables)) {
  readr::write_csv(
    dplyr::bind_rows(tables[[name]]),
    file.path(out, paste0(name, ".csv"))
  )
}
arrow::write_parquet(
  dplyr::bind_rows(paired_outputs),
  file.path(out, "paired_respondent_items.parquet")
)
jsonlite::write_json(list(
  coding_sha256 = frozen$coding_sha256,
  response_input_sha256 = response_hash,
  seed = 20261010L, bootstrap_draws = 5000L,
  comparison = "Equal-item mean of paired-time group means; U minus C",
  knowledge_gap = "Equal-item C minus U accuracy, before and after",
  same_people_in_both_categories_required = FALSE,
  causal = FALSE,
  code_sha256 = digest::digest("R/comparative.R", file = TRUE, algo = "sha256"),
  orchestration_sha256 = digest::digest("scripts/analyze_comparative.R",
    file = TRUE, algo = "sha256"
  )
), file.path(out, "analysis_manifest.json"), pretty = TRUE, auto_unbox = TRUE)
