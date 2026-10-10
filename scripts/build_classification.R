source("R/classification.R")
source_root <- Sys.getenv("DP_DATA", "../dp_data")
directory <- "output/classification"
dir.create(directory, recursive = TRUE, showWarnings = FALSE)

baseline_dir <- file.path(source_root, "output", "baseline_positions")
baseline_manifest <- jsonlite::read_json(
  file.path(baseline_dir, "manifest.json"),
  simplifyVector = TRUE
)
baseline_path <- file.path(baseline_dir, "baseline_positions.parquet")
baseline_hash <- check_hash(baseline_path, baseline_manifest$output_sha256)
audit <- jsonlite::read_json("metadata/input_audit.json", simplifyVector = TRUE)
person_spec <- audit$inputs$analysis_phase_participants
person_path <- file.path(source_root, person_spec$path)
person_hash <- check_hash(person_path, person_spec$sha256)
stopifnot(identical(person_hash, baseline_manifest$participant_sha256))
item_spec <- audit$inputs$analysis_items
item_hash <- check_hash(
  file.path(source_root, item_spec$path), item_spec$sha256
)
items <- arrow::read_parquet(file.path(source_root, item_spec$path))
packet <- jsonlite::read_json(
  "coding/packets/pilot.json",
  simplifyVector = TRUE
)
packet_axes <- vapply(packet$axes, function(axis) axis$axis_id, character(1))
stopifnot(identical(
  unname(packet_axes[axis_contract$poll_id]), axis_contract$axis_id
))
pilot_items <- tibble::as_tibble(packet$items)
assert_unique(pilot_items, item_keys)
current <- dplyr::semi_join(items, pilot_items, by = item_keys)
current <- current[order(current$poll_id, current$item_id), names(pilot_items)]
pilot_items <- pilot_items[order(pilot_items$poll_id, pilot_items$item_id), ]
for (column in names(pilot_items)) {
  if (is.character(current[[column]])) {
    pilot_items[[column]] <- as.character(pilot_items[[column]])
  }
}
stopifnot(isTRUE(all.equal(current, pilot_items, check.attributes = FALSE)))

positions <- arrow::read_parquet(baseline_path)
stopifnot(nrow(positions) == baseline_manifest$rows)
person_columns <- c(
  person_keys, "study_id", "attended", "attendance_status", "arm"
)
participants <- arrow::read_parquet(
  person_path,
  col_select = dplyr::all_of(person_columns)
)
assert_unique(participants, person_keys)
unmatched <- dplyr::anti_join(positions, participants, by = person_keys)
stopifnot(nrow(unmatched) == 0L)
positions <- dplyr::left_join(positions, participants,
  by = person_keys,
  relationship = "one-to-one"
)
universe <- deduplicate_people(positions)
codes_a <- read_codes("coding/independent/coder_a.json")
codes_b <- read_codes("coding/independent/coder_b.json")
registry <- compare_codes(codes_a, codes_b, pilot_items)
registry <- dplyr::left_join(registry, pilot_items,
  by = item_keys,
  relationship = "one-to-one"
)
relations <- build_relations(universe$people, registry)
target <- file.path(directory, "respondent_items.parquet")
arrow::write_parquet(relations, target)
stopifnot(identical(relations, arrow::read_parquet(target)))
readr::write_csv(universe$bridge, file.path(directory, "identity_bridge.csv"))
readr::write_csv(registry, file.path(directory, "item_registry.csv"))
readr::write_csv(dplyr::count(
  relations, poll_id, classification,
  classification_reason
), file.path(directory, "classification_coverage.csv"))
people <- dplyr::distinct(relations, dplyr::across(dplyr::all_of(c(
  person_keys, "baseline_group", "n_congenial", "n_uncongenial",
  "has_both_categories", "attendance_status", "ready_for_estimation"
))))
readr::write_csv(dplyr::count(
  people, poll_id, baseline_group,
  n_congenial, n_uncongenial, has_both_categories, attendance_status,
  ready_for_estimation
), file.path(directory, "person_coverage.csv"))
confusion <- with(registry, table(fact_implication_a, fact_implication_b))
readr::write_csv(
  as.data.frame(confusion), file.path(directory, "agreement.csv")
)
levels_all <- sort(unique(c(
  registry$fact_implication_a,
  registry$fact_implication_b
)))
ta <- table(factor(registry$fact_implication_a, levels_all))
tb <- table(factor(registry$fact_implication_b, levels_all))
observed <- mean(registry$coder_agreement)
expected <- sum(ta * tb) / nrow(registry)^2
coding_paths <- c(
  "coding/packets/pilot.json",
  "coding/independent/coder_a.json", "coding/independent/coder_b.json",
  "coding/independent/coder_a_notes.txt",
  "coding/independent/coder_b_notes.txt",
  "R/classification.R", "scripts/build_classification.R"
)
jsonlite::write_json(list(
  classification_version = "pilot-2026-10-09-v1",
  status = "provisional_model_assisted_not_estimation_ready",
  source_participants_sha256 = person_hash,
  source_items_sha256 = item_hash,
  baseline_positions_sha256 = baseline_hash,
  output_sha256 = digest::digest(target, file = TRUE, algo = "sha256"),
  inputs = lapply(coding_paths, function(path) {
    list(
      path = path, sha256 = digest::digest(path, file = TRUE, algo = "sha256")
    )
  }),
  baseline_source_records = nrow(positions),
  duplicate_representations_removed = nrow(universe$bridge),
  people = nrow(people), items = nrow(registry), rows = nrow(relations),
  item_agreement = observed,
  cohen_kappa = (observed - expected) / (1 - expected),
  agreement_interpretation = paste(
    "Descriptive agreement on this purposive pilot, not accuracy or",
    "population reliability; one coder accessed NI aggregate results."
  ),
  knowledge_responses_used = FALSE,
  estimation_ready_rows = sum(relations$ready_for_estimation)
), file.path(directory, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
message("Wrote ", nrow(relations), " provisional respondent-item records")
