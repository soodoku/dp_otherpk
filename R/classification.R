person_keys <- c("poll_id", "source_dataset", "respondent_id")
item_keys <- c("poll_id", "item_id")
axis_contract <- tibble::tribble(
  ~poll_id, ~axis_id, ~measure_id,
  "denmark-euro-2000", "join_euro", "euro_preference",
  "america-in-one-room-2019", "party_alignment", "party_identity",
  "northern-ireland-2007", "religious_alignment", "religious_affiliation"
)

assert_unique <- function(data, keys) {
  if (
    !all(keys %in% names(data)) || anyNA(data[keys]) ||
      anyDuplicated(data[keys])
  ) {
    stop("Missing or duplicate key: ", paste(keys, collapse = ", "))
  }
  invisible(data)
}

check_hash <- function(path, expected) {
  actual <- digest::digest(path, file = TRUE, algo = "sha256")
  if (length(expected) != 1L || !identical(actual, expected)) {
    stop("Checksum mismatch: ", path)
  }
  actual
}

read_codes <- function(path) {
  result <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (is.list(result) && "records" %in% names(result)) {
    result <- result$records
  }
  result <- tibble::as_tibble(result)
  assert_unique(result, item_keys)
  allowed <- c(
    "supports", "challenges", "neutral", "ambiguous", "unknown",
    "not_applicable"
  )
  stopifnot(all(result$fact_implication %in% allowed))
  result
}

compare_codes <- function(a, b, items) {
  for (data in list(a, b, items)) assert_unique(data, item_keys)
  if (
    nrow(a) != nrow(items) || nrow(b) != nrow(items) ||
      nrow(dplyr::anti_join(items, a, by = item_keys)) ||
      nrow(dplyr::anti_join(items, b, by = item_keys))
  ) {
    stop("Coder packet coverage mismatch")
  }
  columns <- c(item_keys, "axis_id", "fact_implication", "evidence_locator")
  result <- dplyr::inner_join(
    a[columns], b[columns],
    by = item_keys, suffix = c("_a", "_b"),
    relationship = "one-to-one"
  )
  stopifnot(identical(result$axis_id_a, result$axis_id_b))
  result$axis_id <- result$axis_id_a
  index <- match(result$poll_id, axis_contract$poll_id)
  if (
    anyNA(index) || any(result$axis_id != axis_contract$axis_id[index])
  ) {
    stop("Coder axis differs from protocol axis contract")
  }
  result$coder_agreement <- result$fact_implication_a ==
    result$fact_implication_b
  result$fact_implication <- ifelse(
    result$coder_agreement, result$fact_implication_a, "ambiguous"
  )
  result$coding_status <- "provisional_model_assisted"
  result$coding_version <- "pilot-2026-10-09-v1"
  result$adjudication_required <- TRUE
  result
}

deduplicate_people <- function(positions) {
  assert_unique(positions, c(person_keys, "measure_id"))
  ni <- positions$poll_id == "northern-ireland-2007"
  event_rows <- ni & positions$source_dataset == "cor_sood"
  event_ids <- positions$respondent_id[event_rows]
  overlap <- ni & positions$source_dataset == "control" &
    positions$respondent_id %in% event_ids
  bridge <- positions[overlap, person_keys]
  bridge$retained_source_dataset <- "cor_sood"
  bridge$reason <- "same_original_cserial_in_event_and_followup_sources"
  for (id in bridge$respondent_id) {
    pair <- positions[ni & positions$respondent_id == id, ]
    stopifnot(nrow(pair) == 2L)
    fields <- c("baseline_group", "source_path", "source_row", "source_sha256")
    stopifnot(all(vapply(
      pair[fields], function(x) length(unique(x)) == 1L,
      logical(1)
    )))
  }
  kept <- positions[!overlap, ]
  assert_unique(kept, person_keys)
  list(people = kept, bridge = bridge)
}

classify_relation <- function(implication, side) {
  stopifnot(length(implication) == length(side))
  directional <- implication %in% c("supports", "challenges") &
    side %in% c("positive", "negative")
  agrees <- (implication == "supports" & side == "positive") |
    (implication == "challenges" & side == "negative")
  relation <- rep("cannot_classify", length(side))
  relation[which(directional & agrees)] <- "congenial"
  relation[which(directional & !agrees)] <- "uncongenial"
  relation
}

build_relations <- function(people, registry) {
  assert_unique(people, person_keys)
  assert_unique(registry, item_keys)
  if (any(!people$poll_id %in% registry$poll_id)) {
    stop("Missing item registry for a person cohort")
  }
  result <- dplyr::inner_join(
    people, registry,
    by = "poll_id", relationship = "many-to-many"
  )
  assert_unique(result, c(person_keys, "item_id"))
  contract_index <- match(result$poll_id, axis_contract$poll_id)
  if (
    anyNA(contract_index) ||
      any(result$measure_id != axis_contract$measure_id[contract_index]) ||
      any(result$axis_id != axis_contract$axis_id[contract_index])
  ) {
    stop("Person measure or item axis differs from protocol contract")
  }
  result$baseline_side <- "unclassified"
  positive_pairs <- c(
    "join_euro:supports", "party_alignment:democrat",
    "religious_alignment:catholic"
  )
  negative_pairs <- c(
    "join_euro:opposes", "party_alignment:republican",
    "religious_alignment:protestant"
  )
  observed_pairs <- paste(result$axis_id, result$baseline_group, sep = ":")
  positive <- observed_pairs %in% positive_pairs
  negative <- observed_pairs %in% negative_pairs
  result$baseline_side[positive] <- "positive"
  result$baseline_side[negative] <- "negative"
  result$classification <- classify_relation(
    result$fact_implication, result$baseline_side
  )
  result$person_classifiable <- positive | negative
  result$item_classifiable <- result$fact_implication %in%
    c("supports", "challenges")
  result$item_reason <- ifelse(result$item_classifiable,
    "directional", paste0("item_", result$fact_implication)
  )
  result$item_reason[!result$coder_agreement] <- "coder_disagreement"
  result$person_reason <- ifelse(result$person_classifiable,
    "mapped_baseline", paste0("baseline_", result$baseline_group)
  )
  result$classification_reason <- ifelse(!result$item_classifiable,
    result$item_reason, result$person_reason
  )
  result$classification_reason[result$classification != "cannot_classify"] <-
    "direction_and_baseline_match"
  result <- result |>
    dplyr::group_by(dplyr::across(dplyr::all_of(person_keys))) |>
    dplyr::mutate(
      n_congenial = sum(classification == "congenial"),
      n_uncongenial = sum(classification == "uncongenial"),
      has_both_categories = n_congenial > 0 & n_uncongenial > 0
    ) |>
    dplyr::ungroup()
  result$candidate_contrast_row <- result$has_both_categories &
    result$classification != "cannot_classify"
  result$measurement_validated <- FALSE
  result$ready_for_estimation <- FALSE
  result
}
