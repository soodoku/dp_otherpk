source("R/classification.R")
source("R/analysis.R")
root <- Sys.getenv("DP_DATA", "../dp_data")
out <- "output/analysis"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
manifest <- readr::read_csv(file.path(root, "output/analysis/manifest.csv"),
  show_col_types = FALSE
)
path <- file.path(
  root,
  "output/analysis/analysis_phase_item_responses.parquet"
)
input_hash <- check_hash(
  path,
  manifest$sha256[manifest$table == "analysis_phase_item_responses"]
)
responses <- arrow::read_parquet(path)
selected <- responses$poll_id == "denmark-euro-2000" &
  responses$source_dataset == "cor_sood"
responses <- responses[selected, ]
baseline_manifest <- jsonlite::read_json(file.path(
  root,
  "output/baseline_positions/manifest.json"
))
baseline_path <- file.path(
  root,
  "output/baseline_positions/baseline_positions.parquet"
)
baseline_hash <- check_hash(baseline_path, baseline_manifest$output_sha256)
baseline <- arrow::read_parquet(baseline_path)
baseline <- baseline[baseline$poll_id == "denmark-euro-2000", ]
people <- baseline[
  baseline$baseline_group %in% c("supports", "opposes"),
  c(person_keys, "baseline_group")
]
assert_unique(people, person_keys)
map <- function(ids, directions) {
  tibble::tibble(
    item_id = sprintf("knowledge_%03d", ids), direction = directions
  )
}
mappings <- list(
  common_core = map(c(2, 3), c("challenges", "supports")),
  hansen_subset = map(
    c(2, 3, 4, 5),
    c("challenges", "supports", "supports", "challenges")
  ),
  hansen_without_bank = map(
    c(2, 3, 4),
    c("challenges", "supports", "supports")
  ),
  pilot_agreement = map(
    c(1, 2, 3, 6),
    c("challenges", "challenges", "supports", "supports")
  )
)
mapping_table <- dplyr::bind_rows(mappings, .id = "specification")
readr::write_csv(mapping_table, file.path(out, "mappings.csv"))
all_results <- all_flow <- all_items <- all_groups <- all_ceiling <- list()
all_transitions <- all_loo <- list()
for (specification in names(mappings)) {
  for (pre in c("t0", "t1")) {
    for (rule in c("explicit_dk", "canonical", "substantive_only")) {
      key <- paste(specification, pre, rule, sep = ":")
      pairs <- make_pairs(
        responses, people, mappings[[specification]], pre, rule
      )
      scores <- person_scores(pairs)
      results <- learning_estimates(scores)
      tag <- function(x) {
        tag_analysis(x, specification, pre, rule)
      }
      all_results[[key]] <- tag(results)
      flow <- dplyr::distinct(
        pairs, respondent_id, baseline_group,
        complete_battery
      ) |>
        dplyr::count(baseline_group, complete_battery)
      all_flow[[key]] <- tag(flow)
      kept <- pairs[pairs$complete_battery, ]
      all_items[[key]] <- tag(kept |>
        dplyr::group_by(item_id, baseline_group, relation) |>
        dplyr::summarise(
          n = dplyr::n(), pre = 100 * mean(score_pre),
          post = 100 * mean(score_post),
          gain = 100 * mean(score_post - score_pre), .groups = "drop"
        ))
      all_groups[[key]] <- tag(scores |>
        dplyr::group_by(baseline_group) |>
        dplyr::summarise(
          n = dplyr::n(),
          dplyr::across(c(
            pre_c, post_c, gain_c, pre_u, post_u, gain_u,
            difference
          ), function(x) 100 * mean(x)), .groups = "drop"
        ))
      all_ceiling[[key]] <- tag(ceiling_diagnostic(pairs))
      all_transitions[[key]] <- tag(dplyr::count(
        kept, baseline_group,
        relation, knowledge_response_pre, knowledge_response_post
      ))
      if (rule == "explicit_dk") {
        arrow::write_parquet(pairs, file.path(out, paste0(
          specification, "_", pre, "_pairs.parquet"
        )))
        for (dropped in mappings[[specification]]$item_id) {
          reduced <- mappings[[specification]]
          reduced <- reduced[reduced$item_id != dropped, ]
          if (length(unique(reduced$direction)) < 2L) next
          p <- make_pairs(responses, people, reduced, pre, rule)
          s <- person_scores(p)
          loo <- learning_estimates(s, draws = 2000L)
          loo <- loo[loo$metric == "difference", ]
          loo$dropped_item <- dropped
          all_loo[[paste(key, dropped)]] <- tag(loo)
        }
      }
    }
  }
}
tables <- list(
  estimates = all_results, sample_flow = all_flow,
  item_gains = all_items, group_gains = all_groups,
  ceiling_diagnostic = all_ceiling, transitions = all_transitions,
  leave_one_item_out = all_loo
)
for (name in names(tables)) {
  tables[[name]] <- dplyr::bind_rows(tables[[name]])
  stopifnot(all(tables[[name]]$pre_wave %in% c("t0", "t1")))
  readr::write_csv(tables[[name]], file.path(out, paste0(name, ".csv")))
}
readr::write_csv(dplyr::count(
  responses, wave, response_status,
  response_reason, wave_observed
), file.path(out, "response_audit.csv"))
jsonlite::write_json(list(
  response_input_sha256 = input_hash, baseline_input_sha256 = baseline_hash,
  seed = 20261009L, bootstrap_draws = 5000L, loo_draws = 2000L,
  baseline_people = nrow(baseline), eligible_baseline_people = nrow(people),
  inference = "Conditional on fixed coding and independent respondents",
  causal_effect = FALSE,
  specification_sha256 = digest::digest("ms/design.tex",
    file = TRUE, algo = "sha256"
  ),
  code_sha256 = digest::digest("R/analysis.R", file = TRUE, algo = "sha256"),
  orchestration_sha256 = digest::digest("scripts/analyze.R",
    file = TRUE, algo = "sha256"
  )
), file.path(out, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
