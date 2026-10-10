analysis_score <- function(data, rule = "explicit_dk") {
  stopifnot(rule %in% c("explicit_dk", "canonical", "substantive_only"))
  score <- as.numeric(data$correct)
  score[!data$wave_observed | is.na(data$wave_observed)] <- NA_real_
  if (rule != "canonical") {
    score[data$response_reason %in% c("source_missing", "wave_absent")] <- NA
  }
  if (rule == "substantive_only") {
    score[!data$knowledge_response %in% c("correct", "incorrect")] <- NA
  }
  stopifnot(all(is.na(score) | score %in% c(0, 1)))
  score
}

make_pairs <- function(responses, people, mapping, pre, rule) {
  assert_unique(people, person_keys)
  stopifnot(all(people$baseline_group %in% c("supports", "opposes")))
  assert_unique(mapping, "item_id")
  stopifnot(all(mapping$direction %in% c("supports", "challenges")))
  stopifnot(all(c("supports", "challenges") %in% mapping$direction))
  keys <- c(person_keys, "item_id")
  selected <- responses$item_id %in% mapping$item_id &
    responses$wave %in% c(pre, "t2")
  chosen <- responses[selected, ]
  assert_unique(chosen, c(keys, "wave"))
  chosen$score <- analysis_score(chosen, rule)
  before <- chosen[
    chosen$wave == pre,
    c(keys, "score", "knowledge_response", "response_reason")
  ]
  after <- chosen[
    chosen$wave == "t2",
    c(keys, "score", "knowledge_response", "response_reason")
  ]
  grid <- dplyr::cross_join(people, mapping)
  grid$relation <- classify_relation(
    grid$direction,
    ifelse(grid$baseline_group == "supports", "positive", "negative")
  )
  paired <- dplyr::left_join(grid, before,
    by = keys,
    relationship = "one-to-one"
  )
  paired <- dplyr::left_join(paired, after,
    by = keys,
    relationship = "one-to-one", suffix = c("_pre", "_post")
  )
  stopifnot(nrow(paired) == nrow(people) * nrow(mapping))
  paired <- paired |>
    dplyr::group_by(dplyr::across(dplyr::all_of(person_keys))) |>
    dplyr::mutate(
      complete_battery = all(!is.na(score_pre) & !is.na(score_post))
    ) |>
    dplyr::ungroup()
  paired
}

person_scores <- function(pairs) {
  kept <- pairs[pairs$complete_battery, ]
  stopifnot(nrow(kept) > 0L)
  scores <- kept |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      person_keys,
      "baseline_group", "relation"
    )))) |>
    dplyr::summarise(
      pre = mean(score_pre), post = mean(score_post),
      items = dplyr::n(), .groups = "drop"
    )
  c_scores <- scores[scores$relation == "congenial", ]
  u_scores <- scores[scores$relation == "uncongenial", ]
  result <- dplyr::inner_join(c_scores, u_scores,
    by = c(person_keys, "baseline_group"), suffix = c("_c", "_u"),
    relationship = "one-to-one"
  )
  stopifnot(nrow(result) * 2L == nrow(scores))
  result$gain_c <- result$post_c - result$pre_c
  result$gain_u <- result$post_u - result$pre_u
  result$difference <- result$gain_u - result$gain_c
  result
}

learning_estimates <- function(scores, draws = 5000L, seed = 20261009L) {
  columns <- c(
    "pre_c", "post_c", "gain_c", "pre_u", "post_u", "gain_u",
    "difference"
  )
  groups <- split(scores, scores$baseline_group)
  stopifnot(setequal(names(groups), c("supports", "opposes")))
  values <- lapply(groups, function(x) as.matrix(x[columns]))
  estimate <- Reduce(`+`, lapply(values, colMeans)) / length(values)
  set.seed(seed)
  boot <- replicate(draws, {
    sampled <- lapply(values, function(x) {
      colMeans(x[sample.int(nrow(x), nrow(x), replace = TRUE), , drop = FALSE])
    })
    Reduce(`+`, sampled) / length(sampled)
  })
  interval <- t(apply(boot, 1, stats::quantile, probs = c(0.025, 0.975)))
  tibble::tibble(
    metric = columns, estimate = 100 * unname(estimate),
    lower = 100 * unname(interval[, 1]), upper = 100 * unname(interval[, 2]),
    composition_weighted = 100 * unname(colMeans(as.matrix(scores[columns]))),
    n = nrow(scores), n_supports = nrow(groups$supports),
    n_opposes = nrow(groups$opposes)
  )
}

tag_analysis <- function(data, specification, pre, rule) {
  data$specification <- specification
  data$pre_wave <- pre
  data$post_wave <- "t2"
  data$missing_rule <- rule
  data
}

ceiling_diagnostic <- function(pairs) {
  pairs[pairs$complete_battery, ] |>
    dplyr::group_by(baseline_group) |>
    dplyr::summarise(
      acquisition = mean(score_post[score_pre == 0]),
      loss = mean(1 - score_post[score_pre == 1]),
      pre_c = mean(score_pre[relation == "congenial"]),
      pre_u = mean(score_pre[relation == "uncongenial"]),
      predicted_difference = 100 * (acquisition + loss) * (pre_c - pre_u),
      .groups = "drop"
    )
}
