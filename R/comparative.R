comparative_validate <- function(data, resample) {
  required <- c(
    "respondent_id", "item_id", "relation", "pre", "post", "cluster_id"
  )
  if (!all(required %in% names(data))) stop("Missing comparative input columns")
  data <- as.data.frame(data[required])
  for (name in c("respondent_id", "item_id", "relation", "cluster_id")) {
    data[[name]] <- as.character(data[[name]])
  }
  keys <- data[c("respondent_id", "item_id")]
  if (anyNA(keys) || any(keys == "") || anyDuplicated(keys)) {
    stop("Missing or duplicate respondent-item key; relations may be ambiguous")
  }
  if (!all(data$relation %in% c("congenial", "uncongenial"))) {
    stop("Input must contain only classifiable relations")
  }
  for (name in c("pre", "post")) {
    if (
      !is.numeric(data[[name]]) || anyNA(data[[name]]) ||
        !all(data[[name]] %in% c(0, 1))
    ) {
      stop("Input must contain paired binary pre/post scores")
    }
  }
  if (
    resample == "cluster" &&
      (anyNA(data$cluster_id) || any(data$cluster_id == ""))
  ) {
    stop("Cluster resampling requires complete actual cluster IDs")
  }
  known <- !is.na(data$cluster_id) & data$cluster_id != ""
  clusters <- split(data$cluster_id[known], data$respondent_id[known])
  if (any(vapply(clusters, function(x) length(unique(x)) > 1L, logical(1)))) {
    stop("A respondent cannot belong to multiple resampling clusters")
  }
  data
}

comparative_metrics <- function(pre, post) {
  cbind(
    pre_c = pre[, 1], post_c = post[, 1], gain_c = post[, 1] - pre[, 1],
    pre_u = pre[, 2], post_u = post[, 2], gain_u = post[, 2] - pre[, 2],
    difference = post[, 2] - pre[, 2] - post[, 1] + pre[, 1],
    gap_pre = pre[, 1] - pre[, 2], gap_post = post[, 1] - post[, 2]
  )
}

comparative_estimates <- function(data, draws = 5000L, seed = 20261010L,
                                  resample = c("respondent", "cluster"),
                                  min_valid = 200L, sparse_units = 5L) {
  resample <- match.arg(resample)
  for (value in list(draws, min_valid, sparse_units)) {
    if (
      length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
        value < 1 || value != as.integer(value)
    ) {
      stop("Invalid bootstrap setting")
    }
  }
  data <- comparative_validate(data, resample)
  unit_column <- if (resample == "cluster") "cluster_id" else "respondent_id"
  data$unit <- data[[unit_column]]
  item_ids <- sort(unique(data$item_id))
  support <- expand.grid(
    relation = c("congenial", "uncongenial"), item_id = item_ids,
    stringsAsFactors = FALSE
  )
  support$n <- support$n_units <- integer(nrow(support))
  for (i in seq_len(nrow(support))) {
    selected <- data$item_id == support$item_id[i] &
      data$relation == support$relation[i]
    support$n[i] <- sum(selected)
    support$n_units[i] <- length(unique(data$unit[selected]))
  }
  eligible <- vapply(item_ids, function(id) {
    all(support$n[support$item_id == id] > 0L)
  }, logical(1))
  support$included <- support$item_id %in% item_ids[eligible]
  support$sparse <- support$n_units < sparse_units
  excluded <- data.frame(
    item_id = item_ids[!eligible],
    reason = rep("missing_relation_cell", sum(!eligible)),
    stringsAsFactors = FALSE
  )
  metric_names <- colnames(comparative_metrics(
    matrix(0, 1, 2), matrix(0, 1, 2)
  ))
  diagnostics <- list(
    status = "no_common_items", resample = resample, seed = seed,
    requested_draws = draws, valid_draws = 0L, invalid_draws = 0L,
    invalid_fraction = NA_real_, n_input_pairs = nrow(data),
    n_input_respondents = length(unique(data$respondent_id)),
    n_pairs = 0L, n_respondents = 0L, n_units = 0L, n_items = sum(eligible),
    sparse_units_threshold = sparse_units, sparse_support = FALSE,
    intervals_available = FALSE, conditional_on_cell_support = FALSE
  )
  empty <- data.frame(
    metric = metric_names, estimate = NA_real_,
    lower = NA_real_, upper = NA_real_
  )
  if (!any(eligible)) {
    return(list(
      estimates = empty, item_estimates = data.frame(),
      support = support, excluded_items = excluded, diagnostics = diagnostics
    ))
  }
  retained <- data[data$item_id %in% item_ids[eligible], ]
  item_ids <- item_ids[eligible]
  cell <- 2L * (match(retained$item_id, item_ids) - 1L) +
    match(retained$relation, c("congenial", "uncongenial"))
  order_rows <- order(cell)
  retained <- retained[order_rows, ]
  cell <- cell[order_rows]
  units <- unique(retained$unit)
  unit_index <- match(retained$unit, units)
  n_items <- length(item_ids)
  summarize <- function(weights) {
    sums <- rowsum(cbind(
      n = weights, pre = retained$pre * weights, post = retained$post * weights
    ), cell, reorder = FALSE)
    if (any(sums[, "n"] == 0)) {
      return(NULL)
    }
    comparative_metrics(
      matrix(sums[, "pre"] / sums[, "n"], ncol = 2, byrow = TRUE),
      matrix(sums[, "post"] / sums[, "n"], ncol = 2, byrow = TRUE)
    )
  }
  observed <- summarize(rep(1, nrow(retained)))
  boot <- matrix(NA_real_, nrow = draws, ncol = length(observed))
  withr::with_seed(seed, {
    for (b in seq_len(draws)) {
      counts <- tabulate(
        sample.int(length(units), length(units), replace = TRUE),
        nbins = length(units)
      )
      value <- summarize(counts[unit_index])
      if (!is.null(value)) boot[b, ] <- as.vector(value)
    }
  })
  valid <- stats::complete.cases(boot)
  n_valid <- sum(valid)
  included_support <- support[support$included, ]
  singleton_cell <- any(included_support$n_units < 2L)
  available <- n_valid >= min_valid && !singleton_cell
  intervals <- function(values) {
    if (!available) {
      return(rep(NA_real_, 2))
    }
    unname(stats::quantile(values, c(0.025, 0.975)))
  }
  item_rows <- lapply(seq_len(n_items), function(j) {
    columns <- j + n_items * (seq_along(metric_names) - 1L)
    bounds <- vapply(columns, function(k) intervals(boot[valid, k]), numeric(2))
    counts <- included_support[included_support$item_id == item_ids[j], ]
    data.frame(
      item_id = item_ids[j], metric = metric_names,
      estimate = 100 * unname(observed[j, ]),
      lower = 100 * bounds[1, ], upper = 100 * bounds[2, ],
      n_c = counts$n[counts$relation == "congenial"],
      n_u = counts$n[counts$relation == "uncongenial"]
    )
  })
  pooled <- matrix(NA_real_, nrow = n_valid, ncol = length(metric_names))
  for (k in seq_along(metric_names)) {
    columns <- seq_len(n_items) + n_items * (k - 1L)
    pooled[, k] <- rowMeans(boot[valid, columns, drop = FALSE])
  }
  bounds <- vapply(seq_along(metric_names), function(k) {
    intervals(pooled[, k])
  }, numeric(2))
  estimates <- data.frame(
    metric = metric_names, estimate = 100 * unname(colMeans(observed)),
    lower = 100 * bounds[1, ], upper = 100 * bounds[2, ]
  )
  diagnostics$status <- if (singleton_cell) {
    "singleton_cell"
  } else if (n_valid < min_valid) {
    "insufficient_valid_draws"
  } else if (any(!valid)) {
    "conditional_on_cell_support"
  } else {
    "ok"
  }
  diagnostics$valid_draws <- n_valid
  diagnostics$invalid_draws <- sum(!valid)
  diagnostics$invalid_fraction <- mean(!valid)
  diagnostics$n_pairs <- nrow(retained)
  diagnostics$n_respondents <- length(unique(retained$respondent_id))
  diagnostics$n_units <- length(units)
  diagnostics$sparse_support <- any(included_support$sparse)
  diagnostics$intervals_available <- available
  diagnostics$conditional_on_cell_support <- any(!valid)
  list(
    estimates = estimates, item_estimates = do.call(rbind, item_rows),
    support = support, excluded_items = excluded, diagnostics = diagnostics
  )
}
