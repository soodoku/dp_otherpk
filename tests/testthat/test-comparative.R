source(file.path("..", "..", "R", "comparative.R"))

comparative_fixture <- function() {
  data.frame(
    respondent_id = rep(as.character(1:8), 2),
    item_id = rep(c("one", "two"), each = 8),
    relation = rep(rep(c("congenial", "uncongenial"), each = 4), 2),
    pre = 0, post = c(rep(0, 4), rep(1, 4), rep(1, 4), rep(0, 4)),
    cluster_id = rep(rep(letters[1:4], each = 2), 2)
  )
}

testthat::test_that("equal fixed item weights differ from response weighting", {
  data <- comparative_fixture()
  data <- data[!(data$item_id == "two" & data$respondent_id %in% c("3", "4")), ]
  extra <- data[data$item_id == "one" & data$relation == "congenial", ]
  extra$item_id <- "one_sided"
  result <- comparative_estimates(
    rbind(data, extra),
    draws = 100, min_valid = 20
  )
  values <- setNames(result$estimates$estimate, result$estimates$metric)
  testthat::expect_equal(
    unname(values[c("gain_c", "gain_u", "difference")]),
    c(50, 50, 0)
  )
  testthat::expect_equal(result$excluded_items$item_id, "one_sided")
  testthat::expect_equal(result$diagnostics$n_items, 2)
  testthat::expect_equal(result$diagnostics$n_pairs, 14)
  testthat::expect_equal(result$diagnostics$n_respondents, 8)
  testthat::expect_equal(result$diagnostics$n_input_pairs, 18)
})

testthat::test_that("whole-person resampling retains cross-item dependence", {
  data <- comparative_fixture()
  result <- comparative_estimates(data, draws = 100, min_valid = 20)
  difference <- result$estimates[result$estimates$metric == "difference", ]
  testthat::expect_equal(difference$estimate, 0)
  testthat::expect_equal(difference$lower, 0)
  testthat::expect_equal(difference$upper, 0)
  testthat::expect_true(result$diagnostics$invalid_draws > 0)
  total_draws <- result$diagnostics$valid_draws +
    result$diagnostics$invalid_draws
  testthat::expect_equal(total_draws, 100)
  testthat::expect_true(result$diagnostics$conditional_on_cell_support)
  testthat::expect_equal(result$item_estimates$estimate[
    result$item_estimates$metric == "difference"
  ], c(100, -100))
})

testthat::test_that("relations vary across items but never within a pair", {
  data <- comparative_fixture()
  second <- data$item_id == "two"
  data$relation[second] <- rev(data$relation[second])
  testthat::expect_silent(comparative_estimates(
    data,
    draws = 20, min_valid = 10
  ))
  duplicate <- data[1, ]
  duplicate$relation <- "uncongenial"
  testthat::expect_error(comparative_estimates(rbind(data, duplicate)), "key")
  data$pre[1] <- NA_real_
  testthat::expect_error(comparative_estimates(data), "paired binary")
  data$pre[1] <- 0.5
  testthat::expect_error(comparative_estimates(data), "paired binary")
  data$pre[1] <- 0
  data$relation[1] <- "cannot_classify"
  testthat::expect_error(comparative_estimates(data), "classifiable")
})

testthat::test_that("cluster sampling preserves groups and rejects gaps", {
  data <- comparative_fixture()
  result <- comparative_estimates(data,
    draws = 100, min_valid = 20,
    resample = "cluster"
  )
  testthat::expect_equal(result$diagnostics$n_units, 4)
  testthat::expect_equal(unique(result$support$n_units), 2)
  testthat::expect_true(result$diagnostics$invalid_draws > 0)
  testthat::expect_equal(result$estimates$lower[
    result$estimates$metric == "difference"
  ], 0)
  data$cluster_id[1] <- NA_character_
  testthat::expect_error(
    comparative_estimates(data, resample = "cluster"),
    "complete actual"
  )
  data$cluster_id[1] <- "other"
  testthat::expect_error(comparative_estimates(data), "multiple")
})

testthat::test_that("degenerate support never silently changes the target", {
  data <- comparative_fixture()
  one_sided <- data[data$relation == "congenial", ]
  result <- comparative_estimates(one_sided, draws = 10)
  testthat::expect_equal(result$diagnostics$status, "no_common_items")
  testthat::expect_true(all(is.na(result$estimates$estimate)))
  testthat::expect_equal(nrow(result$excluded_items), 2)
  empty <- comparative_estimates(data[FALSE, ], draws = 10)
  testthat::expect_equal(empty$diagnostics$status, "no_common_items")
  singleton <- data[data$respondent_id %in% c("1", "5"), ]
  result <- comparative_estimates(singleton, draws = 40, min_valid = 2)
  testthat::expect_equal(result$diagnostics$status, "singleton_cell")
  testthat::expect_true(all(is.na(result$estimates$lower)))
  testthat::expect_true(result$diagnostics$sparse_support)
  insufficient <- comparative_estimates(data, draws = 5, min_valid = 20)
  testthat::expect_equal(
    insufficient$diagnostics$status,
    "insufficient_valid_draws"
  )
  testthat::expect_false(insufficient$diagnostics$intervals_available)
  data$pre <- data$post <- rep(1, nrow(data))
  constant <- comparative_estimates(data, draws = 100, min_valid = 20)
  testthat::expect_equal(constant$estimates$estimate[
    constant$estimates$metric == "difference"
  ], 0)
  testthat::expect_equal(constant$estimates$lower[
    constant$estimates$metric == "difference"
  ], 0)
})

testthat::test_that("bootstrap is reproducible without changing caller RNG", {
  data <- comparative_fixture()
  set.seed(42)
  state <- .Random.seed
  a <- comparative_estimates(data, draws = 20, seed = 4, min_valid = 2)
  testthat::expect_identical(.Random.seed, state)
  b <- comparative_estimates(data, draws = 20, seed = 4, min_valid = 2)
  testthat::expect_identical(a, b)
})

testthat::test_that("cluster bootstrap matches direct single-item means", {
  data <- data.frame(
    respondent_id = as.character(1:8), item_id = "single",
    relation = rep(c("congenial", "uncongenial"), 4),
    pre = 0, post = c(0, 1, 0, 0, 1, 1, 1, 1),
    cluster_id = rep(letters[1:4], each = 2)
  )
  result <- comparative_estimates(data,
    draws = 100, seed = 17,
    resample = "cluster", min_valid = 20
  )
  expected <- withr::with_seed(17, replicate(100, {
    index <- sample.int(4, 4, replace = TRUE)
    100 * mean(c(1, 0, 0, 0)[index])
  }))
  actual <- result$estimates[result$estimates$metric == "difference", ]
  testthat::expect_equal(actual$estimate, 25)
  testthat::expect_equal(
    c(actual$lower, actual$upper),
    unname(stats::quantile(expected, c(0.025, 0.975)))
  )
  testthat::expect_equal(result$diagnostics$invalid_draws, 0)
  testthat::expect_equal(result$diagnostics$status, "ok")
  testthat::expect_true(result$diagnostics$sparse_support)
  gap <- result$estimates[result$estimates$metric == "gap_post", ]
  testthat::expect_equal(gap$estimate, -25)
  testthat::expect_equal(
    c(gap$lower, gap$upper),
    unname(stats::quantile(-expected, c(0.025, 0.975)))
  )
})

testthat::test_that("relative gains can reverse a remaining knowledge gap", {
  data <- data.frame(
    respondent_id = as.character(1:8), item_id = "single",
    relation = rep(c("congenial", "uncongenial"), each = 4),
    pre = c(1, 1, 0, 0, 1, 0, 0, 0),
    post = c(1, 1, 0, 0, 1, 1, 1, 0),
    cluster_id = rep(letters[1:4], 2)
  )
  result <- comparative_estimates(data,
    draws = 100, min_valid = 20, resample = "cluster"
  )
  values <- setNames(result$estimates$estimate, result$estimates$metric)
  testthat::expect_equal(
    unname(values[c("gain_u", "difference", "gap_pre", "gap_post")]),
    c(50, 50, 25, -25)
  )
})
