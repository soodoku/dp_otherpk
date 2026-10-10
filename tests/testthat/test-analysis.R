source(file.path("..", "..", "R", "analysis.R"))

testthat::test_that("missing answers are distinct from literal DK", {
  x <- data.frame(correct = c(0, 0, 0, 1), wave_observed = c(
    TRUE, TRUE,
    FALSE, TRUE
  ), response_reason = c(
    "dk", "source_missing", "wave_absent",
    "answered"
  ), knowledge_response = c("dk", "dk", NA, "correct"))
  testthat::expect_equal(analysis_score(x), c(0, NA, NA, 1))
  testthat::expect_equal(analysis_score(x, "canonical"), c(0, 0, NA, 1))
  testthat::expect_equal(
    analysis_score(x, "substantive_only"),
    c(NA, NA, NA, 1)
  )
})

testthat::test_that("paired batteries reject missing rows and duplicate keys", {
  people <- tibble::tibble(
    poll_id = "denmark-euro-2000",
    source_dataset = "cor_sood", respondent_id = as.character(1:3),
    baseline_group = c("supports", "opposes", "opposes")
  )
  mapping <- tibble::tibble(
    item_id = c("a", "b"),
    direction = c("supports", "challenges")
  )
  responses <- expand.grid(
    respondent_id = as.character(1:3),
    item_id = c("a", "b"), wave = c("t0", "t2"),
    stringsAsFactors = FALSE
  )
  responses$poll_id <- "denmark-euro-2000"
  responses$source_dataset <- "cor_sood"
  responses$correct <- 0
  responses$wave_observed <- TRUE
  responses$knowledge_response <- "incorrect"
  responses$response_reason <- "answered"
  missing <- responses$respondent_id == "3" & responses$item_id == "b" &
    responses$wave == "t2"
  responses$response_reason[missing] <- "source_missing"
  pairs <- make_pairs(responses, people, mapping, "t0", "explicit_dk")
  testthat::expect_equal(nrow(pairs), 6)
  testthat::expect_equal(sum(pairs$complete_battery), 4)
  testthat::expect_setequal(person_scores(pairs)$respondent_id, c("1", "2"))
  testthat::expect_error(make_pairs(
    rbind(responses, responses[1, ]), people,
    mapping, "t0", "explicit_dk"
  ), "duplicate key")
  testthat::expect_error(make_pairs(
    responses, people, mapping[1, ], "t0",
    "explicit_dk"
  ))
})

testthat::test_that("balanced weights recover hand-computed gains", {
  x <- data.frame(
    baseline_group = c("supports", "supports", "opposes"),
    pre_c = c(1, 1, 0), post_c = c(1, 1, 1),
    pre_u = c(0, 0, 0), post_u = c(1, 1, 0)
  )
  x$gain_c <- x$post_c - x$pre_c
  x$gain_u <- x$post_u - x$pre_u
  x$difference <- x$gain_u - x$gain_c
  y <- learning_estimates(x, draws = 50L)
  testthat::expect_equal(y$estimate[y$metric == "gain_c"], 50)
  testthat::expect_equal(y$estimate[y$metric == "gain_u"], 50)
  testthat::expect_equal(y$estimate[y$metric == "difference"], 0)
  testthat::expect_equal(
    y$composition_weighted[y$metric == "difference"],
    100 / 3
  )
  testthat::expect_equal(y$lower[y$metric == "difference"], 0)
})

testthat::test_that("analysis tags do not capture similarly named columns", {
  cells <- tibble::tibble(pre = c(86, 65), specification = c("x", "y"))
  tagged <- tag_analysis(cells, "common_core", "t0", "explicit_dk")
  testthat::expect_identical(tagged$pre_wave, c("t0", "t0"))
  testthat::expect_identical(tagged$specification, rep("common_core", 2))
  testthat::expect_identical(tagged$pre, cells$pre)
})
