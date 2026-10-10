testthat::test_that("the same fact reverses across baseline positions", {
  testthat::expect_identical(
    classify_relation(
      rep(c("supports", "challenges"), 2),
      rep(c("positive", "negative"), each = 2)
    ),
    c("congenial", "uncongenial", "uncongenial", "congenial")
  )
  testthat::expect_identical(
    classify_relation(
      c("neutral", "unknown", "ambiguous", "supports"),
      c("positive", "negative", "positive", "unclassified")
    ),
    rep("cannot_classify", 4)
  )
})

testthat::test_that("joins reject missing and multiplied keys", {
  data <- data.frame(id = c("a", "b"))
  testthat::expect_silent(assert_unique(data, "id"))
  testthat::expect_error(assert_unique(rbind(data, data[1, ]), "id"), "key")
  data$id[1] <- NA
  testthat::expect_error(assert_unique(data, "id"), "key")
})

fixture_people <- function() {
  tibble::tibble(
    poll_id = "denmark-euro-2000", source_dataset = "test",
    respondent_id = c("1", "2", "3"), measure_id = "euro_preference",
    baseline_group = c("supports", "opposes", "undecided")
  )
}

fixture_registry <- function() {
  tibble::tibble(
    poll_id = "denmark-euro-2000", item_id = c("1", "2", "3"),
    axis_id = "join_euro", coder_agreement = TRUE,
    fact_implication = c("supports", "challenges", "neutral")
  )
}

testthat::test_that("unclassified rows remain; one-sided people fail support", {
  result <- build_relations(fixture_people(), fixture_registry())
  testthat::expect_equal(nrow(result), 9)
  testthat::expect_equal(sum(result$classification == "cannot_classify"), 5)
  testthat::expect_equal(sum(result$candidate_contrast_row), 4)
  testthat::expect_false(any(result$ready_for_estimation))
  one_side <- fixture_registry()[c(1, 3), ]
  result <- build_relations(fixture_people(), one_side)
  testthat::expect_false(any(result$has_both_categories))
})

testthat::test_that("coder errors fail and disagreements remain unclassified", {
  a <- fixture_registry()
  a$evidence_locator <- "test source"
  b <- a
  b$fact_implication[1] <- "challenges"
  codes <- compare_codes(a, b, a[item_keys])
  testthat::expect_false(codes$coder_agreement[1])
  result <- build_relations(fixture_people(), codes)
  disputed <- result$classification[result$item_id == "1"]
  testthat::expect_true(all(disputed == "cannot_classify"))
  testthat::expect_error(compare_codes(a[-1, ], b, a[item_keys]), "coverage")
  extra <- a[1, ]
  extra$item_id <- "invented"
  testthat::expect_error(
    compare_codes(rbind(a, extra), b, a[item_keys]),
    "coverage"
  )
  b$axis_id <- "wrong"
  testthat::expect_error(compare_codes(a, b, a[item_keys]))
  a$axis_id <- "wrong"
  testthat::expect_error(compare_codes(a, b, a[item_keys]), "axis contract")
  people <- fixture_people()
  people$measure_id <- "party_identity"
  testthat::expect_error(
    build_relations(people, fixture_registry()),
    "protocol contract"
  )
})

testthat::test_that("person bridges preserve unrelated records", {
  people <- tibble::tibble(
    poll_id = "unrelated-poll",
    source_dataset = c("cor_sood", "control"), respondent_id = "1",
    measure_id = "test", baseline_group = "test", source_path = "original",
    source_row = 1L, source_sha256 = "hash"
  )
  testthat::expect_equal(nrow(deduplicate_people(people)$people), 2)
  people$poll_id <- "northern-ireland-2007"
  people$baseline_group[2] <- "conflict"
  testthat::expect_error(deduplicate_people(people))
  people$baseline_group <- "test"
  people$source_sha256[2] <- "conflict"
  testthat::expect_error(deduplicate_people(people))
})

testthat::test_that("later positions and answers do not determine categories", {
  before <- fixture_people()
  after <- before
  after$post_position <- c("opposes", "supports", "supports")
  after$correct <- c(0, 1, 1)
  a <- build_relations(before, fixture_registry())
  b <- build_relations(after, fixture_registry())
  testthat::expect_identical(a$classification, b$classification)
})

testthat::test_that("only verified Northern Ireland representations collapse", {
  people <- tibble::tibble(
    poll_id = "northern-ireland-2007",
    source_dataset = c("cor_sood", "control", "control"),
    respondent_id = c("1", "1", "2"), measure_id = "religious_affiliation",
    baseline_group = "catholic", source_path = "original",
    source_row = c(1L, 1L, 2L), source_sha256 = "hash"
  )
  result <- deduplicate_people(people)
  testthat::expect_equal(nrow(result$people), 2)
  testthat::expect_equal(nrow(result$bridge), 1)
  people$source_row[2] <- 9L
  testthat::expect_error(deduplicate_people(people))
})

testthat::test_that("altered inputs are refused", {
  path <- tempfile()
  on.exit(unlink(path))
  writeLines("original", path)
  checksum <- digest::digest(path, file = TRUE, algo = "sha256")
  testthat::expect_identical(check_hash(path, checksum), checksum)
  writeLines("changed", path)
  testthat::expect_error(check_hash(path, checksum), "Checksum")
})
