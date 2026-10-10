dir.create("tabs", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)
read_table <- function(name) {
  readr::read_csv(file.path("output/analysis", paste0(name, ".csv")),
    show_col_types = FALSE
  )
}
estimates <- read_table("estimates")
primary <- function(x) {
  x[x$pre_wave == "t0" & x$missing_rule == "explicit_dk", ]
}
spec_labels <- c(
  common_core = "Common core", hansen_subset = "Hansen subset",
  hansen_without_bank = "Hansen without bank",
  pilot_agreement = "Pilot agreement"
)
item_labels <- c(
  knowledge_001 = "Deficit fines", knowledge_002 = "Interest rates",
  knowledge_003 = "Tax rates", knowledge_004 = "Circulation date",
  knowledge_005 = "Central bank", knowledge_006 = "National coin symbols"
)
group_labels <- c(opposes = "Opponents", supports = "Supporters")
value <- function(metric, spec = "common_core", pre = "t0",
                  rule = "explicit_dk", column = "estimate") {
  selected <- estimates$metric == metric &
    estimates$specification == spec & estimates$pre_wave == pre &
    estimates$missing_rule == rule
  row <- estimates[selected, ]
  stopifnot(nrow(row) == 1L)
  row[[column]]
}
fmt <- function(x) sprintf("%.1f", x)
interval <- function(x) sprintf("[%.1f, %.1f]", x$lower, x$upper)
write_table <- function(name, alignment, header, rows) {
  writeLines(c(
    paste0("\\begin{tabular}{", alignment, "}"), "\\toprule",
    paste0(header, " \\\\"), "\\midrule",
    paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}"
  ), file.path("tabs", paste0(name, ".tex")))
}
row_text <- function(...) paste(..., sep = " & ")
analysis_manifest <- jsonlite::read_json("output/analysis/manifest.json")
coding_manifest <- jsonlite::read_json("output/classification/manifest.json")
registry <- readr::read_csv("output/classification/item_registry.csv",
  show_col_types = FALSE
)
macros <- c(
  nBootstrap = format(analysis_manifest$bootstrap_draws, big.mark = ","),
  nLooDraws = format(analysis_manifest$loo_draws, big.mark = ","),
  analysisSeed = analysis_manifest$seed,
  nCohort = analysis_manifest$baseline_people,
  nEligible = analysis_manifest$eligible_baseline_people,
  nExcludedPosition = analysis_manifest$baseline_people -
    analysis_manifest$eligible_baseline_people,
  nPaired = value("difference", column = "n"),
  nSupport = value("difference", column = "n_supports"),
  nOppose = value("difference", column = "n_opposes"),
  nOmitted = analysis_manifest$eligible_baseline_people -
    value("difference", column = "n"),
  corePreC = fmt(value("pre_c")), corePostC = fmt(value("post_c")),
  corePreU = fmt(value("pre_u")), corePostU = fmt(value("post_u")),
  coreGainC = fmt(value("gain_c")), coreGainU = fmt(value("gain_u")),
  coreDiff = fmt(value("difference")),
  coreLow = fmt(value("difference", column = "lower")),
  coreHigh = fmt(value("difference", column = "upper")),
  coreComposition = fmt(value("difference", column = "composition_weighted")),
  arrivalDiff = fmt(value("difference", pre = "t1")),
  arrivalLow = fmt(value("difference", pre = "t1", column = "lower")),
  arrivalHigh = fmt(value("difference", pre = "t1", column = "upper")),
  arrivalN = value("difference", pre = "t1", column = "n"),
  substantiveDiff = fmt(value("difference", rule = "substantive_only")),
  substantiveLow = fmt(value("difference",
    rule = "substantive_only",
    column = "lower"
  )),
  substantiveHigh = fmt(value("difference",
    rule = "substantive_only",
    column = "upper"
  )),
  substantiveN = value("difference", rule = "substantive_only", column = "n"),
  pilotDiff = fmt(value("difference", spec = "pilot_agreement")),
  withoutBankDiff = fmt(value("difference", spec = "hansen_without_bank")),
  nPilotItems = nrow(registry), nPilotAgree = sum(registry$coder_agreement),
  pilotAgreement = fmt(100 * mean(registry$coder_agreement)),
  pilotKappa = sprintf("%.3f", coding_manifest$cohen_kappa),
  nPilotPeople = format(coding_manifest$people, big.mark = ","),
  nPilotRows = format(coding_manifest$rows, big.mark = ","),
  nDuplicates = coding_manifest$duplicate_representations_removed
)
ceiling <- primary(read_table("ceiling_diagnostic"))
headroom <- tapply(ceiling$predicted_difference, ceiling$specification, mean)
macros <- c(macros, coreHeadroom = fmt(headroom[["common_core"]]))
writeLines(paste0(
  "\\newcommand{\\", names(macros), "}{", macros, "}"
), "tabs/macros.tex")

core <- primary(estimates)
core <- core[core$specification == "common_core", ]
rows <- vapply(c(c = "Congenial", u = "Uncongenial"), function(label) {
  suffix <- if (label == "Congenial") "c" else "u"
  gain <- core[core$metric == paste0("gain_", suffix), ]
  row_text(
    label, fmt(value(paste0("pre_", suffix))),
    fmt(value(paste0("post_", suffix))), fmt(gain$estimate), interval(gain)
  )
}, character(1))
write_table(
  "levels", "lrrrr",
  "Fact & Recruitment (\\%) & Exit (\\%) & Gain (pp) & 95\\% interval", rows
)
groups <- primary(read_table("group_gains"))
groups <- groups[groups$specification == "common_core", ]
write_table("groups", "lrrrr", paste0(
  "Recruitment position & $N$ & Congenial gain & Uncongenial gain & Difference"
), row_text(
  group_labels[groups$baseline_group], groups$n,
  fmt(groups$gain_c), fmt(groups$gain_u), fmt(groups$difference)
))
differences <- primary(estimates)
differences <- differences[differences$metric == "difference", ]
write_table("coding", "lrrrr", paste0(
  "Battery & $N$ & Congenial gain & Uncongenial gain & Difference"
), vapply(seq_len(nrow(differences)), function(i) {
  x <- differences[i, ]
  row_text(
    spec_labels[x$specification], x$n,
    fmt(value("gain_c", spec = x$specification)),
    fmt(value("gain_u", spec = x$specification)), fmt(x$estimate)
  )
}, character(1)))
write_table(
  "headroom", "lrr", "Battery & Observed difference & Calibrated difference",
  row_text(
    spec_labels[differences$specification], fmt(differences$estimate),
    fmt(headroom[differences$specification])
  )
)
sens <- estimates[estimates$metric == "difference", ]
rules <- c(
  explicit_dk = "Explicit DK", canonical = "Blanks zero",
  substantive_only = "Substantive only"
)
windows <- c(t0 = "Recruitment", t1 = "Arrival")
write_table(
  "sensitivity", "lllrrr",
  "Battery & Start & Scoring & $N$ & Difference & 95\\% interval",
  row_text(
    spec_labels[sens$specification], windows[sens$pre_wave],
    rules[sens$missing_rule], sens$n, fmt(sens$estimate), interval(sens)
  )
)
loo <- primary(read_table("leave_one_item_out"))
write_table(
  "influence", "llrrr",
  "Battery & Omitted item & $N$ & Difference & 95\\% interval",
  row_text(
    spec_labels[loo$specification], item_labels[loo$dropped_item],
    loo$n, fmt(loo$estimate), interval(loo)
  )
)
items <- primary(read_table("item_gains"))
items <- items[items$specification == "common_core", ]
stopifnot(nrow(items) == 4L, all(items$pre_wave == "t0"))
write_table(
  "items", "lllrrrr",
  "Item & Position & Relation & $N$ & Before (\\%) & After (\\%) & Gain (pp)",
  row_text(
    item_labels[items$item_id], group_labels[items$baseline_group],
    items$relation, items$n, fmt(items$pre), fmt(items$post), fmt(items$gain)
  )
)
transitions <- primary(read_table("transitions"))
transitions <- transitions[transitions$specification == "common_core", ]
states <- c("correct", "incorrect", "dk")
transition_rows <- character()
for (group in c("opposes", "supports")) {
  for (relation in c("congenial", "uncongenial")) {
    for (before in states) {
      counts <- vapply(states, function(after) {
        selected <- transitions$baseline_group == group &
          transitions$relation == relation &
          transitions$knowledge_response_pre == before &
          transitions$knowledge_response_post == after
        sum(transitions$n[selected])
      }, numeric(1))
      state_label <- if (before == "dk") "DK" else before
      transition_rows <- c(transition_rows, row_text(
        group_labels[group], relation, state_label,
        counts[1], counts[2], counts[3]
      ))
    }
  }
}
write_table(
  "transitions", "lllrrr",
  "Position & Relation & Before & Correct after & Incorrect after & DK after",
  transition_rows
)
grDevices::pdf("figs/coding_sensitivity.pdf",
  width = 6.5, height = 3.5,
  family = "Times", pointsize = 11, useDingbats = FALSE
)
graphics::par(mar = c(3.5, 9, 0.6, 1), mgp = c(2.2, 0.6, 0), las = 1)
y <- rev(seq_len(nrow(differences)))
limits <- range(c(0, differences$lower, differences$upper))
limits[2] <- limits[2] + 0.05 * diff(limits)
graphics::plot(differences$estimate, y,
  pch = 19, yaxt = "n", ylab = "",
  xlab = "Difference in gains (percentage points)",
  xlim = limits, ylim = c(0.5, length(y) + 0.5), bty = "n", col = "#24546b"
)
graphics::abline(v = 0, col = "gray60", lty = 2)
graphics::segments(differences$lower, y, differences$upper, y,
  col = "#24546b", lwd = 1.6
)
graphics::axis(2,
  at = y, labels = spec_labels[differences$specification],
  tick = FALSE
)
grDevices::dev.off()
artifact_paths <- c(
  list.files("tabs", full.names = TRUE),
  "figs/coding_sensitivity.pdf"
)
hash_record <- function(path) {
  list(path = path, sha256 = digest::digest(path, file = TRUE, algo = "sha256"))
}
input_paths <- c(
  list.files("output/analysis", pattern = "csv$", full.names = TRUE),
  "output/analysis/manifest.json", "output/classification/manifest.json",
  "output/classification/item_registry.csv"
)
jsonlite::write_json(list(
  inputs = lapply(input_paths, hash_record),
  rendering_code_sha256 = digest::digest("scripts/build_manuscript.R",
    file = TRUE, algo = "sha256"
  ),
  artifacts = lapply(artifact_paths, hash_record)
), "output/analysis/manuscript_manifest.json", pretty = TRUE, auto_unbox = TRUE)
