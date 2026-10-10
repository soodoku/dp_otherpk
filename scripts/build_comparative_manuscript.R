out <- "output/comparative"
est <- readr::read_csv(file.path(out, "estimates.csv"), show_col_types = FALSE)
diag <- readr::read_csv(file.path(out, "diagnostics.csv"),
  show_col_types = FALSE
)
for (i in seq_len(nrow(diag))) {
  if (diag$sparse_support[i] || diag$partial_cluster_coverage[i]) {
    selected <- est$poll_id == diag$poll_id[i] &
      est$construct == diag$construct[i] &
      est$specification == diag$specification[i] &
      est$missing_rule == diag$missing_rule[i]
    est$lower[selected] <- est$upper[selected] <- NA_real_
  }
}
registry <- readr::read_csv("coding/adjudicated.csv", show_col_types = FALSE)
inventory <- jsonlite::read_json("metadata/item_inventory.json",
  simplifyVector = TRUE
)$items
labels <- c(
  "uk-crime-1994" = "Britain: crime (1994)",
  "uk-eu-1995" = "Britain: EU (1995)",
  "uk-monarchy-1996" = "Britain: monarchy (1996)",
  "uk-general-election-1997" = "Britain: general election (1997)",
  "uk-health-1998" = "Britain: health (1998)",
  "nic-1996" = "US: National Issues Convention (1996)",
  "cpl-1996" = "Texas: Central Power and Light (1996)",
  "wtu-1996" = "Texas: West Texas Utilities (1996)",
  "swepco-1996" = "Texas: Southwestern Electric Power (1996)",
  "australia-republic-1999" = "Australia: republic (1999)",
  "denmark-euro-2000" = "Denmark: euro (2000)",
  "bulgaria-crime-2002" = "Bulgaria: crime (2002)",
  "new-haven-2004" = "New Haven: airport and tax sharing (2002)",
  "btp-national-2003" = "US: online foreign policy (2003)",
  "nic2-2003" = "US: foreign policy (2003)",
  "btp-general-election-2004" = "US: election (2004)",
  "btp-online-primaries-2004" = "US: online primaries (2004)",
  "btp-presidential-primaries-2004" = "US: presidential primaries (2004)",
  "btp-health-education-2005" = "US: health/education (2005)",
  "zeguo-2005" = "China: Zeguo infrastructure (2005)",
  "marousi-2006" = "Greece: Marousi mayoral selection (2006)",
  "btp-2007" = "US: citizenship (2007)",
  "northern-ireland-2007" = "Northern Ireland: schools (2007)",
  "vermont-energy-2007" = "Vermont: energy (2007)",
  "san-mateo-2008" = "San Mateo: housing (2008)",
  "tomorrows-europe-2007" = "EU: Tomorrow's Europe (2007)",
  "europolis-2009" = "EU: Europolis (2009)",
  "michigan-2009" = "Michigan: economy and budget (2009)",
  "california-whats-next-2011" = "California: government reform (2011)",
  "america-in-one-room-2019" = "US: America in One Room (2019)",
  "a1r-climate-2021" = "US: America in One Room, climate (2021)",
  "amr-2024" = "Multinational: antibiotics (2024)"
)
stopifnot(
  !anyDuplicated(names(labels)), !anyDuplicated(unname(labels)),
  all(inventory$poll_id %in% names(labels)),
  all(est$poll_id %in% names(labels)),
  all(registry$poll_id %in% names(labels))
)
analysis_polls <- names(labels)[names(labels) %in% est$poll_id]
select_spec <- function(x, spec = "primary", rule = "explicit_dk",
                        construct = "policy") {
  selected <- x$specification == spec & x$missing_rule == rule &
    x$construct == construct
  x[selected, ]
}
primary <- select_spec(est)
d <- select_spec(diag)
polls <- names(labels)[names(labels) %in% primary$poll_id]
fmt <- function(x) {
  text <- ifelse(is.na(x), "--", sprintf("%.1f", x))
  text[text == "-0.0"] <- "0.0"
  text
}
get_value <- function(poll, metric, column = "estimate", x = primary) {
  row <- x[x$poll_id == poll & x$metric == metric, ]
  stopifnot(nrow(row) == 1L)
  row[[column]]
}
ci <- function(poll, x = primary, metric = "difference") {
  lo <- get_value(poll, metric, "lower", x)
  hi <- get_value(poll, metric, "upper", x)
  if (is.na(lo)) "--" else paste0("[", fmt(lo), ", ", fmt(hi), "]")
}
write_tab <- function(name, align, header, rows) {
  writeLines(c(
    paste0("\\begin{tabular}{", align, "}"), "\\toprule",
    paste0(header, " \\\\"), "\\midrule",
    paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}"
  ), file.path("tabs", paste0(name, ".tex")))
}
row_text <- function(...) paste(..., sep = " & ")
gain_rows <- level_rows <- support_rows <- gap_rows <- character()
for (poll in polls) {
  counts <- d[d$poll_id == poll, ]
  gain_rows <- c(gain_rows, row_text(
    labels[poll], counts$n_items, counts$n_respondents,
    fmt(get_value(poll, "gain_c")), fmt(get_value(poll, "gain_u")),
    fmt(get_value(poll, "difference")), ci(poll)
  ))
  level_rows <- c(level_rows, row_text(
    labels[poll], fmt(get_value(poll, "pre_c")),
    fmt(get_value(poll, "post_c")), fmt(get_value(poll, "pre_u")),
    fmt(get_value(poll, "post_u"))
  ))
  support_rows <- c(support_rows, row_text(
    labels[poll], counts$resample, counts$n_units,
    counts$invalid_draws, ifelse(counts$sparse_support, "Yes", "No")
  ))
  gap_rows <- c(gap_rows, row_text(
    labels[poll], fmt(get_value(poll, "gap_pre")),
    fmt(get_value(poll, "gap_post")), ci(poll, metric = "gap_post")
  ))
}
write_tab(
  "comparative_gains", "lrrrrrl",
  "Study & Items & People & C gain & U gain & U--C & 95\\% interval",
  gain_rows
)
write_tab(
  "comparative_levels", "lrrrr",
  "Study & C pre & C post & U pre & U post", level_rows
)
write_tab(
  "comparative_gaps", "lrrl",
  "Study & Gap before & Gap after & 95\\% interval after", gap_rows
)
write_tab(
  "comparative_support", "llrrr",
  "Study & Resampling unit & Units & Invalid draws & Sparse cell",
  support_rows
)
diff <- primary[primary$metric == "difference", ]
diff <- diff[match(polls, diff$poll_id), ]
grDevices::pdf("figs/comparative_gains.pdf",
  width = 7, height = 5.6,
  family = "Helvetica", pointsize = 10
)
graphics::par(mar = c(4.2, 13.2, 0.7, 0.7), las = 1)
extent <- range(c(0, diff$lower, diff$upper, diff$estimate), na.rm = TRUE)
y <- rev(seq_len(nrow(diff)))
graphics::plot(diff$estimate, y,
  xlim = extent + c(-2, 2), ylim = c(.5, nrow(diff) + .5),
  yaxt = "n", ylab = "",
  xlab = "Uncongenial minus congenial gain (percentage points)",
  pch = 16, bty = "n"
)
graphics::abline(v = 0, col = "gray55", lty = 2)
graphics::segments(diff$lower, y, diff$upper, y, col = "gray30", lwd = 1.2)
graphics::points(diff$estimate, y, pch = 16, cex = .8)
graphics::axis(2,
  at = y, labels = unname(labels[polls]), tick = FALSE,
  cex.axis = .85
)
invisible(grDevices::dev.off())
gain_u <- primary[primary$metric == "gain_u", ]
party <- select_spec(est, construct = "party")
item_estimates <- readr::read_csv(file.path(out, "item_estimates.csv"),
  show_col_types = FALSE
)
aca <- select_spec(item_estimates)
selected_aca <- aca$poll_id == "america-in-one-room-2019" &
  aca$item_id == "knowledge_005"
aca <- aca[selected_aca, ]
macros <- c(
  inventoryItems = nrow(inventory),
  inventoryStudies = length(unique(inventory$poll_id)),
  codedStudies = length(unique(registry$poll_id)),
  primaryStudies = length(polls), primaryItems = sum(d$n_items),
  comparativePeople = sum(d$n_respondents),
  positiveGainStudies = sum(gain_u$estimate > 0),
  positiveDifferenceStudies = sum(diff$estimate > 0),
  positiveIntervalStudies = sum(diff$lower > 0, na.rm = TRUE),
  comparativeDraws = "5,000",
  comparativeMin = fmt(min(diff$estimate)),
  comparativeMax = fmt(max(diff$estimate)),
  niGainC = fmt(get_value("northern-ireland-2007", "gain_c")),
  niGainU = fmt(get_value("northern-ireland-2007", "gain_u")),
  niDifference = fmt(get_value("northern-ireland-2007", "difference")),
  niLow = fmt(get_value("northern-ireland-2007", "difference", "lower")),
  niHigh = fmt(get_value("northern-ireland-2007", "difference", "upper")),
  dkGapPre = fmt(get_value("denmark-euro-2000", "gap_pre")),
  dkGapPost = fmt(get_value("denmark-euro-2000", "gap_post")),
  niGapPre = fmt(get_value("northern-ireland-2007", "gap_pre")),
  niGapPost = fmt(get_value("northern-ireland-2007", "gap_post")),
  usGainC = fmt(get_value("america-in-one-room-2019", "gain_c")),
  usGainU = fmt(get_value("america-in-one-room-2019", "gain_u")),
  acaPolicyDiff = fmt(get_value("america-in-one-room-2019", "difference",
    x = aca
  )),
  acaPolicyLow = fmt(get_value("america-in-one-room-2019", "difference",
    column = "lower", x = aca
  )),
  acaPolicyHigh = fmt(get_value("america-in-one-room-2019", "difference",
    column = "upper", x = aca
  )),
  sanMateoN = d$n_respondents[d$poll_id == "san-mateo-2008"],
  sanMateoCanonicalN = diag$n_respondents[
    diag$poll_id == "san-mateo-2008" & diag$construct == "policy" &
      diag$specification == "primary" & diag$missing_rule == "canonical"
  ],
  sanMateoDiff = fmt(get_value("san-mateo-2008", "difference")),
  sanMateoCanonicalDiff = fmt(get_value("san-mateo-2008", "difference",
    x = select_spec(est, rule = "canonical")
  )),
  partyGainC = fmt(get_value("america-in-one-room-2019", "gain_c", x = party)),
  partyGainU = fmt(get_value("america-in-one-room-2019", "gain_u", x = party)),
  partyGapPre = fmt(get_value("america-in-one-room-2019", "gap_pre",
    x = party
  )),
  partyGapPost = fmt(get_value("america-in-one-room-2019", "gap_post",
    x = party
  )),
  partyGapLow = fmt(get_value("america-in-one-room-2019", "gap_post", "lower",
    x = party
  )),
  partyGapHigh = fmt(get_value("america-in-one-room-2019", "gap_post", "upper",
    x = party
  )),
  partyDifference = fmt(get_value("america-in-one-room-2019", "difference",
    x = party
  )),
  partyLow = fmt(get_value("america-in-one-room-2019", "difference", "lower",
    x = party
  )),
  partyHigh = fmt(get_value("america-in-one-room-2019", "difference", "upper",
    x = party
  ))
)
writeLines(
  paste0("\\newcommand{\\", names(macros), "}{", macros, "}"),
  "tabs/comparative_macros.tex"
)
sensitivity_rows <- character()
for (poll in analysis_polls) {
  values <- lapply(c("expanded", "canonical", "substantive_only"), function(s) {
    x <- if (s == "expanded") {
      select_spec(est, spec = s)
    } else {
      select_spec(est, rule = s)
    }
    row <- x[x$poll_id == poll & x$metric == "difference", ]
    if (!nrow(row)) {
      return("--")
    }
    paste0(fmt(row$estimate), " ", ci(poll, x))
  })
  sensitivity_rows <- c(sensitivity_rows, row_text(
    labels[poll],
    values[[1]], values[[2]], values[[3]]
  ))
}
write_tab(
  "comparative_sensitivity", "llll",
  "Study & Expanded items & Canonical score & Substantive only",
  sensitivity_rows
)
readr::write_csv(
  data.frame(poll_id = polls, label = unname(labels[polls])),
  file.path(out, "manuscript_studies.csv")
)

escape_tex <- function(x) {
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  gsub("_", "\\_", x, fixed = TRUE)
}
axis_labels <- c(
  act_on_warming = "Act now on global warming",
  antibiotic_awareness = "Emphasize antibiotic harms in awareness campaigns",
  approve_republic_model = "Approve proposed republic",
  assist_uninsured = "Provide insurance assistance",
  below_market_housing = "Require below-market housing",
  closer_eu_relations = "Closer EU relations",
  election_day_registration = "Permit election-day registration",
  equalize_capital_gains_tax = "Tax capital gains as wages",
  expand_integrated_schools = "Expand integrated schools",
  fund_nhs = "Increase NHS spending",
  increase_efficiency_funding = "Increase efficiency funding",
  increase_foreign_aid = "Increase foreign aid",
  increase_housing = "Build more housing",
  increase_old_age_health_funding = "Increase Medicare/Medicaid funds",
  independent_redistricting = "Independent redistricting commissions",
  infection_prevention = "Prioritize infection prevention",
  iraq_war_worth_cost = "Iraq war worth its costs",
  join_euro = "Join euro",
  party_alignment_aca = "Democratic ACA retention platform",
  permit_police_violence = "Permit police violence",
  restore_voting_rights = "Restore voting rights after convictions",
  retain_aca = "Retain ACA",
  retain_academic_schools = "Retain academic/vocational differentiation",
  shared_curriculum_courses = "Share courses across schools",
  shared_school_campus = "Share school campuses",
  tougher_sentences = "Tougher sentences"
)
stopifnot(all(registry$axis %in% names(axis_labels)))
coding_rows <- character()
for (poll in analysis_polls) {
  r <- registry[registry$poll_id == poll & registry$construct == "policy", ]
  r <- r[order(r$item_id), ]
  coding_rows <- c(coding_rows, paste0(
    "\\multicolumn{3}{l}{\\textbf{", labels[poll], "}} \\\\*"
  ))
  for (i in seq_len(nrow(r))) {
    selected <- inventory$poll_id == poll &
      inventory$item_id == r$item_id[i]
    item <- inventory[selected, ]
    wording <- paste0(
      item$question_display, " Answer: ",
      item$correct_answer_display
    )
    if (poll == "northern-ireland-2007") {
      wording <- switch(r$item_id[i],
        knowledge_003 = paste(
          "Curriculum entitlement: a choice of at least 24 subjects",
          "for pupils aged 14 and above."
        ),
        knowledge_004 = "At least one third of subjects must be applied.",
        knowledge_005 = paste(
          "About three quarters of grammar-school pupils proceed",
          "to higher education (not a causal school effect)."
        ),
        wording
      )
    }
    if (poll == "vermont-energy-2007") {
      wording <- paste(
        "Efficiency program reduced the annual increase in electricity",
        "consumption by half; not total consumption. Flawed attribution",
        "identified by the original event report."
      )
    }
    aca <- poll == "america-in-one-room-2019" &&
      r$item_id[i] == "knowledge_005"
    if (aca) {
      wording <- paste(
        "ACA protections: parental coverage through age 26, no annual",
        "or lifetime coverage limits, no higher rates for women."
      )
    }
    if (poll == "amr-2024") {
      wording <- if (r$item_id[i] == "knowledge_003") {
        paste(
          "Antibiotics kill bacteria, not viruses, and can cause",
          "side effects and allergies."
        )
      } else {
        paste(
          "Vaccination, handwashing and hygienic food preparation",
          "prevent infection; archived wording conflict."
        )
      }
    }
    measure <- paste0(
      axis_labels[r$axis[i]], ". ",
      escape_tex(r$source_column[i]), ": + ",
      gsub("|", ",", r$positive_codes[i], fixed = TRUE), "; - ",
      gsub("|", ",", r$negative_codes[i], fixed = TRUE)
    )
    direction <- ifelse(r$direction[i] == "supports", "Favors", "Challenges")
    spec <- ifelse(r$specification[i] == "primary", "P", "E")
    coding_rows <- c(coding_rows, paste0(row_text(
      paste0(
        escape_tex(wording), " [", sub("knowledge_", "", r$item_id[i]), "]"
      ),
      measure, paste(direction, spec, sep = "; ")
    ), " \\\\"))
  }
  coding_rows <- c(coding_rows, "\\addlinespace")
}
writeLines(c(
  paste0(
    "\\begin{longtable}{",
    paste0(">{\\raggedright\\arraybackslash}p{", c(8, 4, 2),
      "cm}",
      collapse = ""
    ), "}"
  ),
  "\\caption{Adjudicated facts and baseline codes.}\\label{tab:registry}\\\\",
  paste0(
    "\\toprule Fact (historical key) & ",
    "Positive position; field and codes & Direction; set \\\\"
  ),
  "\\midrule\\endfirsthead",
  paste0(
    "\\toprule Fact (historical key) & ",
    "Positive position; field and codes & Direction; set \\\\"
  ),
  "\\midrule\\endhead", coding_rows, "\\bottomrule", "\\end{longtable}"
), "tabs/comparative_registry.tex")
coverage <- dplyr::count(inventory, poll_id, name = "inventory_items")
coverage <- coverage[order(labels[coverage$poll_id]), ]
mapped <- registry[registry$construct == "policy", ]
coverage$primary_items <- vapply(coverage$poll_id, function(p) {
  sum(mapped$poll_id == p & mapped$specification == "primary")
}, integer(1))
coverage$expanded_items <- vapply(coverage$poll_id, function(p) {
  sum(mapped$poll_id == p)
}, integer(1))
readr::write_csv(coverage, file.path(out, "inventory_coverage.csv"))
coverage_rows <- vapply(seq_len(nrow(coverage)), function(i) {
  row_text(
    escape_tex(labels[coverage$poll_id[i]]), coverage$inventory_items[i],
    coverage$primary_items[i], coverage$expanded_items[i]
  )
}, character(1))
writeLines(c(
  "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{8.5cm}rrr}",
  paste0(
    "\\caption{Full retained inventory and adjudicated coverage.}",
    "\\label{tab:coverage}\\\\"
  ),
  "\\toprule Study & Inventory & Primary & Expanded total \\\\",
  "\\midrule\\endfirsthead",
  "\\toprule Study & Inventory & Primary & Expanded total \\\\",
  "\\midrule\\endhead", paste0(coverage_rows, " \\\\"),
  "\\bottomrule", "\\end{longtable}"
), "tabs/comparative_coverage.tex")
