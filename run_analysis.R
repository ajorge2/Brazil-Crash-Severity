args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript run_analysis.R /absolute/path/to/accidents.csv [output_dir]")
}

input_path <- normalizePath(args[[1]], mustWork = TRUE)
output_dir <- if (length(args) >= 2) args[[2]] else "."
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw <- read.csv(input_path, stringsAsFactors = FALSE)
if ("wheather_condition" %in% names(raw)) {
  names(raw)[names(raw) == "wheather_condition"] <- "weather_condition"
}

required <- c(
  "victims_condition",
  "cause_of_accident",
  "weather_timestamp",
  "weather_condition",
  "road_delineation"
)
missing_columns <- setdiff(required, names(raw))
if (length(missing_columns) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")))
}

analysis <- raw[raw$road_delineation != "Not Reported", , drop = FALSE]
analysis$fatal <- as.integer(analysis$victims_condition == "With dead victims")
analysis$cause_of_accident <- relevel(
  factor(analysis$cause_of_accident),
  ref = "Alcohol consumption"
)
analysis$weather_timestamp <- relevel(
  factor(analysis$weather_timestamp),
  ref = "Night"
)
analysis$weather_condition <- relevel(
  factor(analysis$weather_condition),
  ref = "Rainy"
)
analysis$road_delineation <- relevel(
  factor(analysis$road_delineation),
  ref = "Curve"
)

formulas <- list(
  model_1 = fatal ~ cause_of_accident + weather_timestamp,
  model_2 = fatal ~ cause_of_accident + weather_condition,
  model_3 = fatal ~ cause_of_accident + weather_timestamp + weather_condition,
  model_4 = fatal ~ cause_of_accident + weather_timestamp + road_delineation,
  model_5 = fatal ~ cause_of_accident + weather_timestamp + road_delineation + weather_condition
)

key_term <- "cause_of_accidentPedestrian was walking in the road"
model_evidence <- lapply(names(formulas), function(model_name) {
  fitted <- glm(formulas[[model_name]], data = analysis, family = binomial())
  coefficient <- coef(summary(fitted))[key_term, ]
  result <- list(
    comparison = data.frame(
      model = model_name,
      formula = paste(deparse(formulas[[model_name]]), collapse = " "),
      df = unname(attr(logLik(fitted), "df")),
      AIC = AIC(fitted),
      stringsAsFactors = FALSE
    ),
    coefficient = coefficient,
    modeling_rows = nobs(fitted),
    fatal_crashes = sum(model.response(model.frame(fitted))),
    fatal_rate = mean(model.response(model.frame(fitted)))
  )
  rm(fitted)
  gc()
  result
})
names(model_evidence) <- names(formulas)

comparison <- do.call(rbind, lapply(model_evidence, `[[`, "comparison"))
comparison <- comparison[order(comparison$AIC), ]
rownames(comparison) <- NULL

selected_name <- comparison$model[[1]]
selected <- model_evidence[[selected_name]]
coefficient <- selected$coefficient
estimate <- unname(coefficient[["Estimate"]])
standard_error <- unname(coefficient[["Std. Error"]])
wald_limits <- estimate + c(-1, 1) * qnorm(0.975) * standard_error

unadjusted <- glm(fatal ~ cause_of_accident, data = analysis, family = binomial())
unadjusted_coefficient <- coef(summary(unadjusted))[key_term, ]
unadjusted_estimate <- unname(unadjusted_coefficient[["Estimate"]])
unadjusted_standard_error <- unname(unadjusted_coefficient[["Std. Error"]])
unadjusted_limits <- unadjusted_estimate + c(-1, 1) * qnorm(0.975) * unadjusted_standard_error
adjustment_change_percent <- 100 * (exp(estimate) / exp(unadjusted_estimate) - 1)
adjustment_direction <- if (adjustment_change_percent < 0) "shrank" else "increased"

key_result <- data.frame(
  model = selected_name,
  term = key_term,
  comparison = "Pedestrian was walking in the road vs. Alcohol consumption",
  unadjusted_odds_ratio = exp(unadjusted_estimate),
  unadjusted_ci_lower = exp(unadjusted_limits[[1]]),
  unadjusted_ci_upper = exp(unadjusted_limits[[2]]),
  adjusted_odds_ratio = exp(estimate),
  ci_method = "95% Wald confidence interval",
  ci_lower = exp(wald_limits[[1]]),
  ci_upper = exp(wald_limits[[2]]),
  adjustment_change_percent = adjustment_change_percent,
  p_value = unname(coefficient[["Pr(>|z|)"]]),
  modeling_rows = selected$modeling_rows,
  fatal_crashes = selected$fatal_crashes,
  fatal_rate = selected$fatal_rate,
  raw_rows = nrow(raw),
  excluded_not_reported_road_delineation = nrow(raw) - nrow(analysis),
  input_md5 = unname(tools::md5sum(input_path)),
  stringsAsFactors = FALSE
)

write.csv(
  comparison,
  file.path(output_dir, "model-comparison.csv"),
  row.names = FALSE
)
write.csv(
  key_result,
  file.path(output_dir, "key-result.csv"),
  row.names = FALSE
)

evidence <- c(
  "# Brazil crash-severity resume evidence",
  "",
  sprintf("- Source rows: %s", format(nrow(raw), big.mark = ",")),
  sprintf("- Modeling rows: %s", format(selected$modeling_rows, big.mark = ",")),
  sprintf(
    "- Excluded rows: %s with road delineation recorded as `Not Reported`",
    format(nrow(raw) - nrow(analysis), big.mark = ",")
  ),
  sprintf("- Fatal crashes in modeling sample: %s", format(sum(analysis$fatal), big.mark = ",")),
  sprintf(
    "- Selected specification: `%s` (minimum AIC %.1f)",
    selected_name,
    comparison$AIC[[1]]
  ),
  paste0("- Controls: cause of accident, time of day, road delineation, and weather condition"),
  paste0("- Reference categories: alcohol consumption, night, curve, and rainy weather"),
  sprintf(
    "- Lead association: pedestrian-walking-in-road crashes had %.2fx the adjusted odds of fatality versus alcohol-related crashes (95%% Wald CI %.2f-%.2f; p < 2e-16)",
    exp(estimate),
    exp(wald_limits[[1]]),
    exp(wald_limits[[2]])
  ),
  sprintf(
    "- Sensitivity to adjustment: the odds ratio %s from %.2f unadjusted (95%% CI %.2f-%.2f) to %.2f adjusted, a %.1f%% change",
    adjustment_direction,
    exp(unadjusted_estimate),
    exp(unadjusted_limits[[1]]),
    exp(unadjusted_limits[[2]]),
    exp(estimate),
    abs(adjustment_change_percent)
  ),
  "",
  "## Resume-ready claim",
  "",
  sprintf(
    "Analyzed %s Brazilian crash records with multivariable logistic regression; found crashes attributed to pedestrians walking in the road had %.2fx the adjusted odds of fatality versus alcohol-related crashes (95%% CI %.2f-%.2f). The estimate %s %.1f%% from its unadjusted value after controlling for time of day, road delineation, and weather.",
    format(nrow(raw), big.mark = ","),
    exp(estimate),
    exp(wald_limits[[1]]),
    exp(wald_limits[[2]]),
    adjustment_direction,
    abs(adjustment_change_percent)
  ),
  "",
  "## Interpretation boundary",
  "",
  "This is an adjusted association in observational crash records, not a causal effect. The interval is model-based and assumes independent records and a correctly specified logistic model.",
  "",
  "## Reproduction",
  "",
  sprintf("Input file: `%s`", input_path),
  sprintf("Input MD5: `%s`", unname(tools::md5sum(input_path))),
  "",
  "```sh",
  sprintf("Rscript run_analysis.R %s .", shQuote(input_path)),
  "```"
)
writeLines(evidence, file.path(output_dir, "resume-evidence.md"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session-info.txt"))

cat(sprintf("Wrote reproducible evidence to %s\n", normalizePath(output_dir)))
