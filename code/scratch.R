```{r}
#| include: false
#| echo: false
## sensitivity/specificity ---------------------------- (2024-09-25 15:40) @----
meta_sens <- meta::metaprop(tp, tp + fn,
                            data = clinical,
                            studlab = study,
                            sm = "PLOGIT",
                            method = "GLMM",
                            method.tau = "ML",
                            incr = 0,
                            hakn = TRUE,
                            prediction = TRUE
)

meta_spec <- meta::metaprop(tn, tn + fp,
                            data = clinical,
                            studlab = study,
                            sm = "PLOGIT",
                            method = "GLMM",
                            method.tau = "ML",
                            incr = 0,
                            hakn = TRUE,
                            subset = tn + fp != 0,
                            prediction = TRUE,
                            allstudies = TRUE
)

sens_spec <- as_tibble(meta_sens[c("studlab", "TE", "lower", "upper")]) |>
  rename(sens = TE, sens_low = lower, sens_up = "upper") |>
  left_join(as_tibble(meta_spec[c("studlab", "TE", "lower", "upper")]), by = "studlab") |>
  rename(spec = TE, spec_low = lower, spec_up = upper) |>
  mutate(
    across(-"studlab", ~ plogis(.)),
    # across(c(sens:spec_up), ~ replace_na_with(.x, 0)),
    sens_ci = sprintf("%1.2f (%1.2f-%1.2f)", sens, sens_low, sens_up),
    spec_ci = sprintf("%1.2f (%1.2f-%1.2f)", spec, spec_low, spec_up),
    across(c(sens_ci, spec_ci), ~ ifelse(.x == "NA (NA-NA)", " ", .x)),
    `     Sensitivity (95% CI)` = str_pad('', 60),
    `     Specificity (95% CI)` = str_pad('', 60),
    ` ` = str_pad('', 0),
    `   ` = str_pad('', 0),
    sens_ci = str_pad(sens_ci, width = 20, side = "left"),
    spec_ci = str_pad(spec_ci, width = 20, side = "left"),
    year = str_extract(studlab, "\\d{4}")
  ) |>
  rename(study = studlab) |>
  left_join(clinical |> select(study, tp:fp), by = "study") |>
  relocate(c(tp, fp, fn, tn), .after = study) |>
  rename(Study = study, TP = tp, FP = fp, FN = fn, TN = tn, `Sensitivity (95% CI)` = sens_ci, `Specificity (95% CI)` = spec_ci) |>
  mutate(
    Study = str_pad(Study, width = 30, side = "right"),
    across(c(TP:TN), ~ str_pad(.x, width = 3, side = "left"))) |>
  arrange(sens)

# Set-up theme
tm <- forest_theme(
  base_size = 12,
  refline_gp = gpar(lwd = 0, lty = "dashed", col = "white"),
  ci_fill = "blue",
  text_hjust = c(0, 1, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5)
)

clinical_plot <- forestploter::forest(sens_spec |> select(Study, TP, FP, FN, TN, `Sensitivity (95% CI)`, `Specificity (95% CI)`, `   `, `     Sensitivity (95% CI)`, ` `, `     Specificity (95% CI)`),
                                      est = list(
                                        sens_spec$sens,
                                        sens_spec$spec
                                      ),
                                      lower = list(
                                        sens_spec$sens_low,
                                        sens_spec$spec_low
                                      ),
                                      upper = list(
                                        sens_spec$sens_up,
                                        sens_spec$spec_up
                                      ),
                                      xlim = c(0, 1),
                                      ci_column = c(9, 11),
                                      ref_line = 1,
                                      sizes = 1,
                                      theme = tm
)

png("assets/clinical_plot.png", width = 15, height = 7.5, units = "in", res = 300)
print(clinical_plot)
dev.off()

trim_image("clinical_plot.png")

```



```{r}
#| eval: false
#| echo: false
#| include: false
library(grid)
library(forestploter)
library(naniar)


dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Indent the subgroup if there is a number in the placebo column
dt <- dt |>
  mutate(
    Subgroup = if_else(!is.na(Placebo), paste0("   ", Subgroup), Subgroup),
    across(c(Treatment, Placebo), ~replace_na_with(.x, "")),
    se = (log(hi) - log(est)) / 1.96,
    # Add a blank column for the forest plot to display CI.
    # Adjust the column width with space.
    ` ` = str_pad('', 100),
    # Create confidence interval column to display
    `HR (95% CI)` = if_else(!is.na(se), sprintf("%.2f (%.2f to %.2f)", est, low, hi), "")
  )

tm <- forest_theme(
  base_size = 10,
  refline_col = "red",
  arrow_type = "closed",
  footnote_gp = gpar(col = "blue", cex = 0.6)
)

p <- forestploter::forest(dt[, c(1:3, 20:21)],
  est = dt$est,
  lower = dt$low,
  upper = dt$hi,
  sizes = dt$se,
  ci_column = 4,
  ref_line = 1,
  arrow_lab = c("Placebo Better", "Treatment Better"),
  xlim = c(0, 6),
  ticks_at = c(0.5, 1, 2, 3, 4, 5, 6),
  footnote = "This is the demo data. Please feel free to change\nanything you want.",
  theme = tm
)

plot(p)
```

```{r}
#| eval: false
#| echo: false
#| include: false
library(grid)
library(forestploter)

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo),
  dt$Subgroup,
  paste0("   ", dt$Subgroup)
)

# NA to blank
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est)) / 1.96

# Add a blank column for the forest plot to display CI.
# Adjust the column width with space.
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
  sprintf(
    "%.2f (%.2f to %.2f)",
    dt$est, dt$low, dt$hi
  )
)

# Define theme
tm <- forest_theme(
  base_size = 10,
  refline_col = "red",
  arrow_type = "closed",
  footnote_gp = gpar(col = "blue", cex = 0.6)
)

p <- forest(dt[, c(1:3, 20:21)],
  est = dt$est,
  lower = dt$low,
  upper = dt$hi,
  sizes = dt$se,
  ci_column = 4,
  ref_line = 1,
  arrow_lab = c("Placebo Better", "Treatment Better"),
  xlim = c(0, 4),
  ticks_at = c(0.5, 1, 2, 3),
  footnote = "This is the demo data. Please feel free to change\nanything you want.",
  theme = tm
)

# Print plot
plot(p)

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Indent the subgroup if there is a number in the placebo column
dt <- dt |>
  mutate(
    Subgroup = if_else(!is.na(Placebo), paste0("   ", Subgroup), Subgroup),
    across(c(Treatment, Placebo), ~replace_na_with(.x, "")),
    se = (log(hi) - log(est)) / 1.96,
    # Add a blank column for the forest plot to display CI.
    # Adjust the column width with space.
    ` ` = str_pad('', 100),
    # Create confidence interval column to display
    `HR (95% CI)` = if_else(!is.na(se), sprintf("%.2f (%.2f to %.2f)", est, low, hi), ""),
    `  ` = str_pad('', 100),
  )

dt$`   ` <- paste(rep(" ", 20), collapse = " ")

# Set-up theme
tm <- forest_theme(
  base_size = 10,
  refline_col = "red",
  footnote_gp = gpar(col = "blue"),
  legend_name = "GP",
  legend_value = c("Trt 1", "Trt 2")
)

p <- forest(dt[, c(1:2, 20, 3, 22)],
  est = list(
    dt$est_gp1,
    dt$est_gp2,
    dt$est_gp3,
    dt$est_gp4
  ),
  lower = list(
    dt$low_gp1,
    dt$low_gp2,
    dt$low_gp3,
    dt$low_gp4
  ),
  upper = list(
    dt$hi_gp1,
    dt$hi_gp2,
    dt$hi_gp3,
    dt$hi_gp4
  ),
  ci_column = c(3, 5),
  ref_line = 1,
  arrow_lab = c("Placebo Better", "Treatment Better"),
  nudge_y = 0.2,
  x_trans = "log",
  theme = tm
)

plot(p)

```
