# Module 13: Regression Models for Count Data

**PA 766 — Advanced Quantitative Research**

## Overview

This tutorial uses the 2015 ICMA Sustainability Survey to model how many transportation actions a municipality reported adopting. We merge the survey with demographic data, fit a Poisson model, interpret expected counts, and briefly compare a negative binomial model.

## 1. Set up R

### Install packages once

Run this chunk only if the packages are not already installed.

```r
install.packages(c("rio", "dplyr", "ggplot2", "marginaleffects", "MASS"))
```

### Load packages

Run this chunk each time you start a new R session. We will call the negative binomial function with `MASS::glm.nb()` so we do not need to attach the entire MASS package.

```r
library(rio)
library(dplyr)
library(ggplot2)
library(marginaleffects)
```

## 2. Import the data

Use the ICMA workbook provided for class, `2015 sustainability full clean dataset.xlsx`, and the [demographic dataset](https://drive.google.com/file/d/1Xp_P0mhfIkyFw3KBo0oL1TQ5yBa5oP-q/view?usp=sharing). Download both files before running the code. Each `file.choose()` call opens a dialog where you select the indicated file.

```r
# Select the ICMA Excel workbook.
sustainability <- import(file.choose(), which = "Clean Data")

# Select the downloaded demographic dataset.
demographics <- import(file.choose())
```

The demographic dataset should contain `GEO_ID`, `MHIncome` (median household income), and `Pr_White` (percentage White). Check the variable names and units before continuing.

```r
names(sustainability)
summary(demographics[c("GEO_ID", "MHIncome", "Pr_White")])
```

## 3. Create a common geographic identifier

The ICMA file contains both municipalities and counties. Keep municipal records (`UTYPE == 1`) for this place-level exercise.

```r
sustainability <- sustainability %>%
  filter(UTYPE == 1)
```

A Census place identifier combines a two-digit state code with a five-digit place code. `sprintf()` restores leading zeros and stores the codes as text. The county code is not part of this identifier.

```r
sustainability <- sustainability %>%
  filter(!is.na(`STATE#`), !is.na(`FIPS#`)) %>%
  mutate(
    state_fips = sprintf("%02d", as.integer(`STATE#`)),
    place_fips = sprintf("%05d", as.integer(`FIPS#`)),
    PlaceFIPS = paste0(state_fips, place_fips)
  ) %>%
  relocate(PlaceFIPS)
```

In the demographic file, remove the Census prefix ending in `US` to obtain the same seven-digit identifier. Keep Census place records.

```r
demographics <- demographics %>%
  filter(grepl("^1600000US[0-9]{7}$", GEO_ID)) %>%
  mutate(PlaceFIPS = sub("^1600000US", "", GEO_ID)) %>%
  select(PlaceFIPS, MHIncome, Pr_White)

head(sustainability$PlaceFIPS)
head(demographics$PlaceFIPS)
```

## 4. Merge the datasets

Check repeated identifiers before merging. Distinct survey jurisdictions can share a Census place code, but the demographic file should have only one row per place. Investigate unexpected duplicates rather than automatically deleting them.

```r
sustainability %>% count(PlaceFIPS) %>% filter(n > 1)
demographics %>% count(PlaceFIPS) %>% filter(n > 1)
```

An inner join keeps survey records with a demographic match. `relationship = "many-to-one"` checks that each survey record matches at most one demographic row.

```r
df <- inner_join(
  sustainability,
  demographics,
  by = "PlaceFIPS",
  relationship = "many-to-one"
)

c(survey_rows = nrow(sustainability), matched_rows = nrow(df))

# Inspect survey jurisdictions without a demographic match.
anti_join(sustainability, demographics, by = "PlaceFIPS") %>%
  select(ID, JURISDICTION, STATE, PlaceFIPS)
```

## 5. Create the count outcome

Question 30 lists eight transportation actions adopted during the previous five years. Following the original classroom exercise, code a selected action (`1`) as 1 and an unselected option (`9`) as 0. Preserve actual missing or unexpected values as `NA`.

```r
action_vars <- paste0("q30_", letters[1:8])

lapply(df[action_vars], table, useNA = "ifany")

df <- df %>%
  mutate(across(all_of(action_vars), ~ case_when(
    .x == 1 ~ 1,
    .x == 9 ~ 0,
    TRUE ~ NA_real_
  )))

df$transportation_action_count <- rowSums(df[action_vars])
```

`rowSums()` adds the eight indicators. We leave `na.rm` at its default, `FALSE`, so an incomplete response does not become a smaller count.

Keep complete observations for the outcome and the predictors actually used in the model. Income must be positive before taking its natural logarithm. This exercise assumes `Pr_White` is a percentage from 0 to 100.

```r
df_complete <- df %>%
  filter(
    !is.na(transportation_action_count),
    is.finite(MHIncome), MHIncome > 0,
    is.finite(Pr_White), between(Pr_White, 0, 100)
  ) %>%
  mutate(ln_income = log(MHIncome))

summary(df_complete[c("transportation_action_count", "Pr_White", "ln_income")])
nrow(df_complete)
```

We do not drop records for missing population or education because those variables are not used below. The sample may therefore differ from the earlier classroom output.

## 6. Inspect the distribution

The bar chart shows how many municipalities recorded each count. The sample mean and variance describe the outcome, but their comparison alone does not diagnose overdispersion in a regression model.

```r
df_complete %>%
  summarise(
    mean_count = mean(transportation_action_count),
    variance_count = var(transportation_action_count),
    proportion_zero = mean(transportation_action_count == 0)
  )

ggplot(df_complete, aes(x = transportation_action_count)) +
  geom_bar(fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = 0:8) +
  labs(
    x = "Number of transportation actions",
    y = "Number of municipalities",
    title = "Transportation actions reported by municipalities"
  ) +
  theme_minimal()
```

## 7. Fit a Poisson regression model

The log link models the logarithm of the **expected count** as a linear function of the predictors. Keep the observed outcome on its original count scale, including zeros.

```r
poisson_model <- glm(
  transportation_action_count ~ Pr_White + ln_income,
  data = df_complete,
  family = poisson(link = "log")
)

summary(poisson_model)
nobs(poisson_model)
```

### Interpret the coefficients

Exponentiating a coefficient gives the ratio of expected counts for a one-unit increase in that predictor, holding the other predictor constant. For example, a ratio of 0.95 means an expected count that is 5% lower.

```r
b <- coef(poisson_model)[c("Pr_White", "ln_income")]

data.frame(
  term = names(b),
  count_ratio = exp(b),
  percent_change = 100 * (exp(b) - 1)
)
```

A one-unit increase in `Pr_White` is one percentage point. Because income is logged, a one-unit increase in `ln_income` means multiplying income by approximately 2.718. The following changes are easier to interpret:

```r
c(
  white_10_percentage_points =
    100 * (exp(unname(b["Pr_White"]) * 10) - 1),
  income_10_percent =
    100 * (1.10^unname(b["ln_income"]) - 1)
)
```

These results describe associations, holding the other predictor constant.

## 8. Check dispersion and compare a negative binomial model

Poisson regression assumes that the conditional variance equals the conditional mean. The Pearson dispersion ratio checks for extra variation after accounting for the predictors. A value substantially above 1 is a reason to investigate fit, not an automatic model-selection rule.

```r
pearson_residuals <- residuals(poisson_model, type = "pearson")
dispersion_ratio <- sum(pearson_residuals^2) / df.residual(poisson_model)
dispersion_ratio

plot(
  fitted(poisson_model), pearson_residuals,
  xlab = "Fitted expected count", ylab = "Pearson residual"
)
abline(h = 0, lty = 2)
```

Look for systematic patterns or unusually large residuals. Unaddressed overdispersion can make conventional Poisson standard errors too small.

Negative binomial regression allows additional variation while retaining the log link and the same count-ratio interpretation. Fit both models to the same observations before comparing AIC; lower AIC indicates better relative fit after accounting for model complexity.

```r
nb_model <- MASS::glm.nb(
  transportation_action_count ~ Pr_White + ln_income,
  data = df_complete
)

summary(nb_model)
AIC(poisson_model, nb_model)
```

Use diagnostics and plausible predictions alongside AIC. For the remaining examples, start with the Poisson model; switch to `nb_model` if your checks favor it. Confidence intervals depend on the chosen model and its assumptions.

```r
analysis_model <- poisson_model
# analysis_model <- nb_model
```

## 9. Plot average predicted counts

At each value of `Pr_White`, `avg_predictions()` sets that predictor to the same value for all municipalities, keeps their observed incomes, and averages the predicted counts. Use the middle 90% of observed percentages to avoid plotting the full 0–100 range where data may be sparse.

```r
white_range <- quantile(df_complete$Pr_White, c(0.05, 0.95))
white_grid <- seq(white_range[1], white_range[2], length.out = 51)

apreds <- avg_predictions(
  analysis_model,
  variables = list(Pr_White = white_grid),
  type = "response"
)

prediction_results <- as.data.frame(apreds)

ggplot(prediction_results, aes(x = Pr_White, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  labs(
    x = "White population (%)",
    y = "Average predicted number of actions",
    title = "Average predicted transportation actions"
  ) +
  theme_minimal()
```

The shaded area is a 95% confidence interval for the average predicted count, not a range for individual municipalities. Leave the vertical axis unrestricted so predictions or intervals outside 0–8 remain visible.

## 10. Estimate an average marginal effect

A marginal effect expresses the local change in the expected **number of actions** associated with a one-unit change in a predictor. `avg_slopes()` averages these slopes across municipalities at their observed predictor values.

```r
avg_slopes(
  analysis_model,
  variables = "Pr_White",
  type = "response"
)
```

For `Pr_White`, the estimate is in actions per percentage point. This is different from the percentage change in the expected count and from the average prediction curve above.

## 11. What to report

When presenting the model, report:

1. the outcome coding, geographic unit, and analysis sample size;
2. the model specification and why its fit is reasonable;
3. count ratios or meaningful percentage changes;
4. predicted counts or marginal effects with uncertainty; and
5. limitations, including missing responses, the eight-action upper limit, and possible dependence between jurisdictions.

```r
nobs(analysis_model)
```

For event counts observed over different amounts of time or population at risk, an exposure offset may be needed. We do not automatically use population as an offset for the number of different policies adopted.

### R documentation

- [Average predictions](https://marginaleffects.com/man/r/predictions.html)
- [Average marginal effects with `avg_slopes()`](https://marginaleffects.com/man/r/slopes.html)
- [Negative binomial regression with `glm.nb()`](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/glm.nb.html)
