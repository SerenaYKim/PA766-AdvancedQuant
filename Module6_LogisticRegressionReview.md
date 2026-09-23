# Module 6, Binary and Ordinal Logistic Regression Review (Optional)

**PA 766 — Advanced Quantitative Research**

## Overview

This tutorial uses the 2014 Baylor Religion Survey to examine two outcomes: support for concealed carry (binary) and support for government surveillance to prevent terrorism (ordinal). Both models use the same supernatural-evil index and controls.

The binary exercise adapts a concealed-carry model from [Ellison et al. (2021)](https://doi.org/10.1016/j.ssresearch.2021.102595). The ordinal exercise extends the topic to a different outcome. Both use unweighted, complete-case analyses rather than the paper's weighted, multiply imputed analysis. Run the sections in order.

## 1. Set up R

Install these packages once, if needed.

```r
install.packages(c("haven", "dplyr", "ggplot2", "ordinal", "marginaleffects"))
```

Load the packages each time you start a new R session.

```r
library(haven)
library(dplyr)
library(ggplot2)
library(ordinal)
library(marginaleffects)
```

## 2. Import the data

In Google Colab with an R runtime, upload `BRS2014_original.sav` using the Files panel. The code below reads it from Colab's `/content` folder. Uploaded files must be uploaded again after the runtime is reset. Data and documentation are available from [ARDA](https://www.thearda.com/data-archive?fid=BRS2014&tab=3).

```r
data_path <- "/content/BRS2014_original.sav"

if (!file.exists(data_path)) {
  stop("BRS2014_original.sav was not found. Upload the file to Colab and check data_path.")
}

BRS2014 <- read_sav(data_path)
```

`read_sav()` converts declared SPSS missing values to `NA`. Sort the columns alphabetically and inspect the variables we will use.

```r
BRS2014 <- BRS2014 %>%
  select(sort(names(.)))

summary(BRS2014[c(
  "Q73H", "Q75_1A", "Q23A", "Q23C", "Q23G", "Q31", "Q4", "AGE", "Q77"
)])
```

## 3. Prepare the predictors

The supernatural-evil index averages three belief items: `Q23A` (Devil/Satan), `Q23C` (Hell), and `Q23G` (demons). Each ranges from 1 (Absolutely not) to 4 (Absolutely). First, retain valid numeric responses.

```r
BRS2014 <- BRS2014 %>%
  mutate(
    across(
      all_of(c("Q23A", "Q23C", "Q23G")),
      ~ ifelse(.x %in% 1:4, as.numeric(.x), NA_real_)
    )
  )
```

Following the paper, average the available items for each respondent. Leave the index missing when all three items are missing.

```r
evil_items <- BRS2014 %>%
  select(Q23A, Q23C, Q23G)

BRS2014$evil_items_answered <- rowSums(!is.na(evil_items))
BRS2014$supernatural_evil <- rowMeans(evil_items, na.rm = TRUE)
BRS2014$supernatural_evil[BRS2014$evil_items_answered == 0] <- NA_real_

summary(BRS2014$supernatural_evil)
```

Prepare the controls: political ideology (1 = Extremely conservative to 7 = Extremely liberal), attendance (0 = Never to 8 = Several times a week), age, and gender. Exclude age 0 and use Male as the gender reference category.

```r
BRS2014 <- BRS2014 %>%
  mutate(
    political_ideology = ifelse(Q31 %in% 1:7, as.numeric(Q31), NA_real_),
    attendance = ifelse(Q4 %in% 0:8, as.numeric(Q4), NA_real_),
    age = ifelse(AGE %in% 18:99, as.numeric(AGE), NA_real_),
    gender = factor(
      ifelse(Q77 %in% 1:2, as.numeric(Q77), NA_real_),
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )
```

Treating ideology and attendance as numeric scores assumes a constant log-odds change per category. An attendance unit is one category, not one additional service per week.

## 4. Prepare the binary outcome: concealed carry

`Q73H` asks whether respondents favor or oppose laws allowing citizens to carry concealed guns. Inspect the original coding: 1 = Favor and 2 = Oppose.

```r
table(BRS2014$Q73H, useNA = "ifany")
```

There are 838 favor responses, 664 oppose responses, and 70 missing values. Recode favor as 1 and oppose as 0 so the model estimates the probability of favoring the policy.

```r
BRS2014 <- BRS2014 %>%
  mutate(
    concealed_carry = case_when(
      Q73H == 1 ~ 1,
      Q73H == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(BRS2014$concealed_carry, useNA = "ifany")
```

## 5. Fit the binary logistic regression model

Keep complete observations for this outcome and the five predictors. This produces 1,351 observations. Each model uses its own complete-case sample.

```r
analysis_binary <- BRS2014 %>%
  select(
    concealed_carry, supernatural_evil,
    political_ideology, attendance, age, gender
  ) %>%
  filter(if_all(everything(), ~ !is.na(.x)))

nrow(analysis_binary)
```

Fit a binary logistic regression using `glm()` with a binomial family and logit link.

```r
binary_model <- glm(
  concealed_carry ~ supernatural_evil + political_ideology +
    attendance + age + gender,
  data = analysis_binary,
  family = binomial(link = "logit")
)

summary(binary_model)
```

A positive coefficient indicates higher log odds of favoring concealed carry, holding the other predictors constant.

## 6. Interpret binary-model odds ratios

Exponentiate the coefficients and calculate 95% Wald confidence intervals.

```r
binary_log_odds <- coef(binary_model)
binary_se <- sqrt(diag(vcov(binary_model)))

binary_odds_ratios <- data.frame(
  term = names(binary_log_odds),
  odds_ratio = exp(binary_log_odds),
  conf_low = exp(binary_log_odds - qnorm(0.975) * binary_se),
  conf_high = exp(binary_log_odds + qnorm(0.975) * binary_se),
  row.names = NULL
)

binary_odds_ratios
```

For supernatural evil, an odds ratio above 1 indicates higher odds of favoring concealed carry per one-unit increase in the index. The paper reports 1.38 in its fully adjusted model; our simpler specification need not match it. An odds ratio of 1.38 means 38% higher odds, not a 38-percentage-point increase in probability.

## 7. Calculate binary-model predicted probabilities

Compare index scores of 1 through 4 for a 50-year-old woman who is politically moderate and attends services several times a year.

```r
binary_profiles <- data.frame(
  supernatural_evil = 1:4,
  political_ideology = 4,
  attendance = 3,
  age = 50,
  gender = factor(rep("Female", 4), levels = levels(analysis_binary$gender))
)

binary_predictions <- predictions(
  binary_model,
  newdata = binary_profiles,
  type = "response"
)

binary_predictions
```

`estimate` is the probability of favoring the policy; the probability of opposing it is `1 - estimate`. Plot these profile-specific predictions with their confidence intervals.

```r
as.data.frame(binary_predictions) %>%
  ggplot(aes(x = supernatural_evil, y = estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.08) +
  scale_x_continuous(breaks = 1:4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Belief in supernatural evil",
    y = "Predicted probability of favoring concealed carry",
    title = "Predicted support for concealed carry",
    subtitle = "Woman, age 50, politically moderate; attends several times a year"
  ) +
  theme_minimal()
```

## 8. Assess binary-model fit and marginal effects

Fit an intercept-only baseline on the same sample and calculate McFadden's pseudo R-squared. It measures improvement over the baseline, not the proportion of variance explained.

```r
binary_null <- glm(
  concealed_carry ~ 1,
  data = analysis_binary,
  family = binomial(link = "logit")
)

binary_mcfadden_R2 <- 1 - as.numeric(logLik(binary_model)) /
  as.numeric(logLik(binary_null))

binary_mcfadden_R2
```

Calculate the average slope of the probability of favoring concealed carry with respect to supernatural evil. Multiply the estimate by 100 to express it in percentage points per index unit.

```r
binary_ame <- avg_slopes(
  binary_model,
  variables = "supernatural_evil",
  type = "response"
)

binary_ame
```

For an exact change rather than a derivative, compare index scores of 3 and 2 while retaining each respondent's other predictor values, then average the probability differences.

```r
binary_difference <- avg_comparisons(
  binary_model,
  variables = list(supernatural_evil = c(2, 3)),
  type = "response"
)

binary_difference
```

## 9. Prepare the ordinal outcome

`Q75_1A` asks respondents to rate their agreement with this statement:

> The government should be able to monitor everyone's email and other online activities if officials say this might prevent future terrorist attacks.

Responses range from **1 = Strongly disagree** to **5 = Strongly agree**. Inspect the frequencies, including missing values.

```r
table(BRS2014$Q75_1A, useNA = "ifany")
```

The five category counts are 512, 442, 258, 207, and 109, with 44 missing responses. Retain valid values and create an ordered factor so R recognizes the ranking.

```r
BRS2014 <- BRS2014 %>%
  mutate(
    surveillance = ordered(
      ifelse(Q75_1A %in% 1:5, as.numeric(Q75_1A), NA_real_),
      levels = 1:5,
      labels = c(
        "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"
      )
    )
  )

levels(BRS2014$surveillance)
table(BRS2014$surveillance, useNA = "ifany")
```

Plot the distribution in its ordered categories.

```r
BRS2014 %>%
  filter(!is.na(surveillance)) %>%
  ggplot(aes(x = surveillance)) +
  geom_bar() +
  labs(
    x = "Support for government surveillance",
    y = "Number of respondents",
    title = "Surveillance to prevent terrorism: 2014 Baylor Religion Survey"
  ) +
  theme_minimal()
```

## 10. Fit an ordinal logistic regression model

Keep respondents with complete values for the ordinal outcome and predictors. This produces 1,369 observations; missingness differs from the binary outcome. The index can still be based on fewer than three answered items.

```r
analysis_ordinal <- BRS2014 %>%
  select(
    surveillance, supernatural_evil,
    political_ideology, attendance, age, gender
  ) %>%
  filter(if_all(everything(), ~ !is.na(.x)))

nrow(analysis_ordinal)
table(analysis_ordinal$surveillance)
```

Fit a proportional-odds model using `clm()` with a logit link.

```r
ordinal_model <- clm(
  surveillance ~ supernatural_evil + political_ideology +
    attendance + age + gender,
  data = analysis_ordinal,
  link = "logit"
)

summary(ordinal_model)
```

A positive predictor coefficient indicates a shift toward greater support for surveillance, holding the other predictors constant. The four thresholds separate adjacent outcome categories; they are not predictor effects.

## 11. Interpret proportional odds ratios

Exponentiate the predictor coefficients and calculate 95% Wald confidence intervals. Select `beta` to exclude the thresholds.

```r
log_odds <- ordinal_model$beta
standard_errors <- sqrt(diag(vcov(ordinal_model)))[names(log_odds)]

odds_ratios <- data.frame(
  term = names(log_odds),
  odds_ratio = exp(log_odds),
  conf_low = exp(log_odds - qnorm(0.975) * standard_errors),
  conf_high = exp(log_odds + qnorm(0.975) * standard_errors),
  row.names = NULL
)

odds_ratios
```

An odds ratio above 1 indicates higher odds of being above any outcome cutoff rather than at or below it. The proportional-odds assumption makes this ratio the same across all four cutoffs. An odds ratio is not a percentage-point change in probability.

## 12. Check the proportional-odds assumption

Use likelihood-ratio tests to assess whether allowing a predictor's effect to vary across cutoffs improves fit.

```r
nominal_test(ordinal_model)
```

A small p-value suggests that the proportional-odds assumption may be inadequate for that predictor. A large p-value does not prove the assumption. If a test reports a fitting warning or no result, it is inconclusive. If the assumption is questionable, treat the following results as a demonstration and consider a partial proportional-odds model for substantive analysis.

## 13. Calculate predicted probabilities

Compare supernatural-evil scores of 1 through 4 for a 50-year-old woman who is politically moderate and attends services several times a year. Omit the outcome from `newdata` so `predict()` returns all five category probabilities.

```r
prediction_data <- data.frame(
  supernatural_evil = 1:4,
  political_ideology = 4,
  attendance = 3,
  age = 50,
  gender = factor(rep("Female", 4), levels = levels(analysis_ordinal$gender))
)

predicted_probabilities <- predict(
  ordinal_model,
  newdata = prediction_data,
  type = "prob"
)$fit

predicted_probabilities
rowSums(predicted_probabilities)
```

Each row contains the probabilities for one profile and should sum to 1. These are profile-specific predictions, not averages across respondents.

Arrange the probabilities for plotting, retaining the outcome order.

```r
probability_plot <- data.frame(
  supernatural_evil = rep(prediction_data$supernatural_evil, times = 5),
  response = factor(
    rep(colnames(predicted_probabilities), each = nrow(prediction_data)),
    levels = levels(analysis_ordinal$surveillance)
  ),
  probability = as.vector(predicted_probabilities)
)

ggplot(probability_plot, aes(
  x = supernatural_evil, y = probability, color = response
)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Belief in supernatural evil",
    y = "Predicted probability",
    color = "Response",
    title = "Predicted support for government surveillance",
    subtitle = "Woman, age 50, politically moderate; attends several times a year"
  ) +
  theme_minimal()
```

## 14. Calculate McFadden's pseudo R-squared

Fit a thresholds-only baseline on the same sample and compare its log likelihood with the fitted model's log likelihood.

```r
ordinal_null <- clm(
  surveillance ~ 1,
  data = analysis_ordinal,
  link = "logit"
)

ordinal_mcfadden_R2 <- 1 - as.numeric(logLik(ordinal_model)) /
  as.numeric(logLik(ordinal_null))

ordinal_mcfadden_R2
```

McFadden's pseudo R-squared measures improvement over the baseline; it is not the proportion of variance explained.

## 15. Estimate average marginal effects

Calculate the average slope of each category's probability with respect to supernatural-evil beliefs, averaging over the analysis sample.

```r
average_marginal_effects <- avg_slopes(
  ordinal_model,
  variables = "supernatural_evil",
  type = "prob"
)

average_marginal_effects
```

Multiply an estimate by 100 to express it in percentage points per index unit. These are derivatives, not exact one-unit differences. Across the five categories, the effects should sum to approximately zero.

For an exact comparison, calculate the average probability difference between index scores of 3 and 2, retaining each respondent's other predictor values.

```r
average_probability_differences <- avg_comparisons(
  ordinal_model,
  variables = list(supernatural_evil = c(2, 3)),
  type = "prob"
)

average_probability_differences
```

## 16. Compare and report the models

| Feature | Binary model | Ordinal model |
| --- | --- | --- |
| Outcome | Concealed-carry support: oppose/favor | Surveillance support: five ordered categories |
| Function | `glm(..., family = binomial)` | `clm(..., link = "logit")` |
| Odds ratio | Odds of favoring versus opposing | Odds of being above versus at/below any cutoff |
| Predicted probabilities | Favor; oppose is its complement | One probability for each of five categories |
| Marginal effects | Effect on probability of favoring | Separate effect for each category |

Report each model's outcome coding, sample size, predictors, focal odds ratio with uncertainty, and probability-based interpretation. For the ordinal model, also report the proportional-odds assessment. Both analyses are unweighted and use complete cases; the index can use partially answered items.

**Discussion:** How is supernatural-evil belief associated with each policy attitude? Explain why an odds ratio and a probability change describe different quantities. These cross-sectional associations do not establish causation. Because the outcomes and samples differ, do not use their pseudo R-squared values to decide which model is better.
