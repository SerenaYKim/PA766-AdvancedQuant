# Module 4: Ordinal Logistic Regression

**PA 766 — Advanced Quantitative Research**

## Overview

This tutorial introduces ordinal logistic regression using the 2021 General Social Survey (GSS). We examine concern about environmental issues, fit proportional-odds models with one and multiple predictors, calculate predicted probabilities, assess model fit, and estimate average marginal effects.

By the end of the tutorial, you should be able to:

- identify an ordinal dependent variable and preserve the order of its categories;
- fit ordinal logistic regression models with one or more predictors;
- calculate and interpret predicted probabilities for every outcome category;
- calculate McFadden's pseudo R-squared; and
- estimate and interpret average marginal effects for an ordinal outcome.

## 1. Set up R

### Install packages once

Run this chunk only if the packages are not already installed.

```r
install.packages(c(
  "rio",
  "dplyr",
  "ggplot2",
  "ordinal",
  "pscl",
  "marginaleffects"
))
```

### Load packages

Run this chunk each time you start a new R session.

```r
library(rio)
library(dplyr)
library(ggplot2)
library(ordinal)
library(marginaleffects)
```

## 2. Import and organize the data

Download `GSS2021.dta` from the [course data folder](https://drive.google.com/drive/folders/1dNo0FPMM7_fCJCShJnb9cQgtIIteXTeF?usp=drive_link) and save it in the same folder as this tutorial.

```r
data_path <- "GSS2021.dta"

if (!file.exists(data_path)) {
  stop("GSS2021.dta was not found. Save it in the same folder as this Markdown file.")
}

GSS2021 <- import(data_path)
```

Reorder the columns alphabetically so variables are easier to locate.

```r
GSS2021 <- GSS2021 %>%
  select(sort(names(.)))
```

Inspect the variables used in this tutorial.

```r
summary(GSS2021[c("grncon", "educ", "tvhours")])
```

## 3. Understand the dependent variable

The focal variable is `grncon`, which asks respondents how concerned they are about environmental issues. The valid responses range from 1 (not at all concerned) to 5 (very concerned).

<p align="center"><img src="https://github.com/SerenaYKim/pa765-2024s/blob/master/img/05/01.png" width="420" alt="Original coding of the grncon variable"></p>

Examine the observed frequencies, including missing values.

```r
frequency <- table(GSS2021$grncon, useNA = "ifany")
frequency
```

## 4. Convert `grncon` to an ordered factor

A factor is a categorical variable with a fixed set of levels. An **ordered factor** also tells R that the categories have a meaningful ranking. For `grncon`, a response of 5 represents more concern than a response of 4, which represents more concern than a response of 3, and so on.

An ordinal regression model requires the response to be a factor. If R does not recognize the response as a factor, it returns an error such as:

> `Error: response in 'formula' needs to be a factor`

Retain valid values from 1 through 5, treat all other values as missing, and convert the result to an ordered factor.

```r
GSS2021 <- GSS2021 %>%
  mutate(
    grncon = ordered(
      ifelse(grncon %in% 1:5, as.numeric(grncon), NA_real_),
      levels = 1:5,
      labels = c(
        "1 - Not at all concerned",
        "2",
        "3",
        "4",
        "5 - Very concerned"
      )
    )
  )
```

Confirm the category order and inspect the distribution.

```r
levels(GSS2021$grncon)
is.ordered(GSS2021$grncon)
table(GSS2021$grncon, useNA = "ifany")
```

Visualize the ordinal outcome.

```r
GSS2021 %>%
  filter(!is.na(grncon)) %>%
  ggplot(aes(x = grncon)) +
  geom_bar() +
  labs(
    x = "Concern about environmental issues",
    y = "Number of respondents",
    title = "Environmental concern in the 2021 GSS"
  ) +
  theme_minimal()
```

## 5. Fit a one-predictor ordinal logistic regression model

Create an analysis dataset containing complete observations for environmental concern and education.

```r
analysis_one <- GSS2021 %>%
  filter(!is.na(grncon), !is.na(educ))
```

Estimate how education is associated with environmental concern using a cumulative logit model. The default `clm()` model is a proportional-odds model.

```r
ordinal_educ <- clm(
  grncon ~ educ,
  data = analysis_one,
  link = "logit"
)

summary(ordinal_educ)
```

In the parameterization used by `clm()`, a positive coefficient indicates that higher values of the predictor are associated with greater probability of being in a higher outcome category.

## 6. Calculate predicted probabilities

Model coefficients are expressed on the log-odds scale. Predicted probabilities are easier to interpret because they show the estimated probability of each response category.

Calculate the predicted probabilities for a respondent with 12 years of education.

```r
education_point <- data.frame(educ = 12)

predicted_probabilities <- predict(
  ordinal_educ,
  newdata = education_point,
  type = "prob"
)$fit

predicted_probabilities
rowSums(predicted_probabilities)
```

The five probabilities should sum to 1, apart from minor rounding differences.

## 7. Fit an ordinal logistic regression model with multiple predictors

Create one analysis dataset containing complete observations for the outcome and both predictors.

```r
analysis_two <- GSS2021 %>%
  filter(
    !is.na(grncon),
    !is.na(educ),
    !is.na(tvhours)
  )
```

Estimate the relationship between environmental concern and education while accounting for hours of television watched per day.

```r
ordinal_model <- clm(
  grncon ~ educ + tvhours,
  data = analysis_two,
  link = "logit"
)

summary(ordinal_model)
```

## 8. Calculate McFadden's pseudo R-squared

McFadden's pseudo R-squared compares the fitted model's log likelihood with the log likelihood of an intercept-only model. It is useful for comparing fit, but it should not be interpreted as the proportion of variance explained in the same way as ordinary least squares R-squared.

```r
pseudo_R2 <- pscl::pR2(ordinal_model)
pseudo_R2
```

The `McFadden` row is the statistic of interest.

## 9. Estimate average marginal effects

For an ordinal outcome, a predictor can decrease the probability of some response categories while increasing the probability of others. The `marginaleffects` package calculates an average marginal effect separately for each category.

```r
average_marginal_effects <- avg_slopes(
  ordinal_model,
  variables = c("educ", "tvhours"),
  type = "prob"
)

average_marginal_effects
```

For a continuous predictor, each estimate is the average change in the predicted probability of the listed outcome category associated with a one-unit increase in that predictor. Across all outcome categories, the marginal effects for a predictor should sum to approximately zero because the category probabilities must sum to 1.

If a one-unit discrete change is easier to explain than a derivative, use average comparisons instead.

```r
average_one_unit_changes <- avg_comparisons(
  ordinal_model,
  variables = list(
    educ = 1,
    tvhours = 1
  ),
  type = "prob"
)

average_one_unit_changes
```

## 10. What to report

When presenting an ordinal logistic regression model, report:

1. the order and coding of the outcome categories;
2. the model specification and number of complete observations;
3. coefficient estimates or proportional odds ratios with uncertainty;
4. predicted probabilities or marginal effects for substantive interpretation;
5. a model-fit measure, when useful; and
6. the proportional-odds assumption, limitations, and scope of the conclusions.

```r
nobs(ordinal_model)
```
