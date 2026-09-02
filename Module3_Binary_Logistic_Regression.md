# Module 3: Binary Logistic Regression

**PA 766 — Advanced Quantitative Research**

## Overview

This tutorial introduces binary logistic regression using the 2021 General Social Survey (GSS). We begin with one predictor, calculate predicted probabilities, add a second predictor, and conclude with average marginal effects.

By the end of the tutorial, you should be able to:

- recode a binary outcome as 0 and 1;
- visualize a binary outcome;
- fit binary logistic regression models with one or more predictors;
- calculate predicted probabilities; and
- summarize a model with odds ratios and average marginal effects.

## 1. Set up R

### Install packages once

Run this chunk only if the packages are not already installed.

```r
install.packages(c("rio", "dplyr", "ggplot2", "margins"))
```

### Load packages

Run this chunk each time you start a new R session.

```r
library(rio)
library(dplyr)
library(ggplot2)
library(margins)
```

## 2. Import and organize the data

Download `GSS2021.dta` from the [course data folder](https://drive.google.com/drive/folders/1HV7QRjEsw8_nd8VnrRwSYaHbRSMahsAQ) and save it in the same folder as this tutorial.

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
summary(GSS2021[c("abany", "age", "polviews")])
```

## 3. Create the binary outcome

We want to model whether a respondent supports allowing abortion if a woman wants one for any reason. The original variable, `abany`, is coded:

- `1`: Yes
- `2`: No

<p align="center"><img src="https://raw.githubusercontent.com/SerenaYKim/pa765-2024s/master/img/02/00.png" width="280" alt="Original coding of the abany variable"></p>

Logistic regression expects the outcome to be coded 0 and 1. We will preserve the original variable and create a new variable named `pro_choice`:

- `1`: Supports allowing abortion for any reason
- `0`: Does not support allowing abortion for any reason

```r
GSS2021 <- GSS2021 %>%
  mutate(
    pro_choice = case_when(
      abany == 1 ~ 1,
      abany == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
```

Check the recoding before fitting a model.

```r
table(GSS2021$pro_choice, useNA = "ifany")
prop.table(table(GSS2021$pro_choice, useNA = "no"))
```

## 4. Visualize the outcome and age

Because `pro_choice` can equal only 0 or 1, many observations would overlap in a standard scatterplot. `geom_jitter()` adds a small amount of random movement so the distribution of observations is visible.

```r
analysis_one <- GSS2021 %>%
  filter(!is.na(age), !is.na(pro_choice))
```

```r
ggplot(analysis_one, aes(x = age, y = pro_choice)) +
  geom_jitter(width = 0.25, height = 0.03, alpha = 0.35) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("Does not support", "Supports")
  ) +
  labs(
    x = "Age",
    y = "Supports abortion for any reason",
    title = "Support for abortion by age"
  ) +
  theme_minimal()
```

## 5. Fit a one-predictor logistic regression model

The model below estimates how age is associated with the probability that `pro_choice` equals 1.

```r
logit_age <- glm(
  pro_choice ~ age,
  data = analysis_one,
  family = binomial(link = "logit")
)

summary(logit_age)
```

### Calculate predicted probabilities

Model coefficients are expressed in log-odds, which are difficult to interpret directly. Predicted probabilities are often more intuitive.

```r
age_points <- data.frame(age = c(20, 40, 60))

age_points <- age_points %>%
  mutate(
    predicted_probability = predict(
      logit_age,
      newdata = age_points,
      type = "response"
    )
  )

age_points
```

Add the fitted probability curve to the observed data.

```r
ggplot(analysis_one, aes(x = age, y = pro_choice)) +
  geom_jitter(width = 0.25, height = 0.03, alpha = 0.2) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = TRUE
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  labs(
    x = "Age",
    y = "Predicted probability of support",
    title = "Predicted support for abortion by age"
  ) +
  theme_minimal()
```

## 6. Create a second predictor

The GSS variable `polviews` measures political ideology from 1 (extremely liberal) to 7 (extremely conservative).

<p align="center"><img src="https://github.com/user-attachments/assets/80cc15e8-6d21-473a-b328-fff3eb57f5f6" width="453" alt="Original coding of the polviews variable"></p>

Because `polviews` measures ideology rather than party identification, we will call the new variable `liberal` rather than `dems`:

- `1`: `polviews` is 1, 2, or 3
- `0`: `polviews` is 4, 5, 6, or 7
- `NA`: `polviews` is missing or outside the valid range

```r
GSS2021 <- GSS2021 %>%
  mutate(
    liberal = case_when(
      polviews %in% 1:3 ~ 1,
      polviews %in% 4:7 ~ 0,
      TRUE ~ NA_real_
    )
  )
```

Check both the number and proportion of observations in each category.

```r
table(GSS2021$liberal, useNA = "ifany")
prop.table(table(GSS2021$liberal, useNA = "no"))
```

> **Interpretation note:** This simplified indicator combines moderate and conservative respondents in the `0` category. It should not be interpreted as a direct measure of Democratic Party identification.

## 7. Fit a multiple-predictor logistic regression model

Create one analysis dataset containing complete observations for the outcome and both predictors.

```r
analysis_two <- GSS2021 %>%
  filter(
    !is.na(pro_choice),
    !is.na(age),
    !is.na(liberal)
  )
```

Estimate the relationship between abortion attitudes and age while accounting for political ideology.

```r
logit_model <- glm(
  pro_choice ~ age + liberal,
  data = analysis_two,
  family = binomial(link = "logit")
)

summary(logit_model)
```

### Convert coefficients to odds ratios

Exponentiating a logistic regression coefficient converts it from log-odds to an odds ratio.

```r
round(exp(coef(logit_model)), 3)
```

### Compare predicted probabilities

The following grid compares predicted probabilities at selected ages for respondents in the two ideology categories.

```r
prediction_grid <- expand.grid(
  age = c(20, 40, 60),
  liberal = c(0, 1)
)

prediction_grid <- prediction_grid %>%
  mutate(
    predicted_probability = predict(
      logit_model,
      newdata = prediction_grid,
      type = "response"
    ),
    ideology = ifelse(liberal == 1, "Liberal", "Moderate or conservative")
  ) %>%
  select(age, ideology, predicted_probability)

prediction_grid
```

## 8. Estimate average marginal effects

Average marginal effects summarize the average change in the predicted probability associated with a one-unit change in a predictor, holding the other variables at their observed values.

```r
margins_model <- margins(logit_model, type = "response")
summary(margins_model)
```

Plot the average marginal effects and their confidence intervals.

```r
plot(margins_model)
```

## 9. What to report

When presenting a binary logistic regression model, report:

1. how the outcome and predictors were coded;
2. the model specification and number of complete observations;
3. coefficient estimates or odds ratios with uncertainty;
4. predicted probabilities or marginal effects for substantive interpretation; and
5. the assumptions, limitations, and scope of the conclusions.

```r
nobs(logit_model)
```
