# PA 766 | Staggered difference-in-differences with SUD treatment services

**Goal:** Recognize staggered treatment timing, calculate one DiD comparison, and distinguish group-time estimates from a pooled event study.

## Research question

How does the supplied measure of SUD treatment services change after a state's Medicaid expansion? For this classroom exercise, use the supplied outcome and treatment coding as given.

The data follow **3,134 counties annually from 2005 through 2021**, with 53,278 county-year observations.

| Variable | Meaning |
|:--|:--|
| `GEOID` | County identifier |
| `Year` | Observation year |
| `first_treat` | First treatment year; 0 is the untreated code |
| `num_service` | Supplied total across service categories, not a count of unique facilities |
| `STATEFP` | State identifier; used for state-clustered standard errors |

The workflow moves from a simple DiD comparison to cohort-time estimates, an overall ATT, and a pooled event study.

## Setup

```r
install.packages("did")    # Install the DiD estimation and plotting package once
install.packages("readr")  # Install the CSV-reading package once
```

## 1. Load the data and identify cohorts

```r
library(readr)  # Make read_csv() available in this R session
library(did)    # Make att_gt(), aggte(), and ggdid() available

df_merged_oct24 <- read_csv(
  "sud_services_by_county.csv",    # Read the CSV from the working directory
  show_col_types = FALSE  # Hide the column-type message; this does not change the data
)

county_cohorts <- unique(
  df_merged_oct24[c("GEOID", "first_treat")]  # Keep county ID and adoption year
)  # Remove repeated county/adoption-year pairs so each county is counted once

table(county_cohorts$first_treat)  # Count counties in each adoption cohort
range(df_merged_oct24$Year)        # Show the earliest and latest observation years
```

Here is the treatment timeline, built from the CSV. **U means untreated and T means treated.** The 2005–2013 column combines nine years because every cohort is untreated in each of those years under the supplied coding. Counts refer to unique counties.

| Cohort (`first_treat`) | Counties | 2005–2013 | 2014 | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 |
|:--|--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| Untreated code (0) | 1,070 | U | U | U | U | U | U | U | U | U |
| Starts in 2014 | 1,182 | U | T | T | T | T | T | T | T | T |
| Starts in 2015 | 189 | U | U | T | T | T | T | T | T | T |
| Starts in 2016 | 120 | U | U | U | T | T | T | T | T | T |
| Starts in 2019 | 149 | U | U | U | U | U | U | T | T | T |
| Starts in 2020 | 166 | U | U | U | U | U | U | U | T | T |
| Starts in 2021 | 192 | U | U | U | U | U | U | U | U | T |
| Starts in 2023 | 66 | U | U | U | U | U | U | U | U | U |

## 2. Calculate one DiD by hand

Compare the cohort starting in 2014 with counties coded `first_treat = 0`, using 2013 as the before year and 2014 as the after year.

```r
example_data <- subset(
  df_merged_oct24,  # Start with the full data
  first_treat %in% c(0, 2014) &  # Keep original code 0 and the 2014 cohort
    Year %in% c(2013, 2014)     # AND keep only the before and after years
)

four_means <- aggregate(
  num_service ~ first_treat + Year,  # Split the outcome by cohort AND year
  data = example_data,              # Use the selected rows
  FUN = mean                        # Calculate the average within each group-year
)
print(four_means)  # Display the four averages for the DiD calculation
```

Here `%in%` means “is one of these values,” `c()` collects values, and `&` requires both conditions. In `aggregate()`, the formula groups observations by `first_treat` and `Year`; it does not estimate a regression.

The DiD estimate is the treated group's change minus the untreated group's change:

$$
\widehat{ATT}=(37.78342-39.80626)-(14.05234-13.79813)
=-2.27705.
$$

Under the DiD assumptions, expansion reduced the average service-category total by approximately **2.28 units per county** in 2014 among counties first treated that year, relative to what would have occurred without expansion. This is not a percentage change.

The package estimate below is slightly different (−2.22707) because it also includes the 2023 adopters as untreated during the sample. No additional recoding is needed for this exercise.

## 3. Estimate and read group-time effects

### 3.1. Fit the model

```r
set.seed(766)  # Make the random bootstrap draws reproducible for this setup

mw.attgt <- att_gt(
  yname = "num_service",           # Outcome whose treatment effect we estimate
  gname = "first_treat",           # First treatment year for each county
  idname = "GEOID",                # County ID: connects the same county over time
  tname = "Year",                  # Calendar year of the observation
  xformla = ~1,                    # Intercept only: no control variables/covariates
  data = df_merged_oct24,          # Data frame containing these variables
  panel = TRUE,                    # The same counties are observed over time
  control_group = "nevertreated"  # Compare with counties untreated throughout the sample
)

summary(mw.attgt)  # Print group-time effects, uncertainty, and available diagnostics
```

`~1` means an intercept-only formula, not “one control variable.” It estimates unadjusted DiD comparisons. With covariates, the formula becomes something like `~ income_baseline + poverty_baseline`; Section 7 explains this extension.

Each post-treatment ATT(g,t) is the estimated effect in calendar year t for counties first treated in year g. With no anticipation, the baseline for post-treatment comparisons is g − 1. The method compares changes, so groups do not need equal baseline outcome levels.

### 3.2. Read one output row

Run this **after** `att_gt()`. It reads the fitted results; it does not estimate another model.

```r
att_table <- data.frame(
  cohort = mw.attgt$group,  # Adoption cohort associated with each estimate
  year = mw.attgt$t,       # Outcome year associated with each estimate
  ATT = mw.attgt$att       # Estimated group-time treatment effect
)
subset(
  att_table,
  cohort == 2014 & year == 2014  # Display only the first-year effect for the 2014 cohort
)
```

The selected ATT is **−2.227068**: the estimated effect in 2014 for counties first treated in 2014. The summary displays the same estimate rounded to **−2.2271**.

Read the corresponding row in `summary(mw.attgt)` for uncertainty:

| Cohort | Year | ATT | Standard error | 95% simultaneous band |
|:--|:--|--:|--:|:--|
| 2014 | 2014 | −2.2271 | 0.4805 | [−3.7730, −0.6811] * |

The entire band is below zero, so this estimate is statistically significant at the 5% level. The extracted three-column table contains only the estimate, not its uncertainty. A row number such as `9` is just a row label.

**Reporting sentence:** Among counties first treated in 2014, Medicaid expansion was associated with 2.23 fewer service-category-total units per county in 2014, relative to the estimated untreated outcome (95% simultaneous confidence band: [−3.77, −0.68]). A causal interpretation requires the DiD assumptions.

### 3.3. Plot the group-time effects

```r
ggdid(mw.attgt)  # Plot the fitted effects in separate panels by adoption cohort
```

You can run `ggdid(mw.attgt)` immediately after `att_gt()`. Neither `summary()` nor the extracted table is required for plotting. Start without `ylim` so all estimates and bands remain visible. Changing plot limits changes the display, not the estimates.

### 3.4. Check the pre-treatment comparisons

Read the pre-treatment rows and joint pre-test before interpreting the post-treatment results causally. With the default `base_period = "varying"`, pre-treatment estimates compare adjacent untreated years. They are diagnostic comparisons, not policy effects. The saved output reports a joint pre-test p-value below .001, raising concerns about comparable untreated trends. Failure to reject would not prove parallel trends either.

### Model defaults used above

The notebook leaves several defaults implicit:

| Default | Simple explanation |
|:--|:--|
| `anticipation = 0` | Assume no outcome response before the treatment year. |
| `base_period = "varying"` | Use adjacent-year pre-treatment comparisons; post-treatment effects still use g − 1. |
| `alp = 0.05` | Use a 5% significance level. |
| `bstrap = TRUE`, `cband = TRUE` | Use multiplier-bootstrap inference and simultaneous confidence bands for group-time effects. |
| `biters = 1000` | Use 1,000 bootstrap repetitions. |
| `est_method = "dr"` | Use the built-in doubly robust estimator; with no covariates, it reduces to unadjusted DiD. |
| `allow_unbalanced_panel = FALSE` | Retain counties observed in every period used in the panel analysis. |
| No `weightsname` | Give counties equal observation weights, rather than population weights. |
| No additional `clustervars` | Use county-unit inference; do not automatically cluster by state. |

A `*` in the group-time summary means the reported 95% simultaneous band excludes zero. These bands account for examining multiple group-time estimates together. They are not simply ATT ± 1.96 × standard error.

## 4. Estimate the overall ATT across cohorts

The next step summarizes the estimated effects across adoption cohorts.

```r
mw.overall <- aggte(
  mw.attgt,        # Start with the group-time effects already estimated
  type = "group"  # Average within each cohort, then combine cohort averages
)
summary(mw.overall)  # Report the overall ATT and each cohort's average effect

ggdid(mw.overall)  # Plot each cohort's average effect, not a single overall point
```

`type = "group"` first averages each cohort's observed post-treatment effects. It then combines those cohort averages using weights proportional to cohort size. Read the **overall ATT** in `summary(mw.overall)` for a single summary across treated cohorts. It is in `num_service` units per county, not percentages.

Earlier cohorts have more observed post-treatment years, so their averages cover longer follow-up periods. This summary answers a different question from the dynamic average at a common exposure duration. Its single overall estimate has an overall confidence interval; do not confuse that with the simultaneous bands across multiple cohort estimates.

## 5. Add `aggte()` for a pooled event study

**Add `aggte()`.** `ggdid(mw.attgt)` displays group-time estimates. A pooled event-study plot requires dynamic aggregation.

```r
mw.dynamic <- aggte(
  mw.attgt,          # Start with the estimated group-time effects
  type = "dynamic"  # Pool cohorts at the same time relative to treatment
)

summary(mw.dynamic)  # Print event-time effects and their uncertainty
ggdid(mw.dynamic)    # Plot event time on the horizontal axis
```

Dynamic aggregation combines eligible cohorts at the same time relative to treatment. For treated cohorts:

**Event time = observation year − first treatment year.**

| Event time | Meaning |
|:--|:--|
| -2 | Two years before treatment |
| 0 | First treatment year |
| 2 | Two years after treatment |

## 6. Optional: use a fixed pre-treatment reference period

The model above retains `base_period = "varying"`, the default. Its pre-treatment estimates compare adjacent pre-treatment periods. They are diagnostic comparisons, not effects of treatment before it happens.

For a conventional event-study reference period, optionally rerun the model with `base_period = "universal"`. With no anticipation, event time -1 is the reference period. This changes the pre-treatment comparisons but leaves post-treatment ATT(g,t) estimates unchanged.

```r
mw.attgt.universal <- att_gt(
  yname = "num_service",           # Same outcome
  gname = "first_treat",           # Same adoption cohorts
  idname = "GEOID",                # Same county panel identifier
  tname = "Year",                  # Same observation years
  xformla = ~1,                    # No covariates
  data = df_merged_oct24,          # Same dataset
  panel = TRUE,                    # County panel
  control_group = "nevertreated", # Same control group
  base_period = "universal"       # Fixed pre-treatment reference at event time -1
)

mw.dynamic.universal <- aggte(
  mw.attgt.universal,  # Aggregate the newly estimated reference-period specification
  type = "dynamic"    # Pool by time relative to treatment
)
ggdid(mw.dynamic.universal)  # Plot the version normalized at event time -1
```

## 7. How to cluster standard errors by state

Counties in the same state share a Medicaid expansion decision and may share economic or policy shocks. Their observations should not automatically be treated as independent evidence across counties. State clustering allows dependence among counties and years within a state when estimating uncertainty.

Use `clustervars = "STATEFP"` in `att_gt()`. The CSV already contains this state identifier. Keep `idname = "GEOID"`: the panel unit remains the county. **Do not replace the county ID with the state ID.**

```r
set.seed(766)  # Make bootstrap draws reproducible for this specification

mw.attgt.state <- att_gt(
  yname = "num_service",           # Outcome remains the service-category total
  gname = "first_treat",           # First treatment year
  idname = "GEOID",                # County ID: still the observation/panel unit
  tname = "Year",                  # Calendar year
  xformla = ~1,                    # No covariates: isolate the clustering change
  data = df_merged_oct24,          # Original data already include STATEFP
  panel = TRUE,                    # Same counties followed over time
  control_group = "nevertreated", # Same comparison group as the original model
  clustervars = "STATEFP",         # Allow dependence within each state
  bstrap = TRUE,                   # Use the multiplier bootstrap for inference
  biters = 1000,                   # Number of bootstrap repetitions
  cband = TRUE,                    # Request simultaneous group-time confidence bands
  alp = 0.05                       # 5% significance level: 95% confidence
)

summary(mw.attgt.state)  # Inspect estimates and state-clustered uncertainty
ggdid(mw.attgt.state)    # Plot the group-time effects with the updated bands

mw.overall.state <- aggte(
  mw.attgt.state,  # Inherit state clustering from this fitted object
  type = "group"  # Cohort averages plus an overall ATT
)
summary(mw.overall.state)  # Overall result using state-clustered inference

mw.dynamic.state <- aggte(
  mw.attgt.state,    # Again use the state-clustered fitted object
  type = "dynamic"  # Pool effects at each event time
)
summary(mw.dynamic.state)  # Event-time estimates and state-clustered uncertainty
ggdid(mw.dynamic.state)    # Plot the resulting pooled event study
```

With the same data and model, changing only the clustering level leaves the ATT point estimates unchanged, but can change standard errors, confidence bands, and significance. State-clustered uncertainty is not guaranteed to be larger for every estimate. Clustering addresses dependence; it does not adjust for confounding or make parallel trends hold. Cohorts with only a few adopting states can still have limited information for inference.

Specify state clustering when fitting `att_gt()`, then pass that fitted object to `aggte()`. Do not try to add a new clustering variable only at the aggregation or plotting step. Calling `ggdid(mw.attgt)` again still plots the old model; use the new object names shown above.

## Check the pre-treatment test

```r
mw.attgt$Wpval  # Extract its p-value
```

Run this after fitting `mw.attgt`. It extracts the joint pre-treatment test p-value from the original model. If it prints `0`, report **p < .001**: the value is rounded. The rejection raises concerns about parallel untreated trends; inspect which cohorts and years drive the differences.

`Anticipation Periods: 0` is an assumption of no effect before adoption, and `Doubly Robust` describes the estimator. Neither establishes parallel trends. Any changes to timing, comparison counties, or pre-treatment covariates should have a substantive justification beyond increasing the p-value.

If you fitted the state-clustered version, use `mw.attgt.state$Wpval` to inspect that model's test instead. Pre-treatment tests cannot establish the unobserved post-treatment counterfactual trends; see the [package pre-testing guide](https://bcallaway11.r-universe.dev/did/doc/pre-testing.html).
