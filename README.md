# Hypressure

Analysis of hypertension onset and blood pressure change in a synthetic adult cohort, with an interactive Shiny dashboard. Written in R.

**Live dashboard:** [https://shirley-song.shinyapps.io/dashboard_hypressure/](https://shirley-song.shinyapps.io/dashboard_hypressure/)

The project answers four questions:

1. How was the analysis dataset built, and which data quality issues were fixed?
2. Among participants without hypertension at entry, how often does hypertension develop, overall and by sex and age group, when it takes one versus two qualifying visits to confirm it?
3. Among participants without hypertension at entry, which baseline factors (age, sex, BMI) are associated with developing hypertension?
4. Among participants without hypertension at entry, how does systolic BP change over follow-up, and is the rate of change associated with baseline age, sex, and BMI, after accounting for antihypertensive medication use at each visit?

Hypertension is defined as blood pressure ≥130/80 mmHg or antihypertensive medication use. The data are synthetic and contain no real participant information.

---

## Project Structure

```
analysis/
├── 00_table_style.R                        # journal-style table settings
├── 00_figure_style.R                       # figure theme and color palettes
├── 01_make_data.R                          # Q1: data cleaning, Tables 1-2
├── 02_hypertension_incidence.Rmd           # Q2: incidence rates
├── 03_risk_factors_hypertension_onset.Rmd  # Q3: Cox models for hypertension onset
├── 04_risk_factors_sbp_change.Rmd          # Q4: mixed-effects model for SBP change
├── 05_render_report.R                      # runs 01-04 and compiles the reports
└── 05b_dashboard_hypressure.Rmd            # Shiny dashboard
data/raw/                                   # input data
report/                                     # HTML reports
```

---

## Reports
* `report/hypertension_incidence.html`
* `report/risk_factors_hypertension_onset.html`
* `report/risk_factors_sbp_change.html`

---

## How to Run
1. **Open the RStudio project and restart R** (Ctrl+Shift+F10).
2. **Compile the reports:**
   ```r
   source(here::here("analysis", "05_render_report.R"))
   ```
3. **Launch the dashboard locally:**
   ```r
   rmarkdown::run(here::here("analysis", "05b_dashboard_hypressure.Rmd"))
   ```