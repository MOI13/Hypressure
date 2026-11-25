# Clear memory
# rm(list = ls()); gc()

# ==============================================================================
# QUESTION: How was the analysis dataset built, and which data quality issues were fixed?
#
# DATA PREPROCESSING SUMMARY
# 1. OUTLIERS: Flagged (SBP <50|>300, DBP <30|>200) as NA.
# 2. SAME-DAY DUPLICATES: Consolidated multiple visits on the same date by:
#    - Keeping the clinic record (entry/ scheduled visit) if there is one,
#      otherwise the record with the least missingness in blood pressure/ body measurements.
#    - Collapsing medication records (any use on that day).
# 3. MEDICATION: Only recorded at clinic visits. NA at home/ unofficial visits
#    means not collected, set to 0 here (carried forward in the longitudinal analysis).
# 4. TEMPORAL ALIGNMENT: 
#    - Exit dates after the data cut (last recorded visit) are censored at the data cut.
#    - Updated exit_date to last follow-up for subjects who did not die.
#    - Confirmed entry day and no visits occurred post-death. 
# 5. BASELINE COVARIATES: BMI category and age group from the first visit,
#    built once here and used by all reports.
# 6. DESCRIPTIVE TABLES: Table 1 (baseline characteristics) and Table 2 (follow-up and visits)
#    for participants without hypertension at entry (analyzed in 02-04), shown on the dashboard (05).
# ==============================================================================


# Load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  DescTools, tidyverse, magrittr, janitor, readxl, here, purrr, knitr, kableExtra,
  gtsummary, flextable
)

source(here("analysis", "00_table_style.R"))


# Set constants -----------------------------------------------------------
DATA_FOLDER <- "data/raw/"

## "losaratan" is spelled this way in the source data
bp_meds <- c("amlodipine", "bisoprolol", "captopril", "carvedilol", 
             "enalapril", "furosemide", "hctz", "lisinopril", 
             "losaratan", "metoprolol")

other_meds <- c("aspirin", "atorvastatin", "simvastatin", "metformin", "glyburide")

## BP medications only recorded in free text (warfarin is not a BP med)
bp_meds_regex <- "ramipril|olmesartan"


# Data Loading-----------------------------------------------------------
baseline <- read_csv(here(DATA_FOLDER, "baseline_df.csv"))
visits   <- read_csv(here(DATA_FOLDER, "visits_df.csv"))
outcomes <- read_csv(here(DATA_FOLDER, "outcomes_df.csv"))


# QC -----------------------------------------------------------
# head(baseline)
summary(baseline)
n_distinct(baseline$id)
baseline %>% get_dupes(id)
table(baseline$sex)
# 8 spellings, including 1/2 codes


# head(visits)
n_distinct(visits$id)
# hist(visits$sbp1)
visits %>% 
  tabyl(visit_type) %>%
  kbl(
    align = "crr",
    digits = 3
  )
print(unique(tolower(visits$free_text_med)))
# "ramipril"   "warfarin"   "olmesartan"

## medications are missing for every home/ unofficial visit
visits %>%
  mutate(visit_type = tolower(visit_type)) %>%
  group_by(visit_type) %>%
  summarise(med_missing = mean(is.na(amlodipine)))


# head(outcomes)
n_distinct(outcomes$id)
outcomes %>% get_dupes(id)
table(outcomes$exit_type)
# death study_exit 
# 708         92 


# Data Preprocessing-----------------------------------------------------------
## Baseline -----------------------------------------------------------
baseline2 <- baseline %>%
  mutate(
    entry_date = dmy(entry_date),
    sex = str_to_sentence(sex),
    ## assumed: 1 = Male, 2 = Female (coding not documented in the source data)
    sex = case_when(
      sex == "1" ~ "Male",
      sex == "2" ~ "Female",
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ sex 
    )
  )
# table(baseline2$sex)


## Visit -----------------------------------------------------------
visits2 <- visits %>%
  mutate(
    visit_date = dmy(visit_date),
    visit_type = tolower(visit_type)
  ) %>%
  mutate(
    # Blood Pressure 
    # Set outliers SBP (<50 or >300) and DBP (<30 or >200) to NA
    across(starts_with("sbp"), ~if_else(.x < 50 | .x > 300, NA_real_, .x)),
    across(starts_with("dbp"), ~if_else(.x < 30 | .x > 200, NA_real_, .x))
  ) %>%
  
  # Medication 
  # only recorded at clinic visits, NA at home/ unofficial visits = not collected
  # set to 0 here so these visits don't count as medication use
  mutate(across(
    c(all_of(bp_meds), all_of(other_meds)),
    ~replace_na(.x, 0)
  )) %>%
  distinct() %>%
  arrange(id, visit_date)


## Same day visit records
visits_same_day <- visits2 %>%
  group_by(id, visit_date) %>%
  filter(n() > 1) %>%
  arrange(id, visit_type, visit_date)
# view(visits_same_day)
# visits_same_day %>% 
#   summarise(types = paste(sort(visit_type), collapse = "+"), .groups = "drop") %>% 
#   count(types, sort = TRUE)

visits3 <- visits2 %>%
  mutate(
    na_count  = rowSums(is.na(select(., sbp1, sbp2, sbp3, dbp1, dbp2, dbp3, weight))),
    is_clinic = str_detect(visit_type, "^(entry|m\\d+)")
  ) %>%
  ## Same day records: keep the clinic record first, then the one with minimal missingness,
  ## so a scheduled visit on the same day as a home visit isn't relabeled as home
  group_by(id, visit_date) %>%
  arrange(desc(is_clinic), na_count, .by_group = TRUE) %>%
  summarise(
    across(c(visit_type, sbp1, sbp2, sbp3, dbp1, dbp2, dbp3, weight), first),
    ## Collapse Medication use: any use on that day
    across(all_of(c(bp_meds, other_meds)), ~ max(.x, na.rm = TRUE)),
    free_text_med = paste(unique(na.omit(free_text_med)), collapse = "; "),
    .groups = "drop"
  ) %>%
  # Calculate Mean Blood Pressure
  mutate(
    mean_sbp = rowMeans(select(., sbp2, sbp3), na.rm = TRUE),
    mean_dbp = rowMeans(select(., dbp2, dbp3), na.rm = TRUE)
  ) %>%
  # Hypertension Medication indicator: use if any bp meds is 1 OR specific meds found in free text column
  mutate(
    any_bp_med = if_else(
      rowSums(select(., all_of(bp_meds))) > 0 | 
        str_detect(tolower(free_text_med), bp_meds_regex), 
      1, 0
    ),
    any_bp_med = replace_na(any_bp_med, 0)
  ) %>%
  arrange(id, visit_date)



## Outcome -----------------------------------------------------------
## Data cut: last recorded visit in the dataset
DATA_CUT <- max(visits3$visit_date)

outcomes2 <- outcomes %>%
  mutate(
    exit_date = dmy(exit_date),
    ## 8 exit dates in the raw data are after the data cut (one death dated 12/12/2026)
    ## treat them as censored at the data cut
    exit_type = if_else(exit_date > DATA_CUT, "study_exit", exit_type),
    exit_date = pmin(exit_date, DATA_CUT)
  )
# outcomes2 %>% filter(exit_date == DATA_CUT)


## Merge -----------------------------------------------------------
visits_long <- baseline2 %>%
  left_join(visits3, by = "id") %>%
  left_join(outcomes2, by = "id")

# Entry day Check
## entry day all aligned between visit and baseline table
entry_check <- visits_long %>%
  filter(visit_type == "entry") %>%
  select(id, entry_date, visit_date) %>%
  mutate(
    date_diff = as.numeric(visit_date - entry_date),
    is_diff = date_diff != 0
  ) %>% 
  filter(is_diff != 0)

# Exit day Check for alive subjects
## need to replace exit day with actual last follow up days
exit_check <- visits_long %>%
  group_by(id) %>%
  filter(exit_type != "death") %>%
  summarise(
    exit_date = first(exit_date),
    last_visit_date = max(visit_date, na.rm = TRUE),
    exit_reason = first(exit_type),
    .groups = "drop"
  ) %>%
  mutate(
    date_diff = as.numeric(last_visit_date -  exit_date),
    is_diff = date_diff != 0
  ) %>% 
  filter(is_diff != 0)


# Exit day Check for death subjects
## no death before last visit
exit_check2 <- visits_long %>%
  group_by(id) %>%
  filter(exit_type == "death") %>%
  summarise(
    exit_date = first(exit_date),
    last_visit_date = max(visit_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date_diff = as.numeric(last_visit_date -  exit_date),
  ) %>% 
  filter(date_diff > 0)


visits_long2 <- visits_long %>%
  group_by(id) %>%
  filter(visit_date >= entry_date) %>%
  mutate(
    last_visit = max(visit_date, na.rm = TRUE),
    exit_date = case_when(
      # Update exit_date if have records in outcome form if:
      # 1. subjects didn't die
      # 2. The last visit date is different with current exit_date
      exit_type != "death" & last_visit != exit_date ~ last_visit,
      # Update exit_date if no outcome form:
      # assume alive, use last visit day
      is.na(exit_type) ~ last_visit,
      TRUE ~ exit_date 
    ),
    exit_type = coalesce(exit_type, "study_exit")
  ) %>%
  select(-last_visit) %>%
  ungroup()

summary(visits_long2)
table(visits_long2$exit_type)
visits_long2 %>%
  summarise(both_sbp_and_dbp_na = sum(is.na(mean_sbp) & is.na(mean_dbp)))


## Baseline covariates -----------------------------------------------------------
## from the first visit, shared by the incidence, risk factor and SBP change reports
baseline_cov <- visits_long2 %>%
  group_by(id) %>%
  arrange(visit_date) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    id,
    bmi = weight / ((height / 100)^2),
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25.0 ~ "Normal weight",
      bmi >= 25.0 & bmi < 30.0 ~ "Overweight",
      bmi >= 30.0 ~ "Obesity",
      TRUE ~ NA_character_
    ),
    bmi_category = factor(bmi_category, levels = c("Normal weight", "Underweight", "Overweight", "Obesity")),
    age_group = cut(age, 
                    breaks = c(18, 30, 40, 50, 60, Inf),
                    labels = c("18-29 years", "30-39 years", "40-49 years", "50-59 years", "60+ years"),
                    right = FALSE)
  )
# table(baseline_cov$bmi_category, useNA = "ifany")

visits_long3 <- visits_long2 %>%
  left_join(baseline_cov, by = "id")


## Descriptive tables -----------------------------------------------------------
## participants analyzed in 02-04: no hypertension at entry
entry_visit <- visits_long3 %>%
  group_by(id) %>%
  arrange(visit_date) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    ## same definition as the incidence report:
    ## missing entry BP with no BP medication counts as no hypertension
    htn_entry = coalesce(mean_sbp >= 130, FALSE) | coalesce(mean_dbp >= 80, FALSE) | any_bp_med == 1,
    sex = factor(sex, levels = c("Female", "Male"))
  )

at_risk_ids <- entry_visit %>% filter(!htn_entry) %>% pull(id)
n_excluded  <- sum(entry_visit$htn_entry)

## plain gtsummary theme here so statistics stay out of the row labels (they are in the notes)
reset_gtsummary_theme()


# Table 1: baseline characteristics
tbl_baseline <- entry_visit %>%
  filter(id %in% at_risk_ids) %>%
  select(age, age_group, sex, bmi, bmi_category, mean_sbp, mean_dbp) %>%
  tbl_summary(
    label = list(
      age          ~ "Age, years",
      age_group    ~ "Age group",
      sex          ~ "Sex",
      bmi          ~ "BMI, kg/m²",
      bmi_category ~ "BMI category",
      mean_sbp     ~ "Systolic BP, mmHg",
      mean_dbp     ~ "Diastolic BP, mmHg"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ 1),
    missing_text = "Missing"
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  ## drop the default gtsummary footnotes and abbreviation line
  remove_footnote_header() %>%
  remove_abbreviation() %>%
  as_flex_table() %>%
  journal_table(
    title = "Table 1. Baseline characteristics of participants without hypertension at entry",
    notes = c(
      "Values are mean (SD) or No. (%). All values are from the entry visit; BP is the mean of the second and third readings.",
      sprintf("%s participants with hypertension at entry (blood pressure ≥130/80 mmHg or antihypertensive medication use) were excluded. Participants with missing entry blood pressure and no medication were included.",
              format(n_excluded, big.mark = ",")),
      "Abbreviations: BMI, body mass index; BP, blood pressure."
    )
  )


# Table 2: follow-up and visits
fu_person <- visits_long3 %>%
  filter(id %in% at_risk_ids) %>%
  mutate(
    visit_setting = case_when(
      visit_date == entry_date | str_detect(visit_type, "^(entry|m\\d+)") ~ "Clinic",
      str_detect(visit_type, "home") ~ "Home",
      TRUE ~ "Unofficial"
    )
  ) %>%
  group_by(id) %>%
  summarise(
    fu_years     = as.numeric(first(exit_date) - first(entry_date)) / 365.25,
    n_visits     = n(),
    n_clinic     = sum(visit_setting == "Clinic"),
    n_home       = sum(visit_setting == "Home"),
    n_unofficial = sum(visit_setting == "Unofficial"),
    exit_status  = if_else(first(exit_type) == "death", "Died", "Alive at end of follow-up"),
    .groups = "drop"
  )

tbl_followup_gt <- fu_person %>%
  select(fu_years, n_visits, n_clinic, n_home, n_unofficial, exit_status) %>%
  tbl_summary(
    type = list(c(n_visits, n_clinic, n_home, n_unofficial) ~ "continuous"),
    label = list(
      fu_years     ~ "Follow-up, years",
      n_visits     ~ "All visits",
      n_clinic     ~ "Clinic",
      n_home       ~ "Home",
      n_unofficial ~ "Unofficial",
      exit_status  ~ "Status at end of follow-up"
    ),
    statistic = list(all_continuous() ~ "{median} ({p25}-{p75})"),
    digits = list(fu_years ~ 1, c(n_visits, n_clinic, n_home, n_unofficial) ~ 0)
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  ## for formatting: visit types grouped under one header row
  modify_table_body(~ {
    idx <- which(.x$variable == "n_visits")[1]
    bind_rows(
      .x %>% filter(row_number() < idx),
      tibble(variable = "visits_header", var_type = "continuous", 
             var_label = "Visits per participant", label = "Visits per participant", row_type = "label"),
      .x %>% filter(row_number() >= idx)
    )
  })

visit_rows <- which(tbl_followup_gt$table_body$variable %in% c("n_visits", "n_clinic", "n_home", "n_unofficial"))

tbl_followup <- tbl_followup_gt %>%
  remove_footnote_header() %>%
  remove_abbreviation() %>%
  as_flex_table() %>%
  journal_table(
    title = "Table 2. Follow-up and visits of participants without hypertension at entry",
    notes = c(
      "Values are median (Q1-Q3) or No. (%).",
      "Clinic visits: the entry visit and scheduled visits at months 12 to 84, where blood pressure and medication are both recorded. Home and unofficial visits are visit types recorded as such in the source data; medication is not recorded at these visits.",
      sprintf("Follow-up runs from entry to death or the last recorded visit, with a data cut of %s. Total follow-up: %s person-years.",
              format(DATA_CUT, "%d %b %Y"),
              formatC(sum(fu_person$fu_years), format = "f", digits = 1, big.mark = ","))
    ),
    indent_rows = visit_rows
  )

# tbl_baseline
# tbl_followup


## QC summary for the dashboard -----------------------------------------------------------
n_sbp_outlier <- visits %>%
  summarise(across(starts_with("sbp"), ~ sum(.x < 50 | .x > 300, na.rm = TRUE))) %>%
  sum()

n_dbp_outlier <- visits %>%
  summarise(across(starts_with("dbp"), ~ sum(.x < 30 | .x > 200, na.rm = TRUE))) %>%
  sum()

qc_summary <- tribble(
  ~Step,                                                          ~Count,
  "Participants",                                                 n_distinct(baseline$id),
  "Visit records in raw data",                                    nrow(visits),
  "Exact duplicate visit records removed",                        nrow(visits) - nrow(visits2),
  "Records removed by combining same-day visits",                 nrow(visits2) - nrow(visits3),
  "Out-of-range SBP readings set to missing",                     n_sbp_outlier,
  "Out-of-range DBP readings set to missing",                     n_dbp_outlier,
  "Sex values recoded to Male/ Female",                           sum(!baseline$sex %in% c("Male", "Female")),
  "Exit dates after the data cut, censored at the data cut",      sum(dmy(outcomes$exit_date) > DATA_CUT),
  "Participants without an outcome record (assumed alive)",       n_distinct(baseline$id) - nrow(outcomes),
  "Visits in the analysis dataset",                               nrow(visits_long3)
)

qc_results <- list(
  question = "How was the analysis dataset built, and which data quality issues were fixed?",
  answer   = sprintf(
    "The analysis dataset includes %s participants and %s visits. %s out-of-range blood pressure readings were set to missing, %s records were removed by combining same-day visits, and follow-up was censored at the last recorded visit (%s).",
    format(n_distinct(baseline$id), big.mark = ","),
    format(nrow(visits_long3), big.mark = ","),
    format(n_sbp_outlier + n_dbp_outlier, big.mark = ","),
    format(nrow(visits2) - nrow(visits3), big.mark = ","),
    format(DATA_CUT, "%d %b %Y")
  ),
  qc = qc_summary,
  tables = list(baseline = tbl_baseline, followup = tbl_followup)
)


# Save -----------------------------------------------------------
saveRDS(visits_long3, here::here("data", "intermediate", "visit_long.RDS"))
saveRDS(qc_results, here("data", "intermediate", "01_qc_summary.RDS"))