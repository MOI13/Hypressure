library(rmarkdown)
library(here) 

rmd_incident    <- here("analysis", "02_hypertension_incidence.Rmd")
rmd_risk_factor <- here("analysis", "03_risk_factors_hypertension_onset.Rmd")
rmd_sbp_change  <- here("analysis", "04_risk_factors_sbp_change.Rmd")
output_dir <- here("report")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Clean Data: visit_long.RDS and QC summary
# -----------------------------------------------------------------------------
message("Running 01_make_data.R")

source(here("analysis", "01_make_data.R"))

# -----------------------------------------------------------------------------
# 2. Knit and Rename: Incident Hypertension Report
# -----------------------------------------------------------------------------
message("Knitting Report 1: 02_hypertension_incidence.Rmd")

render(
  input = rmd_incident,
  output_format = "html_document",
  output_dir = output_dir,
  output_file = "hypertension_incidence.html",
  quiet = FALSE
)

# -----------------------------------------------------------------------------
# 3. Knit and Rename: Risk Factor Analysis Report
# -----------------------------------------------------------------------------
message("Knitting Report 2: 03_risk_factors_hypertension_onset.Rmd")

render(
  input = rmd_risk_factor,
  output_format = "html_document",
  output_dir = output_dir,
  output_file = "risk_factors_hypertension_onset.html",
  quiet = FALSE
)

# -----------------------------------------------------------------------------
# 4. Knit and Rename: SBP Change Report
# -----------------------------------------------------------------------------
message("Knitting Report 3: 04_risk_factors_sbp_change.Rmd")

render(
  input = rmd_sbp_change,
  output_format = "html_document",
  output_dir = output_dir,
  output_file = "risk_factors_sbp_change.html",
  quiet = FALSE
)

message("Execution complete! All HTML reports successfully compiled to: ", output_dir)

# -----------------------------------------------------------------------------
# 5. Dashboard: bundle results from 01-04
# -----------------------------------------------------------------------------
message("Saving dashboard results")

dashboard_results <- list(
  qc        = readRDS(here("data", "intermediate", "01_qc_summary.RDS")),
  incidence = readRDS(here("data", "intermediate", "02_incidence.RDS")),
  onset     = readRDS(here("data", "intermediate", "03_hypertension_onset.RDS")),
  sbp       = readRDS(here("data", "intermediate", "04_sbp_change.RDS"))
)

saveRDS(dashboard_results, here("data", "intermediate", "dashboard_results.RDS"))
