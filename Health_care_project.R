# ============================================================
# ELDERCARE STUDY ANALYSIS PIPELINE
# Target journal style: BMC Public Health
# Study: Health-system readiness, workforce capacity, and care quality
# ============================================================

# ============================================================
# 0. SETUP
# ============================================================

packages <- c(
  "readxl", "dplyr", "stringr", "writexl", "psych",
  "gtsummary", "flextable", "officer", "broom",
  "ggplot2", "tidyr", "patchwork", "ggtext", "mediation"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(psych)
library(gtsummary)
library(flextable)
library(officer)
library(broom)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggtext)
library(mediation)

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)
dir.create("outputs/data", showWarnings = FALSE)

# ============================================================
# STEP 1: DATA CODING
# ============================================================

raw <- read_excel("Elder care study.xlsx")

expected_names <- c(
  "age", "gender", "residence", "education", "income",
  "functional_limitation", "homecare_received", "care_frequency",
  "care_available", "care_affordable", "emergency_access", "unmet_need",
  "respectful_care", "timely_care", "safe_care", "continuity_care",
  "communication", "responsiveness", "personal_care", "satisfaction",
  "caregiver_training", "caregiver_skills", "caregiver_workload",
  "caregiver_burnout", "fair_payment", "caregiver_availability",
  "policy_awareness", "service_integration", "referral_system",
  "primary_care_support", "monitoring", "government_regulation",
  "rural_inequality", "income_inequality", "financial_support",
  "national_priority"
)

if (ncol(raw) != length(expected_names)) {
  stop("Column number mismatch. Dataset must contain exactly 36 columns.")
}

names(raw) <- expected_names

likert_code <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x == "strongly disagree" ~ 1,
    x == "disagree" ~ 2,
    x %in% c("neutral", "neither agree nor disagree") ~ 3,
    x == "agree" ~ 4,
    x == "strongly agree" ~ 5,
    TRUE ~ NA_real_
  )
}

yesno_code <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x == "yes" ~ 1,
    x == "no" ~ 0,
    TRUE ~ NA_real_
  )
}

coded <- raw %>%
  mutate(
    age = as.numeric(age),
    
    gender = case_when(
      str_to_lower(str_trim(gender)) == "male" ~ 1,
      str_to_lower(str_trim(gender)) == "female" ~ 2,
      str_to_lower(str_trim(gender)) == "other" ~ 3,
      TRUE ~ NA_real_
    ),
    
    residence = case_when(
      str_to_lower(str_trim(residence)) == "urban" ~ 0,
      str_to_lower(str_trim(residence)) == "rural" ~ 1,
      TRUE ~ NA_real_
    ),
    
    education = case_when(
      str_detect(str_to_lower(education), "no formal") ~ 0,
      str_detect(str_to_lower(education), "primary") ~ 1,
      str_detect(str_to_lower(education), "secondary") ~ 2,
      str_detect(str_to_lower(education), "higher|college|university") ~ 3,
      TRUE ~ NA_real_
    ),
    
    income = case_when(
      str_detect(str_to_lower(income), "low") ~ 1,
      str_detect(str_to_lower(income), "middle") ~ 2,
      str_detect(str_to_lower(income), "high") ~ 3,
      TRUE ~ NA_real_
    ),
    
    functional_limitation = yesno_code(functional_limitation),
    homecare_received = yesno_code(homecare_received),
    unmet_need = yesno_code(unmet_need),
    caregiver_training = yesno_code(caregiver_training),
    
    care_frequency = case_when(
      str_to_lower(str_trim(care_frequency)) == "rarely" ~ 1,
      str_to_lower(str_trim(care_frequency)) == "occasionally" ~ 2,
      str_to_lower(str_trim(care_frequency)) == "once a week" ~ 3,
      str_to_lower(str_trim(care_frequency)) == "several times a week" ~ 4,
      str_to_lower(str_trim(care_frequency)) == "daily" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    across(
      c(
        care_available, care_affordable, emergency_access,
        respectful_care, timely_care, safe_care, continuity_care,
        communication, responsiveness, personal_care, satisfaction,
        caregiver_skills, caregiver_workload, caregiver_burnout,
        fair_payment, caregiver_availability,
        policy_awareness, service_integration, referral_system,
        primary_care_support, monitoring, government_regulation,
        rural_inequality, income_inequality, financial_support,
        national_priority
      ),
      likert_code
    ),
    caregiver_workload_rev = 6 - caregiver_workload,
    caregiver_burnout_rev  = 6 - caregiver_burnout
  )

write_xlsx(coded, "outputs/data/eldercare_coded_dataset.xlsx")

# ============================================================
# STEP 2: CREATE INDEX SCORES
# ============================================================

data_indexed <- coded %>%
  mutate(
    access_score = rowMeans(
      dplyr::select(., care_available, care_affordable, emergency_access),
      na.rm = TRUE
    ),
    
    care_quality_index = rowMeans(
      dplyr::select(
        ., respectful_care, timely_care, safe_care, continuity_care,
        communication, responsiveness, personal_care, satisfaction
      ),
      na.rm = TRUE
    ),
    
    workforce_resource_index = rowMeans(
      dplyr::select(., caregiver_skills, fair_payment, caregiver_availability),
      na.rm = TRUE
    ),
    
    caregiver_burden_index = rowMeans(
      dplyr::select(., caregiver_workload, caregiver_burnout),
      na.rm = TRUE
    ),
    
    workforce_capacity_index = rowMeans(
      dplyr::select(
        ., caregiver_skills, caregiver_workload_rev, caregiver_burnout_rev,
        fair_payment, caregiver_availability
      ),
      na.rm = TRUE
    ),
    
    health_system_readiness_index = rowMeans(
      dplyr::select(
        ., policy_awareness, service_integration, referral_system,
        primary_care_support, monitoring, government_regulation
      ),
      na.rm = TRUE
    ),
    
    equity_concern_index = rowMeans(
      dplyr::select(., rural_inequality, income_inequality),
      na.rm = TRUE
    ),
    
    policy_support_index = rowMeans(
      dplyr::select(., financial_support, national_priority),
      na.rm = TRUE
    )
  )

write_xlsx(data_indexed, "outputs/data/eldercare_indexed_dataset.xlsx")

index_summary <- data_indexed %>%
  summarise(
    N = n(),
    access_score = paste0(round(mean(access_score, na.rm = TRUE), 2), " ± ", round(sd(access_score, na.rm = TRUE), 2)),
    care_quality_index = paste0(round(mean(care_quality_index, na.rm = TRUE), 2), " ± ", round(sd(care_quality_index, na.rm = TRUE), 2)),
    workforce_resource_index = paste0(round(mean(workforce_resource_index, na.rm = TRUE), 2), " ± ", round(sd(workforce_resource_index, na.rm = TRUE), 2)),
    caregiver_burden_index = paste0(round(mean(caregiver_burden_index, na.rm = TRUE), 2), " ± ", round(sd(caregiver_burden_index, na.rm = TRUE), 2)),
    workforce_capacity_index = paste0(round(mean(workforce_capacity_index, na.rm = TRUE), 2), " ± ", round(sd(workforce_capacity_index, na.rm = TRUE), 2)),
    health_system_readiness_index = paste0(round(mean(health_system_readiness_index, na.rm = TRUE), 2), " ± ", round(sd(health_system_readiness_index, na.rm = TRUE), 2)),
    equity_concern_index = paste0(round(mean(equity_concern_index, na.rm = TRUE), 2), " ± ", round(sd(equity_concern_index, na.rm = TRUE), 2)),
    policy_support_index = paste0(round(mean(policy_support_index, na.rm = TRUE), 2), " ± ", round(sd(policy_support_index, na.rm = TRUE), 2))
  )

write_xlsx(index_summary, "outputs/tables/index_summary.xlsx")
print(index_summary)

# ============================================================
# STEP 3: RELIABILITY ANALYSIS
# ============================================================

reliability_table <- data.frame(
  Scale = c(
    "Access score",
    "Care Quality Index",
    "Workforce Resource Index",
    "Caregiver Burden Index",
    "Workforce Capacity Index",
    "Health-System Readiness Index",
    "Equity Concern Index",
    "Policy Support Index"
  ),
  Items = c(
    "B3-B5",
    "C1-C8",
    "D2, D5, D6",
    "D3-D4",
    "D2, reverse D3, reverse D4, D5, D6",
    "E1-E6",
    "F1-F2",
    "F3-F4"
  ),
  Cronbach_alpha = c(
    psych::alpha(dplyr::select(data_indexed, care_available, care_affordable, emergency_access))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, respectful_care, timely_care, safe_care, continuity_care, communication, responsiveness, personal_care, satisfaction))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, caregiver_skills, fair_payment, caregiver_availability))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, caregiver_workload, caregiver_burnout))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, caregiver_skills, caregiver_workload_rev, caregiver_burnout_rev, fair_payment, caregiver_availability))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, policy_awareness, service_integration, referral_system, primary_care_support, monitoring, government_regulation))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, rural_inequality, income_inequality))$total$raw_alpha,
    psych::alpha(dplyr::select(data_indexed, financial_support, national_priority))$total$raw_alpha
  )
) %>%
  mutate(Cronbach_alpha = round(Cronbach_alpha, 3))

write_xlsx(reliability_table, "outputs/tables/Table_Reliability.xlsx")

save_as_docx(
  autofit(flextable(reliability_table)),
  path = "outputs/tables/Table_Reliability.docx"
)

# ============================================================
# STEP 4: PREPARE LABELLED DATA
# ============================================================

analysis_data <- data_indexed %>%
  mutate(
    gender_f = factor(gender, levels = c(1, 2, 3), labels = c("Male", "Female", "Other")),
    residence_f = factor(residence, levels = c(0, 1), labels = c("Urban", "Rural")),
    education_f = factor(education, levels = c(0, 1, 2, 3), labels = c("No formal education", "Primary", "Secondary", "Higher")),
    income_f = factor(income, levels = c(1, 2, 3), labels = c("Low", "Middle", "High")),
    functional_limitation_f = factor(functional_limitation, levels = c(0, 1), labels = c("No", "Yes")),
    homecare_received_f = factor(homecare_received, levels = c(0, 1), labels = c("No", "Yes")),
    unmet_need_f = factor(unmet_need, levels = c(0, 1), labels = c("No", "Yes")),
    care_frequency_f = factor(
      care_frequency,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Rarely", "Occasionally", "Once a week", "Several times a week", "Daily")
    )
  )

# ============================================================
# STEP 5: TABLE 1 - SOCIODEMOGRAPHIC CHARACTERISTICS
# ============================================================

table1 <- analysis_data %>%
  dplyr::select(age, gender_f, residence_f, education_f, income_f, functional_limitation_f) %>%
  tbl_summary(
    statistic = list(
      age ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age ~ "Age",
      gender_f ~ "Gender",
      residence_f ~ "Residence",
      education_f ~ "Education",
      income_f ~ "Monthly household income",
      functional_limitation_f ~ "Functional limitation"
    ),
    missing = "no"
  ) %>%
  modify_caption("Table 1. Socio-demographic characteristics of respondents") %>%
  bold_labels()

table1

save_as_docx(as_flex_table(table1), path = "outputs/tables/Table1_Sociodemographic.docx")

table1_by_residence <- analysis_data %>%
  dplyr::select(residence_f, age, gender_f, education_f, income_f, functional_limitation_f) %>%
  tbl_summary(
    by = residence_f,
    statistic = list(
      age ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age ~ "Age",
      gender_f ~ "Gender",
      education_f ~ "Education",
      income_f ~ "Monthly household income",
      functional_limitation_f ~ "Functional limitation"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      age ~ "t.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  modify_caption("Table 1. Socio-demographic characteristics by residence") %>%
  bold_labels()

table1_by_residence

save_as_docx(as_flex_table(table1_by_residence), path = "outputs/tables/Table1_By_Residence.docx")


##########==============================
# ============================================================
# STEP 6: TABLE 2 - ACCESS TO HOME-BASED ELDERCARE
# Exact journal-style manual table
# ============================================================

analysis_data <- analysis_data %>%
  mutate(
    residence_f = factor(residence, levels = c(0, 1), labels = c("Urban", "Rural")),
    homecare_received_f = factor(homecare_received, levels = c(1, 0), labels = c("Yes", "No")),
    unmet_need_f = factor(unmet_need, levels = c(1, 0), labels = c("Yes", "No")),
    care_frequency_f = factor(
      care_frequency,
      levels = c(5, 4, 3, 2, 1),
      labels = c(
        "Daily",
        "Several times a week",
        "Once a week",
        "Occasionally",
        "Rarely"
      )
    )
  )

fmt_n_pct <- function(x) {
  tab <- table(x, useNA = "no")
  pct <- prop.table(tab) * 100
  paste0(as.numeric(tab), " (", round(as.numeric(pct), 1), ")")
}

fmt_mean_sd <- function(x) {
  paste0(
    sprintf("%.2f", mean(x, na.rm = TRUE)),
    " ± ",
    sprintf("%.2f", sd(x, na.rm = TRUE))
  )
}

table2_manual <- bind_rows(
  data.frame(
    Variable = "Home-based care received",
    `Category / Indicator` = names(table(analysis_data$homecare_received_f)),
    `n (%) / Mean ± SD` = fmt_n_pct(analysis_data$homecare_received_f),
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Care frequency",
    `Category / Indicator` = names(table(analysis_data$care_frequency_f)),
    `n (%) / Mean ± SD` = fmt_n_pct(analysis_data$care_frequency_f),
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Unmet care need",
    `Category / Indicator` = names(table(analysis_data$unmet_need_f)),
    `n (%) / Mean ± SD` = fmt_n_pct(analysis_data$unmet_need_f),
    check.names = FALSE
  ),
  
  data.frame(
    Variable = c(
      "Care availability score",
      "Care affordability score",
      "Emergency access score",
      "Access score"
    ),
    `Category / Indicator` = "Mean ± SD",
    `n (%) / Mean ± SD` = c(
      fmt_mean_sd(analysis_data$care_available),
      fmt_mean_sd(analysis_data$care_affordable),
      fmt_mean_sd(analysis_data$emergency_access),
      fmt_mean_sd(analysis_data$access_score)
    ),
    check.names = FALSE
  )
)

table2_manual <- table2_manual %>%
  mutate(
    Variable = ifelse(duplicated(Variable), "", Variable)
  )

ft2 <- flextable(table2_manual) %>%
  set_caption("Table 2. Access to Home-Based Eldercare") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bold(i = ~ Variable == "Access score", part = "body") %>%
  bold(i = ~ `Category / Indicator` == "Mean ± SD" & Variable == "Access score", part = "body") %>%
  align(align = "left", part = "all") %>%
  align(j = 3, align = "right", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  autofit()

ft2

save_as_docx(
  ft2,
  path = "outputs/tables/Table2_Access_to_Home_Based_Eldercare.docx"
)

# ============================================================
# Reusable function for Urban vs Rural Mean ± SD tables
# ============================================================

make_residence_table <- function(data, variables, labels, bold_rows = NULL, caption, output_path) {
  
  table_out <- lapply(seq_along(variables), function(i) {
    
    var <- variables[i]
    
    urban_values <- data[[var]][data$residence_f == "Urban"]
    rural_values <- data[[var]][data$residence_f == "Rural"]
    
    p <- t.test(data[[var]] ~ data$residence_f)$p.value
    
    data.frame(
      Indicator = labels[i],
      `Urban Mean ± SD` = mean_sd(urban_values),
      `Rural Mean ± SD` = mean_sd(rural_values),
      `p-value` = fmt_p(p),
      check.names = FALSE
    )
  }) %>%
    bind_rows()
  
  ft <- flextable(table_out) %>%
    set_caption(caption) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    align(align = "left", part = "all") %>%
    align(j = 2:4, align = "center", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    autofit()
  
  # ✅ FIX: apply bold safely
  if (!is.null(bold_rows)) {
    rows_to_bold <- which(table_out$Indicator %in% bold_rows)
    ft <- bold(ft, i = rows_to_bold, part = "body")
  }
  
  save_as_docx(ft, path = output_path)
  
  return(ft)
}

table3_vars <- c(
  "respectful_care",
  "timely_care",
  "safe_care",
  "continuity_care",
  "communication",
  "responsiveness",
  "personal_care",
  "satisfaction",
  "care_quality_index"
)

table3_labels <- c(
  "Respectful care",
  "Timely care",
  "Safe care",
  "Continuity of care",
  "Communication",
  "Responsiveness",
  "Personal care",
  "Overall satisfaction",
  "Care Quality Index"
)

ft3 <- make_residence_table(
  data = analysis_data,
  variables = table3_vars,
  labels = table3_labels,
  bold_rows = "Care Quality Index",
  caption = "Table 3. Care Quality Indicators by Residence",
  output_path = "outputs/tables/Table3_Care_Quality_by_Residence.docx"
)

ft3

##########==================Table 4
table4_vars <- c(
  "caregiver_skills",
  "caregiver_workload",
  "caregiver_burnout",
  "fair_payment",
  "caregiver_availability",
  "workforce_resource_index",
  "caregiver_burden_index",
  "workforce_capacity_index"
)

table4_labels <- c(
  "Caregiver skills",
  "Caregiver workload",
  "Caregiver burnout",
  "Fair payment",
  "Caregiver availability",
  "Workforce Resource Index",
  "Caregiver Burden Index",
  "Workforce Capacity Index"
)

ft4 <- make_residence_table(
  data = analysis_data,
  variables = table4_vars,
  labels = table4_labels,
  bold_rows = c(
    "Workforce Resource Index",
    "Caregiver Burden Index",
    "Workforce Capacity Index"
  ),
  caption = "Table 4. Caregiver Workforce Capacity by Residence",
  output_path = "outputs/tables/Table4_Workforce_by_Residence.docx"
)

ft4
#########===================Table 5
table5_vars <- c(
  "policy_awareness",
  "service_integration",
  "referral_system",
  "primary_care_support",
  "monitoring",
  "government_regulation",
  "health_system_readiness_index"
)

table5_labels <- c(
  "Policy awareness",
  "Service integration",
  "Referral system",
  "Primary care support",
  "Monitoring mechanisms",
  "Support for regulation",
  "Readiness Index"
)

ft5 <- make_residence_table(
  data = analysis_data,
  variables = table5_vars,
  labels = table5_labels,
  bold_rows = "Readiness Index",
  caption = "Table 5. Health-System Readiness by Residence",
  output_path = "outputs/tables/Table5_HealthSystem_by_Residence.docx"
)

ft5

# ============================================================
# Helper functions for regression tables
# ============================================================

fmt_num <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

make_clean_ft <- function(df, caption, output_path) {
  ft <- flextable(df) %>%
    set_caption(caption) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    align(align = "left", part = "all") %>%
    align(j = 2:ncol(df), align = "center", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    autofit()
  
  save_as_docx(ft, path = output_path)
  ft
}

# ============================================================
# TABLE 6: LOGISTIC REGRESSION FOR UNMET CARE NEED
# ============================================================

model_logit <- glm(
  unmet_need ~ residence + income + health_system_readiness_index +
    workforce_resource_index + access_score,
  data = analysis_data,
  family = binomial()
)

table6_manual <- broom::tidy(model_logit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = recode(
      term,
      residence = "Rural (vs Urban)",
      income = "Income (higher)",
      health_system_readiness_index = "Health-system readiness",
      workforce_resource_index = "Workforce resources",
      access_score = "Access score"
    ),
    OR = fmt_num(estimate, 2),
    `95% CI` = paste0(fmt_num(conf.low, 2), "–", fmt_num(conf.high, 2)),
    `p-value` = fmt_p(p.value)
  ) %>%
  dplyr::select(Variable, OR, `95% CI`, `p-value`)

ft6 <- make_clean_ft(
  table6_manual,
  caption = "Table 6. Logistic Regression for Unmet Care Need",
  output_path = "outputs/tables/Table6_Logistic_Regression.docx"
)

ft6


# ============================================================
# TABLE 7: LINEAR REGRESSION FOR CARE QUALITY
# ============================================================

model_linear <- lm(
  care_quality_index ~ health_system_readiness_index +
    workforce_resource_index + access_score + income + residence,
  data = analysis_data
)

table7_manual <- broom::tidy(model_linear) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = recode(
      term,
      health_system_readiness_index = "Health-system readiness",
      workforce_resource_index = "Workforce resources",
      access_score = "Access score",
      income = "Income",
      residence = "Rural (vs Urban)"
    ),
    `β` = fmt_num(estimate, 2),
    SE = fmt_num(std.error, 2),
    `p-value` = fmt_p(p.value)
  ) %>%
  dplyr::select(Variable, `β`, SE, `p-value`)

ft7 <- make_clean_ft(
  table7_manual,
  caption = "Table 7. Linear Regression for Care Quality",
  output_path = "outputs/tables/Table7_Linear_Regression.docx"
)

ft7

# ============================================================
# TABLE 8: MEDIATION ANALYSIS
# ============================================================

med_data <- analysis_data %>%
  dplyr::select(
    health_system_readiness_index,
    workforce_resource_index,
    care_quality_index,
    residence,
    income
  ) %>%
  tidyr::drop_na()

mediator_model <- lm(
  workforce_resource_index ~ health_system_readiness_index + residence + income,
  data = med_data
)

outcome_model <- lm(
  care_quality_index ~ health_system_readiness_index +
    workforce_resource_index + residence + income,
  data = med_data
)

set.seed(123)

mediation_result <- mediation::mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "health_system_readiness_index",
  mediator = "workforce_resource_index",
  boot = TRUE,
  sims = 5000
)

med_summary <- summary(mediation_result)

table8_manual <- data.frame(
  Effect = c(
    "Indirect effect, ACME",
    "Direct effect, ADE",
    "Total effect",
    "Proportion mediated"
  ),
  Estimate = c(
    med_summary$d0,
    med_summary$z0,
    med_summary$tau.coef,
    med_summary$n0
  ),
  `95% CI` = c(
    paste0(fmt_num(med_summary$d0.ci[1], 2), "–", fmt_num(med_summary$d0.ci[2], 2)),
    paste0(fmt_num(med_summary$z0.ci[1], 2), "–", fmt_num(med_summary$z0.ci[2], 2)),
    paste0(fmt_num(med_summary$tau.ci[1], 2), "–", fmt_num(med_summary$tau.ci[2], 2)),
    paste0(fmt_num(med_summary$n0.ci[1], 2), "–", fmt_num(med_summary$n0.ci[2], 2))
  ),
  `p-value` = c(
    fmt_p(med_summary$d0.p),
    fmt_p(med_summary$z0.p),
    fmt_p(med_summary$tau.p),
    fmt_p(med_summary$n0.p)
  ),
  check.names = FALSE
) %>%
  mutate(
    Estimate = fmt_num(Estimate, 2)
  )

ft8 <- make_clean_ft(
  table8_manual,
  caption = "Table 8. Mediation Analysis",
  output_path = "outputs/tables/Table8_Mediation_Analysis.docx"
)

ft8

# ============================================================
# COMPLETE PUBLICATION-READY FIGURE CODE
# Figures 1, 2, 3, 4, 6, 7, 8, 9, 10
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(forcats)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# Nature/BMC-style palette
nc_pal <- c(
  blue   = "#3B5BA9",
  teal   = "#2FA7A0",
  orange = "#E69F00",
  red    = "#D55E00",
  purple = "#7E62A3",
  green  = "#009E73",
  grey   = "#6C757D",
  light  = "#F4F6F8"
)

theme_pub <- function() {
  theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#222222"),
      plot.subtitle = element_text(size = 11, color = "#555555"),
      axis.title = element_text(face = "bold", color = "#222222"),
      axis.text = element_text(color = "#222222"),
      legend.position = "top",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "#F4F6F8", color = NA),
      strip.text = element_text(face = "bold", color = "#222222"),
      panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ============================================================
# FIGURE 1: ACCESS & UNMET NEED
# ============================================================

fig1_data <- analysis_data %>%
  summarise(
    `Received home-based care` = mean(homecare_received == 1, na.rm = TRUE) * 100,
    `Unmet care need` = mean(unmet_need == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), names_to = "indicator", values_to = "percent")

fig1 <- ggplot(fig1_data, aes(x = indicator, y = percent, fill = indicator)) +
  geom_col(width = 0.58, color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    vjust = -0.45,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Received home-based care" = nc_pal["teal"],
    "Unmet care need" = nc_pal["red"]
  )) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Access to home-based eldercare",
    x = NULL,
    y = "Respondents (%)"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure1_Access_UnmetNeed.png", fig1, width = 6, height = 4, dpi = 600)


# ============================================================
# FIGURE 2: URBAN–RURAL INDEX COMPARISON
# ============================================================

fig2_data <- analysis_data %>%
  dplyr::select(
    residence_f,
    access_score,
    care_quality_index,
    workforce_resource_index,
    health_system_readiness_index
  ) %>%
  pivot_longer(cols = -residence_f, names_to = "index", values_to = "score") %>%
  mutate(
    index = recode(
      index,
      access_score = "Access",
      care_quality_index = "Care quality",
      workforce_resource_index = "Workforce resources",
      health_system_readiness_index = "System readiness"
    )
  )

fig2 <- ggplot(fig2_data, aes(x = residence_f, y = score, fill = residence_f)) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.75,
    color = "#333333",
    linewidth = 0.45
  ) +
  geom_jitter(
    aes(color = residence_f),
    width = 0.12,
    alpha = 0.35,
    size = 1.2,
    show.legend = FALSE
  ) +
  facet_wrap(~index, nrow = 1) +
  scale_fill_manual(values = c("Urban" = nc_pal["blue"], "Rural" = nc_pal["orange"])) +
  scale_color_manual(values = c("Urban" = nc_pal["blue"], "Rural" = nc_pal["orange"])) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Urban-rural differences in eldercare system indicators",
    x = NULL,
    y = "Index score"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure2_Urban_Rural_Indices.png", fig2, width = 10, height = 4, dpi = 600)


# ============================================================
# FIGURE 3: LINEAR REGRESSION COEFFICIENTS
# Requires: model_linear
# ============================================================

fig3_data <- tidy(model_linear, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      health_system_readiness_index = "System readiness",
      workforce_resource_index = "Workforce resources",
      access_score = "Access score",
      income = "Income level",
      residence = "Rural residence"
    ),
    direction = ifelse(estimate >= 0, "Positive association", "Negative association"),
    term = factor(term, levels = rev(term))
  )

fig3 <- ggplot(fig3_data, aes(x = estimate, y = term, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#9A9A9A", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.18,
    linewidth = 0.75
  ) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c(
    "Positive association" = nc_pal["teal"],
    "Negative association" = nc_pal["red"]
  )) +
  labs(
    title = "Predictors of care quality",
    x = "Regression coefficient",
    y = NULL
  ) +
  theme_pub()

ggsave("outputs/figures/Figure3_Regression_Coefficients.png", fig3, width = 7, height = 4.5, dpi = 600)


# ============================================================
# FIGURE 4: MEDIATION DIAGRAM
# ============================================================

fig4 <- ggplot() +
  geom_label(
    aes(x = 1, y = 1, label = "Health-system\nreadiness"),
    size = 4.3,
    fill = "#EAF2FF",
    color = nc_pal["blue"],
    label.size = 0.35,
    fontface = "bold"
  ) +
  geom_label(
    aes(x = 3, y = 1, label = "Workforce\nresources"),
    size = 4.3,
    fill = "#EAF7F5",
    color = nc_pal["teal"],
    label.size = 0.35,
    fontface = "bold"
  ) +
  geom_label(
    aes(x = 5, y = 1, label = "Care quality"),
    size = 4.3,
    fill = "#FFF4E1",
    color = nc_pal["orange"],
    label.size = 0.35,
    fontface = "bold"
  ) +
  geom_segment(
    aes(x = 1.6, xend = 2.4, y = 1, yend = 1),
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.9,
    color = nc_pal["blue"]
  ) +
  geom_segment(
    aes(x = 3.6, xend = 4.4, y = 1, yend = 1),
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.9,
    color = nc_pal["teal"]
  ) +
  geom_curve(
    aes(x = 1, xend = 5, y = 0.72, yend = 0.72),
    curvature = -0.25,
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.75,
    linetype = "dashed",
    color = nc_pal["red"]
  ) +
  annotate("text", x = 2, y = 1.18, label = "a path", size = 4, fontface = "bold", color = nc_pal["blue"]) +
  annotate("text", x = 4, y = 1.18, label = "b path", size = 4, fontface = "bold", color = nc_pal["teal"]) +
  annotate("text", x = 3, y = 0.48, label = "direct path", size = 4, fontface = "bold", color = nc_pal["red"]) +
  annotate(
    "text",
    x = 3,
    y = 1.45,
    label = "Mediated pathway through workforce resources",
    fontface = "bold",
    size = 4.6,
    color = "#222222"
  ) +
  coord_cartesian(xlim = c(0.4, 5.6), ylim = c(0.3, 1.6)) +
  theme_void()

ggsave("outputs/figures/Figure4_Mediation_Model.png", fig4, width = 8, height = 3.5, dpi = 600)


# ============================================================
# FIGURE 6: CARE QUALITY BY RESIDENCE
# ============================================================

fig6_data <- analysis_data %>%
  dplyr::select(
    residence_f,
    respectful_care,
    timely_care,
    safe_care,
    continuity_care,
    communication,
    responsiveness,
    personal_care,
    satisfaction,
    care_quality_index
  ) %>%
  pivot_longer(cols = -residence_f, names_to = "indicator", values_to = "score") %>%
  mutate(
    indicator = recode(
      indicator,
      respectful_care = "Respectful care",
      timely_care = "Timely care",
      safe_care = "Safe care",
      continuity_care = "Continuity of care",
      communication = "Communication",
      responsiveness = "Responsiveness",
      personal_care = "Personal care",
      satisfaction = "Overall satisfaction",
      care_quality_index = "Care Quality Index"
    ),
    indicator = factor(indicator, levels = rev(c(
      "Respectful care",
      "Timely care",
      "Safe care",
      "Continuity of care",
      "Communication",
      "Responsiveness",
      "Personal care",
      "Overall satisfaction",
      "Care Quality Index"
    )))
  )

fig6 <- ggplot(fig6_data, aes(x = indicator, y = score, color = residence_f)) +
  stat_summary(fun = mean, geom = "point", size = 3.2, position = position_dodge(width = 0.45)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, linewidth = 0.7,
               position = position_dodge(width = 0.45)) +
  coord_flip() +
  scale_color_manual(values = c("Urban" = nc_pal["blue"], "Rural" = nc_pal["orange"])) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Care quality indicators by residence",
    x = NULL,
    y = "Mean score ± SE"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure6_Care_Quality_by_Residence.png", fig6, width = 7, height = 5, dpi = 600)


# ============================================================
# FIGURE 7: WORKFORCE CAPACITY BY RESIDENCE
# ============================================================

fig7_data <- analysis_data %>%
  dplyr::select(
    residence_f,
    caregiver_skills,
    caregiver_workload,
    caregiver_burnout,
    fair_payment,
    caregiver_availability,
    workforce_resource_index,
    caregiver_burden_index,
    workforce_capacity_index
  ) %>%
  pivot_longer(cols = -residence_f, names_to = "indicator", values_to = "score") %>%
  mutate(
    indicator = recode(
      indicator,
      caregiver_skills = "Caregiver skills",
      caregiver_workload = "Caregiver workload",
      caregiver_burnout = "Caregiver burnout",
      fair_payment = "Fair payment",
      caregiver_availability = "Caregiver availability",
      workforce_resource_index = "Workforce Resource Index",
      caregiver_burden_index = "Caregiver Burden Index",
      workforce_capacity_index = "Workforce Capacity Index"
    ),
    indicator = factor(indicator, levels = rev(c(
      "Caregiver skills",
      "Caregiver workload",
      "Caregiver burnout",
      "Fair payment",
      "Caregiver availability",
      "Workforce Resource Index",
      "Caregiver Burden Index",
      "Workforce Capacity Index"
    )))
  )

fig7 <- ggplot(fig7_data, aes(x = indicator, y = score, fill = residence_f)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.25,
    width = 0.6,
    color = "#333333"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("Urban" = nc_pal["blue"], "Rural" = nc_pal["orange"])) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Caregiver workforce capacity by residence",
    x = NULL,
    y = "Score"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure7_Workforce_Capacity_by_Residence.png", fig7, width = 7, height = 5, dpi = 600)


# ============================================================
# FIGURE 8: HEALTH-SYSTEM READINESS BY RESIDENCE
# ============================================================

fig8_data <- analysis_data %>%
  dplyr::select(
    residence_f,
    policy_awareness,
    service_integration,
    referral_system,
    primary_care_support,
    monitoring,
    government_regulation,
    health_system_readiness_index
  ) %>%
  pivot_longer(cols = -residence_f, names_to = "indicator", values_to = "score") %>%
  mutate(
    indicator = recode(
      indicator,
      policy_awareness = "Policy awareness",
      service_integration = "Service integration",
      referral_system = "Referral system",
      primary_care_support = "Primary care support",
      monitoring = "Monitoring mechanisms",
      government_regulation = "Support for regulation",
      health_system_readiness_index = "Readiness Index"
    ),
    indicator = factor(indicator, levels = rev(c(
      "Policy awareness",
      "Service integration",
      "Referral system",
      "Primary care support",
      "Monitoring mechanisms",
      "Support for regulation",
      "Readiness Index"
    )))
  )

fig8 <- ggplot(fig8_data, aes(x = indicator, y = score, color = residence_f, group = residence_f)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, linewidth = 0.6) +
  coord_flip() +
  scale_color_manual(values = c("Urban" = nc_pal["blue"], "Rural" = nc_pal["orange"])) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Health-system readiness indicators by residence",
    x = NULL,
    y = "Mean score ± SE"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure8_Health_System_Readiness_by_Residence.png", fig8, width = 7, height = 5, dpi = 600)


# ============================================================
# FIGURE 9: LOGISTIC REGRESSION FOREST PLOT
# Requires: model_logit
# ============================================================

fig9_data <- broom::tidy(model_logit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      residence = "Rural (vs Urban)",
      income = "Income",
      health_system_readiness_index = "System readiness",
      workforce_resource_index = "Workforce resources",
      access_score = "Access score"
    ),
    term = factor(term, levels = rev(c(
      "Rural (vs Urban)",
      "Income",
      "System readiness",
      "Workforce resources",
      "Access score"
    )))
  )

fig9 <- ggplot(fig9_data, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#9A9A9A", linewidth = 0.6) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.18,
    linewidth = 0.75,
    color = "#333333"
  ) +
  geom_point(size = 3.5, color = nc_pal["teal"]) +
  scale_x_log10() +
  labs(
    title = "Factors associated with unmet care need",
    x = "Odds ratio, log scale",
    y = NULL
  ) +
  theme_pub()

ggsave("outputs/figures/Figure9_Logistic_Regression_Forest.png", fig9, width = 6.5, height = 4.5, dpi = 600)


# ============================================================
# FIGURE 10: MEDIATION EFFECTS
# Requires: med_summary
# ============================================================

fig10_data <- data.frame(
  effect = c("Indirect effect\nACME", "Direct effect\nADE", "Total effect"),
  estimate = c(
    med_summary$d0,
    med_summary$z0,
    med_summary$tau.coef
  ),
  lower = c(
    med_summary$d0.ci[1],
    med_summary$z0.ci[1],
    med_summary$tau.ci[1]
  ),
  upper = c(
    med_summary$d0.ci[2],
    med_summary$z0.ci[2],
    med_summary$tau.ci[2]
  )
)

fig10 <- ggplot(fig10_data, aes(x = effect, y = estimate, fill = effect)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#9A9A9A") +
  scale_fill_manual(values = c(
    "Indirect effect\nACME" = nc_pal["teal"],
    "Direct effect\nADE" = nc_pal["orange"],
    "Total effect" = nc_pal["blue"]
  )) +
  labs(
    title = "Mediation effects of workforce resources",
    x = NULL,
    y = "Effect estimate with 95% CI"
  ) +
  theme_pub()

ggsave("outputs/figures/Figure10_Mediation_Effects.png", fig10, width = 6, height = 4, dpi = 600)


message("All figures saved successfully in outputs/figures/")

