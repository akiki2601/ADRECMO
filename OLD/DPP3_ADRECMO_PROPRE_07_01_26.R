###############################################################################
# ADRECMO — Tables & Figures (DPP3 / Haptoglobin)
# Goal: avoid object-name collisions; keep intermediate datasets explicit
###############################################################################

# ---- Packages ----
library(dplyr)
library(tidyr)
library(stringr)
library(tableone)
library(flextable)
library(officer)
library(ggplot2)
library(survival)
library(survminer)
library(broom)
library(lubridate)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(patchwork)
library(tibble)
library(DescTools)

# ---- Global constants ----
CAPTION_T1 <- "Table 1: Characteristics of the population before ECMO implantation"
CAPTION_T2 <- "Table 2: Outcomes"
FOOTNOTE_TABLES <- "Data are expressed in n(%) for categorical variables and median [25th–75th] for continuous variables."

source("fonction_adrecmo_DPP3.R")

#==============================================================================
# TABLE 1 —  Baseline characteristics by DPP3_median with median differences
#==============================================================================



t1_vars_cat <- c(
  "Gender", "HTN", "DM", "Immunodepression", "CKD", "Neurologic_deficit",
  "Chronic_respiratory_disease", "HF", "Alphablocker", "Betablocker",
  "cause", "ische", "Cardiac_arrest_before_canul", "Intubation"
)

t1_vars_cont <- c(
  "Age", "BMI", "PAS_D0", "PAD_D0", "PAM_D0", "HR_D0", "Preecmo_tte_ef",
  "Preecmo_tte_vtiao", "sofa_D0", "NAD_24H", "DOB_24H", "Lact_D0",
  "Creat_D0", "ASAT_D0", "Bili_D0", "Tropo_i_hs_D0", "Ntprobnp_D0", "Plq_D0"
)

t1_vars_all <- c(t1_vars_cat, t1_vars_cont)

df_t1_input <- df %>%
  dplyr::select(all_of(t1_vars_all), DPP3_median) %>%
  mutate(
    across(all_of(t1_vars_cont), as.numeric),
    across(all_of(t1_vars_cat), as.factor)
  )

t1_tableone_obj <- CreateTableOne(
  vars       = t1_vars_all,
  factorVars = t1_vars_cat,
  strata     = "DPP3_median",
  data       = df_t1_input
)
t1_vars_cont <- c(
  "Age", "BMI", "PAS_D0", "PAD_D0", "PAM_D0", "HR_D0", "Preecmo_tte_ef",
  "Preecmo_tte_vtiao", "sofa_D0", "NAD_24H", "DOB_24H", "Lact_D0",
  "Creat_D0", "ASAT_D0", "Bili_D0", "Tropo_i_hs_D0", "Ntprobnp_D0", "Plq_D0"
)

t1_tableone_print <- print(
  t1_tableone_obj,
  nonnormal      = t1_vars_cont,
  contDigits     = 1,
  showAllLevels  = FALSE,
  test          = FALSE   
)

df_t1_tableone <- as.data.frame(t1_tableone_print) %>%
  tibble::rownames_to_column("Variables") %>%
  dplyr::select(Variables, `below median`, `above median`)

nonnormal_0digit <- df_t1_tableone %>%
  dplyr::filter(!grepl("^Lact_D0 ", Variables)) %>%     
  dplyr::pull(Variables)

rows_to_round <- df_t1_tableone$Variables %in% nonnormal_0digit

df_t1_tableone$`below median`[rows_to_round] <-
  round_nonnormal_0(df_t1_tableone$`below median`[rows_to_round])

df_t1_tableone$`above median`[rows_to_round] <-
  round_nonnormal_0(df_t1_tableone$`above median`[rows_to_round])

df_t1_missing <- df_t1_input %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "raw_id", values_to = "n_missing")

df_t1_tableone_id <- df_t1_tableone %>%
  mutate(
    raw_id = sub(
      "\\(median \\[IQR\\]\\)|= 1 \\(\\%\\)|= Women \\(\\%\\)|\\(\\%\\)|= Non ischémique \\(\\%\\)|= ischémique \\(\\%\\)",
      "",
      Variables
    ),
    raw_id = trimws(raw_id),
    raw_id = sub(" = YES", "", raw_id),
    Variables = str_remove(Variables, " \\(median \\[IQR\\]\\)")
  )

cutoff <- round(median(df$DPP3_D0, na.rm = TRUE))

below <- paste0("cDPP3 ≤ ", cutoff, " ng/ml")
above <- paste0("cDPP3 > ", cutoff, " ng/ml")

df_t1_final <- df_t1_tableone_id %>%
  left_join(df_t1_missing, by = "raw_id") %>%
  left_join(hl_diff_ci,   by = "raw_id") %>%
  left_join(diff_prop_ci, by = "raw_id") %>%
  mutate(
    `Group difference [95% CI]` = dplyr::if_else(
      !is.na(`Median difference [95% CI]`),
      `Median difference [95% CI]`,
      `Difference in proportions (% points) [95% CI]`
    )
  ) %>%
  transmute(
    Variables,
    Missing = n_missing,
    !!below  := `below median`,
    !!above := `above median`,
    `Group difference [95% CI]`
  )


df_t1_final$`Group difference [95% CI]` <-
  round_nonnormal_0(df_t1_final$`Group difference [95% CI]`)



dict_t1_labels <- c(
  "n" = "n",
  "Gender = Women (%)" = "Gender = Female (%)",
  "HTN = 1 (%)" = "Hypertension (%)",
  "DM = 1 (%)" = "Diabetes Mellitus (%)",
  "Immunodepression = 1 (%)" = "Immunosuppression (%)",
  "CKD = 1 (%)" = "Chronic Kidney Disease (%)",
  "Neurologic_deficit = 1 (%)" = "Neurological Deficit (%)",
  "Chronic_respiratory_disease = 1 (%)" = "Chronic Respiratory Disease (%)",
  "HF = 1 (%)" = "Heart Failure (%)",
  "Alphablocker = 1 (%)" = "Alpha-blocker (%)",
  "Betablocker = YES (%)" = "Beta-blocker (%)",
  "cause = Non ischémique (%)" = "Non-ischemic (%)",
  "ische = ischémique (%)" = "Ischemic (%)",
  "Cardiac_arrest_before_canul = 1 (%)" = "Cardiac Arrest Before Cannulation (%)",
  "Intubation = 1 (%)" = "Mechanical ventilation (%)",
  "Age" = "Age (years)",
  "BMI" = "Body Mass Index (kg/m²)",
  "PAS_D0" = "Systolic Blood Pressure (mmHg)",
  "PAD_D0" = "Diastolic Blood Pressure (mmHg)",
  "PAM_D0" = "Mean Arterial Pressure (mmHg)",
  "HR_D0" = "Heart Rate (bpm)",
  "Preecmo_tte_ef" = "Pre-ECMO Ejection Fraction (%)",
  "Preecmo_tte_vtiao" = "Pre-ECMO Aortic Velocity Time Integral (cm)",
  "sofa_D0" = "SOFA pre-ECMO implantation",
  "NAD_24H" = "Norepinephrine cumulative Dose on first day (γ/kg/24H)",
  "DOB_24H" = "Dobutamine cumulative Dose on first day (γ/kg/24H)",
  "Lact_D0" = "Lactate (mmol/L)",
  "Creat_D0" = "Creatinine (µmol/L)",
  "ASAT_D0" = "AST (IU/L)",
  "Bili_D0" = "Bilirubin (mg/dL)",
  "Tropo_i_hs_D0" = "High Sensitivity Troponin I (ng/L)",
  "Ntprobnp_D0" = "NT-proBNP (pg/mL)",
  "Plq_D0" = "Platelet Count (10⁹/L)"
)

df_t1_final <- df_t1_final %>%
  mutate(Variables = dplyr::recode(Variables, !!!dict_t1_labels))

t1_sections <- tibble::tribble(
  ~Variables, ~Missing, ~`cDPP3 ≤ 73 ng/ml`, ~`cDPP3 > 73 ng/ml`, ~`Group difference [95% CI]`,
  "Demographics", NA_integer_, "", "", "",
  "Medical History", NA_integer_, "", "", "",
  "Prior-adrenergic medications", NA_integer_, "", "", "",
  "Cardiogenic shock cause", NA_integer_, "", "", "",
  "Pre-ECMO Clinical parameters", NA_integer_, "", "", "",
  "Pre-ECMO heart function", NA_integer_, "", "", "",
  "Pre-ECMO supports", NA_integer_, "", "", "",
  "Pre-ECMO Biological assessment", NA_integer_, "", "", ""
)

t1_order <- c(
  "n",
  "Demographics",
  "Age (years)",
  "Gender = Female (%)",
  "Body Mass Index (kg/m²)",
  "Medical History",
  "Hypertension (%)",
  "Diabetes Mellitus (%)",
  "Heart Failure (%)",
  "Chronic Kidney Disease (%)",
  "Chronic Respiratory Disease (%)",
  "Neurological Deficit (%)",
  "Immunosuppression (%)",
  "Prior-adrenergic medications",
  "Beta-blocker (%)",
  "Alpha-blocker (%)",
  "Cardiogenic shock cause",
  "Non-ischemic (%)",
  "Ischemic (%)",
  "Cardiac Arrest Before Cannulation (%)",
  "Pre-ECMO Clinical parameters",
  "Systolic Blood Pressure (mmHg)",
  "Diastolic Blood Pressure (mmHg)",
  "Mean Arterial Pressure (mmHg)",
  "Heart Rate (bpm)",
  "Pre-ECMO heart function",
  "Pre-ECMO Ejection Fraction (%)",
  "Pre-ECMO Aortic Velocity Time Integral (cm)",
  "Pre-ECMO supports",
  "Mechanical ventilation (%)",
  "Norepinephrine cumulative Dose on first day (γ/kg/24H)",
  "Dobutamine cumulative Dose on first day (γ/kg/24H)",
  "Pre-ECMO Biological assessment",
  "Lactate (mmol/L)",
  "Creatinine (µmol/L)",
  "AST (IU/L)",
  "Bilirubin (mg/dL)",
  "High Sensitivity Troponin I (ng/L)",
  "NT-proBNP (pg/mL)",
  "Platelet Count (10⁹/L)",
  "SOFA pre-ECMO implantation"
)

df_t1_display <- bind_rows(t1_sections, df_t1_final) %>%
  mutate(Variables = factor(Variables, levels = t1_order)) %>%
  arrange(Variables) %>%
  mutate(    across(everything(), ~gsub("\\s*[;,]\\s*", " - ", .x)),
    is_section = is.na(Missing) & `cDPP3 ≤ 73 ng/ml` == "" & `cDPP3 > 73 ng/ml` == "")

ft_t1 <- flextable(df_t1_display %>% select(-is_section)) %>%
  set_caption(CAPTION_T1) %>%
  autofit() %>%
  align(j = 2:5, align = "center", part = "body") %>%
  bold(i = which(df_t1_display$is_section), bold = TRUE) %>%
  align(i = which(df_t1_display$is_section), j = 1, align = "left", part = "body") %>%
  bg(i = which(df_t1_display$is_section), bg = "#FFFFFF", part = "body") %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) %>%
  add_footer_lines(values = FOOTNOTE_TABLES) %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 9)

ft_t1
save_as_docx(ft_t1, path = "Tables/Table1_characteristics_DPP3_no_p.docx")


#==============================================================================
# TABLE 2 — Outcomes / management by DPP3_median no p-values
#==============================================================================


t2_vars_cont <- c(
  "Lenght_eer", "Lenght_vm", "Time_hosp", "Time_ecmo",
  "ECMO_duration_weaned", "ECMO_duration_Dead"
)

t2_vars_cat <- c("BCPIA_Tot", "EER_Tot", "Outcome", "j90_deces", "j28_deces")
t2_vars_all <- c(t2_vars_cat, t2_vars_cont)

df_t2_input <- df %>%
  dplyr::select(all_of(t2_vars_all), DPP3_median) %>%
  mutate(
    across(all_of(t2_vars_cont), as.numeric),
    across(all_of(t2_vars_cat), as.factor),
      Outcome_Death            = Outcome == "Death",
      Outcome_ECMO_Weaning     = Outcome == "ECMO Weaning",
      Outcome_Bridge_LVAD      = Outcome == "Bridge to LVAD",
      Outcome_Bridge_Transplant= Outcome == "Bridge to Transplant"
    )
  
t2_vars_cat_bin <- c(
  "BCPIA_Tot", "EER_Tot", "j90_deces", "j28_deces",
  "Outcome_Death", "Outcome_ECMO_Weaning",
  "Outcome_Bridge_LVAD", "Outcome_Bridge_Transplant"
)

t2_vars_all <- c(t2_vars_cat_bin, t2_vars_cont)
t2_tableone_obj <- CreateTableOne(
  vars       = t2_vars_all,
  factorVars = t2_vars_cat_bin,
  strata     = "DPP3_median",
  data       = df_t2_input
)

t2_tableone_print <- print(
  t2_tableone_obj,
  nonnormal      = t2_vars_cont,
  contDigits     = 1,
  showAllLevels  = FALSE,
  test = FALSE
  
)


df_t2_final <- as.data.frame(t2_tableone_print) %>%
  tibble::rownames_to_column("Variables") %>%
  select(Variables, `below median`, `above median`) %>%
  mutate(Variables = str_remove(Variables, " \\(median \\[IQR\\]\\)")) %>%
  mutate(raw_id = trimws(sub("\\(.*\\)|=.*", "", Variables))) %>%
  left_join(hl_diff_ci_t2,   by = "raw_id") %>%
  left_join(prop_diff_ci_t2, by = "raw_id") %>%
  mutate(
    `Group difference [95% CI]` = dplyr::coalesce(Group_diff_CI.x, Group_diff_CI.y)
  ) %>%
  transmute(
    Variables,
    !!below  := `below median`,
    !!above := `above median`,
    `Group difference [95% CI]`
  )

nonnormal_0digit <-  c("Lenght_eer",
"Lenght_vm",
"Time_hosp",
"Time_ecmo",
"ECMO_duration_weaned"  ,
"ECMO_duration_Dead"  )

rows_to_round <- df_t2_final$Variables %in% nonnormal_0digit

df_t2_final$`cDPP3 ≤ 73 ng/ml` <-
  round_nonnormal_0(df_t2_final$`cDPP3 ≤ 73 ng/ml`)

df_t2_final$`cDPP3 > 73 ng/ml`<-
  round_nonnormal_0(df_t2_final$`cDPP3 > 73 ng/ml`)

df_t2_final$`Group difference [95% CI]` <-
  round_nonnormal_0(df_t2_final$`Group difference [95% CI]`)


t2_sections <- tibble::tribble(
  ~Variables, ~`cDPP3 ≤ 73 ng/ml`, ~`cDPP3 > 73 ng/ml`, ~`Group difference [95% CI]`,
  "Outcome", "", "", ""
)

dict_t2_labels <- c(
  "Outcome_Bridge_LVAD = TRUE (%)"        = "Bridge to LVAD (%)",
  "Outcome_Bridge_Transplant = TRUE (%)"  = "Bridge to Transplant (%)",
  "Outcome_Death = TRUE (%)"                 = "Death in ICU (%)",
  "Outcome_ECMO_Weaning = TRUE (%)"          = "ECMO Weaning (%)",
  "BCPIA_Tot = 1 (%)"        = "Intra-Aortic Balloon Pump (%)",
  "j90_deces = 1 (%)"        = "90-day Mortality (%)",
  "j28_deces = 1 (%)"        = "28-day Mortality (%)",
  "EER_Tot = 1 (%)"          = "Renal Replacement Therapy (%)",
  "Lenght_eer"               = "Duration of Renal Replacement Therapy (days)",
  "Lenght_vm"                = "Duration of Mechanical Ventilation (days)",
  "Time_hosp"                = "ICU Length of Stay (days)",
  "Time_ecmo"                = "ECMO Duration (days)",
  "ECMO_duration_weaned"     = "ECMO Duration in weaned (days)",
  "ECMO_duration_Dead"       = "ECMO Duration in deceased (days)"
)

t2_order <- c(
  "n",
  "ECMO Duration (days)",
  "ECMO Duration in weaned (days)",
  "ECMO Duration in deceased (days)",
  "Intra-Aortic Balloon Pump (%)",
  "Renal Replacement Therapy (%)",
  "Duration of Renal Replacement Therapy (days)",
  "Duration of Mechanical Ventilation (days)",
  "ICU Length of Stay (days)",
  "Outcome",
  "Death in ICU (%)",
  "Bridge to LVAD (%)",
  "Bridge to Transplant (%)",
  "ECMO Weaning (%)",
  "28-day Mortality (%)",
  "90-day Mortality (%)"
)

df_t2_display <- bind_rows(t2_sections, df_t2_final) %>%
  mutate( Variables = dplyr::recode(Variables, !!!dict_t2_labels),
    Variables = factor(Variables, levels = t2_order),
    across(everything(), ~gsub("\\s*[;,]\\s*", " - ", .x)
)) %>%
  arrange(Variables)



ft_t2 <- flextable(df_t2_display) %>%
  set_caption(CAPTION_T2) %>%
  autofit() %>%
  align(j = 2:4, align = "center", part = "body") %>%
  bold(i = ~ Variables == "Outcome", bold = TRUE) %>%
  align(i = ~ Variables == "Outcome", j = 1, align = "left", part = "body") %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) %>%
  add_footer_lines(values = FOOTNOTE_TABLES) %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 9)

ft_t2
save_as_docx(ft_t2, path = "Tables/Table2_Outcome_DPP3_no_p.docx")

# Optional: export issue file
write.csv2(
  x = df %>% dplyr::select(ID, Outcome),
  file = "issue.csv",
  row.names = FALSE
)

#==============================================================================
# FIGURE — DPP3 baseline comparisons by DPP3_median (helper function)
#==============================================================================

# Create plots
p_dpp3_sofa <- plot_box_by_dpp3_group(df, "sofa_D0", "SOFA")
p_dpp3_lact <- plot_box_by_dpp3_group(df, "Lact_D0", "Lactate\n(mmol/L)")
p_dpp3_nad  <- plot_box_by_dpp3_group(df, "NADcum_D0_w", "Cumulated norepinephrine dose\nday of ECMO implantation (γ/kg)")
p_dpp3_ckd  <- plot_box_by_dpp3_group(df, "CKD_D0", "eGFR\n(ml/min/1.73m²)")
p_dpp3_flow <- plot_box_by_dpp3_group(df, "j0_ecmo_debit", "ECMO Flow\n(L/min)", x_lab = "cDPP3")
p_dpp3_asat <- plot_box_by_dpp3_group(df, "ASAT_D0", "ASAT\n(UI/L)", x_lab = "cDPP3")

# Combine plots into a single figure
fig_dpp3_baseline <- (p_dpp3_sofa + p_dpp3_lact) /
  (p_dpp3_nad + p_dpp3_ckd) /
  (p_dpp3_flow + p_dpp3_asat) +
  plot_annotation(
    title = sprintf("cDPP3 median = %.0f ng/mL",
      median(df$DPP3_D0, na.rm = TRUE)
    ),
    tag_levels = c("A", "B", "C", "D", "E", "F")
  ) & theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))



# Save the figure
ggsave(
  "FIGURES/Figure_DPP3_base.pdf",
  plot   = fig_dpp3_baseline,
  width  = 160, height = 200, units = "mm",
  device = cairo_pdf, bg = "white", scale = 1.5
)

#==============================================================================
# SURVIVAL — 30-day survival, Cox model + PH check
#==============================================================================

df_surv <- df %>%
  mutate(
    diff_days_30 = if_else(diff_days >= 30, 30, diff_days),
    outcome_censored_30 = case_when(
      diff_days >= 30 & outcome_censored == 1 ~ 0,
      TRUE ~ outcome_censored
    ),
    DPP3_median = if_else(DPP3_D0 <= median(DPP3_D0, na.rm = TRUE), "below median", "above median"),
    DPP3_median = factor(DPP3_median, levels = c("below median", "above median"))
  )


label_median <- sprintf("cDPP3 median = %.0f ng/mL",
  median(df$DPP3_D0, na.rm = TRUE)
)


km_fit_30d <- survfit(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median, data = df_surv)

cox_uni <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median, data = df_surv)

df_surv <- df_surv %>%
  mutate(
    ecmo_start_date = if_else(ID == "10-HC", as.Date("2018-06-27"), ecmo_start_date),
    time_adm_ECMO = time_length(interval(icu_admiss_date, ecmo_start_date), "days"),
    Cardiac_arrest_before_canul = as.factor(Cardiac_arrest_before_canul),
    cause = as.factor(cause)
  )

cox_mv <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median + time_adm_ECMO + cause, data = df_surv)
cox_mv2 <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median + time_adm_ECMO + cause + Cardiac_arrest_before_canul, data = df_surv)

cox_mv_tidy <- tidy(cox_mv2, exponentiate = TRUE, conf.int = TRUE)
cox_uni_tidy <- tidy(cox_uni, exponentiate = TRUE, conf.int = TRUE)

lab_uni <- cox_uni_tidy %>%
  mutate(label = sprintf("HR = %.2f 95%%CI (%.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

lab_adj <- cox_mv_tidy %>%
  filter(term == "DPP3_medianabove median") %>%
  mutate(label = sprintf("aHR = %.2f 95%%CI (%.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

lab_surv <- paste(lab_uni, lab_adj, sep = "\n")

ph_test_mv <- cox.zph(cox_mv2)

surv_plot <- ggsurvplot(
  km_fit_30d, data = df_surv,
  pval = FALSE, conf.int = FALSE,
  risk.table = TRUE, risk.table.height = 0.2,
  risk.table.y.text = TRUE, risk.table.title = "Number at Risk",
  xlim = c(0, 30), ylab = "30-day survival", break.time.by = 5,
  ggtheme = theme_classic(), surv.scale = "percent",
  legend.labs = c("below median", "above median"),
  legend = "none"
)

surv_plot$plot <- surv_plot$plot +
  annotate("text", x = 5, y = 0.15, label = lab_surv, hjust = 0, size = 4) +
  annotate("text", x = 15,
    y = 1,
    label = label_median,
    hjust = 0.5,
    size = 4,
    fontface = "bold"
  )

fig_survival <- ggpubr::ggarrange(
  surv_plot$plot,
  surv_plot$table,
  ncol = 1, nrow = 2,
  heights = c(3, 1)
)

ggsave(
  "FIGURES/Figure_survival_DPP3.pdf",
  plot   = fig_survival,
  width  = 180, height = 140, units = "mm",
  device = cairo_pdf, bg = "white"
)

#==============================================================================
# LONGITUDINAL DPP3 — time & outcome (mixed model)
#==============================================================================

df_dpp3_long <- df %>%
  select(ID, DPP3_D0, DPP3_D3_5, DPP3_w, Outcome_death_bridge) %>%
  pivot_longer(cols = starts_with("DPP3_"), names_to = "timepoint_raw", values_to = "value") %>%
  mutate(
    timepoint = str_remove(timepoint_raw, "^DPP3_"),
    timepoint = case_when(
      timepoint == "D0"   ~ "implantation",
      timepoint == "D3_5" ~ "day 3 to 5",
      timepoint == "w"    ~ "explantation",
      TRUE ~ timepoint
    ),
    timepoint = factor(timepoint, levels = c("implantation", "day 3 to 5", "explantation")),
    value_log = log10(value)
  )

df_dpp3_n_time <- df_dpp3_long %>%
  group_by(timepoint) %>%
  summarise(n = sum(!is.na(value)), 
            median = median(value, na.rm = TRUE),
            IQR_low = quantile(value, 0.25, na.rm = TRUE),
            IQR_high = quantile(value, 0.75, na.rm = TRUE),.groups = "drop")

lmm_dpp3_time <- lmer(value_log ~ timepoint + (1 | ID), data = df_dpp3_long)
anova_dpp3_time <- anova(lmm_dpp3_time, ddf = "Kenward-Roger")
p_time_dpp3 <- format.pval(anova_dpp3_time["timepoint", "Pr(>F)"], digits = 3, eps = 0.001)

p_dpp3_time <- ggplot(df_dpp3_long, aes(x = timepoint, y = value_log)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(
    data = df_dpp3_n_time,
    aes(x = timepoint, y = min(df_dpp3_long$value_log, na.rm = TRUE), label = paste0("n=", n)),
    inherit.aes = FALSE,
    vjust = 1, size = 4
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(df_dpp3_long$value_log, na.rm = TRUE) * 1.05,
    label = paste0("Time p = ", p_time_dpp3),
    hjust = 0, size = 4
  ) +
  labs(x = "", y = "DPP3 (log scale)") +
  theme_minimal()+
  theme(legend.position="none",
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x  = element_blank())

df_dpp3_n_by_outcome <- df_dpp3_long %>%
  group_by(timepoint, Outcome_death_bridge) %>%
  summarise(n = sum(!is.na(value)), .groups = "drop")

lmm_dpp3_inter <- lmer(value_log ~ timepoint * Outcome_death_bridge + (1 | ID), data = df_dpp3_long)
anova_dpp3_inter <- anova(lmm_dpp3_inter, ddf = "Kenward-Roger")

df_dpp3_pvals <- anova_dpp3_inter %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  transmute(
    term,
    p_value = `Pr(>F)`,
    p_label = case_when(
      p_value < 0.001 ~ "< 0.001",
      TRUE ~ formatC(p_value, format = "f", digits = 2)
    ),
    label = case_when(
      term == "timepoint" ~ paste0("Time: p ", p_label),
      term == "Outcome_death_bridge" ~ paste0("Outcome: p = ", p_label),
      term == "timepoint:Outcome_death_bridge" ~ paste0("Interaction: p = ", p_label),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

p_dpp3_by_outcome <- ggplot(df_dpp3_long, aes(x = timepoint, y = value_log, fill = factor(Outcome_death_bridge))) +
  geom_boxplot() +
  geom_text(
    data = df_dpp3_n_by_outcome,
    aes(x = timepoint, y = min(df_dpp3_long$value_log, na.rm = TRUE), label = paste0("n=", n), group = factor(Outcome_death_bridge)),
    position = position_dodge(0.7),
    inherit.aes = FALSE,
    vjust = 1, size = 4
  ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste(df_dpp3_pvals$label, collapse = "\n"),
    hjust = 1.05, vjust = 1.1
  ) +
  labs(x = "Timepoints", y = "DPP3 (log scale)", color = "Outcomes") +
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))

fig_dpp3_serial <- p_dpp3_time / p_dpp3_by_outcome +
  plot_annotation(tag_levels = c("A", "B"))

ggsave(
  "FIGURES/Figure_DPP3_serial.pdf",
  plot   = fig_dpp3_serial,
  width  = 160, height = 220, units = "mm",
  device = cairo_pdf, bg = "white", scale = 1.1
)

#==============================================================================
# HAPTOGLOBIN — longitudinal (mixed model)
#==============================================================================

df_hapto_long <- df %>%
  select(ID, hapto_J0, `hapto_J3-J5`, hapto_JS, Outcome_death_bridge) %>%
  pivot_longer(cols = starts_with("hapto_"), names_to = "timepoint_raw", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    timepoint = str_remove(timepoint_raw, "^hapto_"),
    timepoint = case_when(
      timepoint == "J0"    ~ "implantation",
      timepoint == "J3-J5" ~ "day 3 to 5",
      timepoint == "JS"    ~ "explantation",
      TRUE ~ timepoint
    ),
    timepoint = factor(timepoint, levels = c("implantation", "day 3 to 5", "explantation")),
    value = as.numeric(value)
  )



df_hapto_n_time <- df_hapto_long %>%
  group_by(timepoint) %>%
  summarise(n = sum(!is.na(value)), .groups = "drop")

lmm_hapto_time <- lmer(value ~ timepoint + (1 | ID), data = df_hapto_long)
anova_hapto_time <- anova(lmm_hapto_time, ddf = "Kenward-Roger")
p_time_hapto <- format.pval(anova_hapto_time["timepoint", "Pr(>F)"], digits = 3, eps = 0.001)


###RESTRICT D0-D3-5 ONLY#####
df_hapto_long_restrict<-df_hapto_long%>%filter(timepoint!="explantation")
lmm_hapto_time_restrict <- lmer(value ~ timepoint + (1 | ID), data = df_hapto_long_restrict)
anova_hapto_time_restrict <- anova(lmm_hapto_time_restrict, ddf = "Kenward-Roger")
p_time_hapto_restrict <- format.pval(anova_hapto_time_restrict["timepoint", "Pr(>F)"], digits = 3, eps = 0.001)
lmm_hapto_inter_restrict <- lmer(value ~ timepoint * Outcome_death_bridge + (1 | ID), data = df_hapto_long_restrict)
anova_hapto_inter <- anova(lmm_hapto_inter_restrict, ddf = "Kenward-Roger")



p_hapto_time <- ggplot(df_hapto_long, aes(x = timepoint, y = value)) +
  geom_boxplot() +
  geom_text(
    data = df_hapto_n_time,
    aes(x = timepoint, y = min(df_hapto_long$value, na.rm = TRUE), label = paste0("n=", n)),
    inherit.aes = FALSE,
    vjust = 1, size = 4
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(df_hapto_long$value, na.rm = TRUE) * 1.05,
    label = paste0("Time p = ", p_time_hapto),
    hjust = 0, size = 4
  ) +
  labs(x = "", y = "Haptoglobin (g/L)") +
  theme_minimal()+
  theme(legend.position="none",
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x  = element_blank()) 

df_hapto_n_by_outcome <- df_hapto_long %>%
  group_by(timepoint, Outcome_death_bridge) %>%
  summarise(n = sum(!is.na(value)), .groups = "drop")

lmm_hapto_inter <- lmer(value ~ timepoint * Outcome_death_bridge + (1 | ID), data = df_hapto_long)
anova_hapto_inter <- anova(lmm_hapto_inter, ddf = "Kenward-Roger")

p_inter_hapto <- format.pval(
  anova_hapto_inter["timepoint:Outcome_death_bridge", "Pr(>F)"],
  digits = 3, eps = 0.001
)

p_hapto_by_outcome <- ggplot(df_hapto_long, aes(x = timepoint, y = value, fill = Outcome_death_bridge)) +
  geom_boxplot() +
  geom_text(
    data = df_hapto_n_by_outcome,
    aes(x = timepoint, y = min(df_hapto_long$value, na.rm = TRUE), label = paste0("n=", n), group = factor(Outcome_death_bridge)),
    position = position_dodge(0.7),
    inherit.aes = FALSE,
    vjust = 1, size = 4
  ) +
  annotate(
    "text",
    x = 1, y = max(df_hapto_long$value, na.rm = TRUE) * 1.05,
    label = paste0("Interaction p = ", p_inter_hapto),
    hjust = 0) +
  labs(x = "Time points", y = "Haptoglobin (g/L)", color = "Outcome") +
  theme_minimal()+
  theme(legend.position="bottom",
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
        )

fig_hapto_serial <- p_hapto_time / p_hapto_by_outcome +
  plot_annotation(tag_levels = c("A", "B"))

ggsave(
  "FIGURES/Figure_hapto_serial.pdf",
  plot   = fig_hapto_serial,
  width  = 160, height = 200, units = "mm",
  device = cairo_pdf, bg = "white", scale = 1.1)


#####SPAGHETTI PLOT HAPTOGLOBIN####

df_hapto_delta <- df %>%
  select(ID, hapto_J0, `hapto_J3-J5`) %>%
  mutate(
    hapto_J0 = as.numeric(hapto_J0),
    `hapto_J3-J5` = as.numeric(`hapto_J3-J5`)
  ) %>%
  filter(!is.na(hapto_J0), !is.na(`hapto_J3-J5`)) %>%
  mutate(
    delta_hapto = `hapto_J3-J5` - hapto_J0,
    hapto_trend = if_else(delta_hapto > 0, "increase", "decrease")
  )

df_cDPP3_long <- df %>%
  select(ID, DPP3_D0, DPP3_D3_5) %>%
  pivot_longer(cols = starts_with("DPP3_"), names_to = "timepoint", values_to = "value") %>%
  mutate(
    timepoint = str_remove(timepoint, "^DPP3_"),
    timepoint = case_when(
      timepoint == "D0"   ~ "implantation",
      timepoint == "D3_5" ~ "day 3 to 5",
      TRUE ~ timepoint
    ),
    timepoint = factor(timepoint, levels = c("implantation", "day 3 to 5")),
    log_value = log10(value)
  )   

df_cDPP3_plot <- df_cDPP3_long %>%
  arrange(ID, timepoint) %>%
  group_by(ID) %>%
  mutate(
    delta_cDPP3 = value[timepoint == "day 3 to 5"] -
      value[timepoint == "implantation"],
    cdpp3_trend = if_else(delta_cDPP3 > 0, "increase", "decrease")
  ) %>%
  ungroup() %>%
  left_join(df_hapto_delta, by = "ID")%>%
  filter(!is.na(cdpp3_trend), !is.na(hapto_trend))


df_cDPP3_plot%>%
  group_by(cdpp3_trend,hapto_trend) %>%
  summarise(n = n_distinct(ID), .groups = "drop")

###test statistiques d'interaction####
mod <- lmer(log_value ~ timepoint * hapto_trend + (1 | ID), data = df_cDPP3_plot)
anova_hapto_mod <- anova(mod, ddf = "Kenward-Roger")

p_inter <- format.pval(
  anova_hapto_mod["timepoint:hapto_trend", "Pr(>F)"],
  digits = 3, eps = 0.001)

df_panels <- bind_rows(
  
  # Panel 1 — Global (all patients)
  df_cDPP3_plot %>%
    dplyr::mutate(panel = "Global"),
  
  # Panel 2 — Hapto decrease
  df_cDPP3_plot %>%
    filter(hapto_trend == "decrease") %>%
    mutate(panel = "Haptoglobin decrease"),
  
  # Panel 3 — Hapto increase
  df_cDPP3_plot %>%
    filter(hapto_trend == "increase") %>%
    mutate(panel = "Haptoglobin increase"),
  
  # Panel 4 — Hapto decrease + cDPP3 increase
  df_cDPP3_plot %>%
    filter(hapto_trend == "decrease", cdpp3_trend == "increase") %>%
    mutate(panel = "Haptoglobin decrease\n+ cDPP3 increase")
)


annot_df <- data.frame(
  panel = "Global",
  x = 1.5,
  y = max(df_panels$log_value, na.rm = TRUE) * 1.05,
  label = paste0("Interaction p = ", p_inter)
)


hapto_stats <- df_cDPP3_plot %>%
  dplyr::filter(timepoint %in% c("implantation")) %>%
  select(-hapto_trend, -cdpp3_trend, -delta_cDPP3, -delta_hapto,-value,-log_value,-timepoint) %>%
  pivot_longer(
    cols = starts_with("hapto_"),
    names_to = "timepoint",
    values_to = "hapto_value"
  ) %>%
  dplyr::group_by(timepoint) %>%
  dplyr::summarise(
    med = median(hapto_value, na.rm = TRUE),
    q1  = quantile(hapto_value, 0.25, na.rm = TRUE),
    q3  = quantile(hapto_value, 0.75, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    label = paste0(
      round(med, 2), " [",
      round(q1, 2), "–",
      round(q3, 2), "]"
    )
  )

hapto_map <- hapto_stats %>%
  mutate(
    timepoint = case_when(
      timepoint == "hapto_J0"     ~ "implantation",
      timepoint == "hapto_J3-J5"  ~ "day 3 to 5"
    ))%>%
  select(timepoint, label)


df_panels <- df_panels %>%
  mutate(
    timepoint_x = factor(case_when(
      panel == "Global" & timepoint == "implantation" ~ paste0("implantation","\n", 
       "haptoglobin:\n", hapto_map$label[hapto_map$timepoint == "implantation"], "\nng/mL"),
      
      panel == "Global" & timepoint == "day 3 to 5" ~paste0("day 3 to 5","\n", 
                                                            "haptoglobin:\n",  hapto_map$label[hapto_map$timepoint == "day 3 to 5"], "\nng/mL"),
      
      TRUE ~ timepoint
    )))%>%
  mutate(
    timepoint_x = factor(
      timepoint_x,
      levels = c(
        "implantation\nhaptoglobin:\n1.01 [0.16–1.73]\nng/mL",
        "day 3 to 5\nhaptoglobin:\n0.66 [0–1.51]\nng/mL",
        "implantation",
        "day 3 to 5"
      )
    )
  ) %>%
  droplevels()

panel_letters <- data.frame(
  panel = levels(factor(df_panels$panel)),
  label = LETTERS[seq_along(levels(factor(df_panels$panel)))],
  x = -Inf,
  y = Inf
)

spagh_plot <- ggplot(
  df_panels,
  aes(
    x = timepoint_x,
    y = log_value,
    group = ID,
    color = hapto_trend
  )
) +
  geom_line() +
  geom_point(size = 2) +
  
  scale_color_manual(
    values = c(
      "increase" = "#E63946",
      "decrease" = "#457B9D"
    ),
    labels = c(
      "increase" = "haptoglobin increase",
      "decrease" = "haptoglobin decrease"
    )
  ) +
  scale_y_continuous(limits = c(min(df_panels$log_value), max(df_panels$log_value)+0.2)
  ) +
  
  facet_wrap(~ panel, nrow = 1, scales = "free")+  
  labs(
    x = "",
    y = "cDPP3 (log scale)",
    color = ""
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x  = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )+
  geom_text(
    data = annot_df,
    aes(x = x, y = 3.1, label = label),
    inherit.aes = FALSE,
    hjust = 0.5,
    size = 4,
    fontface = "bold"
  )+
  geom_text(
    data = panel_letters,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = -0.2,
    vjust = 1.2,
    size = 5,
    fontface = "bold"
  )


ggsave(
  "FIGURES/Figure_cDPP3_spaghetti.pdf",
  plot   = spagh_plot,
  width  = 200, height = 120, units = "mm",
  device = cairo_pdf, bg = "white", scale = 2)  

#####CORRELATIONS DPP3####
  ###calculate delta####
  df <- df %>%
    mutate(
      DPP3_delta_D0_D3_5 = log10(DPP3_D3_5) - log10(DPP3_D0),
      delta_Lact_D0_D3_5 = jp2_lact - Lact_D0,
      delta_NADcum_D0_D3_5 = jp2_NAd_cum_w - NAD_24H,
      delta_ASAT_D0_D3_5 = jp2_asat - ASAT_D0,
      delta_CKD_D0_D3_5 = CKD_J3J5 - CKD_D0 )
  
  
  
  p_corr_NAD <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_NADcum_D0_D3_5",
    "Δ cDPP3\n(log scale)",
    "Δ Cumulated norepinephrine dose\n(γ/kg)"
  )
  
  p_corr_Lact <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_Lact_D0_D3_5",
    "Δ cDPP3\n(log scale)",
    "Δ Lactate\n(mmol/L)"
  )
  
  
  p_corr_ASAT <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_ASAT_D0_D3_5",
    "Δ cDPP3\n(log scale)",
    "Δ ASAT\n(IU/L)"
  )
  
  p_corr_CKD <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_CKD_D0_D3_5",
    "Δ cDPP3\n(log scale)",
    "Δ eGFR\n(ml/min/1.73m²)"
  )
  
  Figure_correlation_DPP3 <- (p_corr_NAD + p_corr_Lact) /
    (p_corr_ASAT + p_corr_CKD) +
    plot_annotation(title = "",
                    tag_levels = c("A","B","C","D")) & theme(plot.title=element_text(hjust=0.5))
  ggsave("FIGURES/Figure_correlation_DPP3.pdf",
         plot   = Figure_correlation_DPP3,
         width  = 160, height = 200, units = "mm",
         device = cairo_pdf, bg = "white",scale=1)

