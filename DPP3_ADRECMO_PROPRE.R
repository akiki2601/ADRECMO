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

#==============================================================================
# TABLE 1 — Baseline characteristics by DPP3_median
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

t1_tableone_print <- print(
  t1_tableone_obj,
  nonnormal      = t1_vars_cont,
  contDigits     = 1,
  showAllLevels  = FALSE
)

df_t1_tableone <- as.data.frame(t1_tableone_print) %>%
  tibble::rownames_to_column("Variables") %>%
  dplyr::select(Variables, `below median`, `above median`, p)

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

df_t1_final <- df_t1_tableone_id %>%
  left_join(df_t1_missing, by = "raw_id") %>%
  transmute(
    Variables,
    Missing = n_missing,
    `low cDPP3`  = `below median`,
    `high cDPP3` = `above median`,
    p
  )

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
  ~Variables, ~Missing, ~`low cDPP3`, ~`high cDPP3`, ~p,
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
  mutate(
    is_section = is.na(Missing) & `low cDPP3` == "" & `high cDPP3` == ""
  )

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
save_as_docx(ft_t1, path = "Tables/Table1_characteristics_DPP3.docx")

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

hl_diff_ci <- purrr::map_dfr(
  t1_vars_cont,
  ~{
    x <- df_t1_input[[.x]]
    g <- df_t1_input$DPP3_median
    
    ok <- !is.na(x) & !is.na(g)
    x  <- x[ok]
    g  <- droplevels(g[ok])
    
    # si un groupe vide -> NA
    if (nlevels(g) < 2 || any(table(g) == 0)) {
      return(tibble(
        raw_id = .x,
        `Median difference [95% CI]` = NA_character_
      ))
    }
    
    wt <- suppressWarnings(
      wilcox.test(
        x ~ g,
        conf.int = TRUE,
        exact = FALSE
      )
    )
    
    # estimate = HL shift (tends to represent a "median difference" / location shift)
    est <- unname(wt$estimate)
    ci  <- unname(wt$conf.int[1:2])
    
    tibble(
      raw_id = .x,
      `Median difference [95% CI]` =
        sprintf("%.1f [%.1f; %.1f]", est, ci[1], ci[2])
    )
  }
)

# --- Différence de proportions + IC95% via BinomDiffCI (méthode scorecc ou mn) ---
diff_prop_ci <- purrr::map_dfr(
  t1_vars_cat,
  ~{
    x <- df_t1_input[[.x]]
    g <- df_t1_input$DPP3_median
    
    # masquage des NA
    ok <- !is.na(x) & !is.na(g)
    x  <- x[ok]
    g  <- droplevels(g[ok])
    
    # si moins de 2 niveaux ou effectifs nuls → NA
    if (nlevels(g) < 2 || any(table(g) == 0)) {
      return(tibble(
        raw_id = .x,
        `Difference in proportions (% points) [95% CI]` = NA_character_
      ))
    }
    
    # compter succès (par défaut niveau "1" ou TRUE)
    tab <- table(g, x)
    levs <- colnames(tab)
    event <- if ("1" %in% levs) "1" else levs[2]  # prendre "1" si possible, sinon second niveau
    
    x1 <- sum(x[g == "above median"] == event)
    n1 <- sum(g == "above median")
    x0 <- sum(x[g == "below median"] == event)
    n0 <- sum(g == "below median")
    
    # IC de différence de proportions (scorecc ou mn)
    ci <- tryCatch(
      DescTools::BinomDiffCI(
        x1 = x1, n1 = n1,
        x2 = x0, n2 = n0,
        conf.level = 0.95,
        method = "ac"     
      ),
      error = function(e) NULL
    )
    
    ?BinomDiffCI
    
    if (is.null(ci)) {
      return(tibble(
        raw_id = .x,
        `Difference in proportions (% points) [95% CI]` = NA_character_
      ))
    }
    
    # diff observée en pourcentage
    diff_pct  <- 100 * (x1/n1 - x0/n0)
    low_pct   <- 100 * ci[2]
    high_pct  <- 100 * ci[3]
    
    tibble(
      raw_id = .x,
      `Difference in proportions (% points) [95% CI]` =
        sprintf("%+.1f [%+.1f; %+.1f]", diff_pct, low_pct, high_pct)
    )
  }
)

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
    `low cDPP3`  = `below median`,
    `high cDPP3` = `above median`,
    `Group difference [95% CI]`
  )



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
  ~Variables, ~Missing, ~`low cDPP3`, ~`high cDPP3`, ~`Group difference [95% CI]`,
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
  mutate(
    is_section = is.na(Missing) & `low cDPP3` == "" & `high cDPP3` == ""
  )

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
# TABLE 2 — Outcomes / management by DPP3_median
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
    across(all_of(t2_vars_cat), as.factor)
  )

t2_tableone_obj <- CreateTableOne(
  vars       = t2_vars_all,
  factorVars = t2_vars_cat,
  strata     = "DPP3_median",
  data       = df_t2_input
)

t2_tableone_print <- print(
  t2_tableone_obj,
  nonnormal      = t2_vars_cont,
  contDigits     = 1,
  showAllLevels  = FALSE
)

df_t2_final <- as.data.frame(t2_tableone_print) %>%
  tibble::rownames_to_column("Variables") %>%
  select(Variables, `below median`, `above median`, p) %>%
  mutate(Variables = str_remove(Variables, " \\(median \\[IQR\\]\\)")) %>%
  transmute(
    Variables,
    `low cDPP3`  = `below median`,
    `high cDPP3` = `above median`,
    p
  )

dict_t2_labels <- c(
  "Outcome (%)"              = "Outcome",
  "   Bridge to LVAD"        = "Bridge to LVAD (%)",
  "   Bridge to Transplant"  = "Bridge to Transplant (%)",
  "   Death"                 = "Death in ICU (%)",
  "   ECMO Weaning"          = "ECMO Weaning (%)",
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

df_t2_display <- df_t2_final %>%
  mutate(
    Variables = dplyr::recode(Variables, !!!dict_t2_labels),
    Variables = factor(Variables, levels = t2_order)
  ) %>%
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
save_as_docx(ft_t2, path = "Tables/Table2_Outcome_DPP3.docx")

# Optional: export issue file
write.csv2(
  x = df %>% dplyr::select(ID, Outcome),
  file = "issue.csv",
  row.names = FALSE
)


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


# Hodges–Lehmann for continuous outcomes
hl_diff_ci_t2 <- purrr::map_dfr(
  t2_vars_cont,
  ~{
    x <- df_t2_input[[.x]]
    g <- df_t2_input$DPP3_median
    
    ok <- !is.na(x) & !is.na(g)
    x  <- x[ok]
    g  <- droplevels(g[ok])
    
    if (nlevels(g) < 2 || any(table(g) == 0)) {
      return(tibble(raw_id = .x, Group_diff_CI = NA_character_))
    }
    
    wt <- suppressWarnings(
      wilcox.test(x ~ g, conf.int = TRUE, exact = FALSE)
    )
    est <- unname(wt$estimate)
    ci  <- unname(wt$conf.int[1:2])
    
    tibble(
      raw_id = .x,
      Group_diff_CI = sprintf("%.1f [%.1f; %.1f]", est, ci[1], ci[2])
    )
  }
)



# Difference in proportions for categorical outcomes
prop_diff_ci_t2 <- purrr::map_dfr(
  t2_vars_cat_bin,
  ~{
    x <- df_t2_input[[.x]]
    g <- df_t2_input$DPP3_median
    
    ok <- !is.na(x) & !is.na(g)
    x  <- x[ok]
    g  <- droplevels(g[ok])
    
    if (nlevels(g) < 2 || any(table(g) == 0)) {
      return(tibble(raw_id = .x, Group_diff_CI = NA_character_))
    }
    
    x <- as.factor(x)
    levs <- levels(x)
    event <- if ("1" %in% levs) "1" else levs[2]
    
    x1 <- sum(x[g == "above median"] == event)
    n1 <- sum(g == "above median")
    x0 <- sum(x[g == "below median"] == event)
    n0 <- sum(g == "below median")
    
    ci <- tryCatch(
      DescTools::BinomDiffCI(
        x1 = x1, n1 = n1,
        x2 = x0, n2 = n0,
        conf.level = 0.95,
        method = "ac"
      ),
      error = function(e) NULL
    )
    
    if (is.null(ci)) {
      Group_diff_CI <- NA_character_
    } else {
      diff_pct <- 100 * (x1/n1 - x0/n0)
      low_pct  <- 100 * ci[2]
      high_pct <- 100 * ci[3]
      Group_diff_CI <- sprintf("%+.1f [%+.1f; %+.1f]",
                               diff_pct, low_pct, high_pct)
    }
    
    tibble(raw_id = .x, Group_diff_CI = Group_diff_CI)
  }
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
    `low cDPP3`  = `below median`,
    `high cDPP3` = `above median`,
    `Group difference [95% CI]`
  )

t2_sections <- tibble::tribble(
  ~Variables, ~`low cDPP3`, ~`high cDPP3`, ~`Group difference [95% CI]`,
  "Outcome", "", "", ""
)
df_t2_final$Variables

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
    Variables = factor(Variables, levels = t2_order)) %>%
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

plot_box_by_dpp3_group <- function(df_in, y_var, y_lab, x_lab = NULL) {
  
  
  fmt_p <- function(p, digits = 3, eps = 0.001) {
    if (is.na(p)) return(NA_character_)
    if (p < eps) return(paste0("< ", eps))
    formatC(p, format = "f", digits = digits)
  }
  

  df_counts <- df_in %>%
    group_by(DPP3_median) %>%
    summarise(n = sum(!is.na(.data[[y_var]])), .groups = "drop")
  
  test_wilcox <- wilcox.test(as.formula(paste0(y_var, " ~ DPP3_median")), data = df_in)
  p_lbl <- fmt_p(test_wilcox$p.value, digits = 3, eps = 0.001)
  
  y_max <- max(df_in[[y_var]], na.rm = TRUE)
  y_min <- min(df_in[[y_var]], na.rm = TRUE)
  
  ggplot(df_in, aes(x = DPP3_median, y = .data[[y_var]], fill = DPP3_median)) +
    geom_boxplot() +
    labs(x = x_lab, y = y_lab) +
    theme_classic() +
    theme(legend.position = "none") +
    annotate("text", x = 1, y = y_max * 1.10, label = paste0("p = ", p_lbl), hjust = 0, size = 4) +
    geom_text(
      data = df_counts,
      aes(x = DPP3_median, y = y_min, label = paste0("n=", n)),
      size = 4, vjust = 1
    )
}

p_dpp3_sofa <- plot_box_by_dpp3_group(df, "sofa_D0", "SOFA")
p_dpp3_lact <- plot_box_by_dpp3_group(df, "Lact_D0", "Lactate (mmol/L)")
p_dpp3_nad  <- plot_box_by_dpp3_group(df, "NADcum_D0_w", "Cumulated norepinephrine dose\nday of ECMO implantation (γ/kg)")
p_dpp3_ckd  <- plot_box_by_dpp3_group(df, "CKD_D0", "CKD-EPI (ml/min/1.73m²)")
p_dpp3_flow <- plot_box_by_dpp3_group(df, "j0_ecmo_debit", "ECMO Flow (L/min)", x_lab = "cDPP3")
p_dpp3_alat <- plot_box_by_dpp3_group(df, "ALAT_D0", "ALAT (IU/L)", x_lab = "cDPP3")

fig_dpp3_baseline <- (p_dpp3_sofa + p_dpp3_lact) /
  (p_dpp3_nad + p_dpp3_ckd) /
  (p_dpp3_flow + p_dpp3_alat) +
  plot_annotation(
    title = sprintf(
      "Association between cDPP3 at ECMO implantation and organ dysfunction\ncDPP3 median = %.0f ng/mL",
      median(df$DPP3_D0, na.rm = TRUE)
    ),
    tag_levels = c("A", "B", "C", "D", "E", "F")
  ) & theme(plot.title = element_text(hjust = 0.5))

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

cox_mv_tidy <- tidy(cox_mv, exponentiate = TRUE, conf.int = TRUE)
cox_uni_tidy <- tidy(cox_uni, exponentiate = TRUE, conf.int = TRUE)

lab_uni <- cox_uni_tidy %>%
  mutate(label = sprintf("HR = %.2f (95%% CI %.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

lab_adj <- cox_mv_tidy %>%
  filter(term == "DPP3_medianabove median") %>%
  mutate(label = sprintf("aHR = %.2f (95%% CI %.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

lab_surv <- paste(lab_uni, lab_adj, sep = "\n")

ph_test_mv <- cox.zph(cox_mv)

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
  annotate("text", x = 5, y = 0.15, label = lab_surv, hjust = 0, size = 4)

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
  summarise(n = sum(!is.na(value)), .groups = "drop")

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
  labs(x = "Time points", y = "DPP3 (log scale)", color = "Outcomes") +
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x  = element_blank())

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

  
  #####CORRELATIONS DPP3####
  ###calculate delta####
  df <- df %>%
    mutate(
      DPP3_delta_D0_D3_5 = DPP3_D3_5 - DPP3_D0,
      delta_Lact_D0_D3_5 = jp2_lact - Lact_D0,
      delta_NADcum_D0_D3_5 = jp2_NAd_cum_w - NAD_24H,
      delta_ASAT_D0_D3_5 = jp2_asat - ASAT_D0,
      delta_CKD_D0_D3_5 = CKD_J3J5 - CKD_D0 )
  
  
  correlation_DPP3_NAD <- cor.test(df$DPP3_delta_D0_D3_5, df$delta_NADcum_D0_D3_5, method = "spearman", use = "complete.obs")
  correlation_DPP3_Lactate <- cor.test(df$DPP3_delta_D0_D3_5, df$delta_Lact_D0_D3_5, method = "spearman", use = "complete.obs")
  correlation_DPP3_SOFA <- cor.test(x = df$DPP3_delta_D0_D3_5, y = df$delta_SOFA_D0_D3_5, method = "spearman", use = "complete.obs")
  correlation_DPP3_AST <- cor.test(df$DPP3_delta_D0_D3_5, y=df$delta_ASAT_D0_D3_5, method = "spearman", use = "complete.obs")
  correlation_DPP3_CKD <- cor.test(df$DPP3_delta_D0_D3_5, y=df$delta_CKD_D0_D3_5, method = "spearman", use = "complete.obs")
  
  
  df_corr_long <- df %>%
    select(
      DPP3_delta_D0_D3_5,
      delta_NADcum_D0_D3_5,
      delta_Lact_D0_D3_5,
      delta_ASAT_D0_D3_5,
      delta_CKD_D0_D3_5
    ) %>%
    pivot_longer(
      cols = -DPP3_delta_D0_D3_5,
      names_to = "variable",
      values_to = "delta_outcome"
    )
  
  
  plot_spearman <- function(data, x, y, xlab, ylab) {
    
    test <- cor.test(data[[x]], data[[y]],
                     method = "spearman",
                     use = "complete.obs")
    
    x_min <- min(data[[x]], na.rm = TRUE)
    
    label <- paste0(
      "ρ = ", round(test$estimate, 2),
      "\n p = ", fmt_p(test$p.value)
    )
    
    ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      annotate(
        "text",
        x = x_min+500, y = Inf,
        label = label,
        hjust = 1.05, vjust = 1.1,
        size = 3.5) +
      labs(x = xlab, y = ylab, title = "Day 3 to 5 minus Day 0 changes") +
      theme_minimal()
  }
  
  p_corr_NAD <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_NADcum_D0_D3_5",
    "Δ cDPP3 (ng/ml)",
    "Δ Cumulated norepinephrine dose (γ/kg)"
  )
  
  p_corr_Lact <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_Lact_D0_D3_5",
    "Δ cDPP3 (ng/ml)",
    "Δ Lactate (mmol/L)"
  )
  
  
  p_corr_ASAT <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_ASAT_D0_D3_5",
    "Δ cDPP3 (ng/ml) ",
    "Δ AST (IU/L)"
  )
  
  p_corr_CKD <- plot_spearman(
    df,
    "DPP3_delta_D0_D3_5",
    "delta_CKD_D0_D3_5",
    "Δ cDPP3 (ng/ml) ",
    "Δ CKD-EPI (ml/min/1.73m²)"
  )
  
  Figure_correlation_DPP3 <- (p_corr_NAD + p_corr_Lact) /
    (p_corr_ASAT + p_corr_CKD) +
    plot_annotation(title = "Correlations between changes in cDPP3 and\n organ function from ECMO implantation to day 3–5",
                    tag_levels = c("A","B","C","D")) & theme(plot.title=element_text(hjust=0.5))
  ggsave("FIGURES/Figure_correlation_DPP3.pdf",
         plot   = Figure_correlation_DPP3,
         width  = 160, height = 220, units = "mm",
         device = cairo_pdf, bg = "white",scale=1.05)

