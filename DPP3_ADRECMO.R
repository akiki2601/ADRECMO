#####Table 1#######
var.cat <- c("Gender", "HTN", "DM", "Immunodepression", "CKD", "Neurologic_deficit", "Chronic_respiratory_disease", "HF","Alphablocker","Betablocker", "cause","ische", "Cardiac_arrest_before_canul", "Intubation")
var.cont <- c("Age", "BMI", "PAS_D0", "PAD_D0", "PAM_D0", "HR_D0", "Preecmo_tte_ef", "Preecmo_tte_vtiao","sofa_D0", "NAD_24H", "DOB_24H", "Lact_D0", "Creat_D0",  "ASAT_D0", "Bili_D0", "Tropo_i_hs_D0", "Ntprobnp_D0",  "Plq_D0")
var.tot <- c(var.cat, var.cont)



T1 <- df %>%
  dplyr::select(all_of(var.tot),DPP3_median) %>%
  mutate(across(all_of(var.cont), as.numeric), 
         across(all_of(var.cat), as.factor))

table1 <- CreateTableOne(vars = var.tot, factorVars = var.cat,strata = "DPP3_median", data = T1) 
table1 <- print(table1, nonnormal = var.cont, contDigits = 1, showAllLevels = FALSE)

table1_df <- as.data.frame(table1)
table1_df$Variables <- row.names(table1_df)
row.names(table1_df) <- NULL
table1_df <- table1_df[, c(5,1:3)]

T1_n <- T1%>%
  summarise(across(everything(), ~ sum(is.na(.))))%>%
  pivot_longer(cols= everything(),
               values_to = "n_missing",
               names_to = "ID")

table1_df1 <- table1_df %>%
  mutate(ID = sub("\\(median \\[IQR\\]\\)|= 1 \\(\\%\\)|= Women \\(\\%\\)|\\(\\%\\)|= Non ischémique \\(\\%\\)|= ischémique \\(\\%\\)", "",Variables),
         ID = trimws(ID))%>%
  mutate(ID =sub(" = YES","",ID))

table_1_vf <- left_join(table1_df1, T1_n, by = "ID") %>%
  select(Variables, n_missing, `below median`, `above median`, p) %>%
  mutate(Variables = str_remove(Variables, " \\(median \\[IQR\\]\\)"))%>%
  rename(`low cDPP3`=`below median`,
         `high cDPP3`=`above median`)


traduction <- c(
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

# Application du dictionnaire de traduction
table_1_vf <- table_1_vf %>%
  mutate(Variables = dplyr::recode(Variables, !!!traduction))%>%
  rename(Missing = n_missing)

sections <- tibble::tribble(
  ~Variables, ~Missing, ~Overall,
  "Demographics", "", "",
  "Medical History", "", "",
  "Prior-adrenergic medications", "", "",
  "Cardiogenic shock cause", "", "",
  "Pre-ECMO Clinical parameters", "", "",
  "Pre-ECMO heart function", "", "",
  "Pre-ECMO supports", "", "",
  "Pre-ECMO Biological assessment", "", ""
)%>%
  mutate(Missing = as.integer(Missing))


# Ordre des variables corrigé (selon la table fournie)
order <- c(
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
  "Levosimendan (%)",
  
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

table_1_vf2 <- bind_rows(sections, table_1_vf) %>%
  mutate(Variables = factor(Variables, levels = order)) %>%
  arrange(Variables)




table1 <- flextable(table_1_vf2) %>%
  set_caption("Table 1: Characteristics of the population before ECMO implantation") %>% 
  autofit() %>%
  align(j = 2:3, align = "center") %>%
  bold(i = ~ is.na(Missing) & Overall == "") %>%        # titres en gras
  align(i = ~ is.na(Missing) & Overall == "", j = 1, align = "left") %>% 
  bg(i = ~ is.na(Missing) & Overall == "", bg = "#FFFFFF") %>%
  hline_bottom(part = "header", border = fp_border(color="black", width = 1)) %>%  # sous l'en-tête
  hline_bottom(part = "body",   border = fp_border(color="black", width = 1)) %>%  # ligne finale du tableau
  add_footer_lines(
    values = "Data are expressed in n(%) for categorical variables and median [25th–75th] for continuous variables."
  ) %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 9)



table1
save_as_docx(table1, path = "Tables/Table1_characteristics_DPP3.docx")



# TABLE 2: Management and Outcomes

var.cont = c("Lenght_eer",
             "Lenght_vm",
             "Time_hosp",
             "Time_ecmo",
             "ECMO_duration_weaned",
             "ECMO_duration_Dead")

var.cat = c("BCPIA_Tot", "EER_Tot" ,"Outcome","j90_deces", "j28_deces")

var.tot = c(var.cat, var.cont)

T2 <- df %>%
  dplyr::select(all_of(var.tot), DPP3_median) %>%
  mutate(across(all_of(var.cont), as.numeric), 
         across(all_of(var.cat), as.factor))

table2 <- CreateTableOne(vars = var.tot, factorVars = var.cat, strata = "DPP3_median", data = T2) 
table2 <- print(table2, nonnormal = var.cont, contDigits = 1, showAllLevels = FALSE)

table2_df <- as.data.frame(table2)
table2_df$Variables <- row.names(table2_df)
row.names(table2_df) <- NULL
table2_df <- table2_df[, c(5,1:3)]

table_2_vf <-table2_df  %>%
  select(Variables, `below median`, `above median`, p) %>%
  mutate(Variables = str_remove(Variables, " \\(median \\[IQR\\]\\)"))%>%
  rename(`low cDPP3`=`below median`,
         `high cDPP3`=`above median`)

traduction <- c(
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
  "ECMO_duration_weaned"                = "ECMO Duration in weaned (days)",
  "ECMO_duration_Dead"                = "ECMO Duration in deceased (days)"
  
)

# 3) Ordre final
order <- c(
  "n",
  "ECMO Weaning (%)",
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
  "28-day Mortality (%)",
  "90-day Mortality (%)"
)

# 4) Application traduction + ordre + renommage Missing
table_2_vf2 <- table_2_vf %>%
  mutate(
    Variables = dplyr::recode(Variables, !!!traduction),
    Variables = factor(Variables, levels = order)
  ) %>%
  arrange(Variables)

# 5) Flextable

table2 <- flextable(table_2_vf2) %>%
  set_caption("Table 2: Outcomes") %>% 
  autofit() %>%
  align(j = 2, align = "center") %>%
  bold(i = ~ Variables == "Outcome") %>%
  align(i = ~ Variables == "Outcome", j = 1, align = "left") %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
  add_footer_lines(
    values = "Data are expressed in n(%) for categorical variables and median [25th–75th] for continuous variables."
  ) %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 9)

table2
save_as_docx(table2, path = "Tables/Table2_Outcome_DPP3.docx")

write.csv2(
  x = df %>% dplyr::select(ID, Outcome),
  file = "issue.csv",
  row.names = FALSE
)

####FIGURE DPP3 BASELINE#######
figure_DPP3 <-function(data,
                       y_var,
                       y_lab,
                       x_lab=NULL){
  
  count <- df%>%
    group_by(DPP3_median) %>%
    summarise(n = sum(!is.na(.data[[y_var]])))
  
  p <- ggplot(data, aes(x = DPP3_median, y = .data[[y_var]], fill = DPP3_median)) +
    geom_boxplot() +
    labs(
      title = "",
      x = x_lab,
      y = y_lab
    ) + 
    scale_fill_manual(values = c("#0000FF","#F4A300"))    +
    theme_classic()+
    theme(legend.position = "none")
  
MW <- wilcox.test(as.formula(paste0(y_var,"~DPP3_median")), data)
  
  pvalue <- format.pval(MW$p.value, digits = 3, eps = .001)
  
  p <- p +
    annotate(
      "text",
      x = 1,
      y = max(data[[y_var]], na.rm = TRUE) * 1.1,
      label = paste0("p: ", pvalue),
      parse = FALSE,
      size = 4,
      hjust = 0
    ) +
    geom_text(
      data = count,
      aes(
        x = DPP3_median,
        y =  min(data[[y_var]], na.rm = TRUE),
        label = paste0("n=", n)
      ),
      position = position_dodge(0.7),
      size = 4,
      vjust = 1
    )
    
  
  print(p)
}

figure_DPP3_a<- figure_DPP3(data=df, "sofa_D0", "SOFA")

figure_DPP3_b<- figure_DPP3(data=df, "Lact_D0", "lactate\n(mmol/L)")

figure_DPP3_c<- figure_DPP3(data=df, "NADcum_D0_w", "cumulated norepinephrine dose\nday of ECMO implantation (γ/kg)")

figure_DPP3_d <- figure_DPP3(data=df, "CKD_D0", "CKD-EPI\n(ml/min/1.73m²)")

figure_DPP3_e <- figure_DPP3(data=df, "j0_ecmo_debit", "ECMO Flow \n(L/min)","cDPP3")

figure_DPP3_f <- figure_DPP3(data=df, "ALAT_D0", "ALAT\n(UI/l)", "cDPP3")

Figure_DPP3_base <- (figure_DPP3_a + figure_DPP3_b) /
  (figure_DPP3_c + figure_DPP3_d) /
  (figure_DPP3_e + figure_DPP3_f) +
  plot_annotation(title="Association between cDDP3 at ECMO implantation and organ dysfunction\ncDPP3 median= 73ng/ml",
                  tag_levels = c("A","B","C","D","E","F")) & theme(plot.title=element_text(hjust=0.5))


ggsave("FIGURES/Figure_DPP3_base.pdf",
       plot   = Figure_DPP3_base,
       width  = 160, height = 200, units = "mm",
       device = cairo_pdf, bg = "white",scale=1.5)

###Survival####
df <- df %>%
  mutate(diff_days_30 = case_when(diff_days>=30 ~ 30,
                                  TRUE ~ diff_days),
         outcome_censored_30 = case_when(diff_days >= 30 & outcome_censored == 1 ~ 0,
                                       TRUE ~ outcome_censored
                                        ),
    DPP3_median = ifelse(DPP3_D0 <= median(DPP3_D0, na.rm = TRUE), "below median", "above median"),
         DPP3_median = factor(DPP3_median, levels = c("below median", "above median"))
  )


km_fit_comp <- survfit(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median , data = df)

mod_censored_simple <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median , data = df)


df <- df %>%
  mutate(
    ecmo_start_date = if_else(
      ID == "10-HC",
      as.Date("2018-06-27"),
      ecmo_start_date
    )
  )

df <- df%>%
  mutate(time_adm_ECMO = time_length(interval(icu_admiss_date, ecmo_start_date), "days"),
         Cardiac_arrest_before_canul = as.factor(Cardiac_arrest_before_canul),
         cause = as.factor(cause)
         )


ggplot(df, aes(x=Cardiac_arrest_before_canul, y=time_adm_ECMO))+
  geom_boxplot()

mod_censored_MV <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median  + time_adm_ECMO + cause, data = df)
mod_censored_MV2 <- coxph(Surv(diff_days_30, outcome_censored_30) ~ DPP3_median  + time_adm_ECMO + cause + Cardiac_arrest_before_canul, data = df)

summary(mod_censored_MV)
summary(mod_censored_MV2)

anova(mod_censored_MV, mod_censored_simple)

mod_censored_MV_cox <- tidy(mod_censored_MV, exponentiate = TRUE, conf.int = TRUE)

res_cox <- tidy(mod_censored_simple, exponentiate = TRUE, conf.int = TRUE)

lab <- res_cox %>%
  mutate(label = sprintf("HR = %.2f (95%% CI %.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

lab2 <- mod_censored_MV_cox %>%
  filter(term == "DPP3_medianabove median") %>%
  mutate(label = sprintf("aHR = %.2f (95%% CI %.2f–%.2f)", estimate, conf.low, conf.high)) %>%
  pull(label)

test_PH <- cox.zph(mod_censored_MV)
test_PH
plot(test_PH)



lab3 <- median(df$DPP3_D0, na.rm = TRUE) %>%
  sprintf("Median DPP3 at D0 = %.0f ng/mL", .)

lab_T <- paste0(lab, "\n", lab2) 


Figure_survival <- ggsurvplot(km_fit_comp, data = df, 
                        pval = F,
                        conf.int = FALSE,
                        risk.table = TRUE,
                        risk.table.height = 0.2,
                        risk.table.y.text = TRUE, risk.table.title = "Number at Risk",
                        xlim = c(0, 30), ylab = "30 day survival", break.time.by = 5,
                        legend.title = " ", palette = c("#E63946", "#457B9D"), ggtheme = theme_classic(),surv.scale = "percent",
                        legend.labs = c("below median", "above median"),
                        legend = "none")
Figure_survival$plot <- Figure_survival$plot +
  annotate(
    "text",
    x = 5,
    y = 0.15,
    label = lab_T,
    hjust = 0,
    size = 4
  )

Figure_survival <- ggarrange(
  Figure_survival$plot,
  Figure_survival$table,
  ncol = 1, nrow = 2,
  heights = c(3, 1)
)
ggsave("FIGURES/Figure_survival_DPP3.pdf",
       plot   = Figure_survival,
       width  = 180, height = 140, units = "mm",
       device = cairo_pdf, bg = "white")

#######
df_long <- df%>%
  select(ID,DPP3_D0, DPP3_D3_5,DPP3_w, Outcome_death_bridge)%>%
  pivot_longer(
    cols = starts_with("DPP3_"),
    names_to = c("timepoint"),
    values_to = "value"
  )%>%
  mutate(timepoint = str_remove(timepoint, "^DPP3_"),
         timepoint = case_when(timepoint=="D0" ~ "implantation",
                               timepoint=="D3_5" ~ "day 3 to 5",
                               timepoint=="w" ~ "explantation"),
         timepoint = factor(timepoint, levels = c("implantation", "day 3 to 5", "explantation")),
         value_log= log10(value)
         )

df_long_count<- df_long %>%
  group_by(timepoint) %>%
  summarise(n = sum(!is.na(value)))
library(rstatix)

model <- lmer(
  log(value) ~ timepoint + (1 | ID),
  data = df_long
)


res <- resid(model)

hist(res, breaks = 30, main = "Residuals", xlab = "Residuals")
qqnorm(res)
qqline(res, col = "red")

anova_results <- anova(model, ddf = "Kenward-Roger")
p_time <- format.pval(anova_results["timepoint", "Pr(>F)"], digits = 3, eps = .001)


Figure_DPP3_1 <- ggplot(df_long, aes(x = timepoint, y = value_log)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "", x = "Time", y = "DPP3 (log scale)") +
  geom_text(data = df_long_count,
    aes(
      x = timepoint,
      y =  0,
      label = paste0("n=", n)
    )) +
  annotate(
    "text",
    x = 1.5,
    y = max(df_long$value_log, na.rm = TRUE) * 1.5,
    label = paste0("p = ", p_time),
    size = 4,
    hjust = 0
  ) +
  theme_minimal()
#######
df_count_DDP3_2 <- df_long %>%
  dplyr::group_by(timepoint, Outcome_death_bridge) %>%
  dplyr::summarise(
    n = sum(!is.na(value)),
    .groups = "drop"
  )

model_DPP3_2 <- lmer(
  value_log ~ timepoint * Outcome_death_bridge + (1 | ID),
  data = df_long
)
res_DPP3_2 <- resid(model_DPP3_2)
hist(res_DPP3_2, breaks = 30, main = "Residuals", xlab = "Residuals")
qqnorm(res_DPP3_2)
qqline(res_DPP3_2, col = "red")

anova_results_DPP3_2 <- anova(model_DPP3_2, ddf = "Kenward-Roger")


pvals_df <- anova_results_DPP3_2 %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  select(term, p_value = `Pr(>F)`)
pvals_df <- pvals_df %>%
  mutate(
    p_label = case_when(
      p_value < 0.001 ~ "< 0.001",
      TRUE ~ formatC(p_value, format = "f", digits = 2)
    ),
    label = case_when(
      term == "timepoint" ~ paste0("Time: p ", p_label),
      term == "Outcome_death_bridge" ~ paste0("Outcome: p = ", p_label),
      term == "timepoint:Outcome_death_bridge" ~ paste0("Interaction: p = ", p_label)
    )
  )

figure_DPP3_2 <- ggplot(
  df_long,
  aes(
    x = timepoint,
    y = value_log,
    color = factor(Outcome_death_bridge)
  )
) +
  geom_boxplot() +
  labs(
    x = "Time",
    y = "DPP3 (log scale)",
    color = "Outcome of death or bridge"
  ) +
  theme_minimal() +
  geom_text(
    data = df_count_DDP3_2,
    aes(
      x = timepoint,
      y = min(df_long$value_log, na.rm = TRUE),
      label = paste0("n=", n),
      group = factor(Outcome_death_bridge)
    ),
    position = position_dodge(0.7),
    size = 4,
    vjust = 1
  )  +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste(pvals_df$label, collapse = "\n"),
    hjust = 1.05,
    vjust = 1.1,
    size = 3.5
  )+
  theme(legend.position = "bottom")



Figure_tot <- Figure_DPP3_1/figure_DPP3_2+
  plot_annotation( tag_levels = c("A","B")) 
ggsave("FIGURES/Figure_DPP3_serial.pdf",
       plot   = Figure_tot,
       width  = 160, height = 220, units = "mm",
       device = cairo_pdf, bg = "white",scale=.9)


#####HAPTO####

df_long_hapto <- df%>%
  select(ID,hapto_J0, `hapto_J3-J5`,hapto_JS, Outcome_death_bridge)%>%
  pivot_longer(
    cols = starts_with("hapto_"),
    names_to = c("timepoint"),
    values_to = "value"
  )%>%
filter(!is.na(value))%>%
  mutate(timepoint = str_remove(timepoint, "hapto_"),
         timepoint = case_when(timepoint=="J0" ~ "implantation",
                               timepoint=="J3-J5" ~ "day 3 to 5",
                               timepoint=="JS" ~ "explantation"),
         timepoint = factor(timepoint, levels = c("implantation", "day 3 to 5", "explantation")),
         value= as.numeric(value),
         value_log= log1p(as.numeric(value))
  )


df_long_hapto%>%
  group_by(timepoint)%>%
  summarise(med= median(value, na.rm=TRUE),
            IQR_low= quantile(value,0.25, na.rm=TRUE),
            IQR_high= quantile(value,0.75, na.rm=TRUE))

df_long_hapto%>%
  group_by(timepoint, Outcome_death_bridge)%>%
  filter(value<0.05)%>%
summarise(n=n())

df_count_hapto<- df_long_hapto %>%
  group_by(timepoint) %>%
  summarise(n = sum(!is.na(value)))

model <- lmer(
  value ~ timepoint + (1 | ID),
  data = df_long_hapto
)

res <- resid(model)
hist(res, breaks = 30, main = "Residuals", xlab = "Residuals")
qqnorm(res)
qqline(res, col = "red")

anova_results <- anova(model, ddf = "Kenward-Roger")
p_time <- format.pval(anova_results["timepoint", "Pr(>F)"], digits = 3, eps = .001)
  
figure_hapto1  <-ggplot(df_long_hapto, aes(x = timepoint, y = value)) +
  geom_boxplot() +
  labs(x = "Times",
       y = "Haptoglobin\n(g/l)") +
  theme_minimal()+
  geom_text(data = df_count_hapto,
            aes(
              x = timepoint,
              y =  min(df_long_hapto$value, na.rm = TRUE),
              label = paste0("n=", n)
            ),
            position = position_dodge(0.7),
            size = 4,
            vjust = 1
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(df_long_hapto$value, na.rm = TRUE),
    label = paste0("p = ", p_time),
    size = 4,
    hjust = 0
  )

df_count_hapto2 <- df_long_hapto %>%
  dplyr::group_by(timepoint, Outcome_death_bridge) %>%
  dplyr::summarise(
    n = sum(!is.na(value)),
    .groups = "drop"
  )

model_hapto2 <- lmer(
  value ~ timepoint * Outcome_death_bridge + (1 | ID),
  data = df_long_hapto
)
res_hapto2 <- resid(model_hapto2)
hist(res_hapto2, breaks = 30, main = "Residuals", xlab = "Residuals")
qqnorm(res_hapto2)
qqline(res_hapto2, col = "red")

anova_results_hapto2 <- anova(model_hapto2, ddf = "Kenward-Roger")
pvals_hapto2 <- anova_results_hapto2["timepoint:Outcome_death_bridge", "Pr(>F)"]
p_interact_hapto2 <- format.pval(pvals_hapto2, digits = 3, eps = .001)

figure_hapto2 <- ggplot(
  df_long_hapto,
  aes(
    x = timepoint,
    y = value,
    color = factor(Outcome_death_bridge)
  )
) +
  geom_boxplot() +
  labs(
    x = "Time points",
    y = "Haptoglobin",
    color = "Outcome"
  ) +
  theme_minimal() +
  geom_text(
    data = df_count_hapto2,
    aes(
      x = timepoint,
      y = min(df_long_hapto$value, na.rm = TRUE),
      label = paste0("n=", n),
      group = factor(Outcome_death_bridge)
    ),
    position = position_dodge(0.7),
    size = 4,
    vjust = 1
  ) +
  annotate(
    "text",
    x = 1,
    y = max(df_long_hapto$value, na.rm = TRUE),
    label = paste0("Interaction p = ", p_interact_hapto2),
    size = 4,
    hjust = 0
  )+
  theme(legend.position = "bottom")

Figure_hapto <- figure_hapto1/figure_hapto2+
  plot_annotation( tag_levels = c("A","B"))
ggsave("FIGURES/Figure_hapto_serial.pdf",plot = Figure_hapto,
       width  = 160, height = 200, units = "mm",
       device = cairo_pdf, bg = "white",scale=1)



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

