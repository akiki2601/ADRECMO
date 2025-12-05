

time_cols = c("ADR1L_J0", "ADR1L_J3_J5","ADR1L_JS")

run_th1th2_analysis <- function(df, time_cols) {
  # mapping et niveaux fixes
  outcome_map    <- c(
    "Bridge to LVAD"       = "Bridge to transplant or LVAD",
    "Bridge to Transplant"  = "Bridge to transplant or LVAD"
  )
  outcome_levels <- c("Death", "Bridge to transplant or LVAD", "ECMO Weaning")
  
  # 1) Passage en long et log + filtrage
  F1 <- df %>%
    select(ID, Outcome, all_of(time_cols)) %>%
    mutate(
      Outcomes = factor(
        dplyr::recode(Outcome, !!!outcome_map, .default = Outcome),
        levels = outcome_levels
      )
    ) %>%
    pivot_longer(
      cols      = all_of(time_cols),
      names_to  = "time_point",
      values_to = "value"
    ) %>%
    mutate(
      time      = match(time_point, time_cols),
      value_log = log10(value)
    ) %>%
    filter(!is.na(value_log), is.finite(value_log))
  
  # 2) Comptages pour annotations
  count_data <- F1 %>%
    group_by(time_point, Outcomes) %>%
    summarise(n = n(), .groups = "drop")
  
  # 3) Construire les formules
  form_full <- as.formula(
    paste0("value_log ~ time * Outcomes + (1 | ID)")
  )
  form_red <- as.formula(
    paste0("value_log ~ time + Outcomes + (1  | ID)")
  )
  
  # 4) Ajuster les modèles
  mod1 <- lmerTest::lmer(form_full, data = F1)
  mod0 <- lmer(form_red,  data = F1)
  
  # 5) Tests
  interact_test <- anova(mod0, mod1)
  kr           <- anova(mod0, ddf = "Kenward-Roger")
  pvals        <- kr$`Pr(>F)`[1:2]
  label_text   <- paste0(
    "time p=",  format.pval(pvals[1], digits=3), "\n",
    "outcomes p=", format.pval(pvals[2], digits=3)
  )
  
  # 6) Figure
  fig <- ggplot(F1, aes(x = time_point, y = value_log, fill = Outcomes)) +
    geom_boxplot(position = position_dodge(0.7), width = 0.6) +
    scale_y_continuous(name = "log(XXXX)") +
    scale_x_discrete(labels = setNames(time_cols, time_cols)) +
    labs(
      title = "A",
      x     = "Time point",
      y     = "xx"
    ) +
    geom_text(
      data     = count_data,
      aes(label = paste0("n=", n), y = min(F1$value_log) * 0.9),
      position = position_dodge(0.7), vjust = 1, size = 3
    ) +
    annotate(
      "text", x = 1, y = max(F1$value_log) * 0.9,
      label = label_text, hjust = 0
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 7) Retour
  list(
    data_long         = F1,
    count_data        = count_data,
    model_full        = mod1,
    model_no_interact = mod0,
    interact_test     = interact_test,
    kr_test           = kr,
    figure            = fig
  )
}
