plot_cytokine <- function(df, cytokine_name) {
  
  # Colomn name
  col_J0 <- paste0(cytokine_name, "_J0")
  col_J3_J5 <- paste0(cytokine_name, "_J3_J5")
  col_JS <- paste0(cytokine_name, "_JS")
  
  # 1. dataset
  data_plot <- df %>%
    select(ID, Outcome, all_of(c(col_J0, col_J3_J5, col_JS))) %>%
    mutate(
      Outcomes = factor(case_when(
        Outcome %in% c("Bridge to LVAD", "Bridge to Transplant") ~ "Bridge to transplant or LVAD",
        TRUE ~ as.character(Outcome)
      ), levels = c("Death", "Bridge to transplant or LVAD", "ECMO Weaning"))
    ) %>%
    pivot_longer(
      cols = c(all_of(c(col_J0, col_J3_J5, col_JS))),
      names_to = "cyto_time",
      values_to = "value"
    ) %>%
    mutate(
      time = case_when(
        cyto_time == col_J0 ~ 1,
        cyto_time == col_J3_J5 ~ 2,
        cyto_time == col_JS ~ 3,
        TRUE ~ NA_real_
      ),
      value_log = log1p(value)
    )
  
  # 2. n
  count_data <- data_plot %>%
    filter(!is.na(value)) %>%
    group_by(cyto_time, Outcomes) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(!is.na(Outcomes))
  
  # 3. Models
  mod_no_interaction <- lmerTest::lmer(
    value_log ~ time + Outcomes + (1  | ID),
    data = data_plot
  )
  
  p_anova <- anova(mod_no_interaction, ddf = "Kenward-Roger")
  p_time <- format.pval(p_anova["time", "Pr(>F)"], digits = 3, eps = .001)
  p_outcome <- format.pval(p_anova["Outcomes", "Pr(>F)"], digits = 3, eps = .001)
  
  # 4. Plot
  p <- ggplot(data_plot, aes(x = cyto_time, y = value_log, fill = Outcomes)) +
    geom_boxplot(position = position_dodge(0.7), width = 0.6) +
    labs(
      title = paste("Association between", cytokine_name, "and outcomes"),
      x = "Times of measurement",
      y = paste("Log of", cytokine_name)
    ) +
    geom_text(data = count_data, aes(
      x = cyto_time,
      y = -min(data_plot$value_log, na.rm = TRUE) * 0.01,
      label = paste0("n=", n),
      group = Outcomes
    ), position = position_dodge(0.7), size = 4, vjust = 1) +
    scale_x_discrete(labels = setNames(
      c("implantation", "day 3 to 5", "explantation"),
      c(col_J0, col_J3_J5, col_JS)
    )) +
    scale_fill_manual(values = c(
      "ECMO Weaning" = "#4DAF4A",
      "Bridge to transplant or LVAD" = "#377EB8",
      "Death" = "#F8766D"
    )) +
    annotate(
      "text",
      x = 1,
      y = max(data_plot$value_log, na.rm = TRUE) * 1.1,
      label = paste0("p[time]: ", p_time, " p[outcomes]: ", p_outcome),
      parse = FALSE,
      size = 4,
      hjust = 0
    )
  
  print(p)
}