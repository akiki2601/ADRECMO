# ===========================
# Helpers génériques
# ===========================
.map_outcomes <- function(x) {
  # robuste : accepte numérique (1..4) ou libellés texte
  if (is.numeric(x)) {
    dplyr::case_when(
      x == 1 ~ "ECMO Weaning",
      x %in% c(2,3) ~ "Bridge to transplant or LVAD",
      x == 4 ~ "Death",
      TRUE ~ as.character(x)
    )
  } else {
    x <- as.character(x)
    dplyr::case_when(
      x %in% c("Bridge to LVAD","Bridge to Transplant") ~ "Bridge to transplant or LVAD",
      TRUE ~ x
    )
  }
}

.fmt_p <- function(p, eps = 1e-4, digits = 4) {
  if (is.na(p)) return(NA_character_)
  if (p < eps) sprintf("<%.4f", eps) else formatC(p, format = "f", digits = digits)
}

# ===========================
# 1) Préparation des données
# ===========================
prep_marker_data <- function(df,
                             prefix,                 # ex: "ADR1L", "ADR2L", "M_ADR1", "M_ADR2"
                             id_col = "ID",
                             outcome_col = "Outcome",
                             times = c("_J0","_J3_J5","_JS"),   # ordre = 1,2,3
                             log1p_y = TRUE) {
  
  stopifnot(all(paste0(prefix, times) %in% names(df)),
            id_col %in% names(df),
            outcome_col %in% names(df))
  
  time_cols <- paste0(prefix, times)             # ex: c("ADR1L_J0","ADR1L_J3_J5","ADR1L_JS")
  time_names <- c("implantation","day 3 to 5","explantation")
  names(time_names) <- time_cols
  
  out <- df %>%
    select(all_of(c(id_col, outcome_col, time_cols))) %>%
    mutate(
      Outcomes = factor(
        .map_outcomes(.data[[outcome_col]]),
        levels = c("Death","Bridge to transplant or LVAD","ECMO Weaning")
      )
    ) %>%
    pivot_longer(
      cols = all_of(time_cols),
      names_to = "marker_time",
      values_to = "value"
    ) %>%
    mutate(
      time = dplyr::case_when(
        marker_time == time_cols[1] ~ 1,
        marker_time == time_cols[2] ~ 2,
        marker_time == time_cols[3] ~ 3,
        TRUE ~ NA_real_
      ),
      value_log = if (log1p_y) log1p(value) else value
    )
  
  count_data <- out %>%
    filter(!is.na(value)) %>%
    group_by(marker_time, Outcomes) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(!is.na(Outcomes))
  
  list(
    data = out,
    counts = count_data,
    x_labels = time_names
  )
}

# ===========================
# 2) Tests statistiques (LMM)
# ===========================
test_marker <- function(dat, id_col = "ID") {
  stopifnot(all(c("value_log","time","Outcomes") %in% names(dat)),
            id_col %in% names(dat))
  
  # Modèle réduit vs complet (interaction)
  mod_full <- lmerTest::lmer(value_log ~ time * Outcomes + (1 | ID), data = dat)
  mod_red  <- lmerTest::lmer(value_log ~ time + Outcomes + (1 | ID), data = dat)
  
  inter_anova <- anova(mod_red, mod_full)  # test d'interaction
  
  # ANOVA Kenward-Roger sur le modèle réduit
  a <- anova(mod_red, ddf = "Kenward-Roger")
  p_time     <- a[rownames(a) == "time",      "Pr(>F)"]
  p_outcomes <- a[rownames(a) == "Outcomes",  "Pr(>F)"]
  
  label_text <- paste0(
    "time p = ", .fmt_p(p_time),
    "\n",
    "outcomes p = ", .fmt_p(p_outcomes)
  )
  
  list(
    mod_full   = mod_full,
    mod_red    = mod_red,
    inter_test = inter_anova,
    anova_red  = a,
    p_time     = p_time,
    p_outcomes = p_outcomes,
    label      = label_text
  )
}

# ===========================
# 3) Figure (boxplot + effectifs + p)
# ===========================
plot_marker <- function(dat,
                        counts,
                        x_labels,
                        panel_title = "A",
                        y_var = c("value_log","value"),
                        y_lab = "Value (log1p scale)",
                        show_legend = FALSE) {
  
  y_var <- match.arg(y_var)
  y_vec <- dat[[y_var]]
  
  y_limits <- range(dat[[y_var]], na.rm = TRUE)
  expand <- 0.1 * diff(y_limits)
  y_limits <- y_limits + c(-expand, expand)
  
  p <- ggplot(dat, aes(x = marker_time, y = .data[[y_var]], fill = Outcomes)) +
    geom_boxplot(position = position_dodge(0.7), width = 0.6) +
    labs(
      title = panel_title,
      x = "Time",
      y = y_lab
    ) +
    geom_text(
      data = counts,
      aes(
        x = marker_time,
        y = if (y_var == "value_log") min(y_vec, na.rm = TRUE) else min(y_vec, na.rm = TRUE),
        label = paste0("n=", n),
        group = Outcomes
      ),
      position = position_dodge(0.7),
      size = 4,
      vjust = 1
    ) +
    coord_cartesian(ylim = y_limits) +
    scale_x_discrete(labels = x_labels) +
    scale_fill_manual(values = c(
      "ECMO Weaning" = "#4DAF4A",
      "Bridge to transplant or LVAD" = "#377EB8",
      "Death" = "#F8766D"
    )) +
    theme(legend.position = if (show_legend) "right" else "none")
  
  p
}

