test_linearity <- function(data, xvar, yvar, plot = TRUE) {
  library(ggplot2)
  library(rms)
  library(mgcv)
  library(rlang)
  
  # 1) Extraction des variables
  x <- data[[xvar]]
  y <- data[[yvar]]
  
  # 2) LM linéaire et quadratique
  m_lin  <- lm(y ~ x)
  m_quad <- lm(y ~ x + I(x^2))
  an_quad <- anova(m_lin, m_quad)
  p_quad <- an_quad$`Pr(>F)`[2]                        # test non-linéarité
  p_lin  <- summary(m_lin)$coefficients[2, "Pr(>|t|)"] # p de la pente linéaire
  
  # 3) RCS
  form_rcs <- as.formula(paste0(yvar, " ~ rcs(", xvar, ", 4)"))
  fit_rcs  <- ols(form_rcs, data = data)
  an_rcs   <- anova(fit_rcs)
  tab_rcs  <- as.data.frame(an_rcs)
  
  rn <- trimws(rownames(tab_rcs))
  if ("Nonlinear" %in% rn) {
    idx_row <- which(rn == "Nonlinear")
    p_rcs_nonlin <- tab_rcs[idx_row, "P"]
  } else {
    p_rcs_nonlin <- NA  # aucun terme non linéaire
  }
  
  # 4) GAM
  form_gam <- as.formula(paste0(yvar, " ~ s(", xvar, ")"))
  fit_gam  <- gam(form_gam, data = data)
  sum_gam  <- summary(fit_gam)
  gam_edf  <- sum_gam$s.table[1, "edf"]
  gam_p    <- sum_gam$s.table[1, "p-value"]
  
  # 5) Plot LM + LOESS
  if (plot) {
    p <- ggplot(data, aes(x = !!sym(xvar), y = !!sym(yvar))) +
      geom_point() +
      geom_smooth(method = "lm", colour = "blue", se = FALSE) +
      geom_smooth(method = "loess", colour = "red", se = FALSE) +
      labs(title = paste("Linearity check for", yvar, "~", xvar),
           x = xvar, y = yvar) +
      theme_classic()
    print(p)
  }
  
  # 6) Résumé
  # 6) Résumé + interprétation automatique
  any_nonlin <- (p_quad < 0.05) |
    (!is.na(p_rcs_nonlin) && p_rcs_nonlin < 0.05) |
    (gam_edf > 1.1 && gam_p < 0.05)
  
  if (p_lin < 0.05 && !any_nonlin) {
    interpretation <- "Association linéaire statistiquement significative, sans évidence de non-linéarité : une spécification linéaire semble appropriée."
  } else if (p_lin >= 0.05 && !any_nonlin) {
    interpretation <- "Pas d'association linéaire significative et pas d'évidence de non-linéarité : la relation paraît globalement faible ou incertaine."
  } else if (any_nonlin) {
    interpretation <- "Évidence de non-linéarité (quadratique, spline ou GAM) : une modélisation non linéaire (spline, GAM) est préférable."
  } else {
    interpretation <- "Résultats difficiles à interpréter (cas non prévu), vérifier manuellement les sorties de modèle."
  }
  
  list(
    Linear_slope_p   = p_lin,         # test d'association linéaire
    Quadratic_p      = p_quad,        # test de non-linéarité (x²)
    RCS_nonLinear_p  = p_rcs_nonlin,  # test de non-linéarité spline
    GAM_edf          = gam_edf,       # edf ~ 1 si linéaire
    GAM_p            = gam_p,
    RCS_table        = an_rcs,
    GAM_summary      = sum_gam,
    Interpretation   = interpretation
  )
}

#####################

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
                             prefix,                 # "ADR1L", "ADR2L", "M_ADR1", "M_ADR2"
                             id_col = "ID",
                             outcome_col = "Outcome",
                             times = c("_J0","_J3_J5","_JS"),
                             transform = c("log1p","log10","none","log2"),
                             eps = 1e-6) {
  
  transform <- match.arg(transform)
  
  stopifnot(all(paste0(prefix, times) %in% names(df)),
            id_col %in% names(df),
            outcome_col %in% names(df))
  
  time_cols  <- paste0(prefix, times)                       # ex. ADR1L_J0 ...
  time_names <- c("implantation","day 3 to 5","explantation")
  names(time_names) <- time_cols
  
  out <- df %>%
    dplyr::select(dplyr::all_of(c(id_col, outcome_col, time_cols))) %>%
    dplyr::mutate(
      Outcomes = factor(
        .map_outcomes(.data[[outcome_col]]),
        levels = c("Death","Bridge to transplant or LVAD","ECMO Weaning")
      )
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(time_cols),
      names_to  = "marker_time",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      time = dplyr::case_when(
        marker_time == time_cols[1] ~ 1,
        marker_time == time_cols[2] ~ 2,
        marker_time == time_cols[3] ~ 3,
        TRUE ~ NA_real_
      ),
      # --- transformation choisie ---
      value_log = dplyr::case_when(
        transform == "log1p" ~ log1p(pmax(as.numeric(value), 0)),
        transform == "log10" ~ log10(pmax(as.numeric(value), 0) + eps),
        transform == "log2"  ~ log2 (pmax(as.numeric(value), 0) + eps),
        TRUE                  ~ as.numeric(value)  # none
      )
    )
  
  # Compter après la même logique que celle tracée
  count_data <- out %>%
    dplyr::filter(is.finite(value_log), !is.na(Outcomes)) %>%
    dplyr::group_by(marker_time, Outcomes) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  list(
    data     = out,
    counts   = count_data,
    x_labels = time_names,
    transform = transform,
    y_label  = switch(transform,
                      "log1p" = "Log1p-transformed value",
                      "log10" = "Log10-transformed value",
                      "log2"  = "Log2-transformed value",
                      "none"  = "Value")
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


######################
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
      title = "",
      x = "Times of measurement",
      y = paste(cytokine_name, "log scale")
    ) +
    geom_text(data = count_data, aes(
      x = cyto_time,
      y = -min(data_plot$value_log, na.rm = TRUE) * 0.01,
      label = paste0("n=", n),
      group = Outcomes
    ), position = position_dodge(0.7), size = 4, vjust = 1) +
    scale_x_discrete(labels = setNames(
      c("implantation", "D3-D5", "explantation"),
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
      label = paste0("p[time]: ", p_time, "\n", "p[outcomes]: ", p_outcome),
      parse = FALSE,
      size = 4,
      hjust = 0
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
    
  p
}

#######################
# Fonction utilitaire pour créer les terciles de SOFA + plot IL-6
make_il6_sofa_panel <- function(data,
                                il6_var,
                                sofa_var,
                                title_label)
  {
  
  # On enlève les NA pour ce couple de variables
  dat <- data %>%
    select(all_of(c(il6_var, sofa_var))) %>%
    filter(!is.na(.data[[il6_var]]), !is.na(.data[[sofa_var]]))
  
  if (nrow(dat) == 0) return(NULL)
  
  # Terciles de SOFA
  qs <- quantile(dat[[sofa_var]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  # sécurisation des bornes entières
  qs[1] <- floor(qs[1])
  qs[4] <- ceiling(qs[4])
  
  dat <- dat %>%
    mutate(
      sofa_tercile = cut(
        .data[[sofa_var]],
        breaks = unique(qs),
        include.lowest = TRUE,
        right = TRUE
      )
    )
  
  # Labels propres pour les terciles
  sofa_labels <- levels(dat$sofa_tercile)
  dat$sofa_tercile <- factor(
    dat$sofa_tercile,
    levels = sofa_labels,
    labels = paste0("T", seq_along(sofa_labels), " ", sofa_labels)
  )
  
  # Test non paramétrique : Kruskal–Wallis (IL-6 ~ terciles de SOFA)
  kw <- kruskal.test(dat[[il6_var]] ~ dat$sofa_tercile)
  p_kw <- signif(kw$p.value, 2)
  
  ggplot(dat, aes(x = sofa_tercile, y = .data[[il6_var]], fill = sofa_tercile)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.15, alpha = 0.7, size = 1.8) +
    scale_fill_brewer(palette = "Blues", name = "SOFA terciles") +
    labs(
      title = title_label,
      x = "SOFA (terciles)",
      y = "log(IL-6 + 1)"
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      hjust = 1.1, vjust = 1.5, size = 3.5,
      label = paste0( "P = ", p_kw)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}


### --- Fonction CKD-EPI Créatinine 2021 --- ###
ckd_epi <- function(creat_mgdl, age, gender) {
  # si une info manque → NA
  if (is.na(creat_mgdl) | is.na(age) | is.na(gender)) return(NA_real_)
  
  # paramètres selon le sexe et la créatinine
  if (gender == "Women") {
    A <- 0.7
    B <- if (creat_mgdl <= 0.7) -0.241 else -1.2
    sex_factor <- 1.012
  } else if (gender == "Men") {
    A <- 0.9
    B <- if (creat_mgdl <= 0.9) -0.302 else -1.2
    sex_factor <- 1
  } else {
    return(NA_real_)  # sexe non reconnu
  }
  
  eGFR <- 142 *
    (creat_mgdl / A)^B *
    (0.9938 ^ age) *
    sex_factor
  
  return(eGFR)
}


####################

make_il6_cor_plot <- function(data, xvar, yvar, ylab, title_lab,  ylim = NULL) {
  # test Spearman
  ct <- cor.test(data[[xvar]], data[[yvar]],
                 method = "spearman", use = "complete.obs")
  rho  <- round(unname(ct$estimate), 2)
  pval <- signif(ct$p.value, 2)
  
  # Définir la limite Y si non fournie
  if (is.null(ylim)) {
    ylim <- c(0, max(data[[yvar]], na.rm = TRUE))
  }
  
  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, span = 0.9,
                color = "darkred") +
    labs(
      title = title_lab,
      x = "log(IL-6 + 1)",
      y = ylab,
    ) +
    coord_cartesian(ylim = ylim) +   # <<< coupe en dessous de 0
    theme_classic(base_size = 14) +
    annotate("text", x = Inf, y = ylim[2],
             label = paste0("\u03C1 = ", rho, "\nP = ", pval),
             hjust = 1.1, vjust = 1.5, size = 4.5)
}


make_il_cor_plot <- function(data, 
                             yvar_percent, xvar_IL, 
                             ylab = "%", 
                             xlab = "IL (log +1)", 
                             title_lab = "", 
                             ylim = c(0, 100),
                             xlim = NULL) {
  
  # test Spearman
  ct <- cor.test(data[[yvar_percent]], data[[xvar_IL]],
                 method = "spearman", use = "complete.obs")
  rho  <- round(unname(ct$estimate), 2)
  pval <- signif(ct$p.value, 2)
  
  # Définir les limites Y si non fournies
  if (is.null(ylim)) {
    ylim <- c(0, 100)
  }
  
  # Définir les limites X si non fournies
  if (is.null(xlim)) {
    x_max <- max(data[[xvar_IL]], na.rm = TRUE)
    xlim  <- c(0, x_max)
  } else {
    x_max <- xlim[2]
  }
  
  ggplot(data, aes(x = .data[[xvar_IL]], y = .data[[yvar_percent]])) +
    geom_point(alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, span = 0.9,
                color = "darkred") +
    labs(
      title = title_lab,
      x = xlab,
      y = ylab
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_classic(base_size = 14) +
    annotate(
      "text",
      x = x_max,
      y = ylim[2],
      label = paste0("\u03C1 = ", rho, "\nP = ", pval),
      hjust = 1.1, vjust = 1.5, size = 4.5
    )
}
