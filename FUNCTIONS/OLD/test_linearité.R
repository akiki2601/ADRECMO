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