plot_density_log10 <- function(df, vars, breaks = 30) {
  for (var in vars) {
    # extraction + transformation
    x <-log10(df[[var]])
    
    # titre et label dynamiques
    main_title <- paste0("Densité de log10(", var, ")")
    x_label    <- paste0("log10(", var, ")")
    
    # histogramme en densité
    hist(
      x,
      freq   = FALSE,
      breaks = breaks,
      main   = main_title,
      xlab   = x_label
    )
    
    # courbe de densité lissée
    lines(
      density(x, na.rm = TRUE),
      lwd = 2
    )
  }
}
