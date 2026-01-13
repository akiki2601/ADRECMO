####ARRONDIR#####

round_nonnormal_0 <- function(x) {
  x <- as.character(x)
  m <- gregexpr("\\d+\\.\\d+", x, perl = TRUE)
  regmatches(x, m) <- lapply(
    regmatches(x, m),
    function(v) as.character(round(as.numeric(v), 0))
  )
  x
}

#==============================================================================
# FIGURE — DPP3 baseline comparisons by DPP3_median (helper function)
#==============================================================================

fmt_p <- function(p, digits = 3, eps = 0.001) {
  if (is.na(p)) return(NA_character_)
  if (p < eps) return(paste0("< ", eps))
  formatC(p, format = "f", digits = digits)
}



plot_box_by_dpp3_group <- function(df_in, y_var, y_lab, x_lab = NULL) {
 
  df_in<-df_in%>%
    mutate(DPP3_median=factor(DPP3_median,levels=c("cDPP3>73 ng/mL","cDPP3≤73 ng/mL")))
  
  fmt_p <- function(p, digits = 3, eps = 0.001) {
    if (is.na(p)) return(NA_character_)
    if (p < eps) return(paste0("<", eps))
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
    annotate("text",
             x = 1,
             y = y_max * 1.10,
             label = ifelse(
               startsWith(p_lbl, "<"),
               paste0("p ", p_lbl),
               paste0("p=", p_lbl)
             ) 
             , hjust = 0, size = 4) +
    geom_text(
      data = df_counts,
      aes(x = DPP3_median, y = y_min, label = paste0("n=", n)),
      size = 4, vjust = 1
    )+
    theme(axis.text.y = element_text(size = 12, face="bold"),
          axis.text.x = element_text(size = 12, face="bold"),
          axis.title.x = element_text(size = 14, face="bold"),
          axis.title.y = element_text(size = 14, face="bold"))
}



###

plot_spearman <- function(data, x, y, xlab, ylab) {
  
  # Correlation test
  test <- cor.test(
    data[[x]], data[[y]],
    method = "spearman",
    use = "complete.obs"
  )
  
  label <- paste0(
    "ρ=", round(test$estimate, 2),
    "\n p=", fmt_p(test$p.value)
  )
  
  # Remove NA for positioning
  df <- data[!is.na(data[[x]]) & !is.na(data[[y]]), ]
  
  
  # Ranges
  x_rng <- range(df[[x]], na.rm = TRUE)
  y_rng <- range(df[[y]], na.rm = TRUE)
  dx <- diff(x_rng)
  dy <- diff(y_rng)
  
  # Margins (où placer le texte par rapport aux bords)
  mx <- 0.08 * dx
  my <- 0.08 * dy
  
  # Taille de la zone qu'on veut garder "vide" autour du texte
  # (à ajuster si besoin)
  box_w <- 0.25 * dx
  box_h <- 0.25 * dy
  
  candidates <- data.frame(
    pos = c("TL","TR","BL","BR"),
    ax  = c(x_rng[1] + mx, x_rng[2] - mx, x_rng[1] + mx, x_rng[2] - mx),
    ay  = c(y_rng[2] - my, y_rng[2] - my, y_rng[1] + my, y_rng[1] + my),
    hjust = c(0, 1, 0, 1),
    vjust = c(1, 1, 0, 0)
  )
  
  # Score = nombre de points proches de la zone de texte (plus petit = mieux)
  count_in_box <- function(ax, ay) {
    sum(
      abs(df[[x]] - ax) <= box_w/2 &
        abs(df[[y]] - ay) <= box_h/2
    )
  }
  
  candidates$score <- mapply(count_in_box, candidates$ax, candidates$ay)
  
  best <- candidates[which.min(candidates$score), ]
  
  annot_x <- best$ax
  annot_y <- best$ay
  hjust   <- best$hjust
  vjust   <- best$vjust
  
  # Plot
  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.7) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      color = "blue"
    ) +
    annotate(
      "text",
      x = annot_x,
      y = annot_y,
      label = label,
      hjust = hjust,
      vjust = vjust,
      size = 3.5
    ) +
    labs(
      x = xlab,
      y = ylab,
      title = ""
    ) +
    theme_minimal()+
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}
  