library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
names(df_bio)
# 1) Identifiez vos marqueurs pro- et anti-inflammatoires
# --- vecteurs de variables ---
pro_markers  <- c("IL1b", "IL2", "IL6","IL12p70", "IL-33", "TNFa", "IFNg", "IL17A/CTLA-8", "IL8/CXCL8")
cell_anti_inflam <- c("L_TH2","M_IL10")
anti_markers <- c("IL4","IL5", "IL10", "GDF-15")
cell_inflam      <- c("L_TH1","M_TNF","M_IL1","L_IL17_pathogen","L_IL17")  # L_TH1 en simple exemplaire



df_long <- df_bio %>%
  # ne garder que les colonnes d’intérêt
  dplyr::select(
    ID, Outcome,
    dplyr::starts_with(pro_markers),
    dplyr::starts_with(cell_inflam),
    dplyr::starts_with(anti_markers),
    dplyr::starts_with(cell_anti_inflam)
  ) %>%
  tidyr::pivot_longer(
    cols = -c(ID, Outcome),
    names_to  = c("BIO", "day"),
    names_pattern = "(.*)_(J0|J3_J5|JS)$",
    values_to = "value"
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  # classifier pro vs anti vs autres
  dplyr::mutate(
    category = forcats::fct_relevel(
      factor(dplyr::case_when(
        BIO %in% pro_markers        ~ "plasma Pro-inf cyt",
        BIO %in% cell_inflam        ~ "pro inf immmune cell",
        BIO %in% anti_markers       ~ "plasma anti-inf cyt",
        BIO %in% cell_anti_inflam   ~ "anti inf\n immmune cell",
        TRUE                        ~ NA_character_
      )),
      c("plasma Pro-inf cyt",
        "pro inf immmune cell",
        "plasma anti-inf cyt",
        "anti inf\n immmune cell")
    ),
    # ordre stable pour l'axe Y
    BIO = forcats::fct_relevel(BIO, sort(unique(BIO)))
  ) %>%
  # supprimer les lignes non classées et 2 marqueurs indésirables
  dplyr::filter(!is.na(category)) %>%
  dplyr::filter(!BIO %in% c("IL4+ (CD3+ CD4+)",
                            "IL4+ INFg+ (CD3+ CD4+)"))


normality_results <- df_long %>%
  group_by(BIO, day) %>%
  summarise(
    n = n(),
    n_unique = n_distinct(value),
    p_value = if (n >= 3 && n <= 5000 && n_unique > 1) {
      shapiro.test(value)$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# Visualiser ceux qui ne sont PAS normalement distribués
normality_results %>%
  filter(p_value < 0.05)



# 3) Optionnel : normaliser ou log-transformer
df_long <- df_long %>% 
  mutate(value_log = log1p(value)) %>%         # évite log(0)
  group_by(category) %>% 
  mutate(value_z = (value_log - mean(value_log, na.rm = TRUE)) /
           sd(value_log,  na.rm = TRUE)) %>% 
  ungroup()


normality_results <- df_long %>%
  group_by(BIO, day) %>%
  summarise(
    n = n(),
    n_unique = n_distinct(value_log),
    p_value = if (n >= 3 && n <= 5000 && n_unique > 1) {
      shapiro.test(value)$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(n >= 3, n_unique > 2)   # groupes testables

# 2. Filtrer df_long en ne gardant que ces combinaisons
df_long_non_unique <- df_long %>%
  semi_join(normality_results, by = c("BIO", "day"))

# Visualiser ceux qui ne sont PAS normalement distribués
res <- normality_results %>%
  filter(p_value < 0.05)

df_heat <- df_long_non_unique %>% 
  group_by(category, BIO, day, Outcome) %>% 
  summarise(value_z = median(value_z, na.rm = TRUE), .groups = "drop")%>%
  mutate(BIO = gsub("_", " ", BIO))

#### FIGURE#####
library(ggnewscale)

pro_cats    <- c("plasma Pro-inf cyt", "pro inf immmune cell")
anti_cats   <- c("plasma anti-inf cyt", "anti inf\n immmune cell")
neutral_cats<- c("cells")

p <- ggplot() +
  # --- PRO (rouge) ---
  geom_tile(
    data = df_heat %>% dplyr::filter(category %in% pro_cats),
    aes(x = day, y = BIO, fill = value_z), color = "white"
  ) +
  scale_fill_gradientn(
    colours = c("white","#fde0dc","#f9bdbb","#f36c60","#d32f2f"),
    name    = "Z-score\n(log₁₊x)",
    limits  = c(-3, 3),
    oob     = scales::squish,
    guide   = guide_colorbar(order = 1)
  ) +
  ggnewscale::new_scale("fill") +
  
  # --- ANTI (bleu) ---
  geom_tile(
    data = df_heat %>% dplyr::filter(category %in% anti_cats),
    aes(x = day, y = BIO, fill = value_z), color = "white"
  ) +
  scale_fill_gradientn(
    colours = c("white","#e1f5fe","#81d4fa","#0288d1","#01579b"),
    name    = "Z-score\n(log₁₊x)",
    limits  = c(-3, 3),
    oob     = scales::squish,
    guide   = guide_colorbar(order = 2)
  ) +
  ggnewscale::new_scale("fill")  +
  
  facet_grid(
    rows = vars(category),
    cols = vars(Outcome),
    scales = "free_y",
    switch = "y"
  ) +
  labs(
    title = "Inflammatory heatmap according to outcomes",
    x = "Time points", y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    strip.text.y    = element_text(face = "bold"),
    strip.text.x    = element_text(face = "bold", size = 11),
    legend.key.height = unit(0.7, "cm")
  )

p
