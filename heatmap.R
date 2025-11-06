library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
names(df_bio)
# 1) Identifiez vos marqueurs pro- et anti-inflammatoires
pro_markers  <- c("IL1b", "IL2", "IL6","IL12p70", "IL-33", "TNFa", "IFNg", "IL17A/CTLA-8", "IL8/CXCL8")
anti_markers <- c("IL4","IL5", "IL10", "GDF-15")  # à adapter selon votre définition
cell_global      <- c("T4")
cell_inflam      <- c("L_TH1","M_TNF","M_IL1","L_TH1","L_IL17_pathogen","L_IL17" )
cell_anti_inflam <- c("L_TH2","M_IL10")
cell_mixte <- ("mixte")


# 2) Pivot long sur les suffixes de jours
df_long <- df_bio %>%
  # ne garder que les colonnes cytokines+jour
  select(ID,Outcome,
         starts_with(pro_markers),
         starts_with(anti_markers),
         starts_with(cell_global),
         starts_with(cell_inflam),
         starts_with(cell_anti_inflam),
         starts_with(cell_mixte),) %>%
  pivot_longer(
    cols = -c(ID, Outcome),
    names_to  = c("BIO", "day"),
    names_pattern = "(.*)_(J0|J3_J5|JS)$",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  # classifier pro vs anti
  mutate(
    category = fct_relevel(factor(case_when(
      BIO %in% pro_markers  ~ "plasma Pro-inf cyt",
      BIO %in% anti_markers ~ "plasma anti-inf cyt",
      BIO %in% cell_global  ~  "cells",
      BIO %in% cell_inflam  ~  "pro inf immmune cell",
      BIO %in% cell_anti_inflam  ~  "anti inf\n immmune cell",
      BIO %in% cell_mixte             ~  "anti/hyper\n inf cell",
      TRUE                        ~ NA_character_
    )), c("plasma Pro-inf cyt",
                 "plasma anti-inf cyt","cells",
                 "pro inf immmune cell",
                 "anti inf\n immmune cell",
                 "anti/hyper\n inf cell")),
    # pour un bel ordre de l'axe y
    BIO = fct_relevel(BIO, sort(unique(BIO)))
  )%>%
  filter(!is.na(BIO))%>%
  filter(! BIO %in% c("IL4+ (CD3+ CD4+)",
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
  mutate(value_log = log2(value)) %>%         # évite log(0)
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
  )

# Visualiser ceux qui ne sont PAS normalement distribués
res <- normality_results %>%
  filter(p_value < 0.05)

####By evolution

df_heat <- df_long %>% 
  group_by(category, BIO, day, Outcome) %>% 
  summarise(value_z = median(value_z, na.rm = TRUE), .groups = "drop")


ggplot(df_heat, aes(x = day, y = BIO, fill = value_z)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours  = c("white","#fde0dc","#f9bdbb","#f36c60","#d32f2f"),
    name     = "Z-score\n(log₂)",
    limits   = c(-3, 3),
    oob      = scales::squish
  ) +
  facet_grid(
    rows = vars(category),
    cols = vars(Outcome),
    scales = "free_y",
    switch = "y"        # catégorie sur la bande de gauche
  ) +
  labs(
    title ="Cytokine heatmap according to outcomes",
    x     = "Time points",
    y     = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid    = element_blank(),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    strip.text.y  = element_text(face = "bold"),
    strip.text.x  = element_text(face = "bold", size = 11),
    legend.key.height = unit(0.7, "cm")
  )
