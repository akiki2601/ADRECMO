library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(gtsummary)
library(ggplot2)

DATA <- read_delim("C://Users//jolie//Desktop//Research//ADRECMO//ADRECMO_DATA.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(DATA)

## Data preparation, new variables for Table_1

DATA <- DATA %>% mutate(Temp_D0 = round(Temp_D0, 1))
DATA$BNP_D0 <- as.numeric(DATA$BNP_D0) # In the table is numeric, although the add on of this code, it remains categorial in the Table!!!

DATA <- DATA %>%
  mutate(
    Gender = case_when(
      Gender == 0 ~ "men",
      Gender == 1 ~ "woman",
      TRUE ~ as.character(Gender)  
    ),
    Outcome = case_when(
      Outcome == 1 ~ "ECMO Weaning",
      Outcome == 2 ~ "Bridge to Transplant",
      Outcome == 3 ~ "Bridge to LVAD",
      Outcome == 4 ~ "Death",
      TRUE ~ as.character(Outcome)  
    )
  )


DATA <- DATA %>%
  mutate(
    Pulsepressure_D0 = PAS_D0 - PAD_D0
  )

# Age at icu admission
DATA <- DATA %>%
  mutate(
    date_de_naissance = dmy(date_de_naissance),
    icu_admiss_date = dmy(icu_admiss_date),
    Age = as.numeric(icu_admiss_date - date_de_naissance)
  ) ## Can't find the error

# Duration hospitalisation
DATA <- DATA %>%
  mutate(
    icu_admiss_date = dmy(icu_admiss_date),
    icu_discharge_date = dmy(icu_discharge_date),
    Time_hosp = as.numeric(icu_discharge_date - icu_admiss_date)
  ) ## Can't find the error

# Duration ECMO support
DATA <- DATA %>%
  mutate(
    ecmo_start_date = dmy(ecmo_start_date),
    ecmo_stop_date = dmy(ecmo_stop_date),
    Time_ecmo = as.numeric(ecmo_stop_date - ecmo_start_date)
  )

# Distribuition DPP3
library(ggplot2)

plot <- ggplot(DATA, aes(x = DPP3_D0)) +
  geom_density(alpha = 0.5, fill = "blue") +
  labs(title = paste("Density plot DPP3"),
       x = "DPP3", y = "Density") +
  theme_minimal()

print(plot)

# Median DPP3
median_DPP3 <- median(DATA$DPP3_D0, na.rm = TRUE)
print(median_DPP3)
# 72.9

# Quantil DPP3 
Q_DPP3 <- quantile(DATA$DPP3_D0, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
print(Q_DPP3)

# 25%   50%   75% 
# 50.0  72.9 164.1 
## In the literature normal DPP3 40 ng/ml. Could we separate our population in two group low vs high DPP3 (i.e. Q1 (<50) vs Q2-4 (>50)) as meaningful (near normal) cut point? Better Q2 for numerosity of the group? In OptimaCC 59.1 (Q3) was cutoff. In CARD Schock 33.4 (Q2).

DATA <- DATA %>%
  mutate(DPP3_quartile = case_when(
    DPP3_D0 <= Q_DPP3[1] ~ "Q1",                       
    DPP3_D0 > Q_DPP3[1] & DPP3_D0 <= Q_DPP3[2] ~ "Q2", 
    DPP3_D0 > Q_DPP3[2] & DPP3_D0 <= Q_DPP3[3] ~ "Q3", 
    TRUE ~ "Q4"                                         
  ))

DATA <- DATA %>%
  mutate(DPP3_group = case_when(
    DPP3_D0 <= Q_DPP3[1] ~ "Q1",
    TRUE ~ "Q2+Q3+Q4"  # Tutto il resto (Q2, Q3, Q4)
  ))


Table_1 <- DATA |> 
  select(Age, Gender, HTN, DM, Immunodepression, CKD, Neurologic_deficit, Chronic_respiratory_disease, 
         HF, Cardiac_arrest_before_canul, Intubation, EER_D0, PAS_D0, PAD_D0, PAM_D0, Pulsepressure_D0, 
         HR_D0, Temp_D0, IABP_D0, Preecmo_tte_ef, Preecmo_tte_vtiao, 
         NADcum_D0, DOBUcum_D0, pH_D0, Sao2_D0, Paco2_D0, Lact_D0, 
         Hco3_D0, Creat_D0, ALAT_D0, ASAT_D0, Bili_D0, Tropo_i_hs_D0, 
         BNP_D0, Ntprobnp_D0, DPP3_group, Time_hosp, Time_ecmo, Outcome) |> 
  tbl_summary(by = DPP3_group, missing = "no") |> 
  add_p() |> 
  add_overall() |> 
  add_n()

Table_1
 ## to correct: age, ecmo time, hosp time, BNP
