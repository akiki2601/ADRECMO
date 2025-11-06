
library(tidyverse)
library(survival)
library(haven)
library(Hmisc)
library(sjlabelled)

#######################################################################################################;
# 1. Effect of a continuous variable in logistic model ####
#######################################################################################################;

data <- pbc %>% tibble() %>% 
  mutate(death = if_else(status == 2, 1, 0)) %>% 
  mutate(wt = 1) %>% 
  select(id, death, copper, age, sex, wt)

model <- glm(death ~ copper, data = data, family = "binomial", weights = wt)
summary(model)

# Test de linearite
linearity_test(t2event = NULL, event = "death", model = "logistic", var = "copper",
               adj = c("age", "sex"), data = data, weight = "wt") # on rejette la linearite, RCS e 3 noeuds est suffisant

model <- glm(death ~ I(Hmisc::rcspline.eval(copper, nk = 3, inclx = T)) + age + sex, data = data, family = "binomial", weights = wt)
summary(model)

results <- rcs_effect(t2event = NULL, event = "death", model = "logistic",
                      var = "copper", ref_value = 100, min = NULL, max = 200, length = 100, 
                      use_rcs = T, nk = 3, adj = c("age", "sex"), data = data, weight = "wt")
names(results)
results$effect

par(mar = c(5.1, 4.1, 4.1, 2.1))

col1 <- rgb(red = 0, green = 0, blue = 1, alpha = 1)
col2 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.05)
plot_rcs_effect(results, label_var = "Urine copper",
                main = "Title", xlab="Urine copper (ug/day)", ylab=NULL, ylog=F,
                lwd=2, cex_lab=1.2, cex_axis=1.2, polygon_ci=F,
                col1 = col1, col2 = col2, xaxis=NULL, yaxis=NULL, 
                hline=T, col_hline = "black", lwd_hline=1,  
                add_pglobal=T, name_pglobal=NULL, xpglobal = NULL, ypglobal=NULL, adj_pglobal=0, cex_pglobal=1,
                add_plin=T, name_plin=NULL, xplin = NULL, yplin=NULL, adj_plin=0, cex_plin=1)


#######################################################################################################;
# 2. Interaction between continuous variable and factor variable in logistic model ####
#######################################################################################################;

# Remarque importante : 
# La fonction "interaction_cont_factor" n'est pas optimisee. 
# Pour qu'elle fonctionne, il faut absolument qu'aucune variable utilisee en argument de la fonction 
# (arguments t2event, event, factor, cont, adj, weight) ne se nomme "x1" (dans le cas ou la variable continue est consideree en lineaire) ou
# x1, x2, ..., xj (avec j = nb de noeuds - 1 dans le cas ou la variable continue est consideree en splines cubiques restreints a j noeuds)

data <- pbc %>% tibble() %>% 
  mutate(
    trt = as_factor(case_when(trt == 1 ~ "D-penicillmain", 
                              trt == 2 ~ "placebo")),
    death = if_else(status == 2, 1, 0)) %>% 
  filter(!is.na(trt)) %>% 
  mutate(wt = 1)

## 2.1. Age considered as linear ####

# Logistic model (age linear)
model <- glm(death ~ I(trt == "D-penicillmain") * age, data = data, family = "binomial", weights = wt)
summary(model)
wald.test(model, ntest = 4)

# Calcul effects
results <- interaction_cont_factor(event = "death", type = "logistic",
                                   factor = "trt", ref_level = "placebo", 
                                   cont = "age", ref_value = 50, use_rcs = F, nk = NULL, data = data, weight = "wt")

attributes(results)

# Effect of treatment (vs. placebo) according to values of age
col1 <- rgb(red=0, green=0, blue=0.9, alpha=1)
col2 <- rgb(red=0, green=0, blue=0.9, alpha=0.05)

par(mar=c(5.1, 4.1, 4.1, 2.1))
plot_interaction_cont_factor(results = results, effect = "factor",
                             main = "Title", 
                             xlab = "Age (years)",
                             col1=col1, col2=col2, 
                             add_pint=T, name_pint="Interaction cont x factor: ")

# Effect of age in each subgroup (treatment, placebo)
col1 <- c(rgb(red=0, green=0, blue=0.9, alpha=1), rgb(red=0.9, green=0, blue=0, alpha=1))
col2 <- c(rgb(red=0, green=0, blue=0.9, alpha=0.05), rgb(red=0.9, green=0, blue=0, alpha=0.05))

par(mar=c(5.1, 4.1, 4.1, 2.1))
plot_interaction_cont_factor(results=results, effect="cont",
                             main = "Title", 
                             xlab = "Age (years)",
                             col1=col1, col2=col2,  
                             add_pint=T, name_pint="Interaction cont x factor: ",
                             xpint = 35, ypint=4, adj_pint=0,
                             add_legend = T, xlegend = 30, ylegend=8, grid=T)


## 2.2. Age considered as RC splines with 3 knots ####

model <- glm(death ~ I(trt == "D-penicillmain") * I(rcspline.eval(age, nk = 3, inclx = T)), data = data, family = "binomial", weights = wt)
summary(model)
wald.test(model, ntest = 5:6) # interaction p-value

results <- interaction_cont_factor(event = "death", type = "logistic",
                                   factor = "trt", ref_level = "placebo", 
                                   cont = "age", ref_value = 50, use_rcs = T, nk = 3, data = data, weight = "wt")

attributes(results)

# Effect of treatment (vs. placebo) according to values of age
col1 <- rgb(red=0, green=0, blue=0.9, alpha=1)
col2 <- rgb(red=0, green=0, blue=0.9, alpha=0.05)

par(mar=c(5.1, 4.1, 4.1, 2.1))
plot_interaction_cont_factor(results = results, effect = "factor",
                             main = "Title", 
                             xlab = "Age (years)",
                             col1=col1, col2=col2, 
                             add_pint=T, name_pint="Interaction cont x factor: ")

# Effect of age in each subgroup (treatment, placebo)
col1 <- c(rgb(red=0, green=0, blue=0.9, alpha=1), rgb(red=0.9, green=0, blue=0, alpha=1))
col2 <- c(rgb(red=0, green=0, blue=0.9, alpha=0.05), rgb(red=0.9, green=0, blue=0, alpha=0.05))

par(mar=c(5.1, 4.1, 4.1, 2.1))
plot_interaction_cont_factor(results=results, effect="cont",
                             main = "Title", 
                             xlab = "Age (years)",
                             col1=col1, col2=col2,  
                             add_pint=T, name_pint="Interaction cont x factor: ",
                             xpint = 35, ypint=10, adj_pint=0,
                             add_legend = T, xlegend = 30, ylegend=20, grid=T)


#######################################################################################################;
# 3. Check normality ####
#######################################################################################################;

vars_cont <- c("age", "bili", "chol", "albumin", "copper")
path <- "C:/Users/u002453.U001PRD/Desktop" # chemin a red?finir

# Overall population
check_normality_pdf(data = data, strata = NULL, vars = vars_cont, labels = NULL, breaks = 15, 
                    path = path, file = "check_test1", width = 12, height = 8)

# According treatment group
check_normality_pdf(data = data, strata = "trt", vars = vars_cont, labels = NULL, breaks = 15, 
                    path = "FIGURES", file = "check_test2", width = 12, height = 12)
