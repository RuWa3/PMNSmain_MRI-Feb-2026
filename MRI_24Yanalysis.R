
library(tidyverse)
library(haven)

pmnsgrand<-read_sav("D:/RUCHA/Rucha_KEM/PMNS data original files/PMNS Masterfile(final)/PMNS_F0_F1_Initialto24Y_6Apr26.sav")
dim(pmnsgrand)
colnames(pmnsgrand)
pmnsgrand$sex_f1child<-factor(pmnsgrand$sex_f1child)


## Glycemic status

pmnsgrand <- pmnsgrand %>%
  mutate(f1_c_gly_status_24y = case_when(
    # 1. Diabetes (Any criteria met)
    c_gluf_24y >= 126 | c_glu120m_24y >= 200 ~ "Diabetes",
    
    # 2. Combined IFG + IGT
    (c_gluf_24y >= 100 & c_gluf_24y <= 125) & 
      (c_glu120m_24y >= 140 & c_glu120m_24y <= 199) ~ "IFG+IGT",
    
    # 3. Isolated IGT (Only 120-min is elevated)
    (c_glu120m_24y >= 140 & c_glu120m_24y <= 199) ~ "IGT",
    
    # 4. Isolated IFG (Only fasting is elevated)
    (c_gluf_24y >= 100 & c_gluf_24y <= 125) ~ "IFG",
    
    # 5. Normal
    c_gluf_24y < 100 & c_glu120m_24y < 140 ~ "Normal",
    
    # Catch-all for missing values
    TRUE ~ NA_character_
  ))

pmnsgrand <- pmnsgrand %>%
  mutate(f1_c_ifgigtdiab_24y = case_when(
    f1_c_gly_status_24y == "Diabetes" ~ "Diabetes",
    f1_c_gly_status_24y %in% c("IFG", "IGT", "IFG+IGT") ~ "Prediabetes",
    f1_c_gly_status_24y == "Normal" ~ "Normal",
    TRUE ~ NA_character_
  ))


table(pmnsgrand$f1_c_gly_status_24y)
prop.table(table(pmnsgrand$f1_c_gly_status_24y))

table(pmnsgrand$f1_c_ifgigtdiab_24y)
prop.table(table(pmnsgrand$f1_c_ifgigtdiab_24y))


pmnsgrand <- pmnsgrand %>%
  mutate(f1_c_TG_HDL_24y=c_tg_24y/c_hdl_24y,
         f1_c_ldl_24y=(c_chol_24y-c_hdl_24y-(c_tg_24y/5)))

pmnsgrand$f1_c_ifg

### Calculate insulin indices on the updated number for 24-years

## IGI (Clive Osmond - modified)
#ln{Insulin(30-minute/fasting)/Glucose(30-minute/fasting)

pmnsgrand$c_hsCRP_24y

pmnsgrand <- pmnsgrand %>%
  mutate(f1_c_IGI_24y = log( (c_ins30m_24y / c_fins_24y) / (c_glu30m_24y / c_gluf_24y) ))


#Matsuda Index (original)
#10000/sqrt(fasting_glucose*fasting_insulin*Mean_OGTT_glucose*Mean_OGTT_insulin).

pmnsgrand <- pmnsgrand %>%
  mutate(f1_c_MatInd_24y = 10000 / sqrt((c_gluf_24y) * (c_fins_24y) *
                                  (rowMeans(cbind(c_gluf_24y, c_glu30m_24y, c_glu120m_24y), na.rm = T)) *
                                  (rowMeans(cbind(c_fins_24y, c_ins30m_24y, c_ins120m_24y), na.rm = T))))


##Metabolic Syndrome

# Check central obesity 
pmnsgrand$f1_c_waist_24y<-rowMeans(pmnsgrand[, c("c_waist_1_24y", "c_waist_2_24y")], na.rm = T)

pmnsgrand$central_obesity_24y <- ifelse(pmnsgrand$sex_f1child == "1", pmnsgrand$f1_c_waist_24y >= 90, pmnsgrand$f1_c_waist_24y >= 80)

# Add other components
pmnsgrand$idf_metS_components_24y <- rowSums(data.frame(
  tg = pmnsgrand$c_tg_24y >= 150,
  hdl = ifelse(pmnsgrand$sex_f1child == "1", pmnsgrand$c_hdl_24y < 40, pmnsgrand$c_hdl_24y < 50),
  bp = pmnsgrand$c_syst_2_24y >= 130 | pmnsgrand$c_diast_2_24y >= 85 ,
  glucose = pmnsgrand$c_gluf_24y >= 100))

# IDF MetS = Central obesity + at least 2 other criteria
pmnsgrand$metS_idf_24y <- pmnsgrand$central_obesity_24y & pmnsgrand$idf_metS_components_24y >= 2

str(pmnsgrand$metS_idf_24y)
table(pmnsgrand$metS_idf_24y)

pmnsgrand$metS_idf_24y <- factor(pmnsgrand$metS_idf_24y, levels = c(FALSE, TRUE), labels = c("Absent", "Present"))

pmnsgrand <- pmnsgrand %>%
mutate(c_age_24y = as.numeric(difftime(c_run, cdob, units = "days")) / 365.25)

str(pmnsgrand$c_run)
str(pmnsgrand$cdob)

pmnsgrand$f1_c_wt_24y<-rowMeans(pmnsgrand$c_ht_1_24y )

library(dplyr)

pmnsgrand <- pmnsgrand %>%
  mutate(
    # Average the two weight columns
    # Replace wt1, wt2 with your actual column names
    f1_c_wt_24y = rowMeans(select(., c_wt_1_24y, c_wt_2_24y), na.rm = TRUE),
    
    # Average the two height columns 
    # Replace ht1, ht2 with your actual column names
    f1_c_ht_24y = rowMeans(select(., c_ht_1_24y, c_ht_2_24y), na.rm = TRUE),
    
    # Average the two WC columns 
    
    f1_c_WC_24y = rowMeans(select(., c_waist_1_24y, c_waist_2_24y), na.rm = TRUE),
    
    # Average the two HC columns 
    
    f1_c_HipC_24y = rowMeans(select(., c_hip_1_24y, c_hip_2_24y), na.rm = TRUE),
    
    # Calculate BMI
    # If height is in cm, use: mean_wt / (mean_ht / 100)^2
    f1_c_bmi_24y = f1_c_wt_24y / (f1_c_ht_24y / 100)^2
  )



##Join the 24Y data to mridf


pmns24dfformri<-pmnsgrand %>%
  select("key","c_age_24y","f1_c_wt_24y","f1_c_ht_24y","f1_c_bmi_24y","f1_c_WC_24y","f1_c_HipC_24y","c_gluf_24y","c_glu30m_24y","c_glu120m_24y","c_fins_24y", "c_ins30m_24y", "c_ins120m_24y",
         "f1_c_IGI_24y","f1_c_MatInd_24y",
         "c_syst_2_24y","c_diast_2_24y","c_pulse_2_24y",
         "c_tg_24y","c_chol_24y","c_hdl_24y","f1_c_ldl_24y","f1_c_TG_HDL_24y",
         "c_hsCRP_24y",
         "metS_idf_24y","f1_c_ifgigtdiab_24y")

mridfv2 <- mridf %>%
  left_join(pmns24dfformri, by = "key")

colnames(mridfv2)

# Calculate your Median (IQR) summary
final_summary <- mridfv2 %>%
  group_by(MRI_status, sex) %>%
  summarise(
    across(c(c_age_24y, f1_c_bmi_24y), list(
      med = ~median(.x, na.rm = TRUE),
      q25 = ~quantile(.x, 0.25, na.rm = TRUE),
      q75 = ~quantile(.x, 0.75, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )

print(final_summary)


library(dplyr)

mridfv2 <- mridfv2 %>%
  mutate(
    # BMI Classification
    bmi24y_category = case_when(
      f1_c_bmi_24y < 18.5 ~ "Underweight",
      f1_c_bmi_24y >= 18.5 & f1_c_bmi_24y < 25 ~ "Normal",
      f1_c_bmi_24y >= 25 & f1_c_bmi_24y < 30 ~ "Overweight",
      f1_c_bmi_24y >= 30 ~ "Obese",
      TRUE ~ NA_character_  # Handles missing values
    ),
    
    # Central Obesity (Sex-specific WC)
    # Assumes your sex column is coded as "Male"/"Female" or "M"/"F"
    central_obesity24y = case_when(
      sex %in% c("Male", "1") & f1_c_WC_24y >= 90 ~ "Yes",
      sex %in% c("Female", "2") & f1_c_WC_24y >= 80 ~ "Yes",
      is.na(f1_c_WC_24y) | is.na(sex) ~ NA_character_,
      TRUE ~ "No"
    )
  )

# Quick look at the distribution
table(mridfv2$MRI_status, mridfv2$sex, mridfv2$bmi24y_category)

library(dplyr)

# Assuming your dataframe is named 'df'
mridfv2 <- mridfv2 %>%
  mutate(
    # 1. Define Dyslipidaemia using mg/dL values
    dyslipidaemia24y = case_when(
      c_tg_24y > 150 ~ TRUE,
      sex == "1"   & c_hdl_24y < 40 ~ TRUE,
      sex == "2" & c_hdl_24y < 50 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # 2. Define Hypertension (units remain mmHg)
    hypertension24y = if_else(c_syst_2_24y >= 130 | c_diast_2_24y >= 85, TRUE, FALSE)
  )

table(mridfv2$MRI_status, mridfv2$sex, mridfv2$dyslipidaemia24y)
table(mridfv2$MRI_status, mridfv2$sex, mridfv2$hypertension24y)


mridfv2 <- mridfv2 %>%
  mutate(
    # Dyslipidaemia: Keeps NA if data is missing
    dyslipidaemia24y = case_when(
      c_tg_24y > 150 ~ TRUE,
      sex == "1"   & c_hdl_24y < 40 ~ TRUE,
      sex == "2" & c_hdl_24y < 50 ~ TRUE,
      # Only mark FALSE if we actually have the numbers to prove it
      is.na(c_tg_24y) | is.na(c_hdl_24y) | is.na(sex) ~ NA, 
      TRUE ~ FALSE
    ),
    
    # Hypertension: Keeps NA if both SBP and DBP are missing
    hypertension24y = case_when(
      c_syst_2_24y >= 130 | c_diast_2_24y >= 85 ~ TRUE,
      is.na(c_syst_2_24y) & is.na(c_diast_2_24y) ~ NA,
      TRUE ~ FALSE
    )
  )



#MLRA

# Define your vector of y-variable names
y_vars <- c("f1_c_ht_24y","f1_c_WC_24y","f1_c_bmi_24y",
            "c_gluf_24y","c_glu30m_24y","c_glu120m_24y",
            "c_fins_24y", "c_ins30m_24y", "c_ins120m_24y",
            "f1_c_IGI_24y","f1_c_MatInd_24y",
            "c_syst_2_24y","c_diast_2_24y","c_pulse_2_24y",
            "c_tg_24y","c_chol_24y","c_hdl_24y","f1_c_ldl_24y","f1_c_TG_HDL_24y",
            "c_hsCRP_24y"
)  #list of y vars

mridfv2 <- mridfv2 %>%
  mutate(across(
    all_of(y_vars),
    ~ ifelse(. < 0, NA, .)
  ))

# Loop over each y-variable to calculate Standardized residuals

for (y in y_vars) {
  
  y_log <- paste0("log_", y)
  
  # Create log variable safely
  mridfv2[[y_log]] <- ifelse(mridfv2[[y]] > 0, log(mridfv2[[y]]), NA)
  
  model1 <- lm(
    as.formula(paste(y_log, "~ c_age_24y + sex")),
    data = mridfv2,
    na.action = na.exclude
  )
  
  std_residuals <- rstandard(model1)
  
  mridfv2[[paste0(y, "_Zresid")]] <- NA
  mridfv2[[paste0(y, "_Zresid")]][as.numeric(names(std_residuals))] <- std_residuals
}


outcomes24y  <- c("c_gluf_24y_Zresid","c_glu30m_24y_Zresid","c_glu120m_24y_Zresid","c_fins_24y_Zresid","c_ins30m_24y_Zresid","c_ins120m_24y_Zresid",
                  "f1_c_IGI_24y_Zresid","f1_c_MatInd_24y_Zresid","c_syst_2_24y_Zresid","c_diast_2_24y_Zresid","c_pulse_2_24y_Zresid",
                  "c_tg_24y_Zresid","c_chol_24y_Zresid","c_hdl_24y_Zresid","f1_c_ldl_24y_Zresid","f1_c_TG_HDL_24y_Zresid","c_hsCRP_24y_Zresid")
exposures <- c("ZRE_SAT_combined", "ZRE_VAT_Combined")
#covars    <- c("age", "sex")   # optional; can be NULL


#2️ Core function: fit model + extract everything

####### Separately for sex ######

run_two_exposure_lm_sex <- function(data, outcome, exp1, exp2, covars, sex_label) {
  
  vars_needed <- c(outcome, exp1, exp2, covars)
  d <- data[, vars_needed]
  d <- d[complete.cases(d), ]
  
  if (nrow(d) < 10) return(NULL)
  
  fml <- as.formula(
    paste(outcome, "~", paste(c(exp1, exp2, covars), collapse = " + "))
  )
  
  fit <- lm(fml, data = d)
  sm  <- summary(fit)$coefficients
  
  if (!all(c(exp1, exp2) %in% rownames(sm))) return(NULL)
  
  b1 <- sm[exp1, "Estimate"]
  se1 <- sm[exp1, "Std. Error"]
  p1 <- sm[exp1, "Pr(>|t|)"]
  
  b2 <- sm[exp2, "Estimate"]
  se2 <- sm[exp2, "Std. Error"]
  p2 <- sm[exp2, "Pr(>|t|)"]
  
  z  <- (b1 - b2) / sqrt(se1^2 + se2^2)
  pz <- 2 * (1 - pnorm(abs(z)))
  
  data.frame(
    Outcome = outcome,
    Sex = sex_label,
    Beta_exp1 = b1, SE_exp1 = se1, P_exp1 = p1,
    Beta_exp2 = b2, SE_exp2 = se2, P_exp2 = p2,
    N = nrow(d),
    Wald_Z = z,
    Wald_P = pz
  )
}


mridfv2_male   <- subset(mridfv2, sex == 1) 
mridfv2_female <- subset(mridfv2, sex == 2)


sex_groups <- list(
  Male   = mridfv2 %>% dplyr::filter(sex == 1),
  Female = mridfv2 %>% dplyr::filter(sex == 2)
)

results_all <- list()

for (sx in c(1,2)) {
  
  dsub <- if (sx == 1) mridfv2_male else mridfv2_female
  sex_lab <- ifelse(sx == 1, "Male", "Female")
  
  for (out in outcomes24y) {
    
    res <- run_two_exposure_lm_sex(
      data = dsub,
      outcome = out,
      exp1 = "ZRE_SAT_combined",
      exp2 = "ZRE_VAT_Combined",
      covars = "f1_c_WC_24y_Zresid",
      sex_label = sx
    )
    
    if (!is.null(res)) {
      results_all[[length(results_all) + 1]] <- res
    }
  }
}

results_sex_strat <- do.call(rbind, results_all)

write.csv(
  results_sex_strat,
  "MRI_MLRA_sex_stratified_wcadj_24y.csv",
  row.names = FALSE
)




##### Conditionals adjusted for age & sex ######

results24y <- lapply(na.omit(unique(mridfv2$sex)), function(sx){
  
  df <- subset(mridfv2, sex == sx)
  
  res <- lapply(outcomes24y, function(y){
    
    fit_asat <- lm(as.formula(paste(y, "~ ASATresidpure")), data = df)
    fit_vat  <- lm(as.formula(paste(y, "~ VATresidpure")), data = df)
    
    data.frame(
      sex = sx,
      outcome = y,
      
      beta_ASAT = coef(summary(fit_asat))[2,1],
      se_ASAT= coef(summary(fit_asat))[2,2],
      p_ASAT    = coef(summary(fit_asat))[2,4],
      
      beta_VAT  = coef(summary(fit_vat))[2,1],
      se_VAT= coef(summary(fit_vat))[2,2],
      p_VAT     = coef(summary(fit_vat))[2,4]
      
      )
  })
  
  do.call(rbind, res)
  
})

results_univariate24y <- do.call(rbind, results24y)

write.csv(
  results_univariate24y,
  "MRI_uni_sex_stratified_24Y_18May26.csv",
  row.names = FALSE
)



### logistic regression for binary outcomes ####

mets24logit<-glm(metS_idf_24y~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = mridfv2)
summary(mets24logit)
exp(coef(mets24logit))
exp(confint(mets24logit))
mets24logitM<-glm(metS_idf_24y~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridfv2, sex==1))
summary(mets24logitM)
exp(coef(mets24logitM))
exp(confint(mets24logitM))
mets24logitF<-glm(metS_idf_24y~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridfv2, sex==2))
summary(mets24logitF)
exp(coef(mets24logitF))
exp(confint(mets24logitF))

##################################
mridfv2 <- mridfv2 %>%
  mutate(f1_c_ifgigtdiab_24yv2 = case_when(
    f1_c_ifgigtdiab_24y == "Diabetes" ~ "Glucose Intolerance",
    f1_c_ifgigtdiab_24y == "Prediabetes" ~ "Glucose Intolerance",
    f1_c_ifgigtdiab_24y == "Normal" ~ "Normal",
    TRUE ~ NA_character_
  ))

mridfv2$f1_c_ifgigtdiab_24yv2<-factor(mridfv2$f1_c_ifgigtdiab_24yv2, levels = c("Normal","Glucose Intolerance"))
###################################

pred24logit<-glm(f1_c_ifgigtdiab_24yv2~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = mridfv2)
summary(pred24logit)
exp(coef(pred24logit))
exp(confint(pred24logit))
pred24logitM<-glm(f1_c_ifgigtdiab_24yv2~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridfv2, sex==1))
summary(pred24logitM)
exp(coef(pred24logitM))
exp(confint(pred24logitM))
pred24logitF<-glm(f1_c_ifgigtdiab_24yv2~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridfv2, sex==2))
summary(pred24logitF)
exp(coef(pred24logitF))
exp(confint(pred24logitF))


exp(cbind(OR = pred24logit$coef,
          lower = pred24logit$ci.lower,
          upper = pred24logit$ci.upper))


######

library(gtsummary)
library(dplyr)

# Put your models in a list
models <- list(
  GI_All = pred24logit,
  GI_Male = pred24logitM,
  GI_Female = pred24logitF,
  MetS_All = mets24logit,
  MetS_Male = mets24logitM,
  MetS_Female = mets24logitF
)

# Create tables and merge
tbl_list <- lapply(models, function(x) {
  tbl_regression(
    x,
    exponentiate = TRUE  # gives OR instead of log-odds
  )
})

final_tbl <- tbl_merge(
  tbl_list,
  tab_spanner = names(models)
)

final_tbl

as_gt(final_tbl)          # for Word/HTML
as_flex_table(final_tbl) # Word


install.packages("modelsummary")
library(modelsummary)
library(flextable)

tab<-modelsummary(
  models,
  exponentiate = TRUE,
  statistic = "({conf.low}, {conf.high})",
  stars = TRUE,
  output = "flextable"
)
save_as_xlsx(tab, path = "MRI_ORsex.xlsx")

