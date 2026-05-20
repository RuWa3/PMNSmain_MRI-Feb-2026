
rm(list = ls()) #Clear the environment

#------------------------------------------------------
# Metabolic syndrome - IDF
#------------------------------------------------------

library(haven)
library(foreign)
library(tidyverse)

mridf<-read_sav("PMNS_18y_MRI_test.sav") ## Reading the main dataframe
dim(mridf)
colnames(mridf)
str(mridf$sex)
mridf$sex<-factor(mridf$sex)



cor.test(mridf$ZRE_SAT_combined, mridf$ZRE_VAT_Combined) # Test the correlation between ASAT and VAT
cor.test(mridf$tot_VAT_Area_18yr, mridf$tot_ASAT_area_18yr)

mridf$TG_HDL<-mridf$ctg_18y/mridf$chdl_18y #Calculate TG:HDL ratio
mridf$cldl_18y<-(mridf$cchol_18y-mridf$chdl_18y-(mridf$ctg_18y/5)) #Calculate LDL-c by Friedwahl formula


# Check central obesity 
mridf$central_obesity_18y <- ifelse(mridf$sex == "1", mridf$cwaist_18y >= 90, mridf$cwaist_18y >= 80)

# Add other components
mridf$idf_metS_components <- rowSums(data.frame(
  tg = mridf$ctg_18y >= 150,
  hdl = ifelse(mridf$sex == "Male", mridf$chdl_18y < 40, mridf$chdl_18y < 50),
  bp = mridf$csyst_18y >= 130 | mridf$cdiast_18y >= 85 ,
  glucose = mridf$cgluf_18y >= 100))

# IDF MetS = Central obesity + at least 2 other criteria
mridf$metS_idf <- mridf$central_obesity_18y & mridf$idf_metS_components >= 2

str(mridf$metS_idf)
table(mridf$metS_idf)

mridf$metS_idf <- factor(mridf$metS_idf, levels = c(FALSE, TRUE), labels = c("Absent", "Present"))


mridf$gly_st_18y_new<-factor(mridf$gly_st_18y_new)
mridf <- mridf %>%
  mutate(gly_st_18y_new = recode(gly_st_18y_new,
                            "1" = "NGT",
                            "2" = "Prediabetes"
                            ))

# #Dyslipidemia
# 
# colnames(mridf)
# table(mridf$sex)
# 
# mridf <- mridf %>%
#   mutate(
#     dyslipidemia = case_when(
#       if_any(c(cchol_18y, cldl_18y, ctg_18y, chdl_18y), is.na) ~ NA_real_,
#       (cchol_18y >= 200 | cldl_18y >= 130 | ctg_18y >= 150 |
#          (sex == "1" & chdl_18y < 40) |
#          (sex == "2" & chdl_18y < 50)) ~ 1,
#       TRUE ~ 0
#     )
#   )
# 
# 
# mridf %>%
#   group_by(sex) %>%
#   summarise(
#     dyslipidemia_n = sum(dyslipidemia, na.rm = TRUE),
#     total_n = n(),
#     prevalence = mean(dyslipidemia, na.rm = TRUE) * 100
#   )
# 
# view(mridf %>% select(cchol_18y, cldl_18y, ctg_18y, chdl_18y, dyslipidemia))


##############################################################################################################

#----------------------------------------------------------------------------------------------------------
# Multiple regression models for ASAT/VAT as exposures ########
#----------------------------------------------------------------------------------------------------------

#We have 

#Calculating one-way residualization for Partitioning Variance for ASAT Dominance

# Assuming your data is called 'df'
mridf <- mridf %>%
  group_by(sex) %>%
  mutate(
    # Step 1: ASAT independent of Age, SES, and VAT (separately by sex)
    ASATresidpure = if(any(!is.na(sex))) rstandard(lm(tot_ASAT_area_18yr~cage18y+sliscore_18y+tot_VAT_Area_18yr, na.action = na.exclude)) else NA,
    
    ASATresid = if(any(!is.na(sex))) rstandard(lm(tot_ASAT_area_18yr~cage18y+sliscore_18y, na.action = na.exclude)) else NA,
    
    # Step 2: VAT independent of Age and SES (separately by sex)
    VATresid = if(any(!is.na(sex))) rstandard(lm(tot_VAT_Area_18yr~cage18y+sliscore_18y, na.action = na.exclude)) else NA,
    
    VATresidpure = if(any(!is.na(sex))) rstandard(lm(tot_VAT_Area_18yr~cage18y+sliscore_18y+tot_ASAT_area_18yr, na.action = na.exclude)) else NA,
    
    ASAT_VATresid = if(any(!is.na(sex))) rstandard(lm(MRI_subcut_visc_ratio~cage18y+sliscore_18y, na.action = na.exclude)) else NA
    
  ) %>%
  ungroup() # Important to ungroup before running the final model


# Define your vector of y-variable names
y_vars <- c("cgluf_18y","cglu30_18y","cglu2hr_18y","cinsf_18y","cins30_18y","cins2hr_18y", "homa_ir_18", "homa_beta_18",
             "homa_sens_18","disp_index_18","cIGI_18y","cmat_18y","cdyn_DI_18y","csyst_18y","cdiast_18y","cpulse_18y",
             "ctg_18y","cchol_18y","chdl_18y","cldl_18y","TG_HDL","cleptin_18y","totaladip_18y","hmwadip_18y","ccrp_18y","cwbc_18y"
             )  #list of y vars

mridf <- mridf %>%
  mutate(across(
    all_of(y_vars),
    ~ ifelse(. < 0, NA, .)
  ))


# Loop over each y-variable to calculate Standardized residuals
# Define your covariates (now including SES)
covariates <- "cage18y+sliscore_18y"

# Split data by sex to calculate residuals independently

for (y in y_vars) {
    mridf[[paste0(y, "_Zresid")]] <- NA
  }
  
  # na.omit(unique(...)) ensures the loop ignores the NA "category"
  for (s in na.omit(unique(mridf$sex))) {
  
  # Create a temporary subset for the current sex
  subset_idx <- which(mridf$sex == s)
  temp_data <- mridf[subset_idx, ]
  
  for (y in y_vars) {
    y_log <- paste0("log_", y)
    
    # 1. Create log variable
    temp_data[[y_log]] <- ifelse(temp_data[[y]] > 0, log(temp_data[[y]]), NA)
    
    # 2. Fit model with SES included (within this sex group)
    formula_str <- paste(y_log, "~", covariates)
    model1 <- lm(as.formula(formula_str), data = temp_data, na.action = na.exclude)
    
    # 3. Calculate standardized residuals
    std_residuals <- rstandard(model1)
    
    # 4. Map back to the main dataframe
    # Use names(std_residuals) to ensure rows align correctly
    mridf[[col_name]][as.numeric(names(std_residuals))] <- std_residuals
  }
}



# 1. Define your variables
covariates <- c("cage18y", "sliscore_18y")

# 2. Pre-create the result columns with NAs in the main dataframe
for (y in y_vars) {
  mridf[[paste0(y, "_Zresid")]] <- NA
}

# 3. Loop through sexes, excluding NAs
for (s in na.omit(unique(mridf$sex))) {
  
  # Identify indices for the current sex
  subset_idx <- which(mridf$sex == s)
  
  # Create the subset
  temp_data <- mridf[subset_idx, ]
  
  for (y in y_vars) {
    y_log <- paste0("log_", y)
    res_name <- paste0(y, "_Zresid") # Define the target column name clearly
    
    # Create log variable
    temp_data[[y_log]] <- ifelse(!is.na(temp_data[[y]]) & temp_data[[y]] > 0, log(temp_data[[y]]), NA)
    
    # Check for complete cases within this subset
    valid_rows <- complete.cases(temp_data[, c(y_log, covariates)])
    
    if (sum(valid_rows) > 10) {
      
      formula_str <- paste(y_log, "~", paste(covariates, collapse = " + "))
      
      # Fit model - na.exclude is crucial here
      model1 <- lm(as.formula(formula_str), data = temp_data, na.action = na.exclude)
      
      # Extract standardized residuals
      # rstandard() with na.exclude will return a vector the same length as temp_data
      std_res <- rstandard(model1)
      
      # Map these back into the main dataframe using the indices we identified earlier
      mridf[subset_idx, res_name] <- std_res
      
    } else {
      message(paste("Skipping:", y, "for sex:", s, "- not enough data."))
    }
  }
}


outcomes  <- c("cgluf_18y_Zresid", "cglu30_18y_Zresid", "cglu2hr_18y_Zresid","cinsf_18y_Zresid","cins30_18y_Zresid","cins2hr_18y_Zresid",
               "homa_ir_18_Zresid","homa_beta_18_Zresid","homa_sens_18_Zresid","disp_index_18_Zresid",
               "cIGI_18y_Zresid","cmat_18y_Zresid","cdyn_DI_18y_Zresid","csyst_18y_Zresid","cdiast_18y_Zresid","cpulse_18y_Zresid",
               "ctg_18y_Zresid","cchol_18y_Zresid","chdl_18y_Zresid","cldl_18y_Zresid","TG_HDL_Zresid",
               "cleptin_18y_Zresid","totaladip_18y_Zresid","hmwadip_18y_Zresid","cwbc_18y_Zresid","ccrp_18y_Zresid")
exposures <- c("ASATresid", "VATresid")
#covars    <- c("age", "sex")   # optional; can be NULL

#Residuals adjusted for height
outcomes1  <- c("cgluf_18y_Zresidht", "cglu30_18y_Zresidht", "cglu2hr_18y_Zresidht","cinsf_18y_Zresidht","cins30_18y_Zresidht","cins2hr_18y_Zresidht",
               "homa_ir_18_Zresidht","homa_beta_18_Zresidht","homa_sens_18_Zresidht","disp_index_18_Zresidht",
               "cIGI_18y_Zresidht","cmat_18y_Zresidht","cdyn_DI_18y_Zresidht","csyst_18y_Zresidht","cdiast_18y_Zresidht","cpulse_18y_Zresidht",
               "ctg_18y_Zresidht","cchol_18y_Zresidht","chdl_18y_Zresidht","cldl_18y_Zresidht","TG_HDL_Zresidht",
               "cleptin_18y_Zresidht","totaladip_18y_Zresidht","hmwadip_18y_Zresidht","cwbc_18y_Zresidht","ccrp_18y_Zresidht")


#2️ Core function: fit model + extract everything

run_two_exposure_lm <- function(data, outcome, exp1, exp2) {
  
  # Build formula
  rhs <- c(exp1, exp2)
  fml <- as.formula(
    paste(outcome, "~", paste(rhs, collapse = " + "))
  )
  
  # Fit model
  fit <- lm(fml, data = data)
  sm  <- summary(fit)
  vc  <- vcov(fit)
  
  # Extract coefficients
  est1 <- sm$coefficients[exp1, "Estimate"]
  se1  <- sm$coefficients[exp1, "Std. Error"]
  p1   <- sm$coefficients[exp1, "Pr(>|t|)"]
  
  est2 <- sm$coefficients[exp2, "Estimate"]
  se2  <- sm$coefficients[exp2, "Std. Error"]
  p2   <- sm$coefficients[exp2, "Pr(>|t|)"]
  
  # Sample size actually used
  n <- nobs(fit)
  
  # Wald test: beta1 vs beta2
  wald_z <- (est1 - est2) / sqrt(se1^2 + se2^2)
  wald_p <- 2 * (1 - pnorm(abs(wald_z)))
  
  # Return tidy row
  data.frame(
    Outcome = outcome,
    
    Beta_exp1 = est1,
    SE_exp1   = se1,
    P_exp1    = p1,
    
    Beta_exp2 = est2,
    SE_exp2   = se2,
    P_exp2    = p2,
    
    N = n,
    
    Wald_Z = wald_z,
    Wald_P = wald_p,
    
    stringsAsFactors = FALSE
  )
}

#3️ Loop over all outcomes (clean + safe)

results_all <- do.call(
  rbind,
  lapply(
    outcomes1,
    run_two_exposure_lm,
    data  = mridf,
    exp1  = "ZRE_SAT_combined",
    exp2  = "ZRE_VAT_Combined"
    )
)

#4️ Write final summary CSV

write.csv(
  results_all,
  "MRI_paper_MLRAsummary_adjht.csv",
  row.names = FALSE
)


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


mridf_male   <- subset(mridf, sex == 1) 
mridf_female <- subset(mridf, sex == 2)


# sex_groups <- list(
#   Male   = mridf %>% dplyr::filter(sex == 1),
#   Female = mridf %>% dplyr::filter(sex == 2)
# )

results_all <- list()

for (sx in c(1,2)) {
  
  dsub <- if (sx == 1) mridf_male else mridf_female
  sex_lab <- ifelse(sx == 1, "Male", "Female")
  
  for (out in outcomes) {
    
    res <- run_two_exposure_lm_sex(
      data = dsub,
      outcome = out,
      exp1 = "ASATresidpure",
      exp2 = NULL,
      covars = NULL,
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
  "MRI_MLRA_sex_stratified_ASATpure_15May26.csv",
  row.names = FALSE
)

## The above MLRA is adjusted for age and ses and performed separately for sexes

####### Add BMI and WC as covariates at each time ##########

#Modified the above codes to adjust new covariates, one at a time


############ Conditional variable framework - Residualised univariate models #########



results1 <- lapply(na.omit(unique(mridf$sex)), function(sx){
  
  df <- subset(mridf, sex == sx)
  
  res <- lapply(outcomes, function(y){
    
    fit_asat <- lm(as.formula(paste(y, "~ ASATresidpure")), data = df)
    fit_vat  <- lm(as.formula(paste(y, "~ VATresidpure")), data = df)
    fit_ratio <- lm(as.formula(paste(y, "~ ASAT_VATresid")), data = df)
    
    data.frame(
      sex = sx,
      outcome = y,
      
      beta_ASAT = coef(summary(fit_asat))[2,1],
      se_ASAT= coef(summary(fit_asat))[2,2],
      p_ASAT    = coef(summary(fit_asat))[2,4],
      
      beta_VAT  = coef(summary(fit_vat))[2,1],
      se_VAT= coef(summary(fit_vat))[2,2],
      p_VAT     = coef(summary(fit_vat))[2,4],
      
      beta_ratio = coef(summary(fit_ratio))[2,1],
      se_ratio= coef(summary(fit_ratio))[2,2],
      p_ratio    = coef(summary(fit_ratio))[2,4]
    )
  })
  
  do.call(rbind, res)
  
})

results_univariate <- do.call(rbind, results1)

write.csv(
  results_univariate,
  "MRI_uni_sex_stratified_16May26.csv",
  row.names = FALSE
)


### logistic regression

mets1logit<-glm(metS_idf~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = mridf)
summary(mets1logit)

exp(0.11)
exp(0.11 - (1.96 * 0.10))
exp(0.11 + (1.96 * 0.10))

exp(0.12)
exp(0.12 - (1.96 * 0.10))
exp(0.12 + (1.96 * 0.10))

pred1logit<-glm(gly_st_18y_new~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = mridf)
summary(pred1logit)
exp(coef(pred1logit))
exp(confint(pred1logit))

pred2logit<-glm(gly_st_18y_new~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridf, sex==1))
summary(pred2logit)

# #Call:
# #glm(formula = gly_st_18y_new ~ ZRE_VAT_Combined + ZRE_SAT_combined, 
#     family = "binomial", data = subset(mridf, sex == 1))
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.4143     0.1178  -3.517 0.000436 ***
#   ZRE_VAT_Combined   0.2284     0.1351   1.691 0.090859 .  
# ZRE_SAT_combined   0.1763     0.1340   1.315 0.188355    

exp(coef(pred2logit))
exp(confint(pred2logit))


pred3logit<-glm(gly_st_18y_new~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridf, sex==2))
summary(pred3logit)

exp(coef(pred3logit))
exp(confint(pred3logit))


exp(cbind(OR = pred2logit$coef,
          lower = pred2logit$ci.lower,
          upper = pred2logit$ci.upper))


##New approach for metS modelling give the small numbers - Firth penalised logistic regression

install.packages("logistf")   # once
library(logistf)

mets2logit <- logistf(metS_idf~ZRE_VAT_Combined+ZRE_SAT_combined,  data = mridf)
summary(mets2logit)

exp(cbind(OR = mets2logit$coef,
          lower = mets2logit$ci.lower,
          upper = mets2logit$ci.upper))

#                          OR       lower      upper   p
#(Intercept)      0.007244044 0.002207831 0.01732245  
#ZRE_VAT_Combined 1.569336933 0.848217240 2.93204584 0.149
#ZRE_SAT_combined 2.722640827 1.805959050 4.26394447 <0.001


####################create overlaying density plots#################

VAT_den_plotsex<-ggplot(mridf, aes(x=tot_VAT_Area_18yr, fill=sex, color=sex)) +
  geom_density(alpha=.25, linewidth=2) + 
  ggtitle(expression("MRI VAT area (cm"^2*") in men and women")) + 
  scale_color_manual(values = c("blue","hotpink"), labels = c("1" = "Men", "2" = "Women"))+
  scale_fill_manual(values = c("lightblue","pink"), labels = c("1" = "Men", "2" = "Women"))+
  #geom_vline(xintercept = c(18.5, 25), linetype="dashed", linewidth=1, color="red")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"), 
        axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(size = 12, face = "bold", colour = "black"), axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))

SAT_den_plotsex<-ggplot(mridf, aes(x=tot_ASAT_area_18yr, fill=sex, color=sex)) +
  geom_density(alpha=.25, linewidth=2) + 
  ggtitle(expression("MRI ASAT area (cm"^2*") in men and women")) + 
  scale_color_manual(values = c("blue","hotpink"), labels = c("1" = "Men", "2" = "Women"))+
  scale_fill_manual(values = c("lightblue","pink"), labels = c("1" = "Men", "2" = "Women"))+
  #geom_vline(xintercept = c(18.5, 25), linetype="dashed", linewidth=1, color="red")+
  theme_classic()+ xlim(0, 1000) +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"), 
        axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(size = 12, face = "bold", colour = "black"), axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))

totMRIabd_den_plotsex<-ggplot(mridf, aes(x=tot_abd_fat_MRI, fill=sex, color=sex)) +
  geom_density(alpha=.25, linewidth=2) + 
  ggtitle(expression("MRI tot abd area (cm"^2*") in men and women")) + 
  scale_color_manual(values = c("blue","hotpink"), labels = c("1" = "Men", "2" = "Women"))+
  scale_fill_manual(values = c("lightblue","pink"), labels = c("1" = "Men", "2" = "Women"))+
  #geom_vline(xintercept = c(18.5, 25), linetype="dashed", linewidth=1, color="red")+
  theme_classic()+ xlim(0, 1000) +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"), 
        axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(size = 12, face = "bold", colour = "black"), axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))

MRIdist <- ggarrange(VAT_den_plotsex,SAT_den_plotsex,totMRIabd_den_plotsex, labels = c("a","b","c"),
                           common.legend = TRUE, legend = "bottom", ncol = 3, nrow = 1, font.label = list(size = 14, color = "black", face = "bold"))
ggsave(MRIdist, filename = "MRI distribution.tiff", dpi = 300, width = 15, height = 5, units = "in")


####### Subset analysis - Underweight vs normal vs overweight-obese ########

dim(mridf)

mridf$BMI18y_grp <- factor(
  ifelse(mridf$cbmi_18y < 18.5, 1, 2),
  levels = c(1, 2),
  labels = c("Underweight", "Non-underweight")
)

table(mridf$BMI18y_grp)

#Loop regression models

grp_levels <- sort(unique(mridf$cbmi_cat18y[!is.na(mridf$cbmi_cat18y)]))


results_all <- list()
k <- 1

for (g in grp_levels) {
  
  ## subset by 3-level group
  df_grp <- mridf[!is.na(mridf$cbmi_cat18y) & mridf$cbmi_cat18y == g, ]
  
  ## ---- COMBINED SEX ----
  for (out in outcomes) {
    
    res <- run_two_exposure_lm(
      data    = df_grp,
      outcome = out,
      exp1    = "ZRE_SAT_combined",
      exp2    = "ZRE_VAT_Combined"
      )
    
    if (!is.null(res)) {
      res$GroupVar <- g
      res$Sex      <- "Combined"
      results_all[[k]] <- res
      k <- k + 1
    }
  }
  
  ## ---- SEX-STRATIFIED ----
  for (sx in c(1, 2)) {
    
    dsub <- df_grp[!is.na(df_grp$sex) & df_grp$sex == sx, ]
    sex_lab <- ifelse(sx == 1, "Male", "Female")
    
    for (out in outcomes) {
      
      res <- run_two_exposure_lm(
        data    = dsub,
        outcome = out,
        exp1    = "ZRE_SAT_combined",
        exp2    = "ZRE_VAT_Combined"
        
      )
      
      if (!is.null(res)) {
        res$GroupVar <- g
        res$Sex      <- sex_lab
        results_all[[k]] <- res
        k <- k + 1
      }
    }
  }
}

results_group_sex <- do.call(rbind, results_all)

write.csv(
  results_group_sex,
  "two_exposure_results_group_and_sex.csv",
  row.names = FALSE
)



#### Visual summary ###

colnames(results_group_sex)


#Trial 1

library(ggplot2)

results_group_sex$Outcome <- factor(
  results_group_sex$Outcome,
  levels = rev(unique(results_group_sex$Outcome))
)

results_group_sex$GroupVar <- factor(
  results_group_sex$GroupVar,
  levels = c(1, 2, 3),
  labels = c("Underweight", "Normal", "Overweight-obese")
)


results_group_sex$Sig_exp1 <- results_group_sex$P_exp1 < 0.05

keep_outcomes <- c(
  "cgluf_18y_Zresid","cglu2hr_18y_Zresid","cIGI_18y_Zresid","cmat_18y_Zresid",
  "csyst_18y_Zresid","cdiast_18y_Zresid",
  "ctg_18y_Zresid","cchol_18y_Zresid","chdl_18y_Zresid","cldl_18y_Zresid","TG_HDL_Zresid",
  "cleptin_18y_Zresid","totaladip_18y_Zresid","cwbc_18y_Zresid","ccrp_18y_Zresid"
)

plot_df <- results_group_sex[
  results_group_sex$Outcome %in% keep_outcomes, 
]

plot_df$Sig_exp1 <- plot_df$P_exp1 < 0.05
plot_df$Sig_exp2 <- plot_df$P_exp2 < 0.05


grp.plotexp1<-ggplot(plot_df,
       aes(x = Beta_exp1, y = Outcome)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  
  geom_errorbarh(
    aes(xmin = Beta_exp1 - 1.96 * SE_exp1,
        xmax = Beta_exp1 + 1.96 * SE_exp1),
    height = 0.2
  ) +
  
  geom_point(aes(colour = Sig_exp1), size = 2) +
  
  facet_grid(Sex ~ GroupVar) +
  scale_colour_manual(
    values = c("FALSE" = "black", "TRUE" = "maroon")
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    
    ## 🔑 separation tweaks
    panel.spacing.x = unit(1.2, "lines"),   # space between groups
    panel.spacing.y = unit(1.5, "lines"),   # space between sexes
    
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "grey95", colour = "grey60", linewidth = 0.3
    ),
    legend.position = "none"
  ) +
  
  labs(
    x = "Regression coefficient (95% CI)",
    y = "Outcome",
    title = "aSAT–outcome associations by group and sex"
  )

ggsave(grp.plotexp1, filename="MRI_BMIcat_sex_aSAT_col.tiff", dpi = 300, height = 7, width = 7, units = "in")

#VAT

grp.plotexp2<-ggplot(plot_df,
                     aes(x = Beta_exp2, y = Outcome)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  
  geom_errorbarh(
    aes(xmin = Beta_exp2 - 1.96 * SE_exp2,
        xmax = Beta_exp2 + 1.96 * SE_exp2),
    height = 0.2
  ) +
  
  geom_point(aes(colour = Sig_exp2), size = 2) +
  
  facet_grid(Sex ~ GroupVar) +
  scale_colour_manual(
    values = c("FALSE" = "black", "TRUE" = "maroon")
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    
    ## 🔑 separation tweaks
    panel.spacing.x = unit(1.2, "lines"),   # space between groups
    panel.spacing.y = unit(1.5, "lines"),   # space between sexes
    
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "grey95", colour = "grey60", linewidth = 0.3
    ),
    legend.position = "none"
  ) +
  
  labs(
    x = "Regression coefficient (95% CI)",
    y = "Outcome",
    title = "VAT–outcome associations by group and sex"
  )

ggsave(grp.plotexp2, filename="MRI_BMIcat_sex_VAT_col.tiff", dpi = 300, height = 7, width = 7, units = "in")


####### correlogram of adiposity and obesity measures #######

spearman_corrplot <- function(df, vars) {
  res <- Hmisc::rcorr(as.matrix(df[, vars]), type = "spearman")
  corrplot::corrplot(res$r,
                     method = "number", type = "upper",
                     p.mat = res$P, sig.level = 0.05, insig = "blank",
                     tl.col = "black", tl.srt = 45)
  invisible(res)
}

out <- spearman_corrplot(mridf, c("cbmi_18y", "cwaist_18y", "tot_VAT_Area_18yr", "tot_ASAT_area_18yr"))
outM <- spearman_corrplot(mridf_male, c("cbmi_18y", "cwaist_18y", "tot_VAT_Area_18yr", "tot_ASAT_area_18yr"))
outF <- spearman_corrplot(mridf_female, c("cbmi_18y", "cwaist_18y", "tot_VAT_Area_18yr", "tot_ASAT_area_18yr"))


#by sex

vars <- c("cbmi_18y", "cwaist_18y", "tot_VAT_Area_18yr", "tot_ASAT_area_18yr")

for (s in unique(mridf$sex)) {
  df_s <- mridf[mridf$sex == s, vars]
  df_s <- df_s[, sapply(df_s, function(x) sd(x, na.rm = TRUE) > 0)]
  cor_mat <- cor(df_s, method = "spearman", use = "pairwise.complete.obs")
  corrplot::corrplot(cor_mat, type = "upper", tl.srt = 45,
                     title = paste("Spearman – Sex", s))
}

