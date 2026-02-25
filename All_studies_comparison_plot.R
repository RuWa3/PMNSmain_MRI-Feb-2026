###### Standardized mean Z-scores#####

####### FEMALES #######

library(dplyr)

# Standardize within each variable
df_standardized <- df %>%
  group_by(variable) %>%
  mutate(
    mean_std = (mean - mean(mean)) / sd(mean)
  )


dffem<-data.frame(Study = c("GUSTO", "GUSTO", "PMNS", "UK.SA1","UK.SA2","MolSHARE","CURES","MASALA"),
                  n=c(29,37,281,17,10,24,88,420),
                  Age = c(0, 4.5, 18, 23, 24, 37, 45, 55),
                  Wt = c(3.02,17.22,44.8,60.07,65.10,68.6,61.0,64.5),
                  BMI = c(12.0,15.4,18.7,22.4,24.5,26.3,25.7,26.4),
                  ASAT= c(95.86,714.26,3538,4780,6480,255.7,263,260),
                  VAT= c(20.12,184.18,1702,1020,1260,91.3,122,114),
                  ASAT.VAT_Ratio = c(4.8,3.9,1.9,4.9,5.4,2.8,2.2,2.3))

dffem$semstd<-(1/sqrt(dffem$n))

dffem_long <- dffem %>%
  pivot_longer(cols = c(Wt,BMI,ASAT,VAT,ASAT.VAT_Ratio), names_to = "Variable", values_to = "Value")

# Standardize within each variable
dffem_std <- dffem_long %>%
  group_by(Variable) %>%
  mutate(
    mean_std = (Value - mean(Value)) / sd(Value)
  )

dffem_std$Variable<-factor(dffem_std$Variable, levels = c("Wt","BMI","ASAT","VAT","ASAT.VAT_Ratio"))

pfem<-ggplot(dffem_std, aes(x = Age, y = mean_std, color = Variable)) +
  geom_point(size = 7, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Standardise Value (Z score within variable) in Females",
    x = "Age",
    y = "Value",
    color = "Metric",
    shape = "Metric"
  ) +
  #geom_text(aes(label = Study), vjust = -1) +
  geom_text_repel(
    aes(label = Study, color = Variable),
    size = 6,
    fontface = "bold",
    family = "Arial",
    box.padding = 0.4,   # space around text
    point.padding = 0.5, # space between text and point
    max.overlaps = Inf   # allow as many as needed (no warning)
  ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "none", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pfem)

ggsave(pfem, filename="PC_MRI_Female.tiff", width = 16, height = 12, dpi = 400)

dffem_long1 <- dffem %>%
  pivot_longer(cols = c(ASAT,VAT,ASAT.VAT_Ratio), names_to = "Variable", values_to = "Value")

# Standardize within each variable
dffem_std1 <- dffem_long1 %>%
  group_by(Variable) %>%
  mutate(
    mean_std = (Value - mean(Value)) / sd(Value)
  )

dffem_std1$Variable<-factor(dffem_std1$Variable, levels = c("ASAT","VAT","ASAT.VAT_Ratio"))


pfem1<-ggplot(dffem_std1, aes(x = BMI, y = mean_std, color = Study)) +
  geom_point(size = 7, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Standardise Value (Z score within variable) in Females",
    x = "BMI",
    y = "Value",
    color = "Study",
    shape = "Study"
  ) +
  #geom_text(aes(label = Study), vjust = -1) +
  # geom_text_repel(
  #   aes(label = Study, color = Variable),
  #   size = 6,
  #   fontface = "bold",
  #   family = "Arial",
  #   box.padding = 0.4,   # space around text
  #   point.padding = 0.5, # space between text and point
  #   max.overlaps = Inf   # allow as many as needed (no warning)
  # ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "right", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pfem1)

ggsave(pfem1, filename="PC_MRI_Female1.tiff", width = 16, height = 6, dpi = 400)

pfem2<-ggplot(dffem_std1, aes(x = Age, y = mean_std, color = Study)) +
  geom_point(size = 7, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Standardise Value (Z score within variable) in Females",
    x = "Age",
    y = "Value",
    color = "Study",
    shape = "Study"
  ) +
  #geom_text(aes(label = Study), vjust = -1) +
  # geom_text_repel(
  #   aes(label = Study, color = Variable),
  #   size = 6,
  #   fontface = "bold",
  #   family = "Arial",
  #   box.padding = 0.4,   # space around text
  #   point.padding = 0.5, # space between text and point
  #   max.overlaps = Inf   # allow as many as needed (no warning)
  # ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "right", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pfem2)

ggsave(pfem2, filename="PC_MRI_Female2.tiff", width = 16, height = 6, dpi = 400)

####### MALES #######

dfmal<-data.frame(Study = c("GUSTO", "GUSTO", "PMNS", "UK.SA1","UK.SA2","MolSHARE","CRISIS","CURES","MASALA"),
                  n=c(32,27,310,10,34,32,143,76,486),
                  Age = c(0, 4.5, 18, 23, 24, 37, 42, 45, 55),
                  Wt = c(3.21,16.82,55.5,75.75,74.35,83.6,59.3,65.7,74.8),
                  BMI = c(12.7,15.2,19.0,23.9,24.8,27.7,22.0,24.2,26.0),
                  ASAT= c(93.79,605.92,2997,3150,5060,237.6,2600,168.7,217),
                  VAT= c(21.87,184.01,2031,1220,1990,153.8,2500,139.6,152),
                  ASAT.VAT_Ratio = c(4.2,3.2,1.2,2.8,2.9,1.5,1.04,1.2,1.4))

dfmal$semstd<-(1/sqrt(dfmal$n))


dfmal_long <- dfmal %>%
  pivot_longer(cols = c(Wt,BMI,ASAT,VAT,ASAT.VAT_Ratio), names_to = "Variable", values_to = "Value")

# Standardize within each variable
dfmal_std <- dfmal_long %>%
  group_by(Variable) %>%
  mutate(
    mean_std = (Value - mean(Value)) / sd(Value)
  )

dfmal_std$Variable<-factor(dfmal_std$Variable, levels = c("Wt","BMI","ASAT","VAT","ASAT.VAT_Ratio"))

pmal<-ggplot(dfmal_std, aes(x = Age, y = mean_std, color = Variable)) +
  geom_point(size = 2, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1.5, linetype = "solid") +
  geom_errorbar(aes(ymin=mean_std-semstd, ymax=mean_std+semstd), width=3)+
  labs(
    title = "Standardise Value (Z score within variable) in Males",
    x = "Age",
    y = "Value",
    color = "Metric",
    shape = "Metric"
  ) + ylim(-2.5,2.5) +
  #geom_text(aes(label = Study), vjust = -1, size = 6, fontface = "bold") +
  geom_text_repel(
    aes(label = Study, color = Variable),
    size = 3,
    fontface = "bold",
    family = "Arial",
    box.padding = 0.4,   # space around text
    point.padding = 0.5, # space between text and point
    max.overlaps = Inf   # allow as many as needed (no warning)
  ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_text(size = 14, face = "bold", colour = "black"), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "none", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pmal)

ggsave(pmal, filename="PC_MRI_Male.tiff", width = 16, height = 12, dpi = 400)

dfmal_long1 <- dfmal %>%
  pivot_longer(cols = c(ASAT,VAT,ASAT.VAT_Ratio), names_to = "Variable", values_to = "Value")

# Standardize within each variable
dfmal_std1 <- dfmal_long1 %>%
  group_by(Variable) %>%
  mutate(
    mean_std = (Value - mean(Value)) / sd(Value)
  )

dfmal_std1$Variable<-factor(dfmal_std1$Variable, levels = c("ASAT","VAT","ASAT.VAT_Ratio"))

pmal1<-ggplot(dfmal_std1, aes(x = BMI, y = mean_std, color = Study)) +
  geom_point(size = 7, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1.5, linetype = "solid") +
  labs(
    title = "Standardise Value (Z score within variable) in Males",
    x = "BMI",
    y = "Value",
    color = "Study",
    shape = "Study"
  ) + ylim(-1.5,2.5) +
  #geom_text(aes(label = Study), vjust = -1, size = 6, fontface = "bold") +
  # geom_text_repel(
  #   aes(label = Study, color = Variable),
  #   size = 6,
  #   fontface = "bold",
  #   family = "Arial",
  #   box.padding = 0.4,   # space around text
  #   point.padding = 0.5, # space between text and point
  #   max.overlaps = Inf   # allow as many as needed (no warning)
  # ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "right", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pmal1)

ggsave(pmal1, filename="PC_MRI_Male1.tiff", width = 16, height = 6, dpi = 400)


pmal2<-ggplot(dfmal_std1, aes(x = Age, y = mean_std, color = Study)) +
  geom_point(size = 7, shape=17) +
  #geom_line(aes(group = Variable), linewidth = 1.5, linetype = "solid") +
  labs(
    title = "Standardise Value (Z score within variable) in Males",
    x = "Age",
    y = "Value",
    color = "Study",
    shape = "Study"
  ) + ylim(-1.5,2.5) +
  #geom_text(aes(label = Study), vjust = -1, size = 6, fontface = "bold") +
  # geom_text_repel(
  #   aes(label = Study, color = Variable),
  #   size = 6,
  #   fontface = "bold",
  #   family = "Arial",
  #   box.padding = 0.4,   # space around text
  #   point.padding = 0.5, # space between text and point
  #   max.overlaps = Inf   # allow as many as needed (no warning)
  # ) +
  theme_bw(base_size = 14)+
  facet_wrap(~Variable)+
  theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
        axis.title.x = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 14),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold", colour = "black"), 
        legend.position = "right", 
        strip.text = element_text(size = 14, face = "bold", colour = "black")) 

plot(pmal2)

ggsave(pmal2, filename="PC_MRI_Male2.tiff", width = 16, height = 6, dpi = 400)

Age_sexsep<-ggarrange(pmal2,pfem2,
                      nrow = 2, ncol = 1,
                      font.label = list(size = 12, color = "black", face = "bold"))
ggsave(Age_sexsep, filename = "MRI_paper_figure3A.tiff", width = 16, height = 12, dpi = 400)


BMI_sexsep<-ggarrange(pmal1,pfem1,
                      nrow = 2, ncol = 1,
                      font.label = list(size = 12, color = "black", face = "bold"))
ggsave(BMI_sexsep, filename = "MRI_paper_figure3B.tiff", width = 16, height = 12, dpi = 400)


###################### Forest plot style visuals ####################
### These versions were finally used in the paper ####

library(tidyverse)

# your data frame is assumed to be called df

# ensure Study and Variable are nicely ordered factors
dfmal_std <- dfmal_std %>%
  mutate(
    Study = factor(Study),
    Variable = factor(Variable)
  )

# Plot
fpmale <- ggplot(dfmal_std, aes(x = mean_std, y = Study)) +
  geom_point(size = 3, aes(color = Variable), alpha = 0.9) +
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd,
                     color = Variable),
                 height = 0.15, linewidth = 0.9) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Standardized Mean (Z-score)",
    y = "Study",
    color = "Parameter"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16)
  )

fpmale


###### Large FP - MALES ######

study_colorsM <- c(
  "GUSTO" = "#0033cc",   
  "UK.SA1" = "#0033cc",   
  "UK.SA2" = "#0033cc",
  "MolSHARE" = "#0033cc",   
  "MASALA" = "#0033cc",
  "PMNS" = "#80ccff",
  "CRISIS" = "#80ccff",
  "CURES"= "#80ccff"
)


dfmal_std<-dfmal_std%>%
  mutate(
    Study_group = case_when(
      Study %in% c("Study1", "Study2", "Study3") ~ "Blue",
      Study %in% c("Study4", "Study5", "Study6") ~ "LightBlue",
      TRUE ~ "Other"
    )
  )

## WIth all variables and all studies #####

library(tidyverse)

dfmal_std <- dfmal_std %>%
  # Add sample size to study label
  mutate(
    Study_label = paste0(Study, " (", Age, "y)")
  ) %>%
  # Order studies by Age (ascending)
  mutate(
    Study_label = fct_reorder(Study_label, Age, .desc = TRUE),
    Variable = factor(Variable)
  )

fpmalev1<-ggplot(dfmal_std, aes(x = mean_std, y = Study_label, colour = Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsM)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpmalev1

ggsave(fpmalev1, filename="lineplot_allstudies_male.tiff", dpi=400, height = 17, width = 7)


## With selected variables and all studies #####

library(tidyverse)

dfmal_std_v2 <- dfmal_std %>%
  filter(Variable != "Wt")

fpmalev2<-ggplot(dfmal_std_v2, aes(x = mean_std, y = Study_label, colour = Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsM)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    title = "MALES",
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpmalev2

ggsave(fpmalev2, filename="lineplot_allstudies_maleV2.tiff", dpi=400, height = 13, width = 7)


## With selected variables and selcted studies studies #####

library(tidyverse)

dfmal_std_v3 <- dfmal_std_v2 %>%
  filter(!Study_label %in% c("GUSTO (0y)", "GUSTO (4.5y)"))

fpmalev3<-ggplot(dfmal_std_v3, aes(x = mean_std, y = Study_label, colour = Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsM)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    title = "MALES",
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpmalev3

ggsave(fpmalev3, filename="lineplot_allstudies_maleV3.tiff", dpi=400, height = 13, width = 7)



###### Large FP - FEMALES ######

study_colorsF <- c(
  "GUSTO" = "#b30059",   
  "UK.SA1" = "#b30059",   
  "UK.SA2" = "#b30059",
  "MolSHARE" = "#b30059",   
  "MASALA" = "#b30059",
  "PMNS" = "#ff80bf",
  "CRISIS" = "#ff80bf",
  "CURES"= "#ff80bf"
)

## WIth all variables and all studies #####

library(tidyverse)

dffem_std <- dffem_std %>%
  # Add sample size to study label
  mutate(
    Study_label = paste0(Study, " (", Age, "y)")
  ) %>%
  # Order studies by Age (ascending)
  mutate(
    Study_label = fct_reorder(Study_label, Age, .desc = TRUE),
    Variable = factor(Variable)
  )

fpfemalev1<-ggplot(dffem_std, aes(x = mean_std, y = Study_label, color=Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsF)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    title = "FEMALE",
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpfemalev1

ggsave(fpfemalev1, filename="lineplot_allstudies_female.tiff", dpi=400, height = 17, width = 7)



## With selected variables and all studies #####

library(tidyverse)

dffem_std_v2 <- dffem_std %>%
  filter(Variable != "Wt")

fpfemalev2<-ggplot(dffem_std_v2, aes(x = mean_std, y = Study_label, colour = Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsF)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    title = "FEMALES",
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpfemalev2

ggsave(fpfemalev2, filename="lineplot_allstudies_femaleV2.tiff", dpi=400, height = 13, width = 7)


## With selected variables and selcted studies studies #####

library(tidyverse)

dffem_std_v3 <- dffem_std_v2 %>%
  filter(!Study_label %in% c("GUSTO (0y)", "GUSTO (4.5y)"))

fpfemalev3<-ggplot(dffem_std_v3, aes(x = mean_std, y = Study_label, colour = Study)) +
  geom_point(size = 2.6, alpha = 0.9) +
  scale_color_manual(values = study_colorsF)+
  geom_errorbarh(aes(xmin = mean_std - semstd,
                     xmax = mean_std + semstd
                     ),
                 height = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  # --- KEY CHANGE: facet_wrap instead of facet_grid ---
  facet_wrap(~ Variable,
             ncol = 1,
             strip.position = "top") +
  labs(
    title = "FEMALES",
    x = "Standardized Mean (Z-score)",
    y = NULL
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
    axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")   # spacing between variable sections
  )

fpfemalev3

ggsave(fpfemalev3, filename="lineplot_allstudies_femaleV3.tiff", dpi=400, height = 13, width = 7)

library(ggpubr)

LitstudiesV1<-ggarrange(fpmalev1,fpfemalev1,
                      nrow = 1, ncol = 2,
                      font.label = list(size = 12, color = "black", face = "bold"))
ggsave(LitstudiesV1, filename = "MRI_paper_LitstudiesV1.tiff", width = 14, height = 13, dpi = 400)

LitstudiesV2<-ggarrange(fpmalev2,fpfemalev2,
                        nrow = 1, ncol = 2,
                        font.label = list(size = 12, color = "black", face = "bold"))
ggsave(LitstudiesV2, filename = "MRI_paper_LitstudiesV2.tiff", width = 14, height = 13, dpi = 400)

LitstudiesV3<-ggarrange(fpmalev3,fpfemalev3,
                        nrow = 1, ncol = 2,
                        font.label = list(size = 12, color = "black", face = "bold"))
ggsave(LitstudiesV3, filename = "MRI_paper_LitstudiesV3.tiff", width = 14, height = 13, dpi = 400)



###### AGE vs BMI, and other plots ###########

ggplot(dfmal, aes(x = Age, y = BMI, size = VATstd, color = Study)) +
  geom_point(aes(size = VATstd, color = Study), alpha = 0.8) +
  scale_size(range = c(2, 20))+
  theme_bw()

dfmal$Study<-factor(dfmal$Study, levels = c("GUSTO","PMNS", "UKBB.SAS1","UKBB.SAS2","MolSHARE","CRISIS","CURES","MASALA"))
dfmal$VATstd_lower <- dfmal$VATstd - dfmal$semstd
dfmal$VATstd_upper <- dfmal$VATstd + dfmal$semstd
dfmal$VATstd_lower[dfmal$VATstd_lower < 0] <- 0

ggplot(dfmal, aes(x = Age, y = BMI, size = SATstd, color = Study)) +
  geom_point(aes(size = SATstd, color = Study), alpha = 0.8) +
  scale_size(range = c(2, 20))+
  theme_bw()

ggplot(dfmal, aes(x = Age, y = BMI, size = SATVAT_Ratiostd, color = Study)) +
  geom_point(aes(size = SATVAT_Ratiostd, color = Study), alpha = 0.8) +
  scale_size(range = c(2, 20))+
  theme_bw()

