install.packages("ggplot2")
library(ggplot2)

PC_MRI<-read.csv("Graphs.csv", header = T, sep = ",")

ggplot(PC_MRI, aes(x=Height, y=Ratio, colour=Gender)) +
  geom_point() + geom_smooth(method = lm) + xlim(150, 180)

ggplot(PC_MRI, aes(x=Weight, y=Ratio, colour=Gender)) +
  geom_point() + xlim(40, 100) + geom_smooth(method = lm)


###### comparative plots #######

PreD_boys <- data.frame(parameters=c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","Fat %","Lean %","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"),
                        md=c(-0.184,0.258,0.250,0.381,0.298,0.286,0.341,-0.343,0.453,0.023,-0.019,-0.654,-0.544,0.052,0.119), 
                        lower=c(-0.413,0.031,0.023,0.155,0.072,0.058,0.114,-0.571,0.229,-0.205,-0.251,-0.874,-0.768,-0.175,-0.109), 
                        upper=c(0.044,0.485,0.478,0.606,0.524,0.513,0.569,-0.115,0.676,0.253,0.211,-0.435,-0.32,0.281,0.347))


head(PreD_boys)

p1 <- ggplot(data=PreD_boys, aes(x=parameters, y=md)) +
  geom_bar(stat="identity", color="black", fill="lightblue", width = 0.5) + coord_flip() +
  scale_x_discrete(limits=rev(c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","Fat %","Lean %","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"))) + 
  ggtitle("Cardiometabolic risk in glucose intolerant men (vs NGT)") + 
  xlab("Parameters")+ ylab("Mean Z scores, 95% CI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=.05, alpha = 1) 
plot(p1)
ggsave(p1, filename = "PreDmen.jpg", dpi = 600)



PreD_girls <- data.frame(parameters=c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","Fat %","Lean %","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"),
                        md=c(0.113,0.055,-0.112,0.062,0.031,-0.032,-0.016,0.017,0.31,-0.117,-0.424,-0.785,-0.914,0.128,0.001), 
                        lower=c(-0.405,-0.352,-0.188,-0.363,-0.327,-0.265,-0.314,-0.28,0.013,-0.416,-0.725,-1.079,-1.19,-0.169,-0.273), 
                        upper=c(0.178,0.241,0.412,0.237,0.264,0.329,0.280,0.315,0.606,0.181,-0.122,-0.491,-0.630,0.425,0.276))


head(PreD_girls)

p2 <- ggplot(data=PreD_girls, aes(x=parameters, y=md)) +
  geom_bar(stat="identity", color="black", fill="pink", width = 0.5) + coord_flip() +
  scale_x_discrete(limits=rev(c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","Fat %","Lean %","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"))) + 
  ggtitle("Cardiometabolic risk in glucose intolerant women (vs NGT)") + 
  xlab("Parameters")+ ylab("Mean Z scores, 95% CI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=.05, alpha = 1) 
plot(p2)
ggsave(p2, filename = "PreDwomen.jpg", dpi = 600)

#----------------------------new graphs-------------------------------

newdatmen<-data.frame(group=rep(c("NGT","Prediabetes"),4),
                   type=rep(c("Height","BMI","MRI VAT","MRI ASAT"),each=2),
                    val=c(170.1,168.8,18.8,19.4,188.0,201.2,203.3,241.6),
                   lower=c(165.4,165.5,16.9,18.0,149.7,162.2,149.8,154.4),
                   upper=c(174.3,173.5,20.9,22.2,236.1,255.1,326.0,426.7))

View(newdatmen)


pmen<-ggplot(newdatmen, aes(x=type, y=val, fill=group)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title = "Body size/composition in men", fill="Group", y= "Median (25th, 75th)") +
  scale_fill_manual(values = c("seagreen","indianred")) +
  scale_x_discrete(limits=c("Height","BMI","MRI VAT","MRI ASAT")) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), width=.05, alpha = 1) +
  theme_gray() + theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
                          axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 12),
                          axis.title.y = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 12, face = "bold", colour = "black"), 
                          legend.position = "right", legend.title = element_text(size = 14), legend.text = element_text(size = 14)) 



newdatwomen<-data.frame(group=rep(c("NGT","Prediabetes"),4),
                      type=rep(c("Height","BMI","MRI VAT","MRI ASAT"),each=2),
                      val=c(156.9,157.2,18.2,17.4,165.9,159.0,323.7,273.5),
                      lower=c(153.3,152.7,16.8,16.5,132.0,121.1,220.7,214.1),
                      upper=c(161.4,160.3,20.2,19.9,203.3,207.5,441.1,426.1))

View(newdatwomen)

pwomen<-ggplot(newdatwomen, aes(x=type, y=val, fill=group)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title = "Body size/composition in women", fill="Group", y= "Median (25th, 75th)") +
  scale_fill_manual(values = c("seagreen","indianred")) +
  scale_x_discrete(limits=c("Height","BMI","MRI VAT","MRI ASAT")) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), width=.05, alpha = 1) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"), 
                       axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 12),
                       axis.title.y = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 12, face = "bold", colour = "black"), 
                       legend.position = "right", legend.title = element_text(size = 14), legend.text = element_text(size = 14)) 
plot(pwomen)




##################################################################3

PreD_boys1 <- data.frame(parameters=c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"),
                        md=c(-0.184,0.258,0.250,0.381,0.298,0.286,0.453,0.023,-0.019,-0.654,-0.544,0.052,0.119), 
                        lower=c(-0.413,0.031,0.023,0.155,0.072,0.058,0.229,-0.205,-0.251,-0.874,-0.768,-0.175,-0.109), 
                        upper=c(0.044,0.485,0.478,0.606,0.524,0.513,0.676,0.253,0.211,-0.435,-0.32,0.281,0.347))


head(PreD_boys)

p1 <- ggplot(data=PreD_boys1, aes(x=parameters, y=md)) +
  geom_bar(stat="identity", color="black", fill="dodgerblue", width = 0.6, size=1) + coord_flip() +
  scale_x_discrete(limits=rev(c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"))) + 
  ggtitle("Cardiometabolic risk in men with prediabetes (vs NGT)") + 
  xlab("Parameters")+ ylab("Mean Z scores, 95% CI") +
  geom_hline(yintercept = 0.0, color="black", size=1) +
  theme_minimal()+
  theme(panel.border = element_blank(), plot.title = element_text(colour = "black", size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(colour = "black", size = 16), axis.text.x = element_text(colour = "black", size = 16),
        axis.title.y = element_text(colour = "black", size = 16), axis.text.y = element_text(colour = "black", size = 16),
        legend.position = "right", legend.title = element_text(colour = "black", size = 16), legend.text = element_text(colour = "black", size = 16),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=.05, alpha = 1, size=1) 
plot(p1)
ggsave(p1, filename = "PreDmen1.tiff", dpi = 600)



PreD_girls1 <- data.frame(parameters=c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"),
                         md=c(0.113,0.055,-0.112,0.062,0.031,-0.032,0.31,-0.117,-0.424,-0.785,-0.914,0.128,0.001), 
                         lower=c(-0.405,-0.352,-0.188,-0.363,-0.327,-0.265,0.013,-0.416,-0.725,-1.079,-1.19,-0.169,-0.273), 
                         upper=c(0.178,0.241,0.412,0.237,0.264,0.329,0.606,0.181,-0.122,-0.491,-0.630,0.425,0.276))


head(PreD_girls)

p2 <- ggplot(data=PreD_girls1, aes(x=parameters, y=md)) +
  geom_bar(stat="identity", color="black", fill="hotpink", width = 0.6, size=1) + coord_flip() +
  scale_x_discrete(limits=rev(c("Height","BMI","WHR","Skinfolds","MRI VAT","MRI ASAT","HOMA IR","HOMA β","IGI","Matsuda","DI","HDL","Trgly"))) + 
  ggtitle("Cardiometabolic risk in women with prediabetes (vs NGT)") + 
  xlab("Parameters")+ ylab("Mean Z scores, 95% CI") +
  geom_hline(yintercept = 0.0, color="black", size=1) +
  theme_minimal()+
  theme(panel.border = element_blank(), plot.title = element_text(colour = "black", size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(colour = "black", size = 16), axis.text.x = element_text(colour = "black", size = 16),
        axis.title.y = element_text(colour = "black", size = 16), axis.text.y = element_text(colour = "black", size = 16),
        legend.position = "right", legend.title = element_text(colour = "black", size = 16), legend.text = element_text(colour = "black", size = 16),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=.05, alpha = 1, size=1) 
plot(p2)
ggsave(p2, filename = "PreDwomen.jpg", dpi = 600)

MRI_figure2_v1 <- ggarrange(p1,p2,
                                      ncol = 2, nrow = 1, labels = c("a","b"), font.label = list(size = 12, color = "black", face = "bold"))
ggsave(MRI_figure2_v1, filename = "MRI_figure2_v1_25Apr25.jpeg", dpi = 300, width = 16, height = 6, units = "in")


#-----------------------------------------------------------------------------------------
# New graphs 
#-----------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Simulated data

data <- data.frame(
  Outcome = rep(paste0("Outcome", 1:10), each = 2),
  Exposure = rep(c("A", "B"), times = 10),
  Beta = c(0.12, -0.1, 0.22, -0.15, 0.05, -0.02, -0.18, 0.09, 
           0.07, -0.12, 0.03, 0.14, -0.05, 0.02, -0.09, 0.11,
           -0.06, 0.08, 0.15, -0.13)
)

data <- data %>%
  mutate(
    Outcome = factor(Outcome, levels = paste0("Outcome", 1:10)),
    x = as.numeric(Outcome),
    x_dodge = x + ifelse(Exposure == "A", -0.2, 0.2),
    Size = abs(Beta),
    stars = sample(c("ns", "*", "**", "***"), nrow(data), replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    stars_plot = ifelse(stars == "ns", "", stars),  # remove 'ns' from plot
    label_nudge = ifelse(Beta >= 0, 0.03, -0.03)  # right for +, left for -
  )

ggplot(data, aes(y = Beta, color = Exposure)) +
  geom_segment(aes(x = x_dodge, xend = x_dodge, y = 0, yend = Beta), size = 1) +
  geom_point(aes(x = x_dodge, size = Size)) +
  geom_text(aes(x = x_dodge, y = Beta + label_nudge, label = stars_plot), size = 5, show.legend = FALSE) +  # ← stars beside points
  scale_x_continuous(breaks = 1:10, labels = paste0("Outcome", 1:10)) +
  scale_size_continuous(range = c(2, 6), name = "|Beta|") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Outcome", y = "Beta Estimate", title = "Lollipop Plot with Significance Stars") +
  theme_minimal()

# real data

newplotdf <- read_xlsx("newplotdf.xlsx")
colnames(newplotdf)

newplotdf <- newplotdf %>%
  mutate(
    Trait = factor(Trait, levels = rev(c("Fasting glucose (mg/dl)","30-minute glucose (mg/dl)",
            "120-minute glucose (mg/dl)","Insulinogenic Index","Matsuda Index","Disposition Index",
            "Systolic BP (mm Hg)","Diastolic BP (mm Hg)","Cholesterol (mg/dl)","HDL-cholesterol (mg/dl)",
            "Triglycerides (mg/dl)","Total Adiponectin (ng/ml)","HMW adiponectin (ng/ml)",
            "Leptin (ng/ml)","Total leukocyte count (ng/ml)"))),
    x = as.numeric(Trait),
    x_dodge = x + ifelse(Exposure == "aSAT", -0.2, 0.2),
    Size = abs(Beta),
    stars_plot = ifelse(Stars == "ns", "", Stars),  # remove 'ns' from plot
    label_nudge = ifelse(Beta >= 0, 0.07, -0.07)  # right for +, left for -
  )

newplot1<-ggplot(newplotdf, aes(y = Beta, color = Exposure)) +
  geom_segment(aes(x = x_dodge, xend = x_dodge, y = 0, yend = Beta), size = 1.5) +
  geom_point(aes(x = x_dodge, size = Size)) +
  geom_text(aes(x = x_dodge, y = Beta + label_nudge, label = stars_plot), size = 5, show.legend = FALSE) +  # ← stars beside points
  scale_x_continuous(breaks = 1:length(levels(newplotdf$Trait)),
                     labels = levels(newplotdf$Trait)) +
  scale_size_continuous(name = "|Beta|") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "solid", size=1) +
  scale_color_manual(values = c("aSAT" = "royalblue", "VAT" = "darkorange")) +  # custom colors
  labs(x = "Trait", y = "Beta Estimate", title = "MRI Abdomen fat and metabolic profile at 18-years") +
  theme_classic() + theme(plot.title = element_text(size = 14, color="darkred", face = "bold"),
                          axis.text.x = element_text(size = 12, color="black", face = "bold"),
                          axis.title.x = element_text(size = 12,color="black", face = "bold"),
                          axis.text.y = element_text(size = 12, color="black", face = "bold"),
                          axis.title.y = element_blank(),
                          legend.text = element_text(size = 12, color="black", face = "bold"),
                          legend.title = element_text(size = 12, color="black", face = "bold")) +
  geom_vline(xintercept = seq(1.5, 14.5, by = 1), color = "grey80", linetype = "dotted", size=0.7)

ggsave(newplot1, filename="MRI_abdfat_plot1.jpeg", dpi = 300, width = 7, height = 6, units = "in")

#plot type 2
colnames(newplotdf)

bubplot1<-ggplot(newplotdf, aes(x = Exposure, y = Trait)) +
  geom_point(aes(size = abs(Beta), color = Beta)) +
  geom_text(aes(label = stars_plot), vjust = 0, size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  #scale_size_continuous(range = c(-1, 1)) +
  coord_fixed() +  # maintain equal spacing for rows/columns
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10, face = "bold"),
    axis.text.y = element_text(hjust = 1, color = "black", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "MRI Abd. fat & metabolic profile",
    x = "MRI Abdominal fat",
    y = "Trait",
    color = "Beta",
    size = "Beta"
  )

ggsave(bubplot1, filename="MRI_abdfat_plot2.jpeg", dpi = 300)

## Separately for sexes

newplotdfsex <- read_xlsx("newplotdf_sexsep.xlsx")
colnames(newplotdfsex)

newplotdfsex <- newplotdfsex %>%
  mutate(
    Trait = factor(Trait, levels = rev(c("Fasting glucose (mg/dl)","30-minute glucose (mg/dl)",
                                         "120-minute glucose (mg/dl)","Insulinogenic Index","Matsuda Index","Disposition Index",
                                         "Systolic BP (mm Hg)","Diastolic BP (mm Hg)","Cholesterol (mg/dl)","HDL-cholesterol (mg/dl)",
                                         "Triglycerides (mg/dl)","Total Adiponectin (ng/ml)","HMW adiponectin (ng/ml)",
                                         "Leptin (ng/ml)","Total leukocyte count (ng/ml)"))),
    x = as.numeric(Trait),
    x_dodge = x + ifelse(Exposure == "aSAT", -0.2, 0.2),
    Size = abs(Beta),
    stars_plot = ifelse(Stars == "ns", "", Stars),  # remove 'ns' from plot
    label_nudge = ifelse(Beta >= 0, 0.08, -0.08)  # right for +, left for -
  )

newplotsex1<-ggplot(newplotdfsex, aes(y = Beta, color = Exposure)) +
  geom_segment(aes(x = x_dodge, xend = x_dodge, y = 0, yend = Beta), size = 1.5) +
  geom_point(aes(x = x_dodge, size = Size, color = Exposure)) +
  geom_text(aes(x = x_dodge, y = Beta + label_nudge, label = stars_plot), size = 5, show.legend = FALSE) +  # ← stars beside points
  scale_x_continuous(breaks = 1:length(levels(newplotdfsex$Trait)),
                     labels = levels(newplotdfsex$Trait)) +
  scale_size_continuous(name = "|Beta|") +
  coord_flip() +
  facet_wrap(~Sex)+
  geom_hline(yintercept = 0, linetype = "solid", size=0.7) +
  scale_color_manual(values = c("aSAT" = "royalblue", "VAT" = "darkorange")) +  # custom colors
  labs(x = "Trait", y = "Beta Estimate", title = "MRI Abdomen fat and metabolic profile at 18-years") +
  theme_classic() + theme(plot.title = element_text(size = 14, color="darkred", face = "bold", hjust = 0.5),
                          axis.text.x = element_text(size = 12, color="black", face = "bold"),
                          axis.title.x = element_text(size = 12,color="black", face = "bold"),
                          axis.text.y = element_text(size = 12, color="black", face = "bold"),
                          axis.title.y = element_blank(),
                          legend.text = element_text(size = 12, color="black", face = "bold"),
                          legend.title = element_text(size = 12, color="black", face = "bold"),
                          strip.text = element_text(size = 12, face = "bold", color = "maroon")) +
  geom_vline(xintercept = seq(1.5, 14.5, by = 1), color = "grey80", linetype = "dotted", size=0.7)

plot(newplotsex1)

ggsave(newplotsex1, filename="MRI_abdfat_plot_sex.jpeg", dpi = 300, width = 10, height = 6, units = "in")

#############

library(ggplot2)

delb<-read_xlsx("delplotdf.xlsx")
delb$Trait<-factor(delb$Trait, levels = rev(c("Fasting glucose (mg/dl)","30-minute glucose (mg/dl)",
                                         "120-minute glucose (mg/dl)","Insulinogenic Index","Matsuda Index","Disposition Index",
                                         "Systolic BP (mm Hg)","Diastolic BP (mm Hg)","Cholesterol (mg/dl)","HDL-cholesterol (mg/dl)",
                                         "Triglycerides (mg/dl)","Total Adiponectin (ng/ml)","HMW adiponectin (ng/ml)",
                                         "Leptin (ng/ml)","Total leukocyte count (ng/ml)")))

segcol <- c("Fasting glucose (mg/dl)" = "red3", "30-minute glucose (mg/dl)" = "red3", "120-minute glucose (mg/dl)" = "red3", 
            "Insulinogenic Index" = "red3","Matsuda Index" = "royalblue", "Disposition Index" = "royalblue", "Systolic BP (mm Hg)" = "red3", 
            "Diastolic BP (mm Hg)" = "red3",   "Cholesterol (mg/dl)" = "red3", "HDL-cholesterol (mg/dl)" = "royalblue",
            "Triglycerides (mg/dl)"="red3","Total Adiponectin (ng/ml)"="royalblue", "HMW adiponectin (ng/ml)"="royalblue", "Leptin (ng/ml)"="red3",
            "Total leukocyte count (ng/ml)"="red3")

delb <- delb %>%
  mutate(hjust_star = ifelse(Delta_beta < 0, 1.4, -0.4))

del_all<-ggplot(delb, aes(x = Trait, y = Delta_beta)) +
  # lollipop sticks
  geom_segment(aes(xend = Trait, y = 0, yend = Delta_beta), color = segcol, size=1.5) +
  # points with custom color
  geom_point(aes(color = Trait, size =abs(Delta_beta))) +
  scale_color_manual(values = segcol) +
  # significance stars
  geom_text(aes(label = Stars, hjust = hjust_star), size = 5, color="black") +
  coord_flip() + ylim(-0.1,0.9)+
  labs(x = "Trait", y = "Delta Beta (SAT-VAT)", title = "MRI Abdomen fat and metabolic profile at 18-years")+
  geom_hline(yintercept = 0, linetype = "solid", size=0.7)+
  theme_bw() + theme(plot.title = element_text(size = 14, color="darkred", face = "bold", hjust = 0.5),
                          axis.text.x = element_text(size = 12, color="black", face = "bold"),
                          axis.title.x = element_text(size = 12,color="black", face = "bold"),
                          axis.text.y = element_text(size = 12, color="black", face = "bold"),
                          axis.title.y = element_blank(),
                          legend.position = "none" ) +
  geom_vline(xintercept = seq(1.5, 14.5, by = 1), color = "grey80", linetype = "dotted", size=0.7)

ggsave(del_all, filename="MRI_abdfat_deltaplot.jpeg", dpi = 300, width = 8, height = 6, units = "in")


# Sex sep

delbsex<-read_xlsx("deltadf_sex.xlsx")

delbsex$Trait<-factor(delbsex$Trait, levels = rev(c("Fasting glucose (mg/dl)","30-minute glucose (mg/dl)",
                                              "120-minute glucose (mg/dl)","Insulinogenic Index","Matsuda Index","Disposition Index",
                                              "Systolic BP (mm Hg)","Diastolic BP (mm Hg)","Cholesterol (mg/dl)","HDL-cholesterol (mg/dl)",
                                              "Triglycerides (mg/dl)","Total Adiponectin (ng/ml)","HMW adiponectin (ng/ml)",
                                              "Leptin (ng/ml)","Total leukocyte count (ng/ml)")))

segcol <- c("Fasting glucose (mg/dl)" = "red3", "30-minute glucose (mg/dl)" = "red3", "120-minute glucose (mg/dl)" = "red3", 
            "Insulinogenic Index" = "red3","Matsuda Index" = "royalblue", "Disposition Index" = "royalblue", "Systolic BP (mm Hg)" = "red3", 
            "Diastolic BP (mm Hg)" = "red3",   "Cholesterol (mg/dl)" = "red3", "HDL-cholesterol (mg/dl)" = "royalblue",
            "Triglycerides (mg/dl)"="red3","Total Adiponectin (ng/ml)"="royalblue", "HMW adiponectin (ng/ml)"="royalblue", "Leptin (ng/ml)"="red3",
            "Total leukocyte count (ng/ml)"="red3")

delbsex <- delbsex %>%
  mutate(hjust_star = ifelse(Delta_beta < 0, 1.5, -0.5))


del_sex<-ggplot(delbsex, aes(x = Trait, y = Delta_beta)) +
  # lollipop sticks
  geom_segment(aes(xend = Trait, y = 0, yend = Delta_beta, color = Trait), size=1.5) +
  # points with custom color
  geom_point(aes(color = Trait, size =abs(Delta_beta))) +
  scale_color_manual(values = segcol) +
  # significance stars
  geom_text(aes(label = Stars, hjust = hjust_star), size = 5, color="red") +
  coord_flip() +ylim(-0.3,0.9)+
  labs(x = "Trait", y = "Delta Beta (SAT-VAT)", title = "MRI Abdomen fat and metabolic profile at 18-years")+
  geom_hline(yintercept = 0, linetype = "solid", size=0.7)+
  facet_wrap(~Sex)+
  theme_bw() + theme(plot.title = element_text(size = 14, color="darkred", face = "bold", hjust = 0.5),
                     axis.text.x = element_text(size = 12, color="black", face = "bold"),
                     axis.title.x = element_text(size = 12,color="black", face = "bold"),
                     axis.text.y = element_text(size = 12, color="black", face = "bold"),
                     axis.title.y = element_text(size = 12, color="black", face = "bold"),
                     legend.position = "none",
                     strip.text = element_text(size = 12, face = "bold", color = "maroon")) +
  geom_vline(xintercept = seq(1.5, 14.5, by = 1), color = "grey80", linetype = "dotted", size=0.7)

ggsave(del_sex, filename="MRI_abdfat_deltaplotsex.jpeg", dpi = 300, width = 12, height = 6, units = "in")


## New attempt

delb <- delb %>%
  mutate(hjust_star = ifelse(Delta_beta < 0, 1, 0))

del_allv2<-ggplot(delb, aes(x= Trait, y = Delta_beta, fill = Trait)) +
  geom_bar(stat="identity", color="black", width = 0.7)+
  scale_fill_manual(values = segcol) +
  # significance stars
  geom_text(aes(label = Stars, hjust = hjust_star), size = 5, color="black") +
  ylim(-0.1,0.9)+
  labs(x = "Trait", y = "Delta Beta (SAT-VAT)", title = "MRI Abdomen fat and metabolic profile at 18-years")+
  geom_vline(xintercept = 0, linetype = "solid", size=0.7)+
  theme_bw() + theme(plot.title = element_text(size = 14, color="darkred", face = "bold", hjust = 0.5),
                     axis.text.x = element_text(size = 12, color="black", face = "bold"),
                     axis.title.x = element_text(size = 12,color="black", face = "bold"),
                     axis.text.y = element_text(size = 12, color="black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "none" ) 
  #geom_vline(xintercept = seq(1.5, 14.5, by = 1), color = "grey80", linetype = "dotted", size=0.7)

ggsave(del_all, filename="MRI_abdfat_deltaplot.jpeg", dpi = 300, width = 8, height = 6, units = "in")



plivmeandiff<-ggplot(df_livmeandiff, aes(x=time, y=meandiff)) +
  geom_bar(stat="identity",position="dodge", color="black",fill="pink", width = 0.5, lwd=1) + #coord_flip() +
  scale_x_discrete(limits=c("28wks","34wks","Growth(28-34wks)","18Y")) + 
  geom_hline(yintercept = 0, lwd=1, linetype="dashed") +
  ggtitle("Mean difference") + 
  ylab("Standardized Mean difference (SD), 95% CI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
                     axis.title.x = element_blank(), axis.text.x = element_text(face = "bold", colour = "black", size = 16, angle = 45),
                     axis.title.y = element_text(face = "bold", colour = "black", size = 16), axis.text.y = element_text(face = "bold", colour = "black", size = 16),
                     legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(face = "bold", colour = "black", size = 16),
                     panel.background = element_rect(fill = "linen"))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width=.05, alpha = 1, lwd=1) 



#------------------------------------------------------
# Metabolic syndrome - IDF
#------------------------------------------------------

library(haven)
library(foreign)

#mridf<-read_spss("PMNS_18yr_MRI_DATA_FINAL_master_file.sav")
mridf<-read_sav("PMNS_18y_MRI_test.sav")
dim(mridf)
colnames(mridf)
str(mridf$sex)
mridf$sex<-factor(mridf$sex)

# Check central obesity first
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

mridf <- mridf %>%
  mutate(group = recode(gly_st_18y_new,
                            "1" = "NGT",
                            "2" = "Prediabetes"
                            ))


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

pred2logit<-glm(gly_st_18y_new~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridf, sex==1))
summary(pred2logit)

pred3logit<-glm(gly_st_18y_new~ZRE_VAT_Combined+ZRE_SAT_combined, family = "binomial", data = subset(mridf, sex==2))
summary(pred3logit)

##New approach for metS modelling give the small numbers - Firth penlised logistic regression

install.packages("logistf")   # once
library(logistf)

mets2logit <- logistf(metS_idf~ZRE_VAT_Combined+ZRE_SAT_combined,  data = mridf)
summary(mets2logit)

exp(cbind(OR = mets2logit$coef,
          lower = mets2logit$ci.lower,
          upper = mets2logit$ci.upper))

#                          OR       lower      upper
#(Intercept)      0.007244044 0.002207831 0.01732245
#ZRE_VAT_Combined 1.569336933 0.848217240 2.93204584
#ZRE_SAT_combined 2.722640827 1.805959050 4.26394447

###########################


lm1<-lm(log(nefa_fast_E3_C80)~ZRE_VAT_Combined+ZRE_SAT_combined, data = mridf)
summary(lm1)


# Barplots for ratios - SAT:VAT

dfratio<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"),
                    sex=rep(c("Male","Female"), each=8),
                    value=c(4.2,3.2,1.2,2.8,2.9,1.5,1.2,1.4,4.8,3.9,1.9,4.9,5.4,2.8,2.2,2.3))

head(dfratio)
dfratio$Study<-factor(dfratio$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"))

library(ggplot2)

SATVATbar<-ggplot(dfratio, aes(x=Study, y=value, fill = sex))+
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  scale_fill_manual(values = c("pink","lightblue"))+
  ggtitle("SAT/VAT ratio in South Asian Indians (Native+Migrants)") + 
  ylab("SAT/VAT ratio") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))



library(ggplot2)

## Bar graphs in males

dfratioM<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"),
                    value=c(4.2,3.2,1.2,2.8,2.9,1.5,1.04,1.2,1.4))

head(dfratioM)
dfratioM$Study<-factor(dfratioM$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"))

SATVATbarM<-ggplot(dfratioM, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill="cornflowerblue") +
  ggtitle("SAT/VAT ratio in South Asian Indians (Native+Migrants)") + 
  ylab("SAT/VAT ratio") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))



dfbmiM<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"),
                   value=c(12.7,15.2,19.0,23.9,24.8,27.7,22.0,24.2,26.0))

head(dfbmiM)
dfbmiM$Study<-factor(dfbmiM$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"))

BMIbarM<-ggplot(dfbmiM, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill="cornflowerblue") +
  ggtitle("BMI in South Asian Indians (Native+Migrants)") + 
  ylab("BMI (kg/m2)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))


dfheightM<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"),
                   value=c(50.3,105.2,169.6,178,173,174,165,166,170))

head(dfheightM)
dfheightM$Study<-factor(dfheightM$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","CRISIS (45y)","RMAnjana (45y)","AKanaya (55y)"))

HtbarM<-ggplot(dfheightM, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill="cornflowerblue") +
  ggtitle("Height in South Asian Indians (Native+Migrants)") + 
  ylab("Height (cm)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))


## Bar graphs in females

dfratioF<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"),
                     value=c(4.8,3.9,1.9,4.9,5.4,2.8,2.2,2.3))

head(dfratioF)
dfratioF$Study<-factor(dfratioF$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"))

library(ggplot2)

SATVATbarF<-ggplot(dfratioF, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width=0.7, colour = "black", fill="hotpink1") +
  ggtitle("SAT/VAT ratio in South Asian Indians (Native+Migrants)") + 
  ylab("SAT/VAT ratio") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))


dfbmiF<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"),
                     value=c(12.0,15.4,18.7,22.4,24.5,26.3,25.7,26.4))

head(dfbmiF)
dfbmiF$Study<-factor(dfbmiF$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"))

library(ggplot2)

BMIbarF<-ggplot(dfbmiF, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width=0.7, colour = "black", fill="hotpink1") +
  ggtitle("BMI in South Asian Indians (Native+Migrants)") + 
  ylab("BMI (kg/m2)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))



dfhtF<-data.frame(Study=c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"),
                   value=c(50,105,157,165,163,162,154,157))

head(dfhtF)
dfhtF$Study<-factor(dfhtF$Study, levels = c("GUSTO (birth)", "GUSTO (4.5y)", "PMNS (18y)", "LThomas (23y)","LThomas (24y)","SAnand (37y)","RMAnjana (45y)","AKanaya (55y)"))

HtbarF<-ggplot(dfhtF, aes(x=Study, y=value))+
  geom_bar(stat = "identity", width=0.7, colour = "black", fill="hotpink1") +
  ggtitle("Height in South Asian Indians (Native+Migrants)") + 
  ylab("Height (cm)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(face = "bold", colour = "black", size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", colour = "black", size = 14), 
        axis.text.y = element_text(face = "bold", colour = "black", size = 14),
        legend.title = element_text(face = "bold", colour = "black", size = 14), 
        legend.text = element_text(face = "bold", colour = "black", size = 14))



df.f <- data.frame(
  Study = c("GUSTO", "GUSTO", "PMNS", "UKBB.SAS1","UKBB.SAS2","MolSHARE","CURES","MASALA"),
  Age = c(0, 4.5, 18, 23, 24, 37, 45, 55),
  BMI = c(12.0,15.4,18.7,22.4,24.5,26.3,25.7,26.4),
  SAT.VAT_Ratio = c(4.8,3.9,1.9,4.9,5.4,2.8,2.2,2.3)
)


df.m <- data.frame(
  Study = c("GUSTO", "GUSTO", "PMNS", "UKBB.SAS1","UKBB.SAS2","MolSHARE","CRISIS","CURES","MASALA"),
  Age = c(0, 4.5, 18, 23, 24, 37, 45, 45, 55),
  BMI = c(12.7,15.2,19.0,23.9,24.8,27.7,22.0,24.2,26.0),
  SAT.VAT_Ratio = c(4.2,3.2,1.2,2.8,2.9,1.5,1.04,1.2,1.4)
)



#----------------------------------------------------------------------------------------------------------
# Multiple regression models for SAT/VAT as exposures ########
#----------------------------------------------------------------------------------------------------------

dim(mridf)
colnames(mridf)

cor.test(mridf$ZRE_SAT_combined, mridf$ZRE_VAT_Combined)
cor.test(mridf$tot_VAT_Area_18yr, mridf$tot_ASAT_area_18yr)

mridf$TG_HDL<-mridf$ctg_18y/mridf$chdl_18y
mridf$cldl_18y<-(mridf$cchol_18y-mridf$chdl_18y-(mridf$ctg_18y/5))

# Define your vector of y-variable names
y_vars <- c("cgluf_18y","cglu30_18y","cglu2hr_18y","cinsf_18y","cins30_18y","cins2hr_18y", "homa_ir_18", "homa_beta_18",
             "homa_sens_18","disp_index_18","cIGI_18y","cmat_18y","cdyn_DI_18y","csyst_18y","cdiast_18y","cpulse_18y",
             "ctg_18y","cchol_18y","chdl_18y","cldl_18y","TG_HDL","cleptin_18y","totaladip_18y","hmwadip_18y","ccrp_18y","cwbc_18y"
             )  #list if y vars

mridf <- mridf %>%
  mutate(across(
    all_of(y_vars),
    ~ ifelse(. < 0, NA, .)
  ))

# Loop over each y-variable

for (y in y_vars) {
  
  y_log <- paste0("log_", y)
  
  # Create log variable safely
  mridf[[y_log]] <- ifelse(mridf[[y]] > 0, log(mridf[[y]]), NA)
  
  model1 <- lm(
    as.formula(paste(y_log, "~ cage18y + sex + cht_18y")),
    data = mridf,
    na.action = na.exclude
  )
  
  std_residuals <- rstandard(model1)
  
  mridf[[paste0(y, "_Zresidht")]] <- NA
  mridf[[paste0(y, "_Zresidht")]][as.numeric(names(std_residuals))] <- std_residuals
}


outcomes  <- c("cgluf_18y_Zresid", "cglu30_18y_Zresid", "cglu2hr_18y_Zresid","cinsf_18y_Zresid","cins30_18y_Zresid","cins2hr_18y_Zresid",
               "homa_ir_18_Zresid","homa_beta_18_Zresid","homa_sens_18_Zresid","disp_index_18_Zresid",
               "cIGI_18y_Zresid","cmat_18y_Zresid","cdyn_DI_18y_Zresid","csyst_18y_Zresid","cdiast_18y_Zresid","cpulse_18y_Zresid",
               "ctg_18y_Zresid","cchol_18y_Zresid","chdl_18y_Zresid","cldl_18y_Zresid","TG_HDL_Zresid",
               "cleptin_18y_Zresid","totaladip_18y_Zresid","hmwadip_18y_Zresid","cwbc_18y_Zresid","ccrp_18y_Zresid")
exposures <- c("ZRE_SAT_combined", "ZRE_VAT_Combined")
covars    <- c("age", "sex")   # optional; can be NULL

outcomes1  <- c("cgluf_18y_Zresidht", "cglu30_18y_Zresidht", "cglu2hr_18y_Zresidht","cinsf_18y_Zresidht","cins30_18y_Zresidht","cins2hr_18y_Zresidht",
               "homa_ir_18_Zresidht","homa_beta_18_Zresidht","homa_sens_18_Zresidht","disp_index_18_Zresidht",
               "cIGI_18y_Zresidht","cmat_18y_Zresidht","cdyn_DI_18y_Zresidht","csyst_18y_Zresidht","cdiast_18y_Zresidht","cpulse_18y_Zresidht",
               "ctg_18y_Zresidht","cchol_18y_Zresidht","chdl_18y_Zresidht","cldl_18y_Zresidht","TG_HDL_Zresidht",
               "cleptin_18y_Zresidht","totaladip_18y_Zresidht","hmwadip_18y_Zresidht","cwbc_18y_Zresidht","ccrp_18y_Zresidht")


#2️⃣ Core function: fit model + extract everything

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

#3️⃣ Loop over all outcomes (clean + safe)

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

#4️⃣ Write final summary CSV

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


sex_groups <- list(
  Male   = mridf %>% dplyr::filter(sex == 1),
  Female = mridf %>% dplyr::filter(sex == 2)
)

results_all <- list()

for (sx in c(1,2)) {
  
  dsub <- if (sx == 1) mridf_male else mridf_female
  sex_lab <- ifelse(sx == 1, "Male", "Female")
  
  for (out in outcomes) {
    
    res <- run_two_exposure_lm_sex(
      data = dsub,
      outcome = out,
      exp1 = "ZRE_SAT_combined",
      exp2 = "ZRE_VAT_Combined",
      covars = "ZRE_57",
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
  "MRI_MLRA_sex_stratified_wcadj.csv",
  row.names = FALSE
)

####### Add BMI and WC as covariates at each time ##########

#Modified the above codes to adjust new covariates, one at a time



####### SankeyMatic text files ######

col_fun <- function(b, p)
  ifelse(is.na(b), "#BDBDBD",
         ifelse(p < 0.05 & b > 0, "#ff4f4f",
                ifelse(p < 0.05 & b < 0, "#4fa4ff", "#94a6b4")))

make_txt <- function(sex_lab, file) {
  
  d <- results_sex_strat[results_sex_strat$Sex == sex_lab, ]
  
  txt <- c(
    paste("aSAT", paste0("[", round(abs(d$Beta_exp1), 3), "]"),
          d$Outcome, col_fun(d$Beta_exp1, d$P_exp1)),
    
    paste("VAT", paste0("[", round(abs(d$Beta_exp2), 3), "]"),
          d$Outcome, col_fun(d$Beta_exp2, d$P_exp2))
  )
  
  writeLines(txt, file)
}


make_txt("Male",   "links_male.txt")
make_txt("Female", "links_female.txt")


#create overlaying density plots
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


####### correlogram #######

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

