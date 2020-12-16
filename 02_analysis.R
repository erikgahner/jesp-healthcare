###
## Article:   Personal politics? 
##            Healthcare policies, personal experiences and government attitudes 
## 
##            Journal of European Social Policy
##
##            Erik Gahner Larsen
##            erikgahner@gmail.com
##
##        
## Data:      Health and Health Care - ISSP 2011: https://doi.org/10.4232/1.12252
##            WDI: https://datacatalog.worldbank.org/dataset/world-development-indicators
##
###

library("tidyverse")
library("mice")
library("plyr")
library("stargazer")
library("conflicted")
library("grid")
library("gridExtra")
library("lme4")
library("interplot")
library("sjstats")
library("insight")
library("xtable")
library("LMERConvenienceFunctions")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("mutate", "dplyr")

# Load theme
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)



# Load data
issp <- read_csv("data_issp_healthcare.csv", # Use 01_create_data.R to create the dataset
                 col_types = cols(
                   gini = col_double(),
                   beds = col_double()
                 ))


# Impute data
issp_imp <- issp %>%
  select(name, cntry, year, moregov, male, age, age2100, edu, married, employed, income, healthstatus, insurance, occ, the, phe, phe100, gini, gdp, beds, oop, sat_doctor, sat_hospital, badexp) %>%
  mice(., seed = 1)

issp <- complete(issp_imp)

issp <- ddply(issp, 
              "cntry", 
              mutate, 
              sat_doctor_c = (sat_doctor - mean(sat_doctor, na.rm=TRUE))/sd(sat_doctor, na.rm=TRUE),
              sat_hospital_c = (sat_hospital - mean(sat_hospital, na.rm=TRUE))/sd(sat_hospital, na.rm=TRUE),
              badexp_c = (badexp - mean(badexp, na.rm=TRUE))/sd(badexp, na.rm=TRUE)
)

issp_phil <- issp
issp <- issp %>%
  filter(cntry != "PH")

corr_hospital <- issp %>% 
  group_by(cntry) %>%
  summarise(
    healthexp = mean(phe100, na.rm=TRUE),
    correlation = cor(sat_hospital, moregov, use = "pairwise.complete.obs"),
    .groups = "drop")

corr_doctor <- issp %>% 
  group_by(cntry) %>%
  summarise(
    healthexp = mean(phe100, na.rm=TRUE),
    correlation = cor(sat_doctor, moregov, use = "pairwise.complete.obs"),
    .groups = "drop")

corr_badexp <- issp %>% 
  group_by(cntry) %>%
  summarise(
    healthexp = mean(phe100, na.rm=TRUE),
    correlation = cor(badexp, moregov, use = "pairwise.complete.obs"),
    .groups = "drop")

badexp_hospital <- issp %>% 
  group_by(cntry) %>%
  summarise(
    healthexp = mean(phe100, na.rm=TRUE),
    correlation = cor(badexp, sat_hospital, use = "pairwise.complete.obs"),
    .groups = "drop") 

mean(badexp_hospital$correlation)
cor(badexp_hospital$healthexp, badexp_hospital$correlation)

corr_experience <- issp %>% 
  group_by(cntry) %>%
  summarise(
    `Hospital evaluation` = cor(moregov, sat_hospital, use = "pairwise.complete.obs"),
    `Doctor evaluation` = cor(moregov, sat_doctor, use = "pairwise.complete.obs"),
    `Objective experience` = cor(moregov, badexp, use = "pairwise.complete.obs"),
    .groups = "drop") %>%
  gather(outcome, correlation, `Hospital evaluation`, `Doctor evaluation`, `Objective experience`) 

ggplot(corr_experience, aes(x=correlation, fill = outcome)) +
  geom_vline(xintercept = median(corr_experience$correlation), linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "gray") +
  geom_dotplot(binpositions = "all", stackgroups = TRUE) +
  scale_fill_manual(values = c("black", "grey", "white")) + 
  theme(legend.justification=c(0,1), legend.position=c(0.00,0.98),
        legend.background = element_rect(colour = NA, fill = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "",
       x = "Correlation") +
  annotate("text", x = -0.01, y = 0.97, size = 4, label = "-") +
  annotate("text", x = 0.01, y = 0.97, size = 4, label = "+") 

ggsave("fig-correlations.pdf", width = 7, height = 4)

mean(corr_experience$correlation)

fig_agg_1 <- ggplot(corr_hospital, aes(healthexp, correlation)) + 
  ylab(expression(paste(rho, " (Hospital evaluation, Government attitude)"))) +
  xlab("") +
  geom_smooth(method="lm", se=FALSE, colour = "black") +
  geom_hline(yintercept=0, colour="gray70") +
  geom_text(size=3, aes(label=cntry)) + 
  xlab("") +
  scale_x_continuous(limits=c(0.45, 0.85), breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) +
  theme(legend.position="none") 

fig_agg_2 <- ggplot(corr_doctor, aes(healthexp, correlation)) + 
  ylab(expression(paste(rho, " (Doctor evaluation, Government attitude)"))) +
  xlab("") +
  geom_smooth(method="lm", se=FALSE, colour = "black") +
  geom_hline(yintercept=0, colour="gray70") +
  geom_text(size=3, aes(label=cntry)) + 
  xlab("Public health care (%)") +
  scale_x_continuous(limits=c(0.45, 0.85), breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) +
  theme(legend.position="none") 

fig_agg_3 <- ggplot(corr_badexp, aes(healthexp, correlation)) + 
  ylab(expression(paste(rho, " (Objective experience, Government attitude)"))) +
  xlab("") +
  geom_hline(yintercept=0, colour="gray70") +
  geom_smooth(method="lm", se=FALSE, colour = "black") +
  geom_text(size=3, aes(label=cntry)) + 
  xlab("") +
  scale_x_continuous(limits=c(0.45, 0.85), breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) +
  theme(legend.position="none") 

pdf("fig-agg-main.pdf", width = 8, height = 4)
grid.arrange(fig_agg_1, fig_agg_2, fig_agg_3, ncol = 3)
dev.off()

reg_0_c <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + (1 | cntry), data = issp)
reg_1_c <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_hospital_c*phe100 + (1 | cntry), data = issp) 
reg_2_c <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_doctor_c*phe100 + (1 | cntry), data = issp) 
reg_3_c <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + badexp_c*phe100 + (1 | cntry), data = issp)

stargazer(reg_0_c, reg_1_c, reg_2_c, reg_3_c,
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits= 2,
          model.numbers = FALSE,
          font.size = "footnotesize",
          notes.align = "l",
          star.cutoffs = c(0.05, 0.01, 0.001),
          float.env = "sidewaystable",
          out = "tab-mainregression.html",
          keep = c("male", "age", "age2100", "married", "employed", "income",
                   "healthstatus", "the", "phe100", "sat_hospital_c", "phe100:sat_hospital_c",
                   "sat_doctor_c", "phe100:sat_doctor_c", "badexp_c", "phe100:badexp_c"), 
          covariate.labels =c("Male", "Age", "Age (sq)", "Married",
                              "Employed", "Income", "Subjective health", "Health exp. (%GDP)", "PHE (%)",
                              "Hospital evaluation", "Hospital evaluation * PHE",
                              "Doctor evaluation", "Doctor evaluation * PHE",
                              "Objective experience", "Objective experience * PHE"),
          column.labels=c("(1)", "(2)", "(3)", "(4)"),
          add.lines=c(list(c("Education FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Occupation FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Health care insurance FE", "Yes", "Yes", "Yes", "Yes")),
                      
                      list(c(c("ICC"), round(c(as.numeric(icc(reg_0_c)), 
                           as.numeric(icc(reg_1_c)), 
                           as.numeric(icc(reg_2_c)),
                           as.numeric(icc(reg_3_c))),4))),
                      
                      list(c(c("Within country variance"), round(c(as.numeric(get_variance(reg_0_c, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_1_c, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_2_c, component = c("residual"))),
                                                                   as.numeric(get_variance(reg_3_c, component = c("residual")))),4))),
                      
                      
                      list(c(c("Between country variance"), round(c(as.numeric(get_variance(reg_0_c, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_1_c, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_2_c, component = c("random"))),
                                                                    as.numeric(get_variance(reg_3_c, component = c("random")))),4))),
                      
                      list(c("Countries", 30, 30, 30, 30))
                      
                      ),

          model.names=FALSE,
          type = "text"
)

reg_marg_1 <- interplot(m = reg_1_c, var1 = "sat_hospital_c", var2 = "phe100", plot=FALSE, ci = 0.95)
reg_marg_2 <- interplot(m = reg_2_c, var1 = "sat_doctor_c", var2 = "phe100", plot=FALSE, ci = 0.95)
reg_marg_3 <- interplot(m = reg_3_c, var1 = "badexp_c", var2 = "phe100", plot=FALSE, ci = 0.95)

number_lower <- min(c(min(reg_marg_1$ub), min(reg_marg_2$ub), min(reg_marg_3$ub)))
number_upper <- max(c(max(reg_marg_1$lb), max(reg_marg_2$lb), max(reg_marg_3$lb)))

plot_marg_1 <- ggplot(reg_marg_1, aes(x = phe100)) +
  geom_hline(yintercept=0, colour="gray60") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  labs(title = "(A) Hospital evaluation") +
  scale_y_continuous("Marginal effect on government attitude", limits = c(number_lower, number_upper)) +
  scale_x_continuous("", limits=c(0.45, 0.85),breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) 

plot_marg_2 <- ggplot(reg_marg_2, aes(x = phe100)) +
  geom_hline(yintercept=0, colour="gray60") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  labs(title = "(B) Doctor evaluation") +
  scale_y_continuous("", limits = c(number_lower, number_upper)) +
  scale_x_continuous("Public health care (%)", limits=c(0.45, 0.85),breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

plot_marg_3 <- ggplot(reg_marg_3, aes(x = phe100)) +
  geom_hline(yintercept=0, colour="gray60") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  labs(title = "(C) Objective experience") +
  scale_y_continuous("", limits = c(number_lower, number_upper)) +
  scale_x_continuous("", limits=c(0.45, 0.85),breaks=c(0.45,0.55,0.65,0.75,0.85), labels=c("45","55","65","75","85")) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) 

pdf("fig-marg-main.pdf", width = 8, height = 4)
grid.arrange(plot_marg_1, plot_marg_2, plot_marg_3, ncol = 3)
dev.off()


### APPENDIX

# Appendix D

issp %>% 
  filter(!is.na(moregov)) %>%
  group_by(name) %>%
  summarise(n = n(), 
            moregov_mean = mean(moregov), 
            moregov_sd = sd(moregov), 
            sat_hospital_mean = mean(sat_hospital), 
            sat_hospital_sd = sd(sat_hospital), 
            sat_doctor_mean = mean(sat_doctor), 
            sat_doctor_sd = sd(sat_doctor), 
            badexp_mean = mean(badexp), 
            badexp_sd = sd(badexp), 
            healthexp = mean(phe),
            .groups = "drop"
  ) %>%
  xtable() %>%
  print(type="html", file="tab-descriptive.html")

issp %>% filter(phe100 == min(phe100)) %>% group_by(name) %>% summarise(mean(phe100), .groups = "drop")
issp %>% filter(phe100 == max(phe100)) %>% group_by(name) %>% summarise(mean(phe100), .groups = "drop")

phe_avg_df <- issp %>% group_by(name) %>% summarise(phe_avg = mean(phe100), .groups = "drop") 
mean(phe_avg_df$phe_avg)
sd(phe_avg_df$phe_avg)
median(phe_avg_df$phe_avg)

issp %>% 
  group_by(name) %>%
  summarise(phe = mean(phe),
            the = mean(the),
            gini = mean(gini),
            beds = mean(beds),
            oop = mean(oop),
            gdp = mean(gdp),
            .groups = "drop"
  ) %>%
  xtable() %>%
  print(type="html", file="tab-descriptive-cntry.html")

issp %>% 
  select(-c(ends_with("_c"), "year", "phe100", "age2100", "edu", "insurance", "occ")) %>%
  select(moregov, sat_hospital, sat_doctor, badexp, everything()) %>%
  stargazer(out="tab-descriptive-agg.htm",
            covariate.labels = c("Government attitudes", "Hospital evaluation", "Doctor evaluation", "Objective experience",
                                 "Male", "Age", "Married", "Employed", "Income", "Subjective health", "Health exp. (%GDP)", "PHE", "Inequality", "GDP (log)", "Hospital beds", "OOP" 
          ),
          
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, ISSP",
            digits = 2, type="text")


# Appendix E

pdf('fig-govattitudes.pdf', height=11, width=12)
ggplot(issp, aes(x = moregov)) + 
  geom_bar(colour = "black", fill = "gray70") +
  ylab("Frequency") +
  xlab("Support for government health care") +
  scale_x_discrete(limits = c(1,5),breaks=c(1:5), labels=c("Low","","","","High")) +
  facet_wrap(~ name) 
dev.off()

pdf('fig-sat_hospital.pdf', height=11, width=12)
ggplot(data=issp, aes(x=sat_hospital)) + 
  geom_bar(colour = "black", fill = "gray70") +
  facet_wrap(~ name) +
  ylab("Frequency") +
  scale_x_discrete("Hospital evaluation", limits = c(1,7), breaks=c(1:7), labels=c("Low","","","","","","High")) 
dev.off()

pdf('fig-sat_doctor.pdf', height=11, width=12)
ggplot(data=issp, aes(x=sat_doctor)) + 
  geom_bar(colour = "black", fill = "gray70") +
  facet_wrap(~ name) +
  ylab("Frequency") +
  scale_x_discrete("Doctor evaluation", limits = c(1,7), breaks=c(1:7), labels=c("Low","","","","","","High")) 
dev.off()

pdf('fig-badexp.pdf', height=11, width=12)
ggplot(data=issp, aes(x=badexp)) + 
  geom_bar(colour = "black", fill = "gray70") +
  facet_wrap(~ name) +
  ylab("Frequency") +
  scale_x_discrete("Objective experience", limits = c(0,1), breaks=c(0, 1), labels=c("No", "Yes")) 
dev.off()


reg_0_c_macro <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + gini + gdp + beds + oop + phe100 + (1 | cntry), data = issp)
reg_1_c_macro <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + gini + gdp + beds + oop + phe100 + sat_hospital_c*phe100 + (1 | cntry), data = issp) 
reg_2_c_macro <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + gini + gdp + beds + oop + phe100 + sat_doctor_c*phe100 + (1 | cntry), data = issp) 
reg_3_c_macro <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + gini + gdp + beds + oop + phe100 + badexp_c*phe100 + (1 | cntry), data = issp)

stargazer(reg_0_c_macro, reg_1_c_macro, reg_2_c_macro, reg_3_c_macro,
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits= 2,
          model.numbers = FALSE,
          font.size = "footnotesize",
          notes.align = "l",
          star.cutoffs = c(0.05, 0.01, 0.001),
          float.env = "sidewaystable",
          out = "tab-mainregression-macro.html",
          keep = c("male", "age", "age2100", "married", "employed", "income",
                   "healthstatus", "the", "gini", "gdp", "beds", "oop", "phe100", "sat_hospital_c", "phe100:sat_hospital_c",
                   "sat_doctor_c", "phe100:sat_doctor_c", "badexp_c", "phe100:badexp_c"), 
          covariate.labels =c("Male", "Age", "Age (sq)", "Married",
                              "Employed", "Income", "Subjective health", "Health exp. (%GDP)", "Inequality", "GDP (log)", "Hospital beds", "OOP", "PHE (%)",
                              "Hospital evaluation", "Hospital evaluation * PHE",
                              "Doctor evaluation", "Doctor evaluation * PHE",
                               "Objective experience", "Objective experience * PHE"),
          column.labels=c("(1)", "(2)", "(3)", "(4)"),
          add.lines=c(list(c("Education FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Occupation FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Health care insurance FE", "Yes", "Yes", "Yes", "Yes")),
                      
                      list(c(c("ICC"), round(c(as.numeric(icc(reg_0_c_macro)), 
                                               as.numeric(icc(reg_1_c_macro)), 
                                               as.numeric(icc(reg_2_c_macro)),
                                               as.numeric(icc(reg_3_c_macro))),4))),
                      
                      list(c(c("Within country variance"), round(c(as.numeric(get_variance(reg_0_c_macro, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_1_c_macro, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_2_c_macro, component = c("residual"))),
                                                                   as.numeric(get_variance(reg_3_c_macro, component = c("residual")))),4))),
                      
                      
                      list(c(c("Between country variance"), round(c(as.numeric(get_variance(reg_0_c_macro, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_1_c_macro, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_2_c_macro, component = c("random"))),
                                                                    as.numeric(get_variance(reg_3_c_macro, component = c("random")))),4))),
                      
                      list(c("Countries", 30, 30, 30, 30))
                      
          ),
          
          model.names=FALSE,
          type = "text"
)

# Appendix G: Outliers

reg_0_c_trimmed <- romr.fnc(reg_0_c, issp, trim = 2.5)$data
reg_1_c_trimmed <- romr.fnc(reg_1_c, issp, trim = 2.5)$data
reg_2_c_trimmed <- romr.fnc(reg_2_c, issp, trim = 2.5)$data
reg_3_c_trimmed <- romr.fnc(reg_3_c, issp, trim = 2.5)$data

reg_0_c_outlier <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + (1 | cntry), data = reg_0_c_trimmed)
reg_1_c_outlier <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_hospital_c*phe100 + (1 | cntry), data = reg_1_c_trimmed) 
reg_2_c_outlier <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_doctor_c*phe100 + (1 | cntry), data = reg_2_c_trimmed) 
reg_3_c_outlier <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + badexp_c*phe100 + (1 | cntry), data = reg_3_c_trimmed)

stargazer(reg_0_c_outlier, reg_1_c_outlier, reg_2_c_outlier, reg_3_c_outlier,
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits= 2,
          model.numbers = FALSE,
          font.size = "footnotesize",
          notes.align = "l",
          float.env = "sidewaystable",
          out = "tab-mainregression-outlier.html",
          keep = c("male", "age", "age2100", "married", "employed", "income",
                   "healthstatus", "the", "phe100", "sat_hospital_c", "phe100:sat_hospital_c",
                   "sat_doctor_c", "phe100:sat_doctor_c", "badexp_c", "phe100:badexp_c"), 
          covariate.labels =c("Male", "Age", "Age (sq)", "Married",
                              "Employed", "Income", "Subjective health", "Health exp. (%GDP)", "PHE (%)",
                              "Hospital evaluation", "Hospital evaluation * PHE",
                              "Doctor evaluation", "Doctor evaluation * PHE",
                              "Objective experience", "Objective experience * PHE"),
          column.labels=c("(1)", "(2)", "(3)", "(4)"),
          add.lines=c(list(c("Education FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Occupation FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Health care insurance FE", "Yes", "Yes", "Yes", "Yes")),
                      
                      list(c(c("ICC"), round(c(as.numeric(icc(reg_0_c_outlier)), 
                                               as.numeric(icc(reg_1_c_outlier)), 
                                               as.numeric(icc(reg_2_c_outlier)),
                                               as.numeric(icc(reg_3_c_outlier))),4))),
                      
                      list(c(c("Within country variance"), round(c(as.numeric(get_variance(reg_0_c_outlier, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_1_c_outlier, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_2_c_outlier, component = c("residual"))),
                                                                   as.numeric(get_variance(reg_3_c_outlier, component = c("residual")))),4))),
                      
                      
                      list(c(c("Between country variance"), round(c(as.numeric(get_variance(reg_0_c_outlier, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_1_c_outlier, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_2_c_outlier, component = c("random"))),
                                                                    as.numeric(get_variance(reg_3_c_outlier, component = c("random")))),4))),
                      
                      list(c("Countries", 30, 30, 30, 30))
                      
          ),
          
          model.names=FALSE,
          type = "text"
)


df_robustness <- data.frame(
  cntry = rep(unique(issp$cntry), 3),
  outcome = c(rep("sat_hospital", 30), rep("sat_doctor", 30), rep("badexp", 30)),
  statistics = NA
)

for (i in unique(issp$cntry)) {
  df_robustness$statistics[df_robustness$cntry == i & df_robustness$outcome == "sat_hospital"] <- data.frame(coef(summary(lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_hospital_c*phe100 + (1 | cntry), data = issp[issp$cntry != i,]))))["phe100:sat_hospital_c","t.value"]
  df_robustness$statistics[df_robustness$cntry == i & df_robustness$outcome == "sat_doctor"] <- data.frame(coef(summary(lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + sat_doctor_c*phe100 + (1 | cntry), data = issp[issp$cntry != i,]))))["phe100:sat_doctor_c","t.value"]
  df_robustness$statistics[df_robustness$cntry == i & df_robustness$outcome == "badexp"] <- data.frame(coef(summary(lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + phe100 + badexp_c*phe100 + (1 | cntry), data = issp[issp$cntry != i,]))))["phe100:badexp_c","t.value"]
}

df_robustness <- df_robustness %>%
  mutate(statistics = abs(statistics),
         name = case_when(
           outcome == "sat_hospital" ~ "Hospital evaluation",
           outcome == "sat_doctor" ~ "Doctor evaluation",
           outcome == "badexp" ~ "Objective experience"
         ))

pdf("fig-robustness-macrooutlier.pdf", width = 8, height = 4)
ggplot(df_robustness, aes(x=statistics)) +
  geom_histogram(bins = 10, colour = "black", fill = "gray70") +
  facet_wrap(~ name, scales = "free_x") +
  labs(y = "",
       x = "Test statistic")
dev.off()


oecd_raw <- read_csv("HEALTH_PROT_31052019005406509.csv")

oecd <- oecd_raw %>%
  filter(Variable == "Total health care", Measure == "% of total population") %>% 
  select(-c("VAR", "Variable", "UNIT", "COU", "YEA", "Flag Codes", "Flags")) %>% 
  setNames( c("measure", "name", "year", "value")) %>% 
  mutate(name = case_when(
    name == "Russia" ~ "Russian Federation",
    name == "Korea" ~ "Korea, Rep.",
    TRUE ~ name
  ))

oecd_agg <- oecd %>%
  group_by(name, year) %>%
  summarise(healthcoverage = mean(value, na.rm=TRUE) / 100,
            .groups = "drop")

issp_oecd <- left_join(issp, oecd_agg, by = c("name", "year"))

reg_0_c_oecd <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + healthcoverage + (1 | cntry), data = issp_oecd) 
reg_1_c_oecd <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + healthcoverage + sat_hospital_c*healthcoverage + (1 | cntry), data = issp_oecd) 
reg_2_c_oecd <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + healthcoverage + sat_doctor_c*healthcoverage + (1 | cntry), data = issp_oecd) 
reg_3_c_oecd <- lmer(moregov ~ male + age + age2100 + factor(edu) + married + employed + income + healthstatus + factor(insurance) + factor(occ) + the + healthcoverage + badexp_c*healthcoverage + (1 | cntry), data = issp_oecd)

stargazer(reg_0_c_oecd,reg_1_c_oecd,reg_2_c_oecd,reg_3_c_oecd,
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits= 2,
          model.numbers = FALSE,
          font.size = "footnotesize",
          notes.align = "l",
          star.cutoffs = c(0.05, 0.01, 0.001),
          float.env = "sidewaystable",
          out = "tab-mainregression-oecd.html",
          keep = c("male", "age", "age2100", "married", "employed", "income",
                   "healthstatus", "the", "healthcoverage", "sat_hospital_c", "healthcoverage:sat_hospital_c",
                   "sat_doctor_c", "healthcoverage:sat_doctor_c", "badexp_c", "healthcoverage:badexp_c"), 
          covariate.labels =c("Male", "Age", "Age (sq)", "Married",
                              "Employed", "Income", "Subjective health", "Health exp. (%GDP)", "Coverage",
                              "Hospital evaluation", "Hospital evaluation * Coverage",
                              "Doctor evaluation", "Doctor evaluation * Coverage",
                              "Objective experience", "Objective experience * Coverage"),
          column.labels=c("(1)", "(2)", "(3)", "(4)"),
          add.lines=c(list(c("Education FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Occupation FE", "Yes", "Yes", "Yes", "Yes")),
                      list(c("Health care insurance FE", "Yes", "Yes", "Yes", "Yes")),
                      
                      list(c(c("ICC"), round(c(as.numeric(icc(reg_0_c_oecd)), 
                                               as.numeric(icc(reg_1_c_oecd)), 
                                               as.numeric(icc(reg_2_c_oecd)),
                                               as.numeric(icc(reg_3_c_oecd))),4))),
                      
                      list(c(c("Within country variance"), round(c(as.numeric(get_variance(reg_0_c_oecd, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_1_c_oecd, component = c("residual"))), 
                                                                   as.numeric(get_variance(reg_2_c_oecd, component = c("residual"))),
                                                                   as.numeric(get_variance(reg_3_c_oecd, component = c("residual")))),4))),
                      
                      
                      list(c(c("Between country variance"), round(c(as.numeric(get_variance(reg_0_c_oecd, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_1_c_oecd, component = c("random"))), 
                                                                    as.numeric(get_variance(reg_2_c_oecd, component = c("random"))),
                                                                    as.numeric(get_variance(reg_3_c_oecd, component = c("random")))),4))),
                      
                      list(c("Countries", 24, 24, 24, 24))
                      
          ),
          
          model.names=FALSE,
          type = "text"
          )


# Create and save sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")