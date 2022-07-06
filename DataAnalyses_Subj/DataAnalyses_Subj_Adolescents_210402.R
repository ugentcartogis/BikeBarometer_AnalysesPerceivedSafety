# Load and manipulate data
library(car)
library(QuantPsyc)
library(nlme)
library(lme4)
library(effects)
library(jtools)
library(lmerTest)
library(scales)
library(MASS)
library(lmtest)
library(betareg)
library(mgcv)
library(plyr)
library(lmerTest)
library(data.table)
library(glmmTMB)
library(Metrics)
library(sjstats)
library(DHARMa)
library(performance)
library(MASS)
library(cAIC4)

rm(list=ls())

###############
# ADOLESCENTS #
###############

#### TO DO
# OK Gender = other niet mee integreren in dataset
# OK Nieuw full model enkel met relevante termen
# OK Zoek naar random effect
# OK Bekijk resultaten full model
# OK Niet-significante termen afbouwen (telkens deze met hoogste p-waarde weglaten) + AIC's vergelijken
# OK Model met enkel significante termen en laagste AIC overhouden
# OK Assumpties en predicties checken
# Factoranalyse doen want veel dimensies (maar niet gebruiken)
# OK Forward / Backward selectie doen uit interesse


### Load data ###
#################

### Load data
adolescents <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/dataset_adolescents_210402.csv', fileEncoding="UTF-8-BOM")

# Delete records with 'gender = other'
adolescents <- adolescents[!(adolescents$gender=="other"),]

# Convert variables
adolescents$school_id <- as.factor(adolescents$school_id)
adolescents$start_time <- as.numeric(as.POSIXct(adolescents$start_time, format = '%H:%M'))
adolescents$end_time <- as.numeric(as.POSIXct(adolescents$end_time, format = '%H:%M'))

# Convert to beta variable
adolescents$overall_safety_score_standard <- (adolescents$overall_safety_score - 0)/ (10-0)
adolescents$overall_safety_score_standard_beta <- (adolescents$overall_safety_score_standard * (1916 - 1) + 0.5)/1916

# Rescale the variables
adolescents$age_scaled <- rescale(adolescents$age)
adolescents$route_assessed_dist_scaled <- rescale(adolescents$route_assessed_dist) 
adolescents$start_time_scaled <- rescale(adolescents$start_time) 
adolescents$end_time_scaled <- rescale(adolescents$end_time) 
adolescents$frequency_scaled_scaled <- rescale(adolescents$frequency_scaled) 
adolescents$company_nr_scaled <- rescale(adolescents$company_nr) 

# Transform the dependent variable to have only positive values
adolescents$safe_sc_gamma <- (adolescents$overall_safety_score * (1916 - 1) + 0.5)/1916
adolescents$overall_safety_score_nozero <- adolescents$overall_safety_score
adolescents$overall_safety_score_nozero[adolescents$overall_safety_score_nozero==0] <- 0.0000000001

# T-test overall versus average safety
t.test(adolescents$overall_safety_score, adolescents$avg_safety_score, paired = TRUE)

### RANDOM EFFECT SEARCH ###
############################

### Saturated lineair model wihtout random effects (with rescaled variables) ###
adolescents_model_sat00 <- lm(overall_safety_score~
                                gender+
                                age_scaled+
                                frequency_scaled_scaled +
                                route_assessed_cyclist + 
                                route_assessed_dist_scaled+
                                end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                determ_leastobstacles + determ_leastobstacles*gender + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled,
                              data=adolescents,REML=TRUE,  na.action = na.exclude)

summary(adolescents_model_sat00) 
# Woman give lower scores
# Cyclists give higher scores
# Higher scores when route with least traffic is chosen
# With company: lower scores, 
# Cyclists that take the shortest route: lower score
# Further and choice for least traffic = lower score
# Choice for safe infrastructure and further = higher score
# Choice for safe infrastructure and more frequent = higher score
# Girls with company: higher score
# Cyclists with company: higher score
# No interactions with age significant, age is not significant
# Slow traffic, most beautiful, least obstacles, others determining route are not significant

# RE: municipality
adolescents_model_sat01 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*gender + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=TRUE, na.action = na.exclude)

lrtest(adolescents_model_sat00, adolescents_model_sat01)  # 01 is sign. better
anova(adolescents_model_sat00, adolescents_model_sat01)
AIC(adolescents_model_sat00)
AIC(adolescents_model_sat01)  # 01 is better
summary(adolescents_model_sat01) 

# RE: school_id 
adolescents_model_sat02 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*gender + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|school_id), 
                                data=adolescents, REML=TRUE, na.action = na.exclude)

lrtest(adolescents_model_sat00, adolescents_model_sat02) # 02 is significantly better
AIC(adolescents_model_sat00)
AIC(adolescents_model_sat02)  # 02 is better
AIC(adolescents_model_sat01)  # 01 is similar to 02 (AIC 01 is 0.2 smaller than AIC 02)

# RE: school_id in municipality
adolescents_model_sat03 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*gender + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality/school_id), 
                                data=adolescents, REML=TRUE, na.action = na.exclude)

lrtest(adolescents_model_sat03, adolescents_model_sat00) # 03 is sign. better
anova(adolescents_model_sat00, adolescents_model_sat03)
AIC(adolescents_model_sat00)
AIC(adolescents_model_sat03)  # 03 is better
AIC(adolescents_model_sat01)  # 01 is better

# Include random effect of municipality

### FIXED EFFECT SEARCH ###
###########################
adolescents_model_sat01 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*gender + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

summary(adolescents_model_sat01) # Random effect variance = 0.16
# Woman give lower scores
# Cyclists give higher scores
# Higher scores when route with least traffic is chosen
# With company: lower scores, 
# Cyclists that take the shortest route: lower score
# Further and choice for least traffic = lower score
# Choice for safe infrastructure and further = higher score
# Choice for safe infrastructure and more frequent = higher score
# Girls with company: higher score
# Cyclists with company: higher score
# No interactions with age significant, age is not significant
# Slow traffic, most beautiful, least obstacles, others determining route are not significant

# Delete genderwoman:determ_leastobstacleson               
adolescents_model_sat02 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*age_scaled + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat01)
AIC(adolescents_model_sat02)  # 02 is better
summary(adolescents_model_sat02)

# Delete age_scaled:determ_leasttrafficon                     
adolescents_model_sat03 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*age_scaled + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat02)
AIC(adolescents_model_sat03)  # 03 is better
summary(adolescents_model_sat03)

# Delete age_scaled:determ_cycleinfraon                    
adolescents_model_sat04 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled + determ_mostbeautiful*frequency_scaled_scaled+
                                  determ_leastobstacles + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat03)
AIC(adolescents_model_sat04)  # 04 is better
summary(adolescents_model_sat04)

# Delete frequency_scaled_scaled:determ_mostbeautifulon                      
adolescents_model_sat05 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*gender + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*gender + determ_slowtraffic*age_scaled + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + determ_slowtraffic*frequency_scaled_scaled+
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + determ_mostbeautiful*route_assessed_cyclist + determ_mostbeautiful*route_assessed_dist_scaled +
                                  determ_leastobstacles + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*route_assessed_dist_scaled + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*age_scaled + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + determ_others*frequency_scaled_scaled +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*age_scaled + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat04)
AIC(adolescents_model_sat05)  # 05 is better
summary(adolescents_model_sat05)

# Delete all interaction terms with p > 0.50: 
# age_scaled:determ_otherson                         
# age_scaled:company_nr_scaled                       
# route_assessed_cyclistTRUE:determ_mostbeautifulon  
# genderwoman:determ_leasttrafficon                  
# frequency_scaled_scaled:determ_slowtrafficon       
# route_assessed_dist_scaled:determ_leastobstacleson 
# genderwoman:determ_slowtrafficon                   
# age_scaled:determ_slowtrafficon                    
# frequency_scaled_scaled:determ_otherson            
# route_assessed_dist_scaled:determ_mostbeautifulon  

adolescents_model_sat06 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + 
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + 
                                  determ_leastobstacles + determ_leastobstacles*age_scaled + determ_leastobstacles*route_assessed_cyclist + determ_leastobstacles*frequency_scaled_scaled+
                                  determ_others + determ_others*gender + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat05)
AIC(adolescents_model_sat06)  # 06 is better
summary(adolescents_model_sat06)

# Delete all main terms with p > 0.60 and not in interactions with p < 0.10
# Delete determ_leastobstacleson  and interactions
adolescents_model_sat07 <- lmer(overall_safety_score~
                                  gender+
                                  age_scaled+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender + determ_shortest*age_scaled + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + 
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + determ_mostbeautiful*age_scaled + 
                                  determ_others + determ_others*gender + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat06)
AIC(adolescents_model_sat07)  # 07 is better
summary(adolescents_model_sat07)

# Delete age_scaled  and interactions
adolescents_model_sat08 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + end_time_scaled*route_assessed_dist_scaled +
                                  determ_shortest + determ_shortest*gender +determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + 
                                  determ_cycleinfra + determ_cycleinfra*gender + determ_cycleinfra*route_assessed_cyclist + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*gender + determ_others*route_assessed_cyclist + determ_others*route_assessed_dist_scaled + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat07)
AIC(adolescents_model_sat08)  # 08 is better
summary(adolescents_model_sat08)

# Delete interaction terms with p > 0.40
# route_assessed_dist_scaled:end_time_scaled      
# route_assessed_dist_scaled:determ_otherson      
# genderwoman:determ_shorteston                   
# route_assessed_cyclistTRUE:determ_cycleinfraon  
# genderwoman:determ_cycleinfraon                 

adolescents_model_sat09 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + determ_shortest*frequency_scaled_scaled +
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + determ_slowtraffic*route_assessed_dist_scaled + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*gender + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat08)
AIC(adolescents_model_sat09)  # 09 is better
summary(adolescents_model_sat09)

# Delete interaction terms with p > 0.20
# route_assessed_dist_scaled:determ_slowtrafficon  
# frequency_scaled_scaled:determ_shorteston        
# genderwoman:determ_otherson                      

adolescents_model_sat10 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat09)
AIC(adolescents_model_sat10)  # 10 is better
summary(adolescents_model_sat10)

# Delete main effect determ_mostbeautiful and interaction terms
adolescents_model_sat11 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_cyclist + determ_leasttraffic*route_assessed_dist_scaled + determ_leasttraffic*frequency_scaled_scaled+
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + company_nr_scaled*route_assessed_dist_scaled + company_nr_scaled*frequency_scaled_scaled +
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat10)
AIC(adolescents_model_sat11)  # 11 is worse
summary(adolescents_model_sat11)

# Set back main effect of determ_mostbeautiful
# Delete interaction terms with p > 0.10
# frequency_scaled_scaled:company_nr_scaled        
# route_assessed_dist_scaled:company_nr_scaled     
# route_assessed_cyclistTRUE:determ_leasttrafficon 
# frequency_scaled_scaled:determ_leasttrafficon   

adolescents_model_sat12 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat10)
AIC(adolescents_model_sat12)  # 12 is similar as 10 (AIC 10 = AIC 12 - 0.2)
summary(adolescents_model_sat12)

## Delete other interaction terms from high to low p-value
# Delete genderwoman:determ_mostbeautifulon               
adolescents_model_sat13 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat13)  # 13 is worse
summary(adolescents_model_sat13)

# Set back genderwoman:determ_mostbeautifulon
# Delete route_assessed_dist_scaled:determ_shorteston     
adolescents_model_sat14 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat14)  # 14 is worse
summary(adolescents_model_sat14)
# Tested: deleting both route_assessed_dist_scaled:determ_shorteston and genderwoman:determ_mostbeautifulon leads to an even higher AIC

# Set back route_assessed_dist_scaled:determ_shorteston 
# Delete route_assessed_cyclistTRUE:determ_otherson       
adolescents_model_sat15 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + determ_cycleinfra*frequency_scaled_scaled +
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat15)  # 15 is worse
summary(adolescents_model_sat15)

# Set back route_assessed_cyclistTRUE:determ_otherson 
# Delete frequency_scaled_scaled:determ_cycleinfraon      
adolescents_model_sat16 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat16)  # 16 is worse
summary(adolescents_model_sat16) 

# Delete all interactions terms with p > 0.05
# Delete genderwoman:determ_mostbeautifulon 
# Delete route_assessed_dist_scaled:determ_shorteston 
# Delete route_assessed_cyclistTRUE:determ_otherson 
# Delete frequency_scaled_scaled:determ_cycleinfraon  
adolescents_model_sat17 <- lmer(overall_safety_score~
                                  gender+
                                  frequency_scaled_scaled +
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + 
                                  determ_others + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat17)  # 17 is worse
summary(adolescents_model_sat12)

## Delete main effects and its interaction terms with p > 0.05 (when not in significant interaction terms)
# Delete frequency_scaled_scaled and its interaction terms
adolescents_model_sat18 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12)
AIC(adolescents_model_sat18)  #  18 is similar as 12 (AIC 18 = AIC 12 - 0.2)
summary(adolescents_model_sat18)

# Delete determ_mostbeautiful and its interaction terms
adolescents_model_sat19 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat18)
AIC(adolescents_model_sat19)  # 19 is worse

# Delete genderwoman:determ_mostbeautifulon              
adolescents_model_sat20 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful +
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat18)
AIC(adolescents_model_sat20)  #  20 is worse

# Delete route_assessed_dist_scaled:determ_shorteston    
adolescents_model_sat21 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat18)
AIC(adolescents_model_sat21)  #  21 is worse

# Delete route_assessed_cyclistTRUE:determ_otherson
adolescents_model_sat22 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + determ_shortest*route_assessed_dist_scaled + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others +
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat18)
AIC(adolescents_model_sat22)  #  22 is worse

#### Model adolescents_model_sat18 has the lowest possible AIC
summary(adolescents_model_sat18)

# Random effect variance = 0.17

# Woman give lower scores
# Cyclists give higher scores
# Those arriving late give higher scores
# Higher scores when choosing the shortest route, the route with the least traffic and the route with the slowest traffic, when others choose the route (not: most beautiful, cycle infra)
# When choosing the shortest route, but cyclist : lower score
# When choosing the route with the least traffic, but further: lower score
# When choosing the route with the slowest traffic, but cyclist: lower score
# When choosing the route with most safe infrastructure, and furhter: higher score
# With company: lower scores
# Girls with company: higher score
# Cyclists with company: higher score

## CHECK THE PREDICTIONS ##
###########################

preds=predict(adolescents_model_sat18, na.action = na.exclude)
tens <- count(adolescents$overall_safety_score == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 0 have a prediction above 10, 47 have a value of 10 in the original data.
zeros <- count(adolescents$overall_safety_score == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 0 have a prediction below 0, 11 have a value of 0 in the original data.

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(adolescents_model_sat18)
influenceIndexPlot(adolescents_model_sat18, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 0 observations with Bonf.p < 0.05

cd = cooks.distance(adolescents_model_sat18)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(adolescents_model_sat18, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(adolescents_model_sat18))
qqline(residuals(adolescents_model_sat18))
qqPlot(residuals(adolescents_model_sat18))
# On a straight line between -2 and 1.25, but underestimation between 1.25 and 2

## Homogeneity of variance
plot(adolescents_model_sat18)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(adolescents_model_sat23)
check_collinearity(adolescents_model_sat23)
# High correlation for determ_shortest, and determ_shortest:route_assessed_dist


# Delete determ_shortest:route_assessed_dist
adolescents_model_sat23 <- lmer(overall_safety_score~
                                 gender+
                                 route_assessed_cyclist + 
                                 route_assessed_dist_scaled+
                                 end_time_scaled + 
                                 determ_shortest + determ_shortest*route_assessed_cyclist + 
                                 determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                 determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                 determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                 determ_mostbeautiful + determ_mostbeautiful*gender + 
                                 determ_others + determ_others*route_assessed_cyclist + 
                                 company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                 (1|municipality), 
                               data=adolescents, REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat18)
AIC(adolescents_model_sat23)  #  AIC 18 < AIC 23 - 1
check_collinearity(adolescents_model_sat23)   # but no high multicollinearity anymore

summary(adolescents_model_sat18)
summary(adolescents_model_sat23)
# Same results, but distance becomes significant (lower score if further) and time becomes insignificant (later is safer)

## CHECK THE PREDICTIONS ##
###########################

preds=predict(adolescents_model_sat23, na.action = na.exclude)
tens <- count(adolescents$overall_safety_score == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 0 have a prediction above 10, 47 have a value of 10 in the original data.
zeros <- count(adolescents$overall_safety_score == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 0 have a prediction below 0, 11 have a value of 0 in the original data.

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(adolescents_model_sat23)
influenceIndexPlot(adolescents_model_sat23, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 0 observations with Bonf.p < 0.05

cd = cooks.distance(adolescents_model_sat23)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(adolescents_model_sat23, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(adolescents_model_sat23))
qqline(residuals(adolescents_model_sat23))
qqPlot(residuals(adolescents_model_sat23))
# On a straight line between -2 and 1.25, but underestimation between 1.25 and 2

## Homogeneity of variance
plot(adolescents_model_sat23)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(adolescents_model_sat23)
check_collinearity(adolescents_model_sat23)
# Only some moderate correlation, but < 8.5

## FINAL MODEL ##
#################
adolescents_model_sat23 <- lmer(overall_safety_score~
                                  gender+
                                  route_assessed_cyclist + 
                                  route_assessed_dist_scaled+
                                  end_time_scaled + 
                                  determ_shortest + determ_shortest*route_assessed_cyclist + 
                                  determ_leasttraffic + determ_leasttraffic*route_assessed_dist_scaled + 
                                  determ_slowtraffic + determ_slowtraffic*route_assessed_cyclist + 
                                  determ_cycleinfra + determ_cycleinfra*route_assessed_dist_scaled + 
                                  determ_mostbeautiful + determ_mostbeautiful*gender + 
                                  determ_others + determ_others*route_assessed_cyclist + 
                                  company_nr_scaled + company_nr_scaled*gender + company_nr_scaled*route_assessed_cyclist + 
                                  (1|municipality), 
                                data=adolescents, REML=FALSE, na.action = na.exclude)

summary(adolescents_model_sat23)

### STEP AIC ###
################
adolescents_model_step_back <- stepcAIC(adolescents_model_sat01, direction = 'backward')
adolescents_model_step_back
# Very large resulting model

adolescents_model_step_forw <- stepcAIC(adolescents_model_sat01, direction = 'forward')
adolescents_model_step_forw
# Very large resulting model
