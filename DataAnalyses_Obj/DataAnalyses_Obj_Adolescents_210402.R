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
adolescents <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/08_dataset_adolescents_210402.csv', fileEncoding="UTF-8-BOM")

# Remove unnecessary columns
adolescents <- adolescents[,c(1,2,3,6,8,13,16,19,20,31:57), drop=FALSE]

# Delete records with 'gender = other'
adolescents <- adolescents[!(adolescents$gender=="other"),]

# Convert variables
adolescents$school_id <- as.factor(adolescents$school_id)
adolescents$pedestrian_lane[is.na(adolescents$pedestrian_lane)] <- 0

# Take cycling infrastructure column together to have less variables
adolescents$non_sep_bike_lane <- rowSums(adolescents[, c(21,22)])       # non_sep_bike_lane + bike_suggestion_lane
adolescents$bike_path_without_cars <- rowSums(adolescents[, c(25,26)])  # bike_street_no_cars + bike_highway
adolescents$other_infra <- rowSums(adolescents[, c(27,28,29)])  # pedestrian_lane + forbidden_for_cyclists + other_cycling_infra
adolescents <- adolescents[-c(21,25:29)]

# Delete rows with NA values
adolescents_nona = adolescents[complete.cases(adolescents),]

# Rescale the variables
adolescents$age_scaled <- rescale(adolescents$age)
adolescents$freq_scaled_scaled <- rescale(adolescents$freq_sc)
adolescents$rt_a_dist_scaled <- rescale(adolescents$rt_a_dist) 
adolescents$ov_saf_sc_scaled <- rescale(adolescents$ov_saf_sc) 
adolescents$TrafficVolume_scaled <- rescale(adolescents$TrafficVolume) 
adolescents$TrafficSpeed_scaled <- rescale(adolescents$TrafficSpeed) 
adolescents$MaxSpeed_scaled <- rescale(adolescents$maxspeed) 
adolescents$CyclingAcci_scaled <- rescale(adolescents$cyclingacci) 
adolescents$Light_scaled <- rescale(adolescents$light) 
adolescents$Rails_scaled <- rescale(adolescents$rails) 
adolescents$SideStreets_scaled <- rescale(adolescents$SideStreets) 
adolescents$DiffRoutes_scaled <- rescale(adolescents$DiffRoutes) 
adolescents$Width_scaled <- rescale(adolescents$width) 
adolescents$Condition_scaled <- rescale(adolescents$Condition) 

### EFFECT OF INCLUDING CONDITION OR NOT ###
############################################

# Delete columns condition and width
drops <- c('Condition', 'Condition_scaled', 'width', 'Width_scaled')
adolescents_noincompletevar <- adolescents[,!(names(adolescents) %in% drops)]
adolescents_noincompletevar_nona = adolescents_noincompletevar[complete.cases(adolescents_noincompletevar),]
# Less incomplete records


### Saturated lineair model wihtout random effects (with rescaled variables) - WITH incomplete variables ###
adolescents_model_sat00_a <- lm(ov_saf_sc~
                                gender+
                                age_scaled+
                                freq_scaled_scaled +
                                rt_a_cycl + 
                                rt_a_dist_scaled +
                                TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled,
                              data=adolescents,REML=TRUE,  na.action = na.exclude)

summary(adolescents_model_sat00_a) 

### Saturated lineair model wihtout random effects (with rescaled variables) - WITHOUT incomplete variables ###
adolescents_model_sat00_b <- lm(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                  Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled,
                                data=adolescents_noincompletevar,REML=TRUE,  na.action = na.exclude)

summary(adolescents_model_sat00_b)

AIC(adolescents_model_sat00_a, adolescents_model_sat00_b)
# Similar results for both models
# AIC of model a is smaller than model b --> Include incomplete variables in the analysis

### SATURATED MODEL ###
#######################

### Saturated lineair model without random effects (with rescaled variables) - WITH incomplete variables ###
adolescents_model_sat00 <- lm(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                  Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled,
                                data=adolescents,REML=TRUE,  na.action = na.exclude)

summary(adolescents_model_sat00)

# Negative effect: SideStreets, residential, agricultural
# Positive effect: Light
# Interactions with positive effect: TrafficVolume:women, MaxSpeed:women, bike street:dist, other infra:dist, residental:women, industrial:dist, recreational:cyclist
# Interactions with negative effect: TrafficSpeed:women, CyclingAcci:age, Light:women, Light:cyclist

### RANDOM EFFECT SEARCH ###

# RE: municipality
adolescents_model_sat01 <- lmer(ov_saf_sc~
                                gender+
                                age_scaled+
                                freq_scaled_scaled +
                                rt_a_cycl + 
                                rt_a_dist_scaled +
                                TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled +
                                (1|municipal),
                              data=adolescents,REML=TRUE, na.action = na.exclude)

AIC(adolescents_model_sat00, adolescents_model_sat01) # 01 is better than 00 (lower AIC)
summary(adolescents_model_sat01)

# RE: school_id 
adolescents_model_sat02 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                  Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled +
                                  (1|school_id),
                                data=adolescents,REML=TRUE, na.action = na.exclude)

AIC(adolescents_model_sat00, adolescents_model_sat02) # 02 is better than 00 (lower AIC)
AIC(adolescents_model_sat01, adolescents_model_sat02) # 02 is very similar to 01
summary(adolescents_model_sat02)

# RE: school_id in municipality
adolescents_model_sat03 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                  Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled +
                                  (1|municipal/school_id),
                                data=adolescents,REML=TRUE, na.action = na.exclude)

AIC(adolescents_model_sat00, adolescents_model_sat03) # 03 is better than 00 (lower AIC)
AIC(adolescents_model_sat01, adolescents_model_sat03) # 03 is a bit worse than 01
summary(adolescents_model_sat03)

# Include random effect of municipality

### FIXED EFFECT SEARCH ###
###########################
adolescents_model_sat01 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_cycl + CyclingAcci_scaled*rt_a_dist_scaled + CyclingAcci_scaled*freq_scaled_scaled +
                                  Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_cycl + Rails_scaled*rt_a_dist_scaled + Rails_scaled*freq_scaled_scaled +
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + DiffRoutes_scaled*freq_scaled_scaled +
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + Width_scaled*freq_scaled_scaled +
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_cycl + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_cycl + non_sep_bike_lane*rt_a_dist_scaled + non_sep_bike_lane*freq_scaled_scaled +
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_cycl + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_cycl + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled + other_infra*freq_scaled_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_cycl + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*age_scaled + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

summary(adolescents_model_sat01)

# Delete all interactions with p > 0.80
# age_scaled:Light_scaled                  
# age_scaled:commercial                    
# freq_scaled_scaled:CyclingAcci_scaled    
# rt_a_cyclTRUE:CyclingAcci_scaled         
# age_scaled:TrafficSpeed_scaled           
# freq_scaled_scaled:other_infra           
# rt_a_cyclTRUE:Rails_scaled               
# freq_scaled_scaled:DiffRoutes_scaled     
# rt_a_cyclTRUE:residential                
# freq_scaled_scaled:Width_scaled          
# rt_a_cyclTRUE:no_cycling_infra           
# rt_a_cyclTRUE:non_sep_bike_lane          
# age_scaled:TrafficVolume_scaled          
# rt_a_cyclTRUE:sep_bike_lane              
# freq_scaled_scaled:Rails_scaled          
# rt_a_cyclTRUE:bike_path_without_cars     
# freq_scaled_scaled:non_sep_bike_lane  

adolescents_model_sat04 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  freq_scaled_scaled +
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + TrafficVolume_scaled*freq_scaled_scaled +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled + TrafficSpeed_scaled*freq_scaled_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + MaxSpeed_scaled*freq_scaled_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + Light_scaled*freq_scaled_scaled +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + SideStreets_scaled*freq_scaled_scaled +
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + Condition_scaled*freq_scaled_scaled +
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + no_cycling_infra*freq_scaled_scaled +
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + sep_bike_lane*freq_scaled_scaled +
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + bike_street*freq_scaled_scaled +
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + bike_path_without_cars*freq_scaled_scaled +
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_dist_scaled + residential*freq_scaled_scaled +
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + industrial*freq_scaled_scaled +
                                  commercial + commercial*gender + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + commercial*freq_scaled_scaled +
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + recreational*freq_scaled_scaled +
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + agricultural*freq_scaled_scaled +
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + natural*freq_scaled_scaled +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat01, adolescents_model_sat04) # 04 is better than 03
summary(adolescents_model_sat04)

# Delete all interactions with and main effect of freq_scaled (because no significant interactions)
adolescents_model_sat05 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + 
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_cycl + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + 
                                  no_cycling_infra + no_cycling_infra*gender + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*gender + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*gender + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*gender + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*gender + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*gender + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*gender + residential*age_scaled + residential*rt_a_dist_scaled + 
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + 
                                  commercial + commercial*gender + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + 
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + 
                                  natural + natural*gender + natural*age_scaled + natural*rt_a_cycl + natural*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat04, adolescents_model_sat05) # 05 is better than 04
summary(adolescents_model_sat05)

# Delete all interactions with p > 0.80
# genderwoman:sep_bike_lane              
# age_scaled:residential                 
# age_scaled:Condition_scaled            
# rt_a_cyclTRUE:SideStreets_scaled       
# genderwoman:non_sep_bike_lane          
# genderwoman:bike_path_without_cars     
# genderwoman:no_cycling_infra           
# genderwoman:bike_street                
# age_scaled:natural                     
# genderwoman:other_infra                

adolescents_model_sat06 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_cycl + TrafficVolume_scaled*rt_a_dist_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl + MaxSpeed_scaled*rt_a_dist_scaled + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + 
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*age_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*age_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*gender + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*rt_a_cycl + Condition_scaled*rt_a_dist_scaled + 
                                  no_cycling_infra + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*gender + residential*rt_a_dist_scaled + 
                                  industrial + industrial*gender + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + 
                                  commercial + commercial*gender + commercial*rt_a_cycl + commercial*rt_a_dist_scaled + 
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + 
                                  natural + natural*gender + natural*rt_a_cycl + natural*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat05, adolescents_model_sat06) # 06 is better than 05
summary(adolescents_model_sat06)

# Delete interactions with p > 0.60
# rt_a_dist_scaled:commercial            
# genderwoman:industrial  
# rt_a_dist_scaled:Condition_scaled      
# age_scaled:Rails_scaled                
# rt_a_dist_scaled:MaxSpeed_scaled       
# genderwoman:DiffRoutes_scaled          
# age_scaled:SideStreets_scaled          
# rt_a_cyclTRUE:TrafficVolume_scaled     

adolescents_model_sat07 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_dist_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + 
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*gender + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_cycl + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*rt_a_cycl +  
                                  no_cycling_infra + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*gender + residential*rt_a_dist_scaled + 
                                  industrial + industrial*age_scaled + industrial*rt_a_cycl + industrial*rt_a_dist_scaled + 
                                  commercial + commercial*gender + commercial*rt_a_cycl + 
                                  recreational + recreational*gender + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + agricultural*gender + agricultural*age_scaled + agricultural*rt_a_cycl + agricultural*rt_a_dist_scaled + 
                                  natural + natural*gender + natural*rt_a_cycl + natural*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat06, adolescents_model_sat07) # 07 is better than 06
summary(adolescents_model_sat07)

# Delete interactions with p > 0.50
# rt_a_dist_scaled:natural               
# genderwoman:commercial                 
# genderwoman:natural                    
# genderwoman:agricultural               
# genderwoman:recreational               
# rt_a_cyclTRUE:Width_scaled             
# genderwoman:residential                
# genderwoman:SideStreets_scaled         
# rt_a_cyclTRUE:industrial               
# rt_a_cyclTRUE:natural                  
# rt_a_cyclTRUE:agricultural

adolescents_model_sat08 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_dist_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl + Light_scaled*rt_a_dist_scaled + 
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*rt_a_cycl +  
                                  no_cycling_infra + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*rt_a_dist_scaled + 
                                  industrial + industrial*age_scaled + industrial*rt_a_dist_scaled + 
                                  commercial + commercial*rt_a_cycl + 
                                  recreational + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + agricultural*age_scaled + agricultural*rt_a_dist_scaled + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat07, adolescents_model_sat08) # 08 is better than 07
summary(adolescents_model_sat08)

# Delete interactions with p > 0.40
# age_scaled:agricultural                
# age_scaled:industrial                  
# rt_a_dist_scaled:Light_scaled

adolescents_model_sat09 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*rt_a_dist_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*rt_a_dist_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + Rails_scaled*gender + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*rt_a_dist_scaled + 
                                  Condition_scaled + Condition_scaled*gender + Condition_scaled*rt_a_cycl +  
                                  no_cycling_infra + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential + residential*rt_a_dist_scaled + 
                                  industrial + industrial*rt_a_dist_scaled + 
                                  commercial + commercial*rt_a_cycl + 
                                  recreational + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + agricultural*rt_a_dist_scaled + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat08, adolescents_model_sat09) # 09 is better than 08
summary(adolescents_model_sat09)

# Delete interactions with p > 0.20
# rt_a_dist_scaled:Width_scaled          
# rt_a_dist_scaled:residential           
# rt_a_dist_scaled:CyclingAcci_scaled    
# rt_a_cyclTRUE:commercial               
# rt_a_dist_scaled:agricultural          
# genderwoman:Rails_scaled               
# age_scaled:MaxSpeed_scaled             
# rt_a_dist_scaled:TrafficVolume_scaled  
# genderwoman:Condition_scaled           

adolescents_model_sat10 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_cycl + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*age_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                                  Condition_scaled + Condition_scaled*rt_a_cycl +  
                                  no_cycling_infra + no_cycling_infra*age_scaled + no_cycling_infra*rt_a_dist_scaled + 
                                  non_sep_bike_lane + non_sep_bike_lane*age_scaled + non_sep_bike_lane*rt_a_dist_scaled + 
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  residential +
                                  industrial + industrial*rt_a_dist_scaled + 
                                  commercial + 
                                  recreational + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  agricultural + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat09, adolescents_model_sat10) # 10 is better than 09
summary(adolescents_model_sat10)

# Check if main effects with p > 0.20 can be removed and remove 2 interactions with p > 0.20
# age_scaled:DiffRoutes_scaled  --> better  
# rt_a_cyclTRUE:TrafficSpeed_scaled --> better
# recreational  --> worse                          
# commercial --> better                           
# other_infra  --> little bit worse                        
# rt_a_cyclTRUE --> not possible because in significant interactions                           
# Width_scaled  --> worse                      
# no_cycling_infra  --> better                      
# bike_street    --> little bit worse                               
# non_sep_bike_lane    --> better                   
# bike_path_without_cars  --> worse               
# sep_bike_lane --> worse                  
# agricultural  --> better                      
# residential   --> better                          
# genderwoman -->   not possible because in significant interactions                              
# Condition_scaled  --> worse, but better when interaction with cyclist is removed                      
# CyclingAcci_scaled --> worse                     

adolescents_model_sat11 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*rt_a_cycl + DiffRoutes_scaled*rt_a_dist_scaled + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                                  Condition_scaled +
                                  sep_bike_lane + sep_bike_lane*age_scaled + sep_bike_lane*rt_a_dist_scaled + 
                                  bike_street + bike_street*age_scaled + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + bike_path_without_cars*age_scaled + bike_path_without_cars*rt_a_dist_scaled + 
                                  other_infra + other_infra*age_scaled + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  industrial + industrial*rt_a_dist_scaled + 
                                  recreational + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat10, adolescents_model_sat11) # 11 is better than 10
summary(adolescents_model_sat11)

# Delete interactions with p > 0.20
# rt_a_dist_scaled:sep_bike_lane         
# age_scaled:sep_bike_lane               
# age_scaled:bike_path_without_cars      
# rt_a_dist_scaled:bike_path_without_cars
# age_scaled:other_infra                 
# age_scaled:bike_street                 
# rt_a_dist_scaled:DiffRoutes_scaled   

adolescents_model_sat12 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + TrafficVolume_scaled*gender +
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*rt_a_cycl +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*rt_a_cycl + 
                                  Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + 
                                  other_infra + other_infra*rt_a_cycl + other_infra*rt_a_dist_scaled +
                                  industrial + industrial*rt_a_dist_scaled + 
                                  recreational + recreational*age_scaled + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat11, adolescents_model_sat12) # 12 is better than 11
summary(adolescents_model_sat12)

# Delete interactions with p > 0.05
# age_scaled:Width_scaled             
# rt_a_cyclTRUE:MaxSpeed_scaled 
# age_scaled:recreational             
# genderwoman:TrafficVolume_scaled    
# rt_a_cyclTRUE:other_infra           
# rt_a_cyclTRUE:DiffRoutes_scaled --> little bit worse  
# rt_a_dist_scaled:Rails_scaled  --> little bit worse

adolescents_model_sat13 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  MaxSpeed_scaled + MaxSpeed_scaled*gender + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + Rails_scaled*rt_a_dist_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*rt_a_cycl + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + 
                                  other_infra +other_infra*rt_a_dist_scaled +
                                  industrial + industrial*rt_a_dist_scaled + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  natural +
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat12, adolescents_model_sat13) # 13 is similar to 12
summary(adolescents_model_sat13)

# Check if main effects and some interactions with p > 0.05 can be removed
# genderwoman --> not possible bceause in significant interactions                   
# Condition_scaled  --> worse                  
# bike_street    --> worse                     
# age_scaled  --> not possible bceause in significant interactions                           
# MaxSpeed_scaled  --> similar, so ok to remove                   
# natural    --> better                         
# other_infra  --> better                       
# industrial    --> better                      
# genderwoman:TrafficSpeed_scaled --> better 
# DiffRoutes_scaled --> worse                  
# rt_a_dist_scaled:Rails_scaled --> better      
# recreational   --> worse                     

adolescents_model_sat14 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  Rails_scaled + 
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + DiffRoutes_scaled*rt_a_cycl + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat13, adolescents_model_sat14) # 14 is better than 13
summary(adolescents_model_sat14)

# Check if main effects and some interactions with p > 0.05 can be removed
# rt_a_cyclTRUE:DiffRoutes_scaled --> similar so ok to remove
# bike_street   --> worse                       
# Condition_scaled  --> worse                   
# age_scaled  --> not possible bceause in significant interactions                             
# genderwoman   --> not possible bceause in significant interactions                           
# DiffRoutes_scaled   --> worse               
# Rails_scaled  --> similar so ok to remove                       
     
adolescents_model_sat15 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat14, adolescents_model_sat15) # 15 is similar to 14
summary(adolescents_model_sat15)

# Check if main effects and some interactions with p > 0.05 can be removed
# bike_street --> a little bit worse                         
# age_scaled   --> not possible bceause in significant interactions                             
# Condition_scaled   --> worse                 
# DiffRoutes_scaled  --> worse                 
# genderwoman --> not possible bceause in significant interactions      

adolescents_model_sat16 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + 
                                  TrafficSpeed_scaled + TrafficSpeed_scaled*rt_a_dist_scaled +
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  SideStreets_scaled + SideStreets_scaled*rt_a_dist_scaled + 
                                  DiffRoutes_scaled + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + bike_street*rt_a_cycl + bike_street*rt_a_dist_scaled + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat15, adolescents_model_sat16) # 16 and 15 are the same
summary(adolescents_model_sat16)

check_collinearity(adolescents_model_sat16)  # Some terms with high multicollinearity, but not extreme

# Check deleting terms with VIF > 10
# rt_a_dist_scaled:SideStreets_scaled --> a little bit higher AIC (+ 4), but no very high multicollinearity anymore
# rt_a_dist_scaled:TrafficSpeed_scaled --> a little bit higher AIC (+ 4), but no very high multicollinearity anymore
# rt_a_cycl:bike_street --> a little bit higher AIC, but only high multicollinearity for gender (VIF = 11)
# rt_a_dist:bike_street --> is not significant, and less relevant
# SideStreets_scale --> keep
# rt_a_cycl:Light_scaled --> keep
# TrafficSpeed_scaled --> keep

adolescents_model_sat16 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficVolume_scaled + 
                                  TrafficSpeed_scaled + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  SideStreets_scaled + 
                                  DiffRoutes_scaled + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_street + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat15, adolescents_model_sat16) # 16 is a bit worse than 15, but no high multicollinearity anymore
summary(adolescents_model_sat16)

check_collinearity(adolescents_model_sat16)  # Only high multicollinearity (but vif < 11) for gender

# Check effect of removing other irrelevant or insignificant terms
# Condition_scaled --> worse             
# DiffRoutes_scaled --> worse            
# age_scaled --> in significant interactions   
# bike_street  --> better                 
# recreational  --> worse (also interactions checked)              
# genderwoman      --> in significant interactions             
# rt_a_dist_scaled   --> worse           
# TrafficVolume_scaled  --> better      
# rt_a_cyclTRUE  --> worse               

adolescents_model_sat17 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficSpeed_scaled + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  SideStreets_scaled + 
                                  DiffRoutes_scaled + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

AIC(adolescents_model_sat16, adolescents_model_sat17) # 17 is better than 16
summary(adolescents_model_sat17)

check_collinearity(adolescents_model_sat17)  # Only high multicollinearity (but vif < 11) for gender

#### Model adolescents_model_sat17 is the best possible model with relevant factors

## CHECK THE PREDICTIONS ##
###########################

preds=predict(adolescents_model_sat17, na.action = na.exclude)
tens <- count(adolescents$ov_saf_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 0 have a prediction above 10, 47 have a value of 10 in the original data.
zeros <- count(adolescents$ov_saf_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 0 have a prediction below 0, 11 have a value of 0 in the original data.

## ASSUMPTIONS ##
#################
## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(adolescents_model_sat17)
influenceIndexPlot(adolescents_model_sat17, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 0 observations with Bonf.p < 0.05

cd = cooks.distance(adolescents_model_sat17)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(adolescents_model_sat17, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(adolescents_model_sat17))
qqline(residuals(adolescents_model_sat17))
qqPlot(residuals(adolescents_model_sat17))
# On a straight line between -2 and 1

## Homogeneity of variance
plot(adolescents_model_sat17)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(adolescents_model_sat17)
check_collinearity(adolescents_model_sat17)
# 1 term with high correlation, but < 11

## FINAL MODEL ##
#################
adolescents_model_sat17 <- lmer(ov_saf_sc~
                                  gender+
                                  age_scaled+
                                  rt_a_cycl + 
                                  rt_a_dist_scaled +
                                  TrafficSpeed_scaled + 
                                  CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + 
                                  Light_scaled + Light_scaled*gender + Light_scaled*rt_a_cycl +
                                  SideStreets_scaled + 
                                  DiffRoutes_scaled + 
                                  Width_scaled + Width_scaled*gender + 
                                  Condition_scaled +
                                  sep_bike_lane + 
                                  bike_path_without_cars + 
                                  recreational + recreational*rt_a_cycl + recreational*rt_a_dist_scaled + 
                                  (1|municipal),
                                data=adolescents,REML=FALSE, na.action = na.exclude)

summary(adolescents_model_sat17)

# Negative effect: SideStreets > TrafficSpeed > CyclingAcci_scaled
# Positive effect: bike_path_without_cars > Width_scaled > sep_bike_lane                 
# Interactions with positive effect: recreational:cyclist > CyclingAcci:women
# Interactions with negative effect: recreational:dist > CyclingAcci:age > Light:cyclist > Light:women > Width:women

