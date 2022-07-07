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

############
# SEGMENTS #
############

#### TO DO


### Load data ###
#################

### Load data
segments <- read.csv('08_dataset_segments_210402.csv', fileEncoding="UTF-8-BOM")

# Keep only necessary columns
segments <- segments[,c(1,3:7,9,11,12, 35:47), drop=FALSE]

# Delete records with 'gender = other'
segments <- segments[!(segments$gender=="other"),]

# Convert variables
segments$segment_id <- as.factor(segments$segment_id)
segments$aggsegm_id <- as.factor(segments$aggsegm_id)
segments$student_id <- as.factor(segments$student_id)
segments$school_id <- as.factor(segments$school_id)
segments$DoubleDir <- as.logical(segments$DoubleDir)
segments$Rails <- as.logical(segments$rails)
segments$CyclingInfra <- segments$cycl_infra 

segments$Condition <- as.character(segments$Condition)    
segments$Condition[segments$Condition == "excellent"] <- "5"
segments$Condition[segments$Condition == "adequate"] <- "4"
segments$Condition[segments$Condition == "deficient"] <- "3"
segments$Condition[segments$Condition == "poor"] <- "2"
segments$Condition[segments$Condition == "very poor"] <- "1"
segments$Condition <- as.numeric(segments$Condition)

segments$CyclingInfra <- as.character(segments$CyclingInfra)
segments$CyclingInfra[segments$CyclingInfra == 'bike suggestion lane'] <- "non-separated bike lane"
segments$CyclingInfra[segments$CyclingInfra == 'bike highway'] <- "bike street or lane without cars"
segments$CyclingInfra[segments$CyclingInfra == 'pedestrian zone or street'] <- "other infra"
segments$CyclingInfra[segments$CyclingInfra == 'forbidden for cyclists'] <- "other infra"
segments$CyclingInfra[segments$CyclingInfra == 'other'] <- "other infra"
segments$CyclingInfra[segments$CyclingInfra == 'no infrastructure found'] <- "no cycling infrastructure"
segments$CyclingInfra <- as.factor(segments$CyclingInfra)  

# Delete rows with NA values
segments_nona = segments[complete.cases(segments),]

# Rescale the variables
segments$age_scaled <- rescale(segments$age)
segments$length_scaled <- rescale(segments$length_m)
segments$safe_sc_scaled <- rescale(segments$safe_sc) 
segments$TrafficVolume_scaled <- rescale(segments$TrafficVolume) 
segments$TrafficSpeed_scaled <- rescale(segments$TrafficSpeed) 
segments$MaxSpeed_scaled <- rescale(segments$maxspeed) 
segments$CyclingAcci_scaled <- rescale(segments$cyclingacci) 
segments$Light_scaled <- rescale(segments$light.y) 
segments$WidthRoad_scaled <- rescale(segments$widthroad) 
segments$Width_scaled <- rescale(segments$width) 
segments$Condition_scaled <- rescale(segments$Condition) 

### EFFECT OF INCLUDING CONDITION, WIDTH, DOUBLEDIR OR NOT ###
##############################################################

# Delete columns condition, width and doubledir
drops <- c('Condition', 'Condition_scaled', 'width', 'Width_scaled', 'DoubleDir')
segments_noincompletevar <- segments[,!(names(segments) %in% drops)]
segments_noincompletevar_nona = segments_noincompletevar[complete.cases(segments_noincompletevar),]
# Much more incomplete records, but still 23 953 records included (versus 68 883 if incomplete variables are not included)

### Saturated lineair model without random effects (with rescaled variables) - WITH incomplete variables ###
segments_model_sat00_a <- lm(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                             WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                             Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                             DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             length_scaled, 
                           data=segments, REML=TRUE, na.action = na.exclude)

summary(segments_model_sat00_a) 

### Saturated lineair model wihtout random effects (with rescaled variables) - WITHOUT incomplete variables ###
segments_model_sat00_b <- lm(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               # Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               # Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               # DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled, 
                             data=segments_noincompletevar, REML=TRUE, na.action = na.exclude)

summary(segments_model_sat00_b) 

AIC(segments_model_sat00_a, segments_model_sat00_b)
# Similar results for both models
# AIC of model a is much smaller than model b --> Include incomplete variables in the analysis

### SATURATED MODEL ###
#######################

### Saturated lineair model without random effects (with rescaled variables) - WITH incomplete variables ###
segments_model_sat00 <- lm(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled, 
                             data=segments, REML=TRUE, na.action = na.exclude)

summary(segments_model_sat00) 

# Negative effect: traffic volume, age, max speed, cycling accidents, CyclingInfra (no, non-sep, other), width road, double dir
# Positive effect: length
# Interactions with positive effect: traffic volume:age, traffic speed:age, traffic speed: cyclist, max speed:women, cycling infra: women, cycling infra: age, width road: women, doubledir:age, doubledir:cyclist
# Interactions with negative effect: traffic volume: cyclist, traffic volume:time cat late, cyclingacci: length, cyclinginfra: cyclist, cyclinginfra: length, light:cyclist, doubledir:women

### RANDOM EFFECT SEARCH ###

# RE: geagg_segment_id
segments_model_sat01 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                             WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                             Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                             DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             length_scaled +
                              (1|aggsegm_id), 
                           data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat00, segments_model_sat01) # 01 is better than 00 (much lower (but negative) AIC)
summary(segments_model_sat01) # unrealistic results, a lot of p-values of 1

# RE: geagg_segment_id in student_id
segments_model_sat02 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id/aggsegm_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat00, segments_model_sat02) # 02 is better than 00 (much lower (but negative) AIC)
AIC(segments_model_sat01, segments_model_sat02) # 02 is better than 01 (lower AIC)
summary(segments_model_sat02) # unrealistic results, a lot of p-values of 1

# RE: student_id
segments_model_sat03 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat00, segments_model_sat03) # 03 is better than 00 (lower AIC)
AIC(segments_model_sat02, segments_model_sat03) # 02 is better than 03 (much lower (but negative) AIC)
summary(segments_model_sat03) # Realistic results

# RE: segment_id
segments_model_sat04 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat04) # 03 is better than 04 (lower AIC)
AIC(segments_model_sat02, segments_model_sat04) # 02 is better than 04 (much lower (but negative) AIC)
summary(segments_model_sat04) # Realistic results

# RE: segment_id in municipality
segments_model_sat05 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|municipal/segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat05) # 03 is better than 05 (lower AIC)
AIC(segments_model_sat02, segments_model_sat05) # 02 is better than 05 (much lower (but negative) AIC)
summary(segments_model_sat05) # Realistic results

# RE: aggsegm_id + segment_id
segments_model_sat06 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|aggsegm_id) + (1|segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat06) # 06 is better than 03 (much lower (but negative) AIC)
AIC(segments_model_sat02, segments_model_sat06) # 02 is better than 06 (lower AIC)
summary(segments_model_sat06) # unrealistic results, a lot of p-values of 1

# RE: aggsegm_id in student_id + road_segment_id
segments_model_sat07 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id/aggsegm_id) + (1|segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat07) # 07 is better than 03 (much lower (but negative) AIC)
AIC(segments_model_sat02, segments_model_sat07) # 02 is better than 07 (lower AIC)
summary(segments_model_sat07) # unrealistic results, a lot of p-values of 1

# RE: aggsegment in student_id + segment_id in municipal
segments_model_sat08 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id/aggsegm_id) + (1|municipal/segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat08) # 08 is better than 03 (much lower (but negative) AIC)
AIC(segments_model_sat02, segments_model_sat08) # 02 is better than 08 (lower AIC)
summary(segments_model_sat08) # unrealistic results, a lot of p-values of 1

# RE: student_id + segment_id
segments_model_sat09 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat03, segments_model_sat09) # 09 is better than 03 (lower AIC)
AIC(segments_model_sat02, segments_model_sat09) # 02 is better than 09 (much lower (but negative) AIC)
summary(segments_model_sat09) # Realistic results

# RE: student_id + segment_id + aggsegm_id
segments_model_sat10 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id) + (1|aggsegm_id), 
                             data=segments, REML=TRUE, na.action = na.exclude)

AIC(segments_model_sat09, segments_model_sat10) # 10 is better than 09 (much lower (but negative) AIC)
AIC(segments_model_sat02, segments_model_sat10) # 02 is better than 10 (lower AIC)
summary(segments_model_sat10) # unrealistic results, a lot of p-values of 1

# 02 has the lowest AIC, but unrealistic results.
# 09 has the lowest AIC with realistic results.
# Include random effect of student_id and segment_id

### EFFECT OF INCLUDING CONDITION, WIDTH, DOUBLEDIR OR NOT ###
##############################################################

# Delete columns condition, width and doubledir
drops <- c('Condition', 'Condition_scaled', 'width', 'Width_scaled', 'DoubleDir')
segments_noincompletevar <- segments[,!(names(segments) %in% drops)]
segments_noincompletevar_nona = segments_noincompletevar[complete.cases(segments_noincompletevar),]
# Much more incomplete records, but still 23 953 records included (versus 68 883 if incomplete variables are not included)

### Saturated lineair model without random effects (with rescaled variables) - WITH incomplete variables ###
segments_model_sat09_a <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                                 (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

summary(segments_model_sat09_a) 

### Saturated lineair model wihtout random effects (with rescaled variables) - WITHOUT incomplete variables ###
segments_model_sat09_b <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                               CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                               Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               # Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                               # Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                               # DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                                 (1|student_id) + (1|segment_id), 
                             data=segments_noincompletevar, REML=FALSE, na.action = na.exclude)

summary(segments_model_sat09_b) 

AIC(segments_model_sat09_a, segments_model_sat09_b)
# AIC of model b is much larger than model a 
# Much more significant (but seems not logical) results for model b
# Keep working with a


### FIXED EFFECT SEARCH ###
###########################

segments_model_sat09<- lmer(safe_sc~
                                 TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                                 TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                                 MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*length_scaled +
                                 CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                                 CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                                 Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                                 Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + Rails*length_scaled +
                                 WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*age_scaled + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                                 Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                                 Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + Condition_scaled*length_scaled +
                                 DoubleDir + DoubleDir*gender + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                                 time_cat + 
                                 cyclist + 
                                 gender + 
                                 age_scaled +
                                 length_scaled +
                                 (1|student_id) + (1|segment_id), 
                               data=segments, REML=FALSE, na.action = na.exclude)

summary(segments_model_sat09)

# Negative effect: traffic volume, time_catlate, traffic speed, cyclinginfra, width road
# Positive effect: width
# Interactions with positive effect: traffic volume:age, traffic volume:time med, max speed:women, cycling infra: women, cyclinginfra: cyclist, width road: women, width road:cyclist, width: age, doubledir:age, doubledir:cyclist
# Interactions with negative effect: traffic volume: cyclist, traffic volume:time cat late, traffic speed:women, traffic speed:length, max speed: age, cyclingacci: length, cyclingacci: cyclist, cyclinginfra:age, light:cyclist, width:gender


# Delete all interactions with p > 0.80
# genderwoman:DoubleDirTRUE                                 
# length_scaled:Condition_scaled                            
# genderwoman:CyclingInfranon-separated bike lane --> not yet   
# cyclistTRUE:MaxSpeed_scaled
# length_scaled:MaxSpeed_scaled                             
# age_scaled:CyclingInfrabike street or lane without cars --> not yet   
# age_scaled:WidthRoad_scaled                               
# length_scaled:RailsTRUE                                   

segments_model_sat11<- lmer(safe_sc~
                              TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                              TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                              MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                              CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                              CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                              Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                              Rails + Rails*gender + Rails*age_scaled + Rails*cyclist + 
                              WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                              Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*cyclist + Width_scaled*length_scaled +
                              Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + 
                              DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                              time_cat + 
                              cyclist + 
                              gender + 
                              age_scaled +
                              length_scaled +
                              (1|student_id) + (1|segment_id), 
                            data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat09, segments_model_sat11) # 11 is better than 09
summary(segments_model_sat11)

# Delete all interactions with p > 0.60
# genderwoman:CyclingInfranon-separated bike lane --> not yet           
# age_scaled:CyclingInfrabike street or lane without cars  --> not yet 
# cyclistTRUE:Width_scaled                                  
# cyclistTRUE:RailsTRUE                                     
# length_scaled:CyclingInfraseparated bike lane --> not (because lower AIC)            

segments_model_sat12 <- lmer(safe_sc~
                              TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                              TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                              MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                              CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + CyclingAcci_scaled*length_scaled +
                              CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                              Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat + Light_scaled*length_scaled +
                              Rails + Rails*gender + Rails*age_scaled +
                              WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                              Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*length_scaled +
                              Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist + 
                              DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat + DoubleDir*length_scaled +
                              time_cat + 
                              cyclist + 
                              gender + 
                              age_scaled +
                              length_scaled +
                              (1|student_id) + (1|segment_id), 
                            data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat11, segments_model_sat12) # 12 is better than 11
summary(segments_model_sat12)

# Delete interactions with p > 0.40
# genderwoman:CyclingInfranon-separated bike lane --> not yet      
# age_scaled:CyclingInfrabike street or lane without cars --> not yet 
# length_scaled:CyclingInfraseparated bike lane --> not yet     
# genderwoman:CyclingAcci_scaled                            
# genderwoman:RailsTRUE                                     
# cyclistTRUE:Condition_scaled
# length_scaled:DoubleDirTRUE                               
# time_catmedium:Light_scaled

segments_model_sat13 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + Width_scaled*length_scaled +
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat12, segments_model_sat13) # 13 is better than 12
summary(segments_model_sat13)

# Delete all interactions with length and its main effect
segments_model_sat14 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat + 
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                               Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + 
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat14) # 13 is better than 14
summary(segments_model_sat13)

# Delete interactions with p > 0.20
# genderwoman:CyclingInfranon-separated bike lane  --> not yet         
# age_scaled:CyclingInfrabike street or lane without cars   --> not yet
# length_scaled:CyclingInfranon-separated bike lane  --> ??       
# cyclistTRUE:TrafficSpeed_scaled                           
# time_catlate:DoubleDirTRUE                                
# age_scaled:Light_scaled                                   
# TrafficVolume_scaled:time_catlate --> not yet 
# genderwoman:Condition_scaled                              
# genderwoman:Light_scaled                                  
# age_scaled:Condition_scaled                               
# length_scaled:Width_scaled                                
# age_scaled:CyclingAcci_scaled                             

segments_model_sat15 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat15) # 13 is better than 15, but difference is not very large
summary(segments_model_sat15)

# Delete interactions of length and age with cycling infrastructure
segments_model_sat16 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender +CyclingInfra*cyclist +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat15, segments_model_sat16) # 16 is worse
summary(segments_model_sat15)

# Delete main effect of Condition
segments_model_sat17 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat15, segments_model_sat17) # 17 is much worse

# Delete main effect and interactions of Rails
segments_model_sat18 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Condition_scaled +
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat15, segments_model_sat18) # 18 is worse
summary(segments_model_sat13)

# Delete interactions with p > 0.20, test one by one which deletions provide a better model
# genderwoman:CyclingInfranon-separated bike lane  --> worse    
# age_scaled:CyclingInfrabike street or lane without cars   --> worse
# length_scaled:CyclingInfranon-separated bike lane  --> worse       
# cyclistTRUE:TrafficSpeed_scaled --> better                          
# time_catlate:DoubleDirTRUE --> worse                             
# age_scaled:Light_scaled --> better                                   
# TrafficVolume_scaled:time_catlate --> similar so ok to remove
# genderwoman:Condition_scaled --> similar so ok to remove                             
# genderwoman:Light_scaled --> similar so ok to remove                                  
# age_scaled:Condition_scaled --> better                             
# length_scaled:Width_scaled --> better
# age_scaled:CyclingAcci_scaled --> better                            

segments_model_sat19 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*length_scaled +
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat13, segments_model_sat19) # 19 is better than 13
summary(segments_model_sat19)

# Delete interactions with p > 0.10, test one by one which deletions provide a better model
# genderwoman:CyclingInfranon-separated bike lane --> worse         
# age_scaled:CyclingInfrabike street or lane without cars --> worse 
# Condition_scaled --> worse
# length_scaled:CyclingInfranon-separated bike lane --> worse        
# time_catlate:DoubleDirTRUE --> worse                            
# TrafficVolume_scaled:genderwoman --> similar, so ok to remove                       
# cyclistTRUE:CyclingInfranon-separated bike lane --> worse           
# age_scaled:TrafficSpeed_scaled --> similar, so ok to remove                           
# TrafficVolume_scaled:length_scaled --> similar, so ok to remove                        

segments_model_sat20 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*cyclist +  CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + Light_scaled*length_scaled +
                               Rails + Rails*age_scaled +
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + WidthRoad_scaled*length_scaled +
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat19, segments_model_sat20) # 20 is similar to 19
summary(segments_model_sat20)

# Delete interactions with p > 0.05, test one by one which deletions provide a better model
# age_scaled:CyclingInfrabike street or lane without cars --> worse, but not a lot worse
# genderwoman:CyclingInfranon-separated bike lane   --> worse        
# Condition_scaled --> worse
# length_scaled:CyclingInfranon-separated bike lane --> worse, but not a lot worse        
# time_catlate:DoubleDirTRUE   --> worse, but not a lot worse                                 
# cyclistTRUE:CyclingInfranon-separated bike lane --> worse      
# genderwoman:WidthRoad_scaled  --> worse, but not a lot worse                              
# age_scaled:RailsTRUE  --> similar, so ok to remove                                  
# length_scaled:Light_scaled  --> similar, so ok to remove                              
# length_scaled:WidthRoad_scaled --> similar, so ok to remove                           
# cyclistTRUE:CyclingAcci_scaled --> similar, so ok to remove                             
# TrafficSpeed_scaled:time_catlate  --> worse, but not a lot worse                 

segments_model_sat21 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*time_cat + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + CyclingInfra*length_scaled +
                               Light_scaled + Light_scaled*cyclist + 
                               Rails + 
                               WidthRoad_scaled + WidthRoad_scaled*gender + WidthRoad_scaled*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat20, segments_model_sat21) # 21 is similar to 20 (a little bit worse)
summary(segments_model_sat21)

# Delete interactions that give worse, but not too worse results
# length_scaled:CyclingInfranon-separated bike lane --> worse, but not a lot worse and none of the interactions was significant      
# genderwoman:WidthRoad_scaled  --> worse, but not a lot worse and none of the interactions was significant     
# TrafficSpeed_scaled:time_catlate  --> worse, but not a lot worse and none of the interactions was significant  
# age_scaled:CyclingInfrabike street or lane without cars --> worse
# time_catlate:DoubleDirTRUE   --> worse                               

segments_model_sat22 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                               Light_scaled + Light_scaled*cyclist + 
                               Rails + 
                               WidthRoad_scaled + WidthRoad_scaled*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat21, segments_model_sat22) # 22 is similar to 21 (a little bit worse)
summary(segments_model_sat22)

# Test if insignificant terms can be deleted one by one
# cyclistTRUE --> not possible because in significant interactions                                            
# genderwoman:CyclingInfranon-separated bike lane --> worse   
# age_scaled:CyclingInfrabike street or lane without cars --> worse
# TrafficSpeed_scaled  --> not possible, because significant interactions                                   
# CyclingAcci_scaled --> ????                                     
# Condition_scaled  --> much worse                                      
# CyclingInfraseparated bike lane    --> not possible, because significant interactions
# genderwoman  --> not possible because in significant interactions                                            
# DoubleDirTRUE:time_catlate --> worse                             
# age_scaled  --> not possible because in significant interactions                                              
# MaxSpeed_scaled  --> not possible, because significant interactions                                       
# cyclistTRUE:CyclingInfranon-separated bike lane --> worse
# RailsTRUE --> similar, so ok to remove                                              
# WidthRoad_scaled  --> similar, so ok to remove                                      
# time_catlate --> worse                                           

segments_model_sat23 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*length_scaled +
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + CyclingAcci_scaled*length_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                               Light_scaled + Light_scaled*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat22, segments_model_sat23) # 23 is similar to 22
summary(segments_model_sat23)

# Delete main effect and interactions with length
segments_model_sat24 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               TrafficSpeed_scaled + TrafficSpeed_scaled*gender + 
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                               Light_scaled + Light_scaled*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat23, segments_model_sat24) # 24 is similar to 23 (a little bit worse: < 10)
summary(segments_model_sat24)

# Test if insignificant terms can be deleted one by one
# genderwoman:CyclingInfranon-separated bike lane --> worse          
# age_scaled:CyclingInfrabike street or lane without cars  --> worse
# TrafficSpeed_scaled --> similar, so ok to remove                                     
# Condition_scaled  --> worse                                       
# cyclistTRUE --> not possible because in significant interactions                                          
# CyclingInfraseparated bike lane  --> not possible because significant interactions                        
# genderwoman  --> not possible because in significant interactions                                                 
# MaxSpeed_scaled --> worse                                         
# DoubleDirTRUE:time_catlate  --> worse                             
# DoubleDirTRUE --> worse                                           
# age_scaled --> not possible because in significant interactions                                                   
# cyclistTRUE:CyclingInfranon-separated bike lane    --> worse      
# Light_scaled  --> similar, so ok to remove (AIC diff < 7)                                           
# time_catlate  --> worse  

segments_model_sat25 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat24, segments_model_sat25) # 25 is similar to 24 (a little bit worse)
summary(segments_model_sat25)

check_collinearity(segments_model_sat25)  # High multicollineairty for terms with CyclingInfra

# Delete interaction CyclingInfra:cyclist because very high VIF
segments_model_sat26 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled +  
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

AIC(segments_model_sat25, segments_model_sat26) # 26 has a high AIC (worse)
summary(segments_model_sat26)
check_collinearity(segments_model_sat26)  # High multicollineairty for terms with CyclingInfra, but much smaller (no extreme VIFs)

#### Model segments_model_sat26 is the best possible model with relevant factors

## CHECK THE PREDICTIONS ##
###########################

preds=predict(segments_model_sat26, na.action = na.exclude)
tens <- count(segments$safe_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 56 have a prediction above 10, 4753 have a value of 10 in the original data. (< 10.6)
zeros <- count(segments$safe_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 13 have a prediction below 0, 808 have a value of 0 in the original data. (> -0.16)

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(segments_model_sat26)
influenceIndexPlot(segments_model_sat26, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 10 observations with Bonf.p < 0.05

cd = cooks.distance(segments_model_sat26)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(segments_model_sat26, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(segments_model_sat26))
qqline(residuals(segments_model_sat26))
qqPlot(residuals(segments_model_sat26))
# On a straight line between -1 and 1

## Homogeneity of variance
plot(segments_model_sat26)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(segments_model_sat26)
check_collinearity(segments_model_sat26)
# 2 terms with high correlation, but < 232

## FINAL MODEL ##
#################
segments_model_sat26 <- lmer(safe_sc~
                               TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + 
                               MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + 
                               CyclingAcci_scaled + 
                               CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled +  
                               Width_scaled + Width_scaled*gender + Width_scaled*age_scaled + 
                               Condition_scaled + 
                               DoubleDir + DoubleDir*age_scaled + DoubleDir*cyclist + DoubleDir*time_cat +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|student_id) + (1|segment_id), 
                             data=segments, REML=FALSE, na.action = na.exclude)

summary(segments_model_sat26)

# Negative effect: TrafficVolume > No cycling infrastructure > CyclingAcci > Non-separated bike lane
# Positive effect: Width > Cyclist > Separated bike lane > Bike street without cars > time medium
# Interactions with negative effect: No cycling infra:age > MaxSpeed:age > TrafficVolume:cyclist > Width:women > Separated bike lane:women > Other infra:women > DoubleDir:time medium 
# Interactions with positive effect: TrafficVolume:age > Width:age > MaxSpeed:women > No cycling infra:women > DoubleDir:age > DoubleDir:cyclist 
