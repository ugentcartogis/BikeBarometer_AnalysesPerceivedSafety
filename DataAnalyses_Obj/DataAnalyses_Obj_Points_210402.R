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

##########
# POINTS #
##########


### Load data ###
#################

### Load data
points <- read.csv('08_dataset_points_210402.csv', fileEncoding="UTF-8-BOM")

# Keep only necessary columns
points <- points[,c(1,2,5,6,8,28:40), drop=FALSE]

# Delete records with 'gender = other'
points <- points[!(points$gender=="other"),]

# Convert variables
points$node_id <- as.factor(points$node_id)
points$student_id <- as.factor(points$student_id)
points$Rails <- points$rails 

points$Condition <- as.character(points$Condition)    
points$Condition[points$Condition == "excellent"] <- "5"
points$Condition[points$Condition == "adequate"] <- "4"
points$Condition[points$Condition == "deficient"] <- "3"
points$Condition[points$Condition == "poor"] <- "2"
points$Condition[points$Condition == "very poor"] <- "1"
points$Condition <- as.numeric(points$Condition)

points$CyclingInfra <- as.character(points$CyclingInfra)
points$CyclingInfra[points$CyclingInfra == 'bike suggestion lane'] <- "non-separated bike lane"
points$CyclingInfra[points$CyclingInfra == 'bike highway'] <- "bike street or lane without cars"
points$CyclingInfra[points$CyclingInfra == 'pedestrian zone or street'] <- "other infra"
points$CyclingInfra[points$CyclingInfra == 'forbidden for cyclists'] <- "other infra"
points$CyclingInfra[points$CyclingInfra == 'other'] <- "other infra"
points$CyclingInfra[points$CyclingInfra == 'no infrastructure found'] <- "no cycling infrastructure"
points$CyclingInfra <- as.factor(points$CyclingInfra)  

# Delete rows with NA values
points_nona = points[complete.cases(points),]



# Rescale the variables
points$age_scaled <- rescale(points$age)
points$safe_sc_scaled <- rescale(points$safe_sc) 
points$TrafficVolume_scaled <- rescale(points$TrafficVolume) 
points$TrafficSpeed_scaled <- rescale(points$TrafficSpeed) 
points$MaxSpeed_scaled <- rescale(points$MaxSpeed) 
points$CyclingAcci_scaled <- rescale(points$CyclingAcci) 
points$Light_scaled <- rescale(points$Light) 
points$NrRoads_scaled <- rescale(points$NrRoads) 
points$Direction_scaled <- rescale(points$direction) 
points$Condition_scaled <- rescale(points$Condition) 

### EFFECT OF INCLUDING CONDITION OR NOT ###
############################################

# Delete column condition
drops <- c('Condition', 'Condition_scaled')
points_nocondition <- points[,!(names(points) %in% drops)]
points_nocondition_nona = points_nocondition[complete.cases(points_nocondition),]

### Saturated lineair model wihtout random effects (with rescaled variables) - WITH condition variable ###
points_model_sat00_a <- lm(safe_sc~
                           TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                           TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                           MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                           CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                           Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                           Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                           NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                           CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                           Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                           Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                           time_cat + 
                           cyclist + 
                           gender + 
                           age_scaled, 
                         data=points, REML=TRUE, na.action = na.exclude)

summary(points_model_sat00_a) 

### Saturated lineair model wihtout random effects (with rescaled variables) - WITHOUT condition variable ###
points_model_sat00_b <- lm(safe_sc~
                           TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                           TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                           MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                           CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                           Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                           Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                           NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                           CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                           Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                           #Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                           time_cat + 
                           cyclist + 
                           gender + 
                           age_scaled, 
                         data=points_nocondition, REML=TRUE, na.action = na.exclude)

summary(points_model_sat00_b) 

AIC(points_model_sat00_a, points_model_sat00_b)
# Similar results for both models
# AIC of model a is much smaller than model b --> Include condition in the analysis

### SATURATED MODEL ###
#######################

### Saturated lineair model wihtout random effects (with rescaled variables) - WITH condition variable ###
points_model_sat00 <- lm(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled, 
                           data=points, REML=TRUE, na.action = na.exclude)

summary(points_model_sat00) 

# Negative effect: traffic volume, max speed, cycling accidents,
# Positive effect: girls,
# Interactions with positive effect: traffic volume:girls, traffic volume:time cat late, max speed:cyclist, light:age, rails:cyclist, condition:cyclist
# Interactions with negative effect: traffic speed:girls, light:girls, rails:age, nr roads:girls, cycling infra:girls, cycling infra:cyclist

### RANDOM EFFECT SEARCH ###

# RE: student_id
points_model_sat01a <- lmer(safe_sc~
                           TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                           TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                           MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                           CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                           Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                           Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                           NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                           CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                           Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                           Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                           time_cat + 
                           cyclist + 
                           gender + 
                           age_scaled +
                           (1|student_id), 
                         data=points, REML=TRUE, na.action = na.exclude)

AIC(points_model_sat00, points_model_sat01a)  # 01a is better (lower AIC)
summary(points_model_sat01a) 

# RE: node_id
points_model_sat01b <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

AIC(points_model_sat00, points_model_sat01b)  # 01b is better than 00 (lower AIC)
AIC(points_model_sat01a, points_model_sat01b)  # 01a is better than 01 (lower AIC)
summary(points_model_sat01b) 

# RE: student_id, RE: node_id
points_model_sat02 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

AIC(points_model_sat00, points_model_sat02)  # 02 is better than 00 (lower AIC)
AIC(points_model_sat01a, points_model_sat02)  # 02 is better than 01a (lower AIC)
summary(points_model_sat02) 

# RE: student_id, RE: municipality / node_id
points_model_sat03 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|municipal/node_id) + (1|student_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

AIC(points_model_sat00, points_model_sat03)  # 03 is better than 00 (lower AIC)
AIC(points_model_sat02, points_model_sat03)  # 03 is very similar to 02 (lower AIC)
summary(points_model_sat03) 

# 02 has the lowest AIC
# Include random effect of student_id and node_id

### FIXED EFFECT SEARCH ###
###########################
points_model_sat02 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + Light_scaled*cyclist + Light_scaled*time_cat +
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*age_scaled + Direction_scaled*cyclist + Direction_scaled*time_cat +
                            Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

summary(points_model_sat02) # Random effect variance of node_id = 0.48, of student_id = 3.13

# Negative effect: traffic volume, age, max speed, cycling accidents, rails
# Positive effect: girls, cyclist, traffic speed, 
# Interactions with positive effect: traffic volume:age,  max speed:time cat medium, light:age, cyclinginfra:age
# Interactions with negative effect:traffic speed:time cat medium, cyclingacci:age, light: girls, rails:age, nr roads:girls, cycling infra:girls, cycling infra:cyclist


# Delete all interactions with p > 0.80
# time_catlate:Direction_scaled                           
# time_catmedium:Direction_scaled                         
# time_catlate:Light_scaled                               
# age_scaled:Direction_scaled                             
# cyclistTRUE:Light_scaled                                

points_model_sat04 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*gender + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*gender + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat02, points_model_sat04) # 04 is better than 02
summary(points_model_sat04)

# Delete interactions with p > 0.60
# genderwoman:CyclingInfrabike street or lane without cars --> not yet  
# age_scaled:CyclingInfraother infra  --> not yet                      
# TrafficVolume_scaled:genderwoman                          
# genderwoman:Condition_scaled                              
# TrafficVolume_scaled:time_catmedium --> not yet                     
points_model_sat05 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*cyclist + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*gender + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + Rails*cyclist +
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*age_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat04, points_model_sat05) # 05 is better than 04
summary(points_model_sat05)

# Delete interactions with p > 0.40
# genderwoman:CyclingInfrabike street or lane without cars --> not yet
# age_scaled:CyclingInfraother infra --> not yet                      
# TrafficVolume_scaled:time_catmedium --> not yet                     
# cyclistTRUE:Railstrue                                   
# age_scaled:Condition_scaled                             
# TrafficVolume_scaled:cyclistTRUE                        
# genderwoman:CyclingAcci_scaled                          
# time_catlate:MaxSpeed_scaled --> not yet
points_model_sat06 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*gender + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*age_scaled + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + 
                             NrRoads_scaled + NrRoads_scaled*gender + NrRoads_scaled*age_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat05, points_model_sat06) # 06 is better than 05
summary(points_model_sat06)

# Delete interactions with p > 0.20
# genderwoman:CyclingInfrabike street or lane without cars --> not yet
# age_scaled:CyclingInfraother infra --> not yet                      
# TrafficVolume_scaled:time_catmedium --> not yet                     
# cyclistTRUE:CyclingInfrabike street or lane without cars --> not yet
# time_catlate:MaxSpeed_scaled --> not yet                            
# genderwoman:NrRoads_scaled                              
# age_scaled:NrRoads_scaled                               
# time_catlate:TrafficSpeed_scaled --> not yet                      
# TrafficSpeed_scaled:genderwoman                         
# age_scaled:MaxSpeed_scaled                              

points_model_sat07 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + 
                             NrRoads_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat06, points_model_sat07) # 07 is better than 06
summary(points_model_sat07)

# Delete main effect and interactions of Condition
points_model_sat08 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + 
                             NrRoads_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07, points_model_sat08) # 07 is better than 08

# Delete main effect and interactions of Direction
points_model_sat09 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + 
                             NrRoads_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Condition_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07, points_model_sat09) # 07 is better than 09

# Delete main effect and interactions of NrRoads
points_model_sat10 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails + Rails*gender + Rails*age_scaled + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07, points_model_sat10) # 07 is better than 10

# Delete main effect and interactions of Rails
points_model_sat11 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*cyclist + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             NrRoads_scaled + NrRoads_scaled*cyclist + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + Direction_scaled*cyclist +
                             Condition_scaled + Condition_scaled*cyclist +
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07, points_model_sat11) # 07 is better than 10

# Delete interactions with p > 0.10
# genderwoman:CyclingInfrabike street or lane without cars --> not yet
# age_scaled:CyclingInfraother infra --> not yet                      
# TrafficVolume_scaled:time_catmedium --> not yet                     
# cyclistTRUE:CyclingInfrabike street or lane without cars --> not yet
# time_catlate:MaxSpeed_scaled --> not yet                           
# time_catlate:TrafficSpeed_scaled  --> not yet                    
# TrafficSpeed_scaled:cyclistTRUE                         
# age_scaled:CyclingInfrabike street or lane without cars --> not yet
# cyclistTRUE:Direction_scaled                            
# cyclistTRUE:NrRoads_scaled                              
# cyclistTRUE:Condition_scaled                            
# age_scaled:Railstrue                                    
# genderwoman:Railstrue
points_model_sat12 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*age_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*gender + MaxSpeed_scaled*cyclist + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled + CyclingAcci_scaled*cyclist + 
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07, points_model_sat12) # 07 is similar as 12, so keep the simplest model (12)
summary(points_model_sat12)

# Delete interactions with p > 0.05
# genderwoman:CyclingInfrabike street or lane without cars --> not yet
# age_scaled:CyclingInfraother infra --> not yet                      
# TrafficVolume_scaled:time_catmedium --> not yet                     
# cyclistTRUE:CyclingInfrabike street or lane without cars --> not yet
# time_catlate:MaxSpeed_scaled --> not yet 
# age_scaled:CyclingInfrabike street or lane without cars --> not yet
# time_catlate:TrafficSpeed_scaled  --> not yet                        
# MaxSpeed_scaled:cyclistTRUE                             
# cyclistTRUE:CyclingAcci_scaled                          
# MaxSpeed_scaled:genderwoman                             
# age_scaled:TrafficSpeed_scaled                          
points_model_sat13 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat12, points_model_sat13) # 13 is similar as 12, so keep the simplest model (13)
summary(points_model_sat13)
                          
# Delete CyclingInfra:gender and CyclingInfra:age_scaled
points_model_sat14 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             CyclingInfra + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat13, points_model_sat14) # 13 is somewhat better than 14, but similar
summary(points_model_sat14)

# Delete time_cat and its interactions
points_model_sat15 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + 
                             TrafficSpeed_scaled +
                             MaxSpeed_scaled + 
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat13, points_model_sat15) # 13 is  better than 15
summary(points_model_sat15)

# Delete various main effects and check if the model improves:
# Condition
# Rails
# NrRoads_scaled
# Direction_scaled
# Light_scaled
points_model_sat16 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             # Light_scaled + Light_scaled*gender + Light_scaled*age_scaled +
                             # Rails +
                             # NrRoads_scaled + 
                             CyclingInfra + CyclingInfra*gender + CyclingInfra*age_scaled + CyclingInfra*cyclist + 
                             # Direction_scaled + Direction_scaled*gender + 
                             # Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat13, points_model_sat16) # 13 is always better than 16. 
# Removing Condition or Direction strongly increases the AIC.
# Removing Rails, NrRoads, Light or CyclingInfra slightly increases the AIC.
summary(points_model_sat13)

check_collinearity(points_model_sat13)  # High multicollineairty for terms with CyclingInfra

# Delete main effect and interactions of CyclingInfra
# because no significant main effects for the groups, only differences in gender, age and cyclists --> less meaningfull or interesting
points_model_sat17 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + MaxSpeed_scaled*time_cat +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat13, points_model_sat17) # 13 is better than 17 (AIC 17 - AIC 13 = 130), but no extreme multicollinearity anymore
check_collinearity(points_model_sat17)

# Delete main effect and interactions of time_cat
points_model_sat18 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled +
                             TrafficSpeed_scaled + 
                             MaxSpeed_scaled +
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat17, points_model_sat18) # 17 is better than 18 (AIC 17 - AIC 18 < 20)
summary(points_model_sat18)
check_collinearity(points_model_sat18)  # Only low multicollinearity
# Keep time_cat because some interesting interactions e.g. traffic volume and time cat, 

# Delete interaction MaxSpeed:time_cat
points_model_sat19 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + 
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat17, points_model_sat19) # 19 is similar as 17 (AIC 19 - AIC 17 < 2) --> keep the simplest model (19)
check_collinearity(points_model_sat19)
summary(points_model_sat19)

# Delete interaction TrafficSpeed:time_cat (high multicollinearity)
points_model_sat20 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + 
                             MaxSpeed_scaled + 
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat19, points_model_sat20) # 19 is better than 20
check_collinearity(points_model_sat20)
summary(points_model_sat20)
# Keep the interaction TrafficSpeed:time_cat because significant and no huge VIFs (but still high)

#### Model points_model_sat19 has the lowest possible AIC with relevant factors

## CHECK THE PREDICTIONS ##
###########################

preds=predict(points_model_sat19, na.action = na.exclude)
tens <- count(points$safe_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 6 have a prediction above 10, 2374 have a value of 10 in the original data. (< 10.9)
zeros <- count(points$safe_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 6 have a prediction below 0, 800 have a value of 0 in the original data. (> -0.7)

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(points_model_sat19)
influenceIndexPlot(points_model_sat19, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 9 observations with Bonf.p < 0.05

cd = cooks.distance(points_model_sat19)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(points_model_sat19, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(points_model_sat19))
qqline(residuals(points_model_sat19))
qqPlot(residuals(points_model_sat19))
# On a straight line between -1.5 and 1.5

## Homogeneity of variance
plot(points_model_sat19)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(points_model_sat19)
check_collinearity(points_model_sat19)
# 2 terms with high correlation, but < 31

## FINAL MODEL ##
#################
points_model_sat19 <- lmer(safe_sc~
                             TrafficVolume_scaled + TrafficVolume_scaled*age_scaled + TrafficVolume_scaled*time_cat +
                             TrafficSpeed_scaled + TrafficSpeed_scaled*time_cat +
                             MaxSpeed_scaled + 
                             CyclingAcci_scaled + CyclingAcci_scaled*age_scaled +
                             Light_scaled + Light_scaled*gender + Light_scaled*age_scaled + 
                             Rails +
                             NrRoads_scaled + 
                             Direction_scaled + Direction_scaled*gender + 
                             Condition_scaled + 
                             time_cat + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

summary(points_model_sat19)

# Negative effect: TrafficVolume > CyclingAcci > NrRoads > Rails
# Positive effect: TrafficSpeed > time_cat medium > Condition
# Interactions with negative effect: CyclingAcci:age > TrafficSpeed:time medium > Light:woman > Direction:woman
# Interactions with positive effect: TrafficVolmue:age > Light:age > TrafficVolume:time late

