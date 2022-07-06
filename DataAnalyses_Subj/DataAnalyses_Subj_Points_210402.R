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

rm(list=ls())

##########
# POINTS #
##########

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

# Load data
points <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/06_dataset_points_210402.csv')

# Delete records with 'gender = other'
points <- points[!(points$gender=="other"),]

# Convert variables
points$node_id <- as.factor(points$node_id)
points$student_id <- as.factor(points$student_id)
points$school_id <- as.factor(points$school_id)
points$time <- as.numeric(as.POSIXct(points$time, format = '%H:%M:%S'))

# Transform the dependent variable to have values between 0 and 1 (excluding 0 and 1)
points$safe_sc_standard <- (points$safe_sc - 0)/ (10-0)
points$safe_sc_beta <- (points$safe_sc_standard * (23145 - 1) + 0.5)/23145

# Transform the dependent variable to have only positive values
points$safe_sc_gamma <- (points$safe_sc * (23145 - 1) + 0.5)/23145
points$safe_sc_nozero <- points$safe_sc
points$safe_sc_nozero[points$safe_sc_nozero==0] <- 0.0000000001

# Rescale the variables
points$volu_sc_scaled <- rescale(points$volu_sc)
points$speed_sc_scaled <- rescale(points$speed_sc) 
points$obst_sc_scaled <- rescale(points$obst_sc) 
points$cond_sc_scaled <- rescale(points$cond_sc) 
points$light_sc_scaled <- rescale(points$light_sc) 
points$cutof_sc_scaled <- rescale(points$cutof_sc)
points$cross_sc_scaled <- rescale(points$cross_sc) 
points$acci_sc_scaled <- rescale(points$acci_sc) 
points$infra_sc_scaled <- rescale(points$infra_sc) 
points$time_scaled <- rescale(points$time) 
points$age_scaled <- rescale(points$age)

# Delete columns explan and other_reas
drops <- c('explan', 'other_reas')
points <- points[,!(names(points) %in% drops)]

# Delete rows with NA values
points_nona = points[complete.cases(points),]

### RANDOM EFFECT SEARCH ###
############################

### Saturated lineair model wihtout random effects (with rescaled variables) ###

points_model_sat00 <- lm(safe_sc~
                           volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                           speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                           obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                           cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                           light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                           cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                           cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                           acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                           infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                           volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                           speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                           obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                           cond + cond*cyclist + cond*gender + cond*age_scaled + 
                           light + light*cyclist + light*gender + light*age_scaled + 
                           space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                           rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                           signs + signs*cyclist + signs*gender + signs*age_scaled + 
                           sight + sight*cyclist + sight*gender + sight*age_scaled + 
                           time_scaled + 
                           cyclist + 
                           gender + 
                           age_scaled, 
                         data=points, na.action = na.exclude)

summary(points_model_sat00) # Limited number of significant terms

### RANDOM EFFECT SEARCH ###

# RE: student_id
points_model_sat01a <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                             cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                             speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                             obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + light*age_scaled + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + signs*age_scaled + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|student_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

lrtest(points_model_sat00, points_model_sat01a)  # 01a is sign. better
AIC(points_model_sat00)
AIC(points_model_sat01a)  # 00 is better
summary(points_model_sat01a) 

# RE: node_id
points_model_sat01b <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                             cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                             speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                             obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + light*age_scaled + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + signs*age_scaled + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

lrtest(points_model_sat00, points_model_sat01b)  # 01 is sign. better
AIC(points_model_sat00)
AIC(points_model_sat01b)  # 00 is better
summary(points_model_sat01b) 


# RE: student_id, RE: node_id
points_model_sat02 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                             cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                             speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                             obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + light*age_scaled + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + signs*age_scaled + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

lrtest(points_model_sat00, points_model_sat02)  # 02 is sign. better
AIC(points_model_sat00)
AIC(points_model_sat01)  # 00 is better
AIC(points_model_sat02)  # 00 is better
summary(points_model_sat02) 

# RE: student_id, RE: municipality / node_id
points_model_sat03 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                             cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                             speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                             obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + light*age_scaled + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + signs*age_scaled + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|municipality/node_id) + (1|student_id), 
                           data=points, REML=TRUE, na.action = na.exclude)

lrtest(points_model_sat00, points_model_sat03)  # 02 is sign. better
AIC(points_model_sat00)
AIC(points_model_sat01) 
AIC(points_model_sat02)  
AIC(points_model_sat03)
# 02 has the lowest AIC
# Include random effect of student_id and node_id

### FIXED EFFECT SEARCH ###
###########################
points_model_sat02 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat +
                             cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*gender + cutof_sc_scaled*age_scaled + cutof_sc_scaled*time_cat + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + cross_sc_scaled*time_cat +  
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +
                             speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat +
                             obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat +
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + light*age_scaled + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + signs*age_scaled + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

summary(points_model_sat02) # Random effect variance of node_id = 0.48, of student_id = 0.33

# Traffic volume is given as reason for lower safety scores
# Space is given as reason for higher safety scores
# The safer to cross, the higher the score
# The safer the infrastructure, the higher the score
# When traffic volume is indicated as reason, a better traffic volume leads to a higher score
# The better obstacles, and the older, the lower the safety score
# When road condition is indicated as reason, a better road condition leads to a higher score
# When lighting is indicated as reason, a better lighting leads to a higher score
# When a cyclist gives a better score to infrastructure, the safety score is lower
# When an older ado gives a better score to infrastructure, the safety score is higher
# When traffic volume is indicated as reason and one passes at intermediate time, the score is higher
# Girls give higher scores when the traffic rules are obeyed

# Delete all interactions with p > 0.80
# age_scaled:signsTRUE          
# age_scaled:lightTRUE          
#voluTRUE:age_scaled           
#age_scaled:speedTRUE          
#time_catmedium:cutof_sc_scaled
#cyclistTRUE:obstTRUE          
#obst_sc_scaled:obstTRUE       
#time_catmedium:acci_sc_scaled 
#time_catlate:cross_sc_scaled  
# genderwoman:cutof_sc_scaled   
#genderwoman:cond_sc_scaled    
#time_catmedium:obst_sc_scaled 
#genderwoman:speedTRUE         
#time_catmedium:obstTRUE       
#cyclistTRUE:cond_sc_scaled    
points_model_sat04 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*age_scaled +
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*gender + cross_sc_scaled*age_scaled + 
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*time_cat +
                             speed + speed*cyclist + speed*age_scaled + speed*time_cat +
                             obst + obst*gender + obst*age_scaled + 
                             cond + cond*cyclist + cond*gender + cond*age_scaled + 
                             light + light*cyclist + light*gender + 
                             space + space*cyclist + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + 
                             sight + sight*cyclist + sight*gender + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)


AIC(points_model_sat02)
AIC(points_model_sat04)  # 04 is better
summary(points_model_sat04)

# Delete interactions with p > 0.60
# age_scaled:speedTRUE       
# obst_sc_scaled:obstTRUE    
# genderwoman:cross_sc_scaled
# cyclistTRUE:obst_sc_scaled 
# genderwoman:light_sc_scaled
# cyclistTRUE:light_sc_scaled
# genderwoman:lightTRUE      
# time_catmedium:speedTRUE   
# age_scaled:rulesTRUE       
# genderwoman:sightTRUE      
# cyclistTRUE:spaceTRUE      
# genderwoman:condTRUE       

points_model_sat05 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*light + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*age_scaled +
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*age_scaled + 
                             acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*time_cat +
                             speed + speed*cyclist + 
                             obst + obst*gender + obst*age_scaled + 
                             cond + cond*cyclist + cond*age_scaled + 
                             light + light*cyclist + 
                             space + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + 
                             sight + sight*cyclist + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat04)
AIC(points_model_sat05)  # 05 is better
summary(points_model_sat05)

# Delete main effect acci_sc_scaled and its interactions
points_model_sat06 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled + 
                             light_sc_scaled + light_sc_scaled*light + light_sc_scaled*age_scaled +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + cutof_sc_scaled*age_scaled +
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*age_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist + volu*gender + volu*time_cat +
                             speed + speed*cyclist + 
                             obst + obst*gender + obst*age_scaled + 
                             cond + cond*cyclist + cond*age_scaled + 
                             light + light*cyclist + 
                             space + space*gender + space*age_scaled + space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*time_cat + 
                             signs + signs*cyclist + signs*gender + 
                             sight + sight*cyclist + sight*age_scaled + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat05)
AIC(points_model_sat06)  # 06 is worse
summary(points_model_sat06)

# Delete interactions with p > 0.30
# cyclistTRUE:lightTRUE         
# age_scaled:speed_sc_scaled    
# genderwoman:obst_sc_scaled    
# age_scaled:sightTRUE          
# age_scaled:cutof_sc_scaled    
# genderwoman:speed_sc_scaled   
# age_scaled:cond_sc_scaled     
# voluTRUE:genderwoman          
# genderwoman:obstTRUE          
# cyclistTRUE:acci_sc_scaled    
# age_scaled:light_sc_scaled    
# volu_sc_scaled:age_scaled     
# age_scaled:spaceTRUE          
# genderwoman:signsTRUE         
# cyclistTRUE:speedTRUE         

points_model_sat07 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender +  volu_sc_scaled*time_cat +
                             speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed +speed_sc_scaled*time_cat +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + cross_sc_scaled*age_scaled + 
                             acci_sc_scaled + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist +  volu*time_cat +
                             speed + 
                             obst + obst*age_scaled + 
                             cond + cond*cyclist + cond*age_scaled + 
                             light + 
                             space + space*gender +space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*time_cat + 
                             signs + signs*cyclist + 
                             sight + sight*cyclist + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat05)
AIC(points_model_sat07)  # 07 is better
summary(points_model_sat07)

# Delete interaction terms with p > 0.20
# volu_sc_scaled:time_catlate   
# age_scaled:cross_sc_scaled    
# age_scaled:acci_sc_scaled     
# time_catlate:speed_sc_scaled  
# genderwoman:infra_sc_scaled   
# cyclistTRUE:speed_sc_scaled   
# volu_sc_scaled:cyclistTRUE    
# age_scaled:condTRUE  
# age_scaled:obstTRUE           

points_model_sat08 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             speed_sc_scaled + speed_sc_scaled*speed +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + cross_sc_scaled*cyclist + 
                             acci_sc_scaled + acci_sc_scaled*gender + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*cyclist +  volu*time_cat +
                             speed + 
                             obst + 
                             cond + cond*cyclist + 
                             light + 
                             space + space*gender +space*time_cat + 
                             rules + rules*cyclist + rules*gender + rules*time_cat + 
                             signs + signs*cyclist + 
                             sight + sight*cyclist + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat07)
AIC(points_model_sat08)  # 08 is better
summary(points_model_sat08)

# Delete interaction terms with p > 0.10
# time_catlate:spaceTRUE     
# genderwoman:acci_sc_scaled 
# time_catlate:rulesTRUE     
# voluTRUE:cyclistTRUE       
# cyclistTRUE:sightTRUE      
# time_scaled --> time_cat             
# cyclistTRUE:rulesTRUE      
# cyclistTRUE:signsTRUE      
# condTRUE:cyclistTRUE       
# cyclistTRUE:cross_sc_scaled
# genderwoman:spaceTRUE

points_model_sat09 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             speed_sc_scaled + speed_sc_scaled*speed +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*time_cat +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender + 
                             signs + 
                             sight + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat08)
AIC(points_model_sat09)  # 09 is worse
summary(points_model_sat09)

# Delete main effect of sight and signs
points_model_sat10 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             speed_sc_scaled + speed_sc_scaled*speed +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*time_cat +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat09)
AIC(points_model_sat10)  # 10 is better
summary(points_model_sat10)

# Delete main effect and interaction of light
points_model_sat11 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             speed_sc_scaled + speed_sc_scaled*speed +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + 
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*time_cat +
                             speed + 
                             obst + 
                             cond + 
                             space + 
                             rules + rules*gender + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat10)
AIC(points_model_sat11)  # 11 is worse

# Delete main effect of speed_sc and interactions
points_model_sat12 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*time_cat +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat10)
AIC(points_model_sat12)  #  # AIC 12 - AIC 10 < 1 --> # Similar, so keep 12
summary(points_model_sat12)

# Delete main effect and interaction of light_sc_scaled
points_model_sat13 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu + volu*time_cat +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat12)
AIC(points_model_sat13)  # 13 is worse
summary(points_model_sat13)

check_collinearity(points_model_sat13)
# High multicollinearity for volu and time_cat

# Delete volue*time_cat
points_model_sat14 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender + 
                             time_scaled + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat12)
AIC(points_model_sat14)   # AIC 14 - AIC 12 < 2 --> # Similar, so keep 14
summary(points_model_sat14)

# Delete time_scaled
points_model_sat15 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender +
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat14)
AIC(points_model_sat15)  # AIC 15 - AIC 14 < 1 --> # Similar, so keep 15
summary(points_model_sat15)

# Delete genderwoman:rulesTRUE  and rules      
points_model_sat16 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat15)
AIC(points_model_sat16)  # 16 is worse
summary(points_model_sat16)

check_collinearity(points_model_sat16)
# Some moderate correlation, but no high correlation

# Delete cutof_sc_scaled and cutof_sc_scaled*cyclist
points_model_sat17 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender +
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

AIC(points_model_sat15)
AIC(points_model_sat17)  # 17 is worse
summary(points_model_sat15)

#### Model points_model_sat15 has the lowest possible AIC with relevant factors
summary(points_model_sat15)

# Random effect variance of node_id = 0.45
# Random effect variance of student_id = 0.30

# Reasons given for higher safety scores: space
# Reasons given for lower safety scores: traffic volume, road condition, light, speed, obstacles, rules
# Better scored, higher safety score: safety to cross, obstacles, accidents, infrastructure, 
# Better scored, lower safety score: 
# Girls give higher scores
# When traffic volume is indicated as reason, a better traffic volume leads to a higher score
# When road condition is indicated as reason, a better road condition leads to a higher score
# Girls perceive lower safety when better traffic volume
# When a cyclist gives a better score to infrastructure, the safety score is lower
# When an older ado gives a better score to infrastructure, the safety score is higher
# Older ado and giving a higher score to obstacles, results in lower safety score
# Cyclists being less cut of the path give higher scores

## CHECK THE PREDICTIONS ##
###########################

preds=predict(points_model_sat15, na.action = na.exclude)
tens <- count(points$safe_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# 25 have a prediction above 10, 2374 have a value of 10 in the original data. (< 11)
zeros <- count(points$safe_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 55 have a prediction below 0, 800 have a value of 0 in the original data. (> -1.2)

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(points_model_sat15)
influenceIndexPlot(points_model_sat15, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 1 observations with Bonf.p < 0.05

cd = cooks.distance(points_model_sat15)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(points_model_sat15, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(points_model_sat15))
qqline(residuals(points_model_sat15))
qqPlot(residuals(points_model_sat15))
# On a straight line between -1.5 and 1.25, but underestimation between 1.25 and 2

## Homogeneity of variance
plot(points_model_sat15)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(points_model_sat15)
check_collinearity(points_model_sat15)
# Only some moderate correlation, but < 10

## FINAL MODEL ##
#################
points_model_sat15 <- lmer(safe_sc~
                             volu_sc_scaled + volu_sc_scaled*volu + volu_sc_scaled*gender +
                             obst_sc_scaled + obst_sc_scaled*age_scaled + 
                             cond_sc_scaled + cond_sc_scaled*cond + 
                             light_sc_scaled + light_sc_scaled*light +  
                             cutof_sc_scaled + cutof_sc_scaled*cyclist + 
                             cross_sc_scaled + 
                             acci_sc_scaled + 
                             infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*age_scaled + 
                             volu +
                             speed + 
                             obst + 
                             cond + 
                             light + 
                             space + 
                             rules + rules*gender +
                             cyclist + 
                             gender + 
                             age_scaled +
                             (1|node_id) + (1|student_id), 
                           data=points, REML=FALSE, na.action = na.exclude)

summary(points_model_sat15)

### STEP AIC ###
################
points_model_step_back <- stepcAIC(points_model_sat02, direction = 'backward')
points_model_step_back
# Very large resulting model

points_model_step_for <- stepcAIC(points_model_sat02, direction = 'forward')
points_model_step_for
# ERROR
