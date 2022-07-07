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
library(optimx)
library(nloptr)
library(dfoptim)

rm(list=ls())

############
# SEGMENTS #
############

### Load data ###
#################

### Load data
segments <- read.csv('06_dataset_segments_210402.csv')

# Delete records with 'gender = other'
segments <- segments[!(segments$gender=="other"),]

# Convert id's to factors
segments$road_segment_id <- as.factor(segments$road_segment_id)
segments$geagg_segment_id <- as.factor(segments$geagg_segment_id)
segments$student_id <- as.factor(segments$student_id)
segments$school_id <- as.factor(segments$school_id)
segments$time <- as.numeric(as.POSIXct(segments$time, format = '%H:%M:%S'))

# Transform the dependent variable to have only positive values
segments$safe_sc_gamma <- (segments$safe_sc * (70979 - 1) + 0.5)/70979
segments$safe_sc_nozero <- segments$safe_sc
segments$safe_sc_nozero[segments$safe_sc_nozero==0] <- 0.0000000001

# Transform the dependent variable to have values between 0 and 1 (excluding 0 and 1)
segments$safe_sc_standard <- (segments$safe_sc - 0)/ (10-0)
segments$safe_sc_beta <- (segments$safe_sc_standard * (70979 - 1) + 0.5)/70979

# Rescaled variables
segments$volu_sc_scaled <- rescale(segments$volu_sc)
segments$speed_sc_scaled <- rescale(segments$speed_sc) 
segments$obst_sc_scaled <- rescale(segments$obst_sc) 
segments$cond_sc_scaled <- rescale(segments$cond_sc) 
segments$light_sc_scaled <- rescale(segments$light_sc) 
segments$dist_sc_scaled <- rescale(segments$dist_sc)
segments$hind_sc_scaled <- rescale(segments$hind_sc)
segments$hindc_sc_scaled <- rescale(segments$hindc_sc) 
segments$acci_sc_scaled <- rescale(segments$acci_sc) 
segments$infra_sc_scaled <- rescale(segments$infra_sc) 
segments$time_scaled <- rescale(segments$time)
segments$length_m_scaled <- rescale(segments$length_m)
segments$age_scaled <- rescale(segments$age)

# Delete columns explan and other_reas
drops <- c('explan', 'other_reas')
segments <- segments[,!(names(segments) %in% drops)]

# Delete rows with NA values
segments_nona = segments[complete.cases(segments),]

### EFFECT OF LENGTH ###
########################
### Saturated lineair model wihtout random effects (with rescaled variables) ###

segments_model_sat00a <- lm(safe_sc~
                              volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                              speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                              obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                              cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                              light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                              dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                              hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                              hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                              acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                              infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                              volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                              speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                              obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                              cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                              light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                              space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                              rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                              signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                              sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                              time_cat + 
                              cyclist + 
                              gender + 
                              age_scaled +
                              length_m_scaled, 
                            data=segments_nona, na.action = na.exclude)

# Saturated model without interaction with length_m
segments_model_sat00b <- lm(safe_sc~
                              volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + 
                              speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat +
                              obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + 
                              cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + 
                              light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                              dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                              hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  
                              hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  
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
                              time_cat + 
                              cyclist + 
                              gender + 
                              age_scaled +
                              length_m_scaled, 
                            data=segments_nona, na.action = na.exclude)

AIC(segments_model_sat00a)
AIC(segments_model_sat00b)  # sat00a > sat00b --> include interactions with length
summary(segments_model_sat00a)  # A lot of significant terms, also with interactions with length
summary(segments_model_sat00b)


### RANDOM EFFECT SEARCH ###

# RE: geagg_segment_id
segments_model_sat01 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|geagg_segment_id), 
                             data=segments_nona, REML = TRUE, na.action = na.exclude)


AIC(segments_model_sat00a, segments_model_sat01) # 01 has lower (but negative) AIC
summary(segments_model_sat01)

# RE: geagg_segment_id in student_id - ERROR for lmer with RMEL = TRUE
segments_model_sat02 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|student_id/geagg_segment_id), 
                             data=segments_nona, REML=TRUE, na.action=na.exclude, control = lmerControl(optimizer ="Nelder_Mead"))


# RE: student_id
segments_model_sat03 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|student_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat00a, segments_model_sat03)  # 03 is sign. better
AIC(segments_model_sat01, segments_model_sat03)  # 03 has higher (but not negative) AIC

# RE: road_segment_id
segments_model_sat04 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat04)  # 04 is worse
AIC(segments_model_sat01, segments_model_sat04)  # 04 has no negative AIC

# RE: road_segment_id in municipality
segments_model_sat05 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|municipality/road_segment_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat05)  # 05 is worse
AIC(segments_model_sat01, segments_model_sat05)  # 05 has no negative AIC

# RE: geagg_segment_id + road_segment_id
segments_model_sat06 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|geagg_segment_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat06)  # 06 has lower but negative AIC
AIC(segments_model_sat01, segments_model_sat06)  # 01 is sign. better (lower AIC)

# RE: geagg_segment_id in student_id + road_segment_id
segments_model_sat07 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id/geagg_segment_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat07)  # 07 has lower but negative AIC
AIC(segments_model_sat01, segments_model_sat07)  # 01 is sign. better

# RE: geagg_segment_id in student_id + road_segment_id in municipality - ERROR
segments_model_sat08 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|municipality/road_segment_id) + (1|student_id/geagg_segment_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

# RE: student_id + road_segment_id
segments_model_sat09 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat09)  # 03 has a lower (but similar < 2) AIC
AIC(segments_model_sat01, segments_model_sat09)  # 01 is sign. better (negative AIC)
summary(segments_model_sat09)

# RE: student_id + road_segment_id + geagg_segment_id
segments_model_sat10 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id) + (1|geagg_segment_id), 
                             data=segments_nona, REML=TRUE, na.action=na.exclude)

AIC(segments_model_sat03, segments_model_sat10)  # 10 has lower but negative AIC
AIC(segments_model_sat01, segments_model_sat10)  # 01 is sign. better

# 01 has the lowest AIC, but is very negative and shows weird results
# 03 has the lowest of the positive AICs and realistic results
# 09 has a very similar AIC, has realistic resuls and uses the same random effects as for the objective model
# Include random effect of road_segment_id and student_id

### FIXED EFFECT SEARCH ###
###########################
segments_model_sat09 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + speed_sc_scaled*length_m_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled + hindc_sc_scaled*time_cat +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*gender + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*time_cat + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

summary(segments_model_sat09)

# Delete interactions with p > 0.80
# time_catmedium:acci_sc_scaled --> worse 
# voluTRUE:genderwoman  --> better
# time_catmedium:speedTRUE   --> similar so ok to remove    
# obstTRUE:hind_sc_scaled --> worse       
# length_m_scaled:speed_sc_scaled --> better
# time_catlate:hindc_sc_scaled  --> similar so ok to remove 

segments_model_sat10 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*gender + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled + cond_sc_scaled*length_m_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*space + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*obst + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*age_scaled + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + signs*length_m_scaled +
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat09, segments_model_sat10)
summary(segments_model_sat10)

# Delete interactions or main effects with p > 0.60
# volu_sc_scaled   --> worse              
# time_catmedium:acci_sc_scaled  --> worse
# dist_sc_scaled:spaceTRUE    --> beter   
# length_m_scaled:signsTRUE --> better     
# obstTRUE:hind_sc_scaled --> better   
# time_catlate:dist_sc_scaled  --> worse  
# age_scaled:rulesTRUE  --> better         
# length_m_scaled:cond_sc_scaled --> better
# genderwoman:obst_sc_scaled  --> better

segments_model_sat11 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + obst_sc_scaled*length_m_scaled +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + dist_sc_scaled*length_m_scaled +
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  hind_sc_scaled*length_m_scaled +
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + infra_sc_scaled*length_m_scaled +
                               volu + volu*cyclist + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + light*length_m_scaled +
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat10, segments_model_sat11)
summary(segments_model_sat11)

# Delete interactions or main effects with p > 0.40
# time_catmedium:acci_sc_scaled  --> worse
# volu_sc_scaled    --> worse             
# time_catlate:dist_sc_scaled    --> worse
# length_m_scaled:hind_sc_scaled --> better
# length_m_scaled:dist_sc_scaled --> better
# length_m_scaled:obst_sc_scaled --> better
# length_m_scaled:lightTRUE --> better 
# dist_sc_scaled  --> worse               
# length_m_scaled:infra_sc_scaled --> better

segments_model_sat12 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*cyclist + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + hindc_sc_scaled*age_scaled +  hindc_sc_scaled*length_m_scaled +
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + infra_sc_scaled*age_scaled + 
                               volu + volu*cyclist + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + cond*length_m_scaled +
                               light + light*cyclist + light*gender + light*age_scaled + 
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*cyclist + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat11, segments_model_sat12)
summary(segments_model_sat12)

# Delete interactions or main effects with p > 0.20
# time_catmedium:acci_sc_scaled  --> worse
# volu_sc_scaled    --> worse             
# time_catlate:dist_sc_scaled    --> worse
# dist_sc_scaled --> worse
# length_m_scaled:condTRUE  --> better      
# age_scaled:hindc_sc_scaled --> better    
# cyclistTRUE:rulesTRUE   --> better       
# length_m_scaled:hindc_sc_scaled --> better
# cyclistTRUE:speed_sc_scaled   --> better 
# age_scaled:infra_sc_scaled   --> better  

segments_model_sat13 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*speed + speed_sc_scaled*gender + speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*cyclist + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*cyclist + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*gender + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat12, segments_model_sat13)
summary(segments_model_sat13)

# Delete interactions or main effects with p > 0.10
# genderwoman:speed_sc_scaled  --> better  
# cyclistTRUE:obstTRUE     --> better        
# time_catlate:spaceTRUE  --> worse       
# cyclistTRUE:voluTRUE    --> better         
# genderwoman:signsTRUE   --> better         

segments_model_sat14 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu + volu_sc_scaled*gender + volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + volu_sc_scaled*length_m_scaled +
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst + obst_sc_scaled*age_scaled + obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*cyclist + light_sc_scaled*light + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender + hind_sc_scaled*age_scaled + hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + obst*time_cat + obst*length_m_scaled +
                               cond + cond*cyclist + cond*gender + cond*age_scaled + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat13, segments_model_sat14)
summary(segments_model_sat14)

# Delete interactions or main effects with p > 0.05
# volu_sc_scaled:genderwoman --> better or similar    
# time_catlate:spaceTRUE  --> worse       
# age_scaled:obst_sc_scaled   --> better or similar      
# age_scaled:condTRUE  --> better or similar             
# light_sc_scaled:lightTRUE --> better or similar        
# length_m_scaled:obstTRUE --> better or similar         
# cyclistTRUE:light_sc_scaled  --> better or similar     
# volu_sc_scaled:length_m_scaled --> better or similar   
# spaceTRUE --> worse                     
# age_scaled:hind_sc_scaled --> little bit worse, but ok     

segments_model_sat15 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + 
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst +obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + light_sc_scaled*length_m_scaled +
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst + hindc_sc_scaled*gender + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + obst*time_cat + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               space + space*cyclist + space*gender + space*age_scaled + space*time_cat + space*length_m_scaled +
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat14, segments_model_sat15)
summary(segments_model_sat15)

# Delete interactions or main effects with p > 0.05
# volu_sc_scaled  --> worse               
# time_catmedium:acci_sc_scaled  --> worse 
# time_catlate:dist_sc_scaled --> worse    
# dist_sc_scaled  --> worse                
# time_catlate:spaceTRUE   --> worse       
# genderwoman:hindc_sc_scaled --> similar or better   
# light_sc_scaled:length_m_scaled --> similar or better   
# spaceTRUE --> similar or better                      

segments_model_sat16 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + 
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst +obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst +  
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + obst*time_cat + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + sight*length_m_scaled +
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat15, segments_model_sat16)
summary(segments_model_sat16)

# Delete interactions or main effects with p > 0.05
# volu_sc_scaled                
# signsTRUE --> worse                    
# length_m_scaled:sightTRUE  --> better    

segments_model_sat17 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + 
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst +obst_sc_scaled*time_cat + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + dist_sc_scaled*time_cat + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  hind_sc_scaled*time_cat +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst +  
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*time_cat + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + obst*time_cat + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat16, segments_model_sat17)
summary(segments_model_sat17)
check_collinearity(segments_model_sat17)  # Very high multicollinearity for several interactions

# Delete terms with very high multicollinearity if not a huge increase in AIC (< 100)
# time_cat:hind_sc_scaled
# time_cat:acci_sc_scaled
# time_cat:dist_sc_scaled
# time_cat:speed_sc_scaled --> large increase
# time_cat:obst_sc_scaled

segments_model_sat18 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + volu_sc_scaled*time_cat + 
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled + speed_sc_scaled*time_cat + 
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst +  
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*time_cat +volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + obst*time_cat + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender + rules*time_cat + rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               time_cat + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat17, segments_model_sat18)  # Higher AIC, but less multicollinearity
check_collinearity(segments_model_sat18)  # Still high multicollinearity for several interactions with time_cat
summary(segments_model_sat18)

# Delete all interactions with time_cat and its main effect
segments_model_sat19 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*speed +speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + obst_sc_scaled*obst +
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + hindc_sc_scaled*obst +  
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender +rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat18, segments_model_sat19)  #Higher AIC (+400) but less multicollinearity
check_collinearity(segments_model_sat19)  # Still high multicollinearity for interactions with same reason
summary(segments_model_sat19)

# Delete interactions with reasons
# volu_sc*volu --> worse
# speed_sc*speed --> better
# obst_sc*obst --> small increase
# cond_sc*cond --> worse
# hindc_sc*obst --> small increase

segments_model_sat20 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + acci_sc_scaled*length_m_scaled +
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + volu*length_m_scaled +
                               speed + speed*cyclist + speed*gender + speed*age_scaled + speed*length_m_scaled +
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender +rules*length_m_scaled +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               length_m_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat19, segments_model_sat20)  #Higher AIC (+50) but less multicollinearity
check_collinearity(segments_model_sat20)  # Still high multicollinearity for scored reasons but vif < 15
summary(segments_model_sat20)

# Delete interactions with and main effect of length_scaled
segments_model_sat21 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cyclist + cond_sc_scaled*cond + cond_sc_scaled*gender + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*cyclist + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*gender + acci_sc_scaled*age_scaled + 
                               infra_sc_scaled + infra_sc_scaled*cyclist + infra_sc_scaled*gender + 
                               volu + volu*age_scaled + 
                               speed + speed*cyclist + speed*gender + speed*age_scaled + 
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*cyclist + light*gender + light*age_scaled + 
                               rules + rules*gender +
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat20, segments_model_sat21)  #Higher AIC (+40) but less multicollinearity
check_collinearity(segments_model_sat21)  # Still high multicollinearity for scored reasons but vif < 14
summary(segments_model_sat21)

# Delete terms with p > 0.05
# cyclistTRUE:lightTRUE   --> better   
# signsTRUE   --> worse            
# cyclistTRUE:dist_sc_scaled --> better   
# voluTRUE:age_scaled  --> better         
# genderwoman:acci_sc_scaled --> better   
# rulesTRUE    --> better                 
# cyclistTRUE:cond_sc_scaled --> better   
# cyclistTRUE:infra_sc_scaled --> better   
# hind_sc_scaled --> worse           
# cond_sc_scaled:genderwoman --> better   

segments_model_sat22 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*age_scaled + 
                               infra_sc_scaled + infra_sc_scaled*gender + 
                               volu + 
                               speed + speed*cyclist + speed*gender + speed*age_scaled + 
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*gender + light*age_scaled + 
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat21, segments_model_sat22)  # Similar AIC
check_collinearity(segments_model_sat22)  # Only 2 terms with high multicollinearity
summary(segments_model_sat22)

# Delete terms with p > 0.05
# signs --> worse
# genderwoman:infra_sc_scaled --> similar

segments_model_sat23 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*age_scaled + 
                               infra_sc_scaled + 
                               volu + 
                               speed + speed*cyclist + speed*gender + speed*age_scaled + 
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*gender + light*age_scaled + 
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat22, segments_model_sat23)  # Similar AIC
check_collinearity(segments_model_sat23)  # Only 2 terms with high multicollinearity
summary(segments_model_sat23)

# Check if main effects without interactions can be removed
# infra_sc --> worse
# volu --> worse

segments_model_sat24 <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*age_scaled + 
                               infra_sc_scaled + 
                               volu + 
                               speed + speed*cyclist + speed*gender + speed*age_scaled + 
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*gender + light*age_scaled + 
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat23, segments_model_sat24)  # same as 23

## CHECK THE PREDICTIONS ##
###########################
preds=predict(segments_model_sat23, na.action = na.exclude)
tens <- count(segments$safe_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# >200 have a prediction above 10, 4753 have a value of 10 in the original data. (< 11.5)
zeros <- count(segments$safe_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 83 have a prediction below 0, 808 have a value of 0 in the original data. (> -1.6)

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(segments_model_sat23)
influenceIndexPlot(segments_model_sat23, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 10 observations with Bonf.p < 0.05

cd = cooks.distance(segments_model_sat23)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(segments_model_sat23, vars="Cook")
# 2 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(segments_model_sat23))
qqline(residuals(segments_model_sat23))
qqPlot(residuals(segments_model_sat23))
# On a straight line between -1 and 1

## Homogeneity of variance
plot(segments_model_sat23)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(segments_model_sat23)
check_collinearity(segments_model_sat23)
# 2 terms with high multicollinearity (< 14)

# Delete influential observations #
###################################
segments_noinflobs = segments[-inflobs,]

# Refit the model
segments_model_sat23b <- lmer(safe_sc~
                               volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                               speed_sc_scaled + speed_sc_scaled*age_scaled +
                               obst_sc_scaled + obst_sc_scaled*cyclist + 
                               cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled +
                               light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                               dist_sc_scaled + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                               hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                               hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                               acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*age_scaled + 
                               infra_sc_scaled + 
                               volu + 
                               speed + speed*cyclist + speed*gender + speed*age_scaled + 
                               obst + obst*gender + obst*age_scaled + 
                               cond + cond*cyclist + cond*gender + 
                               light + light*gender + light*age_scaled + 
                               signs + signs*cyclist + signs*age_scaled + 
                               sight + sight*cyclist + sight*gender + sight*age_scaled + 
                               cyclist + 
                               gender + 
                               age_scaled +
                               (1|road_segment_id) + (1|student_id), 
                             data=segments_noinflobs, REML=FALSE, na.action=na.exclude)

AIC(segments_model_sat23, segments_model_sat23b)  # Lower AIC
check_collinearity(segments_model_sat23b)  # Only 2 terms with high multicollinearity
summary(segments_model_sat23b)

## CHECK THE PREDICTIONS ##
###########################
preds=predict(segments_model_sat23b, na.action = na.exclude)
tens <- count(segments_noinflobs$safe_sc == 10)
tens
y <- as.numeric(names(which(preds > 10)))
y   
preds[which(preds > 10)]
# >200 have a prediction above 10, 4753 have a value of 10 in the original data. (< 11.5)
zeros <- count(segments_noinflobs$safe_sc == 0)
zeros
y <- as.numeric(names(which(preds < 0)))
y   
preds[which(preds<0)]
# 83 have a prediction below 0, 807 have a value of 0 in the original data. (> -3)

## ASSUMPTIONS ##
#################

### Assumptions ###

## Outliers and influential observations --> not really necessary for glmer
out_test = outlierTest(segments_model_sat23b)
influenceIndexPlot(segments_model_sat23b, vars=c("Studentized", "Bonf"))
outl=as.numeric(names(which(out_test$bonf.p<0.05)))
outl
# 10 observations with Bonf.p < 0.05

cd = cooks.distance(segments_model_sat23b)
inflobs = which(cd>1)
inflobs
influenceIndexPlot(segments_model_sat23b, vars="Cook")
# 0 observations with Cook's distance > 1

## Normality of residuals - if the dependent variable is normally distributed
qqnorm(residuals(segments_model_sat23b))
qqline(residuals(segments_model_sat23b))
qqPlot(residuals(segments_model_sat23b))
# On a straight line between -1 and 1

## Homogeneity of variance
plot(segments_model_sat23b)
# Not random due to actually rather discrete variable instead of completely continuous variable

## Multicollinearity
vif(segments_model_sat23b)
check_collinearity(segments_model_sat23b)
# 2 terms with high multicollinearity (volu_sc, acci_sc) (< 14)


## FINAL MODEL ##
#################
segments_model_sat23b <- lmer(safe_sc~
                                volu_sc_scaled + volu_sc_scaled*cyclist + volu_sc_scaled*volu +  volu_sc_scaled*age_scaled + 
                                speed_sc_scaled + speed_sc_scaled*age_scaled +
                                obst_sc_scaled + obst_sc_scaled*cyclist + 
                                cond_sc_scaled + cond_sc_scaled*cond + cond_sc_scaled*age_scaled +
                                light_sc_scaled + light_sc_scaled*gender + light_sc_scaled*age_scaled + 
                                dist_sc_scaled + dist_sc_scaled*gender + dist_sc_scaled*age_scaled + 
                                hind_sc_scaled + hind_sc_scaled*cyclist + hind_sc_scaled*gender +  
                                hindc_sc_scaled + hindc_sc_scaled*cyclist + 
                                acci_sc_scaled + acci_sc_scaled*cyclist + acci_sc_scaled*age_scaled + 
                                infra_sc_scaled + 
                                volu + 
                                speed + speed*cyclist + speed*gender + speed*age_scaled + 
                                obst + obst*gender + obst*age_scaled + 
                                cond + cond*cyclist + cond*gender + 
                                light + light*gender + light*age_scaled + 
                                signs + signs*cyclist + signs*age_scaled + 
                                sight + sight*cyclist + sight*gender + sight*age_scaled + 
                                cyclist + 
                                gender + 
                                age_scaled +
                                (1|road_segment_id) + (1|student_id), 
                              data=segments_noinflobs, REML=FALSE, na.action=na.exclude)

summary(segments_model_sat23b)
