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
library(Hmisc)
library(pastecs)
library(ggplot2)
library(psych)

theme_plot <- theme_bw() + theme(plot.background = element_rect(size = 2, color = "white", fill = "white"),
                                 text=element_text(size = 15, color = "black"),
                                 axis.text.y = element_text(colour = "black", margin=margin(0,15,0,0)),
                                 axis.text.x = element_text(colour = "black", margin=margin(15,0,0,0), angle=0),
                                 plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                                 axis.title.x=element_text(margin=margin(15,0,0,0), size=15),
                                 axis.title.y=element_text(margin=margin(0,15,0,0), angle=90, size=15),
                                 plot.title=element_text(size=15, margin=margin(0,0,15,0))) 

rm(list=ls())

###############
# ADOLESCENTS #
###############

### Load data ###
#################

### Load data
adolescents <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/08_dataset_adolescents_210402.csv', fileEncoding="UTF-8-BOM")

# Delete records with 'gender = other'
# adolescents <- adolescents[!(adolescents$gender=="other"),]

# Convert variables
adolescents$school_id <- as.factor(adolescents$school_id)
adolescents$pedestrian_lane[is.na(adolescents$pedestrian_lane)] <- 0

# Take cycling infrastructure column together to have less variables
adolescents$non_sep_bike_lane <- rowSums(adolescents[, c(21,22)])       # non_sep_bike_lane + bike_suggestion_lane
adolescents$bike_path_without_cars <- rowSums(adolescents[, c(25,26)])  # bike_street_no_cars + bike_highway
adolescents$other_infra <- rowSums(adolescents[, c(27,28,29)])  # pedestrian_lane + forbidden_for_cyclists + other_cycling_infra
# adolescents <- adolescents[-c(21,25:29)]


### Descriptive statistics ###
##############################

summary(adolescents$gender)
describe(adolescents$gender)
stat.desc(adolescents$gender)

summary(adolescents$age)
describe(adolescents$age)
stat.desc(adolescents$age)

summary(adolescents$class_nam)
describe(adolescents$class_nam)
stat.desc(adolescents$class_nam)

summary(adolescents$school_id)
describe(adolescents$school_id)
stat.desc(adolescents$school_id)

summary(adolescents$municipal)
describe(adolescents$municipal)
stat.desc(adolescents$municipal)

summary(adolescents$start_time)
describe(adolescents$start_time)
stat.desc(adolescents$start_time)

summary(adolescents$end_time)
describe(adolescents$end_time)
stat.desc(adolescents$end_time)

summary(adolescents$dur)
describe(adolescents$dur)
stat.desc(adolescents$dur)

summary(adolescents$rt_t_dist)
describe(adolescents$rt_t_dist)
stat.desc(adolescents$rt_t_dist)

summary(adolescents$rt_t_dm)
describe(adolescents$rt_t_dm)
stat.desc(adolescents$rt_t_dm)

summary(adolescents$ov_saf_sc)
describe(adolescents$ov_saf_sc)
stat.desc(adolescents$ov_saf_sc)

p<-ggplot(adolescents, aes(x=ov_saf_sc)) + 
  geom_histogram(color="black", fill="steelblue",binwidth=1)+
  scale_y_continuous(name = "Frequency", limits=c(0,400)) +
  scale_x_continuous(name = "Subjective safety score for routes ( / 10)",breaks=seq(0,10,1))+
  theme_plot
p

describe(adolescents$det_short)
describe(adolescents$det_least)
describe(adolescents$det_slow)
describe(adolescents$det_infra)
describe(adolescents$det_beau)
describe(adolescents$det_obs)
describe(adolescents$det_oths)

summary(adolescents$comp_nr)
describe(adolescents$comp_nr)
stat.desc(adolescents$comp_nr)
describe.by(adolescents$comp_nr, adolescents$rt_a_cycl)

summary(adolescents)


############
# SEGMENTS #
############

### Load data ###
#################

### Load data
segments <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/08_dataset_segments_210402.csv', fileEncoding="UTF-8-BOM")

# Delete records with 'gender = other'
# segments <- segments[!(segments$gender=="other"),]

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

### Descriptive statistics ###
##############################
myvars <- c("volu", "volu_sc","length_m")
newdata <- segments[myvars]
segments_nona = newdata[complete.cases(newdata),]
sum(segments_nona$length_m)

summary(segments$safe_sc)
describe(segments$safe_sc)
stat.desc(segments$safe_sc)

p<-ggplot(segments, aes(x=safe_sc)) + 
  geom_histogram(color="black", fill="steelblue",binwidth=1)+
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Subjective safety score for road segments ( / 10)",breaks=seq(0,10,1))+
  theme_plot
p

summary(segments)

### Load data ###
#################

### Load data
points <- read.csv('C:/Users/Sien Benoit/OneDrive - UGent/Werk_UGent/Doctoraat/Datasets_routes/DataFietsbarometerVL_210402/08_dataset_points_210402.csv', fileEncoding="UTF-8-BOM")

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

### Descriptive statistics ###
##############################
myvars <- c("volu", "volu_sc")
newdata <- points[myvars]
points_nona = newdata[complete.cases(newdata),]

summary(points$safe_sc)
describe(points$safe_sc)
stat.desc(points$safe_sc)

p<-ggplot(points, aes(x=safe_sc)) + 
  geom_histogram(color="black", fill="steelblue",binwidth=1)+
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Subjective safety score for intersections ( / 10)",breaks=seq(0,10,1))+
  theme_plot
p

summary(points)
