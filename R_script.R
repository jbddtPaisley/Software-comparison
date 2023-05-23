install.packages("ggplot")
library("ggplot")
install.packages("car")
library("car")
install.packages("lme4")
library("lme4")
install.packages("afex")
library("afex")
install.packages("multcomp")
library("multcomp")
install.packages("lsmeans")
library("lsmeans")

# Import "all_coordinates_1st_trial" i.e. the error ratio and ID switches results from the different software in the CW, MT and HT treatments

# Import "metrics_turbidity" i.e. the tracking accuracy results from the different software in the CW, MT and HT treatments

# Import "all_coordinates_2nd_trial" i.e. the error ratio and ID switches from the different software in challenging / unchallenging treatments


###################### dataset = all_coordinates_1st_trial (i.e. CW, MT and HT treatments)

######### Error ratios

#We apply a Gaussian linear model

all_coordinates_1st_trial$Software=as.factor(all_coordinates_1st_trial$Software)
all_coordinates_1st_trial$Turbidity=as.factor(all_coordinates_1st_trial$Turbidity)

ER_turbidity_glm1 <- glm(`Error ratio`~Turbidity, data=all_coordinates_1st_trial, family = gaussian())
Anova (ER_turbidity_glm1)

ER_turbidity_glm2 <- glm(`Error ratio`~Software, data=all_coordinates_1st_trial, family = gaussian())
Anova (ER_turbidity_glm2)

ER_turbidity_glm12 <- glm(`Error ratio`~Turbidity+Software, data=all_coordinates_1st_trial, family = gaussian())
Anova (ER_turbidity_glm12)

ER_turbidity_glm1x2 <- glm(`Error ratio`~Turbidity*Software, data=all_coordinates_1st_trial, family = gaussian())
Anova (ER_turbidity_glm1x2)

anova(ER_turbidity_glm1, ER_turbidity_glm2, test='Chisq')
anova(ER_turbidity_glm1, ER_turbidity_glm12, test='Chisq')
anova(ER_turbidity_glm1, ER_turbidity_glm1x2, test='Chisq')

anova(ER_turbidity_glm2, ER_turbidity_glm12, test='Chisq')
anova(ER_turbidity_glm2, ER_turbidity_glm1x2, test='Chisq')

anova(ER_turbidity_glm12, ER_turbidity_glm1x2, test='Chisq')

# We keep model "ER_turbidity_glm12" i.e. software + turbidity (no interaction)

# Post-hoc comparisons

ER_sftw_main <- glht(ER_turbidity_glm12, mcp(Software = "Tukey"))
summary(ER_sftw_main)

ER_turbidity_main <- glht(ER_turbidity_glm12, mcp(Turbidity = "Tukey"))
summary(ER_turbidity_main)

ER_turbidity_Tukey <- emmeans(ER_turbidity_glm1x2, pairwise~Software*Turbidity, adjust="tukey")
summary(ER_turbidity_Tukey)

######### ID swaps

#We apply a Poisson linear model

ID_turbidity_glm1 <- glm(`ID swaps`~Turbidity, data=all_coordinates_1st_trial, family = poisson())
Anova (ID_turbidity_glm1)

ID_turbidity_glm2 <- glm(`ID swaps`~Software, data=all_coordinates_1st_trial, family = poisson())
Anova (ID_turbidity_glm2)

ID_turbidity_glm12 <- glm(`ID swaps`~Turbidity+Software, data=all_coordinates_1st_trial, family = poisson())
Anova (ID_turbidity_glm12)

ID_turbidity_glm1x2 <- glm(`ID swaps`~Turbidity*Software, data=all_coordinates_1st_trial, family = poisson())
Anova (ID_turbidity_glm1x2)

ID_turbidity_glm1.2 <- glm(`ID swaps`~Software + Software:Turbidity, data=all_coordinates_1st_trial, family = poisson())
Anova (ID_turbidity_glm1.2)

anova(ID_turbidity_glm2, ID_turbidity_glm1.2, test='Chisq')

# There's a significant interaction effect, but no main Turbidity effect. 

#Post-hoc on software effect7
ID_turbidity_Tukey <- lsmeans(ID_turbidity_glm1.2, pairwise~Software*Turbidity, adjust="tukey")
summary(ID_turbidity_Tukey)



######### Average distance moved (compared to groundtruth)


#We apply a Gamma linear model

metrics_turbidity$Software=as.factor(metrics_turbidity$Software)
metrics_turbidity$Turbidity=as.factor(metrics_turbidity$Turbidity)

DM_turbidity_glm1 <- glm(`Abs_average_total_distance_moved`~Turbidity, data=metrics_turbidity, family = Gamma())
Anova (DM_turbidity_glm1)

DM_turbidity_glm2 <- glm(`Abs_average_total_distance_moved`~Software, data=metrics_turbidity, family = Gamma())
Anova (DM_turbidity_glm2)

DM_turbidity_glm12 <- glm(`Abs_average_total_distance_moved`~Software+Turbidity, data=metrics_turbidity, family = Gamma())
Anova (DM_turbidity_glm12)

DM_turbidity_glm1x2 <- glm(`Abs_average_total_distance_moved`~Turbidity*Software, data=metrics_turbidity, family = Gamma())
Anova (DM_turbidity_glm1x2)

# No significant effects


######### Average time spent moving (compared to groundtruth)


#We apply a Gamma linear model

TSM_turbidity_glm1 <- glm(`Abs_average_time_spent_moving`~Turbidity, data=metrics_turbidity, family = Gamma())
Anova (TSM_turbidity_glm1)

TSM_turbidity_glm2 <- glm(`Abs_average_time_spent_moving`~Software, data=metrics_turbidity, family = Gamma())
Anova (TSM_turbidity_glm2)

TSM_turbidity_glm12 <- glm(`Abs_average_time_spent_moving`~Turbidity + Software, data=metrics_turbidity, family = Gamma())
Anova (TSM_turbidity_glm12)

TSM_turbidity_glm1x2 <- glm(`Abs_average_time_spent_moving`~Turbidity*Software, data=metrics_turbidity, family = Gamma())
Anova (TSM_turbidity_glm1x2)

# Turbidity effect

#Post-hoc on main turbidity effect

TSM_turbidity_main <- glht(TSM_turbidity_glm1, mcp(Turbidity = "Tukey"))
summary(TSM_turbidity_main)

######### Average NND (compared to groundtruth)

#Gaussian distribution

#We apply a Gaussian linear model

NND_turbidity_glm1 <- glm(`Abs_average_NND`~Turbidity, data=metrics_turbidity, family = Gamma())
Anova (NND_turbidity_glm1)

NND_turbidity_glm2 <- glm(`Abs_average_NND`~Software, data=metrics_turbidity, family = Gamma())
Anova (NND_turbidity_glm2)
summary(NND_turbidity_glm2)

NND_turbidity_glm12 <- glm(`Abs_average_NND`~Turbidity + Software, data=metrics_turbidity, family = Gamma())
Anova (NND_turbidity_glm12)
summary(NND_turbidity_glm12)

NND_turbidity_glm1x2 <- glm(`Abs_average_NND`~Turbidity*Software, data=metrics_turbidity, family = Gamma())
Anova (NND_turbidity_glm1x2)

anova(NND_turbidity_glm2, NND_turbidity_glm12, test='Chisq')

# Software effect

#Post-hoc on main software effect

NND_turbidity_main <- glht(NND_turbidity_glm2, mcp(Software = "Tukey"))
summary(NND_turbidity_main)

######### Average time feeding (compared to groundtruth)


#We apply a Gamma linear model

shapiro.test(metrics_turbidity$`Abs_average_time_feeding`)

TF_turbidity_glm1 <- glm(`Abs_average_time_feeding`~Turbidity, data=metrics_turbidity, family = Gamma())
Anova (TF_turbidity_glm1)

TF_turbidity_glm2 <- glm(`Abs_average_time_feeding`~Software, data=metrics_turbidity, family = Gamma())
Anova (TF_turbidity_glm2)

TF_turbidity_glm12 <- glm(`Abs_average_time_feeding`~Turbidity+Software, data=metrics_turbidity, family = Gamma())
Anova (TF_turbidity_glm12)

TF_turbidity_glm1x2 <- glm(`Abs_average_time_feeding`~Turbidity*Software, data=metrics_turbidity, family = Gamma())
Anova (TF_turbidity_glm1x2)


# No significant effect whatsoever


######### Average distance to feed (compared to groundtruth)

DF_turbidity_glm1 <- glm(`Abs_average_distance_to_feed`~Turbidity, data=metrics_turbidity, family = Gamma())
Anova (DF_turbidity_glm1)

DF_turbidity_glm2 <- glm(`Abs_average_distance_to_feed`~Software, data=metrics_turbidity, family = Gamma())
Anova (DF_turbidity_glm2)

DF_turbidity_glm12 <- glm(`Abs_average_distance_to_feed`~Turbidity+Software, data=metrics_turbidity, family = Gamma())
Anova (DF_turbidity_glm12)

DF_turbidity_glm1x2 <- glm(`Abs_average_distance_to_feed`~Turbidity*Software, data=metrics_turbidity, family = Gamma())
Anova (DF_turbidity_glm1x2)

# No significant effect whatsoever


###################### 2nd trial (challenging/non-challenging videos), dataset = all_coordinates_2nd_trial

######### Error ratios

#We apply a Gaussian linear model

all_coordinates_2nd_trial$Software=as.factor(all_coordinates_2nd_trial$Software)
all_coordinates_2nd_trial$Challenge=as.factor(all_coordinates_2nd_trial$Challenge)

ER_challenge_glm1 <- glm(`error_ratio`~Software, data=all_coordinates_2nd_trial, family = gaussian())
Anova (ER_challenge_glm1)

ER_challenge_glm2 <- glm(`error_ratio`~`Challenge`, data=all_coordinates_2nd_trial, family = gaussian())
Anova (ER_challenge_glm2)

ER_challenge_glm12 <- glm(`error_ratio`~Software+`Challenge`, data=all_coordinates_2nd_trial, family = gaussian())
Anova (ER_challenge_glm12)

ER_challenge_glm1x2 <- glm(`error_ratio`~Software*`Challenge`, data=all_coordinates_2nd_trial, family = gaussian())
Anova (ER_challenge_glm1x2)

anova(ER_challenge_glm1, ER_challenge_glm12, test='Chisq')
anova(ER_challenge_glm12, ER_challenge_glm1x2, test='Chisq')

# Significant software and challenging effects, no interaction

# Post-hoc comparisons

ER_challenge_main <- glht(ER_challenge_glm12, mcp(Challenge= "Tukey"))
summary(ER_challenge_main)

ER_sftw_main <- glht(ER_challenge_glm12, mcp(Software= "Tukey"))
summary(ER_sftw_main)

ER_challenge_int <- lsmeans(ER_challenge_glm1x2, pairwise~Software*Challenge, adjust="tukey")
summary(ER_challenge_int)

######### ID swaps

#We apply a Poisson linear model

ID_challenge_glm1 <- glm(`ID_switches`~Software, data=all_coordinates_2nd_trial, family = poisson())
Anova (ID_challenge_glm1)

ID_challenge_glm2 <- glm(`ID_switches`~`Challenge`, data=all_coordinates_2nd_trial, family = poisson())
Anova (ID_challenge_glm2)

ID_challenge_glm12 <- glm(`ID_switches`~Software+`Challenge`, data=all_coordinates_2nd_trial, family = poisson())
Anova (ID_challenge_glm12)

ID_challenge_glm1x2 <- glm(`ID_switches`~Software*`Challenge`, data=all_coordinates_2nd_trial, family = poisson())
Anova (ID_challenge_glm1x2)

anova(ID_challenge_glm1, ID_challenge_glm12, test='Chisq')
anova(ID_challenge_glm12, ID_challenge_glm1x2, test='Chisq')

# Significant software and challenging effects, no interaction

# Post-hoc comparisons

ID_challenge_main <- glht(ID_challenge_glm12, mcp(Challenge= "Tukey"))
summary(ID_challenge_main)

ID_sftw_main <- glht(ID_challenge_glm12, mcp(Software= "Tukey"))
summary(ID_sftw_main)

ID_interaction <- lsmeans(ID_challenge_glm1x2, pairwise~`Challenge`*Software, adjust="tukey")
summary(ID_interaction)
