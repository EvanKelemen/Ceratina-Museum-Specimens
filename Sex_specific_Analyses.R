#-------------------------------------------------------------------------------
# Sex Specific Models
#-------------------------------------------------------------------------------
# Load Required Packages
library(nlme)
library(raster)
library(bbmle)
library(AICcmodavg)
library(lmtest)
library(ggplot2)
library(piecewiseSEM)

# Makes data.frame morph.data.lat.long
source("./Land.Use.Combining.R")

# Calculate_RAC
source("./calculate_rac.R")


# MALES
morph.data.temp.male <- subset(morph.data.temp, morph.data.temp$Sex == "Male")
morph.data.land.use.male <- subset(morph.data.land.use, morph.data.land.use$Sex == "Male")

#>>>> Body Size across time
# Make the model without controlling for spatial effects (Using the top model)
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.precip +
                                     previous.summer,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp.male)
# Calculate rac
morph.data.temp.male$focal_rac_vect_time <- calculate_rac(morph.data.temp.male, head.vs.year.interaction.lm)

# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.year.var.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.summer + previous.precip +
                             focal_rac_vect_time,weights = varFixed(~Specimen.Year),
                           random =~1|Location.State, data = morph.data.temp.male)
head.vs.year.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.summer + previous.precip +
                         focal_rac_vect_time, random =~1|Location.State, control =lmeControl(opt='optim'),
                       data = morph.data.temp.male)
plot(head.vs.year.lm)
summary(head.vs.year.lm)
rsquared(head.vs.year.lm)


# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      weights = TRUE)

# Comparing if vairation imporved the model
lrtest(head.vs.year.lm, head.vs.year.var.lm)



#>>> Body Size across agricultural cover
# Make the model without controlling for spatial effects
agriculture.interaction.lme <- lme(Head.Width.mm ~  Agriculture + Elevation + Development + Specimen.Year, 
                                   random = list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year),
                                   data=morph.data.land.use.male)
# Calculate rac
morph.data.land.use.male$focal_rac_vect_land <- calculate_rac(morph.data.land.use.male, agriculture.interaction.lme)


# fit the RAC model using the environmental variables and the residuals autocovariate
agriculture.var.lme <- lme(Head.Width.mm ~  Agriculture + Elevation + Development + Specimen.Year +
                                   focal_rac_vect_land, 
                                   weights = varFixed(~Specimen.Year),
                                   random = list(Location.State = ~1),  
                                   data=morph.data.land.use.male)
agriculture.lme <- lme(Head.Width.mm ~  Agriculture + Elevation +  Development + Specimen.Year +
                         focal_rac_vect_land, 
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use.male)
summary(agriculture.lme)

rsquared(agriculture.lme)
ICtab(agriculture.var.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.var.lme, agriculture.lme)
plot(agriculture.lme)



# Creating a object containing the values of the slopes and intercepts for the model
agriculture.interaction.lme.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.interaction.lme.fixed$fixed
#agriculture.fixed<- agriculture.interaction.lme[[1]]











# FEMALES
morph.data.temp.female <- subset(morph.data.temp, morph.data.temp$Sex == "Female")
morph.data.land.use.female <- subset(morph.data.land.use, morph.data.land.use$Sex == "Female")

#>>>> Body Size across time
# Make the model without controlling for spatial effects (Using the top model)
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.precip +
                                     previous.summer,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp.female)
# Calculate rac
morph.data.temp.female$focal_rac_vect_time <- calculate_rac(morph.data.temp.female, head.vs.year.interaction.lm)

# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.year.var.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.summer + previous.precip +
                             focal_rac_vect_time,weights = varFixed(~Specimen.Year),
                           random =~1|Location.State, data = morph.data.temp.female)
head.vs.year.lm <- lme(Head.Width.mm ~ Specimen.Year + Elevation + previous.summer + previous.precip +
                         focal_rac_vect_time, random =~1|Location.State, control =lmeControl(opt='optim'),
                       data = morph.data.temp.female)
plot(head.vs.year.lm)
summary(head.vs.year.lm)
rsquared(head.vs.year.lm)


# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      weights = TRUE)

# Comparing if vairation imporved the model
lrtest(head.vs.year.lm, head.vs.year.var.lm)



#>>> Body Size across agricultural cover
# Make the model without controlling for spatial effects
agriculture.interaction.lme <- lme(Head.Width.mm ~  Agriculture + Elevation + Development + Specimen.Year, 
                                   random = list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year),
                                   data=morph.data.land.use.female)
# Calculate rac
morph.data.land.use.female$focal_rac_vect_land <- calculate_rac(morph.data.land.use.female, agriculture.interaction.lme)


# fit the RAC model using the environmental variables and the residuals autocovariate
agriculture.var.lme <- lme(Head.Width.mm ~  Agriculture + Elevation + Development + Specimen.Year +
                                     focal_rac_vect_land, 
                                   weights = varFixed(~Specimen.Year),
                                   random = list(Location.State = ~1),  
                                   data=morph.data.land.use.female)
agriculture.lme <- lme(Head.Width.mm ~  Agriculture + Elevation +  Development + Specimen.Year +
                         focal_rac_vect_land, weights = varFixed(~Specimen.Year),
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use.female)
summary(agriculture.var.lme)

rsquared(agriculture.lme)
ICtab(agriculture.var.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.var.lme, agriculture.lme)
plot(agriculture.lme)


# Creating a object containing the values of the slopes and intercepts for the model
agriculture.interaction.lme.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.interaction.lme.fixed$fixed
#agriculture.fixed<- agriculture.interaction.lme[[1]]

