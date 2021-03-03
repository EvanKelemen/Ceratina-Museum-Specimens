#-------------------------------------------------------------------------------
# Mixed Models accounting for spatial proximity
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

# intergular Widths
morph.data.temp.iw <- subset(morph.data.temp, !is.na(morph.data.temp$Intertegular.Width.mm))
morph.data.land.use.iw <- subset(morph.data.land.use, !is.na(morph.data.land.use$Intertegular.Width.mm))


#>>>> Body Size across time
# Make the model without controlling for spatial effects (Using the top model)
head.vs.year.interaction.lm <- lme(Intertegular.Width.mm ~ Specimen.Year + Sex + Elevation + previous.precip +
                                   previous.summer * Sex,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp.iw)

# Calulate the RAC
morph.data.land.use.iw$focal_rac_vect_time <- calculate_rac(morph.data.temp.iw, head.vs.year.interaction.lm)


# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.year.interaction.two.lm <- lme(Intertegular.Width.mm  ~ Specimen.Year * Sex + Elevation + previous.summer * Sex+ previous.precip +
                                         focal_rac_vect_time, random =~1|Location.State, data = morph.data.temp.iw)
head.vs.year.interaction.two.var.lm <- lme(Intertegular.Width.mm ~ Specimen.Year * Sex + Elevation + previous.summer * Sex+previous.precip +
                                             focal_rac_vect_time, weights = varFixed(~Specimen.Year),
                                           random =~1|Location.State, data = morph.data.temp.iw)
head.vs.year.interaction.var.lm <- lme(Intertegular.Width.mm ~ Specimen.Year + Sex + Elevation + previous.summer * Sex+previous.precip +
                                         focal_rac_vect_time,weights = varFixed(~Specimen.Year),
                                       random =~1|Location.State, data = morph.data.temp.iw)
head.vs.year.interaction.lm <- lme(Intertegular.Width.mm  ~ Specimen.Year + Sex + Elevation + previous.summer * Sex+previous.precip +
                                     focal_rac_vect_time, random =~1|Location.State, control =lmeControl(opt='optim'),
                                   data = morph.data.temp.iw)
head.vs.year.var.lm <- lme(Intertegular.Width.mm  ~ Specimen.Year + Sex + Elevation + previous.summer + Sex+previous.precip +
                             focal_rac_vect_time,weights = varFixed(~Specimen.Year),
                           random =~1|Location.State, data = morph.data.temp.iw)
head.vs.year.lm <- lme(Intertegular.Width.mm ~ Specimen.Year + Sex + Elevation + previous.summer + Sex+previous.precip +
                         focal_rac_vect_time, random =~1|Location.State, control =lmeControl(opt='optim'),
                       data = morph.data.temp.iw)
plot(head.vs.year.lm)
summary(head.vs.year.lm)
rsquared(head.vs.year.lm)


# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      head.vs.year.interaction.var.lm, head.vs.year.interaction.lm,
      head.vs.year.interaction.two.var.lm, head.vs.year.interaction.two.lm,
      weights = TRUE)

# Comparing if vairation imporved the model
lrtest(head.vs.year.lm, head.vs.year.var.lm)




#>>> Body Size across agricultural cover
# Make the model without controlling for spatial effects
agriculture.interaction.lme <- lme(Intertegular.Width.mm ~  Agriculture * Sex + Elevation + Development + Specimen.Year, 
                                   random = list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year),
                                   data=morph.data.land.use.iw)
# Calcualte RAC
morph.data.land.use.iw$focal_rac_vect_land <- calculate_rac(morph.data.land.use.iw, agriculture.interaction.lme)

# fit the RAC model using the environmental variables and the residuals autocovariate
agriculture.interaction.lme <- lme(Intertegular.Width.mm ~  Agriculture * Sex + Elevation + Development + Specimen.Year +
                                     focal_rac_vect_land, 
                                   weights = varFixed(~Specimen.Year),
                                   random = list(Location.State = ~1),  
                                   data=morph.data.land.use.iw)
agriculture.lme <- lme(Intertegular.Width.mm  ~  Agriculture + Sex + Elevation +  Development + Specimen.Year +
                         focal_rac_vect_land, weights = varFixed(~Specimen.Year),
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use.iw)
summary(agriculture.lme)

rsquared(agriculture.lme)
ICtab(agriculture.interaction.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.interaction.lme, agriculture.lme)
plot(agriculture.lme)






#-------------------------------------------------------------------------------
#===============================================================================
#-------------------------------------------------------------------------------


morph.data.temp.rw <- subset(morph.data.temp, !is.na(morph.data.temp$Right.Wing.mm))
morph.data.land.use.rw <- subset(morph.data.land.use, !is.na(morph.data.land.use$Right.Wing.mm))

#>>>> Body Size across time
# Make the model without controlling for spatial effects (Using the top model)
head.vs.summer.interaction.lm <- lme(Right.Wing.mm ~ Specimen.Year + Sex + Elevation + previous.precip +
                                   previous.summer * Sex,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp.rw)

# calculate_rac
morph.data.land.use.rw$temp_lm_rac <- calculate_rac(morph.data.temp.rw, head.vs.summer.interaction.lm)


# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.year.interaction.two.lm <- lme(Right.Wing.mm  ~ Specimen.Year * Sex + Elevation + previous.summer * Sex+ previous.precip +
                                         temp_lm_rac, random =~1|Location.State, data = morph.data.temp.rw)
head.vs.year.interaction.two.var.lm <- lme(Right.Wing.mm ~ Specimen.Year * Sex + Elevation + previous.summer * Sex+previous.precip +
                                             temp_lm_rac, weights = varFixed(~Specimen.Year),
                                           random =~1|Location.State, data = morph.data.temp.rw)
head.vs.year.interaction.var.lm <- lme(Right.Wing.mm ~ Specimen.Year + Sex + Elevation + previous.summer * Sex+previous.precip +
                                         temp_lm_rac,weights = varFixed(~Specimen.Year),
                                       random =~1|Location.State, data = morph.data.temp.rw)
head.vs.year.interaction.lm <- lme(Right.Wing.mm  ~ Specimen.Year + Sex + Elevation + previous.summer * Sex+previous.precip +
                         temp_lm_rac, random =~1|Location.State, control =lmeControl(opt='optim'),
                         data = morph.data.temp.rw)
head.vs.year.var.lm <- lme(Right.Wing.mm  ~ Specimen.Year + Sex + Elevation + previous.summer + Sex+previous.precip +
                         temp_lm_rac,weights = varFixed(~Specimen.Year),
                         random =~1|Location.State, data = morph.data.temp.rw)
head.vs.year.lm <- lme(Right.Wing.mm ~ Specimen.Year + Sex + Elevation + previous.summer + Sex+previous.precip +
                         temp_lm_rac, random =~1|Location.State, control =lmeControl(opt='optim'),
                         data = morph.data.temp.rw)
plot(head.vs.year.lm)
summary(head.vs.year.lm)
rsquared(head.vs.year.lm)


# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      head.vs.year.interaction.var.lm, head.vs.year.interaction.lm,
      head.vs.year.interaction.two.var.lm, head.vs.year.interaction.two.lm,
      weights = TRUE)

# Comparing if vairation imporved the model
lrtest(head.vs.year.lm, head.vs.year.var.lm)







#>>> Body Size across agricultural cover
# Make the model without controlling for spatial effects
agriculture.interaction.lme <- lme(Right.Wing.mm ~  Agriculture * Sex + Elevation + Development + Specimen.Year, 
                                   random = list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year),
                                   data=morph.data.land.use.iw)
# Calculates_rac
morph.data.land.use.rw $landuse_lm_rac <- calculate_rac(morph.data.land.use.rw, 
                                                        agriculture.interaction.lme)

# fit the RAC model using the environmental variables and the residuals autocovariate
agriculture.interaction.lme <- lme(Right.Wing.mm ~  Agriculture * Sex + Elevation + Development + Specimen.Year +
                                     landuse_lm_rac,  
                                   weights = varFixed(~Specimen.Year),
                                   random = list(Location.State = ~1),  
                                   data=morph.data.land.use.rw)
agriculture.lme <- lme(Right.Wing.mm  ~  Agriculture + Sex + Elevation +  Development + Specimen.Year +
                         landuse_lm_rac, weights = varFixed(~Specimen.Year),
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use.rw)
summary(agriculture.lme)

rsquared(agriculture.lme)
ICtab(agriculture.interaction.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.interaction.lme, agriculture.lme)
plot(agriculture.lme)



# Creating a object containing the values of the slopes and intercepts for the model
agriculture.interaction.lme.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.interaction.lme.fixed$fixed
#agriculture.fixed<- agriculture.interaction.lme[[1]]

