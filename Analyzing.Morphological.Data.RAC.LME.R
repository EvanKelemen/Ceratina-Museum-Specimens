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
# calculate_rac
source("./calculate_rac.R")



# Pearson's Correlations
#Subset to specimens with all measurements 
morph.data.all.measurements <- subset(morph.data.temp, !is.na(morph.data.temp$Intertegular.Width.mm) &
                                        !is.na(morph.data.temp$Right.Wing.mm) & !is.na(morph.data.temp$Left.Wing.mm))
#Intergegular Width == 0.9435326
cor(morph.data.all.measurements$Head.Width.mm, morph.data.all.measurements$Intertegular.Width.mm)

#Right Wing == 0.9556519
cor(morph.data.all.measurements$Head.Width.mm, morph.data.all.measurements$Right.Wing.mm)

#Left WIng == 0.9550752
cor(morph.data.all.measurements$Head.Width.mm, morph.data.all.measurements$Left.Wing.mm)




#>>>> Body Size across time
# Make the model without controlling for spatial effects (Using the top model)
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp)
morph.data.temp$focal_rac_vect_time <- calculate_rac(morph.data.temp, head.vs.year.interaction.lm)

# fit the RAC model using the environmental variables and the residuals autocovariate
# .var models allow for variance to increase with time
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex + 
                                     Elevation + focal_rac_vect_time,  
                                   random =~1|Location.State, data = morph.data.temp)
head.vs.year.interaction.var.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex + Elevation + 
                                         focal_rac_vect_time, 
                                       weights = varFixed(~Specimen.Year),
                                       random =~1|Location.State, data = morph.data.temp)
head.vs.year.var.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation + 
                             focal_rac_vect_time, 
                           weights = varFixed(~Specimen.Year),
                           random =~1|Location.State, data = morph.data.temp)
head.vs.year.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation + 
                         focal_rac_vect_time,
                       random =~1|Location.State, data = morph.data.temp)
plot(head.vs.year.lm)
summary(head.vs.year.var.lm)
rsquared(head.vs.year.lm)


# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      head.vs.year.interaction.var.lm, head.vs.year.interaction.lm, weights = TRUE)

# Comparing if vairation imporved the model
lrtest(head.vs.year.lm, head.vs.year.var.lm)





#>>> Body Size across temperature
# Make the model without controling for spatial effects
head.vs.summer.temp.interaction.lm <- lme(Head.Width.mm ~ previous.summer * Sex + Elevation,  
                                           random =list(Location.State = ~1),
                                          data = morph.data.temp)
morph.data.temp$focal_rac_vect_temp <- calculate_rac(morph.data.temp, head.vs.summer.temp.interaction.lm)

# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.summer.temp.lm <- lme(Head.Width.mm ~ previous.summer + Sex + Elevation + 
                                focal_rac_vect_temp,  
                              random =list(Location.State = ~1), 
                              data = morph.data.temp)
head.vs.summer.temp.interaction.lm <- lme(Head.Width.mm ~ previous.summer * Sex + Elevation + 
                                            focal_rac_vect_temp,  
                                          random =list(Location.State = ~1), 
                                          data = morph.data.temp)

summary(head.vs.summer.temp.interaction.lm)
rsquared(head.vs.summer.temp.interaction.lm)
lrtest(head.vs.summer.temp.interaction.lm, head.vs.summer.temp.lm)
ICtab(head.vs.summer.temp.interaction.lm, head.vs.summer.temp.lm, weights = TRUE)

# Creating a Figure 
# Creating a object containing the values of the slopes and intercepts for the model
head.vs.summer.fixed <- head.vs.summer.temp.interaction.lm[[4]][1]
head.vs.temp.fixed <- head.vs.summer.fixed$fixed
#head.vs.average.fixed <- head.vs.average.temp.interaction.lm[[1]]






#>>> Body Size across agricultural cover
# Make the model without controlling for spatial effects
agriculture.interaction.lme <- lme(Head.Width.mm ~  Agriculture * Sex + Elevation, 
                                  random = list(Location.State = ~1), 
                                  data=morph.data.land.use)
morph.data.land.use$focal_rac_vect_land  <- calculate_rac(morph.data.land.use, agriculture.interaction.lme)


# fit the RAC model using the environmental variables and the residuals autocovariate
agriculture.interaction.lme <- lme(Head.Width.mm ~  Agriculture * Sex + Elevation + 
                                     focal_rac_vect_land, 
                                   random = list(Location.State = ~1),  
                                   data=morph.data.land.use)
agriculture.lme <- lme(Head.Width.mm ~  Agriculture + Sex + Elevation + 
                         focal_rac_vect_land, 
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use)
summary(agriculture.interaction.lme)

rsquared(agriculture.interaction.lme)
ICtab(agriculture.interaction.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.interaction.lme, agriculture.lme)
plot(agriculture.lme)



# Creating a object containing the values of the slopes and intercepts for the model
agriculture.interaction.lme.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.interaction.lme.fixed$fixed
#agriculture.fixed<- agriculture.interaction.lme[[1]]





#>>> Body Size across urban gradient
# Make the model without controlling for spatial effects
development.interaction.lme <- lme(Head.Width.mm ~  Development + Sex + Elevation, 
                                  random = list(Location.State = ~1),  
                                  data=morph.data.land.use)
morph.data.land.use$focal_rac_vect_land_dev  <- calculate_rac(morph.data.land.use, development.interaction.lme)


# fit the RAC model using the environmental variables and the residuals autocovariate
development.interaction.lme <- lme(Head.Width.mm ~  Development * Sex + Elevation + 
                                     focal_rac_vect_land_dev, 
                                   random = list(Location.State = ~1), 
                                   data=morph.data.land.use)
development.lme <- lme(Head.Width.mm ~  Development + Sex + Elevation + 
                         focal_rac_vect_land_dev, 
                       random = list(Location.State = ~1), 
                       data=morph.data.land.use)
summary(development.lme)
rsquared(development.lme)

lrtest(development.interaction.lme, development.lme)
ICtab(development.interaction.lme, development.lme, weights = TRUE)
plot(development.lme)









# Body Size across precipitation
# Make the model without controlling for spatial effects
head.vs.summer.precip.lm <- lme(Head.Width.mm ~ previous.precip + Sex + Elevation,  
                                             random =list(Location.State = ~1),
                                             data = morph.data.temp)
morph.data.temp$focal_rac_vect_precip  <- calculate_rac(morph.data.temp, head.vs.summer.precip.lm )


# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.summer.precip.lm <- lme(Head.Width.mm ~ previous.precip + Sex + Elevation + 
                                  focal_rac_vect_precip,  
                                random =list(Location.State = ~1), 
                                data = morph.data.temp)
head.vs.summer.precip.interaction.lm <- lme(Head.Width.mm ~ previous.precip * Sex + Elevation + 
                                              focal_rac_vect_precip,  
                                            random =list(Location.State = ~1), 
                                            data = morph.data.temp)
summary(head.vs.summer.precip.lm)
r.squared.lme(head.vs.summer.precip.lm)
lrtest(head.vs.summer.precip.interaction.lm, head.vs.summer.precip.lm)
ICtab(head.vs.summer.precip.interaction.lm, head.vs.summer.precip.lm, weights = TRUE)

# Creating a object containing the values of the slopes and intercepts for the model
precip.lme.fixed <-head.vs.summer.precip.lm[[4]][1]
precip.fixed <- precip.lme.fixed$fixed


# Body Size across precipitation minus the samples from the Great Flood of 1951
morph.data.noflood <- subset(morph.data.temp, morph.data.temp$previous.precip < 225)
# Make the model without controlling for spatial effects
head.vs.noflood.lm <- lme(Head.Width.mm ~ previous.precip + Sex + Elevation,  
                                            random =list(Location.State = ~1),
                                            data = morph.data.noflood)
morph.data.land.use$focal_rac_vect_noflood   <- calculate_rac(morph.data.noflood, head.vs.noflood.lm)

# fit the RAC model using the environmental variables and the residuals autocovariate
head.vs.noflood.lm <- lme(Head.Width.mm ~ previous.precip + Sex + Elevation + 
                            focal_rac_vect_noflood,  
                          random =list(Location.State = ~1), 
                          data = morph.data.noflood)
head.vs.noflood.interaction.lm <- lme(Head.Width.mm ~ previous.precip * Sex + Elevation + 
                                        focal_rac_vect_noflood,  
                                      random =list(Location.State = ~1), 
                                      data = morph.data.noflood)
summary(head.vs.noflood.lm)
r.squared.lme(head.vs.noflood.lm)
lrtest(head.vs.noflood.lm, head.vs.summer.precip.lm)
ICtab(head.vs.noflood.interaction.lm, head.vs.noflood.lm, weights = TRUE)

