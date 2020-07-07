library(nlme)
library(raster)
library(bbmle)
library(AICcmodavg)
library(lmtest)
library(ggplot2)

# Makes data.frame morph.data.lat.long
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Land.Use.Combining.R")


# Body Size across time
# Make the model without controling for spatial effects
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation,  
                                   random =list(Location.State = ~1), 
                                   weights = varFixed(~Specimen.Year), # Control for increasing variation
                                   data = morph.data.temp)
#######################################################
#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.temp$Long, morph.data.temp$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = min(xy[,2]-1.5), ymx = max(xy[,2] +1), 
               xmn = min(xy[,1]-1), xmx = max(xy[,1]+1)) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(head.vs.year.interaction.lm))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.temp$focal_rac_vect_time <- focal_rac_vect 
# fit the RAC model using the environmental variables and the residuals autocovariate


head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex + Elevation + 
                                           focal_rac_vect_time,  
                                random =~1|Location.State, 
                                data = morph.data.temp)
head.vs.year.interaction.var.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex + Elevation + 
                                           focal_rac_vect_time, 
                                weights = varFixed(~Specimen.Year),
                                random =~1|Location.State, control =lmeControl(maxIter = 1e8, msMaxIter = 1e8),
                                data = morph.data.temp)
head.vs.year.var.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation + 
                                    focal_rac_vect_time, 
                                weights = varFixed(~Specimen.Year),
                                random =~1|Location.State, control =lmeControl(maxIter = 1e8, msMaxIter = 1e8),
                                data = morph.data.temp)
head.vs.year.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Elevation + 
                                focal_rac_vect_time,
                                random =~1|Location.State, control =lmeControl(opt='optim'),
                                data = morph.data.temp)
plot(head.vs.year.lm)
summary(head.vs.year.var.lm)
r.squared.lme(head.vs.year.lm)

# Using aictab instead of ICtab
#  candiate.models <- list()
#  candiate.models[[1]] <- head.vs.year.lm
#  candiate.models[[2]] <- head.vs.year.interaction.lm
#  model.names <- c(head.vs.year.lm, head.vs.year.interaction.lm)
#  aictab(cand.set = candiate.models, nobs = 2460)

# Using ICtab
ICtab(head.vs.year.lm, head.vs.year.var.lm, 
      head.vs.year.interaction.var.lm, head.vs.year.interaction.lm, weights = TRUE)
lrtest(head.vs.year.lm, head.vs.year.var.lm)



# Body Size across temperature
# Make the model without controling for spatial effects
head.vs.average.temp.interaction.lm <- lme(Head.Width.mm ~ previous.summer * Sex + Elevation,  
                                           random =list(Location.State = ~1),
                                          data = morph.data.temp)
#######################################################
#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.temp$Long, morph.data.temp$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = min(xy[,2]-1.5), ymx = max(xy[,2] +1), 
               xmn = min(xy[,1]-1), xmx = max(xy[,1]+1)) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(head.vs.average.temp.interaction.lm))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.temp$focal_rac_vect_temp <- focal_rac_vect 
# fit the RAC model using the environmental variables and the residuals autocovariate

head.vs.average.temp.lm <- lme(Head.Width.mm ~ previous.summer + Sex + Elevation + 
                                focal_rac_vect_temp,  
                               random =list(Location.State = ~1), 
                              data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lme(Head.Width.mm ~ previous.summer * Sex + Elevation + 
                                            focal_rac_vect_temp,  
                                          random =list(Location.State = ~1), 
                                          data = morph.data.temp)
summary(head.vs.average.temp.interaction.lm)
r.squared.lme(head.vs.average.temp.interaction.lm)
lrtest(head.vs.average.temp.interaction.lm, head.vs.average.temp.lm)
ICtab(head.vs.average.temp.interaction.lm, head.vs.average.temp.lm, weights = TRUE)

# Creating a Figure 
# Creating a object containing the values of the slopes and intercepts for the model
head.vs.average.fixed <- head.vs.average.temp.interaction.lm[[4]][1]
head.vs.average.fixed <- head.vs.average.fixed$fixed
#head.vs.average.fixed <- head.vs.average.temp.interaction.lm[[1]]






# Body Size across agicultural cover
# Make the model without controling for spatial effects
agriculture.interaction.lme <- lme(Head.Width.mm ~  Agriculture * Sex + Elevation, 
                                  random = list(Location.State = ~1), 
                                  data=morph.data.land.use)
#######################################################
#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.land.use$Long, morph.data.land.use$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(agriculture.interaction.lme))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.land.use$focal_rac_vect_land <- focal_rac_vect 
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

r.squared.lme(agriculture.interaction.lme)
ICtab(agriculture.interaction.lme, agriculture.lme, weights = TRUE)
lrtest(agriculture.interaction.lme, agriculture.lme)
plot(agriculture.lme)

# Creating a object containing the values of the slopes and intercepts for the model
agriculture.interaction.lme.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.interaction.lme.fixed$fixed
#agriculture.fixed<- agriculture.interaction.lme[[1]]





# Body Size across urban gradient
# Make the model without controling for spatial effects
development.interaction.lme <- lme(Head.Width.mm ~  Development + Sex + Elevation, 
                                  random = list(Location.State = ~1),  
                                  data=morph.data.land.use)
#######################################################
#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.land.use$Long, morph.data.land.use$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(development.interaction.lme))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.land.use$focal_rac_vect_land_dev <- focal_rac_vect 
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
r.squared.lme(development.lme)

lrtest(development.interaction.lme, development.lme)
ICtab(development.interaction.lme, development.lme, weights = TRUE)
plot(development.lme)








#-------------------------------------------------------------------------------
# Background States (OLD)
#-------------------------------------------------------------------------------

temp.year.lme <- lme(previous.summer ~ Specimen.Year + Elevation, 
                     random = ~1|Location.State,
                    data = morph.data.temp)


#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.temp$Long, morph.data.temp$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(temp.year.lme))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.temp$focal_rac_vect_temptime <- focal_rac_vect 



temp.year.lme <- lme(previous.summer ~ Specimen.Year + Elevation + focal_rac_vect_temptime,
                    random = ~1|Location.State,
                    data = morph.data.temp)

summary(temp.year.lme)
r.squared.lme(temp.year.lme)






#library(glmmTMB)
#library(DHARMa)
#agriculture.year <- glmmTMB(Agriculture_raw ~ Specimen.Year + (1|Location.State),
#                            data=morph.data.land.use,
#                            #ziformula=~1,
#                            family=poisson)
#simulationOutput <- simulateResiduals(fittedModel = agriculture.year, plot = T)
#testZeroInflation(simulationOutput)
library(lme4)
agriculture.year <- glmer((Agriculture_raw) ~ Specimen.Year + Elevation + (1|Location.State),
                        #random = ~1|Location.State, 
                        family = poisson,
                       data=morph.data.land.use)
plot(agriculture.year)
qqnorm(resid(agriculture.year))
qqline(resid(agriculture.year))
#morph.data.land.use$residuals <- resid(agriculture.year)

#hist(morph.data.land.use$residuals)
#morph.data.land.use[morph.data.land.use$residuals == min(morph.data.land.use$residuals) | 
#                            morph.data.land.use$residuals == max(morph.data.land.use$residuals),]
##morph.data.land.use[morph.data.land.use$residuals == max(morph.data.land.use$residuals),]
#morph.data.land.use[morph.data.land.use$Agriculture_raw < 10,]


#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.land.use$Long, morph.data.land.use$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(agriculture.year))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.land.use$focal_rac_vect_landtime <- focal_rac_vect 


agriculture.year <- lme(Agriculture_raw ~ Specimen.Year + Elevation + focal_rac_vect_landtime,
                        random = ~ 1|Location.State,
                        data=morph.data.land.use)

#agriculture.year <- glmmTMB(Agriculture_raw ~ Specimen.Year + Elevation + focal_rac_vect_landtime + (1|Location.State),
#                            data=morph.data.land.use,
#                            #ziformula=~1,
#                            family=poisson)

agriculture.year <- glmer(Agriculture_raw ~ Specimen.Year + Elevation + focal_rac_vect_landtime + (1|Location.State),
                          #random = ~1|Location.State, 
                          family = poisson,
                          data=morph.data.land.use)

summary(agriculture.year)

#https://stats.stackexchange.com/questions/46345/how-to-calculate-goodness-of-fit-in-glm-r
with(summary(agriculture.year), 1 - deviance/null.deviance)
plot(agriculture.year)
qqnorm(agriculture.year)
hist((morph.data.land.use$Agriculture_raw))
hist((morph.data.land.use$Development))

plot(Agriculture_raw ~ Specimen.Year, data = morph.data.land.use)
abline(-12.41, 0.0013)

hist(morph.data.land.use$Agriculture_raw)



development.year <-glmer((Development_raw) ~ Specimen.Year + Elevation  + (1|Location.State),
                         #random = ~1|Location.State, 
                         family = poisson,
                         data=morph.data.land.use)

#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(morph.data.land.use$Long, morph.data.land.use$Lat)
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(development.year))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
plot(rast)
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
plot(focal_rac_rast)  
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
morph.data.land.use$focal_rac_vect_devtime <- focal_rac_vect 



development.year <-glmer((Development_raw) ~ Specimen.Year + Elevation + focal_rac_vect  + (1|Location.State),
                                                #random = ~1|Location.State, 
                                                family = poisson,
                                                data=morph.data.land.use)


summary(development.year)
with(summary(development.year), 1 - deviance/null.deviance)
plot(development.year)
qqnorm(resid(development.year))



#-------------------------------------------------------------------------------
# Repeatedly measured cites (OLD)
#-------------------------------------------------------------------------------

ag.changed <- unlist(apply(unique(morph.data.land.use["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.land.use[morph.data.land.use$Location.State == location, "Agriculture" ])) > 1)
                print(location)
}))

ag.max <- unlist(apply(unique(morph.data.land.use["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.land.use[morph.data.land.use$Location.State == location, "Agriculture" ])) > 1)
                print(morph.data.land.use[ 
                        morph.data.land.use$Agriculture == max(morph.data.land.use[morph.data.land.use$Location.State == location, "Agriculture" ]), "Location.State.Year"])
}))

ag.min.year.value <- unlist(apply(unique(morph.data.land.use["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.land.use[morph.data.land.use$Location.State == location, "Agriculture" ])) > 1)
                print(morph.data.land.use[ 
                        morph.data.land.use$Specimen.Year == min(morph.data.land.use[morph.data.land.use$Location.State == location, "Specimen.Year" ]), "Agriculture"][1])
}))
ag.max.year.value <- unlist(apply(unique(morph.data.land.use["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.land.use[morph.data.land.use$Location.State == location, "Agriculture" ])) > 1)
                print(morph.data.land.use[ 
                        morph.data.land.use$Specimen.Year == max(morph.data.land.use[morph.data.land.use$Location.State == location, "Specimen.Year" ]), "Agriculture"][1])
}))

morph.data.land.use.ag.change <- morph.data.land.use[morph.data.land.use$Location.State %in% ag.changed, ]
morph.data.land.use.ag.change$max.min <- ifelse(morph.data.land.use.ag.change$Location.State.Year %in% ag.max,
                                        "Max", "Min")

boxplot(Head.Width.mm ~ max.min, data = morph.data.land.use.ag.change[morph.data.land.use.ag.change$Sex == "Female", ])
max.min <- lme(Head.Width.mm ~max.min + Sex, # + Elevation + focal_rac_vect_land,
               random = ~1|Location.State, data=morph.data.land.use.ag.change)
summary(max.min)


number.of.remesured.sites <- unlist(apply(unique(morph.data.land.use["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.land.use[morph.data.land.use$Location.State == location, "Specimen.Year" ])) > 1)
                print(morph.data.land.use[ 
                        morph.data.land.use$Specimen.Year == min(morph.data.land.use[morph.data.land.use$Location.State == location, "Specimen.Year" ]), "Agriculture"][1])
}))
length(number.of.remesured.sites)



# Temperature 
 
number.of.remesured.sites.temp <- unlist(apply(unique(morph.data.temp["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ])) > 1)
                print(location)
}))
length(number.of.remesured.sites.temp)


number.of.remesured.sites.temp <- unlist(apply(unique(morph.data.temp["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ])) > 1)
                print(location)
}))
length(number.of.remesured.sites.temp)

morph.data.change.temp <- morph.data.temp[morph.data.temp$Location.State %in% number.of.remesured.sites.temp, ]
temp.changed <- unlist(apply(unique(morph.data.change.temp["Location.State"]), MARGIN =1, function(location){
        for(year in unique(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ])) {
                return(morph.data.temp[morph.data.temp$Specimen.Year == year, "previous.summer"][1] -
                        morph.data.temp[ 
                                morph.data.temp$Specimen.Year == min(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ]), "previous.summer"][1])
        }
                
}))


ag.min.year.value <- unlist(apply(unique(morph.data.temp["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "previous.summer" ])) > 1)
                print(morph.data.temp[ 
                        morph.data.temp$Specimen.Year == min(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ]), "previous.summer"][1])
}))
ag.max.year.value <- unlist(apply(unique(morph.data.temp["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "previous.summer" ])) > 1)
                print(morph.data.temp[ 
                        morph.data.temp$Specimen.Year == max(morph.data.temp[morph.data.temp$Location.State == location, "Specimen.Year" ]), "previous.summer"][1])
}))
difference.temp <- ag.max.year.value - ag.min.year.value
length(which(difference.temp < 0))
mean(difference.temp)


ag.max.year.value <- unlist(apply(unique(morph.data.temp["Location.State"]), MARGIN =1, function(location){
        if(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "previous.summer" ])) > 1)
                print(length(unique(morph.data.temp[morph.data.temp$Location.State == location, "previous.summer" ])))
}))


hist(morp.data.land.use$Agriculture)
