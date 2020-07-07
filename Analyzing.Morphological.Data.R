#-------------------------------------------------------------------------------
# Analysising morphological and temperature data
#-------------------------------------------------------------------------------
library(quantreg)
library(nlme)
library(chron)
library(lmtest)
install.packages("AICcmodavg")
library(bbmle)
library(AICcmodavg)

# Makes data.frame morph.data.lat.long
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Land.Use.Combining.R")


# >>>>>>>>>>>> Plotting Data <<<<<<<<<<<<<<<<<<<<<<<<<<<
#Plotting the data to visualize it

#Plotting by year
#head.vs.year.var.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex, weights = varFixed(~Specimen.Year), 
#                       random = ~1|EcoRegion, 
#                       data = morph.data.temp)
#head.vs.year.var.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex, weights = varFixed(~Specimen.Year), 
#                           random = ~1|EcoRegion, data = morph.data.temp)
head.vs.year.lm <- lme(Head.Width.mm ~ Specimen.Year + Sex + Long + Lat + Elevation,  
                           random = list(EcoRegion = ~1), 
                       data = morph.data.temp)
head.vs.year.interaction.lm <- lme(Head.Width.mm ~ Specimen.Year * Sex + Long + Lat + Elevation,  
                                   random =list(EcoRegion = ~1), data = morph.data.temp)
head.vs.year.interaction.lm <- lm(Head.Width.mm ~ Specimen.Year + Sex + Long + Lat + Elevation,  
                                   #random =list(EcoRegion = ~1), 
                                  data = morph.data.temp)
plot(head.vs.year.var.lm)
summary(head.vs.year.interaction.lm)
r.squared.lme(head.vs.year.lm)
lrtest(head.vs.year.lm, head.vs.year.interaction.lm)
ICtab( head.vs.year.var.lm, head.vs.year.var.interaction.lm,
       head.vs.year.lm, head.vs.year.interaction.lm, weights = TRUE)

#Plotting
tiff("Year.vs.Head.Width.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5.5, 1,2))
plot(Head.Width.mm ~ Specimen.Year, data = morph.data.land.use[morph.data.land.use$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Year",
     ylim = c(min(morph.data.temp$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.temp$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02, pch = 16)
points(Head.Width.mm ~ Specimen.Year, data = morph.data.temp[morph.data.temp$Sex == "Male", ],
       col = "coral", pch = 17)
#abline(head.vs.year.lm[[1]][1], head.vs.year.lm[[1]][2], col = "blue", lwd = 3)
#abline(head.vs.year.lm[[1]][1]  + head.vs.year.lm[[1]][3], head.vs.year.lm[[1]][2], 
#       col = "red", lwd = 3 , lty = 2)
legend(1900, 1.4, legend=c("Female", "Male"),
       col=c("black", "coral"), pch= c(16, 17), cex=0.8, box.lty=0, bg="transparent")
dev.off()


#----
head.vs.average.temp.lm <- lme(Head.Width.mm ~ Long + Sex,  
                               random = ~1|EcoRegion, data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lme(Head.Width.mm ~ Long * Sex,  
                                           random = ~1|EcoRegion, data = morph.data.temp)
plot(head.vs.average.temp.interaction.lm)
qqnorm(head.vs.average.temp.interaction.lm)
summary(head.vs.average.temp.interaction.lm)
r.squared.lme(head.vs.average.temp.interaction.lm)
lrtest(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm)
ICtab(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm, weights = TRUE)

par(mar=c(4.5,5.5, 1,2))
plot(Head.Width.mm ~ Long, data = morph.data.temp[morph.data.temp$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Temperature (°C)",
     ylim = c(min(morph.data.temp$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.temp$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.5, cex.axis = 0.8, tck = 0.02, pch = 16)
points(Head.Width.mm ~ Long, data = morph.data.temp[morph.data.temp$Sex == "Male", ],
       col = "grey", pch = 17)
abline(head.vs.average.fixed[1], head.vs.average.fixed[2], col = "black", lwd = 3)
abline(head.vs.average.fixed[1]  + head.vs.average.fixed[3], head.vs.average.fixed[2] + head.vs.average.fixed[4], 
       col = "grey", lwd = 3 , lty = 2)
legend(10.5, 1.4, legend=c("Female", "Male"),
       col=c("black", "grey"), lty = c( 1, 2),
       cex=0.8, box.lty=0, bg="transparent")
legend(10.90, 1.4, legend=c("", ""),
       col=c("black", "grey"), pch= c(16, 17),
       cex=0.8, box.lty=0, bg="transparent")


head.vs.average.temp.lm <- lme(Head.Width.mm ~ Elevation + Sex,  
                               random = ~1|EcoRegion, data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lme(Head.Width.mm ~ Elevation * Sex,  
                                           random = ~1|EcoRegion, data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lme(Wing.Wear ~  previous.summer * Sex + Long + Elevation,  
                                           random = ~1|EcoRegion, data = morph.data.temp[!is.na(morph.data.temp$Wing.Wear) ,])
head.vs.average.temp.interaction.lm <- lme(asymmetry ~  previous.summer * Sex,  
                                           random = ~1|EcoRegion, data = morph.data.temp[!is.na(morph.data.temp$asymmetry) ,])
plot(head.vs.average.temp.interaction.lm)
qqnorm(head.vs.average.temp.interaction.lm)
summary(head.vs.average.temp.interaction.lm)
r.squared.lme(head.vs.average.temp.interaction.lm)
lrtest(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm)
ICtab(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm, weights = TRUE)

#----


# Plotting by temperature 
# BODY SIZE
head.vs.average.temp.lm <- lme(Head.Width.mm ~ previous.summer + Sex + Long,  
                              random = ~1|EcoRegion, data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lme(Head.Width.mm ~ previous.summer * Sex + Long + Elevation,  
                              random = ~1|EcoRegion, data = morph.data.temp)
head.vs.average.temp.interaction.lm <- lm(Head.Width.mm ~ previous.summer * Sex + Long + Elevation,  
                                           #random = ~1|EcoRegion, 
                                          data = morph.data.temp)

plot(head.vs.average.temp.interaction.lm)
qqnorm(head.vs.average.temp.interaction.lm)
summary(head.vs.average.temp.interaction.lm)
r.squared.lme(head.vs.average.temp.interaction.lm)
lrtest(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm)
ICtab(head.vs.average.temp.lm, head.vs.average.temp.interaction.lm, weights = TRUE)

# Creating a object containing the values of the slopes and intercepts for the model
head.vs.average.fixed <- head.vs.average.temp.interaction.lm[[4]][1]
head.vs.average.fixed <- head.vs.average.fixed$fixed

#Plotting
tiff("Temp.vs.Head.Width.tiff", width = 8, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5.5, 1,2))
plot(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Temperature (°C)",
     ylim = c(min(morph.data.lat.long$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.lat.long$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.5, cex.axis = 0.8, tck = 0.02, pch = 16)
points(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "grey", pch = 17)
abline(head.vs.average.fixed[1], head.vs.average.fixed[2], col = "black", lwd = 3)
abline(head.vs.average.fixed[1]  + head.vs.average.fixed[3], head.vs.average.fixed[2] + head.vs.average.fixed[4], 
       col = "grey", lwd = 3 , lty = 2)
legend(10.5, 1.4, legend=c("Female", "Male"),
       col=c("black", "grey"), lty = c( 1, 2),
       cex=0.8, box.lty=0, bg="transparent")
legend(10.90, 1.4, legend=c("", ""),
       col=c("black", "grey"), pch= c(16, 17),
       cex=0.8, box.lty=0, bg="transparent")
dev.off()

library(ggplot2)
windows()
ggplot(morph.data.temp[morph.data.temp$Sex == "Male", ], aes(previous.summer, Head.Width.mm)) +
   geom_point(color = "lightgray") + geom_density_2d()
# Relative Differences in the relationship
log.head.vs.average.temp.lm <- lme(log(Head.Width.mm) ~ log(previous.summer) + Sex,  
                               random = ~1|EcoRegion, data = morph.data.temp)
log.head.vs.average.temp.interaction.lm <- lme(log(Head.Width.mm) ~ log(previous.summer) * Sex,  
                                           random = ~1|EcoRegion, data = morph.data.temp)
plot(log.head.vs.average.temp.interaction.lm)
qqnorm(log.head.vs.average.temp.interaction.lm)
summary(log.head.vs.average.temp.lm)
r.squared.lme(log.head.vs.average.temp.lm)
lrtest(log.head.vs.average.temp.lm, log.head.vs.average.temp.interaction.lm )
ICtab(log.head.vs.average.temp.lm, log.head.vs.average.temp.interaction.lm , weights = TRUE)

plot(log(Head.Width.mm) ~ log(previous.summer), data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Temperature (C)",
     ylim = c(min(log(morph.data.lat.long$Head.Width.mm),  na.rm=T) - 0.1, max(log(morph.data.lat.long$Head.Width.mm), na.rm=T) + 0.1),
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02, pch = 16)
points(log(Head.Width.mm) ~ log(previous.summer), data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "coral", pch = 17)
abline(head.vs.average.fixed[1], head.vs.average.fixed[2], col = "grey", lwd = 5)
abline(head.vs.average.fixed[1]  + head.vs.average.fixed[3], head.vs.average.fixed[2] + head.vs.average.fixed[4], 
       col = "firebrick", lwd = 5 , lty = 2)
legend(10.5, 1.4, legend=c("Female", "Male"),
       col=c("grey", "firebrick"), lty = c( 1, 2),
       cex=0.8, box.lty=0, bg="transparent")
legend(10.90, 1.4, legend=c("", ""),
       col=c("black", "coral"), pch= c(16, 17),
       cex=0.8, box.lty=0, bg="transparent")

#-----

#-------
#head.to.wing.vs.average.temp.lm <- lme((Intertegular.Width.mm + Right.Wing.mm) ~ previous.summer *Sex + previous.summer* Right.Wing.mm ,
#                                       random = list(EcoRegion = ~1), data = morph.data.temp)
#head.to.wing.vs.average.temp.lm <- lme((Intertegular.Width.mm + Head.Width.mm) ~ previous.summer *Sex + previous.summer*Head.Width.mm,
#                                       random = list(EcoRegion = ~1), data = morph.data.temp)
#head.to.wing.vs.average.temp.lm <- lme((Right.Wing.mm + Head.Width.mm) ~ previous.summer *Sex + previous.summer*Right.Wing.mm,
#                                       random = list(EcoRegion = ~1), data = morph.data.temp)

head.to.wing.vs.average.temp.var.lm <- lme(head.to.intertegular ~ previous.summer + Sex,  weights = varFixed(~Head.Width.mm),
                                           random = ~1|EcoRegion, data = morph.data.temp)                                                                                                          # ! morph.data.lat.long$Sample %in% c("INO24", "QCO63"), ])
#head.to.wing.vs.average.temp.interaction.var.lm <- lme(head.to.wing ~ average.temp * Sex,  weights = varFixed(~Head.Width.mm),
#                                       random = list(EcoRegion = ~1), data = morph.data.remp)
head.to.wing.vs.average.temp.lm <- lme(head.to.intertegular ~ previous.summer + Sex,  
                                        random = list(EcoRegion = ~1), data = morph.data.temp)
#head.to.wing.vs.average.temp.interaction.lm <- lme(head.to.wing ~ previous.summer * Sex, 
#                                           random = list(EcoRegion = ~1), data = morph.data.temp)


morph.data.lat.long.head <- morph.data.lat.long[ ! is.na(morph.data.lat.long$head.to.wing), ]
morph.data.lat.long.head$residual <- resid(head.to.wing.vs.average.temp.var.lm)
plot(head.to.wing.vs.average.temp.lm)
hist(resid(head.to.wing.vs.average.temp.var.lm))
qqnorm(head.to.wing.vs.average.temp.lm)
summary(head.to.wing.vs.average.temp.var.lm)
r.squared.lme(head.to.wing.vs.average.temp.lm)
lrtest(head.to.wing.vs.average.temp.lm, head.to.wing.vs.average.temp.var.lm)
ICtab(head.vs.average.temp.lm, #head.vs.average.temp.interaction.lm, 
      head.to.wing.vs.average.temp.var.lm, #head.to.wing.vs.average.temp.interaction.var.lm ,
      weights = TRUE)
plot(Right.Wing.mm ~ Head.Width.mm, data = morph.data.lat.long)

plot(head.to.wing ~ average.temp, data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Ratio of Right Wing Length to Head Width",
     xlab = "Average Temperature (C)",
     ylim = c(min(morph.data.lat.long$head.to.wing,  na.rm=T) - 0.01, max(morph.data.lat.long$head.to.wing, na.rm=T) + 0.01),
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
points(head.to.wing ~ average.temp, data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "coral", pch = 2)
abline(head.to.wing.vs.average.temp.lm[[1]][1], head.to.wing.vs.average.temp.lm[[1]][2], col = "blue", lwd = 3)
abline(head.to.wing.vs.average.temp.lm[[1]][1]  + head.to.wing.vs.average.temp.lm[[1]][3], head.to.wing.vs.average.temp.lm[[1]][2], 
       col = "red", lwd = 3 , lty = 2)

plot(average.temp ~ Lat, data = morph.data.lat.long)

average.temp.vs.year <- lm(average.temp ~ Specimen.Year, data = morph.data.lat.long)
summary(average.temp.vs.year)
plot(average.temp ~ Specimen.Year, data = morph.data.lat.long,
     xlab = "Year",
     ylab = "Average Temperature of Locations Sampled",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(average.temp.vs.year[[1]][1], average.temp.vs.year[[1]][2], col = "black", lwd = 3)


wing.wear.average.temp <- gls(Wing.Wear ~ average.temp + Sex, data = morph.data.lat.long)
summary(wing.wear.average.temp)
plot(wing.wear.average.temp)
plot(Wing.Wear ~ Specimen.Year, data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Wing Wear",
     xlab = "Year",
     ylim = c(0, 5),
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
points(Wing.Wear ~ Specimen.Year, data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "coral", pch = 2)


morph.data.lat.long$Month <- factor(morph.data.lat.long$Month, levels = c("March",
                                "April", "May", "June", "July", "August", "September", 
                                "October", "November"))

boxplot(Wing.Wear ~ Month, data = morph.data.lat.long[!morph.data.lat.long$Month == "" &
                                                              morph.data.lat.long$Sex == "Male", ], order())

boxplot(Wing.Wear ~ Month, data = morph.data.lat.long[!morph.data.lat.long$Month == "" &
                                                              morph.data.lat.long$Sex == "Female", ], order())



boxplot(Wing.Wear ~ Month, data = morph.data.lat.long[!morph.data.lat.long$Month == "" &
                                                              morph.data.lat.long$Sex == "Female" &
                                                              morph.data.lat.long$average.temp > 12.73, ], order())
boxplot(Wing.Wear ~ Month, data = morph.data.lat.long[!morph.data.lat.long$Month == "" &
                                                              morph.data.lat.long$Sex == "Female" &
                                                              morph.data.lat.long$average.temp < 8.02, ], order())


head.vs.average.temp.lm <- lm(log(Head.Width.mm) ~ log(Intertegular.Width.mm) * log(average.temp), data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ])
summary(head.vs.average.temp.lm)

head.vs.average.temp.lm <- lm(log(Right.Wing.mm) ~ log(Intertegular.Width.mm) * log(average.temp), data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ])
summary(head.vs.average.temp.lm)

head.vs.average.temp.lm <- lm(log(Head.Width.mm) ~ log(Right.Wing.mm) * log(average.temp), data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ])
summary(head.vs.average.temp.lm)
