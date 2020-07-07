#-------------------------------------------------------------------------------
# Analying Land Use Data
#-------------------------------------------------------------------------------
library(quantreg)
library(nlme)
library(chron)
library(lmtest)
library(stringr)
library(lmtest)
#install.packages("bbmle")
library(bbmle)
library(AICmodavg)
library(MuMI)
library(dplyr)
library(MuMIn)
# Makes data.frame morph.data.lat.long
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Land.Use.Combining.R")


#morph.data.land.use <- morph.data.land.use[ !is.na(morph.data.land.use$Head.Width), ]
#morph.data.land.use$newx = str_wrap(morph.data.land.use$land.use, width = 5)
#boxplot(morph.data.land.use$Head_Width ~ morph.data.land.use$newx)
#str(morph.data.land.use)

#land.use.lme.all <- lme(Head.Width.mm ~  Sex * average.precip * Lat * Long* Development * Elevation *Agriculture + 
#                      Sex * average.temp * Lat * Long* Development * Elevation *Agriculture,  
#                    random =  list(Location.State= ~1), data = morph.data.land.use)


# Can only have 35 interactions for dredge
# Included in the 35 model
land.use.lme.2interations <- lme(Head.Width.mm ~  Sex*previous.precip + Sex*previous.summer +
                                   Sex*Lat + Sex*Long + 
                                   Sex*Development + 
                                   Sex*Elevation +
                                   Sex*Agriculture + #Sex*average.dtr + Sex*frost.frequency +
                                   #Sex*wet.frequency +
                                   previous.precip*previous.summer  +
                                   #previous.precip*Development  + 
                                   #average.precip*Elevation + 
                                   #previous.precip*Agriculture + #wet.frequency*average.dtr + #average.precip*frost.frequency +
                                   #average.precip+wet.frequency +
                                   #previous.summer * Development  + 
                                   #average.temp* Elevation + 
                                   #previous.summer *Agriculture + #average.temp*average.dtr + # average.temp*frost.frequency
                                   #average.temp*wet.frequency +
                                   Lat*Long + Elevation +
                                   #Development*Elevation + 
                                   #Development*average.dtr + Development*frost.frequency +
                                   #Development*wet.frequency +
                                   #Agriculture*average.dtr + Agriculture*frost.frequency +
                                   #Agriculture*wet.frequency +
                                   Development*Agriculture, 
                                   #Elevation * Agriculture  + 
                                   #Elevation*average.dtr +  Elevation*frost.frequency +
                                   #Elevation*wet.frequency,  
                                   random =  list(EcoRegion= ~1), data = morph.data.land.use)
all.models.dredge <- dredge(land.use.lme.2interations, trace = 2)
model.sel(all.models.dredge)
?coefTable
?model.avg
ma_df <- model.avg(all.models.dredge, subset = delta < 2)
ma_coefs <- coefTable(ma_df, full = TRUE, adjust.se = TRUE)
coefnames <- row.names(ma_coefs)
ma_coefs <- as.data.frame(ma_coefs)
ma_coefs <- mutate(ma_coefs,
                   coefficient = coefnames,
                   t = Estimate / `Std. Error`,
                   p = pt(abs(t), df = 195, lower.tail = FALSE),
                   lower95 = Estimate - 1.96 * `Std. Error`,
                   upper95 = Estimate + 1.96 * `Std. Error`) %>% select(coefficient, Estimate, `Std. Error`, t, p, lower95, upper95)
knitr::kable(ma_coefs, digits = 4)
# For parrell computing 
#all.models.dredge <- pdredge(land.use.lme.2interations, trace = 2)
str(morph.data.land.use$Sex)
morph.data.land.use$Sex <- as.factor(morph.data.land.use$Sex)

morph.data.land.use.test <- morph.data.land.use[morph.data.land.use$Long > -92,]
land.use.lme<- lme(Head.Width.mm ~  Sex*previous.summer  +Sex*Agriculture +
                                    Long*Elevation + Lat +
                                    #average.precip + 
                                    Elevation + previous.precip + Specimen.Year +
                                    #average.temp +  
                                    Agriculture * Development,
                                    #wet.frequency +
                                    #average.dtr + 
                                    #frost.frequency,
                                    random =  list(EcoRegion= ~1), data = morph.data.land.use.test)

summary(land.use.lme)
test <- dredge(land.use.lme, trace = 2)
print(test)

land.use.lme <- lme(Head.Width.mm ~  Sex + Long,
                    random =  list(State.Province= ~1), data = morph.data.land.use)
r.squared.lme(land.use.lme)




ICtab(land.use.lme.minus.lat, land.use.lme, land.use.lme.interations.2, weights = TRUE)


land.use.lme <- lme(Intertegular.Width.mm ~ Specimen.Year + Sex + average.temp * Agriculture,  
                    random = ~1|Location.State, data = morph.data.land.use[! is.na(morph.data.land.use$Intertegular.Width.mm ) , ])

land.use.lme <- lme(Right.Wing.mm ~ Specimen.Year + Sex + average.temp * Agriculture + average.precip + Development,  
                    random = ~1|Location.State, data = morph.data.land.use[! is.na(morph.data.land.use$Right.Wing.mm ), ])



plot(morph.data.land.use$Head.Width.mm ~ morph.data.land.use$Specimen.Year)
points(Head.Width.mm ~ Specimen.Year, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ], col = "Red")
plot(morph.data.land.use$Agriculture ~ morph.data.land.use$Specimen.Year)

plot(Head.Width.mm ~ Development, data = morph.data.land.use[ morph.data.land.use$Sex == "Female", ],
     ylim= c(min(morph.data.land.use$Head.Width.mm), max(morph.data.land.use$Head.Width.mm)))
points(Head.Width.mm ~ Development, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ], col = "Red")
plot(morph.data.land.use$Head.Width.mm ~ morph.data.land.use$Lat)
plot(morph.data.land.use$Head.Width.mm ~ morph.data.land.use$Long)


development.year <- glmmTMB(Development ~ Specimen.Year + (1|EcoRegion),
                            data=morph.data.land.use,
                            ziformula=~1,
                            family=gaussian)
simulationOutput <- simulateResiduals(fittedModel = agriculture.year, plot = T)
testZeroInflation(simulationOutput)

development.year <- lme(Development ~ Specimen.Year,  
                    random =  list(EcoRegion= ~1), data = morph.data.land.use)
summary(development.year)
r.squared.lme(development.year)
qqnorm(development.year)


library(glmmTMB)
library(DHARMa)
agriculture.year <- glmmTMB(Agriculture ~ Specimen.Year + (1),
                            data=morph.data.land.use,
                            ziformula=~1,
                            family=gaussian)
simulationOutput <- simulateResiduals(fittedModel = agriculture.year, plot = T)
testZeroInflation(simulationOutput)

agriculture.year <- lme(Agriculture ~ Specimen.Year,  
                    random =  list(EcoRegion= ~1), data = morph.data.land.use)

summary(agriculture.year)
r.squared.lme(agriculture.year)
plot(agriculture.year)
qqnorm(agriculture.year)
hist((morph.data.land.use$Agriculture))


library(lme4)
agriculture.interaction.lme <- glm(Wing.Worn ~ previous.summer * Sex +Long +Elevation, # + (1|EcoRegion),
                                   family = binomial,  data=morph.data.temp[!is.na(morph.data.temp$asymmetry),])
morph.data.temp$Wing.Worn <- ifelse(morph.data.temp$Wing.Wear > 2, 1,0)

agriculture.interaction.lme <- lme(Head.Width.mm ~ Agriculture*Sex + Long + Lat + Elevation, random = list(EcoRegion = ~1), data=morph.data.land.use)
agriculture.interaction.lme <- lm(Head.Width.mm ~  Agriculture + Long + Elevation, 
                                  #random = list(EcoRegion = ~1), 
                                  data=morph.data.land.use)
agriculture.lme <- lme(Head.Width.mm ~ Agriculture+Sex + Long + Lat+ Elevation, random = list(EcoRegion = ~1), data=morph.data.land.use)

summary(agriculture.interaction.lme)
r.squared.lme(agriculture.lme)
plot(agriculture.interaction.lme)
qqnorm(agriculture.interaction.lme)
lrtest(agriculture.interaction.lme, agriculture.lme )

# Creating a object containing the values of the slopes and intercepts for the model
agriculture.fixed <- agriculture.interaction.lme[[4]][1]
agriculture.fixed <- agriculture.fixed$fixed

#Plotting
tiff("Agriculture.vs.Head.Width.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5, 1,2))
plot(Head.Width.mm ~ Agriculture, data = morph.data.land.use[ morph.data.land.use$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Foraging Habitat (%); Agricultural Land",
     ylim = c(min(morph.data.land.use$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.land.use$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02, pch = 16)
points(Head.Width.mm ~ Agriculture, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ],
       col = "coral", pch = 17)
abline(agriculture.fixed[1], agriculture.fixed[2], 
       col = "grey", lwd = 5)
abline(agriculture.fixed[1]  + agriculture.fixed[3], agriculture.fixed[2] + agriculture.fixed[4], 
       col = "firebrick", lwd = 5 , lty = 2)
legend(70, 1.4, legend=c("Female", "Male"),
       col=c(NA, "firebrick"), lty = c( NA, 2),
       cex=0.8, box.lty=0, bg="transparent")
legend(72, 1.4, legend=c("", ""),
       col=c("black", "coral"), pch= c(16, 17),
       cex=0.8, box.lty=0, bg="transparent")
dev.off()

development.interaction.lme <- lme(Head.Width.mm ~ Development*Sex + Lat + Long + Elevation, random = ~1|EcoRegion, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])
development.lme <- lme(Head.Width.mm ~ Development+Sex + Lat + Long + Elevation, random = ~1|EcoRegion, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])

summary(development.interaction.lme)
r.squared.lme(development.interaction.lme)
plot(development.interaction.lme)
qqnorm(development.interaction.lme)
lrtest(development.interaction.lme, development.lme )

land.use.interaction.lme <- lme(Head.Width.mm ~ Sex*Agriculture + Agriculture*Development + Sex*Development, random = ~1|State.Province, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])
land.use.lme <- lme(Head.Width.mm ~ Sex + Agriculture + Development, random = ~1|State.Province, data=morph.data.land.use)

test <- dredge(land.use.lme, trace = 2)

summary(land.use.interaction.lme)
plot(development.interaction.lme)
qqnorm(development.interaction.lme)


#land.use.lme <- lme(Agriculture ~ Specimen.Year + Lat + Long, random = ~1|Location.State, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])
#
#land.use.lme <- lme(Agriculture ~ Long,  
#                    random =  list(State.Province= ~1), data = morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])
#land.use.lme <- lm(Long ~ Specimen.Year, data = morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm) , ])

#land.use.lme <- lme(Head.Width.mm ~ frost.frequency + Sex, random = ~1|Location.State, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])
#land.use.lme <- lme(Head.Width.mm ~ average.dtr + Sex, random = ~1|Location.State, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])

#land.use.lme <- lme(frost.frequency ~ Specimen.Year, random = ~1|Location.State, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])

#land.use.lme <- lme(Head.Width.mm ~ Long+Sex, random = ~1|State.Province, data=morph.data.land.use[! is.na(morph.data.land.use$Head.Width.mm), ])

plot(frost.frequency ~ Specimen.Year, data=morph.data.land.use)
plot(previous.summer ~ Specimen.Year, data=morph.data.land.use)
summary(lm(Lat ~ Specimen.Year, data = morph.data.land.use))
summary(lm(Long ~ Specimen.Year, data = morph.data.land.use))
summary(lm(Elevation ~ Specimen.Year, data = morph.data.land.use))

