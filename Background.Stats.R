#-------------------------------------------------------------------------------
# Testing Background Stats
#-------------------------------------------------------------------------------
# Makes data.frame morph.data.lat.long
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Land.Use.Combining.R")

cor(morph.data.temp$Head.Width.mm, morph.data.temp$Intertegular.Width.mm)
# 0.9441092
cor(morph.data.temp$Head.Width.mm, morph.data.temp$Right.Wing.mm)
# 0.9556534
morph.data.temp.left.wing <- subset(morph.data.temp, !is.na(morph.data.temp$Left.Wing.mm))
cor(morph.data.temp.left.wing$Head.Width, morph.data.temp.left.wing$Left.Wing.mm)
#0.9538538

cor(morph.data.temp$Lat, morph.data.temp$previous.summer)
# -0.8654451
cor(morph.data.temp$Long, morph.data.temp$previous.summer)
# -0.5300274
cor(morph.data.temp$Elevation, morph.data.temp$previous.summer)
# -0.009508765

cor(morph.data.temp$Elevation, morph.data.temp$Long)
#  -0.3572058


cor(morph.data.land.use$Agriculture, morph.data.land.use$Long)
#-0.1304797
cor(morph.data.land.use$Agriculture, morph.data.land.use$Elevation)
# 0.1801722


lat.temp.lme <- lm(average.temp ~ Lat, data = morph.data.lat.long)
summary(lat.temp.lme)

plot(average.temp ~ Lat, data = morph.data.lat.long)


long.temp.lme <- lm(average.temp ~ Long, data = morph.data.lat.long)
summary(long.temp.lme)

plot(average.temp ~ Long, data = morph.data.lat.long)

# Subset morphological data inorder for the change and lat sampled through time not to be significant  not to be significant 
morph.data.lat.long.1905 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year > 1905) # &
                                          # morph.data.lat.long$Long < -85)





 # Is there an increase in temperature across years
temp.year.lme <- lme(previous.summer ~ Specimen.Year, random = ~1|EcoRegion, 
                     data = morph.data.temp)
#temp.year.lme <- lm(average.temp ~ Specimen.Year, data = morph.data.lat.long)
summary(temp.year.lme)
r.squared.lme(temp.year.lme)
temp.year.fixed <- temp.year.lme[[4]][1]

par(mar=c(4,5, 1,2))
plot(average.temp ~ Specimen.Year, data = morph.data.temp,
     ylab = "Average Temperature (C)",
     xlab = "Year",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(temp.year.fixed$fixed[1], temp.year.fixed$fixed[2], col = "blue", lwd = 3)






# Is there a change in the latitude that is sampled
lat.year.lme <- lme(Lat ~ Specimen.Year, random = ~1|EcoRegion, data = morph.data.temp)
summary(lat.year.lme)
r.squared.lme(lat.year.lme)

par(mar=c(4,5, 1,2))
plot(Lat ~ Specimen.Year, data = morph.data.temp,
     ylab = "Latitude",
     xlab = "Year",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(lat.year.lme[[1]][1], lat.year.lme[[1]][2], col = "blue", lwd = 3)




# Is there a change in the longitude that is sampled threw time
long.year.lme <- lme(Long ~ Specimen.Year, random = ~1|EcoRegion, data = morph.data.temp)
plot(long.year.lme)
summary(long.year.lme)
r.squared.lme(long.year.lme)

qqnorm(long.year.lme)
plot(long.year.lme)

par(mar=c(4,5, 1,2))
plot(Long ~ Specimen.Year, data = morph.data.temp[morph.data.temp$Long > -68, ],
     ylab = "Longitude",
     xlab = "Year",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(long.year.lme[[1]][1], long.year.lme[[1]][2], col = "blue", lwd = 3)


# Is there a change in the Elevation that is sampled threw time
elv.year.lme <- lme(sqrt(Elevation+1.1) ~ Specimen.Year, random = ~1|EcoRegion, data = morph.data.temp)
summary(elv.year.lme)
r.squared.lme(elv.year.lme)

qqnorm(elv.year.lme)
plot(elv.year.lme)
morph.data.temp$residuals.elv <- residuals(elv.year.lme)
hist((morph.data.temp$residuals.elv))
hist(morph.data.temp$Specimen.Year)


par(mar=c(4,5, 1,2))
plot(Elevation ~ Specimen.Year, data = morph.data.temp,
     ylab = "Elevation",
     xlab = "Year",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(long.year.lme[[1]][1], long.year.lme[[1]][2], col = "blue", lwd = 3)


par(mar=c(4,5, 1,2))
plot(residuals.elv ~ Specimen.Year, data = morph.data.temp,
     ylab = "Elevation",
     xlab = "Year",
     cex.lab = 1.9, cex.axis = 1.75, tck = 0.02)
abline(long.year.lme[[1]][1], long.year.lme[[1]][2], col = "blue", lwd = 3)

