#-------------------------------------------------------------------------------
# Figures 3 and 4
#-------------------------------------------------------------------------------
# Makes data.frame morph.data.lat.long
source("./Final_Analyzing.Morphological.Data.RAC.LME.R")

#Plotting Figure 3
tiff("Temp.vs.Head.Width.tiff", width = 8, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5.5, 1,2))
plot(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Temperature (?C)",
     xlim = c(min(morph.data.lat.long$previous.summer,  na.rm=T) - 0.3, max(morph.data.lat.long$previous.summer, na.rm=T) + 0.3),
     ylim = c(min(morph.data.lat.long$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.lat.long$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.5, cex.axis = 1.5, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "skyblue2", pch = 17)
abline(head.vs.temp.fixed[1], head.vs.temp.fixed[5], col = "steelblue", lwd = 3)
abline(head.vs.temp.fixed[1]  + head.vs.temp.fixed[3], head.vs.temp.fixed[5], # + head.vs.temp.fixed[6], 
       col = "skyblue2", lwd = 3 , lty = 2)
legend(9.8, 1.42, legend=c("Female", "Male"),
       col=c("steelblue", "skyblue2"), lty = c( 1, 2),
       cex=01.5, box.lty=0, bg="transparent")
legend(10.5, 1.42, legend=c("", ""),
       col=c("steelblue", "skyblue2"), pch= c(16, 17),
       cex=1.5, box.lty=0, bg="transparent")
dev.off()







# Figure 4
tiff("Agriculture.vs.Head.Width.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5, 1,2))
plot(Head.Width.mm ~ Agriculture, data = morph.data.land.use[ morph.data.land.use$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Agricultural Cover (%)",
     xlim = c(min(morph.data.land.use$Agriculture,  na.rm=T) - 0.1, max(morph.data.land.use$Agriculture, na.rm=T) + 0.1),
     ylim = c(min(morph.data.land.use$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.land.use$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.5, cex.axis = 1.5, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ Agriculture, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ],
       col = "skyblue2", pch = 17)
abline(agriculture.fixed[1], agriculture.fixed[2], 
       col = "steelblue", lwd = 3)
abline(agriculture.fixed[1]  + agriculture.fixed[3], agriculture.fixed[2], # + agriculture.fixed[6], 
       col = "skyblue2", lwd = 3 , lty = 2)
legend(69, 1.41, legend=c("Female", "Male"),
       col=c("steelblue", "skyblue2"), lty = c( 1, 2),
       cex=1.5, box.lty=0, bg="transparent")
legend(73, 1.41, legend=c("", ""),
       col=c("steelblue", "skyblue2"), pch= c(16, 17),
       cex=1.5, box.lty=0, bg="transparent")
dev.off()





# Figure S1
tiff("Time vs Head Width.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5, 1,2))
plot(Head.Width.mm ~ Specimen.Year, data = morph.data.temp[ morph.data.temp$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Year",
     xlim = c(min(morph.data.temp$Specimen.Year,  na.rm=T) - 0.1, max(morph.data.temp$Specimen.Year, na.rm=T) + 0.1),
     ylim = c(min(morph.data.temp$Head.Width.mm,  na.rm=T) - 0.25, max(morph.data.temp$Head.Width.mm, na.rm=T) + 0.0),
     cex.lab = 1.5, cex.axis = 1.5, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ Specimen.Year, data = morph.data.temp[ morph.data.temp$Sex == "Male", ],
       col = "skyblue2", pch = 17)
legend(1900, 1.30, legend=c("Female", "Male"),
       col=c("steelblue", "skyblue2"), pch= c(16, 17),
       cex=1.5, box.lty=0, bg="transparent")
dev.off()



# Figure S2
tiff("figure S2.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mfrow=c(2,1))

par(mar=c(2.85,5, 2,2))
plot(Head.Width.mm ~ previous.precip, data = morph.data.temp[ morph.data.temp$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "",
     #xaxt = "n",
     xlim = c(min(morph.data.temp$previous.precip,  na.rm=T) - 0.1, max(morph.data.temp$previous.precip, na.rm=T) + 0.1),
     ylim = c(min(morph.data.temp$Head.Width.mm,  na.rm=T) - 0.25, max(morph.data.temp$Head.Width.mm, na.rm=T) + 0.0),
     cex.lab = 1.4, cex.axis = 1.4, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ previous.precip, data = morph.data.temp[ morph.data.temp$Sex == "Male", ],
       col = "skyblue2", pch = 17)
abline(precip.fixed[1], precip.fixed[2], 
       col = "steelblue", lwd = 3)
abline(precip.fixed[1]  + precip.fixed[3], precip.fixed[2], # + precip.fixed[6], 
       col = "skyblue2", lwd = 3 , lty = 2)
text(232, 2.3, "A", cex = 1.5)

par(mar=c(4.5,5, 0.85,2))
plot(Head.Width.mm ~ previous.precip, data = morph.data.temp[ morph.data.temp$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Monthly Precipitation (mm)",
     xlim = c(min(morph.data.temp$previous.precip,  na.rm=T) - 0.1, 205 + 0.1),
     ylim = c(min(morph.data.temp$Head.Width.mm,  na.rm=T) - 0.25, max(morph.data.temp$Head.Width.mm, na.rm=T) + 0.0),
     cex.lab = 1.4, cex.axis = 1.4, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ previous.precip, data = morph.data.temp[ morph.data.temp$Sex == "Male", ],
       col = "skyblue2", pch = 17)
legend(50, 1.39, legend=c("Female"),
       col=c("steelblue"), pch= c(16),
       cex=1.4, box.lty=0, bg="transparent") 
legend(100, 1.39, legend=c("Male"),
       col=c("skyblue2"), pch= c(17),
       cex=1.4, box.lty=0, bg="transparent") 
text(207, 2.3, "B", cex = 1.5)

dev.off()






