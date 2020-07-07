library(effects)
library(psych)
library(ggplot2)
library(ggpubr)

# Correct year, due to changes to calculated the previous years land-use and temperature
morph.data.land.use$Specimen.Year <- morph.data.land.use$Specimen.Year +1
#morph.data.temp.female <- morph.data.temp[ morph.data.temp$Sex == "Female", ]
#morph.data.temp.male <- morph.data.temp[ morph.data.temp$Sex == "Male", ]


describe(morph.data.land.use$Agriculture)
ef1 <- effect(term="Agriculture", xlevels= list(Sex= "Male"), 
              mod=agriculture.interaction.lme)

efdata1 <- as.data.frame(ef1)
efdata1$weight.agriculture <- (.17*(efdata1$Agriculture) + 11)


describe(morph.data.temp$previous.summer)
ef2 <- effect(term="previous.summer", xlevels= list(Sex=c("Male", "Female")), 
              mod=head.vs.average.temp.interaction.lm)
efdata2 <- as.data.frame(ef2)
efdata2$weight.previous.summer <- ((efdata2$previous.summer) - 11 )/.17



#tiff("cummalativeplot.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
female <- ggplot(efdata1, aes(y=efdata1$fit, x=efdata1$Agriculture), color= "skyblue2") +
   xlab("Agricultural Cover (%)") + ylab("Head Width (mm)")  +
   scale_x_continuous(sec.axis=sec_axis(trans= ~. *.17 +11, name="Average Summer Temperature (°C)"))+
   geom_line(data = efdata1, aes(y=fit, x=efdata1$Agriculture ),color= "skyblue2", size=1.2) +
   geom_ribbon(data = efdata1, aes(ymin=efdata1$fit-efdata1$se, ymax=efdata1$fit+efdata1$se, 
                                   x=efdata1$Agriculture ),fill="skyblue2", color = "skyblue2", alpha=0.3) +
   
   
   geom_line(data = efdata2, aes(y= efdata2$fit, x= efdata2$weight.previous.summer)) +
   geom_ribbon(data = efdata2, aes(ymin=efdata2$fit-efdata2$se, ymax=efdata2$fit+efdata2$se,
                                   x=efdata2$weight.previous.summer),alpha=0.3) +
   theme_bw(base_size = 20) +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         #panel.border = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         axis.ticks.length=unit(-0.25, "cm"),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.title.x.top=element_blank(),
         axis.line.y.right = element_line(color = "black"),
         axis.text.y = element_text(margin = margin(0,0.5,0,0, unit = 'cm')),
         axis.line.x = element_line(color = "skyblue2"),
         axis.ticks.x = element_line(color = "skyblue2"),
         #axis.text.x = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm'), color = "skyblue2"),
         axis.text.x =element_blank(),
         axis.line.x.top = element_line(color = "black"),
         axis.ticks.x.top = element_line(color = "black"),
         axis.text.x.top = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm'), color = "black"),
         legend.position = c(-.08, .30),
         legend.justification = c(-0.3, 1),
         legend.text = element_text(size=20),
         legend.title = element_text(size=20),
         legend.background = element_rect(fill= alpha(0.01)))  +
   labs(color='')
#dev.off()



describe(morph.data.land.use$Agriculture)
ef1.m <- effect(term="Agriculture:Sex", #xlevels= list(Sex= "Male"), 
              mod=agriculture.interaction.lme)
efdata1.m <- as.data.frame(ef1.m)
efdata1.m <- subset(efdata1.m, efdata1.m$Sex == "Male")




describe(morph.data.temp$previous.summer)
ef2.m <- effect(term="previous.summer:Sex", #xlevels= list(Sex=c("Male")), 
              mod=head.vs.average.temp.interaction.lm)
efdata2.m <- as.data.frame(ef2.m)
efdata2.m <- subset(efdata2.m, efdata2.m$Sex == "Male")
efdata2.m$weight.previous.summer <- ((efdata2.m$previous.summer) - 11 )/.17



male <- ggplot(efdata1.m, aes(y=efdata1.m$fit, x=efdata1.m$Agriculture), color= "skyblue2") +
   xlab("Agricultural Cover (%)") + ylab("Head Width (mm)")  +
   scale_x_continuous(sec.axis=sec_axis(trans= ~. *.17 +11, name="Average Summer Temperature (°C)"))+
   geom_line(data = efdata1.m, aes(y=fit, x=efdata1.m$Agriculture ),color= "skyblue2", size=1.2) +
   geom_ribbon(data = efdata1.m, aes(ymin=efdata1.m$fit-efdata1.m$se, ymax=efdata1.m$fit+efdata1.m$se, 
                                   x=efdata1.m$Agriculture ),fill="skyblue2", color = "skyblue2", alpha=0.3) +
   
   
   geom_line(data = efdata2.m, aes(y= efdata2.m$fit, x= efdata2.m$weight.previous.summer)) +
   geom_ribbon(data = efdata2.m, aes(ymin=efdata2.m$fit-efdata2.m$se, ymax=efdata2.m$fit+efdata2.m$se,
                                   x=efdata2.m$weight.previous.summer),alpha=0.3) +
   theme_bw(base_size = 20) +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         #panel.border = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         axis.ticks.length=unit(-0.25, "cm"),
         axis.line.y.right = element_line(color = "black"),
         axis.text.y = element_text(margin = margin(0,0.5,0,0, unit = 'cm')),
         axis.line.x = element_line(color = "skyblue2"),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.title.x.top=element_blank(),
         axis.ticks.x = element_line(color = "skyblue2"),
         axis.text.x = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm'), color = "skyblue2"),
         axis.text.x.top=element_blank(),
         axis.line.x.top = element_line(color = "black"),
         axis.ticks.x.top = element_line(color = "black"),
         #axis.text.x.top = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm'), color = "black"),
         legend.position = c(-.08, .30),
         legend.justification = c(-0.3, 1),
         legend.text = element_text(size=20),
         legend.title = element_text(size=20),
         legend.background = element_rect(fill= alpha(0.01)))  +
   labs(color='')

figure <- ggarrange(female , male, 
                    #labels = c("A", "B"),
                    ncol = 1, nrow = 2)


tiff("cummalativeplot.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)

annotate_figure(figure,
                top = text_grob("Average Summer Temperature (°C)", hjust = 0.45, color = "black", size = 20),
                bottom = text_grob("Agricultural Cover (%)",hjust = 0.45, color = "skyblue2", size = 20),
                left = text_grob("Head Width (mm)", rot=90, size = 20))
dev.off()

#------
#tiff("cummalativeplot.tiff", width = 16, height = 10, pointsize = 1/300, units = 'in', res = 300)
par(mfrow=c(2, 1))
par(mar=c(1,5, 4.5,2))
plot(Head.Width.mm ~ Specimen.Year, data = morph.data.temp.female,
     ylab = "Head Width (mm)",
     xaxt = "n", xlab = "",
     xlim = c(min(morph.data.temp$Specimen.Year,  na.rm=T) - 0.1, max(morph.data.temp$Specimen.Year , na.rm=T) + 0.1),
     ylim = c(min(morph.data.temp.female$Head.Width.mm,  na.rm=T) - 0.01, max(morph.data.temp.female$Head.Width.mm, na.rm=T) + 0.01),
     cex.lab = 2.5, cex.axis = 2.5, tck = 0.02, pch = 16) #frame=FALSE)
points(Head.Width.mm ~ Specimen.Year, data = morph.data.land.use[ morph.data.land.use$Sex == "Female", ],
       col = "black", pch = 15, cex=1)
text(1900, 2.3, "A", cex = 2.5)
#points(Head.Width.mm ~ Specimen.Year, data = morph.data.temp.male,
#       pch = 17, col = "grey") #frame=FALSE)
#points(Head.Width.mm ~ Specimen.Year, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ],
#       col = "grey", pch = 25, cex=1, bg = "grey")
curve(((x*0.0197)-18.615179)*-0.0107 + 2.1355, from = 1902, to = 2019, add = TRUE, 
      col = "Red", lwd = 2 )
curve((((x*0.0013)-12.41)/768776)*0.0010 + 1.886, from = 1974, to = 2019, add = TRUE, 
      col = "green", lwd = 2)
#legend(1910, 1.52, legend=c("Temperature", "Agriculture"),
#       col=c("red", "green"), lty = c( 1, 1),
#       cex=1.5, box.lty=0, bg="transparent")

par(mar=c(4.5,5, 0,2))
plot(Head.Width.mm ~ Specimen.Year, data = morph.data.temp.male,
     ylab = "Head Width (mm)",
     xlab = "Year",
     xlim = c(min(morph.data.temp$Specimen.Year,  na.rm=T) - 0.1, max(morph.data.temp$Specimen.Year , na.rm=T) + 0.1),
     ylim = c(min(morph.data.temp.male$Head.Width.mm,  na.rm=T) - 0.22, max(morph.data.temp.male$Head.Width.mm, na.rm=T) + 0.01),
     cex.lab = 2.5, cex.axis = 2.5, tck = 0.02, pch = 17, col = "grey") #frame=FALSE)

#points(Head.Width.mm ~ Specimen.Year, data = morph.data.temp.male,
#       pch = 17, col = "grey") #frame=FALSE)
points(Head.Width.mm ~ Specimen.Year, data = morph.data.land.use[ morph.data.land.use$Sex == "Male", ],
       col = "grey", pch = 25, cex=1, bg = "grey")
curve(((x*0.0197)-18.615179)*-0.0044 + 1.7814, from = 1902, to = 2019, add = TRUE, 
      col = "Red", lwd = 2 )
curve((((x*0.0013)-12.241)/768776)*-0.0007 + 1.661, from = 1974, to = 2019, add = TRUE, 
      col = "green", lwd = 2)
text(1900, 2.05, "B", cex = 2.5)
legend(1900, 1.42, legend=c("Temperature", "Agriculture"),
       col=c("red", "green"), lty = c( 1, 1),
       cex= 2.5, box.lty=0, bg="transparent")
#dev.off()

