library(ggplot2)
library(effects)
library(ggpubr)

# Makes data.frame morph.data.lat.long
source("./Analyzing.Morphological.Data.RAC.LME.R")


# Correct year, due to changes to calculated the previous years land-use
morph.data.land.use$Specimen.Year <- morph.data.land.use$Specimen.Year +1


# Effect of Agriculture 
ef1 <- effect(term="Agriculture", 
              mod=agriculture.lme)

efdata1 <- as.data.frame(ef1)
efdata1$weight.agriculture <- (.17*(efdata1$Agriculture) + 11)


# Effect of Temperature 
ef2 <- effect(term="previous.summer",
              mod=head.vs.year.lm)
efdata2 <- as.data.frame(ef2)
efdata2$weight.previous.summer <- ((efdata2$previous.summer) - 11 )/.17




tiff("cummalativeplot.tiff", width = 7, height = 5, pointsize = 1/300, units = 'in', res = 300)
ggplot(efdata1, aes(y=fit, x=Agriculture,  color= "skyblue2")) +
   xlab("Agricultural Cover (%)") + ylab("Head Width (mm)")  +
   scale_x_continuous(sec.axis=sec_axis(trans= ~. *.17 +11, name="Average Summer Temperature (?C)"))+
   geom_line(data = efdata1, aes(y=fit, x=Agriculture, color= "skyblue2"), size=1.2) +
   geom_ribbon(data = efdata1, aes(ymin=fit-se, ymax=fit+se, 
                                   x=Agriculture),fill="skyblue2", color = "skyblue2", alpha=0.3) +  
   
   
   geom_line(data = efdata2, aes(y= fit, x= weight.previous.summer, color="black")) +
   geom_ribbon(data = efdata2, aes(ymin=fit-se, ymax=fit+se,
                                   x=weight.previous.summer), fill="black", color = "black", alpha=0.3) +
   scale_color_manual(name = "", labels = c("Temp.", "Agri."), values = c("black", "skyblue2")) + 
   theme_bw(base_size = 20) +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.ticks.length=unit(-0.25, "cm"),
         axis.ticks.x = element_line(color = "black"),
         axis.ticks.y = element_line(color = "black"),
         axis.ticks.x.top = element_line(color = "black"),
         axis.text.y = element_text(margin = margin(0,0.5,0,0, unit = 'cm')),
         axis.text.x = element_text(vjust = -5, margin = margin(0,0,0.80,0, unit = 'cm')),
         axis.text.x.top = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm')),
         legend.position = c(.5, .30),
         legend.justification = c(0.8, 0.45),
         legend.text = element_text(size=20),
         legend.title = element_text(size=20), #,
         legend.background = element_rect(fill= alpha(0.01))
   )
labs(color='')


dev.off()


