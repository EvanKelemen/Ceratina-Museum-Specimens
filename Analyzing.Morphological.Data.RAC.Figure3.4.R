#-------------------------------------------------------------------------------
# Figures 3 and 4
#-------------------------------------------------------------------------------


#Plotting Figure 3
tiff("Temp.vs.Head.Width.tiff", width = 8, height = 5, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(4.5,5.5, 1,2))
plot(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Female", ],
     ylab = "Head Width (mm)",
     xlab = "Average Summer Temperature (°C)",
     xlim = c(min(morph.data.lat.long$previous.summer,  na.rm=T) - 0.3, max(morph.data.lat.long$previous.summer, na.rm=T) + 0.3),
     ylim = c(min(morph.data.lat.long$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.lat.long$Head.Width.mm, na.rm=T) + 0.1),
     cex.lab = 1.5, cex.axis = 1.5, tck = 0.02, pch = 16, col = "steelblue") #frame=FALSE)
points(Head.Width.mm ~ previous.summer, data = morph.data.lat.long[morph.data.lat.long$Sex == "Male", ],
       col = "skyblue2", pch = 17)
abline(head.vs.average.fixed[1], head.vs.average.fixed[2], col = "steelblue", lwd = 3)
abline(head.vs.average.fixed[1]  + head.vs.average.fixed[3], head.vs.average.fixed[2] + head.vs.average.fixed[6], 
       col = "skyblue2", lwd = 3 , lty = 2)
legend(9.8, 1.42, legend=c("Female", "Male"),
       col=c("steelblue", "skyblue2"), lty = c( 1, 2),
       cex=01.5, box.lty=0, bg="transparent")
legend(10.5, 1.42, legend=c("", ""),
       col=c("steelblue", "skyblue2"), pch= c(16, 17),
       cex=1.5, box.lty=0, bg="transparent")
dev.off()


# Plotting ggplot
#tiff("Temp.vs.Head.Width.ggplot.tiff", width = 8, height = 5, pointsize = 1/300, units = 'in', res = 300)
#ggplot(morph.data.temp, aes(y=Head.Width.mm, x=previous.summer, color=Sex, shape=Sex)) + geom_point(size=3) + 
#  xlab("Average Summer Temperature (°C)") + ylab("Head Width (mm)") +
#  geom_abline(intercept =head.vs.average.fixed[1], 
#              slope =head.vs.average.fixed[2], color = "steelblue", size =1.5)+
#  geom_abline(intercept =head.vs.average.fixed[1]  + head.vs.average.fixed[3],
#              slope =head.vs.average.fixed[2] + head.vs.average.fixed[6], color = "skyblue2", linetype = "dashed", size =1.5)+
#  scale_color_manual(name = "", labels= c("Female", "Male"), values= c("Steelblue", "Skyblue2")) +
#  scale_shape_manual(name = "", labels= c("Female", "Male"), values = c(16, 17)) +
#  theme_bw(base_size = 20) +
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        #panel.border = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"),
#        axis.ticks.length=unit(-0.25, "cm"),
#        axis.text.y = element_text(margin = margin(0,0.5,0,0, unit = 'cm')),
#        axis.text.x = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm')), 
#        legend.position = c(-.06, .30),
#        legend.justification = c(-0.3, 1),
#        legend.text = element_text(size=20),
#        legend.title = element_text(size=20),
#        legend.background = element_rect(fill= alpha(0.01)))  +
#  labs(color='')
#dev.off()




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
abline(agriculture.fixed[1]  + agriculture.fixed[3], agriculture.fixed[2] + agriculture.fixed[6], 
       col = "skyblue2", lwd = 3 , lty = 2)
legend(69, 1.41, legend=c("Female", "Male"),
       col=c("steelblue", "skyblue2"), lty = c( 1, 2),
       cex=1.5, box.lty=0, bg="transparent")
legend(73, 1.41, legend=c("", ""),
       col=c("steelblue", "skyblue2"), pch= c(16, 17),
       cex=1.5, box.lty=0, bg="transparent")
dev.off()



# Figure 4 ggplot
#tiff("Agriculture.vs.Head.Width.ggplot.tiff", width = 8, height = 5, pointsize = 1/300, units = 'in', res = 300)
#ggplot(morph.data.land.use, aes(y=Head.Width.mm, x=Agriculture, color=Sex, shape=Sex)) + geom_point(size=3) + 
#  xlab("Average Summer Temperature (°C)") + ylab("Head Width (mm)") +
#  xlim(min(morph.data.land.use$Agriculture,  na.rm=T) - 0.1, max(morph.data.land.use$Agriculture, na.rm=T) + 0.1) +
#  ylim(min(morph.data.land.use$Head.Width.mm,  na.rm=T) - 0.1, max(morph.data.land.use$Head.Width.mm, na.rm=T) + 0.1) + 
#  geom_abline(intercept =agriculture.fixed[1], 
#              slope =agriculture.fixed[2], color = "steelblue", size =1.5, show.legend=TRUE)+
#  geom_abline(intercept =agriculture.fixed[1]  + agriculture.fixed[3],
#              slope =agriculture.fixed[2] + agriculture.fixed[6], color = "skyblue2", linetype = "dashed", size =1.5, show.legend=TRUE)+
#  scale_color_manual(name = "", labels= c("Female", "Male"), values= c("Steelblue", "Skyblue2")) +
#  scale_shape_manual(name = "", labels= c("Female", "Male"), values = c(16, 17)) +
#  scale_linetype_manual(name = "", labels= c("Female", "Male"), values = c(1, "dashed")) +
#  theme_bw(base_size = 20) +
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        #panel.border = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"),
#        axis.ticks.length=unit(-0.25, "cm"),
#        axis.text.y = element_text(margin = margin(0,0.5,0,0, unit = 'cm')),
#        axis.text.x = element_text(vjust = 5, margin = margin(0.80,0,0,0, unit = 'cm')), 
#        legend.position = c(0.7, .30),
#        legend.justification = c(-0.3, 1),
#        legend.text = element_text(size=20),
#        legend.title = element_text(size=20),
#        legend.background = element_rect(fill= alpha(0.01)))  +
#  labs(color='')
#dev.off()

