#-------------------------------------------------------------------------------
# Descriptive Stats
#-------------------------------------------------------------------------------
#>> Female
# Calculating the range of Body Sizes
max(morph.data.temp[morph.data.temp$Sex == "Female", "Head.Width.mm"])
min(morph.data.temp[morph.data.temp$Sex == "Female", "Head.Width.mm"])

# CV
mean.female <- mean(morph.data.temp[morph.data.temp$Sex == "Female", "Head.Width.mm"])
sd.female <- sd(morph.data.temp[morph.data.temp$Sex == "Female", "Head.Width.mm"])

sd.female/mean.female * 100



#>> Male
# Calculating the range of Body Sizes
max(morph.data.temp[morph.data.temp$Sex == "Male", "Head.Width.mm"])
min(morph.data.temp[morph.data.temp$Sex == "Male", "Head.Width.mm"])

# CV
mean.male <- mean(morph.data.temp[morph.data.temp$Sex == "Male", "Head.Width.mm"])
sd.male <- sd(morph.data.temp[morph.data.temp$Sex == "Male", "Head.Width.mm"])

sd.male/mean.male * 100
