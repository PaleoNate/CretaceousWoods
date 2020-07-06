##########################################################################
# This code generates the plot in Jud et al. 2018 Sci Adv. 4(9):eaar8568 #
##########################################################################

#Packages
require(ggplot2)
require(wesanderson)
require(FSA)

#This dataset is on GitHub
Kwoods<-read.csv(file=file.choose(), header=T, dec =".", na.strings = c('NA'))
str(Kwoods)

#subset angiosperms and gymnosperms
FMwoods<- Kwoods[Kwoods[, "Group"] == "Dicot",]
COwoods<- Kwoods[Kwoods[, "Group"] == "Conifer",]

####################
# Explore the data #
####################

#Histogram showing the distribution of sizes
p1 <- hist(~Diameter, data=FPwoods, xlab="diameter class (m)", breaks=seq(0,3,0.1), ylim=range(0,30), las=1)
p2 <- hist(~Diameter, data=COwoods, xlab="diameter class (m)", breaks=seq(0,3,0.1), ylim=range(0,30), las=1)
plot( p1, col=rgb(1,0,0,1/3), main="Distribution of sizes", ylim=range(0,30))
plot( p2, col=rgb(0,0,1,1/3), add=T)

#Alternative
hist(~Diameter,data=COwoods, xlab="diameter class (m)", breaks=seq(0,3,0.1), col='skyblue',border=F, main="Distribution of sizes")
hist(~Diameter, data=FPwoods, xlab="diameter class (m)", breaks=seq(0,3,0.1), col=scales::alpha('red',.5),border=F, add=T)

#Histogram showing the distribution of ages
p3 <- hist(~Age..Ma., data=FPwoods, breaks=seq(-145,-65,10), las=1)
p4 <- hist(~Age..Ma., data=COwoods, breaks=seq(-145,-65,10), las=1)
plot( p3, col=rgb(1,0,0,1/3), main="Distribution of ages", xlab="age (Ma)")
plot( p4, col=rgb(0,0,1,1/3), add=T)
legend("topleft", c("dicots", "conifers"), col=c(rgb(1,0,0,1/3), rgb(0,0,1,1/3)), lwd=5)

hist(~Age..Ma., data=COwoods, xlab="Age (Ma)", breaks=seq(-145,-65,10), col='skyblue',border=F, main="Distribution of ages")
hist(~Age..Ma., data=FPwoods, xlab="Age (Ma)", breaks=seq(-145,-65,10), col=scales::alpha('red',.5),border=F, add=T)

#Histogram showing the distribution of latitudes
p5 <- hist(~Latitude, data=FPwoods, breaks=seq(-90,90,10), las=1)
p6 <- hist(~Latitude, data=COwoods, breaks=seq(-90,90,10), las=1)
plot( p5, col=rgb(1,0,0,1/3), main="Distribution of sites", xlab="Latitude")
plot( p6, col=rgb(0,0,1,1/3), add=T)
legend("topright", c("dicots", "conifers"), col=c(rgb(1,0,0,1/3), rgb(0,0,1,1/3)), lwd=5)

hist(~Latitude, data=COwoods, xlab="Age (Ma)", breaks=seq(-90,90,10), col='skyblue',border=F, main="Distribution of ages")
hist(~Latitude, data=FPwoods, xlab="Age (Ma)", breaks=seq(-90,90,10), col=scales::alpha('red',.5),border=F, add=T)

##################################
# Plot of dicot wood size by age #
##################################

FPwoods$Region <- factor(FPwoods$Region, levels = c("Laramidia","Appalachia","Elsewhere"))
# Basic scatter plot
ggplot(FPwoods, aes(x=Age..Ma., y=Diameter)) + geom_point()
# Change the point size, and shape
ggplot(FPwoods, aes(x=Age..Ma., y=Diameter, shape=Region, color=Region)) +
  ###geom_jitter(size=6, shape=20, width = 1, height = 0)+
  geom_point(size=6, shape=20) +
  scale_color_manual(values = c("#3A9CBC", "#A30119", "#DD7208"), name="Region") +
  scale_x_continuous("Age (Ma)", limits=c(-125, -65), breaks=seq(-120,-65,10)) +
  scale_y_continuous("Diameter (m)", limits=c(0, 2.2)) +
  theme_bw() +
  theme(text = element_text(size=18)) +
  theme(plot.margin = unit(c(3, 1, 1, 1), "cm")) +
  theme(legend.position="top")

################
#show all woods#
################

ggplot(Kwoods, aes(x=Age..Ma., y=Diameter, shape=Region, color=Category)) +
  ###geom_jitter(size=6, shape=20, width = 1, height = 0)+
  geom_point(size=6, shape=20) +
  scale_color_manual(values = c(rgb(1,0,0,1/3), rgb(0,0,1,1/3)), name="Category") +
  scale_x_continuous("Age (Ma)", limits=c(-125, -65), breaks=seq(-120,-65,10)) +
  scale_y_continuous("Diameter (m)", limits=c(0, 2.2)) +
  theme_bw() +
  theme(text = element_text(size=18)) +
  theme(plot.margin = unit(c(3, 1, 1, 1), "cm")) +
  theme(legend.position="top")

###############################################################################################
# This code generates the plots in Jud et al. 2017 IAWA Journal doi:10.1163/22941932-20170164 #
###############################################################################################

library(ggplot2)
library(maps)
library(rworldmap)
library(grid)
library(gridExtra)
library(patchwork)

#First the map
p1 <- ggplot()+ 
  geom_polygon(data=map_data(map='world',region=c("Canada", "USA", "Mexico","Guatemala","Belize","Honduras","El Salvador","Nicaragua")), 
                              aes(x=Longitude, y=Latitude, group=group),
                              fill="white")+
  coord_fixed(ratio=1)+
  xlim(-170,-50)+
  ylim(10,85)+
  geom_point(aes(x=Longitude, y=Latitude), data=FPwoods)+
  geom_point(aes(x=Longitude, y=Latitude), data=COwoods, pch=1, col="gray20")

p1

#then the latitudinal gradient in size

p2 <- ggplot(FPwoods, aes(Diameter, Latitude))+
  geom_point()+
  xlim(0,3)+
  ylim(10,85)+
  theme_bw()+
  geom_point(aes(x=Diameter, y=Latitude), data=COwoods, pch=1, col="gray20")

p2

#Plot them side by side with equivalent latitude!
p1 + p2

################################################
#Jud et al 2017 code modified for South America#
################################################

p3 <- ggplot() + geom_polygon(data=map_data(map='world',region=c("Costa Rica", "Panama", "Colombia", "Ecuador", "Peru","Chile","Argentina", "Antarctica",
                                                                 "Uruguay", "Paraguay","Bolivia","Brazil", "Suriname","French Guiana","Guyana","Venezuela")), 
                              aes(x=long, y=lat, group=group),
                              fill="gray80")+
  ylim(-75,15)+
  xlim(-130,-10)+
  theme_minimal()+
  theme(legend.position='none', 
        panel.grid.major.x= element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour='white'))+
  labs(x='Longitude', y='Latitude', cex = 5)+
  geom_point(aes(x=Longitude, y=Latitude), data=COwoods, pch=1, col="gray30")+
  geom_point(aes(x=Longitude, y=Latitude), data=FPwoods)+
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        plot.background = element_blank())

p3
  
p4 <- ggplot(FPwoods, aes(Diameter, Latitude))+
  geom_point()+
  xlim(0,3)+
  ylim(-75,15)+
  theme_bw()+
  geom_point(aes(x=Diameter, y=Latitude), data=COwoods, pch=1, col="gray20")

p4

#side by side
p3 + p4

#####
#End#
#####