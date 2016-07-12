library(CircStats)
library(circular)

dat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014.csv', as.is=TRUE) #reading the input data

dat <- dat[!duplicated((dat[,2:7])),]

mdat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/RV Weather Data/Manual station/rvs weather 2014.csv', as.is=TRUE) #reading the input data

mdat$Date <- as.Date(mdat$Date, format = '%d.%m.%Y')

mdat$Rainfall.mm <- as.numeric(mdat$Rainfall.mm)

mdat$month <- as.numeric(format(mdat$Date, '%m'))

mdat$day <- as.numeric(format(mdat$Date, '%d'))



for (i in 1: length(dat$fruit_brown)) {                        # converting 0 to N and 1,2 to Y
  
  if(dat$fruit_brown[i] == 'N') dat$fruit_brown[i] <- 0
  if(dat$fruit_brown[i] == 'Y') dat$fruit_brown[i] <- 1
  if(dat$fruit_brown[i] == 2) dat$fruit_brown[i] <- 1
}

for (i in 1: length(dat$fruit_brown)){
  ifelse(dat$day[i] < 16, dat$fortnight[i] <- (dat$month[i]*2)-1,dat$fortnight[i] <- (dat$month[i]*2))
  ifelse(mdat$day[i] < 16, mdat$fortnight[i] <- (mdat$month[i]*2)-1,mdat$fortnight[i] <- (mdat$month[i]*2))
  
}



species <- c("Acacia leucophloea", "Albizzia amara", "Azadirachta indica","Chomelia asiatica","Delonix regia", "Erythroxylon monogynum", "Flacourtia sepiaria", "Lantana camara", "Peltoforum pterocarpum", "Pongamia pinnata", "Randia dumetorum", "Santalum album", "Strychnos nux-vomica", "Tamarindus indicus" ,"Wrightia tinctoria")

png(units="in", width=8, height=8, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/fruit circanalysis.png'))    

a <- layout(matrix(1:16,4,4))

par(mar = c(0,0,0.2,0) + 0.5, xpd = T)

for (j in 1:length(species)){

  temp <- subset(dat, dat$species_name == species[j], select = c(fortnight,tag_id,fruit_brown,species_name ))
  temp$fruit_brown <- as.numeric(temp$fruit_brown)
  temp <- na.omit(temp) 
  rtemp <- with(temp, tapply(X = fruit_brown, INDEX = fortnight, FUN = sum))
  crtemp <- vector('double', length = 0)
  rtemp
  for (i in 1:length(rtemp)){
    rad <- i/length(rtemp)*2*pi
    r1 <- rep(rad,times = rtemp[i])
    crtemp <- c(crtemp,r1)
  }
    
  cavm <- circular(crtemp, type = 'angle', units = 'radians',rotation = 'clock')
  rose.diag(cavm, bins = 24, shrink = 0.7, xlim = c(-2,2), axes = F, col = 'grey', zero = c(rad(90)), prop = 2, tcl = .1, main= species[j])
  arrows.circular(circular(circ.mean(cavm)),zero = c(rad(90)),rotation = 'clock',length = .1, lwd = 2, circ.summary(cavm)[[3]])
  text(0,1.2, 'JAN'); text(1.3,0,'APR');text(0,-1.2,'JUL');text(-1.3,0,'OCT')
  
}




rtemp <- with(mdat, tapply(X = Rainfall.mm, INDEX = fortnight, FUN = sum, na.rm = T))

rtemp_swm <- rtemp
rtemp_nem <- rtemp
rtemp_swm[c(1:10,17:24)] <- 0
rtemp_nem[c(1:16,23,24)] <- 0

crtemp <- vector('double', length = 0)
crtemp_swm <- vector('double', length = 0)
crtemp_nem <- vector('double', length = 0)

for (i in 1:length(rtemp)){
  rad <- i/length(rtemp)*2*pi
  r1 <- rep(rad,times = rtemp[i])
  crtemp <- c(crtemp,r1)
  r1_swm <- rep(rad,times = rtemp_swm[i])
  crtemp_swm <- c(crtemp_swm,r1_swm)
  r1_nem <- rep(rad,times = rtemp_nem[i])
  crtemp_nem <- c(crtemp_nem,r1_nem)
}



cavm <- circular(crtemp, type = 'angle', units = 'radians',rotation = 'clock')
cavm_swm <- circular(crtemp_swm, type = 'angle', units = 'radians',rotation = 'clock')
cavm_nem <- circular(crtemp_nem, type = 'angle', units = 'radians',rotation = 'clock')

rose.diag(cavm, bins = 24, shrink = .7, xlim = c(-2,2), axes = F, col = 'grey', zero = c(rad(90)), prop = 2, tcl = .1, main= 'Rainfall')

arrows.circular(circular(circ.mean(cavm)),zero = c(rad(90)),rotation = 'clock', lwd = 2, circ.summary(cavm)[[3]], length = .1)
arrows.circular(circular(circ.mean(cavm_swm)),zero = c(rad(90)),rotation = 'clock',col = 'dark red', lwd = 2, circ.summary(cavm_swm)[[3]]*3542/10504*2, length = .1)
arrows.circular(circular(circ.mean(cavm_nem)),zero = c(rad(90)),rotation = 'clock',col = 'dark blue', lwd = 2, circ.summary(cavm_nem)[[3]]*4962/10504*2, length = .1)
text(0,1.2, 'JAN'); text(1.3,0,'APR');text(0,-1.2,'JUL');text(-1.3,0,'OCT')
dev.off()
