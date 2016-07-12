dat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014.csv', as.is=TRUE) #reading the input data

dat <- dat[!duplicated((dat[,2:7])),]

mdat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/RV Weather Data/Manual station/rvs weather 2014.csv', as.is=TRUE) #reading the input data

mdat$Date <- as.Date(mdat$Date, format = '%d.%m.%Y')

mdat <- subset(mdat, Date < as.Date('2014-01-01') & Date > as.Date('2007-09-30') )

mdat$Rainfall.mm <- as.numeric(mdat$Rainfall.mm)

mdat$year <- as.numeric(format(mdat$Date, '%Y'))

for (j in 1:length(mdat$Date)){
  ifelse(as.numeric(format(mdat$Date[j],'%d')) < 16, mdat$datep[j] <- paste(format(mdat$Date[j], '%Y-%m'),'01', sep = '-'),mdat$datep[j] <- paste(format(mdat$Date[j], '%Y-%m'),'16', sep = '-'))  
}

for (i in 1:length(mdat$Rainfall.mm)){
  if (is.na(mdat$Rainfall.mm[i]) == TRUE) mdat$Rainfall.mm[i] <- 0 
}


for (i in 1: length(dat$flower_bud)) {                    # converting 0 to N and 1,2 to Y
  if(dat$flower_bud[i] == 0) dat$flower_bud[i] <- 'N'
  if(dat$flower_bud[i] == 1) dat$flower_bud[i] <- 'Y'
  if(dat$flower_bud[i] == 2) dat$flower_bud[i] <- 'Y'
}

dat$flower <- dat$flower_bud

for (i in 1: length(dat$flower_open)) {                    # converting 0 to N and 1,2 to Y
  if(dat$flower_open[i] == 'Y') dat$flower[i] <- 'Y'
  if(dat$flower_open[i] == 1) dat$flower[i] <- 'Y'
  if(dat$flower_open[i] == 2) dat$flower[i] <- 'Y'
}

for (i in 1: length(dat$leaves_old)) {                    # converting 0 to N and 1,2 to Y
  if(dat$leaves_old[i] == 0) dat$leaves_old[i] <- 'N'
  if(dat$leaves_old[i] == 1) dat$leaves_old[i] <- 'Y'
  if(dat$leaves_old[i] == 2) dat$leaves_old[i] <- 'Y'
}


flowerprop = function(flowering){                      #function to calculate flower proportion
  flowering <- flowering[!is.na(flowering)]            #Removing na if any
  total_no <- length(flowering)                        #total no of individuals
  tempf <- ifelse(flowering == 'Y',1,0)                #making a vector assigning 1 if flower or bud present and 0 if none
  flowering_no <- sum(tempf)                           #sum of vector gives number of individuals with flower or bud
  prop <- (flowering_no/total_no)                      #proportion calculated by dividing the two quantities
  prop
}


species <- c("Acacia leucophloea", "Albizzia amara", "Azadirachta indica","Chomelia asiatica","Delonix regia", "Erythroxylon monogynum", "Flacourtia sepiaria", "Lantana camara", "Peltoforum pterocarpum", "Pongamia pinnata", "Randia dumetorum", "Santalum album", "Strychnos nux-vomica", "Tamarindus indicus" ,"Wrightia tinctoria")

png(units="in", width=9, height=5, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/total rain leaf.png'))    

a <- layout(matrix((1:18),3,5, byrow = T))
layout.show(a)

par(oma = c(2,2,1,0), mar = c(2,2,1,1), mgp = c(0,0.5,0), xpd = F )


for(i in 1:length(species)){   # J index for the 5 different png files
  
  use.sp <- species[i]     # assigning which species to use
  
  use.sp_dat <- subset(dat, species_name == use.sp)      #making a subset of the data of individual species

  total <- data.frame(matrix(NA,ncol = 3, nrow = 6))
  
  colnames(total) <- c('rain','leaves','flower')
  
  for (j in 1:6){
    use.sp_dat_year <- subset(use.sp_dat, year == 2007 + j)
    
    mdat_year <- subset(mdat, year == 2007 + j)
    
    use.sp_prop_flower <- with(use.sp_dat_year, tapply(X = flower, INDEX = datep, FUN = flowerprop))    # Using tapply to calculate how proportions of flowering plants on a date of observation
  
    use.sp_prop_leaf <- with(use.sp_dat_year, tapply(X = leaves_old, INDEX = datep, FUN = flowerprop))
  
    total$rain[j] <- sum(mdat_year$Rainfall.mm, na.rm = T)/10
    
    total$leaves[j] <- sum(use.sp_prop_leaf, na.rm = T)
    
    total$flower[j] <- sum(use.sp_prop_flower, na.rm = T)
  }
  plot(total$rain, total$leaves, main = use.sp, pch = 21, bg = 'dark blue', cex.main = .8, xlab = NA, ylab = NA, cex.axis = .7)
  abline(lm(total$leaves~total$rain), col = 'red')

 # plot(total$rain, total$flower, main = use.sp, pch = 21, bg = 'dark blue', cex.main = .8, xlab = NA, ylab = NA, cex.axis = .7)
  #abline(lm(total$flower~total$rain), col = 'red')
}
title(xlab = "Rainfall (cm)",ylab = "Vegetative effort",outer = TRUE, line = 1, cex = 1.5)
dev.off()
