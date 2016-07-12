dat <- read.csv('C:/Users/Abinand Reddy//Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014t.csv', as.is=TRUE) #reading the input data

dat$obs_date <- as.Date(dat$obs_date, '%d-%m-%Y')

dat$datep <- as.Date(dat$datep, '%d-%m-%Y')

head(dat)

mdat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/RV Weather Data/Manual station/rvs weather 2014.csv', as.is=TRUE) #reading the input data

mdat$Date <- as.Date(mdat$Date, format = '%d.%m.%Y')

mdat <- subset(mdat, Date < as.Date('2014-01-01') & Date > as.Date('2007-09-30') )

mdat$Rainfall.mm <- as.numeric(mdat$Rainfall.mm)

mdat$month <- as.numeric(format(mdat$Date, '%m'))

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
  if(dat$species_id[i] == 6 |dat$species_id[i] == 7 |dat$species_id[i] == 18) dat$flower[i] <- 'N' 
}

for (i in 1: length(dat$leaves_new)) {                    # converting 0 to N and 1,2 to Y
  if(dat$leaves_new[i] == 0) dat$leaves_new[i] <- 'N'
  if(dat$leaves_new[i] == 1) dat$leaves_new[i] <- 'Y'
  if(dat$leaves_new[i] == 2) dat$leaves_new[i] <- 'Y'
}

dat$leaves <- dat$leaves_new

for (i in 1: length(dat$leaves)) {                    # converting 0 to N and 1,2 to Y
  if(dat$leaves_old[i] == 'Y') dat$leaves[i] <- 'Y'
  if(dat$leaves_old[i] == 1) dat$leaves[i] <- 'Y'
  if(dat$leaves_old[i] == 2) dat$leaves[i] <- 'Y'
}

for (i in 1:length(dat$leaves)){
  ifelse(dat$leaves[i] == 'Y',dat$leafless[i] <- 'N', dat$leafless[i] <- 'Y') 
}


for (i in 1: length(dat$fruit_green)) {                    # converting 0 to N and 1,2 to Y
  if(dat$fruit_green[i] == 0) dat$fruit_green[i] <- 'N'
  if(dat$fruit_green[i] == 1) dat$fruit_green[i] <- 'Y'
  if(dat$fruit_green[i] == 2) dat$fruit_green[i] <- 'Y'
}


head(dat)
flowerprop = function(flowering){                      #function to calculate flower proportion
  flowering <- flowering[!is.na(flowering)]            #Removing na if any
  total_no <- length(flowering)                        #total no of individuals
  tempf <- ifelse(flowering == 'Y',1,0)                #making a vector assigning 1 if flower or bud present and 0 if none
  flowering_no <- sum(tempf)                           #sum of vector gives number of individuals with flower or bud
  prop <- (flowering_no/total_no)                      #proportion calculated by dividing the two quantities
  prop
}



species <- levels(factor(dat$species_name))

png(units="in", width=16.6, height=24, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/proportionst.png'))    

par(oma = c(5,5,2,2)+ 0.1,mar = c(0,5,0,0)+0.1)

t <- c(1:4,0,5:8,0,9:12,0,13:16,0,17:20,0,21:24,0,25:28,0,29:32,0,33:36,37:40,0,41:44,0,45:48,0,49:52,0,53:56,0,57:60,0,61:64,0,65:68,0,69:72)

length(t)

a <- layout(matrix(t,44,2))

layout.show(a)

for(i in 1:length(species)){   # J index for the 5 different png files
  
  if (i > length(species)/2) par(mar = c(0,0.5,0,1)+ 0.1)
  
  use.sp <- species[i]     # assigning which species to use
  
  use.sp_dat <- subset(dat, species_name == use.sp)      #making a subset of the data of individual species
  
  use.sp_prop_flower <- with(use.sp_dat, tapply(X = flower, INDEX = datep, FUN = flowerprop))    # Using tapply to calculate how proportions of flowering plants on a date of observation
    
  use.sp_prop_leaf <- with(use.sp_dat, tapply(X = leaves_new, INDEX = datep, FUN = flowerprop))
    
  use.sp_prop_deciduous <- with(use.sp_dat, tapply(X = leafless, INDEX = datep, FUN = flowerprop))
    
  use.sp_prop_fruit_green <- with(use.sp_dat, tapply(X = fruit_green, INDEX = datep, FUN = flowerprop))
      
  plot(as.Date(names(use.sp_prop_deciduous)), use.sp_prop_deciduous, ylab = 'Deciduous', xaxt = 'nan', xlab = 'Time', ylim = c(-0.1,1.1), type = 'l', lwd = 2, xlim = c(as.Date('2008-03-01'),as.Date('2015-02-01')), yaxt = 'nan')
  
  mtext(use.sp,3,cex = 1.5)
  
  if (i < 1 + length(species)/2) axis(side = 2, at = c(0,0.5,1))
  
  abline(v = c(as.Date('2008-01-01'),as.Date('2009-01-01'),as.Date('2010-01-01'),as.Date('2011-01-01'),as.Date('2012-01-01'),as.Date('2013-01-01'),as.Date('2014-01-01'),as.Date('2015-01-01')), lty =3 ) # Reference line at beginning of every year
  
  plot(as.Date(names(use.sp_prop_leaf)), use.sp_prop_leaf, ylab = 'leaf flush', xlab = 'Time', xaxt = 'nan', yaxt = 'nan', ylim = c(-0.1,1.1), xlim = c(as.Date('2008-03-01'),as.Date('2015-02-01')), type = 'l', lwd = 2)    #Plotting proportion vs the date it was calculated for
  
  if (i > length(species)/2) axis(side = 4, at = c(0,0.5,1))
  
  abline(v = c(as.Date('2008-01-01'),as.Date('2009-01-01'),as.Date('2010-01-01'),as.Date('2011-01-01'),as.Date('2012-01-01'),as.Date('2013-01-01'),as.Date('2014-01-01'),as.Date('2015-01-01')), lty =3 ) # Reference line at beginning of every year  

  plot(as.Date(names(use.sp_prop_flower)), use.sp_prop_flower, ylab = 'Flowering', xaxt = 'nan',xlab = 'Time', ylim = c(-0.1,1.1), type = 'l', lwd = 2, yaxt = 'nan', xlim = c(as.Date('2008-03-01'),as.Date('2015-02-01')))    #Plotting proportion vs the date it was calculated for
  
  if (i < 1 + length(species)/2) axis(side = 2, at = c(0,0.5,1))
  
  abline(v = c(as.Date('2008-01-01'),as.Date('2009-01-01'),as.Date('2010-01-01'),as.Date('2011-01-01'),as.Date('2012-01-01'),as.Date('2013-01-01'),as.Date('2014-01-01'),as.Date('2015-01-01')), lty =3 ) # Reference line at beginning of every year  

  plot(as.Date(names(use.sp_prop_fruit_green)), use.sp_prop_fruit_green,ylab = 'Fruiting',xlab = 'Time', yaxt = 'nan' , ylim = c(-0.1,1.1), xlim = c(as.Date('2008-03-01'),as.Date('2015-02-01')), type = 'l', lwd = 2, main = )    #Plotting proportion vs the date it was calculated for
  
  if (i >  length(species)/2) axis(side = 4, at = c(0,0.5,1))
  
  abline(v = c(as.Date('2008-01-01'),as.Date('2009-01-01'),as.Date('2010-01-01'),as.Date('2011-01-01'),as.Date('2012-01-01'),as.Date('2013-01-01'),as.Date('2014-01-01'),as.Date('2015-01-01')), lty =3 ) # Reference line at beginning of every year
}
title(xlab = "Years",ylab = "Proportions",outer = TRUE, line = 3, cex.lab = 2)
dev.off()

head(dat)
