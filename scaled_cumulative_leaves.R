dat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014.csv', as.is=TRUE) #reading the input data

dat <-   dat[!duplicated((dat[,2:7])),]

for (i in 1: length(dat$leaves_new)) {                        # converting 0 to N and 1,2 to Y
  if(dat$leaves_new[i] == 0) dat$leaves_new[i] <- 'N'
  if(dat$leaves_new[i] == 1) dat$leaves_new[i] <- 'Y'
  if(dat$leaves_new[i] == 2) dat$leaves_new[i] <- 'Y'
}


mid <- function(curve,days){
  mid.5 <- 0
  for (i in 1:length(curve)){
    if (curve[i] < 50 ) mid.5 <- (days[i+1]-days[i])*((50-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 50) mid.5 <- days[i]
  }
  mid.5
}

slope <- function(curve,days){
  quant.75 <- 0
  quant.25 <- 0
  for (i in 1:length(curve)){
    if (curve[i] < 75 ) quant.75 <- (days[i+1]-days[i])*((75-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 75) quant.75 <- days[i]
    if (curve[i] < 25 ) quant.25 <- (days[i+1]-days[i])*((25-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 25) quant.25 <- days[i]
  }
  (quant.75 - quant.25)
}





flowerprop = function(flowering){                      #function to calculate flower proportion
  flowering <- flowering[!is.na(flowering)]            #Removing na if any
  total_no <- length(flowering)                        #total no of individuals
  tempf <- ifelse(flowering == 'Y',1,0)                #making a vector assigning 1 if flower or bud present and 0 if none
  flowering_no <- sum(tempf)                           #sum of vector gives number of individuals with flower or bud
  prop <- (flowering_no/total_no)                      #proportion calculated by dividing the two quantities
  prop
}


first_flower <- function(dat){
  first <- NA
  for( i in 1:length(dat$leaves_new)){
    if (dat$leaves_new[i] == 'Y'){
      first <- as.character(dat$datep[i])
      break    
    }  
  }
  first
}



species <- c("Erythroxylon monogynum","Flacourtia sepiaria","Lantana camara","Randia dumetorum","Wrightia tinctoria","Albizzia amara","Azadirachta indica","Strychnos nux-vomica","Tamarindus indicus","Acacia leucophloea","Pongamia pinnata","Delonix regia",'Ficus religiosa', 'Ficus benghalensis')

png(units="in", width=8.3, height=11.7, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/scaled cumulative leaf.png'))    

a <- layout(matrix(c(1:14,0),5,3))

par(mar = c(1,2,2,1)+0.1, mgp = c(1.5,0.5,0), oma = c(2,2,1,1)+0.1)

sp_st <- data.frame(matrix(NA, ncol = 2, nrow = length(species)))
rownames(sp_st) <- species
colnames(sp_st) <- c('synchrony', 'consistency')

for (s in 1:length(species)) {
  
  use.sp <- species[s]
  
  use.sp_dat <- subset(dat, species_name == use.sp)  # Creating a subset with just species being used
  
  
  datelist <- as.list(rep(0,length(levels(factor(dat$datep)))))
  
  names(datelist) <- as.Date(levels(factor(dat$datep)))
  
  for (k in 2:7) {  # k index for specifying year
    
    use.sp_dat_year <- subset(use.sp_dat, year == (2006 + k))         #subset of species data divided per year
    
    use.sp_prop_leaf <- with(use.sp_dat_year, tapply(X = leaves_new, INDEX = datep, FUN = flowerprop))
    
    start_date <- NA
    
    for (i in 1:length(use.sp_prop_leaf)){
      if(use.sp_prop_leaf[[i]] < 0.051) break
    }
    if (i < length(use.sp_prop_leaf)-4)  start_date <- names(use.sp_prop_leaf)[i]
    
    for (j in levels(factor(use.sp_dat_year$tag_id))) {                                                 # j index for individuals
      
      use.sp_dat_year_ind <- subset(use.sp_dat_year, tag_id == j & datep > as.Date(start_date) )     #subset yearly species data per individual
      
      if(is.na(start_date) == F & length(use.sp_dat_year_ind$leaves_new > 0)){
        
        if (is.na(first_flower(use.sp_dat_year_ind)) == FALSE)  datelist[first_flower(use.sp_dat_year_ind)] <- datelist[[first_flower(use.sp_dat_year_ind)]]  + 1
        
      }
      
    }
  }
  length(use.sp_dat_year_ind$leaves_new)
  
  datelist
  
  flowering_day <- data.frame(matrix(NA,nrow = 24 , ncol = 9))
  sample_days <- c('01-Jan', '16-Jan','01-Feb', '16-Feb','01-Mar', '16-Mar','01-Apr', '16-Apr','01-May', '16-May','01-Jun', '16-Jun','01-Jul', '16-Jul','01-Aug', '16-Aug','01-Sep', '16-Sep','01-Oct', '16-Oct','01-Nov', '16-Nov','01-Dec', '16-Dec')
  
  colnames(flowering_day) <- c('species','time','year_2008','year_2009','year_2010', 'year_2011', 'year_2012', 'year_2013', 'mean')
  
  flowering_day$time <- sample_days
  
  flowering_day$species <- rep(use.sp, length(sample_days))
  
  flowering_day$year_2008 <-  (100/max(cumsum(datelist[4:27])))*cumsum(datelist[4:27])
  flowering_day$year_2009 <-  (100/max(cumsum(datelist[28:51])))*cumsum(datelist[28:51])
  flowering_day$year_2010 <-  (100/max(cumsum(datelist[52:75])))*cumsum(datelist[52:75])
  flowering_day$year_2011 <-  (100/max(cumsum(datelist[76:99])))*cumsum(datelist[76:99])
  flowering_day$year_2012 <-  (100/max(cumsum(datelist[100:123])))*cumsum(datelist[100:123])
  flowering_day$year_2013 <-  (100/max(cumsum(datelist[124:147])))*cumsum(datelist[124:147])
  
  flowering_day$mean <- apply(flowering_day[,3:8],1,mean, na.rm = T)
  
  flowering_day
  
  plot(julian(as.Date(names(datelist[4:27])),as.Date(names(datelist))[4]),flowering_day$year_2008,ylim = c(0,100), type = 'l', xlab = NA, ylab = NA, main = use.sp,lwd = 2)
  par(new = TRUE)
  plot(julian(as.Date(names(datelist[28:51])),as.Date(names(datelist))[28]),flowering_day$year_2009,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'red', ylab = NA, xlab = NA,lwd = 2)
  par(new = TRUE)
  plot(julian(as.Date(names(datelist[52:75])),as.Date(names(datelist))[52]),flowering_day$year_2010,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'blue', ylab = NA, xlab = NA,lwd = 2)
  par(new = TRUE)
  plot(julian(as.Date(names(datelist[76:99])),as.Date(names(datelist))[76]),flowering_day$year_2011,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'orange', ylab = NA, xlab = NA,lwd = 2)
  par(new = TRUE)
  plot(julian(as.Date(names(datelist[100:123])),as.Date(names(datelist))[100]),flowering_day$year_2012,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'brown', ylab = NA, xlab = NA,lwd = 2)
  par(new = TRUE)
  plot(julian(as.Date(names(datelist[124:147])),as.Date(names(datelist))[124]),flowering_day$year_2013,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'dark green', ylab = NA, xlab = NA,lwd = 2)
  par(new = TRUE)
  plot(flowering_day$mean,ylim = c(0,100), type = 'l', xaxt = 'n', yaxt = 'n', col = 'black', ylab = NA, xlab = NA, lty = 2, lwd = 3)
  
  mid_leaf <- NA
  quant_leaf <- NA
  for ( i in 3:8 ){
    if(is.na(flowering_day[1,i]) == F) mid_leaf[i-2] <- mid(flowering_day[,i],julian(as.Date(names(datelist[4:27])),as.Date(names(datelist))[4]))
    if(is.na(flowering_day[1,i]) == F) quant_leaf[i-2] <- slope(flowering_day[,i],julian(as.Date(names(datelist[4:27])),as.Date(names(datelist))[4]))
  }  

  sp_st$synchrony[s] <- mean(quant_leaf, na.rm = T)
  sp_st$consistency[s] <- max(mid_leaf, na.rm = T) - min(mid_leaf, na.rm = T)

}
par(xpd = NA)

legend(10,-60,c('mean','2008','2009','2010','2011','2012','2013'),lty = c(2,1,1,1,1,1,1), lwd = c(2,2,2,2,2,2), col = c('black','black','red','blue','orange','brown','dark green') )
title(xlab = "Days",ylab = "Scaled cumulative leaf flush",outer = TRUE, line = 1, cex = 1.5)
dev.off()

par(mfrow = c(1,1), mar = c(7,4,2,2))

synco_sp_st <- sp_st[order(-sp_st$synchrony),]

x <- barplot(synco_sp_st$synchrony)

text(cex=1, x=x+.25, y=-1.25, rownames(synco_sp_st), xpd=TRUE, srt= 45, pos=2)

synco_sp_st <- sp_st[order(-sp_st$consistency),]

x <- barplot(synco_sp_st$consistency)

text(cex=1, x=x+.25, y=-1.25, rownames(synco_sp_st), xpd=TRUE, srt= 45, pos=2)
