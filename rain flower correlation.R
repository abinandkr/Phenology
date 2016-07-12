dat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014.csv', as.is=TRUE) #reading the input data

dat <-   dat[!duplicated((dat[,2:7])),]

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

mid <- function(curve,days){
  mid.5 <- 0
  for (i in 1:length(curve)){
    if (curve[i] < 50 ) mid.5 <- (days[i+1]-days[i])*((50-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 50) mid.5 <- days[i]
  }
  mid.5
}

sync <- function(curve,days){
  quant.95 <- 0
  quant.05 <- 0
  for (i in 1:length(curve)){
    if (curve[i] < 95 ) quant.95 <- (days[i+1]-days[i])*((95-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 95) quant.95 <- days[i]
    if (curve[i] < 5 ) quant.05 <- (days[i+1]-days[i])*((5-curve[i])/(curve[i+1]-curve[i])) + days[i]
    if (curve[i] == 5) quant.05 <- days[i]
  }
  quant.95 - quant.05
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
  for( i in 1:length(dat$flower)){
    if (dat$flower[i] == 'Y'){
      first <- as.character(dat$datep[i])
      break    
    }  
  }
  first
}


species <- c("Erythroxylon monogynum","Flacourtia sepiaria","Lantana camara","Randia dumetorum","Wrightia tinctoria","Strychnos nux-vomica", 'Chomelia asiatica')

length(species)

par(mfrow = c(2,2))

for (s in 1:length(species)) {
  
  use.sp <- species[s]
  
  use.sp_dat <- subset(dat, species_name == use.sp)  # Creating a subset with just species being used
  
  
  datelist <- as.list(rep(0,length(levels(factor(dat$datep)))))
  
  names(datelist) <- as.Date(levels(factor(dat$datep)))
  
  for (k in 2:7) {  # k index for specifying year
    
    use.sp_dat_year <- subset(use.sp_dat, year == (2006 + k))         #subset of species data divided per year
    
    use.sp_prop_leaf <- with(use.sp_dat_year, tapply(X = flower, INDEX = datep, FUN = flowerprop))
    
    start_date <- NA
    
    for (i in 1:length(use.sp_prop_leaf)){
      if(use.sp_prop_leaf[[i]] < 0.051) break
    }
    if (i < length(use.sp_prop_leaf)-4)  start_date <- names(use.sp_prop_leaf)[i]
    
    for (j in levels(factor(use.sp_dat_year$tag_id))) {                                                 # j index for individuals
      
      use.sp_dat_year_ind <- subset(use.sp_dat_year, tag_id == j & datep > as.Date(start_date) )     #subset yearly species data per individual
      
      if(is.na(start_date) == F & length(use.sp_dat_year_ind$flower > 0)){
        
        if (is.na(first_flower(use.sp_dat_year_ind)) == FALSE)  datelist[first_flower(use.sp_dat_year_ind)] <- datelist[[first_flower(use.sp_dat_year_ind)]]  + 1
        
      }
      
    }
  }
  length(use.sp_dat_year_ind$flower)
  
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
  
  
  
  leaf_rain <- data.frame(matrix(NA,nrow = 6 , ncol = 2))
  colnames(leaf_rain) <- c('mid_leaf','first_rain')
  rownames(leaf_rain) <- c('2008','2009','2010','2011','2012','2013')
  leaf_rain
  
  
  
  for ( i in 3:8 ){
    if(is.na(flowering_day[1,i]) == F) leaf_rain$mid_leaf[i-2] <- mid(flowering_day[,i],julian(as.Date(names(datelist[4:27])),as.Date(names(datelist))[4]))
    temp <- subset(mdat, year == 2005 + i)
    for (k in 1:length(temp$Rainfall.mm)){
      if (temp$Rainfall.mm[k] > 10 ) break  
    }
    leaf_rain$first_rain[i-2] <- temp$Date[k] - temp$Date[1]
  }
  
  plot( leaf_rain$mid_leaf,leaf_rain$first_rain, xlab = 'Population median leaf flush', ylab = 'first rain', main = use.sp)  
  abline(lm(leaf_rain$first_rain ~ leaf_rain$mid_leaf ) , col = 'red')  
  
  print(c(use.sp,max(leaf_rain$mid_leaf, na.rm = T)-min(leaf_rain$mid_leaf, na.rm = T)))
  leaf_rain
}