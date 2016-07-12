dat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/phenology_june_2014.csv', as.is=TRUE) #reading the input data

dat <-   dat[!duplicated((dat[,2:7])),]


for (i in 1: length(dat$leaves_new)) {                    # converting 0 to N and 1,2 to Y
  if(dat$leaves_new[i] == 0) dat$leaves_new[i] <- 'N'
  if(dat$leaves_new[i] == 1) dat$leaves_new[i] <- 'Y'
  if(dat$leaves_new[i] == 2) dat$leaves_new[i] <- 'Y'
}



flowerprop = function(flowering){                      #function to calculate flower proportion
  flowering <- flowering[!is.na(flowering)]            #Removing na if any
  total_no <- length(flowering)                        #total no of individuals
  tempf <- ifelse(flowering == 'Y',1,0)                #making a vector assigning 1 if flower or bud present and 0 if none
  flowering_no <- sum(tempf)                           #sum of vector gives number of individuals with flower or bud
  prop <- (flowering_no/total_no)                      #proportion calculated by dividing the two quantities
  prop
}

species <- c("Erythroxylon monogynum","Flacourtia sepiaria","Lantana camara","Randia dumetorum","Wrightia tinctoria","Albizzia amara","Azadirachta indica","Strychnos nux-vomica","Tamarindus indicus","Acacia leucophloea","Pongamia pinnata","Delonix regia",'Ficus religiosa', 'Ficus benghalensis')

png(units="in", width=8.3, height=11.7, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/leaf rank.png'))    

a <- layout(matrix(c(1:14,0),5,3))

par(mar = c(1,2,2,1)+0.1, mgp = c(1.5,0.5,0), oma = c(2,2,1,1)+0.1, xpd = F)

for (s in 1:length(species)) {
  
  use.sp <- species[s]
  
  use.sp_dat <- subset(dat, species_name == use.sp)  # Creating a subset with just species being used
  
  flowering_rank <- data.frame(matrix(nrow=length(levels(factor(use.sp_dat$tag_id))),ncol=1))  #Creating a new data frame of individuals along rows and years along columns
  
  flowering_rank <- data.frame(levels(factor(use.sp_dat$tag_id))) # first column assigned the individual tag ids
  
  names(flowering_rank) <- paste('tag_id') #column named tag_id
  
  for (k in 2:7) {  # k index for specifying year
    
    use.sp_dat_year <- subset(use.sp_dat, year == (2006 + k))         #subset of species data divided per year
    
    use.sp_prop_leaf <- with(use.sp_dat_year, tapply(X = leaves_new, INDEX = datep, FUN = flowerprop))
    
    start_date <- NA
    
    for (i in 1:length(use.sp_prop_leaf)){
      if(use.sp_prop_leaf[[i]] < 0.051) break
    }
    if (i < length(use.sp_prop_leaf)-4)  start_date <- names(use.sp_prop_leaf)[i]
    
    flowering_rank[paste('year',(2006 + k), sep = '_')] <- NA         #New column with name of year
    
    print(start_date)
    
    for (j in 1:length(levels(factor(use.sp_dat$tag_id)))) {                                                 # j index for individuals
      
      ind <- levels(factor(use.sp_dat$tag_id))[j]
      
      use.sp_dat_year_ind <- subset(use.sp_dat_year, tag_id == j & datep > as.Date(start_date) )     #subset yearly species data per individual
      
      if (is.na(use.sp_dat_year_ind$leaves_new[1]) == TRUE){           #Assigning NA to years in which certain species have not been recorded
        
        flowering_rank[,k][j] <- NA
        
      } else { 
        for (i in 1:length(use.sp_dat_year_ind$datep)){                 # i index for the number of observations in a year      
          if (use.sp_dat_year_ind$leaves_new[i] == 'Y'){
            flowering_rank[,k][j] <- use.sp_dat_year_ind$datep[i]  #date of first flowering assigned to the new data frame output is as number of days from origin
            break
          }
        } 
      }
    }
  }
  
  flowering_rank
  
  flowering_rank <- flowering_rank[rowSums(is.na(flowering_rank)) < 3, ]  #eliminate rows with more than one NA
  
  for (k in 2:6){
    flowering_rank[,k] <- rank(flowering_rank[,k], na.last = 'keep')        # The day from origin ranked to give ranks
  }
  
  
  
  iter <- 10000   #number of iterations
  
  sim <- matrix(NA,nrow=length(flowering_rank$tag_id), ncol = iter) #matrix with medians of each iteration
  
  for (k in 1:iter){
    simdat <- matrix(NA,nrow = length(flowering_rank$tag_id),ncol = 5) #matrix for assigning ranks for 5 years
    for (i in 1:5){
      simdat[,i] <- sample(flowering_rank[,(i+1)])           #assigning random ranks for 5 years
    }
    sim_median <- apply(simdat,1,median, na.rm = TRUE)  #calculating the medians of the actual data
    
    sim_median <- sort(sim_median)   #ordering the medians in increasing order
    
    sim[,k] <- sim_median
    
    #plot(1:length(sim_median),sim_median, ylim = c(0,(length(flowering_rank$tag_id)+4)), col = grey(.5, alpha = .1), type = 'l', xaxt = 'n',yaxt = 'n', ylab = NA, xlab = NA)  #plotting the real medians
    #par(new = TRUE)
  }
  
  for (i in 1:length(flowering_rank$tag_id)){
    sim[i,] <- sort(sim[i,])
  }
  
  real_median <- apply(flowering_rank[,2:6],1,median, na.rm = TRUE)  #calculating the medians of the actual data
  
  real_median <- sort(real_median)   #ordering the medians in increasing order
  
  
  
  
  plot(1:length(real_median), sim[,5000],ylim = c(0,(length(flowering_rank$tag_id)+4)),  type = 'l', lwd = 3, xlab = NA, ylab = NA)  #plotting the real medians 
  lines(1:length(real_median),sim[,9500],ylim = c(0,(length(flowering_rank$tag_id)+4)), lty = 2, lwd = 2 )
  lines(1:length(real_median),sim[,500],ylim = c(0,(length(flowering_rank$tag_id)+4)), lty = 2, lwd = 2 )
  lines(1:length(real_median), real_median,ylim = c(0,(length(flowering_rank$tag_id)+4)), col = 'red', lwd = 3)
  
  abline(h = length(flowering_rank$tag_id))
  text(length(flowering_rank$tag_id)/2,length(flowering_rank$tag_id)+3, use.sp, cex = 1.5)
}
par(xpd = NA)

legend(0,-8,c('Mean Simulated Medians', '2.5 & 97.5 quantile simulated median', 'Observed Medians'),lty = c(1,2,1), lwd = c(2,2,2), col = c('black','black','red'))

title(xlab = "Individuals",ylab = "Median ranks",outer = TRUE, line = 1, cex = 1.5)
dev.off()