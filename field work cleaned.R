dat <- read.csv('C:/Users/Abinand Reddy//Desktop/Thesis/NCF/R/field work.csv', as.is = TRUE)

dat <- dat[,1:8] #selceting only relevant columns

dat$date <- as.Date(dat$date, format = '%d-%m-%Y') #Changing date format

spec = c('randia','flacourtia') 

spec_name <- c('Randia dumetorum', 'Flacourtia sepiaria')

wat <- c(as.Date('2015-02-11'),as.Date('2015-02-13'))


#png(units="in", width=7, height=6, res=300, file = paste('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/R/Thesis plots/field work.png'))    


#### plotting proportion of mature leaves for both species on all days of observation

a <- layout(matrix(1:3,3,1)) # layout of graphs

par(mar = c(4,4,3,2)+0.1) # parameters for plots

for ( s in c(1,2)){ 

  dat_sp <- subset(dat, species == spec[s]) #subset with just one species (repeated twice for both species)

#### creating an empty matrix for each treatement for the proportion of mature leaves present for each individual to be filled in on each day of observation
  
  # COntrol treatement matrix
  dat_c <- data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) +1 , ncol = length(levels(factor(dat_sp$date)))))  
  colnames(dat_c) <- julian(as.Date(levels(factor(dat_sp$date))), wat[s]) # columns named as the day from the watering event
  rownames(dat_c) <- c(seq(1:(nrow(dat_c)-1)),'mean') #rows are individuals
  
  #15l treatement matrix
  dat_t1 <- data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) +1 , ncol = length(levels(factor(dat_sp$date)))))
  colnames(dat_t1) <- julian(as.Date(levels(factor(dat_sp$date))), wat[s])
  rownames(dat_t1) <- c(seq(1:(nrow(dat_t1)-1)),'mean')
  
  #30l treatement matrix
  dat_t2 <-  data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) + 1   , ncol = length(levels(factor(dat_sp$date)))))
  colnames(dat_t2) <- julian(as.Date(levels(factor(dat_sp$date))), wat[s])
  rownames(dat_t2) <- c(seq(1:(nrow(dat_t2)-1)),'mean')

  for (i in 1:length(levels(factor(dat_sp$tag)))){  #looping on each individual

    dat_sp_tri <-subset(dat_sp, tag == i) #susbetting triads of different treatments

    dat_sp_tri_c <- subset(dat_sp_tri, treatement == 'c') #subsetting control individual

    dat_sp_tri_c_mature  <- with(dat_sp_tri_c, tapply(mature, date, sum, na.rm = T)) #sum of all mature leaves for all 5 branches for each day of observation
      
    dat_c[i,] <- dat_sp_tri_c_mature/dat_sp_tri_c_mature[1] # leaves present on each day divided by no.of leaves before watering for proportiona and added to the created matrix 
  
    #The above 4 steps repeated for the other 2 treatments similarly
    
    dat_sp_tri_t1 <- subset(dat_sp_tri, treatement == 't1') 

    dat_sp_tri_t1_mature  <-  with(dat_sp_tri_t1, tapply(mature, date, sum,na.rm = T))
  
    dat_t1[i,] <- dat_sp_tri_t1_mature/dat_sp_tri_t1_mature[1]

    dat_sp_tri_t2 <- subset(dat_sp_tri, treatement == 't2')

    dat_sp_tri_t2_mature <-  with(dat_sp_tri_t2, tapply(mature, date, sum,na.rm = T))

    dat_t2[i,] <- dat_sp_tri_t2_mature/dat_sp_tri_t2_mature[1]
  }

  if (s == 2) dat_c <- dat_c[-12,] #removing one triad in flacourtia due to faulty field work
  
  dat_c[nrow(dat_c),] <- apply(dat_c[1:nrow(dat_c)-1,],2,mean,na.rm = T)
  dat_t1[nrow(dat_t1),] <- apply(dat_t1[1:nrow(dat_t1)-1,],2,mean,na.rm = T)
  dat_t2[nrow(dat_t2),] <- apply(dat_t2[1:nrow(dat_t2)-1,],2,mean,na.rm = T)

  boxplot(c(dat_c[-nrow(dat_c),],NA,dat_t1[-nrow(dat_t1),],NA, dat_t2[-nrow(dat_t2),]), main = spec_name[s], xlab = "Day from watering", ylab = 'proportion of mature leaf', ylim = c(0,1.4)) #boxplotting each treatment
  abline(v= c(1.5,10.5,19.5), lty = 3,col = 'dark blue') #line for day of watering
  abline(v = c(9,18)) #Line to seperate treatments
  abline(v = c(6.5, 15.5, 24.5), lty = 3, col = 'dark red') #Line for day of rain
}

###### Plotting fresh leaves for flacourtia, done similar to the above section except for fresh leaves and not mature leaves

dat_sp <- subset(dat, species == 'flacourtia')

dat_c <- data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) +1 , ncol = length(levels(factor(dat_sp$date)))))
colnames(dat_c) <- julian(as.Date(levels(factor(dat_sp$date))), wat[2])
rownames(dat_c) <- c(seq(1:(nrow(dat_c)-1)),'mean')

dat_t1 <- data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) +1 , ncol = length(levels(factor(dat_sp$date)))))
colnames(dat_t1) <- julian(as.Date(levels(factor(dat_sp$date))), wat[2])
rownames(dat_t1) <- c(seq(1:(nrow(dat_t1)-1)),'mean')

dat_t2 <-  data.frame(matrix(NA,nrow = length(levels(factor(dat_sp$tag))) + 1   , ncol = length(levels(factor(dat_sp$date)))))
colnames(dat_t2) <- julian(as.Date(levels(factor(dat_sp$date))), wat[2])
rownames(dat_t2) <- c(seq(1:(nrow(dat_t2)-1)),'mean')

for (i in 1:length(levels(factor(dat_sp$tag)))){
  
  dat_sp_tri <-subset(dat_sp, tag == i)
  
  dat_sp_tri_c <- subset(dat_sp_tri, treatement == 'c')
  
  dat_sp_tri_c_fresh  <- with(dat_sp_tri_c, tapply(fresh, date, sum, na.rm = T))
  
  dat_c[i,] <- dat_sp_tri_c_fresh
  
  dat_sp_tri_t1 <- subset(dat_sp_tri, treatement == 't1')
  
  dat_sp_tri_t1_fresh  <-  with(dat_sp_tri_t1, tapply(fresh, date, sum,na.rm = T))
  
  dat_t1[i,] <- dat_sp_tri_t1_fresh
  
  dat_sp_tri_t2 <- subset(dat_sp_tri, treatement == 't2')
  
  dat_sp_tri_t2_fresh <-  with(dat_sp_tri_t2, tapply(fresh, date, sum,na.rm = T))
  
  dat_t2[i,] <- dat_sp_tri_t2_fresh
}


dat_c[nrow(dat_c),] <- apply(dat_c[1:nrow(dat_c)-1,],2,mean,na.rm = T)
dat_t1[nrow(dat_t1),] <- apply(dat_t1[1:nrow(dat_t1)-1,],2,mean,na.rm = T)
dat_t2[nrow(dat_t2),] <- apply(dat_t2[1:nrow(dat_t2)-1,],2,mean,na.rm = T)

boxplot(c(dat_c[-nrow(dat_c),],NA,dat_t1[-nrow(dat_t1),],NA, dat_t2[-nrow(dat_t2),]), main = 'Flacourtia sepiaria', xlab = "Day from watering", ylab = 'Number of fresh leaves')
abline(v= c(1.5,10.5,19.5), lty = 3,col = 'dark blue')
abline(v = c(9,18))
abline(v = c(6.5, 15.5, 24.5), lty = 3, col = 'dark red')
  
#dev.off()