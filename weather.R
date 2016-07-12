mdat <- read.csv('C:/Users/Nandu/Desktop/Thesis/NCF/R/Phenology Data/Data/RV Weather Data/Manual station/rvs weather 2014.csv', as.is=TRUE) #reading the input data

mdat$Date <- as.Date(mdat$Date, format = '%d.%m.%Y')

mdat$Rainfall.mm <- as.numeric(mdat$Rainfall.mm)

mdat$year <- as.numeric(format(mdat$Date, '%Y'))

mdat$Max.temp <- as.numeric(mdat$Max.temp)

mdat$Min.temp <- as.numeric(mdat$Min.temp)

mdat$month <- as.numeric(format(mdat$Date, '%m'))


for (i in 1:length(mdat$Rainfall.mm)){
  if (is.na(mdat$Rainfall.mm[i]) == TRUE) mdat$Rainfall.mm[i] <- 0 
}
head(mdat)
maxt_mean <- with(mdat, tapply(Max.temp, month, mean, na.rm = T))
mint_mean <- with(mdat, tapply(Min.temp, month, mean, na.rm = T))


maxt <- data.frame(matrix(NA,nrow = length(levels(factor(mdat$year))),ncol = 12))
rownames(maxt) <- levels(factor(mdat$year))
colnames(maxt) <- month.abb

mint <- data.frame(matrix(NA,nrow = length(levels(factor(mdat$year))),ncol = 12))
rownames(mint) <- levels(factor(mdat$year))
colnames(mint) <- month.abb

for (i in 1:length(levels(factor(mdat$year)))){
  mdat_year <- subset(mdat, year == levels(factor(mdat$year))[i])
  maxt[i,] <- with(mdat_year, tapply(Max.temp, month, mean, na.rm = T))
  mint[i,] <- with(mdat_year, tapply(Min.temp, month, mean, na.rm = T))
  rain[i,] <- with(mdat_year, tapply(Rainfall.mm, month, sum, na.rm = T))
}

png(units="in", width=7, height= 4, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/temperature.png'))    

par(oma = c(0,0,0,0), mar = c(5,4,1,1))

plot(maxt_mean, type = 'b', ylim = c(0,45), xlim = c(1,12), col = 'dark red',lwd = 3, xlab = 'Months', ylab = 'Temperature (C)', xaxt= 'nan')

par(new = T)

plot(mint_mean,type = 'b', ylim = c(0,45), xlab = NA, ylab = NA, xaxt= 'nan', col = 'dark blue', lwd = 3)

par(new = T)

boxplot(maxt, outline = F, ylim = (c(0,45)), xlim = c(1,12), boxwex = 0.0001, staplewex = 2000, names = month.abb)

par(new = T)

boxplot(mint, outline = F, ylim = (c(0,45)), xlim = c(1,12), boxwex = 0.0001, staplewex = 2000, xaxt = 'nan')

legend(9,45, c('Max temp', 'Min temp'), lty = c(1,1),lwd = c(3,3), col = c('dark red','dark blue'), bty = 'n')

dev.off()

rain  <- data.frame(matrix(NA, ncol = 12, nrow = length(levels(factor(mdat$year)))))

colnames(rain) <- month.abb
rownames(rain) <- levels(factor(mdat$year))
for (i in 1:length(levels(factor(mdat$year)))){
  mdat_year <- subset(mdat, year == levels(factor(mdat$year))[i])
  rain[i,] <- with(mdat_year, tapply(Rainfall.mm, month, sum, na.rm = T))
}

png(units="in", width=7, height= 4, res=300, file = paste('C:/Users/Nandu/Desktop/Thesis/NCF/R/Thesis plots/rain.png'))    

par(oma = c(0,0,0,0), mar = c(5,4,1,1), xpd = NA)


barplot(apply(rain,2,mean), ylim = c(-12,312), xlim = c(0.1,14.5), ylab = 'Rainfall (mm)', xlab = 'Months')

par(new = T)

boxplot(rain , outline = T, boxwex = 0.0001, staplewex = 2000, ylim = c(0,300) )

dev.off()

yearly_rain <- with(mdat, tapply(Rainfall.mm, year, sum, na.rm = T))/10

par(mar = c(4,4,2,2))

plot(names(yearly_rain),yearly_rain, type = 'b', col = 'red', lwd = 3, ylim = c(0,130), xlab = 'Year', ylab = 'Rainfall (cm)')
