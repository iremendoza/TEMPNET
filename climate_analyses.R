####This file contains preliminary climate analyses of compiled datasets from Doñana (S Spain) and Zackenberg (Greenland, Denmark)

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

####STANDARDIZATION OF DATASETS####
zackenberg <- read.delim(file = "meteo_zackenberg_1995-2016.txt")
colnames(zackenberg)[3] <- "temp"
zackenberg$date <- as.Date(zackenberg$Date, format = "%Y-%m-%d")
zackenberg2 <- zackenberg %>% group_by(date)  %>% 
 summarize(tmax = max(temp, na.rm = T), tmin =  min(temp,  na.rm = T))
zackenberg2$site <- "Zackenberg"

donana <- read.delim(file = "meteo_diaria_doñana_1975-2017.txt")
colnames(donana) <- c("Date", "prec", "tmax", "tmin", "site")
donana$date <- as.Date(donana$Date, format = "%d/%m/%Y")
donana2 <- data.frame(date = donana$date, tmax = donana$tmax, tmin = donana$tmin, site = donana$site)

climate <- rbind(donana2, zackenberg2)
climate$year <- lubridate::year(climate$date)
climate$month <- lubridate::month(climate$date)
climate$day <- lubridate::mday(climate$date)


climate2 <- climate %>% group_by(year, month, site)  %>% 
  summarize(tmax = mean(tmax, na.rm = T), tmin =  mean(tmin,  na.rm = T))

climate3<- climate %>% group_by (site, month)  %>% 
  summarize(tmax = mean(tmax, na.rm = T), tmin =  mean(tmin,  na.rm = T))

####FUNCTIONS####
#this function calculates the mean deviates for each value of percentage 
# input must be the result of the function percacti (for instance, ant=percacti(antese))
monthmean = function(dataset = climate)
  
{
   months=c(1:12)
  
  dataset2 <- dataset %>% group_by(site, month)  %>% 
    summarize(tmaxmean = mean(tmax, na.rm = T), tminmean =  mean(tmin,  na.rm = T),
              tmaxsd = sd(tmax, na.rm = T), tminsd = sd(tmin, na.rm = T))
  

  for (i in 1:nrow(dataset) )
    
  {
    dataset$devtmax[i]= dataset$tmax[i] - dataset2$tmaxmean[dataset2$site == dataset$site[i] & dataset2$month == dataset$month[i] ]
    dataset$devtmin[i]= dataset$tmin[i] - dataset2$tminmean[dataset2$site == dataset$site[i] & dataset2$month == dataset$month[i] ]
    
    #dataset$SND[i]=(dataset[i,vr]-mperc$x[mvar$month==dataset$mon[i]])/mvar$x[mvar$month==dataset$mon[i]]
  }
  
  return(dataset)   
  
}


anomalies <- monthmean(dataset = climate)
anomalies2 <- anomalies %>% group_by(year, month, site)  %>% 
  summarize(tmax = mean(devtmax, na.rm = T), tmin =  mean(devtmin,  na.rm = T))
anomalies2$date <- as.Date(paste(1, anomalies2$month, anomalies2$year, sep="-"), format = "%d-%m-%Y")
write.table(anomalies2, file = "anomalies.txt", sep = "\t", row.names = F)

####REPRESENTATION OF TEMPERATURE TRENDS####
anom <- read.delim(file = "anomalies.txt")
anom$date <- as.Date(anom$date, format = "%Y-%m-%d")
ebd <- anom %>% group_by(site) %>% 
  filter(site == "Donana")
temp.ebd <- zoo(ebd$tmax, ebd$date)

zkb <- anom %>% group_by(site) %>% 
  filter(site == "Zackenberg")
temp.zkb <- zoo(zkb$tmax, zkb$date)


#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.ebd, 5,fill = list(NA, NA, NA, NULL, NA, NA, NA))
ebd$movaver = coredata(m.av)

m.avz<-rollmean(temp.zkb, 5,fill = list(NA, NA, NA, NULL, NA, NA, NA))
zkb$movaver = coredata(m.avz)

combined <- rbind(ebd, zkb)

plot(anomalies2$tmax[anomalies2$site == "Donana"], type ="l", col = "red", axes = F, xlab = "date", ylab = "temperature anomalies")
lines(anomalies2$tmin[anomalies2$site == "Donana"], type ="l", col = "blue")

plot(anomalies2$tmax[anomalies2$site == "Zackenberg"], type ="l", col = "red")
lines(anomalies2$tmin[anomalies2$site == "Zackenberg"], type ="l", col = "blue")

ggplot(anom, aes(date, tmax, col = site)) + geom_line() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + xlab("") + ylab("anomalies") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10), legend.title = element_blank()) +
  geom_hline(yintercept = 0)

ggplot(anom, aes(date, tmin, col = site)) + geom_line() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + xlab("") + ylab("anomalies") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10), legend.title = element_blank()) +
  geom_hline(yintercept = 0)

ggplot(ebd, aes(date, tmin)) + geom_line() + geom_line(aes(date, movaver), col = "red") +
  geom_smooth(span = 0.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + xlab("") + ylab("anomalies") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10), legend.title = element_blank()) +
  geom_hline(yintercept = 0)

ggplot(zkb, aes(date, tmin)) + geom_line() + geom_line(aes(date, movaver), col = "red") +
  geom_smooth(span = 0.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + xlab("") + ylab("anomalies") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10), legend.title = element_blank()) +
  geom_hline(yintercept = 0)

ggplot(combined, aes(date, tmax, col = site)) + geom_line() + theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 25), axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 25), legend.title = element_blank(), legend.text = element_text(size = 15)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + xlab("") + ylab("maximum temperature anomalies") + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(date, movaver, col = site), size = 1.7) #+ 
  #scale_color_manual("Lines", values = c("darkred", "darkblue"))
 ggsave("Fig2.tiff", width = 40, height = 20, units = "cm")
 ggsave("Fig2.png", width = 40, height = 20, units = "cm")
   
 
# geom_smooth(span = 0.2,  aes(fill = site), alpha = 0.5) +
