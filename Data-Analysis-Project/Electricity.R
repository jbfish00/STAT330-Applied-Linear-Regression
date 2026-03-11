

#Read in data, change data format, make factor that is the days around and including Chrsitmas
elec<-read.csv("C:/Users/jbren/Desktop/STAT_330/Data Analysis Project/IrishElectricity.csv")
elec$Date <- as.Date(elec$Date, format = "%m/%d/%Y")
elec$near_christmas <- ifelse(abs(as.Date(elec$Date) - as.Date("2009-12-25")) <= 3, "yes", "no")

#Make character vars factors
elec<-elec %>% mutate_if(is.character, as.factor)


summary(elec)

