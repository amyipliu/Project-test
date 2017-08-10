library(readr)
library(ggplot2)
housingdata <- read_csv("C:/Users/Steven Jongerden/Desktop/housingdata.csv")
                            

#Comparison of building year of sample with population "Los Angeles"

buidingyear <-data.frame(table(housingdata$yearbuilt))          
colnames(buidingyear) <- c("Building year", "Freq")
buidingyear$`Building year` <- as.numeric(as.character(buidingyear$`Building year`))

#Data from the Central Bureau of Statistics on the year 2015. 
YearConstruction <- data.frame("Population" = c(525439, 376213, 717794, 512174, 484699,
                                               407955, 218802, 210182, 21848, 1612),
                               "Sample" = c(0,0,0,0,0,0,0,0,0,0))

YearConstruction <-cbind(YearConstruction, data.frame(Description = c("Built 1939 or earlier",
                                                   "Built 1940 to 1949",
                                                   "Built 1950 to 1959",
                                                   "Built 1960 to 1969",
                                                   "Built 1970 to 1979",
                                                   "Built 1980 to 1989",
                                                   "Built 1990 to 1999",
                                                   "Built 2000 to 2009",
                                                   "Built 2010 to 2013",
                                                   "Built 2014 or later")))

YearConstruction[1,2] = sum(buidingyear[buidingyear<=1939, "Freq"], na.rm = TRUE)
YearConstruction[2,2] = sum(buidingyear[buidingyear>=1940 & buidingyear<=1949, "Freq"], na.rm = TRUE)
YearConstruction[3,2] = sum(buidingyear[buidingyear>=1950 & buidingyear<=1959, "Freq"], na.rm = TRUE)
YearConstruction[4,2] = sum(buidingyear[buidingyear>=1960 & buidingyear<=1969, "Freq"], na.rm = TRUE)
YearConstruction[5,2] = sum(buidingyear[buidingyear>=1970 & buidingyear<=1979, "Freq"], na.rm = TRUE)
YearConstruction[6,2] = sum(buidingyear[buidingyear>=1980 & buidingyear<=1989, "Freq"], na.rm = TRUE)
YearConstruction[7,2] = sum(buidingyear[buidingyear>=1990 & buidingyear<=1999, "Freq"], na.rm = TRUE)
YearConstruction[8,2] = sum(buidingyear[buidingyear>=2000 & buidingyear<=2009, "Freq"], na.rm = TRUE)
YearConstruction[9,2] = sum(buidingyear[buidingyear>=2010 & buidingyear<=2013, "Freq"], na.rm = TRUE)
YearConstruction[10,2] = sum(buidingyear[buidingyear>=2014, "Freq"], na.rm = TRUE)
correctionfactor <- sum(YearConstruction$Population)/sum(YearConstruction$Sample)
YearConstruction$CorrectedPop <- round(YearConstruction$Population / correctionfactor,0)
sum(YearConstruction$Sample)
sum(YearConstruction$CorrectedPop)
chisq.test(YearConstruction$CorrectedPop, YearConstruction$Sample)

plot1a<-YearConstruction[,c("Description", "CorrectedPop")]
colnames(plot1a) <- c("Description", "Data")
plot1a$Set <- "Population"
plot1b<-YearConstruction[,c("Description", "Sample")]
colnames(plot1b) <- c("Description", "Data")
plot1b$Set <- "Sample"
plot1 <- rbind(plot1a, plot1b)

ggplot(plot1, aes(Description, Data, fill=Set))+geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background = element_rect(fill="white")) + 
  xlab("Construction year") + ylab("Count") +
  scale_fill_brewer(palette="Blues") + ggtitle("Comparison of Construction Year")




#Comparison of the tax value 
HouseValue <- data.frame("Population" = c(45323, 27508, 35280, 55574, 196281,
                                          525246, 456642, 158025),
                               "Sample" = c(0,0,0,0,0,0,0,0))

HouseValue[1,2] = nrow(housingdata[housingdata$taxvaluedollarcnt<50000,])
HouseValue[2,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=50000 & housingdata$taxvaluedollarcnt<100000,])
HouseValue[3,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=100000 & housingdata$taxvaluedollarcnt<150000,])
HouseValue[4,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=150000 & housingdata$taxvaluedollarcnt<200000,])
HouseValue[5,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=200000 & housingdata$taxvaluedollarcnt<300000,])
HouseValue[6,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=300000 & housingdata$taxvaluedollarcnt<500000,])
HouseValue[7,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>=500000 & housingdata$taxvaluedollarcnt<1000000,])
HouseValue[8,2] = nrow(housingdata[housingdata$taxvaluedollarcnt>1000000,])

correctionfactor <- sum(HouseValue$Population)/sum(HouseValue$Sample)
HouseValue$CorrectedPop <- round(HouseValue$Population / correctionfactor,0)
sum(HouseValue$Sample)
sum(HouseValue$CorrectedPop)
chisq.test(HouseValue$CorrectedPop, HouseValue$Sample)

HouseValue <-cbind(HouseValue, data.frame(Description = c("<50000",
                                                          ">=50000",
                                                          ">=100000",
                                                          ">=150000",
                                                          ">=200000",
                                                          ">=300000",
                                                          ">=500000",
                                                          ">1000000")))
HouseValue$Description
levels(HouseValue$Description) <- c("<50000",">=50000",">=100000",">=150000",">=200000",">=300000",">=500000",">1000000")

plot2a<-HouseValue[,c("Description", "CorrectedPop")]
colnames(plot2a) <- c("Description", "Data")
plot2a$Set <- "Population"
plot2b<-HouseValue[,c("Description", "Sample")]
colnames(plot2b) <- c("Description", "Data")
plot2b$Set <- "Sample"
plot2 <- rbind(plot2a, plot2b)

ggplot(plot2, aes(Description, Data, fill=Set))+geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background = element_rect(fill="white")) + 
  xlab("House value") + ylab("Count") +
  scale_fill_brewer(palette="Blues") + ggtitle("Comparison of House Value")


#Number of bedrooms
bedroomscount <- data.frame(table(housingdata$bedroomcnt))
colnames(bedroomscount) <- c('Bedrooms', "Freq")
bedroomscount$Bedrooms <- as.numeric(as.character(bedroomscount$Bedrooms))

Bedrooms <- data.frame("Population" = c(213939, 693915, 1054696, 981580, 417703,
                                        114885),
                         "Sample" = c(0,0,0,0,0,0))

Bedrooms[1,2] = sum(bedroomscount[bedroomscount$Bedrooms==0,])
Bedrooms[2,2] = sum(bedroomscount[bedroomscount$Bedrooms==1,])
Bedrooms[3,2] = sum(bedroomscount[bedroomscount$Bedrooms==2,])
Bedrooms[4,2] = sum(bedroomscount[bedroomscount$Bedrooms==3,])
Bedrooms[5,2] = sum(bedroomscount[bedroomscount$Bedrooms==4,])
Bedrooms[6,2] = sum(bedroomscount[bedroomscount$Bedrooms>=5,])
correctionfactor <- sum(Bedrooms$Population)/sum(Bedrooms$Sample)
Bedrooms$CorrectedPop <- round(Bedrooms$Population / correctionfactor,0)
sum(Bedrooms$Sample)
sum(Bedrooms$CorrectedPop)
chisq.test(Bedrooms$CorrectedPop, Bedrooms$Sample)

Bedrooms <-cbind(Bedrooms, data.frame(Description = c("<0",
                                                          ">=1",
                                                          ">=2",
                                                          ">=3",
                                                          ">=4",
                                                          ">=5")))

plot3a<-Bedrooms[,c("Description", "CorrectedPop")]
colnames(plot3a) <- c("Description", "Data")
plot3a$Set <- "Population"
plot3b<-Bedrooms[,c("Description", "Sample")]
colnames(plot3b) <- c("Description", "Data")
plot3b$Set <- "Sample"
plot3 <- rbind(plot3a, plot3b)

ggplot(plot3, aes(Description, Data, fill=Set))+geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.background = element_rect(fill="white")) + 
  xlab("Number of bedrooms") + ylab("Count") +
  scale_fill_brewer(palette="Blues") + ggtitle("Comparison of number of bedrooms")




