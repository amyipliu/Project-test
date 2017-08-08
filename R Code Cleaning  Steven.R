library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(kknn)

#Open Data Set
dataset <- fread("C:/Users/Steven Jongerden/Desktop/Machine Learning/Data/properties_2016.csv")

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(regionidcounty),]
dataset <- dataset[!is.na(taxvaluedollarcnt),]

#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$roomcnt!=0,]

#Create table with comparison on missing values 
precentageall <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))
precentagehousing <- data.frame(lapply(housingdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))
totalmissing <- rbind(precentageall, precentagehousing)
write.csv(x = totalmissing, "percentage.csv")

#rawcensustractandblock
summary(dataset$rawcensustractandblock)

#regionidcounty
summary(dataset$regionidcounty)
table(dataset$regionidcounty)
dataset$regionidcounty <- factor(dataset$regionidcounty)

#regionidcity 
summary(dataset$regionidcity)
dataset$regionidcity <- as.numeric(dataset$regionidcity)

#regionidzip ### 
summary(dataset$regionidzip)
table(dataset$regionidzip)
nrow(table(dataset$regionidzip))
dataset$regionidzip <- factor(dataset$regionidzip)

#regionidneighborhood 
summary(dataset$regionidneighborhood)
table(dataset$regionidneighborhood)
nrow(table(dataset$regionidneighborhood))
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood)

#roomcnt
summary(dataset$roomcnt)
table(dataset$roomcnt)
nrow(table(dataset$roomcnt))
dataset[is.na(roomcnt),"roomcnt"] <- 0

#storytypeid 
summary(dataset$storytypeid)
table(dataset$storytypeid)
nrow(table(dataset$storytypeid))
dataset[is.na(storytypeid),"storytypeid"] <- 0

#typeconstructiontypeid 
#Only 7000 values have a value, otherwise missing
summary(dataset$typeconstructiontypeid)
table(dataset$typeconstructiontypeid)
nrow(table(dataset$typeconstructiontypeid))
dataset[is.na(typeconstructiontypeid),"typeconstructiontypeid"] <- 0

#unitcnt 
summary(dataset$unitcnt)
table(dataset$unitcnt)
nrow(table(dataset$unitcnt))

  #Imputation with correlation(R2 = 0.8)
  cor.test(dataset$unitcnt, dataset$finishedsquarefeet15)
  model <- lm(unitcnt ~ finishedsquarefeet15, data=dataset)
  model[1]
  prediction <- data.frame(dataset[is.na(unitcnt),"finishedsquarefeet15"])
  prediction$unitcnt <- as.integer(round(0.7056019097+0.0007814134*prediction$finishedsquarefeet15,0))
  nrow(dataset[is.na(unitcnt),"unitcnt"])
  nrow(prediction)
  dataset[is.na(unitcnt),"unitcnt"] <- prediction$unitcnt
  model <- NULL

#yardbuildingsqft17 
summary(dataset$yardbuildingsqft17)
table(dataset$yardbuildingsqft17)
nrow(table(dataset$yardbuildingsqft17))
#If missing than most probably no garden
dataset[is.na(yardbuildingsqft17),"yardbuildingsqft17"] <- 0

#yardbuildingsqft26 
summary(dataset$yardbuildingsqft26)
table(dataset$yardbuildingsqft26)
nrow(table(dataset$yardbuildingsqft26))
#If missing than most probably no garden
dataset[is.na(yardbuildingsqft26),"yardbuildingsqft26"] <- 0

#yearbuilt 
summary(dataset$yearbuilt)
table(dataset$yearbuilt)
nrow(table(dataset$yearbuilt))

#### DEPENDENT VARIABLES ####

#structuretaxvaluedollarcnt
summary(dataset$structuretaxvaluedollarcnt)
dataset[is.na(structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
summary(dataset$structuretaxvaluedollarcnt)
ggplot(dataset, aes(x=structuretaxvaluedollarcnt)) + geom_histogram(bins = 1000) + xlim(c(0,1000000))

#landtaxvaluedollarcnt
summary(dataset$landtaxvaluedollarcnt)
dataset[is.na(landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]
summary(dataset$landtaxvaluedollarcnt)
ggplot(dataset, aes(x=landtaxvaluedollarcnt)) + geom_histogram(bins = 1000) + xlim(c(0,1000000))

#taxvaluedollarcnt
summary(dataset$taxvaluedollarcnt)
ggplot(dataset, aes(x=taxvaluedollarcnt)) + geom_histogram(bins = 1000) + xlim(c(0,1000000))

#taxamount
summary(dataset$taxamount)

#assessmentyear
#2972420 assessments in 2015
summary(dataset$assessmentyear)
table(dataset$assessmentyear)


