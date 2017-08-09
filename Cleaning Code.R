library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(kknn)

#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("C:/Users/Steven Jongerden/Desktop/Machine Learning/Data/properties_2016.csv")

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(regionidcounty),]

#Remove rows that have empty taxvaluedollarcnt as these cannot be predicted
dataset <- dataset[!is.na(taxvaluedollarcnt),]

#Create table with comparison on missing values 
precentageall <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Imputate missing values and transformation of variables 
dataset[is.na(parcelid),"parcelid"] <- 0
dataset[is.na(fireplaceflag),"fireplaceflag"] <- 0
dataset[is.na(fullbathcnt),"fullbathcnt"] <- 0
dataset[is.na(garagecarcnt),"garagecarcnt"] <- 0
dataset[is.na(garagetotalsqft),"garagetotalsqft"] <- 0
dataset[is.na(hashottuborspa),"hashottuborspa"] <- 0
dataset[is.na(heatingorsystemtypeid),"heatingorsystemtypeid"] <- 0
dataset[is.na(latitude),"latitude"] <- 0
dataset[is.na(longitude),"longitude"] <- 0
dataset[is.na(lotsizesquarefeet),"lotsizesquarefeet"] <- 0
dataset[is.na(numberofstories),"numberofstories"] <- 0
dataset[is.na(poolcnt),"poolcnt"] <- 0
dataset[is.na(poolsizesum),"poolsizesum"] <- 0
dataset[is.na(pooltypeid10),"pooltypeid10"] <- 0
dataset[is.na(pooltypeid2),"pooltypeid2"] <- 0
dataset[is.na(pooltypeid7),"pooltypeid7"] <- 0
dataset[is.na(propertycountylandusecode),"propertycountylandusecode"] <- 0
dataset[is.na(propertylandusetypeid),"propertylandusetypeid"] <- 0
dataset$propertyzoningdesc = as.character(dataset$propertyzoningdesc)
dataset[is.na(propertyzoningdesc),"propertyzoningdesc"] <- "Other"
dataset$propertyzoningdesc = as.factor(dataset$propertyzoningdesc)
dataset$regionidcounty <- factor(dataset$regionidcounty)
dataset$regionidcity <- as.numeric(dataset$regionidcity)
dataset$regionidzip <- factor(dataset$regionidzip)
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood)
dataset[is.na(roomcnt),"roomcnt"] <- 0
dataset[is.na(storytypeid),"storytypeid"] <- 0
dataset[is.na(typeconstructiontypeid),"typeconstructiontypeid"] <- 0
dataset[is.na(yardbuildingsqft17),"yardbuildingsqft17"] <- 0
dataset[is.na(yardbuildingsqft26),"yardbuildingsqft26"] <- 0
dataset[is.na(threequarterbathnbr),"threequarterbathnbr"] <- 0

dataset[is.na(structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
dataset[is.na(landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]

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
  dataset[is.na(unitcnt),"unitcnt"] <- 0

#Compute missing values in the cleaned data set  
precentageallclean <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Record the change in missing values in an .csv file. 
totalmissing <- rbind(precentageall, precentageallclean)
write.csv(x = totalmissing, "percentage.csv")

#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$roomcnt!=0,]

head(dataset)





