library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)

#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("C:/Users/Steven Jongerden/Desktop/Machine Learning/Data/properties_2016.csv")

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(regionidcounty),]
dataset <- dataset[dataset$regionidcounty==3101,]

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
dataset[is.na(propertyzoningdesc),"propertyzoningdesc"] <- "Other"
dataset[is.na(roomcnt),"roomcnt"] <- 0
dataset[is.na(storytypeid),"storytypeid"] <- 0
dataset[is.na(typeconstructiontypeid),"typeconstructiontypeid"] <- 0
dataset[is.na(yardbuildingsqft17),"yardbuildingsqft17"] <- 0
dataset[is.na(yardbuildingsqft26),"yardbuildingsqft26"] <- 0
dataset[is.na(fireplacecnt),"fireplacecnt"] <- 0
dataset[is.na(airconditioningtypeid),"airconditioningtypeid"] <- 0
dataset[is.na(architecturalstyletypeid),"architecturalstyletypeid"] <- 0
dataset[is.na(basementsqft),"basementsqft"] <- 0
dataset[is.na(bathroomcnt),"bathroomcnt"] <- 0
dataset[is.na(bedroomcnt),"bedroomcnt"] <- 0
dataset[is.na(buildingqualitytypeid),"buildingqualitytypeid"] <- 0
dataset[is.na(buildingclasstypeid),"buildingclasstypeid"] <- 0
dataset[is.na(calculatedbathnbr),"calculatedbathnbr"] <- 0
dataset[is.na(decktypeid),"decktypeid"] <- 0
dataset[is.na(threequarterbathnbr),"threequarterbathnbr"] <- 0
dataset[is.na(finishedfloor1squarefeet),"finishedfloor1squarefeet"] <- 0
dataset[is.na(calculatedfinishedsquarefeet),"calculatedfinishedsquarefeet"] <- 0
dataset[is.na(finishedsquarefeet6),"finishedsquarefeet6"] <- 0
dataset[is.na(finishedsquarefeet12),"finishedsquarefeet12"] <- 0
dataset[is.na(finishedsquarefeet13),"finishedsquarefeet13"] <- 0
dataset[is.na(finishedsquarefeet15),"finishedsquarefeet15"] <- 0
dataset[is.na(finishedsquarefeet50),"finishedsquarefeet50"] <- 0
dataset[is.na(unitcnt),"unitcnt"] <- 0

dataset[is.na(structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
dataset[is.na(landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]

dataset$propertyzoningdesc = as.character(dataset$propertyzoningdesc)
dataset$propertyzoningdesc = as.factor(dataset$propertyzoningdesc)
dataset$regionidcounty <- factor(dataset$regionidcounty)
dataset$regionidcity <- factor(dataset$regionidcity)
dataset$regionidzip <- factor(dataset$regionidzip)
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid)
dataset$architecturalstyletypeid <- factor(dataset$architecturalstyletypeid)
dataset$buildingclasstypeid <- factor(dataset$buildingclasstypeid)
dataset$buildingqualitytypeid <- factor(dataset$buildingqualitytypeid)
dataset$decktypeid <- factor(dataset$decktypeid)
dataset$heatingorsystemtypeid <- factor(dataset$heatingorsystemtypeid)
dataset$pooltypeid10 <- factor(dataset$pooltypeid10)
dataset$pooltypeid2 <- factor(dataset$pooltypeid2)
dataset$pooltypeid7 <- factor(dataset$pooltypeid7)
dataset$storytypeid <- factor(dataset$storytypeid)
dataset$typeconstructiontypeid <- factor(dataset$typeconstructiontypeid)

dataset <- dataset[!is.na(dataset$structuretaxvaluedollarcnt),]
dataset <- dataset[!is.na(dataset$landtaxvaluedollarcnt),]

#Compute missing values in the cleaned data set  
precentageallclean <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))
classtest <- data.frame(lapply(dataset, function(x) class(x)))

#Remove columns that are empty, and have no information

dataset$architecturalstyletypeid <- NULL
dataset$basementsqft<- NULL
dataset$decktypeid<- NULL
dataset$finishedfloor1squarefeet<- NULL
dataset$finishedsquarefeet13<- NULL
dataset$finishedsquarefeet50<- NULL
dataset$finishedsquarefeet6<- NULL
dataset$fireplacecnt<- NULL
dataset$garagecarcnt<- NULL
dataset$garagetotalsqft<- NULL
dataset$poolsizesum<- NULL
dataset$pooltypeid2<- NULL
dataset$regionidcity<- NULL
dataset$regionidneighborhood<- NULL
dataset$roomcnt<- NULL
dataset$storytypeid<- NULL
dataset$threequarterbathnbr<- NULL
dataset$typeconstructiontypeid<- NULL
dataset$yardbuildingsqft17<- NULL
dataset$yardbuildingsqft26<- NULL
dataset$fips <-NULL

#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$structuretaxvaluedollarcnt!=0,]
percentagehous <- data.frame(lapply(housingdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Create land dataset on rule ....
landdataset <- dataset[!is.na(regionidzip) & !is.na(yearbuilt),]
percentageland <- data.frame(lapply(landdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))  
  

#Record the change in missing values in an .csv file. 
totalmissing <- cbind(c("precentageall", "precentageallclean", "percentagehous", "percentageland", "class"),
                      rbind(precentageall, precentageallclean, percentagehous, percentageland, classtest))

rm(precentageall)
rm(precentageallclean)
rm(percentagehous)
rm(percentageland)
rm(classtest)
rm(dataset)

#write.csv(x = totalmissing, "percentage.csv")
#write.csv(housingdataset, "housingdata.csv")
#write.csv(landdataset, "landdataset.csv")

###########################################################################################################
############################################## END OF CLEANING ############################################
###########################################################################################################

#Create tableplot with all the variables for landtaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 4)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdata, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "taxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create tableplot with all the variables for structuretaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 4)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdata, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "taxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create corplots with all the numeric variables for landtaxvaluedollarcnt
numeric_features <- names(housingdataset)[sapply(housingdataset, is.numeric)]
corHousingLandTax <- cor(housingdataset %>% select(one_of(numeric_features, "landtaxvaluedollarcnt")), method = "pearson", use = "pairwise.complete.obs")
corHousingLandTax[is.na(corHousingLandTax)] = 0
corrplot(corHousingLandTax, method = "color", order="hclust")

#Create corplots with all the categorical variables for landtaxvaluedollarcnt
#Minimized to save computation time. 
housingdataset2 <- head(housingdataset, 1000)
ordinal_features <- c('airconditioningtypeid', 'buildingqualitytypeid','buildingclasstypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousingLandTax2 <- cor(data.matrix(housingdataset2 %>% select(one_of(ordinal_features, "landtaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

ordinal_features <- c('airconditioningtypeid', 'buildingqualitytypeid','buildingclasstypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousinghouseTax2 <- cor(data.matrix(housingdataset2 %>% select(one_of(ordinal_features, "structuretaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

