library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)
library(caret)
library(car)
library(lmtest)
library(VIM)
library(mice)
library(robustHD)

#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("/Users/huanghaotian/Desktop/Kaggle Competition/Competition Data/properties_2016.csv")
soldhouses <- fread("/Users/huanghaotian/Desktop/Kaggle Competition/Competition Data/train_2016_v2.csv", header = TRUE)
dataset <- as.data.frame(left_join(soldhouses, dataset, by ="parcelid"))


#------------------------------------------------------------------------------------------------------------------------------#

#Missingness 

#MissingValues <- data.frame(missing = sapply(dataset, function(x) round(sum(is.na(x))/nrow(dataset),4)))
#MissingValues$variable <- rownames(MissingValues)
# MissingValues <- arrange(MissingValues, desc(missing)) %>% select(variable, missing)
# ggplot(MissingValues, aes(x=reorder(variable, missing), y = missing))+geom_bar(stat = "identity") + coord_flip() +
#   ylab("Percentage Missing") + xlab("Variable") + ggtitle("Percentage of missing in variables")

#------------------------------------------------------------------------------------------------------------------------------#

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(dataset$regionidcounty),]
dataset <- dataset[dataset$regionidcounty==3101,]


#Remove rows that have empty taxvaluedollarcnt as these cannot be predicted
dataset <- dataset[!is.na(dataset$taxvaluedollarcnt),]

#Create table with comparison on missing values 
precentageall <- data.frame(sapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#------------------------------------------------------------------------------------------------------------------------------#

#Remove columns that are empty, and have no information

#delete parcelid, not useful in prediction
dataset$parcelid <- NULL
#delete propertyzoningdesc, too many levels
dataset$propertyzoningdesc <- NULL
#delete logerror, I dont think it's useful
dataset$logerror <- NULL
#delete fireplaceflag, I dont think it's useful
dataset$fireplaceflag <- NULL

dataset$architecturalstyletypeid <- NULL
dataset$basementsqft<- NULL
dataset$decktypeid<- NULL
dataset$finishedfloor1squarefeet<- NULL
dataset$finishedsquarefeet13<- NULL
dataset$finishedsquarefeet50<- NULL
dataset$finishedsquarefeet6<- NULL
dataset$fireplacecnt<- NULL
dataset$garagecarcnt<- NULL
dataset$poolsizesum<- NULL
dataset$pooltypeid2<- NULL
# dataset$regionidcity<- NULL  I dont want to delete this
# dataset$regionidneighborhood<- NULL   I dont want to delete this
dataset$roomcnt<- NULL
dataset$storytypeid<- NULL
dataset$threequarterbathnbr<- NULL
dataset$typeconstructiontypeid<- NULL
dataset$yardbuildingsqft17<- NULL
dataset$yardbuildingsqft26<- NULL
dataset$fips <-NULL
# dataset$regionidcounty <- NULL
dataset$garagetotalsqft <- NULL
dataset$buildingclasstypeid <- NULL
dataset$numberofstories <- NULL
dataset$rawcensustractandblock <- NULL
dataset$censustractandblock <- NULL
dataset$pooltypeid10 <- NULL

#------------------------------------------------------------------------------------------------------------------------------#

#Impute missing values and transformation of variables 
#dataset[is.na(dataset$fireplaceflag),"fireplaceflag"] <- 0

dataset[is.na(dataset$fullbathcnt),"fullbathcnt"] <- median(!is.na(dataset$fullbathcnt))
#dataset[is.na(dataset$garagecarcnt),"garagecarcnt"] <- 0
#dataset[is.na(dataset$garagetotalsqft),"garagetotalsqft"] <- 0
dataset[is.na(dataset$hashottuborspa),"hashottuborspa"] <- 0
dataset[is.na(dataset$heatingorsystemtypeid),"heatingorsystemtypeid"] <- median(!is.na(dataset$heatingorsystemtypeid))
dataset[is.na(dataset$latitude),"latitude"] <- 0
dataset[is.na(dataset$longitude),"longitude"] <- 0
dataset[is.na(dataset$lotsizesquarefeet),"lotsizesquarefeet"] <- median(!is.na(dataset$lotsizesquarefeet))
#dataset[is.na(dataset$numberofstories),"numberofstories"] <- 0
dataset[is.na(dataset$poolcnt),"poolcnt"] <- 0
#dataset[is.na(dataset$poolsizesum),"poolsizesum"] <- 0
#dataset[is.na(dataset$pooltypeid10),"pooltypeid10"] <- 0
#dataset[is.na(dataset$pooltypeid2),"pooltypeid2"] <- 0
dataset[is.na(dataset$pooltypeid7),"pooltypeid7"] <- 0
#dataset[is.na(dataset$propertycountylandusecode),"propertycountylandusecode"] <- 0
#dataset[is.na(dataset$propertylandusetypeid),"propertylandusetypeid"] <- 0
#dataset[is.na(dataset$propertyzoningdesc),"propertyzoningdesc"] <- "Other"
#dataset[is.na(dataset$roomcnt),"roomcnt"] <- 0
#dataset[is.na(dataset$storytypeid),"storytypeid"] <- 0
#dataset[is.na(dataset$typeconstructiontypeid),"typeconstructiontypeid"] <- 0
#dataset[is.na(dataset$yardbuildingsqft17),"yardbuildingsqft17"] <- 0
#dataset[is.na(dataset$yardbuildingsqft26),"yardbuildingsqft26"] <- 0
#dataset[is.na(dataset$fireplacecnt),"fireplacecnt"] <- 0
dataset[is.na(dataset$airconditioningtypeid),"airconditioningtypeid"] <- median(!is.na(dataset$airconditioningtypeid))
#dataset[is.na(dataset$architecturalstyletypeid),"architecturalstyletypeid"] <- 0
#dataset[is.na(dataset$basementsqft),"basementsqft"] <- 0
#dataset[is.na(dataset$bathroomcnt),"bathroomcnt"] <- 0
#dataset[is.na(dataset$bedroomcnt),"bedroomcnt"] <- 0
dataset[is.na(dataset$buildingqualitytypeid),"buildingqualitytypeid"] <- median(!is.na(dataset$buildingqualitytypeid))
#dataset[is.na(dataset$buildingclasstypeid),"buildingclasstypeid"] <- 0
dataset[is.na(dataset$calculatedbathnbr),"calculatedbathnbr"] <- median(!is.na(dataset$calculatedbathnbr))
#dataset[is.na(dataset$decktypeid),"decktypeid"] <- 0
#dataset[is.na(dataset$threequarterbathnbr),"threequarterbathnbr"] <- 0
#dataset[is.na(dataset$finishedfloor1squarefeet),"finishedfloor1squarefeet"] <- 0
dataset[is.na(dataset$calculatedfinishedsquarefeet),"calculatedfinishedsquarefeet"] <- median(!is.na(dataset$calculatedfinishedsquarefeet))
#dataset[is.na(dataset$finishedsquarefeet6),"finishedsquarefeet6"] <- 0
dataset[is.na(dataset$finishedsquarefeet12),"finishedsquarefeet12"] <- median(!is.na(dataset$finishedsquarefeet12))
#dataset[is.na(dataset$finishedsquarefeet13),"finishedsquarefeet13"] <- 0
dataset[is.na(dataset$finishedsquarefeet15),"finishedsquarefeet15"] <- median(!is.na(dataset$finishedsquarefeet15))
#dataset[is.na(dataset$finishedsquarefeet50),"finishedsquarefeet50"] <- 0
dataset[is.na(dataset$unitcnt),"unitcnt"] <- median(!is.na(dataset$unitcnt))
dataset[is.na(dataset$taxamount), 'taxamount'] <- median(!is.na(dataset$taxamount))
dataset[is.na(dataset$regionidcity), 'regionidcity'] <- median(!is.na(dataset$regionidcity))
dataset[is.na(dataset$regionidneighborhood), 'regionidneighborhood'] <- median(!is.na(dataset$regionidneighborhood))
dataset[is.na(dataset$taxdelinquencyyear), 'taxdelinquencyyear'] <- 0
dataset[is.na(dataset$yearbuilt), 'yearbuilt'] <- median(!is.na(dataset$yearbuilt))

#dataset[is.na(dataset$structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(dataset$structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
#dataset[is.na(dataset$landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(dataset$landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]


classtest1 <- t(data.frame(lapply(dataset, function(x) class(x))))

#------------------------------------------------------------------------------------------------------------------------------#

#Data type manipulation
#dataset$propertyzoningdesc = as.character(dataset$propertyzoningdesc)
#dataset$propertyzoningdesc = factor(dataset$propertyzoningdesc)
test1 = as.factor(dataset$regionidcounty)
dataset$regionidcounty <- factor(dataset$regionidcounty)
test2 = as.factor(dataset$regionidcity)
dataset$regionidcity <- factor(dataset$regionidcity) # 126 levels
test3 = as.factor(dataset$regionidzip)
dataset$regionidzip <- factor(dataset$regionidzip) # 280 levels
test4 = as.factor(dataset$regionidneighborhood)
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood) # 331 levels
test5 = as.factor(dataset$airconditioningtypeid)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid) # 3 levels
# dataset$architecturalstyletypeid <- factor(dataset$architecturalstyletypeid)
# dataset$buildingclasstypeid <- factor(dataset$buildingclasstypeid)
# dataset$decktypeid <- factor(dataset$decktypeid)
test6 = as.factor(dataset$heatingorsystemtypeid)
dataset$heatingorsystemtypeid <- factor(dataset$heatingorsystemtypeid) # 3 levels
#test7 = as.factor(dataset$pooltypeid10)
#dataset$pooltypeid10 <- factor(dataset$pooltypeid10) # 2 levels
#dataset$pooltypeid2 <- factor(dataset$pooltypeid2)
test8 = as.factor(dataset$pooltypeid7)
dataset$pooltypeid7 <- factor(dataset$pooltypeid7) # 2 levels
#dataset$storytypeid <- factor(dataset$storytypeid)
#dataset$typeconstructiontypeid <- factor(dataset$typeconstructiontypeid)
dataset$transactiondate <- base::as.Date(dataset$transactiondate)
test9 = as.factor(dataset$hashottuborspa)
dataset$hashottuborspa = factor(dataset$hashottuborspa) # 2 levels
test10 = as.factor(dataset$taxdelinquencyflag)
dataset$taxdelinquencyflag = factor(dataset$taxdelinquencyflag)
dataset$propertycountylandusecode <- factor(dataset$propertycountylandusecode)

dataset <- dataset[!is.na(dataset$structuretaxvaluedollarcnt),]
dataset <- dataset[!is.na(dataset$landtaxvaluedollarcnt),]

#------------------------------------------------------------------------------------------------------------------------------#


#Compute missing values in the cleaned data set  
precentageallclean <- t(data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x))))))
classtest2 <- t(data.frame(lapply(dataset, function(x) class(x))))



dataset$airconditioningtypeid <- as.character(dataset$airconditioningtypeid)
dataset$heatingorsystemtypeid <- as.character(dataset$heatingorsystemtypeid)
dataset[dataset$airconditioningtypeid==0,"airconditioningtypeid"] <- "None"
dataset[dataset$airconditioningtypeid==1,"airconditioningtypeid"] <- "Central"
dataset[dataset$airconditioningtypeid==9,"airconditioningtypeid"] <- "Central"
dataset[dataset$airconditioningtypeid==13,"airconditioningtypeid"] <- "Central"
dataset[dataset$heatingorsystemtypeid==2,"heatingorsystemtypeid"] <- "Central"
dataset[dataset$heatingorsystemtypeid==7,"heatingorsystemtypeid"] <- "Floor"
dataset[dataset$heatingorsystemtypeid==0,"heatingorsystemtypeid"] <- "Other"
dataset[dataset$heatingorsystemtypeid==20,"heatingorsystemtypeid"] <- "Other"
dataset$heatingorsystemtypeid <- factor(dataset$heatingorsystemtypeid)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid)
dataset[dataset$propertylandusetypeid==31, "propertylandusetypeid"] <-"Commercial/Office/Residential Mixed Used"
dataset[dataset$propertylandusetypeid==47, "propertylandusetypeid"] <-"Store/Office (Mixed Use)"
dataset[dataset$propertylandusetypeid==246, "propertylandusetypeid"] <-"Duplex"
dataset[dataset$propertylandusetypeid==247, "propertylandusetypeid"] <-"Triplex"
dataset[dataset$propertylandusetypeid==248, "propertylandusetypeid"] <-"Quadruplex"
dataset[dataset$propertylandusetypeid==260, "propertylandusetypeid"] <-"Residential General"
dataset[dataset$propertylandusetypeid==261, "propertylandusetypeid"] <-"Single Family Residential"
dataset[dataset$propertylandusetypeid==263, "propertylandusetypeid"] <-"Mobile Home"
dataset[dataset$propertylandusetypeid==264, "propertylandusetypeid"] <-"Townhouse"
dataset[dataset$propertylandusetypeid==266, "propertylandusetypeid"] <-"Condominium"
dataset[dataset$propertylandusetypeid==267, "propertylandusetypeid"] <-"Cooperative"
dataset[dataset$propertylandusetypeid==269, "propertylandusetypeid"] <-"Planned Unit Development"
dataset$propertylandusetypeid <- factor(dataset$propertylandusetypeid)


#------------------------------------------------------------------------------------------------------------------------------#

#Winsorize Numerical Variables
dataset$bathroomcnt = winsorize(dataset$bathroomcnt, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$bedroomcnt = winsorize(dataset$bedroomcnt, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$buildingqualitytypeid = winsorize(dataset$buildingqualitytypeid, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$calculatedbathnbr = winsorize(dataset$calculatedbathnbr, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$calculatedfinishedsquarefeet = winsorize(dataset$calculatedfinishedsquarefeet, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$finishedsquarefeet12 = winsorize(dataset$finishedsquarefeet12, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$finishedsquarefeet15 = winsorize(dataset$finishedsquarefeet15, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$fullbathcnt = winsorize(dataset$fullbathcnt, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$lotsizesquarefeet = winsorize(dataset$lotsizesquarefeet, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$poolcnt = winsorize(dataset$poolcnt, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)
dataset$unitcnt = winsorize(dataset$unitcnt, minval = NULL, maxval = NULL, probs = c(0.025, 0.975),  na.rm = FALSE)

#------------------------------------------------------------------------------------------------------------------------------#

#Create housing dataset on rule roomtcount >0
housingdataset <- dataset[dataset$structuretaxvaluedollarcnt!=0,]
percentagehous <- data.frame(lapply(housingdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Create land dataset on rule ....
landdataset <- dataset[!is.na(dataset$regionidzip) & !is.na(dataset$yearbuilt),]
percentageland <- data.frame(lapply(dataset$landdataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))  

rm(precentageall)
rm(precentageallclean)
rm(percentagehous)
rm(percentageland)
rm(classtest)
rm(dataset)
rm(soldhouses)

#write.csv(x = totalmissing, "percentage.csv")
#write.csv(housingdataset, "housingdata.csv")
#write.csv(landdataset, "landdataset.csv")


