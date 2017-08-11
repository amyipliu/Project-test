library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)

#Open Data Set ##Set your file location of the properties data set##
dataset <- fread("C:/Users/Steven Jongerden/Desktop/Machine Learning/Data/properties_2016.csv")
soldhouses <- fread("C:/Users/Steven Jongerden/Desktop/Machine Learning/Data/train_2016_v2.csv", header = TRUE)
dataset <- as.data.frame(left_join(soldhouses, dataset, by ="parcelid"))

#Remove completely missing rows from the dataset
dataset <- dataset[!is.na(dataset$regionidcounty),]
dataset <- dataset[dataset$regionidcounty==3101,]

#Remove rows that have empty taxvaluedollarcnt as these cannot be predicted
dataset <- dataset[!is.na(dataset$taxvaluedollarcnt),]

#Create table with comparison on missing values 
precentageall <- data.frame(lapply(dataset, function(x) sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))))

#Imputate missing values and transformation of variables 
dataset[is.na(dataset$fireplaceflag),"fireplaceflag"] <- 0
dataset[is.na(dataset$fullbathcnt),"fullbathcnt"] <- 0
dataset[is.na(dataset$garagecarcnt),"garagecarcnt"] <- 0
dataset[is.na(dataset$garagetotalsqft),"garagetotalsqft"] <- 0
dataset[is.na(dataset$hashottuborspa),"hashottuborspa"] <- 0
dataset[is.na(dataset$heatingorsystemtypeid),"heatingorsystemtypeid"] <- 0
dataset[is.na(dataset$latitude),"latitude"] <- 0
dataset[is.na(dataset$longitude),"longitude"] <- 0
dataset[is.na(dataset$lotsizesquarefeet),"lotsizesquarefeet"] <- 0
dataset[is.na(dataset$numberofstories),"numberofstories"] <- 0
dataset[is.na(dataset$poolcnt),"poolcnt"] <- 0
dataset[is.na(dataset$poolsizesum),"poolsizesum"] <- 0
dataset[is.na(dataset$pooltypeid10),"pooltypeid10"] <- 0
dataset[is.na(dataset$pooltypeid2),"pooltypeid2"] <- 0
dataset[is.na(dataset$pooltypeid7),"pooltypeid7"] <- 0
dataset[is.na(dataset$propertycountylandusecode),"propertycountylandusecode"] <- 0
dataset[is.na(dataset$propertylandusetypeid),"propertylandusetypeid"] <- 0
dataset[is.na(dataset$propertyzoningdesc),"propertyzoningdesc"] <- "Other"
dataset[is.na(dataset$roomcnt),"roomcnt"] <- 0
dataset[is.na(dataset$storytypeid),"storytypeid"] <- 0
dataset[is.na(dataset$typeconstructiontypeid),"typeconstructiontypeid"] <- 0
dataset[is.na(dataset$yardbuildingsqft17),"yardbuildingsqft17"] <- 0
dataset[is.na(dataset$yardbuildingsqft26),"yardbuildingsqft26"] <- 0
dataset[is.na(dataset$fireplacecnt),"fireplacecnt"] <- 0
dataset[is.na(dataset$airconditioningtypeid),"airconditioningtypeid"] <- 0
dataset[is.na(dataset$architecturalstyletypeid),"architecturalstyletypeid"] <- 0
dataset[is.na(dataset$basementsqft),"basementsqft"] <- 0
dataset[is.na(dataset$bathroomcnt),"bathroomcnt"] <- 0
dataset[is.na(dataset$bedroomcnt),"bedroomcnt"] <- 0
dataset[is.na(dataset$buildingqualitytypeid),"buildingqualitytypeid"] <- 0
dataset[is.na(dataset$buildingclasstypeid),"buildingclasstypeid"] <- 0
dataset[is.na(dataset$calculatedbathnbr),"calculatedbathnbr"] <- 0
dataset[is.na(dataset$decktypeid),"decktypeid"] <- 0
dataset[is.na(dataset$threequarterbathnbr),"threequarterbathnbr"] <- 0
dataset[is.na(dataset$finishedfloor1squarefeet),"finishedfloor1squarefeet"] <- 0
dataset[is.na(dataset$calculatedfinishedsquarefeet),"calculatedfinishedsquarefeet"] <- 0
dataset[is.na(dataset$finishedsquarefeet6),"finishedsquarefeet6"] <- 0
dataset[is.na(dataset$finishedsquarefeet12),"finishedsquarefeet12"] <- 0
dataset[is.na(dataset$finishedsquarefeet13),"finishedsquarefeet13"] <- 0
dataset[is.na(dataset$finishedsquarefeet15),"finishedsquarefeet15"] <- 0
dataset[is.na(dataset$finishedsquarefeet50),"finishedsquarefeet50"] <- 0
dataset[is.na(dataset$unitcnt),"unitcnt"] <- 0

dataset[is.na(dataset$structuretaxvaluedollarcnt),"structuretaxvaluedollarcnt"] <- dataset[is.na(dataset$structuretaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$structuretaxvaluedollarcnt),"landtaxvaluedollarcnt"]
dataset[is.na(dataset$landtaxvaluedollarcnt),"landtaxvaluedollarcnt"] <- dataset[is.na(dataset$landtaxvaluedollarcnt),"taxvaluedollarcnt"] - dataset[is.na(dataset$landtaxvaluedollarcnt),"structuretaxvaluedollarcnt"]

dataset$propertyzoningdesc = as.character(dataset$propertyzoningdesc)
dataset$propertyzoningdesc = as.factor(dataset$propertyzoningdesc)
dataset$regionidcounty <- factor(dataset$regionidcounty)
dataset$regionidcity <- factor(dataset$regionidcity)
dataset$regionidzip <- factor(dataset$regionidzip)
dataset$regionidneighborhood <- factor(dataset$regionidneighborhood)
dataset$airconditioningtypeid <- factor(dataset$airconditioningtypeid)
dataset$architecturalstyletypeid <- factor(dataset$architecturalstyletypeid)
dataset$buildingclasstypeid <- factor(dataset$buildingclasstypeid)
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
dataset$regionidcounty <- NULL

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

###########################################################################################################
############################################## END OF CLEANING ############################################
###########################################################################################################
############################################### Start of EDA ##############################################

#Create tableplot with all the variables for landtaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 3)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdataset, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "landtaxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create tableplot with all the variables for structuretaxvaluedollarcnt
colMtx <- matrix(names(housingdataset)[1:length(housingdataset)-1], nrow = 3)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdataset, 
            select_string = c(colMtx[,i], "structuretaxvaluedollarcnt"), 
            sortCol = "structuretaxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create corplots with all the numeric variables for landtaxvaluedollarcnt
numeric_features <- names(housingdataset)[sapply(housingdataset, is.numeric)]
corHousingLandTax <- cor(housingdataset %>% select(one_of(numeric_features, "landtaxvaluedollarcnt")), method = "pearson", use = "pairwise.complete.obs")
corHousingLandTax[is.na(corHousingLandTax)] = 0
corrplot(corHousingLandTax, method = "color", order="hclust")

housingdataset2 <- head(housingdataset, 10000)
#Create corplots with all the categorical variables for landtaxvaluedollarcnt
#Minimized to save computation time. 
ordinal_features <- c('airconditioningtypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousingLandTax2 <- cor(data.matrix(housingdataset2 %>% select(one_of(ordinal_features, "landtaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

ordinal_features <- c('airconditioningtypeid', 'buildingqualitytypeid','buildingclasstypeid', 'heatingorsystemtypeid','pooltypeid10', 'pooltypeid7', 'propertylandusetypeid','regionidzip')
corHousinghouseTax2 <- cor(data.matrix(housingdataset %>% select(one_of(ordinal_features, "structuretaxvaluedollarcnt"))), method = "kendall", use = "pairwise.complete.obs")
corrplot(corHousingLandTax2, method = "color", order="hclust")

### EDA ###
#unitcnt
ggplot(housingdataset, aes(x=log(unitcnt), log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Unit count versus house taxes") +
  theme(panel.background = element_rect(fill="white"))
  
#yearbuilt
ggplot(housingdataset, aes(x=yearbuilt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Building year versus house taxes")+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=yearbuilt))+geom_bar()+ggtitle("Building year of house")+
  theme(panel.background = element_rect(fill="white"))

#taxvaluedollarcnt
ggplot(housingdataset, aes(x=taxvaluedollarcnt))+geom_histogram(bins = 200)+ggtitle("Total Tax value") + xlim(c(0,1e6))+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=log(taxvaluedollarcnt)))+geom_histogram(bins = 200)+ggtitle("Total Tax value") + xlim(c(5,17))+
  theme(panel.background = element_rect(fill="white"))

#structuretaxvaluedollarcnt
ggplot(housingdataset, aes(x=structuretaxvaluedollarcnt))+geom_histogram(bins = 200)+ggtitle("House Tax value") + xlim(c(0,1e6))+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=log(structuretaxvaluedollarcnt)))+geom_histogram(bins = 200)+ggtitle("House Tax value") + xlim(c(5,17))+
  theme(panel.background = element_rect(fill="white"))

#landtaxvaluedollarcnt
ggplot(housingdataset, aes(x=landtaxvaluedollarcnt))+geom_histogram(bins = 200)+ggtitle("Land Tax value") + xlim(c(0,1e6))+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=log(landtaxvaluedollarcnt)))+geom_histogram(bins = 200)+ggtitle("Land Tax value") + xlim(c(5,17))+
  theme(panel.background = element_rect(fill="white"))

#assessmentyear
ggplot(housingdataset, aes(x=assessmentyear))+geom_bar()+ggtitle("Assessment year of house")+
  theme(panel.background = element_rect(fill="white"))
housingdataset$assessmentyear <- factor(housingdataset$assessmentyear)
ggplot(housingdataset, aes(x=assessmentyear, y = log(taxvaluedollarcnt)))+geom_boxplot()+ggtitle("Assessment year of house")+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=assessmentyear, y = log(landtaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Assessment year of house")+
  theme(panel.background = element_rect(fill="white"))
ggplot(housingdataset, aes(x=assessmentyear, y = log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Assessment year of house")+
  theme(panel.background = element_rect(fill="white"))


####################### MODEL BUILDING ########################
library(car)
housingdataset$airconditioningtypeid <- as.character(housingdataset$airconditioningtypeid)
housingdataset$heatingorsystemtypeid <- as.character(housingdataset$heatingorsystemtypeid)
housingdataset[housingdataset$airconditioningtypeid==0,"airconditioningtypeid"] <- "None"
housingdataset[housingdataset$airconditioningtypeid==1,"airconditioningtypeid"] <- "Central"
housingdataset[housingdataset$airconditioningtypeid==9,"airconditioningtypeid"] <- "Central"
housingdataset[housingdataset$airconditioningtypeid==13,"airconditioningtypeid"] <- "Central"
housingdataset[housingdataset$heatingorsystemtypeid==2,"heatingorsystemtypeid"] <- "Central"
housingdataset[housingdataset$heatingorsystemtypeid==7,"heatingorsystemtypeid"] <- "Floor"
housingdataset[housingdataset$heatingorsystemtypeid==0,"heatingorsystemtypeid"] <- "Other"
housingdataset[housingdataset$heatingorsystemtypeid==20,"heatingorsystemtypeid"] <- "Other"
housingdataset$heatingorsystemtypeid <- factor(housingdataset$heatingorsystemtypeid)
housingdataset$airconditioningtypeid <- factor(housingdataset$airconditioningtypeid)

library(caret)
set.seed(0)
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

#Linear model with Box Cox transformation
#Model estimation normal
model <- lm(structuretaxvaluedollarcnt~airconditioningtypeid + bathroomcnt + bedroomcnt + buildingqualitytypeid +
              calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt + numberofstories + 
              regionidzip + unitcnt, data = train)
bc <- boxCox(model)
lambda = bc$x[which(bc$y == max(bc$y))]
lambda

#Model estimation with box coxs transformation 
structuretaxvaluedollarcnt.bc = (train$structuretaxvaluedollarcnt^lambda - 1)/lambda

model2 <- lm(structuretaxvaluedollarcnt.bc~airconditioningtypeid + bathroomcnt + bedroomcnt + buildingqualitytypeid +
              calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt + numberofstories + 
               unitcnt+regionidzip , data = train)

#REducte for computational testing
train <- train[1:5000,c("structuretaxvaluedollarcnt", "airconditioningtypeid", "bathroomcnt", "bedroomcnt","buildingqualitytypeid",
  "calculatedfinishedsquarefeet", "heatingorsystemtypeid", "poolcnt", "yearbuilt", "numberofstories", 
                        "unitcnt","regionidzip")]
structuretaxvaluedollarcnt.bc = (train$structuretaxvaluedollarcnt^lambda - 1)/lambda

train$structuretaxvaluedollarcnt <- NULL

library(MASS)
model.empty = lm(structuretaxvaluedollarcnt.bc ~ 1, data = train)
model.full = lm(structuretaxvaluedollarcnt.bc ~ ., data = train)
scope = list(lower = formula(model.empty), upper = formula(model.full))
model3 = step(model2, scope, direction = "both", k = 2)

#Summary and conditions verification
summary(model)
summary(model2)
vif(model2)
BIC(model)
BIC(model2)

library(lmtest)
#Breusch-Pagan test for heteroskedasticity -> violated
bptest(model2)
#Breusch-Godfrey test for serial correlation -> good
bgtest(model2)
#Conditions verification
plot(model2)

#Correction for heteroskedasticity:
#White standard errors:
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

modelresults <- summary(model2, robust=T)
modelresults




