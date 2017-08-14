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


####################### MODEL BUILDING ########################
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
housingdataset[housingdataset$propertylandusetypeid==31, "propertylandusetypeid"] <-"Commercial/Office/Residential Mixed Used"
housingdataset[housingdataset$propertylandusetypeid==47, "propertylandusetypeid"] <-"Store/Office (Mixed Use)"
housingdataset[housingdataset$propertylandusetypeid==246, "propertylandusetypeid"] <-"Duplex"
housingdataset[housingdataset$propertylandusetypeid==247, "propertylandusetypeid"] <-"Triplex"
housingdataset[housingdataset$propertylandusetypeid==248, "propertylandusetypeid"] <-"Quadruplex"
housingdataset[housingdataset$propertylandusetypeid==260, "propertylandusetypeid"] <-"Residential General"
housingdataset[housingdataset$propertylandusetypeid==261, "propertylandusetypeid"] <-"Single Family Residential"
housingdataset[housingdataset$propertylandusetypeid==263, "propertylandusetypeid"] <-"Mobile Home"
housingdataset[housingdataset$propertylandusetypeid==264, "propertylandusetypeid"] <-"Townhouse"
housingdataset[housingdataset$propertylandusetypeid==266, "propertylandusetypeid"] <-"Condominium"
housingdataset[housingdataset$propertylandusetypeid==267, "propertylandusetypeid"] <-"Cooperative"
housingdataset[housingdataset$propertylandusetypeid==269, "propertylandusetypeid"] <-"Planned Unit Development"
housingdataset$propertylandusetypeid <- factor(housingdataset$propertylandusetypeid)



### EDA for model as proof for hypothesized relationship. 
ggplot(housingdataset, aes(airconditioningtypeid, log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Airconditioning Type")
t.test(housingdataset$structuretaxvaluedollarcnt ~ housingdataset$airconditioningtypeid)
#Significant Difference 
ggplot(housingdataset, aes(bathroomcnt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Bathroom count")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$bathroomcnt)
#Significant relation 0.59
ggplot(housingdataset, aes(bedroomcnt, log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Bedroomcnt count")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$bedroomcnt)
#Significant relation 0.31
ggplot(housingdataset, aes(factor(buildingqualitytypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Building Qualitytype")
cor.test(housingdataset$structuretaxvaluedollarcnt, housingdataset$buildingqualitytypeid)
#Significant, however, inverse as expected relationship
ggplot(housingdataset, aes(log(calculatedfinishedsquarefeet), log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Size of the house")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$calculatedfinishedsquarefeet)
#Significant relationship 0.58
ggplot(housingdataset, aes(factor(heatingorsystemtypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Heating System")
ggplot(housingdataset, aes(factor(poolcnt), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Number of Pools")
t.test(housingdataset$structuretaxvaluedollarcnt ~ housingdataset$poolcnt)
#Significant Difference 
ggplot(housingdataset, aes(factor(yearbuilt), log(structuretaxvaluedollarcnt)))+geom_point()+ggtitle("Year Build")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$yearbuilt)
#Significant relationship 0.41
ggplot(housingdataset, aes(factor(numberofstories), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Number of floors")
cor.test(log(housingdataset$structuretaxvaluedollarcnt), housingdataset$numberofstories)
#Significant relationship 0.009
ggplot(housingdataset, aes(factor(propertylandusetypeid), log(structuretaxvaluedollarcnt)))+geom_boxplot()+ggtitle("Housing Type")


######Model estimation for house taxes########
library(caret)
library(car)
set.seed(0)
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]

#Linear model with Box Cox transformation
#Model estimation normal
model <- lm(structuretaxvaluedollarcnt~airconditioningtypeid + bathroomcnt + bedroomcnt + 
              calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt + 
              + propertylandusetypeid + unitcnt + regionidzip, data = train)
bc <- boxCox(model)
lambda = bc$x[which(bc$y == max(bc$y))]
lambda

#Model estimation with box coxs transformation 
structuretaxvaluedollarcnt.bc = (train$structuretaxvaluedollarcnt^lambda - 1)/lambda

model2 <- lm(structuretaxvaluedollarcnt.bc~airconditioningtypeid + bathroomcnt + bedroomcnt + 
              calculatedfinishedsquarefeet + heatingorsystemtypeid + poolcnt + yearbuilt  + 
              propertylandusetypeid + unitcnt+regionidzip , data = train)

colnames(train)

#Reduced for computational testing
train <- train[,c("airconditioningtypeid", "bathroomcnt", "bedroomcnt",
                           "calculatedfinishedsquarefeet", "heatingorsystemtypeid", "poolcnt", "yearbuilt", 
                           "unitcnt", "propertylandusetypeid","regionidzip")]

library(MASS)
model.empty = lm(structuretaxvaluedollarcnt.bc ~ 1, data = train)
model.full = lm(structuretaxvaluedollarcnt.bc ~ ., data = train)
scope = list(lower = formula(model.empty), upper = formula(model.full))
model3 = step(model2, scope, direction = "both", k = 2)

#Summary and conditions verification
library(lmtest)
summary(model)
vif(model)
BIC(model)
bptest(model)
bgtest(model)

summary(model2)
vif(model2)
BIC(model2)
bptest(model2)
bgtest(model2)

summary(model3)
vif(model3)
BIC(model3)
bptest(model3)
bgtest(model3)

#Correction for heteroskedasticity:
#White standard errors:
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

modelresultsHouseTaxes <- summary(model3, robust=T)
modelresultsHouseTaxes
#Model R2: 0.616

##### END MODEL BUILDING HOUSE TAXES ####

##### START MODEL BUILDING LAND TAXES #####

ggplot(housingdataset, aes(propertylandusetypeid, log(landtaxvaluedollarcnt)))+geom_boxplot()+ggtitle("propertylandusetypeid")

ggplot(housingdataset, aes(log(calculatedfinishedsquarefeet), log(landtaxvaluedollarcnt)))+geom_point()+ggtitle("calculatedfinishedsquarefeet")
cor.test(log(housingdataset$landtaxvaluedollarcnt), housingdataset$calculatedfinishedsquarefeet)
#Significant relationship 0.33

set.seed(0)
folds = createFolds(housingdataset$parcelid, 5)
test = housingdataset[folds[[1]], ]
train = housingdataset[-folds[[1]], ]


model4 <- lm(landtaxvaluedollarcnt ~ propertylandusetypeid + calculatedfinishedsquarefeet +
              regionidzip, data = train)

bc2 <- boxCox(model4)
lambda2 = bc$x[which(bc2$y == max(bc2$y))]
lambda2
landtaxvaluedollarcnt.bc = (train$landtaxvaluedollarcnt^lambda - 1)/lambda

model5 <- lm(landtaxvaluedollarcnt.bc ~ propertylandusetypeid + calculatedfinishedsquarefeet +
               regionidzip, data = train)

summary(model4)
vif(model4)
BIC(model4)
bptest(model4)
bgtest(model4)

summary(model5)
vif(model5)
BIC(model5)
bptest(model5)
bgtest(model5)

modelresultsLandTaxes <- summary(model5, robust=T)
modelresultsLandTaxes
#R2 of 0.4254


t.test(train$structuretaxvaluedollarcnt, train$landtaxvaluedollarcnt)
ggplot(train)+geom_density(aes(x=structuretaxvaluedollarcnt), fill='red')+xlim(c(0,2e6)) +
  geom_density(aes(x=landtaxvaluedollarcnt), fill='green')+xlim(c(0,2e6))

summary(train$structuretaxvaluedollarcnt)
summary(train$landtaxvaluedollarcnt)
