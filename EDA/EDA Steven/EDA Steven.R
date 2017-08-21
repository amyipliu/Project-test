library(readr)
library(ggplot2)
library(tabplot)
library(corrplot)
library(dplyr)

#Read in dataset
housingdata <- read_csv("C:/Users/Steven Jongerden/Desktop/housingdata.csv")

#Create tableplot with all the variables for landtaxvaluedollarcnt
colMtx <- matrix(names(housingdata)[1:length(housingdata)-1], nrow = 4)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdata, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "taxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create tableplot with all the variables for structuretaxvaluedollarcnt
colMtx <- matrix(names(housingdata)[1:length(housingdata)-1], nrow = 4)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdata, 
            select_string = c(colMtx[,i], "landtaxvaluedollarcnt"), 
            sortCol = "taxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}

#Create corplots with all the numeric variables for landtaxvaluedollarcnt
nummeric <- c("bedroomcnt","airconditioningtypeid", "bathroomcnt",
              "bedroomcnt", "calculatedfinishedsquarefeet", "finishedsquarefeet12",
              "fullbathcnt", "lotsizesquarefeet", "poolcnt", "unitcnt",
              "yearbuilt", "numberofstories")

corHousingLandTax <- cor(housingdata %>% 
                    select(one_of(nummeric, "landtaxvaluedollarcnt")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corHousingLandTax, method = "color", order="hclust")

#Create corplots with all the numeric variables for structuretaxvaluedollarcnt
corHousingHouseTax <- cor(housingdata %>% 
                    select(one_of(nummeric, "structuretaxvaluedollarcnt")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corHousingHouseTax, method = "color", order="hclust")


#Create corplots with all the categorical variables for landtaxvaluedollarcnt
ordinal_features <- c('airconditioningtypeid', 'buildingqualitytypeid',
                      'buildingclasstypeid', 'heatingorsystemtypeid',
                      'pooltypeid10', 'pooltypeid7', 'propertylandusetypeid',
                      'regionidzip')
corHousingLandTax2 <- cor(data.matrix(housingdata %>% 
                      select(one_of(ordinal_features, "landtaxvaluedollarcnt"))), 
                      method = "kendall", 
                      use = "pairwise.complete.obs")

corrplot(corHousingLandTax2, method = "color", order="hclust")

#Create corplots with all the numeric variables for structuretaxvaluedollarcnt

corHousingHouseTax2 <- cor(data.matrix(housingdata %>% 
                      select(one_of(ordinal_features, "structuretaxvaluedollarcnt"))), 
                      method = "kendall", 
                      use = "pairwise.complete.obs")

corrplot(corHousingHouseTax2, method = "color", order="hclust")

#Spearman Correlation for categorical features
cor.ordcnt <- function(data, x, y) {
  cor(as.numeric(data[[x]]), as.numeric(data[[y]]), 
      method = "spearman", 
      use = "pairwise.complete.obs")
}
  
corr_ordcnttran <- data.frame(Variable = ordinal_features,
                              Correlation = sapply(ordinal_features, 
                              function(x) -cor.ordcnt(housingdata, x, "structuretaxvaluedollarcnt")))

ggplot(corr_ordcnttran, aes(reorder(Variable, Correlation), Correlation)) + 
  geom_bar(stat = "identity") +  coord_flip()+ scale_fill_brewer(palette="Blues") +
  theme(panel.background = element_rect(fill="white")) + xlab("Categorical Variables")

corr_ordcnttran <- data.frame(Variable = ordinal_features,
                              Correlation = sapply(ordinal_features, 
                                                   function(x) -cor.ordcnt(housingdata, x, "landtaxvaluedollarcnt")))

ggplot(corr_ordcnttran, aes(reorder(Variable, Correlation), Correlation)) + 
  geom_bar(stat = "identity") +  coord_flip()+ scale_fill_brewer(palette="Blues") +
  theme(panel.background = element_rect(fill="white")) + xlab("Categorical Variables")

#Pearson Correlation for numerical variables 
cor.numm <- function(data, x, y) {
  cor(as.numeric(data[[x]]), as.numeric(data[[y]]), 
      method = "pearson", 
      use = "pairwise.complete.obs")
}

corr_ordcnttran <- data.frame(Variable = nummeric,
                              Correlation = sapply(nummeric, 
                              function(x) -cor.numm(housingdata, x, "structuretaxvaluedollarcnt")))

ggplot(corr_ordcnttran, aes(reorder(Variable, Correlation), Correlation)) + 
  geom_bar(stat = "identity") +  coord_flip() + scale_fill_brewer(palette="Blues") +
  theme(panel.background = element_rect(fill="white")) + xlab("Numerrical Variables")

corr_ordcnttran <- data.frame(Variable = nummeric, Correlation = sapply(nummeric, 
                   function(x) -cor.numm(housingdata, x, "landtaxvaluedollarcnt")))

ggplot(corr_ordcnttran, aes(reorder(Variable, Correlation), Correlation)) + 
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette="Blues") +
  theme(panel.background = element_rect(fill="white")) + xlab("Numerrical Variables")
