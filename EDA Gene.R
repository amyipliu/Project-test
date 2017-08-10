
# Zillow project 
# step1: EDA on all property
library(dplyr)
library(data.table)


# load data =================================================================================
train = read.csv('./data/train_2016_v2.csv')
prop = read.csv('./data/properties_2016.csv')
sample = read.csv('./data/sample_submission.csv')


#library(data.table)
#prop1 = fread('./data/properties_2016.csv')

# fireplaceflag' to 'propertyzoningdesc'

# =========================================================================================
part = c('parcelid',
'fireplaceflag',
'fullbathcnt',
'garagecarcnt',
'garagetotalsqft',
'hashottuborspa',
'heatingorsystemtypeid',
'latitude',
'longitude',
'lotsizesquarefeet',
'numberofstories',
'poolcnt',
'poolsizesum',
'pooltypeid10',
'pooltypeid2',
'pooltypeid7',
'propertycountylandusecode',
'propertylandusetypeid',
'propertyzoningdesc',
'structuretaxvaluedollarcnt')


prop_part = subset(prop[,part])

save(prop_part , file = './data/prop_part_gene.Rda')
# load('./data/prop_part_gene.Rda')


# convert blank to NA
prop_part[prop_part==''] = NA

# check the shape of dataset
dim(prop_part)
length(unique(prop_part$parcelid)) # there are some duplicate

# checking the missing using sapply()
sapply(prop_part, function(x) sum(is.na(x)))
colSums(is.na(prop_part)) 

# checking the unique value for each columns
sapply(prop_part, function(x) length(unique(x)))

# look at subset of data without missing data
names(prop_part)

### subset again

part2 = c(
         'fullbathcnt',
         'lotsizesquarefeet',
         'heatingorsystemtypeid', 
         'propertycountylandusecode',
         'propertylandusetypeid',
         'propertyzoningdesc',
         'structuretaxvaluedollarcnt')
    
prop_part2 = subset(prop_part[,part2])

# some imputation
summary(prop_part2)
sapply(prop_part2, class)



# propertyzoningdesc NA to 'OTHER'
prop_part2$propertyzoningdesc = as.character(prop_part2$propertyzoningdesc)
prop_part2$propertyzoningdesc[is.na(prop_part2$propertyzoningdesc)] = 'OTHER'
prop_part2$propertyzoningdesc = as.factor(prop_part2$propertyzoningdesc)
# impute other columns

prop_part2[is.na(prop_part2)] = 0

summary(prop_part2)
save(prop_part2 , file = './data/prop_part_gene_imputed.Rda')

#=========================================================================
library(ggplot2)
library(gridExtra)
library(tabplot)
library(lsr)
library(corrplot)
library(dplyr)

# rm(list = ls())


# load the cleaned data
df_house <- fread("/Users/wanggene/Downloads/zillow/housingdata.csv")
df_land <- fread("/Users/wanggene/Downloads/zillow/landdataset.csv")
df_house = data.frame(df_house)
df_house_part = df_house[,part]

df_land = data.frame(df_land)

rm(df_house)

# EDA the cleaned data ==============================================================
# barplot
ggplot(df_house_part, aes(x=fireplaceflag)) + geom_bar()
ggplot(df_house_part, aes(x=fullbathcnt)) + geom_bar()
ggplot(df_house_part, aes(x=garagecarcnt)) + geom_bar()
ggplot(df_house_part, aes(x=heatingorsystemtypeid)) + geom_bar()
ggplot(df_house_part, aes(x=lotsizesquarefeet)) + geom_bar()
ggplot(df_house_part, aes(x=poolcnt)) + geom_bar()


# ==============================================================
grid.arrange(ggplot(df_house_part) + 
                 geom_histogram(aes(structuretaxvaluedollarcnt), bins = 100), 
             ggplot(df_house_part) + 
                 geom_histogram(aes(log(structuretaxvaluedollarcnt + 1)), bins = 100), 
             ncol = 2)



grid.arrange(ggplot(df_land) + 
                 geom_histogram(aes(landtaxvaluedollarcnt), bins = 100), 
             ggplot(df_land) + 
                 geom_histogram(aes(log(landtaxvaluedollarcnt + 1)), bins = 100), 
             ncol = 2)



# tax_mean by group
# fireplaceflag
df_house_part %>% group_by(fireplaceflag) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=fireplaceflag, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('fireplaceflag')

df_house_part %>%
    ggplot() + 
    geom_histogram(aes(x=log(structuretaxvaluedollarcnt), 
                       fill = as.factor(fireplaceflag)), bins =100) + 
    ggtitle('fireplaceflag')
    
# fullbathcnt
df_house_part %>% group_by(fullbathcnt) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=as.factor(fullbathcnt), y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('fullbathcnt')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                       color = as.factor(fullbathcnt))) + 
    facet_wrap( ~ fullbathcnt, ncol = 7) +
    ggtitle('fullbathcnt2') 

# garagecarcnt
df_house_part %>% group_by(garagecarcnt) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=as.factor(garagecarcnt), y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('garagecarcnt')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(garagecarcnt))) + 
    facet_wrap( ~ garagecarcnt, ncol = 6) +
    ggtitle('garagecarcnt2') 


# garagetotalsqft ******************************
df_house_part %>% group_by(garagetotalsqft) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=garagetotalsqft, y=mean_tax)) +
    geom_point() +
    ggtitle('garagetotalsqft')



# hashottuborspa 
df_house_part %>% group_by(hashottuborspa) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=hashottuborspa, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('hashottuborspa')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(hashottuborspa))) + 
    #facet_wrap( ~ hashottuborspa) +
    ggtitle('hashottuborspa2') 



# heatingorsystemtypeid
df_house_part %>% group_by(heatingorsystemtypeid) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=heatingorsystemtypeid, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('heatingorsystemtypeid')


# lotsizesquarefeet ????????????? check next with log
df_house_part %>% group_by(lotsizesquarefeet) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x= lotsizesquarefeet, y=mean_tax)) +
    geom_point() +
    ggtitle('lotsizesquarefeet') 

df_house_part %>% group_by(lotsizesquarefeet) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x= log(lotsizesquarefeet), y=mean_tax)) +
    geom_point() +
    ggtitle('lotsizesquarefeet_log2') +
    ylim(0,4e+7)
    

# numberofstories
df_house_part %>% group_by(numberofstories) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=as.factor(numberofstories), y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('numberofstories')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(numberofstories))) + 
    facet_wrap( ~ numberofstories, ncol=4) +
    ggtitle('numberofstories2') 



# poolcnt
df_house_part %>% group_by(poolcnt) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=poolcnt, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('poolcnt')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(poolcnt))) + 
    ggtitle('poolcnt2') 


# poolsizesum
df_house_part %>% 
    group_by(poolsizesum) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=log(poolsizesum), y=log(mean_tax))) +
    geom_point() +
    ggtitle('poolsizesum')


# pooltypeid10
df_house_part %>% group_by(pooltypeid10) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=pooltypeid10, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('pooltypeid10')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(pooltypeid10))) + 
    ggtitle('pooltypeid102') 




# pooltypeid2
df_house_part %>% group_by(pooltypeid2) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=pooltypeid2, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('pooltypeid2')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(pooltypeid2))) + 
    ggtitle('pooltypeid22') 


# pooltypeid7
df_house_part %>% group_by(pooltypeid7) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=pooltypeid7, y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('pooltypeid7')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(pooltypeid7))) + 
    ggtitle('pooltypeid72') 



# propertycountylandusecode
df_house_part %>% group_by(propertycountylandusecode) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=as.factor(propertycountylandusecode), y=log(mean_tax))) +
    geom_bar(stat = 'identity') +
    ggtitle('propertycountylandusecode2')



# propertylandusetypeid 
df_house_part %>% group_by(propertylandusetypeid) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=as.factor(propertylandusetypeid), y=mean_tax)) +
    geom_bar(stat = 'identity') +
    ggtitle('propertylandusetypeid')

df_house_part %>%
    ggplot() + 
    geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                     color = as.factor(propertylandusetypeid))) + 
    facet_wrap( ~ propertylandusetypeid, ncol = 5) +
    ggtitle('propertylandusetypeid2') 


# propertyzoningdesc
df_house_part %>% group_by(propertyzoningdesc) %>% 
    summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
    ggplot(aes(x=propertyzoningdesc, y=log(mean_tax))) +
    geom_point() +
    ggtitle('propertyzoningdesc')



# Matrix plot ============================================================

colMtx <- matrix(names(df_house_part)[1:length(df_house_part)] , nrow = 4)
for (i in 1:ncol(colMtx)) {
    tableplot(df_house_part, 
              select_string = c(colMtx[,i], "structuretaxvaluedollarcnt"), 
              sortCol = "structuretaxvaluedollarcnt", decreasing = TRUE, 
              nBins = 30)
}


# Numberic variable correlation  ========================================

numeric_features <- names(df_house_part)[sapply(df_house_part, is.numeric)]
numeric_features <- numeric_features[-length(numeric_features)]
print(numeric_features)


## correlation between continuous variables  - pearson
corr_numtran <- cor(df_house_part %>% 
                        select(one_of(numeric_features, "structuretaxvaluedollarcnt")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corr_numtran, method = "color", order="hclust")


# ====================================================================



