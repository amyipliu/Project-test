library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tabplot)
library(corrplot)

setwd('/NYCDA/Assigment/Project 3/datasets/')


amypart = read.csv('amy_part.csv') # Or parts from data cleaning code

###################################
#########  EDA ####################
###################################


### Bar Plot for checking variables' distribution  

ggplot(housingdataset, aes(x=airconditioningtypeid)) + geom_bar()


## Bathroom Count
ggplot(housingdataset, aes(x=bathroomcnt)) + geom_bar()
housingdataset %>% filter(., bathroomcnt != 0) %>%
  select(., bathroomcnt) %>%
  ggplot(aes(x=bathroomcnt)) + geom_histogram(bins = 20) + xlab('Bathroom')



## Bed Room 
ggplot(housingdataset, aes(x=bedroomcnt)) + geom_bar()
housingdataset %>% filter(., bedroomcnt != 0) %>%
  select(., bedroomcnt) %>%
  ggplot(aes(x=bedroomcnt)) + geom_histogram(bins = 20) + xlab('Bedroom')




## Building Class
ggplot(housingdataset, aes(x=buildingclasstypeid)) + geom_bar()
# The graph show huge portion on 0, thus, checking numbers of each category:
housingdataset %>%
  select(., buildingclasstypeid) %>% group_by(., buildingclasstypeid) %>%
  summarise(., Count = n()) %>% filter(., buildingclasstypeid != 0) %>%
  ggplot(., aes(x=buildingclasstypeid, y=Count)) + geom_col()+
  scale_fill_discrete()


#buildingclasstypeid   Count
#<fctr>   <int>
# 1                   0 1962401
# 2                   1      63
# 3                   2      80
# 4                   3    3123
# 5                   4    9174
# 6                   5      57

ggplot(housingdataset, aes(x=buildingqualitytypeid)) + geom_bar()
#Overall assessment of condition of the building from best (lowest) to worst (highest)



## Caculated Bathroom
ggplot(housingdataset, aes(x=calculatedbathnbr)) + geom_bar()
housingdataset %>% filter(., bathroomcnt != 0) %>%
  select(., calculatedbathnbr) %>%
  ggplot(aes(x=calculatedbathnbr)) + geom_histogram(bins = 20) + xlab('Bathroom')




## Calculated total finished living area of the home **
ggplot(housingdataset, aes(x=calculatedfinishedsquarefeet)) + geom_bar() 
housingdataset %>% filter(., calculatedfinishedsquarefeet != 0) %>%
  select(., calculatedfinishedsquarefeet) %>%
  ggplot(aes(x= log(calculatedfinishedsquarefeet))) + geom_histogram(bins = 100) + xlab('Log Calculated total finished living area of the home')




## Finishedsquarefeet12 - Finished living area **
ggplot(housingdataset, aes(x=finishedsquarefeet12)) +???geom_bar() 
#The livin area range too huge, try histogram
ggplot(housingdataset, aes(x=finishedsquarefeet12)) + geom_histogram(bins = 130)
boxplot(housingdataset$finishedsquarefeet12)
# Nerrow range
housingdataset %>% filter(., finishedsquarefeet12 < 3000) %>%
  select(., finishedsquarefeet12) %>% boxplot(housingdataset$finishedsquarefeet12)




## Finishedsquarefeet15 - Total area **
ggplot(housingdataset, aes(x=finishedsquarefeet15)) + geom_histogram(bins = 30)
boxplot(housingdataset$finishedsquarefeet15)
#narrow down range
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0      0.0      0.0    261.4      0.0 820242.0 
boxplot(housingdataset$finishedsquarefeet15 <1000)
#Total Area Log
housingdataset %>% filter(., finishedsquarefeet15 != 0) %>%
  select(., finishedsquarefeet15) %>%
  ggplot(aes(x=log(finishedsquarefeet15))) + geom_histogram(bins = 100) + xlab('Total Area')
#Total Area Under 1000
housingdataset %>% filter(., finishedsquarefeet15 < 1000 & finishedsquarefeet15 != 0) %>%
  select(., finishedsquarefeet15) %>%
  ggplot(aes(x=finishedsquarefeet15)) + geom_histogram(bins = 50) + xlab('Total Area')



## finishedsquarefeet13 - Perimeter  living area # not in housing data
amypart$finishedsquarefeet13[is.na(amypart$finishedsquarefeet13)] = 0
amypart %>% filter(., finishedsquarefeet13 != 0) %>%
  select(., finishedsquarefeet13) %>%
  ggplot(aes(x=finishedsquarefeet13)) + geom_histogram(bins = 100) + xlab('Perimeter Living Area')


#-----------------------------------------------------------------------
# Scatter between variables and tax or others



### Airconditioningtypeid - Tax (mean) -------------

# AC_TAXmean Bar
housingdataset %>% group_by(airconditioningtypeid) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(airconditioningtypeid), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('airconditiontype_taxmean')

# AC_Tax Dencity **
housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(airconditioningtypeid))) + 
    ggtitle('airconditiontype_tax') 

### Bathroomcnt _ pTax ---------------------------------

# Bathroomcnt_Tax Mean
housingdataset %>% group_by(bathroomcnt) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(bathroomcnt), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('bathroomcnt_taxmean')

# Bathroomcnt_Tax 

housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(bathroomcnt))) + 
  facet_wrap( ~ bathroomcnt, ncol = 5) +
  ggtitle('bathroomcnt_tax')


### bedroomcnt _tax -----------------------------------------

# Bedroomcnt_Tax Mean
housingdataset %>% group_by(bedroomcnt) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(bedroomcnt), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('bedroomcnt_taxmean')

# Bedroomcnt_Tax 

housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(bedroomcnt))) + 
  facet_wrap( ~ bedroomcnt, ncol = 5) +
  ggtitle('bedroomcnt_tax')



### Buildingclasstypeid _ tax -------------------------------


# Buildingclasstypeid_Tax Mean (The building framing type (steel frame, wood frame, concrete/brick) )
housingdataset %>% group_by(buildingclasstypeid) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(buildingclasstypeid), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('buildingclassid_taxmean')

# Buildingclasstypeid_Tax 

housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(buildingclasstypeid))) + 
  ggtitle('buildingclassid_tax') 


### Buildingqualitytypeid _ tax (1-best)-------------------------------

# Overall assessment of condition of the building from best (lowest) to worst (highest)

# Buildingqualitytypeid_Tax Mean 
housingdataset %>% group_by(buildingqualitytypeid) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(buildingqualitytypeid), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('buildingqualitytypeid_taxmean')

# Buildingqualitytypeid_Tax 

housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(buildingqualitytypeid))) + 
  facet_wrap( ~ buildingqualitytypeid, ncol = 6) +
  ggtitle('buildingqualitytypeid_tax') 

# Buildingqualitytypeid _  year _scatter

housingdataset %>%
  group_by(., buildingqualitytypeid, yearbuilt) %>%
  filter(., buildingqualitytypeid != 0) %>%
  summarise(., count = n()) %>%
  ggplot()+
  geom_jitter(aes(x = yearbuilt, y = buildingqualitytypeid, color = as.character(buildingqualitytypeid))) +
  ggtitle('Buildingqualitytypeid _ year')
  
# Buildingqualitytypeid _  year _line
  
housingdataset %>%
  group_by(., buildingqualitytypeid, yearbuilt) %>%
  filter(., buildingqualitytypeid != 0) %>%
  summarise(., count = n()) %>%
  ggplot()+
  geom_line(aes(x = yearbuilt, y = count, color = as.character(buildingqualitytypeid))) +
  ggtitle('Buildingqualitytypeid _ year_line')



# Calculatedbathnbr _ Tax -------------------------------------------

# Calculatedbathnbr_Tax Mean 
housingdataset %>% group_by(calculatedbathnbr) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x=as.factor(calculatedbathnbr), y=mean_tax)) +
  geom_bar(stat = 'identity') +
  ggtitle('Calculatedbathnbr_taxmean')

# Buildingqualitytypeid_Tax 

housingdataset %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(calculatedbathnbr))) + 
  facet_wrap( ~ calculatedbathnbr, ncol = 6) +
  ggtitle('calculatedbathnbr_tax') 


### Calculatedfinishedsquarefeet _ tax (1-best)-------------------------------

#1
housingdataset %>% group_by(calculatedfinishedsquarefeet) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(calculatedfinishedsquarefeet), y=mean_tax)) +
  geom_point() +
  ggtitle('calculatedfinishedsquarefeet_meantax') +
  ylim(0,4e+7)


#Narrow
housingdataset %>% group_by(calculatedfinishedsquarefeet) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(calculatedfinishedsquarefeet), y=mean_tax)) +
  geom_point() +
  ggtitle('calculatedfinishedsquarefeet_meantax') +
  ylim(0,4e+7)


### Finished living area _ Tax ** -------------------------------------

# finishedsquarefeet12_tax - Finished living area

housingdataset %>% group_by(finishedsquarefeet12) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet12), y=mean_tax)) +
  geom_point() +
  ggtitle('finishinglivingarea_meantax') 


# Try to Narrow down tax range 
housingdataset %>% group_by(finishedsquarefeet12) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet12), y=mean_tax)) +
  geom_point() +
  ggtitle('finishedlivingarea_meantax')+
  ylim(0,4e+7)


### Total Area _ Tax **  -------------------------------------------------

#finishedsquarefeet15

housingdataset %>% group_by(finishedsquarefeet15) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet15), y=mean_tax)) +
  geom_point() +
  ggtitle('totalarea_meantax')


#Narrwo down tax range to 1.5e+08

housingdataset %>% group_by(finishedsquarefeet15) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet15), y=mean_tax)) +
  geom_point() +
  ggtitle('totalarea_meantax') +
  ylim(0,1.5e+08)


#Narrow down tax range to 5e+07

housingdataset %>% group_by(finishedsquarefeet15) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet15), y=mean_tax)) +
  geom_point() +
  ggtitle('totalarea_meantax') +
  ylim(0,5e+07)


#Narrow down tax range to 1e+07

housingdataset %>% group_by(finishedsquarefeet15) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= log(finishedsquarefeet15), y=mean_tax)) +
  geom_point() +
  ggtitle('totalarea_meantax') +
  ylim(0,1e+07)


### Perimeter  living area _ Tax(from old data) -------------------------------------------------

# structuretaxvalue - too less information (only 251 levels)
amypart %>% group_by(finishedsquarefeet13) %>% 
  summarise(mean_tax = mean(structuretaxvaluedollarcnt)) %>% 
  ggplot(aes(x= finishedsquarefeet13, y=mean_tax)) +
  geom_point() +
  ggtitle('perimeterlivingarea_meantax')


#landtaxvaluedollarcnt

#remove na
amypart$landtaxvaluedollarcnt[is.na(amypart$landtaxvaluedollarcnt)] = 0
#plot
amypart %>% group_by(finishedsquarefeet13) %>%
  filter(., finishedsquarefeet13 != 0) %>%
  summarise(mean_tax = mean(landtaxvaluedollarcnt)) %>% 
  ggplot(aes(x= finishedsquarefeet13, y=mean_tax)) +
  geom_point() +
  ggtitle('perimeterlivingarea_landtax')


### Fireplacement _ tax ---------------------------------
amypart %>%
  ggplot() + 
  geom_density(aes(x=log(structuretaxvaluedollarcnt), 
                   color = as.factor(fireplacecnt))) + 
  facet_wrap(~ fireplacecnt) +  
  ggtitle('fireplacement_tax')





####### MATRIX PLOT #######################################

colMtx <- matrix(names(housingdataset)[2:10] , nrow = 4)
for (i in 1:ncol(colMtx)) {
  tableplot(housingdataset, 
            select_string = c(colMtx[,i], "structuretaxvaluedollarcnt"), 
            sortCol = "structuretaxvaluedollarcnt", decreasing = TRUE, 
            nBins = 30)
}




###### Correlation Plots ################################################
#1. Correlation between incharged parts
#2. Correlation between incharged parts and taxs



#1. Correlation between incharged parts
### Numberic variable correlation  ########################

numeric <- names(amypart)[3:20][sapply(amypart, is.numeric)]
numeric <- numeric[-length(numeric)]
print(numeric)


## correlation between continuous variables  - pearson
corr_num <- cor(amypart %>% 
                      select(one_of(numeric, "structuretaxvaluedollarcnt")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corr_num, method = "color", order="hclust")



#2. Correlation between incharged parts and taxs
#### Correlation between parts and taxs (from old data)

## involve taxs 
part2 = c("parcelid",'airconditioningtypeid',
                 'architecturalstyletypeid',
                 'basementsqft',
                 'bathroomcnt',
                 'bedroomcnt',
                 'buildingqualitytypeid',
                 'buildingclasstypeid',
                 'calculatedbathnbr',
                 'decktypeid',
                 'threequarterbathnbr',
                 'finishedfloor1squarefeet',
                 'calculatedfinishedsquarefeet',
                 'finishedsquarefeet6',
                 'finishedsquarefeet12',
                 'finishedsquarefeet13',
                 'finishedsquarefeet15',
                 'finishedsquarefeet50',
                 'fips',
                 'fireplacecnt',
                 'yearbuilt',
                 'taxvaluedollarcnt',
                 'structuretaxvaluedollarcnt',
                 'landtaxvaluedollarcnt')

amypart2 = amypart[,part2]


#1. Correlation between incharged parts
### Numberic variable correlation  ########################

numeric <- names(amypart2)[sapply(amypart2, is.numeric)]
numeric <- numeric[-length(numeric)]
print(numeric)


## correlation between continuous variables  - pearson
corr_num <- cor(amypart2 %>% 
                  select(one_of(numeric, "structuretaxvaluedollarcnt")), 
                method = "pearson", 
                use = "pairwise.complete.obs")

corrplot(corr_num, method = "color", order="hclust")

