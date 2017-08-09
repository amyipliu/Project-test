

setwd('/NYCDA/Assigment/Project 3/datasets/')
library(data.table)
pro = fread('properties_2016.csv')

part = c("parcelid",'airconditioningtypeid',
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
         'fireplacecnt')

p_part = pro[,part, with = FALSE]


## Transfer blank to NA
p_part[p_part==""] = NA

## Double Check
dim(p_part)

## Checking NA rows
na = sapply(p_part, function(x) sum(is.na(x)))
colSums(is.na(p_part))

notna = sapply(p_part, function(x) sum(is.na(x) == FALSE))
summary(p_part)

##checking the unique value for each columns
uni = sapply(p_part, function(x) length(unique(x)))


########################
## Imputation ##########
########################

# 1'airconditioningtypeid'
p_part$airconditioningtypeid[is.na(p_part$airconditioningtypeid)] = 0

# 2architecturalstyletypeid
p_part$architecturalstyletypeid[is.na(p_part$architecturalstyletypeid)]=0

#3 basementsqft
p_part$basementsqft[is.na(p_part$basementsqft)] = 0
# Friend works in industry: most of house he saw are non basement

#4bathroomcnv ???
p_part$bathroomcnt

#5 bedroomcnt
p_part$bedroomcnt[is.na(p_part$bedroomcnt)] = 0

#6 buildinggualitytypedid -> year?
p_part$buildingqualitytypeid[is.na(p_part$buildingqualitytypeid)] = 0

#7 buildingclasstypeid - construction material
p_part$buildingclasstypeid[is.na(p_part$buildingclasstypeid)] =0

#8 Calculated total finished living area of the home 
p_part$calculatedbathnbr[is.na(p_part$calculatedbathnbr)] = 0


#9 Type of deck (if any) present on parcel 
p_part$decktypeid[is.na(p_part$decktypeid)] = 0

#10 Number of 3/4 bathrooms in house (shower + sink + toilet)
p_part$threequarterbathnbr[is.na(p_part$threequarterbathnbr)] = 0

#11  Size of the finished living area on the first (entry) floor of the home
p_part$finishedfloor1squarefeet[is.na(p_part$finishedfloor1squarefeet)] = 0

#12 Calculated total finished living area of the home 
p_part$calculatedfinishedsquarefeet[is.na(p_part$calculatedfinishedsquarefeet)] = 0


#13 Base unfinished and finished area (only one data)
p_part$finishedsquarefeet6[is.na(p_part$finishedsquarefeet6)] = 0

#14 Finished living area
p_part$finishedsquarefeet12[is.na(p_part$finishedsquarefeet12)] =0

#15 Total Area (TBC)
p_part$finishedsquarefeet15[is.na(p_part$finishedsquarefeet15)] = 0

# 16 Size of the finished living area on the first (entry) floor of the home
p_part$finishedsquarefeet50[is.na(p_part$finishedsquarefeet50)] = 0

# 17 Federal Information Processing Standard code
# *Mark, this data is from conty code, can be modified if conty code be adjuested
library(dplyr)
pro %>% select(., fips, regionidcounty) %>%
  group_by(., fips, regionidcounty) %>% summarise( count = n())

p_part$fips[is.na(p_part$fips)] = 0

#18  Is a fireplace present in this home 
p_part$fireplacecnt[is.na(p_part$fireplacecnt)] =  0




