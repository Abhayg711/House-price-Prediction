#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(tidyverse)

#install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#install.packages("corrplot")
library(corrplot)

## Loading dataset
train_housing <- read.csv("D:/Projects/House Pricing data/train.csv")
test_housing <- read.csv("D:/Projects/House Pricing data/test.csv")

## Removing duplicates
train_housing <- unique(train_housing)
test_housing <- unique(test_housing)

## Creating duplicate of dataset
train <- train_housing
test <- test_housing

test_labels <- test$Id

train$Id <- NULL
test$Id <- NULL

test$SalePrice <- NA
## Combining test and train dataset
all <- rbind(train, test)
dim(all)

combined <- all

## Checking the skewness in the sales price of houses
ggplot(all, aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000)) #We observe that our data is right skewed

summary(all$SalePrice)

### Correlation with SalePrice
numericVars <- which(sapply(all, is.numeric)) #index vectors which are numeric 
numericName <- names(numericVars) #saving names of variables which are numeric
cat('There are', length(numericVars), 'numeric variables')

all_numVars <- all[, numericVars]
cor_numVars <- cor(all_numVars, use = "pairwise.complete.obs") #correlations of all numeric variables
##pairwise.complete.obs computes the correlation or covariance between each pair of variables
## Covariance - Extent to which two random variables change in tandem
## Correlation - Identicates how strongly two random variables are related


# Sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVars[, 'SalePrice'], decreasing = TRUE))

# Selected only highly correlated variables
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)> 0.5)))
cor_numVars <- cor_numVars[CorHigh, CorHigh]

corrplot.mixed(cor_numVars, tl.col = "black", tl.pos = "lt")

### Overall Quality
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(col = 'blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks = seq(0,800000, by = 100000)) #We see positive correlation between SalePrice and Overall Quality

### Greater Living Area
#install.packages("ggrepel")
library(ggrepel)

ggplot(data = all[!is.na(all$SalePrice), ], aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = 'blue') + geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = )) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000)) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))
  #geom_text_repel(aes(label = ifelse))

all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')] 
#To observe other features of thes IDs before deciding whether to include 
#it as part of outlier or not



### Missing Value Treatment
#### Completeness of data
NACol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)

cat('There are', length(NACol), 'which have missing values')

#### Imputing missing values
## PoolQC

all$PoolQC <- as.character(all$PoolQC) #Converting fcator to character

all$PoolQC[is.na(all$PoolQC)] <- 'None'

##PoolQC contains Ex - Excellent, Fa - Fair etc as ordinal variables
##Therefore, will assign values to each variables to make it numerical as they 
##are ordinal in nature

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) #Assigning values to PoolQC variables

# Assigning created numerical values to PoolQC 
library(plyr)

all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

## Checking pool area when poolQC is 0
all[all$PoolArea > 0 & all$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]

##Impute values of above ID's where we have PoolArea > 0 and PoolQC = 0 based on Overall QUality
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2

## Miscellaneous Features
all$MiscFeature <- as.character(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)] <- "None"

all$MiscFeature <- as.factor(all$MiscFeature)

#As MisFeature is not a ordinal variable. Therefore, converting it into factor

ggplot(all[!is.na(all$SalePrice), ], aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000))


## Alley
#As Alley is not a ordinal variable. Therefore, converting it into a factor
all$Alley <- as.character(all$Alley)
all$Alley[is.na(all$Alley)] <- "None"

all$Alley <- as.factor(all$Alley)
unique(all$Alley)

ggplot(all[!is.na(all$SalePrice), ], aes(x = Alley, y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000))

## Fence

unique(all$Fence)
#We see that fence has some level therefore it should be ordinal

class(all$Fence)
all$Fence <- as.character(all$Fence)
all$Fence[is.na(all$Fence)] <- "None"

table(all$Fence)

#Checking saleprice with fence
detach(package:plyr) #As below function was giving summary instead of groupby
library(dplyr)
all[!is.na(all$SalePrice), ]%>%
  group_by(Fence)%>%
  summarise(Avg_Price = mean(SalePrice), Median_Price = median(SalePrice), counts =n())

#As we can price of no-fence house is max. therefore, we will go ahead with fence as nominal
#hence take them as factor

all$Fence <- as.factor(all$Fence)

## FireplaceQu
head(all$FireplaceQu)

#We see that fireplace Quality has order therefore we are going to treat this as
#ordinal and replace the values same as PoolQC
table(all$FireplaceQu)

combined <- all

class(combined$FireplaceQu)
combined$FireplaceQu <- as.character(combined$FireplaceQu) #COnverting factor to character
all$FireplaceQu <- as.character(all$FireplaceQu)

all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"

unique(all$FireplaceQu)

library(plyr)
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu, Qualities))

detach(package:plyr)

table(all$FireplaceQu)

## LotFrontage'- Linear feet of street connected to property
unique(all$LotFrontage) 

#Let's plot cost and lotfrontage to understand the correlation
class(all$Neighborhood)

ggplot(all[!is.na(all$LotFrontage), ], aes(x = as.factor(Neighborhood), y = LotFrontage)) +
  geom_bar(stat = "summary", fun.y = "median", color = "blue") +
  scale_y_continuous(breaks = seq(0,800000, by = 100000))

# plot(all$LotFrontage, all$SalePrice, data = all)
# with(all, plot(all$LotFrontage, all$SalePrice, data = all))
# linearMod <- lm(SalePrice ~ LotFrontage, data = all)
# summary(linearMod)
# abline(linearMod)

#We can replace every missing value by median value of neighborhood
for(i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood == 
                                                              all$Neighborhood[i]], na.rm = TRUE))
  }
}

## Other Lot Variables
unique(all$LotArea) #We won't make any changes to LotArea as it is already numeric
class(all$LotArea)

### LotConfig
unique(all$LotConfig)
class(all$LotConfig)

ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(LotConfig), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "black", color = "red") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000)) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

table(all$LotConfig)

ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(LotConfig), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "black", color = "red") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000)) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

#We can see any order therefore we will convert this into factor
all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)

## LotShape
unique(all$LotShape)

#As description tells us that Reg: Regular and IR:iiregular therefore we will
#give values to each of them also there is no NA

LotShape_Variable <- c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)

library(plyr)
class(all$LotShape)

all$LotShape <- as.character(all$LotShape)
all$LotShape <- as.integer(revalue(all$LotShape, LotShape_Variable))
unique(all$LotShape)

table(all$LotShape)

detach(package: plyr)

### Garage Variables
## GarageYrBlt

temp_view <- all[is.na(all$GarageYrBlt),]
temp_view <- temp_view[ ,c("GarageYrBlt", "YearBuilt", "YearRemodAdd", "GarageType", "GarageFinish", "GarageCars", "GarageArea", "GarageQual", "GarageCond")]

#Replacing GarageYrBlt with YearBuilt column
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

#From temp_view we see that two records have values in other garage variables
#but only one value has garage therefore there are total of 158 houses where we
#we don;t have garage

#We will replace that one record i.e. 2127 with mode of all garage variables
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

# #display "fixed" house
# install.packages("Percentile")
# library(percentile)
# kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])


#fixing 3 values for house 2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#check if NAs of the character variables are now all 158
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

##Garage Type
table(all$GarageType)
sum(table(all$GarageType))

class(all$GarageType)

## Adding a level to garage type in order to change the NA to 'No garage'
levels_garagetype <- levels(all$GarageType)
levels_garagetype[length(levels_garagetype) + 1] <- 'No Garage'

all$GarageType <- factor(all$GarageType, levels = levels_garagetype)
all$GarageType[is.na(all$GarageType)] <- 'No Garage'

table(all$GarageType)
sum(table(all$GarageType))

all$GarageType <- as.factor(all$GarageType)


##GarageFinish
table(all$GarageFinish)
sum(table(all$GarageFinish))

## Implementing the above garagetype process to replace NA with No Garage
level_garagefinish <- levels(all$GarageFinish)
level_garagefinish[length(level_garagefinish) + 1] <- 'None'

all$GarageFinish <- factor(all$GarageFinish, levels = level_garagefinish)
all$GarageFinish[is.na(all$GarageFinish)] <-  'None'

table(all$GarageFinish)
sum(table(all$GarageFinish))

class(all$GarageFinish)

## As it is a ordinal variable we will replace below values
GarageFinish_variable <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

library(plyr)
class(all$GarageFinish)

all$GarageFinish <- as.character(all$GarageFinish)
all$GarageFinish <- as.integer(revalue(all$GarageFinish, GarageFinish_variable))
unique(all$GarageFinish)

table(all$GarageFinish)

detach(package: plyr)

##GarageQual
table(all$GarageQual)
sum(table(all$GarageQual))

class(all$GarageQual)

level_garagequal <- levels(all$GarageQual)
level_garagequal[length(level_garagequal) + 1] <- 'None'

all$GarageQual <- factor(all$GarageQual, levels = level_garagequal)
all$GarageQual[is.na(all$GarageQual)] <- 'None'

table(all$GarageQual)
sum(table(all$GarageQual))

class(all$GarageQual)

garagequal_variables <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

library(plyr)
class(all$GarageQual)

all$GarageQual <- as.character(all$GarageQual)
all$GarageQual <- as.integer(revalue(all$GarageQual, garagequal_variables))
table(all$GarageQual)
sum(table(all$GarageQual))

##Garage Condition
table(all$GarageCond)
sum(table(all$GarageCond))

level_garageCond <- levels(all$GarageCond)
level_garageCond[length(all$GarageCond) + 1] <- "None"

all$GarageCond <- factor(all$GarageCond, levels = level_garageCond)
all$GarageCond[is.na(all$GarageCond)] <- "None"

table(all$GarageCond)
sum(table(all$GarageCond))

class(all$GarageCond)

GarageCond_variables <- c('None' = 0, 'Po' = 1, 'TA' = 2, 'Fa' = 3, 'Gd' = 4, 'Ex' = 5)

all$GarageCond <- as.character(all$GarageCond)
all$GarageCond <- as.integer(revalue(all$GarageCond, GarageCond_variables))
table(all$GarageCond)
sum(table(all$GarageCond))

detach(package: plyr)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
#There are total 11 basement variables which have null values
#Now, we will check these 5 variables if they have common 79 null values

length(which(is.na(all$BsmtCond) & is.na(all$BsmtExposure) & is.na(all$BsmtQual) &
        is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

#nOW, finding additional records which are Na apart from 79 which are common
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtFinType2) | is.na(all$BsmtQual)
                                | is.na(all$BsmtExposure) | is.na(all$BsmtCond)),
    c('BsmtFinType1','BsmtFinType2','BsmtQual', 'BsmtExposure', 'BsmtCond')]

##Replacing the missing values with mode of the following column
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtQual[2218] <- names(sort(-table(all$BsmtQual)))[1]
all$BsmtQual[2219] <- names(sort(-table(all$BsmtQual)))[1]
all$BsmtExposure[949] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtExposure[1488] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtExposure[2349] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[2041] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtCond[2186] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtCond[2525] <- names(sort(-table(all$BsmtCond)))[1]

##BsmTQual
class(all$BsmtQual)

level_Bsmtqual <- levels(all$BsmtQual)
level_Bsmtqual[length(all$BsmtQual) + 1] <- 'None'

all$BsmtQual <- factor(all$BsmtQual, levels = level_Bsmtqual)
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'

class(all$BsmtQual)

library(plyr)
BsmtQual_variables <- c('None' = 0, 'Po' = 1, 'TA' = 2, 'Fa' = 3, 'Gd' = 4, 'Ex' = 5)

all$BsmtQual <- as.character(all$BsmtQual)
all$BsmtQual <- as.integer(revalue(all$BsmtQual, BsmtQual_variables))

table(all$BsmtQual)
sum(table(all$BsmtQual))

##BsmtExpousre
levels(all$BsmtExposure)

levels_BsmtExpo <- levels(all$BsmtExposure)
levels_BsmtExpo[length(all$BsmtExposure) + 1] <- 'None'

all$BsmtExposure <- factor(all$BsmtExposure, levels = levels_BsmtExpo)
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None' 

sum(table(all$BsmtExposure))

class(all$BsmtExposure)

library(plyr)
BsmtExpo_variable <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)  

all$BsmtExposure <- as.character(all$BsmtExposure)
all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, BsmtExpo_variable))

detach(package: plyr)

table(all$BsmtExposure)
sum(table(all$BsmtExposure))

##BsmtFinType1
levels(all$BsmtFinType1)

level_BsmtFinType1 <- levels(all$BsmtFinType1)
level_BsmtFinType1[length(all$BsmtFinType1) + 1] <- 'None'

all$BsmtFinType1 <- factor(all$BsmtFinType1, levels = level_BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'

sum(table(all$BsmtFinType1))

library(plyr)

BsmtFinType1_variable <- c('None' = 0, 'Unf' =  1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4,
                           'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType1 <- as.character(all$BsmtFinType1)
all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, BsmtFinType1_variable))

table(all$BsmtFinType1)
sum(table(all$BsmtFinType1))

detach(package: plyr)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
##BsmtFinType2
levels(all$BsmtFinType2)

level_BsmtFinType2 <- levels(all$BsmtFinType2)
level_BsmtFinType2[length(all$BsmtFinType2) + 1] <- 'None'

all$BsmtFinType2 <- factor(all$BsmtFinType2, levels = level_BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'

library(plyr)

BsmtFinType2_variable <- c('None' = 0, 'Unf' =  1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4,
                           'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType2 <- as.character(all$BsmtFinType2)
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, BsmtFinType2_variable))

table(all$BsmtFinType2)
sum(table(all$BsmtFinType2))

detach(package: plyr)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)

##BsmtCond
levels(all$BsmtCond)

level_BsmtCond <- levels(all$BsmtCond)
level_BsmtCond[length(all$BsmtCond) + 1] <- 'None'

all$BsmtCond <- factor(all$BsmtCond, levels = level_BsmtCond)
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'

levels(all$BsmtCond)

library(plyr)

BsmtCond_variable <- c('None' = 0, 'Po' =  1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4,
                           'Ex' = 5)

all$BsmtCond <- as.character(all$BsmtCond)
all$BsmtCond <- as.integer(revalue(all$BsmtCond, BsmtCond_variable))

table(all$BsmtCond)
sum(table(all$BsmtCond))

detach(package: plyr)

##Other Basement variables with 1 or 2 NA
all[is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) | is.na(all$BsmtFinSF1) |
      is.na(all$BsmtFinSF2) | is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF),
    c("BsmtFullBath", "BsmtHalfBath", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF")]

#Replace all these values by 0 as these cases don't have basement
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0


sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
## Masonary Variables
#Now, lets try to find the common cases where both MAson variables are NA

length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

#No, let's find additional cases where Mason variables are NA apart from these 23
all[!is.na(all$MasVnrArea) & is.na(all$MasVnrType), c('MasVnrType', 'MasVnrArea')]


#As there is only one such case we are going to replace it with the mode of that column
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[1]

detach(package: plyr)

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice), ]%>%group_by(MasVnrType)%>%summarise(Median_sale = median(SalePrice), counts=n())%>%
  arrange(Median_sale)

MasVnrType_variable <- c("BrkCmn" = 0, "None" = 0, "BrkFace" = 1, "Stone" = 2)

library(plyr)

combined <- all

class(all$MasVnrType)
levels(all$MasVnrType)

all$MasVnrType <- as.character(all$MasVnrType)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, MasVnrType_variable))

table(all$MasVnrType)
sum(table(all$MasVnrType))

detach(package: plyr)
##MasVnrArea
all[which(!is.na(all$SalePrice) & is.na(all$MasVnrArea)), ]%>%group_by(MasVnrArea)%>%summarise(Median = median(all$SalePrice))%>%
  arrange(Median)

all[is.na(all$MasVnrArea), c("MasVnrArea", "MasVnrType")]

all[is.na(all$MasVnrArea), c("MasVnrArea")] <- 0


sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
 
##MSZoning
testing_variable <- all$MSZoning[is.na(all$MSZoning), ]
testing_variable[, c("MSZoning", "SalePrice")]

#Replacing NA with mode value
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]


sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)

##KitchenQual
levels(all$KitchenQual)

level_kitchenQual <- levels(all$KitchenQual)
level_kitchenQual[length(all$KitchenQual) + 1] <- 'None'

all$KitchenQual <- factor(all$KitchenQual, levels = level_kitchenQual)
all$KitchenQual[is.na(all$KitchenQual)] <- 'None'

library(plyr)
kitchenqual_variable <- c('None' = 0, 'Po' =  1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4,
                          'Ex' = 5)

all$KitchenQual <- as.character(all$KitchenQual)
all$KitchenQual <- as.integer(revalue(all$KitchenQual, kitchenqual_variable))

detach(package: plyr)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
##Electrical - Categorical variable
levels(all$Electrical)

all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)

##Functional
str(all$Functional)

all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]

levels(all$Functional)
functional_variable <- c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4,
                         'Min2' = 5, 'Min1' = 6, 'Typ' = 7)

library(plyr)
class(all$Functional)

all$Functional <- as.character(all$Functional)
all$Functional <- as.integer(revalue(all$Functional, functional_variable))

table(all$Functional)
sum(table(all$Functional))

detach(package: plyr)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
##Exterior1st - Categorical variable
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]

table(all$Exterior1st)
sum(table(all$Exterior1st))

##Exterior2nd - Categorical variable
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

table(all$Exterior2nd)
sum(table(all$Exterior2nd))

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)
##SaleType
summary(all$SaleType)

all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

class(all$SaleType)

sort(colSums(sapply(all[NACol], is.na)), decreasing = TRUE)

##Utilies
table(all$Utilities)

all$Utilities <- NULL

############################################################################
######################## END OF MISSING VALUE IMPUTION #####################
############################################################################

## Label Encoding
str(all)

character_variables <- names(all[, sapply(all, class) == 'factor'])
character_variables

##Foundation
levels(all$Foundation)

###As no ordarinality we will convert in to factor

all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)

sum(table(all$Foundation))

character_variables

##Heating
levels(all$Heating)

all$Heating <- as.factor(all$Heating)
table(all$Heating)

sum(table(all$Heating))

character_variables
##HeatingQC
levels(all$HeatingQC)

level_heatingQC <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

library(plyr)
all$HeatingQC <- as.integer(revalue(all$HeatingQC, level_heatingQC))

table(all$HeatingQC)
sum(table(all$HeatingQC))

detach(package: plyr)
character_variables

##RoofStyle
levels(all$RoofStyle)

all$RoofStyle <- as.factor(all$RoofStyle)

table(all$RoofStyle)
sum(table(all$RoofStyle))

character_variables

##RoofMatl
levels(all$RoofMatl)

all$RoofMatl <- as.factor(all$RoofMatl)

table(all$RoofMatl)
sum(table(all$RoofMatl))

character_variables

##LandContour
levels(all$LandContour)

all$LandContour <- as.factor(all$LandContour)

table(all$LandContour)
sum(table(all$LandContour))

character_variables

##LandSlope
levels(all$LandSlope)

level_LandSlope <- c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)

library(plyr)

all$LandSlope <- as.integer(revalue(all$LandSlope, level_LandSlope))

table(all$LandSlope)
sum(table(all$LandSlope))

detach(package: plyr)

character_variables

##BldgType
levels(all$BldgType)

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
  geom_bar(stat = 'summary', fun.y = "median", fill = 'blue') +
  scale_y_continuous(breaks = seq(0,800000, by = 100000))

all$BldgType <- as.factor(all$BldgType)

table(all$BldgType)
sum(table(all$BldgType))

character_variables

##HouseStyle
levels(all$HouseStyle)

all$HouseStyle <- as.factor(all$HouseStyle)

table(all$HouseStyle)
sum(table(all$HouseStyle))

character_variables

##Neighbourhood
levels(all$Neighborhood)

all$Neighborhood <- as.factor(all$Neighborhood)

table(all$Neighborhood)
sum(table(all$Neighborhood))

character_variables

##Condition1
levels(all$Condition1)

all$Condition1 <- as.factor(all$Condition1)

table(all$Condition1)
sum(table(all$Condition1))

character_variables

##Condition2
levels(all$Condition2)

all$Condition2 <- as.factor(all$Condition2)

table(all$Condition2)
sum(table(all$Condition2))

character_variables

##Street
levels(all$Street)

level_Street <- c('Grvl' = 0, "Pave" = 1)

library(plyr)

all$Street <- as.integer(revalue(all$Street, level_Street))

table(all$Street)
sum(table(all$Street))

character_variables

##PavedDrive
levels(all$PavedDrive)

level_PavedDrive <- c('N' = 0, 'P' = 1, 'Y' = 2)

all$PavedDrive <- as.integer(revalue(all$PavedDrive, level_PavedDrive))

table(all$PavedDrive)
sum(table(all$PavedDrive))

detach(package: plyr)

###Now converting numeric variables like date column to factor

all$MoSold <- as.factor(all$MoSold)

summary(all$SalePrice)

##MSSubClass
levels(all$MSSubClass)

library(plyr)

all$MSSubClass <- as.factor(all$MSSubClass)
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-',
                                          '40'='1 story unf attic', '45'='1,5 story unf',
                                          '50'='1,5 story fin', '60'='2 story 1946+',
                                          '70'='2 story 1945-', '75'='2,5 story all ages',
                                          '80'='split/multi level', '85'='split foyer',
                                          '90'='duplex all style/age',
                                          '120'='1 story PUD 1946+',
                                          '150'='1,5 story PUD all',
                                          '160'='2 story PUD 1946+', '180'='PUD multilevel',
                                          '190'='2 family conversion'))

detach(package: plyr)


# numeric_Vars <- which(sapply(all, is.numeric))
# factor_Vars <- which(sapply(all, is.factor))

# Feature Engineering
##Calculating no. of bathrooms
all$Bathroom <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath +
  (all$BsmtHalfBath*0.5)

class(all$Bathroom)

ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(Bathroom), y = SalePrice)) +
  geom_point() + geom_smooth(method = 'lm', color="red", aes(group=1))

# reg1 <- lm(SalePrice~Bathroom, data = all)
# summary(reg1)
# 
# with(all,ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(Bathroom), y = SalePrice)) +
#        geom_point())
# abline(reg1)

all$FullBath <- NULL
all$HalfBath <- NULL
all$BsmtFullBath <- NULL
all$BsmtHalfBath <- NULL

##Age of the house and whether remodeling has been done or not
all$Remod <- ifelse(all$YearBuilt == all$YearRemodAdd, 0, 1)
all$Houseage <- as.numeric(all$YrSold) - all$YearRemodAdd

ggplot(all[!is.na(all$SalePrice),], aes(x = Houseage, y = SalePrice)) +
  geom_point() + geom_smooth(method = 'lm', color = 'red', aes(group = 1))

all$Newhouse <- ifelse(all$YrSold == all$YearBuilt, 1, 0)
###New house = 1, Old house = 0

ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(Newhouse), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000))

##Describing the richness of the neighborhood
class(all$Neighborhood)
median_n <- ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(Neighborhood, SalePrice), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median') +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000))

mean_n <- ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(Neighborhood, SalePrice), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean') +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000))

library(grid)

#install.packages("gridExtra")
library(gridExtra)
grid.arrange(median_n, mean_n) ## Used to show two graphs at the same time

all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

table(all$NeighRich)

##Square Feet
all$SqFt <- all$GrLivArea + all$TotalBsmtSF

ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(SqFt), y = SalePrice)) +
  geom_point() + geom_smooth(method = 'lm', color = 'red', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000))

## Skewness of SalePrice variable
#install.packages("psych")
# library(psych)
# skew(all$SalePrice)
# 
# qqnorm(all$SalePrice)
# qqline(all$SalePrice)
# 
# #Reducing the skewness of SalePrice
# all$SalePrice <- log(all$SalePrice)
# skew(all$SalePrice)
# 
# qqnorm(all$SalePrice)
# qqline(all$SalePrice)
# 
## Identifying the important features
#install.packages("randomForest")
library(randomForest)
important_variables = randomForest(x = all)

##Rank features by importance
# install.packages("caret")
# install.packages("mlbench")
library(caret)
library(mlbench)

testing_dataset <- all[!is.na(all$SalePrice),]

data("PimaIndiansDiabetes")

## Movig SalePrice column to last
testing_dataset <- testing_dataset%>%select(-SalePrice,SalePrice)

# control <- rfeControl(functions = rfFuncs, method = 'cv', number = 20) 
# rfe(testing_dataset[, 1:80], testing_dataset[,81], sizes = c(1:8), rfeControl = control)

modelrf <- randomForest(SalePrice ~ ., data = testing_dataset)
importance(modelrf)


model_train <- train(SalePrice ~ ., data = testing_dataset, method = 'RRF')
asd <- varImp(model_train, scale = F)

## Droppong highly correlated variables
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]

all <- all[-c(524, 1299),] ## Outliers

numericName <- numericName[!(numericName %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericName <- append(numericName, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericName]

DFfactors <- all[, !(names(all) %in% numericName)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

## Skewness
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

## One hot encoding
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])

DFdummies <- DFdummies[,-ZerocolTest]

ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])

DFdummies <- DFdummies[,-ZerocolTrain]

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

all$SalePrice <- log(all$SalePrice)

train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]

## Modelling
set.seed(27042018)
#install.packages("caret")
library(caret)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

linear_Mod <- lm(SalePrice ~., data = all)
summary(linear_Mod)

RSS <- c(crossprod(linear_Mod$residuals))
MSE <- RSS / length(linear_Mod$residuals)
RMSE <- sqrt(MSE)
RMSE


#q

























# 
# ## Spliting the variables as in which are categorical and which are numerical
# split(names(train), sapply(train, function(x) paste(class(x), collapse = " ")))
# 
# ## Calculating the number of NA's in each column
# apply(is.na(train),2, sum)
# colnames(train)[colSums(is.na(train)) > 0]
# 
# table(is.na(train)) ## Total NA's in a table
# 
# ### Calculating the missing value %age for each column
# colMeans(is.na(train))*100
# 
# ### Replacing 0 instead of NA in numerical columns
# 
# ind <-   which(sapply(train, is.numeric))
# for(j in ind){
#   set(train, i = which(is.na(train[[j]])), j = j, value = 0)
# }
# 
# ### None instead of NA in case of categorical variables
# ind_factor <- which(sapply(train, is.factor))
# for(j in ind_factor){
#   set(train, i = which(is.na(train[[j]])), j = j, value = "None")
# }
# 
# ### Removing columns with more than 40% of null values
# #train <- train[, -which(colMeans(is.na(train)) > 0.4)]
# 
# # ## Selecting only numeric value columns from train dataset
# # nums <- unlist(lapply(train, is.numeric))
# # nums_train <- train[ ,nums]
# 
# ## Decsision Tree
# 
# ### Creating columns which could be useful
# train['Agewhensold'] = train$YrSold - train$YearBuilt
# train['YearsSinceRemod'] = train$YrSold - train$YearRemodAdd
# 
# ### Selecting columns which might be necessary for our prediction
# train_dt <- train%>%select(LotArea, Utilities, Neighborhood, OverallQual, OverallCond, 
#                            YearBuilt, Agewhensold, YearsSinceRemod, HouseStyle, ExterQual, 
#                            ExterCond, BsmtQual, BsmtCond, HeatingQC, CentralAir, GarageType, 
#                            GarageQual, GarageCond, GarageArea, PoolArea, PoolQC, Fence, 
#                            MiscFeature, SalePrice)
# 
# fit <- rpart(train_dt$SalePrice~., data = train_dt, method = 'class')
# rpart.plot(fit, extra = 101)
# 
# ## Removing columns which are highly correlated with each other
# ### Detecting the correlation of various variables
# nums_train2 <- nums_train%>%
#   as.matrix%>%
#   cor%>%
#   as.data.frame%>%
#   rownames_to_column(var = 'var1')%>%
#   gather(var2, value, -var1)
# 
# 
# ### Filtering out variables which are highly correlated to SalePrice
# 
# highly_cor <- nums_train2%>%
#   filter(var2 == "SalePrice")
# 
# #tmp <- cor(nums_train)
# #tmp[upper.tri(tmp)] <- 0
# #diag(tmp) <- 0
# 
# ### MSZoning
# MSZoning <- train%>%
#   group_by(MSZoning)%>%
#   summarise(Mean_SalePrice = mean(SalePrice, is.na = TRUE))
# 
# ggplot(MSZoning, aes(x = reorder(MSZoning, -Mean_SalePrice), y = Mean_SalePrice)) +
#   geom_bar(stat = 'identity')
# ### Street
# ### Alley
# ### LotShape
# ### LandContour
# ### Utilities
# ### LotConfig
# ### LandSlope
# ### Neighborhood
# ### Condition1
# ### Condition2
# ### BldgType
# ### HouseStyle
# ### RoofStyle
# ### RoofMatl
# ### Exterior1st
# ### Exterior2nd
# ### MasVnrType
# ### ExterQual
# ### ExterCond
# ### Foundation
# ### BsmtQual
# ### BsmtCond
# ### BsmtExposure
# ### BsmtFinType1
# ### BsmtFinType2
# ### Heating
# ### HeatingQC
# ### CentralAir
# ### Electrical
# ### KitchenQual
# ### Functional
# ### FireplaceQu
# ### GarageType
# ### GarageFinish
# ### GargaeQual
# ### GarageCond
# ### PavedDrive
# ### PoolQC
# ### Fence
# ### MiscFeature
# ### SaleType
# ### SaleCondition
# ### MSSubClass
# ### LotFrontage
# ### OverallQual
# ### OverallCond
# ### YearBuilt
# ### YearRemodAdd
# ### MasVnrArea
# ### TotalBsmtSF
# ### GrLivArea
# ### GarageArea
# ### PoolArea
