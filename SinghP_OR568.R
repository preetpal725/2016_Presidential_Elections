library(ggplot2)
library(lattice)
library(caret)
library(dplyr)
library(readr)
library(corrplot)


data <- read_csv("/Users/harry/Downloads/OR568_final project/568 Project Data.csv")
obs <- nrow(data)

#View NULL results across rows, specifically election results
dataResults <- data[,c("Republicans 2016","Democrats 2016","Fips")]

countNAsRow <- rowSums(is.na(dataResults))
nameNAsRow <- data$County
fips <- dataResults$Fips
dataNAsRow <- data.frame(county = nameNAsRow, Fips = dataResults$Fips ,nullCount = countNAsRow)

toRemoveRows <- subset(dataNAsRow, nullCount >= 2)
removeFips <- data.frame(Fips = toRemoveRows$Fips)

#Remove Empty Election Results via Fips from main data
#Run From Here to Restart dataFixed Data Frame
dataFixed <- data %>%
  anti_join(removeFips, by = "Fips")

#Find Non Null Columns in Data
countNAs <- colSums(is.na(data))
nameNAs <- colnames(data)

#Create NULL Percents
dataNAs <- data.frame(predictor = nameNAs, nullCount = countNAs)
row.names(dataNAs) <- NULL

dataNAs$percentNull <- dataNAs$nullCount / obs
toRemove <- subset(dataNAs,percentNull > .4)

#Remove Columns

#Removing Vote Counts due to greater nulls than Vote Percents, also 4th party (all beyond Dem/Rep/Green/Lib had small sum of votes)
#Run to see small vote counts by all candidates > colSums(data[,112:144], na.rm = T)
#Removing weather related data, precip data is seasonal based, aggregates, etc. not on day of the election
#Removing unneccessary geographic data
#Removing Duplicate Columns (Copies of Vote Counts/Results, Copies of Race/Ethnicity)
#Removing Unknown Columns

drop <- colnames(data[,c(7:10,19,45:51,72:163)])
dataFixed = dataFixed[,!(names(dataFixed) %in% drop)]

#Run Correlations on Columns Remaining
dataFixedCor <- dataFixed[,5:59]

res <-cor(dataFixedCor, use = "complete.obs")
res <- round(res,2)

#Enter [x:x,y:Y] after res to segment corrplot
#Save as a 2500 x 2500 image and zoom in to better use CorrPlot
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex= 1/2)

##Remove Highly Correlated Variables

#Reduce Poverty Columns to Single Poverty Rate Metric
drop <- colnames(dataFixed[,c(27:28,33)])
dataFixed = dataFixed[,!(names(dataFixed) %in% drop)]
#Remove Management&Professional Degrees Highly Correlated with Higher Education Variables
dataFixed = dataFixed[,-c(31)]


#Run Correlations AGAIN on Columns Remaining
dataFixedCor <- dataFixed[,5:53]

res <-cor(dataFixedCor, use = "complete.obs")
res <- round(res,2)

#Enter [x:x,y:Y] after res to segment corrplot
#Save as a 2500 x 2500 image and zoom in to better use CorrPlot
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex= 1/2)

#Remove Lat and Long
dataFixed <- dataFixed[,-c(38:39)]
#Remove Grad Degree & School Enrollement
dataFixed <- dataFixed[,-c(18:19)]

#Variable Creation
#Create County Republican Victory Binary Variable
dataFixed$RepublicanWin2016 <- ifelse(dataFixed$`Republicans 2016` > dataFixed$`Democrats 2016`,1,0)
dataFixed$RepublicanWin2012 <- ifelse(dataFixed$`Republicans 2012` > dataFixed$`Democrats 2012`,1,0)
#Minority Percentage
dataFixed$Minority <- 100 - dataFixed$White.not.Latino.Population
dataFixed <- dataFixed[,-c(19:24)]
#Remove Education/Only Higher Education Remains
dataFixed <- dataFixed[,-c(15:16)]


#Scatter Plot Matrix
#Get numerical data only
dataFixedCor <- dataFixed[,c(5:43,45:47)]
#Create Scatter Plot Matrix #3 is 2016 Republican Results, add other variables to see relationship
pairs(dataFixedCor[,c(3,13:15)])


#Colorized Individual Plots
#Change X and Y values below, color on RepublicanWin Binary
x <- log(dataFixed$votes)
y <- dataFixed$`Republicans 2016`

qplot(x, y, data = dataFixed, colour = RepublicanWin2016)

#PreProcess
##Remove Identifier Columns
rowNum <- nrow(dataFixed)
dataFixedZ <- dataFixed[,c(1:4,44:46)]
dataFixedX <- dataFixed[,c(5:43,47)]

dataFixedPP <- preProcess(dataFixedX, method = c("YeoJohnson","center","scale"))

dataFixedXTrans <- predict(dataFixedPP, dataFixedX)

#Rejoin Identifier Fields to Transformed Fields
dataModeling <- data.frame(c(dataFixedZ,dataFixedXTrans))
dataModeling$RepublicanWin2012 <- as.factor(dataModeling$RepublicanWin2012)
dataModeling$RepublicanWin2016 <- as.factor(dataModeling$RepublicanWin2016)

#View Nulls before Modeling/Imputation
rowSums(is.na(dataModeling))
round(colSums(is.na(dataModeling)) / sum(dataModeling$Number.of.Records),4)

#Imputation with Mice Package
#install.packages("mice")
library(mice)

##Remove Columns with over 10% NAs
drop <- colnames(dataModeling[,c(12,31:32,36,40,42,44,46)])
dataModeling <- dataModeling[,!(names(dataModeling) %in% drop)]

##Select Columns with under 10% NAs for Computation
round(colSums(is.na(dataModeling)) / sum(dataModeling$Number.of.Records),4)

dataModelingImp <- dataModeling[,c(20,26,30:32,35,37:38)]

##Compute using CART method
imputed_Data <- mice(dataModelingImp, m=1, maxit = 5, method = 'cart', seed = 500)

completed <- complete(imputed_Data,1)

##Remove Unimputed Columns, add imputed
dataModeling <- dataModeling[,-c(20,26,30:32,35,37:38)]
dataModeling <- cbind(dataModeling,completed)

#Check Imputation has worked
round(colSums(is.na(dataModeling)) / sum(dataModeling$Number.of.Records),4)

#Remove 2016 and 2012 Vote Percentages
dataModeling <- dataModeling[,-c(10:11,13,15)]

#Stratified Random Sample (Used due to Majority of Counties won by Republicans, small pop vs. large pop counties)

set.seed(122)
sizeTrain <- .75

partitionData <- createDataPartition(dataModeling$RepublicanWin2016, p = sizeTrain, list = F, times = 1)
test <- dataModeling[-partitionData,]
train <- dataModeling[partitionData,]

#### Final Datasets to Use for Modeling : Train & Test ####
### Train Model w/ Train dataset using all numeric variables against RepublicanWin2012
### Test Model w/ Test dataset using all numeric variables against Republicanwin2016

#KNN Model

knnModel <- train(train[,8:34], 
                  train$RepublicanWin2012,
                  method = "knn",
                  tuneGrid = data.frame(.k = 1:10),
                  trControl = trainControl(method = "cv"))

knnModel

knnPred <- predict(knnModel, newdata=test)

knnPR <- postResample(pred=knnPred, obs=test$RepublicanWin2016)

confusionMatrix(knnPred,test$RepublicanWin2016)

varImp(knnModel)
######################################

##########################################
################ MARS model ##############
##########################################

library(plotmo)
library(plotrix)
library(TeachingDemos)
library(earth)
library(Matrix)
library(foreach)
library(glmnet)
library(pROC) 


#import test and train data
train_mars <- train
test_mars <- test

# Excluding democratic 2008 results 
train_mars <- subset(train_mars, select=-c(Democrats.2008))
test_mars <- subset(test_mars, select=-c(Democrats.2008))

#running the model
marsGrid = expand.grid(.degree=1:2, .nprune=2:10)
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
marsModel = train(x=train_mars[,8:33], 
                  y=train_mars$RepublicanWin2012, 
                  method="earth", 
                  tuneGrid=marsGrid, trControl = ctrl)
marsModel

summary(marsModel)

marsPred = predict(marsModel, newdata=test_mars)
marsPR = postResample(pred=marsPred, obs=test_mars$RepublicanWin2016)
marsPR
confusionMatrix(marsPred,test_mars$RepublicanWin2016)

#Important variables
varImp(marsModel)

#ROC plots
rocPlot1 <- roc(response =  test_mars$RepublicanWin2016, predictor =  as.numeric(marsPred))
plot(rocPlot1)
rocPlot1



######################################
###### Gradient Boosting #############
######################################

require(gbm)
require(MASS)#package with the boston housing dataset
library(gbm)
library(MASS)

#import training and test data
train_gb=train
test_gb = test

# running the model
gbmGrid <- expand.grid(n.trees = seq(100, 1000, by = 200), 
                       interaction.depth = c(1, 5), 
                       n.minobsinnode = c(10), shrinkage = c(.01, .1))
gbmFit <- train(x=train_mars[,8:33], 
                y=train_mars$RepublicanWin2012,
                method = "gbm",
                tuneGrid = gbmGrid,
                verbose = FALSE,
                trControl = ctrl)
gbmFit
summary(gbmFit)
gbmpred = predict(gbmFit, newdata=test_gb)
gbmPR = postResample(pred=gbmpred, obs=test_gb$RepublicanWin2016)
gbmPR
confusionMatrix(gbmpred,test_gb$RepublicanWin2016)

#Important variables
varImp(gbmFit)

#ROC plots
rocPlotgb <- roc(response =  test_gb$RepublicanWin2016, predictor =  as.numeric(marsPred))
plot(rocPlotgb)
rocPlotgb
