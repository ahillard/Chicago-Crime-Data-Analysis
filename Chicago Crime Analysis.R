#######################################################
#Data Import 
#######################################################

setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis")
crime <- read.csv("Crimes_-_2001_to_present.csv")
save <- crime
crime <- save

#Prune Dataset to Variables of Interest
crime <- crime[crime$Year >= 2010,] #Get Year 2010 and Above
drop = c("ID","Case.Number","Block","IUCR","Description","Arrest","Domestic",
          "District","Beat","Ward","Community.Area","Updated.On","Location",
         "X.Coordinate","Y.Coordinate","Primary.Type","Location.Description")
crime <- crime[ , !(names(crime) %in% drop)]
crime <- crime[complete.cases(crime),] #This drops observations that have NA in lat/long

#######################################################
#Time Predictors - Month and month 
#######################################################

##Add Month and month Day to each Observation
crime$Date <- as.character(crime$Date)
crime$Date <- substr(crime$Date,1,nchar(crime$Date)-12)
crime$Date <- as.Date(crime$Date, "%m/%d/%Y")

#Load Time Manipulation Packages
library(stringi)
library(lubridate)

#Get Day, Month, and Year Factors
#crime$day <- as.factor(wday(crime$Date))
#crime$month <- as.factor(month(crime$Date)) #Method can be masked with data.table package
crime$month <- as.factor(month(crime$Date))
crime$year <- as.factor(crime$Year)

#Convert Latitude and Longitude into numeric type
crime$Latitude <- as.numeric(crime$Latitude)
crime$Longitude <- as.numeric(crime$Longitude)

#######################################################
#Import Census Tracts
#######################################################

#Load Spatial Libraries 
library(rgdal)
library(sp)
library(raster)

#Load Chicago Census Tracts
setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis/Boundaries - Census Tracts - 2010")
tract <- shapefile("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
tract <- spTransform(x=tract, CRSobj=CRS("+init=epsg:4326"))
names(tract@data) <- tolower(names(tract@data))

#Make Crime Data to Spatial Object
spatial_crime <- SpatialPointsDataFrame(coords=crime[,c("Longitude", "Latitude")], data = crime, proj4string=CRS("+init=epsg:4326"))

#Overlay Census Tract Polygon where Each Crime Happened
overlay <- over(x=spatial_crime, y=tract)

#Add Census Track to each Observation
crime <- cbind(crime, tract = as.factor(overlay$tractce10))

#Get Rid of Crimes that Didn't Fall within one of the Cook County Tracts
crime <- crime[complete.cases(crime),]

#######################################################
#Classify Crimes into Property, Persons, or Society
#######################################################

#Reorganize crime dataset
crime <- crime[,c("tract","Date","FBI.Code","Latitude","Longitude","year","month")]
crime$FBI.Code <- as.character(crime$FBI.Code)

#Three Types of Crimes
#http://gis.chicagopolice.org/clearmap_crime_sums/crime_types.html
#Crimes against Person, Crimes against Property, and Crimes against Society
fbi_code <- data.frame(
  FBI.Code=c("01A","01B","02","03","04A","04B","05","06","07","08A","08B","09","10","11","12","13","14","15","16","17","18","19","20","22","24","26"),       
  classification=c("Persons","Persons","Persons","Property","Persons","Persons","Property","Property","Property",
    "Persons","Persons","Property","Property","Property","Property","Property","Property","Society",
    "Society","Persons and Society","Society","Society","Persons and Society","Society","Society",
    "Society"),
  seriousness=c("More Serious","Less Serious","More Serious","More Serious","More Serious","More Serious",
    "More Serious","More Serious","More Serious","Less Serious","Less Serious","More Serious",
    "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
    "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
    "Less Serious","Less Serious")
)

crime = merge(crime, fbi_code, by="FBI.Code")

#Get rid of FBI.Code column, Latitude, Longitude
crime <- crime[,c("tract","Date","year","month","classification","seriousness")]

#Create dummy variables for persons, property, and society
crime$persons = 0
crime$persons[crime$classification == "Persons"] <- 1
crime$persons[crime$classification == "Persons and Society"] <- 1

crime$property = 0
crime$property[crime$classification == "Property"] <- 1

crime$society = 0
crime$society[crime$classification == "Society"] <- 1
crime$society[crime$classification == "Persons and Society"] <- 1

crime$serious = 0
crime$serious[crime$seriousness == "More Serious"] <- 1

#Get Rid of Unnecessary Columns 
crime <- crime[,c("tract","Date","year","month","persons","property","society","serious")]
names(crime) <- c("tract","date","year","month","persons","property","society","serious")

save1 <- crime
crime <- save1
setwd("/Users/sharonhillard/Desktop/Andrew Portfolio/Chicago-Crime-Data-Analysis")
write.csv(save1, "save1.csv")

tmp <- read.csv("save1.csv")

#######################################################
#Load Census Data
#######################################################

#url <- "http://lakshmi.calit2.uci.edu/census2000/R/src/contrib/UScensus2010tract_1.00.tar.gz"
#install.packages(url, repos = NULL)
library(UScensus2010)
library(UScensus2010tract)

census_data <- as.data.frame(county(name="cook", state="illinois", level="tract")) #I think this may be giving tract information 
census_data$tract <- as.factor(census_data$tract)

#######################################################
#Aggregate by month
#######################################################

#Load package to for summary
library(dplyr)

#Census Population / Requires you already loaded 2010 Census Data
#Population is P0010001, Use when creating indicator variables 
population_tract <- data.frame(tract=census_data$tract, population=census_data$P0010001)

#Crimes committed by tract
group <- group_by(crime, tract, year, month)
crime_month <- as.data.frame(summarise(group, 
                            persons_month = sum(persons),
                            property_month = sum(property), 
                            society_month = sum(society),
                            serious_month = sum(serious)
))

crime_month <- merge(crime_month, population_tract, by="tract")
crime_month$persons_month <- crime_month$persons_month/crime_month$population
crime_month$property_month <- crime_month$property_month/crime_month$population
crime_month$society_month <- crime_month$society_month/crime_month$population
crime_month$serious_month <- crime_month$serious_month/crime_month$population 

#Side Note
tmp <- crime_month[!complete.cases(crime_month),] #There are four census tracts with zero population

#Create Median Value of Serious Crime Per Capita using Training Data as Baseline 

persons_median <- median(crime_month$persons_month, na.rm=T)
property_median <- median(crime_month$property_month, na.rm=T)
society_median <- median(crime_month$society_month, na.rm=T)
serious_median <- median(crime_month$serious_month, na.rm=T)

#Create dummy variables for Crimes
crime_month$persons <- ifelse(crime_month$persons_month > persons_median, 1, 0)
crime_month$serious <- ifelse(crime_month$serious_month > serious_median, 1, 0)
crime_month$property <- ifelse(crime_month$property_month > property_median, 1, 0)
crime_month$society <- ifelse(crime_month$society_month > society_median, 1, 0)

#Subset crime_day for variables of interest
crime_month <- subset(crime_month, select=c("tract","year","month","persons","serious","property","society"))

#Change Response Columns to Factors
crime_month$persons <- as.factor(crime_month$persons)
crime_month$serious <- as.factor(crime_month$serious)
crime_month$property <- as.factor(crime_month$property)
crime_month$society <- as.factor(crime_month$society)

#Get Rid of The Four Census Tracts with Zero Popualtion
crime_month <- crime_month[complete.cases(crime_month),]

#Save crime_month as save2 file 
save2 <- crime_month
crime_month <- save2
write.csv(save2, "save2.csv")

setwd("/Users/sharonhillard/Desktop/Andrew Portfolio/Chicago-Crime-Data-Analysis")
crime_month <- read.csv("save2.csv")
crime_month  <- crime_month[,-1]

#######################################################
#Merge Census Data with tract_id 
#######################################################

#Merge census_data with crime_month
crime_month <- merge(crime_month, census_data, by="tract")
table(complete.cases(crime_month)) #Should have all complete cases
drop = c("state","county","fips")
crime_month <- crime_month[ , !(names(crime_month) %in% drop)]
save3 <- crime_month
crime_month <- save3

#######################################################
#Merge Weather Data with tract
#######################################################

setwd("/Users/ahillard/Desktop/Chicago-Crime-Data-Analysis/Underground Weather Data")

#Load package to do rbindlist
library(data.table)

#Import weather files
weather.files = list.files()
weather.list = lapply(weather.files, function(x) read.csv(x))
weather = rbindlist(weather.list) 
weather$CST <- as.Date(weather$CST)
names(weather)[1] <- "date"

#Create time variables in weather
weather$year <- as.factor(year(weather$date))
weather$month <- as.factor(month(weather$date))
weather$month <- as.factor(month(weather$date))
weather$day <- as.factor(day(weather$date))

#Set weather as data.frame
weather <- as.data.frame(weather)

#Fix PreciptationIn Variable
weather$PrecipitationIn <- as.numeric(as.character(weather$PrecipitationIn))
weather[is.na(weather$PrecipitationIn), ]$PrecipitationIn <- mean(weather$PrecipitationIn, na.rm=T) #Set all NAs to mean 

#Drop Max.Gust.SppedMPH (incomplete records), Events (can't be averaged), and day. 
#names(weather)[colSums(is.na(weather)) > 0] #Max.Gust.SpeedMPH has missing values
drop = c("Max.Gust.SpeedMPH","Events","day","date")
weather <- weather[ , !(names(weather) %in% drop)]

#Summarize Weather by month 
group <- group_by(weather, year, month)
weather_month <- as.data.frame(summarise(group, 
                                        max_temp = max(Max.TemperatureF),
                                        min_temp = min(Min.TemperatureF),
                                        mean_temp = mean(Mean.TemperatureF),
                                        max_dew = max(Max.Dew.PointF),
                                        min_dew = min(Min.DewpointF),
                                        mean_dew = mean(MeanDew.PointF),
                                        max_hum = max(Max.Humidity),
                                        min_hum = min(Min.Humidity),
                                        mean_hum = mean(Mean.Humidity),
                                        max_vis_miles = max(Max.VisibilityMiles),
                                        min_vis_miles = min(Min.VisibilityMiles),
                                        mean_vis_miles = mean(Mean.VisibilityMiles),
                                        max_wind = max(Max.Wind.SpeedMPH),
                                        mean_wind = mean(Mean.Wind.SpeedMPH),
                                        precipitation = sum(as.numeric(PrecipitationIn)),
                                        cloud_cover = mean(as.numeric(CloudCover)),
                                        winddirdegrees = mean(WindDirDegrees)
))

#Merge weather with crime_month
crime_month <- merge(crime_month, weather_month, by=c("year","month"))
save4 <- crime_month

#######################################################
#Merge Economic Data with tract
#######################################################
#Economic data goes back to 2006, by month 

#Month two Number function
mo2Num <- function(x) match(tolower(x), tolower(month.abb))

#Import Economic Data
setwd("/Users/ahillard/Desktop/Chicago-Crime-Data-Analysis/BLS Data")
Chi_CPI <- read.csv("BLS Consumer Price Index - All Urban Consumers.csv")
Chi_CPI <- data.frame(year=rep(Chi_CPI[,1],12), stack(Chi_CPI[2:13]))
names(Chi_CPI) <- c("year","CPI","month")
Chi_CPI$month <- mo2Num(Chi_CPI$month)
Chi_CPI$year <- as.factor(Chi_CPI$year)
Chi_CPI$month <- as.factor(Chi_CPI$month)

Chi_Unemployment <- read.csv("BLS Unemployment.csv")
Chi_Unemployment <- data.frame(year=rep(Chi_Unemployment[,1],12), stack(Chi_Unemployment[2:13]))
names(Chi_Unemployment) <- c("year","Unemployment","month")
Chi_Unemployment$month <- mo2Num(Chi_Unemployment$month)
Chi_Unemployment$year <- as.factor(Chi_Unemployment$year)
Chi_Unemployment$month <- as.factor(Chi_Unemployment$month)

Chi_Earnings <- read.csv("BLS State and Area Employment, Hours, and Earnings.csv")
Chi_Earnings <- data.frame(year=rep(Chi_Earnings[,1],12), stack(Chi_Earnings[2:13]))
names(Chi_Earnings) <- c("year","Earnings","month")
Chi_Earnings$month <- mo2Num(Chi_Earnings$month)
Chi_Earnings$year <- as.factor(Chi_Earnings$year)
Chi_Earnings$month <- as.factor(Chi_Earnings$month)

#Merge Economic Data with Crime month Data 
crime_month <- merge(crime_month, Chi_CPI, by=c("year","month"))
crime_month <- merge(crime_month, Chi_Unemployment, by=c("year","month"))
crime_month <- merge(crime_month, Chi_Earnings, by=c("year","month"))

#save5 of crime_month 
save5 <- crime_month
crime_month <- save5

#######################################################
#Merge Population Data with tract
#######################################################

#Annual Estimates of the Resident Population from Census
#http://www.census.gov/popest/data/historical/index.html

#Import Population Data
setwd("/Users/ahillard/Desktop/Chicago-Crime-Data-Analysis/Census Population Data")
pop_00_10 <- read.csv("2000 - 2010 Cook County Population Estimates.csv")
pop_00_10 <- pop_00_10[(pop_00_10$CTYNAME=="Cook County") & (pop_00_10$STNAME=="Illinois"),]

pop_10_15 <- read.csv("2011 - 2015 Cook County Population Estimates.csv")
pop_10_15 <- pop_10_15[(pop_10_15$GEO.display.label=="Cook County, Illinois"),]



#cbind Population Data
population <- cbind(pop_00_10, pop_10_15)
population <- population[,c(15:19, 27:31)]
population <- data.frame(year=2006:2015, population=as.numeric(t(population)))

#Set Variables of year to factor
population$year <- as.factor(population$year)

#Merge Population Data with Crime month
crime_month <- merge(crime_month, population, by="year", all.x=T)
table(complete.cases(crime_month)) #The incomplete cases are the year where no population is provided
#Keep the incomplete cases since the training data has no population provided

#save6 for crime_month
save6 <- crime_month
crime_month <- save6

#######################################################
#Create Train and Test Datasets for Serious Response 
#######################################################

#Drop Responses that are not the "Serious" response 
drop = c("persons","property","society")
crime_serious <- crime_month[ , !(names(crime_month) %in% drop)]

#Train and Test Dataset
crime_serious_train <- crime_serious[as.numeric(crime_serious$year)<5,] #2010 through 2013
crime_serious_test <- crime_serious[(as.numeric(crime_serious$year)>=5) & (as.numeric(crime_serious$year)<7),] #2014 through 2015

#save7 for crime_serious
setwd("/Users/ahillard/Desktop/Thesis Data")
save7 <- crime_serious
write.csv(crime_serious, "crime_serious.csv")

#Read crime_serious data for Analysis
setwd("/Users/ahillard/Desktop/Thesis Data")
crime_serious <- read.csv("crime_serious.csv") #Should have 63758 obs of 485 variables
drop = c("X")
crime_serious <- crime_serious[ , !(names(crime_serious) %in% drop)]

#Convert data that you know is a factor 
crime_serious$month <- as.factor(crime_serious$month)
crime_serious$tract <- as.factor(crime_serious$tract)
crime_serious$year <- as.factor(crime_serious$year)
crime_serious$serious <- as.factor(crime_serious$serious)

#Train and Test Dataset for loadedcrime_serious 
crime_serious_train <- crime_serious[as.numeric(crime_serious$year)<5,] #2010 through 2013
crime_serious_test <- crime_serious[(as.numeric(crime_serious$year)>=5) & (as.numeric(crime_serious$year)<7),] #2014 through 2015

##########################################################################
#Use Ranger Package to Do Random Forest for High Dimension Data
##########################################################################

library(ranger)
library(caret)
library(e1071)

#Tune Random Forest Model with oob 
set.seed(1) 
train_rf <- train(serious~., 
                  data=crime_serious_train,
                  method="ranger",
                  trControl = trainControl(method="oob", search="grid"),
                  tuneLength=25, 
                  num.trees=500
                  )
plot(train_rf$results$mtry,train_rf$results$Accuracy,type='l')
#Best mtry = 576
#Use mtry = 256 because very close to 576 and lower dimension, likely more robust 

#Final Random Forest Model withh 1000 Trees 
x.tmp <- model.matrix(serious~., crime_serious_train)[,-1]
crime_serious_train_matrix <- data.frame(serious = crime_serious_train$serious, x.tmp)

x.tmp <- model.matrix(serious~., crime_serious_test)[,-1]
crime_serious_test_matrix <- data.frame(serious = crime_serious_test$serious, x.tmp)

set.seed(1) 
rf.final = ranger(serious~., 
                  data=crime_serious_train_matrix,
                  num.trees=1000, 
                  mtry=256, 
                  importance="impurity", 
                  verbose=TRUE, 
                  write.forest=TRUE
                  )

rf.final_v2 = ranger(serious~., 
                  data=crime_serious_train_matrix,
                  num.trees=1000, 
                  mtry=576, 
                  importance="impurity", 
                  verbose=TRUE, 
                  write.forest=TRUE
)


setwd("/Users/ahillard/Desktop/Thesis Data")
save(rf.final_v2, "rffinal.rda")

#Predictions and Error Rate rf.final
preds <- predict(rf.final, data=crime_serious_test_matrix, type="response")$predictions
1 - sum(preds==crime_serious_test_matrix$serious)/length(preds)
#Misclassification of 16.50892%

#Predictions and Error Rate rf.final_v2
preds <- predict(rf.final_v2, data=crime_serious_test_matrix, type="response")$predictions
1 - sum(preds==crime_serious_test_matrix$serious)/length(preds)
#Misclassification of 16.26314%

#Importance of Variables
rf.importance <- importance(rf.final_v2)
write.csv(rf.importance, "ranger_importance.csv")
rf.importance <- read.csv("ranger_importance.csv")
varImpPlot(rf.importance)

#Top 50 Variables
topnames <- rf.importance[order(rf.importance$x, decreasing=T),]
topnames[1:20,]
write.csv(topnames, "topnames_rf.csv")
write.csv(topnames[1:20,], "toptwenty_rf.csv")

#######################################################
#Try Out Logistic Regression with LASSO
#######################################################

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Serious Predictor 
library(glmnet)
x = model.matrix(serious ~., crime_serious_train)
y = as.numeric(crime_serious_train$serious)-1

lasso_mod_serious <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1) 
cv.out_lasso = cv.glmnet(x,y,alpha=1, nfolds=5) 
bestlam_lasso = cv.out_lasso$lambda.min
bestlam_index_lasso = which(cv.out_lasso$lambda == bestlam_lasso)
cv_error_lasso <- cv.out_lasso$cvm[bestlam_index_lasso] #Misclassification CV is 10.16957%
predict(lasso_mod_serious, s=bestlam_lasso, type='coefficients')

#table(predict(lasso_mod_serious, s=bestlam_lasso, type="coefficients")[,1]!=0)
#plot(1:length(cv.out_lasso$cvm),cv.out_lasso$cvm)


x.test = model.matrix(serious~., crime_serious_test)
y.test = as.numeric(crime_serious_test$serious)-1
pred_lasso <- predict(lasso_mod_serious, s=bestlam_lasso, newx=x.test, type='response')
error_lasso <- cost(pred_lasso, y.test) #Test Error is 15.92846%


#######################################################
#Try Out Logistic Regression with RIDGE
#######################################################

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Serious Predictor 
library(glmnet)
x = model.matrix(serious ~., crime_serious_train)
y = as.numeric(crime_serious_train$serious)-1

ridge_mod_serious <- glmnet(x, y, alpha=0, family='binomial')
set.seed(1) 
cv.out_ridge = cv.glmnet(x,y,alpha=0, nfolds=5) 
bestlam_ridge = cv.out_ridge$lambda.min
bestlam_index_ridge = which(cv.out_ridge$lambda == bestlam_ridge)
cv_error_ridge <- cv.out_ridge$cvm[bestlam_index_ridge] #Misclassification CV is 0.1016856

x.test = model.matrix(serious~., crime_serious_test)
y.test = as.numeric(crime_serious_test$serious)-1
pred_ridge <- predict(ridge_mod_serious, s=bestlam_ridge, newx=x.test, type='response')
error_ridge <- cost(pred_ridge, y.test) #Test Error is %0.1599644

##########################################################################
#XGBoost - Extreme Gradient Boosting
##########################################################################

install.packages("xgboost")
install.packages("readr")
install.packages("stringr")
install.packages("car")
install.packages("caret")
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

#x.train <-  model.matrix(serious~., data=crime_serious_train[as.numeric(crime_serious_train$year)<4,])[,-1]
#y.train <- as.numeric(crime_serious_train$serious[as.numeric(crime_serious_train$year)<4])-1

#x.val <-  model.matrix(serious~., data=crime_serious_train[as.numeric(crime_serious_train$year)==4,])[,-1]
#y.val <- as.numeric(crime_serious_train$serious[as.numeric(crime_serious_train$year)==4])-1

x.train <-  model.matrix(serious~., data=crime_serious_train)[,-1]
y.train <- as.numeric(crime_serious_train$serious)

x.test <- model.matrix(serious~., data=crime_serious_test)[,-1]
y.test <- as.numeric(crime_serious_test$serious)

#Simple xgboost
set.seed(1)
xgb <- xgboost(data = x.train, 
               label = y.train, 
               eta = .01, 
               max_depth = 10, 
               nround=10, 
               #early.stop.round=2,
               #subsample = 1,
               #colsample_bytree = 1,
               seed = 1,
               eval_metric = "error",
               objective = "binary:logistic"
)

#Setup to Run Caret using XGBoost
#Method - CV on Training to Find Parameter Values
#Run on Validation Set to Tune nrounds on Final Parameters

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#Set nrounds, max_depth, and eta
test_grid =  expand.grid(
    nrounds=10,
    max_depth=2,
    eta=.2, 
    gamma=0,
    colsample_bytree=1,
    min_child_weight=1,
    subsample=1
    #nrounds = c(50, 75, 100),
    #max_depth = c(2, 5, 10),
    #eta = c(0.01, 0.1, 0.2)
  )

xgb_grid_1 = expand.grid(
  nrounds=c(35, 70, 105, 140),
  max_depth=c(1,2,3,4,5,6,7,8,9,10),
  eta=c(.01, .1, .2), 
  gamma=0,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)

xgb_train_1 = train(
  x = x.train,
  y = as.factor(y.train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

#nrounds=70, max_depth=7, eta=0.1, gamma=0, colsample_bytree=1, min_child_weight=1, subsample=1

xgb_results <- xgb_train_1$results 
write.csv(xgb_results, "xgb_train1.csv")

#Tune for colsample_by_tree and min_child_weight

xgb_grid_2 = expand.grid(
  nrounds=70,
  max_depth=7,
  eta=0.1, 
  gamma=0,
  colsample_bytree=c(.1, .5, .7, 1),
  min_child_weight=1,
  subsample=c(.1, .5, .7, 1)
)

xgb_trcontrol_2 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)

xgb_train_2 = train(
  x = x.train,
  y = as.factor(y.train),
  trControl = xgb_trcontrol_2,
  tuneGrid = xgb_grid_2,
  method = "xgbTree"
)

#Keep Same Parameters
#Tune nrounds Round 3

xgb_grid_3 = expand.grid(
  nrounds=c(50, 55, 60, 65, 70, 75, 80, 85, 90),
  max_depth=7,
  eta=0.1, 
  gamma=0,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1
)

xgb_trcontrol_3 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)

xgb_train_3 = train(
  x = x.train,
  y = as.factor(y.train),
  trControl = xgb_trcontrol_3,
  tuneGrid = xgb_grid_3,
  method = "xgbTree"
)

#Tune nrounds Round 4

xgb_grid_4 = expand.grid(
  nrounds=seq(25, 60, 5),
  max_depth=7,
  eta=0.1, 
  gamma=0,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1
)

xgb_trcontrol_4 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)

xgb_train_4 = train(
  x = x.train,
  y = as.factor(y.train),
  trControl = xgb_trcontrol_4,
  tuneGrid = xgb_grid_4,
  method = "xgbTree"
)

#Chose nrounds = 45, max_depth = 7, eta = 0.1, gamma = 0, colsample_bytree = 1, 
# min_child_weight = 1, and subsample = 1

#Predictions with Tuned Model

trained_xgb_model <- xgb_train_4$finalModel
preds_xgb <- ifelse(predict(trained_xgb_model, x.test)>.5, 1, 0)
sum(xgbpreds==y.test)/length(xgbpreds) #Somehow 1 and 0 are flipped in preds
#Test Misclassification is 0.1703185 

stopCluster(cl)

##########################################################################
#Use e1071 to Do SVM - This is Going to Take a Long Time to Run, even if it's only linear 
##########################################################################

library(e1071)

set.seed(1)
Sys.time() #"2016-11-18 22:11:36 CST"
tune.out = tune(svm, serious~., data=crime_serious_train, scale=TRUE, kernel="linear", ranges=list(cost=c(1)))
Sys.time() #"2016-11-19 03:19:57 CST"
names(tune.out)
best_mod <- tune.out$best.model
preds <- predict(best_mod, crime_serious_test) #Cost of 71 worked the best in 3 fold CV
1-sum(preds==crime_serious_test$serious)/length(preds) #0.2305

test <- svm(serious~., data=crime_serious_train[1:1000,], kernel="linear", cost=10, scale=FALSE)


#######################################################
#Try One Layer Neural Network 
#######################################################

library(deepnet)
library(MASS)
library(nnet)

#Scale crime_day_train
crime_serious_train_sc <- crime_serious_train
tmp.response <- crime_serious_train_sc$serious
tmp.predictors <- crime_serious_train_sc[,!(names(crime_serious_train_sc) %in% "serious")]

ind <- sapply(tmp.predictors, is.numeric)
tmp.predictors[ind] <- lapply(tmp.predictors[ind], scale)

train.nnet <- data.frame(serious=tmp.response, tmp.predictors) 
train.nnet <- train.nnet[,!(train.nnet[1,]=="NaN")]
train.nnet$serious <- as.factor(train.nnet$serious)

#Scale crime_day_test
crime_serious_test_sc <- crime_serious_test
tmp.response <- crime_serious_test_sc$serious
tmp.predictors <- crime_serious_test_sc[,!(names(crime_serious_test_sc) %in% "serious")]

#ind <- sapply(tmp.predictors, is.numeric)
tmp.predictors[ind] <- lapply(tmp.predictors[ind], scale)

test.nnet <- data.frame(serious=tmp.response, tmp.predictors) 
test.nnet <- test.nnet[,!(test.nnet[1,]=="NaN")]
test.nnet$serious <- as.factor(test.nnet$serious)

#Do PCA because Neural Net is taking so long 

names(train.nnet)
census_train <- train.nnet[,5:444]
weather_train <- train.nnet[, 445:460]

#Identify variables with nearly-zero covariance
nr0=0.001
near_zero <- which(abs(cov(census_train)) < nr0, arr.ind=TRUE)
census_names <- unique(rownames(near_zero))

census_train1 <- census_train[,!(names(census_train) %in% census_names)]
fit1 <- princomp(census_train1, cor=FALSE)
plot(fit1)
census_load <- loadings(fit1)
test <- as.matrix(census_test) %*% as.matrix(census_load)

head(fit1$scores)

#Weather component 
fit2 <- princomp(weather_train, cor=FALSE)
summary(fit)
plot(fit2)
weather_load <- loadings(fit2)

#Train Comp
train_comp <- data.frame(train.nnet[,1:4], train.nnet[,461:464], census_comp = fit1$scores[,1], weather_comp = fit2$scores[,1])

#Test Comp
census_test = test.nnet[,5:444]
census_test <- census_test[,!(names(census_test) %in% census_names)]

weather <- test.nnet[, 445:460]

test_comp <- data.frame(test.nnet[,1:4],
                        test.nnet[,461:464],
                        census_comp = as.matrix(census_tract) %*% as.matrix(census_load)[,1],
                        weather_comp = as.matrix(weather) %*% as.matrix(weather_load)[,1]
                        )




#Try Neural Net
test <- nnet(serious~., data=train_comp, size=5, MaxNWts=2000)
preds.nnet <- predict(test, test_comp, type='class')
1 - sum(preds.nnet == test_comp$serious) / length(preds.nnet) #25.02222% Misclassification for size 2

#Train Neural Net with Caret
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#Grid 1

nnet_grid1 = expand.grid(
  size=c(1,5,10),
  decay = c(0,.01,.1)
)

nnet_control1 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)


nnet_train1 <- train(serious~., 
                    data=train.nnet, 
                    tuneGrid = nnet_grid1,
                    trControl = nnet_control1,
                    MaxNWts=10000,
                    method = 'nnet'
                    )

nnet_train1
write.csv(nnet_train1$results, "nnet_train1.csv")
nnet_train1 <- read.csv("nnet_train1.csv")


#Grid 2

nnet_grid2 = expand.grid(
  size=c(10, 15, 20, 25, 30),
  decay = c(0.10)
)

nnet_control2 = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE
)


nnet_train2 <- train(serious~., 
                     data=train.nnet, 
                     tuneGrid = nnet_grid2,
                     trControl = nnet_control2,
                     MaxNWts=50000,
                     method = 'nnet'
)

nnet_train2$results
write.csv(nnet_train2$results, "nnet_train2.csv")
nnet_train2 <- read.csv("nnet_train2.csv")

#Turn off Cluster 

stopCluster(cl)

#######################################################
#Spatio-Temporal Models 
#######################################################

library(sp)
library(INLA)
library(rgeos)
library(maptools)
library(spdep)

#Upload Census Tract Shapefile 
setwd("/Users/ahillard/Desktop/Chicago-Crime-Data-Analysis/Boundaries - Census Tracts - 2010")
chicago <- readShapePoly("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
data.chicago = attr(chicago, "data")

#Create Adjacency Graph
setwd("/Users/ahillard/Desktop/Chicago-Crime-Data-Analysis/INLA")
zzz <- poly2nb(chicago)
nb2INLA("Chicago.graph", zzz)
#Creates file called "Chciago.adj" with the graph for INLA
Chicago.adj <- paste(getwd(),"/Chicago.graph",sep="")

#Prepare data frame that has right ordering of tracts by month and year. Will use to merge with crime_serious
#Dates are from January 2010 to August 2016, 80 Months Total 
#Let's Constrain it from January 2010 to December 2015, 72 Months Total
6*12
tracts <- data.chicago$tractce10
tracts <-  rep(tracts, 72)
length(tracts) #57672

year = rep(2010:2015, each=801*12)
month = rep(rep(1:12, each=801), 6)
time = rep(1:72, each=801)

tracts_time <-  data.frame(tractce10=tracts, tract=as.factor(as.numeric(as.character(tracts))), year, month, time, id = 1:length(tracts))
#Have to create two tracts, one to use to merge with crime_serious, the other for data.chicago
#Merge crime_serious with tracts_time to give data in right order with census tracts that have no data

data <- merge(tracts_time, crime_serious[as.numeric(crime_serious$year)<7,], by=c("tract","year","month"), all.x=T)
data <- data[order(data$id),]
data <- data[,!(names(data) %in% c("id","tract"))]

#Create year, area/year, and area variables
data$ID.area <- as.numeric(data$tractce10)
data$ID.area1 <- as.numeric(data$tractce10)

data$ID.time <- data$time
data$ID.time1 <- data$time

data$ID.area.time <- seq(1, nrow(data))

data$serious <- as.numeric(data$serious)-1 #Needs to be numeric and zero or one 

#Split into test and training set
data_train <- data[data$year < 2014,]  

data_test <-  data[data$year >= 2014,]
data_test_response <- data_test$serious
response_index <- !is.na(data_test_response)
data_test$serious <- NA
#For Predictions, subset 38449:nrow and then only look at predictions[38499:nrow,1][response_index] and data_test_response[response_index]

#Merge Together with NAs in data_test
data1 <- rbind(data_train, data_test)
data1$ID.time <- as.numeric(data1$ID.time)
data1$ID.time1 <- as.numeric(data1$ID.time1)

#############################################################
#Run Spatio-Temporal Effects with No Covariates

library(INLA)

#Parametric model alpha + csii + (deltai + beta)*year
formula1 <- serious ~ 1 + f(ID.area, model="bym", graph=Chicago.adj) + 
  f(ID.area1,ID.time,model="iid") + time

stmodel1 <- inla(formula1,
                 family="binomial",
                 data=data1,
                 #control.predictor=list(compute=TRUE),
                 control.compute=list(dic=TRUE,cpo=TRUE),
                 control.predictor=list(link = 1) #Refers to first family listed in family=, or, in this case, 'binomial'
                 #control.family = list(link = "logit"),
                 #verbose=TRUE
)

setwd("/Users/ahillard/Desktop/Thesis Data")
save(stmodel1, file="stmodel1.rda")
load("/Users/ahillard/Desktop/Thesis Data/stmodel1.rda")

#Took about 20 Minutes :)

#For Predictions, subset 38449:nrow and then only look at predictions[38499:nrow,1][response_index] and data_test_response[response_index]
fitted.values <- stmodel1$summary.fitted.values
preds <- fitted.values[38449:57672,1][response_index]
response <- data_test_response[response_index]

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, response) 
#Error = 0.1589709
#I got a better spatio-temporal model with no covariates then my best model with covariates


#Non Parametric model alpha + csii + gammaj + phij #No space time interaction yet!
#csii and are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable

formula2 <- serious ~ 1 + f(ID.area, model="bym", graph=Chicago.adj) +
  f(ID.time, model="rw1") + f(ID.time1, model="iid")

#If you test with smaller subset, you need at least two categories for ID.time. 

stmodel2 <- inla(formula2,
                 family="binomial",
                 data=data1,
                 #control.predictor=list(compute=TRUE),
                 control.compute=list(dic=TRUE,cpo=TRUE),
                 control.predictor=list(link = 1) #Refers to first family listed in family=, or, in this case, 'binomial'
                 #control.family = list(link = "logit"),
                 #verbose=TRUE
)

#This Took About an Hour to Run :)

setwd("/Users/ahillard/Desktop/Thesis Data")
save(stmodel2, file="stmodel2.rda")
load("/Users/ahillard/Desktop/Thesis Data/stmodel2.rda")

#For Predictions, subset 38449:nrow and then only look at predictions[38499:nrow,1][response_index] and data_test_response[response_index]
fitted.values <- stmodel2$summary.fitted.values
preds <- fitted.values[38449:57672,1][response_index]
preds_st2 <- ifelse(preds > .5, 1, 0)
response <- data_test_response[response_index]+1

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, response) 
#0.1522774

################################################################################################
#Mapping Predictions
################################################################################################

response_df <- data_test[response_index,][,c("serious","tractce10", "year","month")]
names(response_df) <- c("serious_response","tract","year","month")
response_df$serious_response <- response_df$serious_response+1
response_df$tract <- as.factor(as.numeric(as.character(response_df$tract)))
response_df$serious_preds <- preds_st2
cost(response_df$serious_response, response_df$serious_preds)

library(dplyr)
aggregate(response_df, "tract", sum)

group <- group_by(response_df, tract)
test <- as.data.frame(summarise(group, 
                                wtf=sum(serious_preds)
))


library(rgdal)
library(sp)
library(raster)

#Import Census Tract Boundaries
setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis/Boundaries - Census Tracts - 2010")
tract <- shapefile("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
tract <- spTransform(x=tract, CRSobj=CRS("+init=epsg:4326"))
names(tract@data) <- tolower(names(tract@data))

#Get Centroids of tracts
library(rgeos)
trueCentroids = gCentroid(tract,byid=TRUE) #Has bbox, proj4string, coords, and class data
centroids <- data.frame(tract@data$tractce10, trueCentroids@coords)
names(centroids) <- c("tract","Longitude", "Latitude")
plot(tract)
points(trueCentroids,pch=2, col='red') #I think that worked

#Check that Explore and Centroid tracts are the same
class(explore$tract) #There are no leading zeros 
class(centroids$tract) #There are leading zeros 
centroids$tract <- as.factor(as.numeric(as.character(centroids$tract)))
table(unique(explore$tract) %in% unique(centroids$tract)) #Awesome, that worked 

#Add Centroid Lat Long of Plot
response_df <- merge(response_df, centroids, by="tract")


#Create Plots for 2014
par(mfrow=c(2,2))

#Plot Serious for January, 2014
index_year = (response_df$year == "2014")
jan_2014 <- response_df[index_year,]
index_month = jan_2014$month == "1"
jan_2014 <- jan_2014[index_month,]

#Response January, 2014
serious_one <- SpatialPointsDataFrame(coords=jan_2014[jan_2014$serious_response == 1,][,c("Longitude", "Latitude")], data = jan_2014[jan_2014$serious_response == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jan_2014[jan_2014$serious_response != 1,][,c("Longitude", "Latitude")], data = jan_2014[jan_2014$serious_response != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Actual January 2014")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Prediction January, 2014
serious_one <- SpatialPointsDataFrame(coords=jan_2014[jan_2014$serious_preds == 1,][,c("Longitude", "Latitude")], data = jan_2014[jan_2014$serious_preds == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jan_2014[jan_2014$serious_preds != 1,][,c("Longitude", "Latitude")], data = jan_2014[jan_2014$serious_preds != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Predictions January 2014")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for June, 2014
index_year = (response_df$year == "2014")
jun_2014 <- response_df[index_year,]
index_month = jun_2014$month == "6"
jun_2014 <- jun_2014[index_month,]

#Response June, 2014
serious_one <- SpatialPointsDataFrame(coords=jun_2014[jun_2014$serious_response == 1,][,c("Longitude", "Latitude")], data = jun_2014[jun_2014$serious_response == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jun_2014[jun_2014$serious_response != 1,][,c("Longitude", "Latitude")], data = jun_2014[jun_2014$serious_response != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Actual June 2014")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Prediction June, 2014
serious_one <- SpatialPointsDataFrame(coords=jun_2014[jun_2014$serious_preds == 1,][,c("Longitude", "Latitude")], data = jun_2014[jun_2014$serious_preds == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jun_2014[jun_2014$serious_preds != 1,][,c("Longitude", "Latitude")], data = jun_2014[jun_2014$serious_preds != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Predictions June 2014")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

test <- merge(jun_2014, jun_2015, by="tract")
table(jun_2014$serious_preds == jan_2014$serious_preds)

#Create Plots for 2015
par(mfrow=c(2,2))

#Plot Serious for January, 2015
index_year = (response_df$year == "2015")
jan_2015 <- response_df[index_year,]
index_month = jan_2015$month == "1"
jan_2015 <- jan_2015[index_month,]

#Response January, 2015
serious_one <- SpatialPointsDataFrame(coords=jan_2015[jan_2015$serious_response == 1,][,c("Longitude", "Latitude")], data = jan_2015[jan_2015$serious_response == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jan_2015[jan_2015$serious_response != 1,][,c("Longitude", "Latitude")], data = jan_2015[jan_2015$serious_response != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Actual January 2015")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Prediction January, 2015
serious_one <- SpatialPointsDataFrame(coords=jan_2015[jan_2015$serious_preds == 1,][,c("Longitude", "Latitude")], data = jan_2015[jan_2015$serious_preds == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jan_2015[jan_2015$serious_preds != 1,][,c("Longitude", "Latitude")], data = jan_2015[jan_2015$serious_preds != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Predictions January 2015")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for June, 2015
index_year = (response_df$year == "2015")
jun_2015 <- response_df[index_year,]
index_month = jun_2015$month == "6"
jun_2015 <- jun_2015[index_month,]

#Response June, 2015
serious_one <- SpatialPointsDataFrame(coords=jun_2015[jun_2015$serious_response == 1,][,c("Longitude", "Latitude")], data = jun_2015[jun_2015$serious_response == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jun_2015[jun_2015$serious_response != 1,][,c("Longitude", "Latitude")], data = jun_2015[jun_2015$serious_response != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Actual June 2015")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Prediction June, 2015
serious_one <- SpatialPointsDataFrame(coords=jun_2015[jun_2015$serious_preds == 1,][,c("Longitude", "Latitude")], data = jun_2015[jun_2015$serious_preds == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=jun_2015[jun_2015$serious_preds != 1,][,c("Longitude", "Latitude")], data = jun_2015[jun_2015$serious_preds != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="Predictions June 2015")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Error Map 

error_map <- response_df
error_map$error <- response_df$serious_response!=response_df$serious_preds

library(dplyr)

group <- group_by(error_map, tract)
error_count <- as.data.frame(summarise(group, 
                                error_count = sum(error)
))

error_count <- merge(error_count, centroids, by="tract")

setwd("/Users/ahillard/Desktop/Thesis Data")
write.csv(error_count, "error_count.csv")

setwd("/Volumes/My Passport/Post T-Break Thesis Code")
error_count <- read.csv("error_count.csv")[,-1]

library(maptools)
library(rgdal)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(shapefiles)

setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis/Boundaries - Census Tracts - 2010")
tract <- shapefile("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")

census_tract <- readShapePoly("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
census_tract@data$id <- rownames(census_tract@data)
census_tract.points <- fortify(census_tract, coords="id")
census_tract.df <- join(census_tract.points, census_tract@data, by="id")

census_tract.df$tractce10 <- as.factor(as.numeric(as.character(census_tract.df$tractce10)))
error_count$tract <- as.factor(error_count$tract)
names(error_count) <- c("tractce10","error_count","Longitude","Latitude")

census_error <- merge(census_tract.df, error_count, by="tractce10")

ggplot() + 
geom_polygon(data = census_error, aes(x = long, y = lat, group = group, fill = error_count), color = "black", size = 0.25) +
labs(title = "", x="Longitude", y="Latitude") 



#Non Parametric model alpha + csii + gammaj + phij + deltaij
#csii are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable
#Interaction (deltaij) is modelled as exchangeable

formula3 <- serious ~ 1 + f(ID.area, model="bym",graph=Chicago.adj) +
  f(ID.time, model="rw1") + f(ID.time1, model="iid") + f(ID.area.time, model="iid")

#To obtain the marginal of phij + gammaj we need to create the corresponding linear combinations and include these in the model 
#lcs = inla.make.lincombs(ID.time = diag(72),  ID.time1 = diag(72))

stmodel3 <- inla(formula3,
                 family="binomial",
                 data=data1, 
                 control.compute=list(dic=TRUE,cpo=TRUE),
                 control.predictor=list(link = 1)
                 #lincomb=lcs,
                 #control.inla = list(lincomb.derived.only=TRUE),
                 #verbose=TRUE
                 )

#This Took About an Hour to Run :)

setwd("/Users/ahillard/Desktop/Thesis Data")
save(stmodel3, file="stmodel3.rda")
#load("/Users/ahillard/Desktop/Thesis Data/stmodel2.rda")

#For Predictions, subset 38449:nrow and then only look at predictions[38499:nrow,1][response_index] and data_test_response[response_index]
fitted.values <- stmodel3$summary.fitted.values
preds <- fitted.values[38449:57672,1][response_index]
response <- data_test_response[response_index]

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, response)
#Test Error 0.1522774


#Try to Run Model 2 with covariates

formula4 <- serious ~ 1 + f(ID.area, model="bym", graph=Chicago.adj) +
  f(ID.time, model="rw1") + f(ID.time1, model="iid") + 
  H0060002 + H0130005 + CPI + mean_wind + Earnings


stmodel4 <- inla(formula4,
                 family="binomial",
                 data=data1,
                 #control.predictor=list(compute=TRUE),
                 control.compute=list(dic=TRUE,cpo=TRUE),
                 control.predictor=list(link = 1) #Refers to first family listed in family=, or, in this case, 'binomial'
                 #control.family = list(link = "logit"),
                 #verbose=TRUE
)

#This Took About an Hour to Run :)

setwd("/Users/ahillard/Desktop/Thesis Data")
save(stmodel4, file="stmodel4.rda")
load("/Users/ahillard/Desktop/Thesis Data/stmodel2.rda")

#For Predictions, subset 38449:nrow and then only look at predictions[38499:nrow,1][response_index] and data_test_response[response_index]
fitted.values <- stmodel4$summary.fitted.values
preds <- fitted.values[38449:57672,1][response_index]
response <- data_test_response[response_index]

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, response) 

#Error = 16.46185%



