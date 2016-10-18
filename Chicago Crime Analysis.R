setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis")
crime <- read.csv("Crimes_-_2001_to_present.csv")
crim <- crime
crim <- crim[complete.cases(crim),]

#################################################
#Load Spatial Packages
install.packages("rgdal")
install.packages("sp")
install.packages("raster")
library(rgdal)
library(sp)
library(raster)

#Load Chicago Census Tracts
setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis/Boundaries - Census Tracts - 2010")
tract <- shapefile("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
tract <- spTransform(x=tract, CRSobj=CRS("+init=epsg:4326"))
names(tract@data) <- tolower(names(tract@data))
plot(tract) #check that tract boundaries are coming in correct

##Make Crime Data to Spatial Object
spatial_crim <- SpatialPointsDataFrame(coords=crim[,c("Longitude", "Latitude")], data = crim, proj4string=CRS("+init=epsg:4326"))

##Overlay Census Tract Polygon where Each Crime Happened
overlay <- over(x=spatial_crim, y=tract)

##Add Census Track to each Observation
crim <- cbind(crim, overlay$tractce10)

#######Add time to Crim Data##########
##Add Month and Week Day to each Observation
crim$Date <- as.character(crim$Date)
crim$Date <- substr(crim$Date,1,nchar(crim$Date)-12)
crim$Date <- as.Date(crim$Date, "%m/%d/%Y")

install.packages("lubridate")
install.packages("stringi")
library(stringi)
library(lubridate)

crim$month <- month(crim$Date) 
crim$day <- wday(crim$Date)
crim$year <- year(crim$Date)

#Set Columns as Specific Data Type 
crim2 <- crim
names(crim2)[23] <- "tract_id"
crim2$Latitude <- as.numeric(crim2$Latitude)
crim2$Longitude <- as.numeric(crim2$Longitude)
crim2$tract_id <- as.factor(crim2$tract_id)
crim2$month <- as.factor(crim2$month)
crim2$day <- as.factor(crim2$day)
crim2$year <- as.factor(crim2$year)

#Aggregate Data by Crime Type 

crim_agg <- crim2[,c("Date","Location.Description","Primary.Type","FBI.Code","Latitude","Longitude","tract_id","month","day","year")]
crim_agg$Primary.Type <- as.character(crim_agg$Primary.Type)
crim_agg$FBI.Code <- as.character(crim_agg$FBI.Code)

#http://gis.chicagopolice.org/clearmap_crime_sums/crime_types.html
#Three Types of Crimes
#Crimes against Person, Crimes against Property, and Crimes against Society

fbi_code <- data.frame(
        c("01A","01B","02","03","04A","04B","05","06","07","08A","08B","09","10","11","12","13","14","15","16","17","18","19","20","22","24","26"),       
        c("Persons","Persons","Persons","Property","Persons","Persons","Property","Property","Property",
          "Persons","Persons","Property","Property","Property","Property","Property","Property","Society",
          "Society","Persons and Society","Society","Society","Persons and Society","Society","Society",
          "Society"),
        c("More Serious","Less Serious","More Serious","More Serious","More Serious","More Serious",
          "More Serious","More Serious","More Serious","Less Serious","Less Serious","More Serious",
          "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
          "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
          "Less Serious","Less Serious")
)

colnames(fbi_code) <- c("FBI.Code","Classification", "Seriousness")

crim_agg1 = merge(crim_agg, fbi_code, by="FBI.Code")

crim_agg1$Persons = 0
crim_agg1$Persons[crim_agg1$Classification == "Persons"] <- 1
crim_agg1$Persons[crim_agg1$Classification == "Persons and Society"] <- 1

crim_agg1$Property = 0
crim_agg1$Property[crim_agg1$Classification == "Property"] <- 1

crim_agg1$Society = 0
crim_agg1$Society[crim_agg1$Classification == "Society"] <- 1
crim_agg1$Society[crim_agg1$Classification == "Persons and Society"] <- 1

crim_agg1$Serious = 0
crim_agg1$Serious[crim_agg1$Seriousness == "More Serious"] <- 1

crim_agg1 <- crim_agg1[complete.cases(crim_agg1),]
save1 <- crim_agg1

#########################################################################
##Get more Census Data: Education, Race, Employment, Poverty Level

#########################################################################
##Add Census Data to each Observation 
#B19013, found table using advanced search, geography census blocks 

install.packages("tigris")
install.packages("acs")
install.packages("stringr")

library(tigris)
library(acs)
library(stringr) #to pad fips codes

##Set counties that we're interested in getting

county_fip <- c(17031) ##Cook County FIPS code is 17031 
tracts <- tracts(state = 'IL', county = c(31), cb=TRUE) ##Could also pass FIPS code for state into function in place of NY

##Create Census Data API http://api.census.gov/data/key_signup.html

api.key.install(key="292acd96a6f97432d59e3572e86dcd54fe140c2a")

#Pull Census Data from Table of Interest, "pretty" gives full column names
geo <- geo.make(state=c("IL"), county=c(31), tract="*") ##Set up region we're interested in. "*" is a wildcard for all tracts that fit county FIPS. 
income <- acs.fetch(endyear = 2014, span=5, geography=geo, table.number="B19013", col.names="pretty") 
names(attributes(income)) ##View names of list
attr(income, "acs.colnames") ##View names of acs.colnames. That will give you what this survey is about. 

##Convert income to data frame. 

income_df <- data.frame(income@geography$tract, income@estimate)
names(income_df) <- c("tract_id","median_income")
income_df <- data.frame(income_df$tract, income_df$median_income)
names(income_df) <- c("tract_id","median_income")

table(unique(income_df$tract)) #sanity check
table(unique(crim[,"overlay$tractce10"]))

#Merge Income Data into Table
crim_agg1 <- merge(crim_agg1, income_df, by.x="tract_id")

library(tigris)
library(acs)
library(stringr) #to pad fips codes

#Set up geography. Already setup API and FIPS code from previous pull for median income. 
geography <- geo.make(state=c("IL"), county=c(31), tract="*") ##Set up region we're interested in. "*" is a wildcard for all tracts that fit county FIPS. 

#Pull EDUCATION Data from ACS
education <- acs.fetch(endyear = 2014, span=5, geography=geo, table.number="B15003", col.names="pretty", dataset="acs") 
names(attributes(education)) ##View names of list
attr(education, "acs.colnames") ##View names of acs.colnames. That will give you what this survey is about. 

##Convert Education to data frame. 

education_df <- data.frame(education@geography$tract, education@estimate)
education_df <- education_df[,c(1:2,22:26)]
names(education_df) <- c("tract_id","total","AS","BA","MA","PRO","DOC")
education_df$HS_below <- 1-((education_df$AS+education_df$BA+education_df$MA+education_df$PRO+education_df$DOC)/education_df$total)
edu_df <- data.frame(education_df$tract_id, education_df$HS_below)
names(edu_df) <- c("tract_id","HS_below")

#Merge Education Data into Table
crim_agg1 <- merge(crim_agg1, edu_df, by.x="tract_id")

#Pull RACE Data from ACS
race <- acs.fetch(endyear = 2014, span=5, geography=geo, table.number="B02001", col.names="pretty", dataset="acs") 
attr(race, "acs.colnames") ##View names of acs.colnames. That will give you what this survey is about. 

##Convert RACE to data frame. 

race_df <- data.frame(race@geography$tract, race@estimate)
race_df <- race_df[,c(1:3)]
names(race_df) <- c("tract_id","total","white_num")
race_df$white <- (race_df$white_num / race_df$total)
race_df <- data.frame(race_df$tract_id, race_df$white)
names(race_df) <- c("tract_id","white")

#Merge RACE Data into Table
crim_agg1 <- merge(crim_agg1, race_df, by.x="tract_id")

#Pull UNEMPLOYMENT Data from ACS
unemploy <- acs.fetch(endyear = 2014, span=5, geography=geo, table.number="B23025", col.names="pretty", dataset="acs") 
attr(unemploy, "acs.colnames") ##View names of acs.colnames. That will give you what this survey is about. 

##Convert UNEMPLOYMENT to data frame. 

unemploy_df <- data.frame(unemploy@geography$tract, unemploy@estimate)
unemploy_df <- unemploy_df[,c(1:2, 6, 8)]
names(unemploy_df) <- c("tract_id","total","unemp","nilf")
unemploy_df$unemployed <- (unemploy_df$unemp / unemploy_df$total)
unemploy_df$not_in_labor_force <- (unemploy_df$nilf / unemploy_df$total)
unemploy_df <- data.frame(unemploy_df$tract_id, unemploy_df$unemployed, unemploy_df$not_in_labor_force)
names(unemploy_df) <- c("tract_id","unemployed","not_in_labor_force")

#Merge UNEMPLOYMENT Data into Table
crim_agg1 <- merge(crim_agg1, unemploy_df, by.x="tract_id")

#Pull POVERTY Data from ACS
poverty <- acs.fetch(endyear = 2014, span=5, geography=geo, table.number="B17020", col.names="pretty", dataset="acs") 
attr(poverty, "acs.colnames") ##View names of acs.colnames. That will give you what this survey is about. 

##Convert POVERTY to data frame. 

poverty_df <- data.frame(poverty@geography$tract, poverty@estimate)
poverty_df <- poverty_df[,c(1:3)]
names(poverty_df) <- c("tract_id","total","pov")
poverty_df$poverty <- (poverty_df$pov / poverty_df$total)
poverty_df <- data.frame(poverty_df$tract_id, poverty_df$poverty)
names(poverty_df) <- c("tract_id","poverty")

#Merge POVERTY Data into Table
crim_agg1 <- merge(crim_agg1, poverty_df, by.x="tract_id")

#Dataset for Spatio-Temporal Model 
crimtime <- crim_agg1
crimtime$year <- as.numeric(crimtime$year)

#####################################################
#Summarize by Census Tract 

sum_predictors <- data.frame(crim_agg1$tract_id,crim_agg1$Persons, crim_agg1$Property, 
                             crim_agg1$Society, crim_agg1$Serious)
names(sum_predictors) <- c("tract_id", "Persons", "Property", "Society", "Serious")
install.packages("dplyr")
library(dplyr)
grouped <- group_by(sum_predictors, tract_id)
sum_predictors <- as.data.frame(summarise(grouped, sum_persons = sum(Persons),
                                          sum_property = sum(Property), sum_Society = sum(Society),
                                          sum_serious=sum(Serious)))

number_crimes <- as.data.frame(table(crim_agg1$tract_id))
names(number_crimes) <- c("tract_id","total")

p_pred <- merge(sum_predictors, number_crimes, by.x="tract_id")
p_pred$p_persons <- p_pred$sum_persons / p_pred$total
p_pred$p_property <- p_pred$sum_property / p_pred$total
p_pred$p_society <- p_pred$sum_Society / p_pred$total
p_pred$p_serious <- p_pred$sum_serious / p_pred$total

p_pred$abovemed_persons <- 0
p_pred$abovemed_persons[p_pred$p_persons > median(p_pred$p_persons)] <- 1

p_pred$abovemed_property <- 0
p_pred$abovemed_property[p_pred$p_property > median(p_pred$p_property)] <- 1

p_pred$abovemed_society <- 0
p_pred$abovemed_society[p_pred$p_society > median(p_pred$p_society)] <- 1

p_pred$abovemed_serious <- 0
p_pred$abovemed_serious[p_pred$p_serious > median(p_pred$p_serious)] <- 1

p_pred1 <- p_pred[,c("tract_id","abovemed_persons","abovemed_property","abovemed_society",
                     "abovemed_serious")]

test <- merge(crim_agg1, p_pred1, by="tract_id")
tmp2 <- test[, c("tract_id", "abovemed_persons","abovemed_property","abovemed_society",
                      "abovemed_serious","median_income","HS_below","white","unemployed",
                      "not_in_labor_force","poverty")]
tmp3 <- crim_agg1
crim_agg1 <- tmp2 
crim_agg1 <- unique(crim_agg1)
crim_agg1 <- crim_agg1[complete.cases(crim_agg1),]

####RUN SOME MODELS 
#Let's try above median % crimes are serious, or below average % crimes are serious. 
#Table for each tract. Number of crimes total. 
#Crimes that are serious. All the 1's. 

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Serious Predictor

install.packages("boot")
library(boot)
set.seed(1)
log_model_serious1 <- glm(abovemed_serious ~ median_income+HS_below+white+unemployed+not_in_labor_force+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_serious1, cost, K=10)$delta[1] 
cv.err #0.2431078 Classification Error

set.seed(1)
log_model_serious1 <- glm(abovemed_serious ~ HS_below+unemployed+not_in_labor_force+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_serious1, cost, K=10)$delta[1] 
cv.err #0.2406015 Classification Error

exp(coefficients(log_model_serious1))

install.packages("glmnet")
library(glmnet)
x = model.matrix(abovemed_serious~median_income+HS_below+white+unemployed+not_in_labor_force+poverty, crim_agg1)[,-1]
y = crim_agg1$abovemed_serious
lasso_mod_serious <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1) #Default is 10 fold cross validation 
bestlam = cv.out$lambda.min
bestlam_index = which(cv.out$lambda == bestlam)
cv.out$cvm[bestlam_index] #Misclassification CV is 16.09395%
predict(lasso_mod_serious, s=bestlam, type="coefficients")

#######Logistic Regressions without Spatial Dependence
#Persons Predictor

set.seed(1)
log_model_persons1 <- glm(abovemed_persons ~ median_income+HS_below+white+unemployed+not_in_labor_force+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_persons1, cost, K=10)$delta[1] 
cv.err #19.29825% Classification Error

set.seed(1)
log_model_persons2 <- glm(abovemed_persons ~ median_income+HS_below+white+unemployed+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_persons2, cost, K=10)$delta[1] 
cv.err #18.67168% Classification Error

exp(coefficients(log_model_persons2))

x = model.matrix(abovemed_persons~median_income+HS_below+white+unemployed+not_in_labor_force+poverty, crim_agg1)[,-1]
y = crim_agg1$abovemed_persons
lasso_mod_persons <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1) #Default is 10 fold cross validation 
bestlam = cv.out$lambda.min
bestlam_index = which(cv.out$lambda == bestlam)
cv.out$cvm[bestlam_index] #Misclassification CV is 14.17588%
predict(lasso_mod_persons, s=bestlam, type="coefficients")

#Property Predictor

set.seed(1)
log_model_property1 <- glm(abovemed_property ~ median_income+HS_below+white+unemployed+not_in_labor_force+poverty, 
                           data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_property1, cost, K=10)$delta[1] 
cv.err #16.16541% Classification Error

set.seed(1)
log_model_property2 <- glm(abovemed_property ~ HS_below+white+unemployed+poverty, 
                           data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_property2, cost, K=10)$delta[1] 
cv.err #16.54135% Classification Error

exp(coefficients(log_model_property2))

x = model.matrix(abovemed_property~median_income+HS_below+white+unemployed+not_in_labor_force+poverty, crim_agg1)[,-1]
y = crim_agg1$abovemed_property
lasso_mod_property <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1) #Default is 10 fold cross validation 
bestlam = cv.out$lambda.min
bestlam_index = which(cv.out$lambda == bestlam)
cv.out$cvm[bestlam_index] #Misclassification CV is 12.19162%
predict(lasso_mod_property, s=bestlam, type="coefficients")

#Society Predictor

set.seed(1)
log_model_society1 <- glm(abovemed_society ~ median_income+HS_below+white+unemployed+not_in_labor_force+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_society1, cost, K=10)$delta[1] 
cv.err #18.42105% Classification Error

set.seed(1)
log_model_society2 <- glm(abovemed_society ~ HS_below+white+poverty, 
                          data=crim_agg1, family=binomial)
cv.err = cv.glm(crim_agg1, log_model_society2, cost, K=10)$delta[1] 
cv.err #18.42105% Classification Error

exp(coefficients(log_model_society2))

x = model.matrix(abovemed_society~median_income+HS_below+white+unemployed+not_in_labor_force+poverty, crim_agg1)[,-1]
y = crim_agg1$abovemed_society
lasso_mod_society <- glmnet(x, y, alpha=1, family='binomial')
set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1) #Default is 10 fold cross validation 
bestlam = cv.out$lambda.min
bestlam_index = which(cv.out$lambda == bestlam)
cv.out$cvm[bestlam_index] #Misclassification CV is 13.67617%
predict(lasso_mod_society, s=bestlam, type="coefficients")

#Get Centroids of tracts
library(rgeos)
trueCentroids = gCentroid(tract,byid=TRUE) #Has bbox, proj4string, coords, and class data
centroids <- data.frame(tract@data$tractce10, trueCentroids@coords)
names(centroids) <- c("tract_id","Longitude", "Latitude")
plot(tract)
points(trueCentroids,pch=2, col='red') #I think that worked

#Add Centroid Lat Long of Plot
crim_agg1 <- merge(crim_agg1, centroids, by="tract_id")

#Plot Predictors Spatially to see if there is possible Spatial Dependence

par(mfrow=c(2,2))
#SERIOUS
serious_one <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_serious == 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_serious != 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract)
points(serious_one, col="red")
points(serious_zero, col="blue")

#PERSONS
persons_one <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_persons == 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_persons == 1,], proj4string=CRS("+init=epsg:4326"))
persons_zero <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_persons != 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_persons != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract)
points(persons_one, col="red")
points(persons_zero, col="blue")

#PROPERTY
property_one <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_property == 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_property == 1,], proj4string=CRS("+init=epsg:4326"))
property_zero <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_property != 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_property != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract)
points(property_one, col="red")
points(property_zero, col="blue")

#SOCIETY

society_one <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_society == 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_society == 1,], proj4string=CRS("+init=epsg:4326"))
society_zero <- SpatialPointsDataFrame(coords=crim_agg1[crim_agg1$abovemed_society != 1,][,c("Longitude", "Latitude")], data = crim_agg1[crim_agg1$abovemed_society != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract)
points(society_one, col="red")
points(society_zero, col="blue")

#There is definitely a spatial relationship
#I think I need to do crime per capita so it relates to risk of person, not
#percent of crime. 

##############Attempt at using geoRglm

#First, need to build geodata object
#Build object with coords, class, and data

cri <- list(coords=crim_agg1[12:13], 
            covariates= crim_agg1[,6:11],
            data=crim_agg1$abovemed_serious
            )
attr(cri, "class") <- "geodata"

#Create model statement for glsm.mcmc

trend <- trend.spatial(geodata=cri, trend=~median_income+HS_below+white+unemployed+not_in_labor_force) #when I add poverty not working
model1 <- list(trend=~median_income+HS_below+white+not_in_labor_force, cov.pars = c(1,1), 
                   beta=c(1,1,1,1,1) ,family="binomial") #what is beta? how to decide cov.pars for model? what is s.scale? How to decide?
mcmc_tune <- mcmc.control(S.scale=.000025, thin=1) #Played with it until I got close to 60% optimization
test1.mcmc <- glsm.mcmc(cri, coords=cri$coords, data=cri$data, 
                        model=model1, mcmc.input=mcmc_tune)

#For cov.pars think about biggest distance between centroids, plot distances, and see where distances level off. This is for decay parameter. 
#For variance, first cov.pars parameter, put vague parameter. 

#Check Convergence
library(coda)
test1.mcmc.c <- create.mcmc.coda(test1.mcmc, mcmc.input = mcmc_tune)
test1.mcmc.c <- create.mcmc.coda(test1.mcmc$simulations[1, ], mcmc.input = mcmc_tune) #Just looking at 45th location
par(mfrow = c(1, 2))
plot(test1.mcmc.c, density=FALSE, ask=FALSE, auto.layout=FALSE) #Burnin Period? Why does it just level off?
autocorr.plot(test1.mcmc.c, ask = FALSE, auto.layout = FALSE)

#Prediction

out2 <- output.glm.control(sim.predict = TRUE)
pred.test1 <- glsm.krige(test1.mcmc, locations = c(-87.66984 42.02123), output = out2) #Need to add trend?
names(pred.test2)
pred.test2$predict
pred.test2$mcmc.error

##########################################
#Attempt at using INLA
#Let's try INLA package to create spatially dependent model

install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
install.packages("sp")
library(sp)
library(INLA)

#Load the package for building the map and import the shapefile
install.packages("maptools")
install.packages("spdep")
install.packages("rgeos")
library(rgeos)
library(maptools)
library(spdep)

chicago.gen = readShapePoly("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
data <- crim_agg1
names(data)[1] <- "tractce10"
Nareas <- length(data[,1])

#In this section we create the adjaceny graph 
temp <- poly2nb(chicago.gen)
setwd("/Volumes/My Passport/Documents/2016 Fall Semester/Thesis/INLA") #Put adjancency graph in INLA folder
nb2INLA("CHI.graph", temp)

#This creates a file name called "CHI=INLA.adj" with the graph for INLA
CHI.adj <- paste(getwd(),"/CHI.graph", sep="")

#The order of the areas need to be the same between the data and the spatial polygon object
#obtained importing the shapefile, so we reorder the data. 

tracts = chicago.gen
data.tracts = attr(tracts, "data")
order <- match(data.tracts$tractce10, data$tractce10)
data <- data[order,]
ID <- seq(1,801) #the number of tracts
data <- cbind(ID, data)

#We now prepare the BYM model and run INLA
formula <- abovemed_persons~1+f(ID, model="bym", graph=CHI.adj)+HS_below+unemployed+not_in_labor_force+poverty #Used predictors from best logistic regression
mod <- inla(formula, family="binomial", data=data, control.predictor=list(compute=TRUE),control.compute=list(link = 1)) #The link = 1 sets it to the first family link, which for binomial is logit.
#Need to read more about CPO value
preds <- mod$summary.fitted.values

#Finally we obtain the proportion of variance explained by the spatially structured component 
#upsilon, taking the structured effect upsilon and calculating the empirical variance. 
#First we create a matrix with rows equal to the number of areas and 1000 columns. 
#Then for each area we extract 1000 values from the corresponding marginal distribution of upsilon 
#and finally we calculate the empirical variance. We also extract the expected value of the variance 
#for the unstructured component and build the spatial fractional variance.
mat.marg<-matrix(NA, nrow=Nareas, ncol=1000)
m<-mod$marginals.random$ID
for (i in 1:Nareas){
  u<-m[[Nareas+i]]
  s<-inla.rmarginal(1000, u)
  mat.marg[i,]<-s}
var.RRspatial<-mean(apply(mat.marg, 2, sd))^2
var.RRhet<-inla.emarginal(function(x) 1/x,
                          mod$marginals.hyper$"Precision for ID (iid component)")
var.RRspatial/(var.RRspatial+var.RRhet) #.948596 of variance is attributed to spatial??

#Let's now run 10-fold Cross Validation using inla model 
k=10
set.seed(1)
folds = sample(1:k, nrow(data), replace=TRUE)
cv.errors=matrix(NA,k,1, dimnames=list(NULL, paste("CV")))

#defined formula in for loop above 
#cost function defined above as cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Persons Crime Spatial Dependence Model 
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_persons[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_persons[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]

  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of 19.11291%

#Persons Crim with just spatial, no predictors 
formula <- abovemed_persons~1+f(ID, model="bym", graph=CHI.adj)
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_persons[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_persons[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]
  
  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of 18.10072%

#Property Crime Spatial Dependence Model 
formula <- abovemed_property~1+f(ID, model="bym", graph=CHI.adj)+median_income+HS_below+white+unemployed+poverty #Used predictors from Lasso 
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_property[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_property[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]
  
  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of 16.28057%

#Property Crim with just spatial, no predictors 
formula <- abovemed_property~1+f(ID, model="bym", graph=CHI.adj)
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_property[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_property[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]
  
  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of 15.41068%

#Society Crime Spatial Dependence Model 
formula <- abovemed_society~1+f(ID, model="bym", graph=CHI.adj)+median_income+HS_below+white+unemployed+poverty #Used predictors from Lasso 
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_society[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_society[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]
  
  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of18.45909%

#Society Crim with just spatial, no predictors 
formula <- abovemed_society~1+f(ID, model="bym", graph=CHI.adj)
for (j in 1:k){
  tmp_data <- data
  tmp_data$abovemed_society[folds==j] <- NA
  mod <- inla(formula, family="binomial", data=tmp_data, control.predictor=list(link = 1))
  pred = mod$summary.fitted.values[,1]
  pred = pred[folds==j]
  
  missing_fold <- data$abovemed_society[folds==j]
  na_index <- is.na(missing_fold)
  missing_fold <- missing_fold[!na_index]
  pred = pred[!na_index]
  
  cv.errors[j,1] = cost(missing_fold, pred)
}

mean(cv.errors) #Misclassification of 17.04157%

#####Let's fit some Random Forest Models 

install.packages("randomForest")
library(randomForest)


#Test Run 
model = randomForest(abovemed_persons~median_income+HS_below+white+unemployed+not_in_labor_force+poverty,
                     data=complete_data, ntry=3, importance=TRUE, ntree=1000, do.trace=100)

#Persons Random Forest with 10 Fold Cross Validation
set.seed(1)
complete_data <- data[complete.cases(data),]

#Need to Convert Responses to Factors
complete_data$abovemed_persons <- as.factor(complete_data$abovemed_persons)
complete_data$abovemed_property <- as.factor(complete_data$abovemed_property)
complete_data$abovemed_society <- as.factor(complete_data$abovemed_society)

folds = sample(1:k, nrow(complete_data), replace=TRUE)
cv.errors=matrix(NA,k,1, dimnames=list(NULL, paste("CV")))

k=10
for (j in 1:k){
  train = (folds!=j)
  model = randomForest(abovemed_persons~median_income+white+unemployed+not_in_labor_force+HS_below+poverty,
                       data=complete_data, subset=train, mtry=2)
  preds = (as.numeric(predict(model, newdata=complete_data[!train,])) -1 )
  missing_fold = (as.numeric(complete_data[!train,]$abovemed_persons) -1 )
  cv.errors[j,1] <- mean((preds-missing_fold)^2)
  cv_value <- mean(cv.errors)
}

#17.63245% Misclassification 

#Property Random Forest with 10 Fold Cross Validation


k=10
for (j in 1:k){
  train = (folds!=j)
  model = randomForest(abovemed_property~median_income+white+unemployed+not_in_labor_force+HS_below+poverty,
                       data=complete_data, subset=train, mtry=2)
  preds = (as.numeric(predict(model, newdata=complete_data[!train,])) -1 )
  missing_fold = (as.numeric(complete_data[!train,]$abovemed_property) -1 )
  cv.errors[j,1] <- mean((preds-missing_fold)^2)
  cv_value <- mean(cv.errors)
}

#Misclassification is 14.30241%

#Society Random Forest with 10 Fold Cross Validation


k=10
for (j in 1:k){
  train = (folds!=j)
  model = randomForest(abovemed_society~median_income+white+unemployed+not_in_labor_force+HS_below+poverty,
                       data=complete_data, subset=train, mtry=2)
  preds = (as.numeric(predict(model, newdata=complete_data[!train,])) -1 )
  missing_fold = (as.numeric(complete_data[!train,]$abovemed_society) -1 )
  cv.errors[j,1] <- mean((preds-missing_fold)^2)
  cv_value <- mean(cv.errors)
}

#Misclassification is 16.55709%

########################################################
#Attempt at Spatio-Temporal Model with INLA package

#Prepare Data for INLA
head(crimtime)

#Take crimtime and summarize if above or below median by year  
year_percent <- data.frame(tract_id = crimtime$tract_id,Persons = crimtime$Persons, 
                             Property = crimtime$Property, Society=crimtime$Society, 
                             Serious = crimtime$Serious, year = crimtime$year)
install.packages("dplyr")
library(dplyr)

#Get Percent for Different Crimes by tract_id and year
grouped <- group_by(year_percent, tract_id, year)
year_percent <- as.data.frame(summarise(grouped, p_persons = sum(Persons)/length(Persons),
                                p_property = sum(Property)/length(Property),
                                p_society = sum(Society)/length(Society),
                                p_serious = sum(Serious)/length(Serious)
                                ))

#Get Medians by Year
grouped <- group_by(year_percent, year)
median <- as.data.frame(summarise(grouped, med_person=median(p_persons), 
                                  med_property=median(p_property),
                                  med_society=median(p_society),
                                  med_serious=median(p_serious)
                                  ))

yr_med <- merge(year_percent, median, by="year")

yr_med$abovemed_persons <- 0
yr_med$abovemed_persons[yr_med$p_persons >= yr_med$med_person] <- 1

yr_med$abovemed_property <- 0
yr_med$abovemed_property[yr_med$p_property >= yr_med$med_property] <- 1

yr_med$abovemed_society <- 0
yr_med$abovemed_society[yr_med$p_society >= yr_med$med_society] <- 1

yr_med$abovemed_serious <- 0
yr_med$abovemed_serious[yr_med$p_serious >= yr_med$med_serious] <- 1

year_response <- yr_med[,c("tract_id","year", "abovemed_persons","abovemed_property",
                           "abovemed_society","abovemed_serious")]

acs_predictors <- crimtime[,c("tract_id","median_income","HS_below","white",
                               "unemployed","not_in_labor_force","poverty")]
acs_predictors <- unique(acs_predictors)

data <- merge(year_response, acs_predictors, by="tract_id")
data$tractce101 <- data$tract_id
data$ID.year <- data$year
data$ID.year1 <- data$year
data$ID.area.year <- seq(1, nrow(data))
names(data)[1] <- "tractce10"

#Tracts by Year
grouped <- group_by(data, year)
tract_count <- as.data.frame(summarise(grouped, tract_count = length(tractce10)))
#Years 2004 and On have data for all census tracts in Chicago
tract_count
#Only use years 2004 and greater
data <- data[data$year >= 4, ]

#Prepare Map
library(maptools)
library(sp)
library(spdep)
setwd("/Users/sharonhillard/Desktop/Boundaries - Census Tracts - 2010")
chicago <- readShapePoly("geo_export_8d79b7af-b8e3-419e-973d-93d1e3a4dcf4.shp")
data.chicago = attr(chicago, "data")
#################################################
#Create the graph for adjacencies in INLA
#Need the non thinned sph file to do the adjacency matrix!!!
zzz <- poly2nb(chicago)
nb2INLA("Chicago.graph", zzz)
#this create a file called "Chciago.adj" with the graph for INLA
Chicago.adj <- paste(getwd(),"/Chicago.graph",sep="")

#Order based on the map
data <- data[order(data$year),]
order <- numeric()
for (i in 4:16){
      tmp <- match(data.chicago$tractce10, data[data$year==i,1]) + (i-4)*801
      order <- append(order, tmp)
}

#Check that there are 801 tracts per year
test <- data[order,]
grouped <- group_by(test, year)
t <- as.data.frame(summarise(grouped, tract_count = length(tractce10)))

#I think that worked to order the tracts for each year. 
data <- data[order,]
data <- data.frame(abovemed_persons=data$abovemed_persons,
                   abovemed_property=data$abovemed_property,
                   abovemed_society=data$abovemed_society,
                   abovemed_serious=data$abovemed_serious,
                   tractce10=as.numeric(data$tractce10),
                   tractce101=as.numeric(data$tractce101),
                   year=data$year,
                   ID.year=data$ID.year, 
                   ID.year1=data$ID.year1,
                   ID.area.year=data$ID.area.year,
                   median_income=data$median_income,
                   HS_below=data$HS_below,
                   white=data$white,
                   unemployed=data$unemployed,
                   not_in_labor_force=data$not_in_labor_force,
                   poverty=data$poverty
                   )

train <- data[data$year!=16,]
test <- data[data$year==16,]
test$abovemed_persons <- NA
test$abovemed_property <- NA
test$abovemed_society <- NA
test$abovemed_serious <- NA

data1 <- rbind(train, test)

#############################################################
#--Prepare the model and run inla with spatio-temporal effects but no covariates--#
library(INLA)
#Parametric model alpha + csii + (deltai + beta)*year
formula <- abovemed_persons ~ 1 + f(tractce10, model="bym", graph=Chicago.adj)+
  f(tractce101,year,model="rw1") + (year-mean(year))
stmodel1 <- inla(formula,family="binomial",data=data1, control.predictor=list(link = 1))

na_index <- which(is.na(data1$abovemed_persons))
preds <- stmodel1$summary.fitted.values[,1][na_index]
test_persons <- data[data$year==16,]$abovemed_persons

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, test_persons)
#17.85268

#Non Parametric model alpha + csii + gammaj + phij #No space time interaction yet!
#csii and are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable
formula_2 <- abovemed_persons ~ 1 + f(tractce10, model="bym", graph=Chicago.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid")
stmodel2 <- inla(formula_2,family="binomial",data=data1, control.predictor=list(link = 1))

na_index <- which(is.na(data1$abovemed_persons))
preds <- stmodel2$summary.fitted.values[,1][na_index]
test_persons <- data[data$year==16,]$abovemed_persons

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, test_persons)
#18.72659

#Non Parametric model alpha + csii + gammaj + phij + deltaij
#csii are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable
#Interaction (deltaij) is modelled as exchangeable
formula_3<- abovemed_persons ~ 1 + f(tractce10,model="bym",graph=Chicago.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")

#To obtain the marginal of phij + gammaj we need to create the corresponding linear combinations and include these in the model 
lcs = inla.make.lincombs(ID.year = diag(13),  ID.year1 = diag(13))

stmodel3 <- inla(formula_3,family="binomial",data=data1, control.predictor=list(link = 1),lincomb=lcs,control.inla = list(lincomb.derived.only=TRUE))

#Find MisClassification
na_index <- which(is.na(data1$abovemed_persons))
preds <- stmodel3$summary.fitted.values[,1][na_index]
test_persons <- data[data$year==16,]$abovemed_persons

#set cost function
cost = function(r, pi=0) mean(abs(r-pi) > 0.5)

#Misclassification
cost(preds, test_persons)
#18.72659

#Put the temporal effect  (gammaj+phij) on the natural scale - Not Sure What This Does?
temporal<-lapply(model.inla.ST3$marginals.lincomb.derived, function(X){
  marg <- inla.marginal.transform(function(x) exp(x), X)
  inla.emarginal(mean, marg)
})




