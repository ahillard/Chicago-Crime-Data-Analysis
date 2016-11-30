#Import Data 
setwd("/Users/ahillard/Desktop/Thesis Data")
crime_serious <- read.csv("crime_serious.csv") #Should have 63758 obs of 485 variables
drop = c("X")
crime_serious <- crime_serious[ , !(names(crime_serious) %in% drop)]

#Convert data that you know is a factor 
crime_serious$month <- as.factor(crime_serious$month)
crime_serious$tract <- as.factor(crime_serious$tract)
crime_serious$year <- as.factor(crime_serious$year)

#Train and Test Dataset for loadedcrime_serious 
crime_serious_train <- crime_serious[as.numeric(crime_serious$year)<5,] #2010 through 2013
crime_serious_test <- crime_serious[(as.numeric(crime_serious$year)>=5) & (as.numeric(crime_serious$year)<7),] #2014 through 2015
explore <- crime_serious[as.numeric(crime_serious$year)<7,]

################################################################################################
#Time Series Graphs 
################################################################################################

#Logistic Regression by Year/Month to Assess Significance of Year/Month

library(glm)

log_year <- glm(serious~year, data=explore, family=binomial)
#The p-values are all significant and the coefficients are all becoming more and more negative  
                
log_month <- glm(serious~month, data=explore, family=binomial)
#Majority of p-values are significant for months
            
par(mfrow=c(1,1))

library(dplyr)

#Count by Year
group <- group_by(explore, year)
year_ts <- as.data.frame(summarise(group, 
                         above_median_count = sum(serious)
))
year_ts$year <- as.numeric(year_ts$year)+2009

barplot(year_ts$above_median_count, 
        names.arg=year_ts$year,
        #main='Total Number of Census Tracts Above the Monthly Median of Serious Crimes Per Capita in Given Year',
        xlab='Year',
        ylab='Number of Census Tracts Above Monthly Median'
        )

#Do Linear Regression to determine Significance
year_time <- lm(above_median_count~year, data=year_ts)
summary(year_time)

#Count by Month 
group <- group_by(explore, month)
month_ts <- as.data.frame(summarise(group, 
                          above_median_count = sum(serious)
))
month_ts$month <- as.numeric(month_ts$month)
barplot(month_ts$above_median_count, 
        names.arg=month_ts$month,
        main='Total Number of Census Tracts Above the Monthly Median of Serious Crimes in Given Month',
        xlab='Month',
        ylab='Number of Census Tracts Above Monthly Median'
)



#Count by Month & Year
group <- group_by(explore, year, month)
year_month_ts <- as.data.frame(summarise(group, 
                                        above_median_count = sum(serious)
))

seq(from, to, by, length.out = NULL, along.with = NULL, ...)
year_month_ts$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="month")
plot(year_month_ts$time, year_month_ts$above_median_count, type='l',
     main='Total Number of Census Tracts Above the Monthly Median of Serious Crimes Over Time',
     xlab='Year',
     ylab='Number of Census Tracts Above Monthly Median'
     )


################################################################################################
#Census Tract Visual for Month by Month in one Year
################################################################################################

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
explore1 <- merge(explore, centroids, by="tract")
keep = c("tract","Longitude","Latitude","year","month","serious")
explore1 <- explore1[,names(explore1) %in% keep]

###########################################################################
#Plots of Four Months in Given Year - 2010
###########################################################################

par(mfrow=c(2,2))
#Plot Serious for January, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for April, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "4"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="April 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for July, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "7"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="July 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for October, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "10"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="October 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

###########################################################################
#Plots of Month over Four Years - June, 2010 through 2014
###########################################################################

par(mfrow=c(2,2))
#Plot Serious for January, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for January, 2011
index_year = (explore1$year == "2011")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2011")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for January, 2012
index_year = (explore1$year == "2012")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2012")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

#Plot Serious for January, 2013
index_year = (explore1$year == "2014")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2013")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)

###########################################################################
#Sum of Counts Across Census Tracts
###########################################################################

#Count by Month & Year
group <- group_by(explore, tract)
tract_count <- as.data.frame(summarise(group, 
                                         above_median_count = sum(serious)
))

par(mfrow=c(1,1))
hist(tract_count$above_median_count,
     breaks=100,
     main="Sum of Serious Crime per Capita that is Above the Median Level for 2010 - 2014 across all Census Tracts",
     xlab="Above Median Count",
     ylab = "Frequency")

###########################################################################
#Weather Across Sum of Counts - Mean_temp, mean_hum, precipitation
###########################################################################

par(mfrow=c(1,1))

explore$serious <- as.factor(explore$serious)
names(explore)[462:478]

boxplot(max_temp~serious,data=explore)
boxplot(mean_temp~serious,data=explore, ylab="Mean Temperature", xlab="Response Variable") #Do This One
boxplot(min_temp~serious,data=explore)

boxplot(max_dew~serious,data=explore)

boxplot(mean_dew~serious,data=explore) #Do This One
wilcox.test(mean_dew~serious, data=explore)

boxplot(min_dew~serious,data=explore)

boxplot(max_hum~serious,data=explore)
boxplot(mean_hum~serious,data=explore)
boxplot(min_hum~serious,data=explore)

boxplot(max_vis_miles~serious,data=explore)
boxplot(min_vis_miles~serious,data=explore)
boxplot(mean_vis_miles~serious,data=explore)

boxplot(max_wind~serious,data=explore)
boxplot(mean_wind~serious,data=explore)

boxplot(precipitation~serious,data=explore)

boxplot(cloud_cover~serious,data=explore, ylab="Cloud Cover Index", xlab="Response Variable") #Do this one 
wilcox.test(cloud_cover~serious, data=explore)

boxplot(winddirdegrees~serious,data=explore)

###########################################################################
#Select Census Variables 
###########################################################################

names(explore)[5:461]

boxplot(P0010001~serious,data=explore) #Population Alone, Do this one 

boxplot(P0030002~serious,data=explore, ylab="White Population", xlab="Response Variable") #White Population, Do this one 
wilcox.test(P0030002~serious, data=explore)

boxplot(P0030003~serious,data=explore) #Black Population
boxplot(P0040002~serious,data=explore) #Not Hispanice or Latino Population 
boxplot(P0040003~serious,data=explore) #Hispanic or Latino Population

head(explore)

boxplot(H0110001~serious,data=explore) #Total Population in Occupied Housing Units
boxplot(H0130002~serious,data=explore) #Population in 1-person household

boxplot(H0140003~serious,data=explore) #Householder who is white alone, Do this one
wilcox.test(H0140003~serious, data=explore)

boxplot(H0140004~serious,data=explore) #Householder who is black alone
boxplot(H0190003~serious,data=explore, ylab="Population of Owners who Occupy with Children less than 18 years old", xlab="Response Variable") #Total Owner occupied occupant with children under 18 years, Do this one 

###############################################################################
#BLS Indicators - CPI, Unemployment, Earnings - Plot next to month crime trends
###############################################################################

par(mfrow=c(2,2))

#Crime Count over Months
group <- group_by(explore, year, month)
year_month_ts <- as.data.frame(summarise(group, 
                                         above_median_count = sum(serious)
))

seq(from, to, by, length.out = NULL, along.with = NULL, ...)
year_month_ts$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="month")
plot(year_month_ts$time, year_month_ts$above_median_count, type='l',
     main='Number of Tracts Above Monthly Median of Serious Crimes',
     xlab='Month',
     ylab='Number of Census Tracts Above Monthly Median'
)

#CPI Graph

explore_cpi <- unique(explore[,names(explore) %in% c("CPI","year","month")])
explore_cpi <- explore_cpi[order(explore_cpi$year, explore_cpi$month),]
explore_cpi$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="month")

plot(explore_cpi$time, explore_cpi$CPI, type='l',
     main='CPI over Time',
     xlab='Month',
     ylab='CPI'
)


#Unemployment
explore_une <- unique(explore[,names(explore) %in% c("Unemployment","year","month")])
explore_une <- explore_une[order(explore_une$year, explore_une$month),]
explore_une$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="month")

plot(explore_une$time, explore_une$Unemployment, type='l',
     main='Unemployment over Time',
     xlab='Month',
     ylab='Unemployment'
)

#Earnings Graph
explore_ear <- unique(explore[,names(explore) %in% c("Earnings","year","month")])
explore_ear <- explore_ear[order(explore_ear$year, explore_ear$month),]
explore_ear$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="month")

plot(explore_ear$time, explore_ear$Earnings, type='l',
     main='Earnings over Time',
     xlab='Month',
     ylab='Earnings'
)

###############################################################################
#Population Indicators - CPI, Unemployment, Earnings - Plot next to month crime trends
###############################################################################

par(mfrow=c(1,2))

#Count by Year
group <- group_by(explore, year)
year_ts <- as.data.frame(summarise(group, 
                                   above_median_count = sum(serious)
))

year_ts$year <- as.numeric(year_ts$year)+2009
plot(year_ts, type='b', 
     main='Census Tracts Above Monthly Serious Crime Median by Year',
     xlab='Year',
     ylab='Number of Census Tracts Above Monthly Median'
)

#Population Graph 

pop <- unique(explore[,names(explore) %in% c("population","year")])
pop$time <- seq(from=as.Date("2010/1/1"), to=as.Date("2015/12/1"), by="year")

plot(pop$time, pop$population, type='l',
     main='Population over Time',
     xlab='Year',
     ylab='Population'
)






###########################################################################
#Plots of Month over Four Years - June, 2010 through 2014
###########################################################################

data_test_plot <- data_test[,c]

par(mfrow=c(2,2))
#Plot Serious for January, 2010
index_year = (explore1$year == "2010")
explore_2010 <- explore1[index_year,]
index_month = explore_2010$month == "1"
explore_2010_1 <- explore_2010[index_month,]

serious_one <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious == 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious == 1,], proj4string=CRS("+init=epsg:4326"))
serious_zero <- SpatialPointsDataFrame(coords=explore_2010_1[explore_2010_1$serious != 1,][,c("Longitude", "Latitude")], data = explore_2010_1[explore_2010_1$serious != 1,], proj4string=CRS("+init=epsg:4326"))
plot(tract, main="January 2010")
points(serious_one, col="red", pch=19)
points(serious_zero, col="blue", pch=19)



