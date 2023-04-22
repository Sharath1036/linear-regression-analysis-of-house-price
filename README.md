# linear-regression-analysis-of-house-price
In the given dataset, we find the relation and equation of house price with other dependent variables

#In this analysis we consider price as a dependent variable while considering other variables as independent variables. We have to find the relation of price with other variables 

require("LiblineaR")
df <- read.csv("D:/Machine Learning/Datasets/House_Price.csv", header=TRUE)
View(df)
summary(df)
![image](https://user-images.githubusercontent.com/83855796/233776657-8ba0fc85-9df5-44cf-a1d2-ec92d8a91a51.png)

#From the analysis we can conclude that there is skewness or outlier issues in three columns i.e. 'crime_rate', 'n_hot_rooms' and 'rainfall'

hist(df$crime_rate)
![crime_rate](https://user-images.githubusercontent.com/83855796/233776526-7946554f-67a2-4822-b9ac-f7febe6e8a2b.png)

pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df)
![scatterplot](https://user-images.githubusercontent.com/83855796/233776107-e5216292-16ec-447b-acf4-1a088448aee6.png)

barplot(table(df$airport))
![airport](https://user-images.githubusercontent.com/83855796/233776131-927a43ee-e3f5-41f4-bf7c-e463e7452b68.png)

barplot(table(df$waterbody))
![waterbody](https://user-images.githubusercontent.com/83855796/233776144-b7c38a83-9832-402f-ad01-27b3a9b947d0.png)

barplot(table(df$bus_ter))
![bus_ter](https://user-images.githubusercontent.com/83855796/233776151-a47b1cfc-4b5e-4343-8887-fc495c824a12.png)

# n_hot_rooms and rainfall are outliers
# n_hos_beds has missing values
# bus_ter is a useless variable since it has only one value
# crime_rate has some other functional relationship with price

df <- df[, -18]#removing bus_ter since it has same value throughout
View(df)


# Outlier Treatment
# We use capping and flooring method. 3xP99 for n_hot_rooms and 0.3xP1 for rainfall
quantile(df$n_hot_rooms,0.99)
UV = 3*quantile(df$n_hot_rooms,0.99)    #15.39952
df$n_hot_rooms[df$n_hot_rooms>UV] <- UV
summary(df$n_hot_rooms)                 

quantile(df$rainfall,0.01)
MN = 0.33*quantile(df$rainfall,0.01)    #20
df$rainfall[df$rainfall>MN] <- MN
summary(df$rainfall)

# Now we find relationship between price and crime_rate
plot(df$price, df$crime_rate) 
![price+crime_rate](https://user-images.githubusercontent.com/83855796/233776322-684199c9-2f5d-4f78-a5e1-afb3ef2e39b3.png)
# The relationship seems to be logarithmic

df$crime_rate <- log(1+df$crime_rate)
plot(df$price, df$crime_rate) 
![linear price+crime_rate](https://user-images.githubusercontent.com/83855796/233776345-05a9e63d-a448-4547-a70b-4e2303a32712.png)
# now the relationship is linear


# As shown in the dataset, there are four dist variables. Instead of getting a seperate relation for each, we find mean of all four to get a single variable
df$avg_dist <- (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4  #a new avg_dist variable is created
View(df)
df <- df[, -7:-10] #removing all dist variables from columns 7 to 10
View(df)

# For variable having non-numeric values, we create dummy variables. Here 'airport' and 'waterbody' have non-numeric values
require("fastDummies")

df$airportYES <- ifelse(df$airport == "YES", 1,0)
df$airportNO <- ifelse(df$airport == "NO", 1,0)
View(df)

df <- dummy_cols(df, select_columns = "waterbody")
View(df)

#Since we need n-1 variables, we delete non required variables 'airportNO' and 'waterbody_Lake and River'
df <- df[, -17]
View(df)
df <- df[, -18]
View(df)

# Also we need to delete waterbody and airport variables to get a numeric co-efficient
df <- df[, -9]
View(df)
df <- df[, -11]
View(df)

# n_hos_beds NA values. To get co-efficient parameters we remove NA values and find the mean
mean(df$n_hos_beds) #NA
mean(df$n_hos_beds, na.rm=TRUE) #7.899767

# Since we got all numeric values, we use Multiple Linear Regression for finding equation of price with all other variables
multiple_model <- lm(price~., data=df)
summary(multiple_model)
![image](https://user-images.githubusercontent.com/83855796/233776489-317b2ccb-bcd8-4a4e-b97f-89f87160a3eb.png)


# The equation for house price is price = -3.043 + 0.128*crime_rate - 0.042*resid_area - 21.307*air_qual - 1.202*avg_dist + 1.239*airportYES + 0.83*waterbody_Lake + 0.582*waterbody_None + 0.408*waterbody_River

# Conclusion
# air_qual, avg_dist, airportYES are significantly impacting house price

