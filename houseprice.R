#In this analysis we consider price as a dependent variable while considering other variables as independent variables. We have to find the relation of price with other variables 

require("LiblineaR")
df <- read.csv("D:/Machine Learning/Datasets/House_Price.csv", header=TRUE)
View(df)
summary(df)
# (FIG 1) From the analysis we can conclude that there is skewness or outlier issues in three columns i.e. 'crime_rate', 'n_hot_rooms' and 'rainfall'

hist(df$crime_rate) # FIG 2
pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df) # FIG 3

barplot(table(df$airport)) # FIG 4
barplot(table(df$waterbody)) # FIG 5
barplot(table(df$bus_ter)) # FIG 6

# n_hot_rooms and rainfall are outliers
# n_hos_beds has missing values
# bus_ter is a useless variable since it has only one value
# crime_rate has some other functional relationship with price

df <- df[, -18]#removing bus_ter since it has same value throughout
View(df)


# Outlier Treatment
# We use capping and flooring method. 3xP99 for n_hot_rooms and 0.3xP1 for rainfall
quantile(df$n_hot_rooms,0.99)
UV = 3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>UV] <- UV
summary(df$n_hot_rooms)

quantile(df$rainfall,0.01)
MN = 0.33*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall>MN] <- MN
summary(df$rainfall)

#  Now we find relationship between price and crime_rate
plot(df$price, df$crime_rate) # The relationship seems to be logarithmic (FIG 7)
df$crime_rate <- log(1+df$crime_rate)
plot(df$price, df$crime_rate) #now the relationship is linear (FIG 8)


# As shown in the dataset, there are four dist variables. Instead of getting a seperate relation for each, we find mean of all four to get a single variable
df$avg_dist <- (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4
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
mean(df$n_hos_beds)
mean(df$n_hos_beds, na.rm=TRUE)

# Since we got all numeric values, we use Multiple Linear Regression for finding equation of price with all other variables
multiple_model <- lm(price~., data=df)
summary(multiple_model) # FIG 9

# The equation for house price is price = -3.043 + 0.128*crime_rate - 0.042*resid_area - 21.307*air_qual - 1.202*avg_dist + 1.239*airportYES + 0.83*waterbody_Lake + 0.582*waterbody_None + 0.408*waterbody_River

# Conclusion
# air_qual, avg_dist, airportYES are significantly impacting house price






png(filename = "D:/Udemy Course Notes/Machine Learning/Plots/houseprice plots/linear price+crime_rate.png")
plot(df$price, df$crime_rate)
dev.off()

