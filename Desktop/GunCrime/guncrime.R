library(RCurl)
library(MASS)
library(corrplot)
library(stats)

crime_website <-getURL("https://raw.githubusercontent.com/eaberbook/gunsandcrime/master/gun_crime_data.csv")
gun_crime = read.csv(text = crime_website)

# Firstly, the last row seems naturally problematic (mostly NA)
# Thus we drop the values:

gun_crime <- gun_crime[-(nrow(gun_crime)),]

# Neural networks do not accept null values as input.

# As such, for this example, I deleted a few variables
# that had non-linear tendencies with a lot of missing data.
# I could not find verifiable online sources to replace this data

drops <- c("gunsamm","amhms","amrmms","domestic_gun_production","hg")
data <- gun_crime[, !(names(gun_crime) %in% drops)]

# However, I noticed that the gun variable (% of households
# owning a gun) is a linear trend, so I extrapolated for it.
# Also did the same for gallup poll of people 
# owning guns:

data$gun[1] <- 34.5
data$gun <- na.approx(data$gun)
data$gallup_owngun <- na.approx(data$gallup_owngun)

# Use an apply function to test for NA data

apply(data,2,function(x) sum(is.na(x)))

# Now we can get into neural network analysis.
# Main goal : Predict the rape variable.

set.seed(123)

# Let's take the years 1980 - 1996 as test, and 1997 - 2013 as
# train


index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(rape~., data=train)

# DIMENSION REDUCTION

# We have a large amount of variables, so
# let us reduce dimensionality with PCA

#### PCA #####
year_lag = 10;
Lag_amt = 1;
neural_net_pred <- array(0,dim=c(nrow(data),ncol(data)))
# random_forest_pred <- array(0,dim=c(nrow(data),ncol(data)))







run_PCA = function(X_train, nrFactors){
  
  myPCA = prcomp(X_train, center = TRUE, scale. = TRUE, retx = TRUE);
  pcaRot = myPCA$rotation
  EIGS_PCA = myPCA$x
  
  q = myPCA$sdev
  
  FACTORS = EIGS_PCA[, 1:nrFactors];
  
  
  return(FACTORS);
}

