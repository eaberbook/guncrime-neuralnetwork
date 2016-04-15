library(RCurl)
library(MASS)

crime_website <-getURL("https://raw.githubusercontent.com/eaberbook/gunsandcrime/master/gun_crime_data.csv")
gun_crime = read.csv(text = crime_website)

# Firstly, the last row seems naturally problematic (mostly NA)
# Thus we drop the values:

gun_crime <- gun_crime[-(nrow(gun_crime)),]

# Neural networks do not accept null values as input.
# As such, for this example, I deleted a few variables
# that had non-linear tendencies with a lot of missing data.

drops <- c("gunsamm","amhms","domestic_gun_production")
data <- gun_crime[, !(names(gun_crime) %in% drops)]

# However, I noticed that the gun variable (% of households
# owning a gun) is a linear trend, so I extrapolated for it:


# Use an apply function to test for NA data
apply(rape_data,2,function(x) sum(is.na(x)))

