library(tidyverse)
library(lubridate)
library(corrplot)
library(caret)
library(neuralnet)
library(randomForest)
rm(list=ls())

# Data Cleaning = This + the other scripts
data <- read_csv("05_data_working/s2_buoy_joined.csv") %>%
    select(-contains("c6"), -ph_nan, -organic_dissolved_oxygen_1_milligrams_per_liter,
           -wind_direction_rads) %>%
    mutate(ndvi = (B8 - B4) / (B8 + B4),
           day_of_year = yday(ymd(paste(year, month, day, sep=" ")))) %>%
    drop_na()

# This data does not have any particularly strong predictors on its own
ggplot(data, aes(day_of_year, ndvi, color=station)) + geom_point()
ggplot(data, aes(x=chlorophylla_rfu, y=ndvi)) + geom_point()

# Toss outliers
chl_upper <- quantile(data$chlorophylla_rfu)[4]
phy_upper <- quantile(data$phycocyanin_rfu)[4]
chl_iqr <- quantile(data$chlorophylla_rfu)[4] - quantile(data$chlorophylla_rfu)[2]
phy_iqr <- quantile(data$phycocyanin_rfu)[4] - quantile(data$phycocyanin_rfu)[2]

data <- data %>%
    filter(phycocyanin_rfu < phy_upper + (1.5*phy_iqr),
           chlorophylla_rfu < chl_upper + (1.5*chl_iqr))

# Correlation matrix shows some physical relationships
cormtx <- cor(data[, 4:20])
corrplot(cormtx)

normdiff <- function(x, y) {
    return ((x - y) / (x + y))
}

# Dimensionality reduction: making all the indices
indices <- data %>%
    select(chlorophylla_rfu, phycocyanin_rfu, contains("B", ignore.case = F)) %>%
    mutate(ari = 1/B3 - 1/B5,
           arvi2  = -0.18 + 1.17 * normdiff(B9, B5),
           bwdrvi = normdiff(0.1*B9, B1),
           bri    = ari / B9,
           cari   = (B5/B4) * ((B5-B3) * 670 / 150) + B4 + (B3-(B5-B3) * 550 / 150) / (((B5-B3)/150^2) + 1)^.5,
           cvi    = B9 * B5 * B3^-2,
           ci     = (B5-B1)/B5,
           gdvi   = B9 - B3,
           evi    = 2.5*((B9-B5) / (B9 + 6 * B5 - 7.5 * B1)+1),
           evi2   = 2.4*((B9-B5) / (B9+B5+1)),
           evi3   = 2.5*((B9-B5) / (B9+2.4*B5+1)),
           gari   = (B9-(B3-(B1-B5))) / (B9-(B3+(B1-B5))),
           gbndvi = (B9 - (B3+B1)) / (B9 + B3 + B1),
           grndvi = normdiff(B9, (B3+B5)),
           ndvi   = normdiff(B8, B4),
           ipvi   = (B9 / (B5+B9))/2 * (ndvi+1),
           logR   = log(B9 / B5),
           mari   = (B3^-1 - B5^-1) * B9,
           bndvi  = normdiff(B9, B1),
           gndvi  = normdiff(B9, B3),
           pndvi  = normdiff(B9, (B3+B5+B1)),
           rbndvi = normdiff(B9, (B5+B1)),
           tndvi  = (ndvi+1)^.5,
           tcari  = 3 * ((B5-B4) - 0.2 * (B5-B3) * (B5/B4)))

# Correlations are stronger with chlorophyll_a than phycocyanin, but individual indices
# do not correlate strongly with either measure while strongly correlating with
# each other.
cormtx2 <- cor(indices)
corrplot(cormtx2, insig = "blank")

# Prediction performance is not very good when using indices as predictors
# Since these are highly correlated, a neural network is a good choice to 
# build a model.
scaled <- scale(indices)
trainIdx <- createDataPartition(data$chlorophylla_rfu, p=0.85, list=F)
trainData <- scaled[trainIdx,]
validData <- scaled[-trainIdx,]

trainInput <- trainData[, 3:15]
trainOutput <- trainData[, 1]

validInput <- validData[, 3:15]
validOutput <- validData[, 1]

# Make the network
nn <- neuralnet(
    trainOutput ~ .,
    data=trainInput,
    hidden=c(5, 3),
    act.fct="logistic",
    linear.output = F
)
plot(nn)
predict <- neuralnet::compute(nn, validInput)

# Get MSE of the data - it's really bad
MSE.nn <- sum((predict$net.result - validOutput) ^ 2)/length(validOutput)
range(scaled[, 1])

# Neural nets are wack, let's try random forest on buoy data and the indices
rf_data <- data %>% select(wind_speed_mean_meters_per_second:B9, -chlorophylla_rfu, -contains("B", ignore.case=F))
rf_train <- rf_data[trainIdx, ]
rf_valid <- rf_data[-trainIdx, ]

# Prediction is just as bad
rf_buoy <- randomForest(phycocyanin_rfu ~ ., rf_train)
varImpPlot(rf_buoy, main="Buoy Model")
plot(rf_buoy)

rf_prediction <- predict(rf_buoy, rf_valid)
rf_buoy_mse <- sum((rf_prediction - rf_valid$phycocyanin_rfu)^2) / length(rf_prediction)

# One last try with the indices
rf2_train <- indices[trainIdx, ]
rf2_valid <- indices[-trainIdx, ]

rf_ind <- randomForest(phycocyanin_rfu ~ . - chlorophylla_rfu, rf2_train)
varImpPlot(rf_ind, main="Satellite Model")
plot(rf_ind)

rf2_prediction <- predict(rf_ind, rf2_valid)
rf_ind_mse <- sum((rf2_prediction - rf_valid$phycocyanin_rfu)^2) / length(rf2_prediction)
range(indices$phycocyanin_rfu)

ggplot() +
    geom_point(aes(rf_buoy$y, rf_buoy$predicted), color="red") +
    geom_point(aes(rf_ind$y, rf_ind$predicted), color="blue") +
    xlab("Actual Phycocyanin RFU") + ylab("Predicted Phycocyanin RFU")
    
