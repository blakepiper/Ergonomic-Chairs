library(PerformanceAnalytics)
library(caret)
library(tidyverse)
setwd("/Users/blakefinnegan/Documents/Programming/R/Chair Analytics/")
master = read.csv("chair_data.csv")

## Defining Variables of Interest
seatpan_width = as.numeric(master$Seatpan.Width..in.)
seatpan_depth = as.numeric(master$Seatpan.Depth..in.)
price = as.numeric(master$Listed.Price....)
max_height = as.numeric(master$Max.Seatpan.Height..in.)
min_height = as.numeric(master$Min.Seatpan.Height..in.)
manufacturer = master$Manufacturer
chair_name = master$Seat.Name
id = master$X.

frame = data.frame(id, chair_name, manufacturer, price, seatpan_width, seatpan_depth, max_height, min_height)
numeric_frame = data.frame(price, seatpan_width, seatpan_depth, max_height, min_height)

chart.Correlation(numeric_frame)

## Log Transformations (Out of Curiosity)

log_num_frame = log10(numeric_frame)

hist(seatpan_width)
hist(log_num_frame$seatpan_width)
shapiro.test(log_num_frame$seatpan_width)
shapiro.test(seatpan_width)

hist(seatpan_depth)
hist(log_num_frame$seatpan_depth)
shapiro.test(log_num_frame$seatpan_depth)
shapiro.test(seatpan_depth)

hist(price)
hist(log_num_frame$price)
shapiro.test(log_num_frame$price)
shapiro.test(price)

hist(max_height)
hist(log_num_frame$max_height)
shapiro.test(log_num_frame$max_height)
shapiro.test(max_height)

hist(min_height)
hist(log_num_frame$min_height)
shapiro.test(log_num_frame$min_height)
shapiro.test(min_height)

chart.Correlation(log_num_frame)


## Validation Set Cross Validation Non Transformed Data
set.seed(123)

training.samples = numeric_frame$price %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data = numeric_frame[training.samples, ]
test.data = numeric_frame[-training.samples, ]

model = lm(price ~., data = train.data)
predictions = model %>% predict(test.data)
model_prediction_error = data.frame(R2 = R2(predictions, test.data$price),
                                        RMSE = RMSE(predictions, test.data$price),
                                        MAE = MAE(predictions, test.data$price))
pred_error_rate = RMSE(predictions, test.data$price) / mean(test.data$price)
mod_viz = plot(model)

## Validation Set Cross Validation for Log Transformed Data
set.seed(123)

log.training.samples = log_num_frame$price %>%
  createDataPartition(p = 0.8, list = FALSE)

log.train.data = log_num_frame[log.training.samples, ]
log.test.data = log_num_frame[-log.training.samples, ]

log.model = lm(price ~., data = log.train.data)
log.predictions = log.model %>% predict(log.test.data)
log.model_prediction_error = data.frame(R2 = R2(log.predictions, log.test.data$price),
                                    RMSE = RMSE(log.predictions, log.test.data$price),
                                    MAE = MAE(log.predictions, log.test.data$price))
log_pred_error_rate = RMSE(log.predictions, log.test.data$price) / mean(log.test.data$price)
log_model_viz = plot(log.model)

