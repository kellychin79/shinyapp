library(dplyr)
library(ggplot2)
library(caret)
library(shiny)
# install.packages('visdat')
library(visdat)
setwd("Z:/kviars/Using R")
#setwd("~/Documents/Purdue/2022 Summer MGMT 590 UR4A/Final Project")
data <- read.csv('lego_sets.csv')
str(data)


#add a indicator if it's a licensed 
data$registered <- grepl('®', data$theme_name, fixed=T)
data$trademark <- grepl('™', data$theme_name, fixed=T) 

#remove special characters
data$theme_name <- gsub('®', '', data$theme_name)
data$theme_name <- gsub('™', '', data$theme_name)

#set blank difficulty to text to avoid n/as
data[data$review_difficulty=="", "review_difficulty"] <- "Blank"

#Set factor columns
factorCol <- c('ages', 'prod_desc', 'theme_name', 'country', 'review_difficulty')
for (i in 1:length(factorCol)) {
  data[, factorCol[i]] <- as.factor(data[, factorCol[i]])
}

#Recode odd characters in ages
data$ages <- gsub('½', '.5', data$ages)

#create min and max age columns
data$min_age <- gsub("-.*", "", data$ages)
data$min_age <- gsub("\\+", "", data$min_age)
data$max_age <- gsub(".*-", "", data$ages)
data$max_age[substr(data$max_age, nchar(data$max_age), nchar(data$max_age))=="+"] = 99
data$min_age <- floor(as.numeric(data$min_age))
data$max_age <- ceiling(as.numeric(data$max_age))

# #add dummy variables for ages
# for (i in 1:99) {
#   data[, as.character(i)] = i <= data$max_age & i >= data$min_age
# }

data$num_reviews[is.na(data$num_reviews)] = 0 #set null reviews to 0

#reorder difficulty levels
data$review_difficulty <- factor(data$review_difficulty, levels=c("Blank", "Very Easy", "Easy", "Average", "Challenging", "Very Challenging"))
#convert difficulty levels to numeric values
data$difficulty <- ifelse(data$review_difficulty=='Very Easy', 1, 
                          ifelse(data$review_difficulty=='Easy', 2, 
                                 ifelse(data$review_difficulty=='Average', 3, 
                                        ifelse(data$review_difficulty=='Challenging', 4, 
                                               ifelse(data$review_difficulty=="Very Challenging", 5, 0)))))

#add price per brick for data exploration
data$price_per_brick <- data$list_price/data$piece_count

# regroup the theme name
data[data$theme_name=='THE LEGO NINJAGO MOVIE', 'theme_name'] = 'NINJAGO'
data[data$theme_name=='LEGO Creator 3-in-1', 'theme_name'] = 'Creator 3-in-1'

# how many theme names?
uncommon_themes <- data %>%
  group_by(theme_name) %>%
  count() %>%
  arrange(n)
# 1/3 of themes have less than 50 counts, rename them as 'Others'

data <- data %>%
  left_join(uncommon_themes, by=c('theme_name' = 'theme_name')) %>%
  mutate(data, theme_name = ifelse(n < 50, 'Others', as.character(theme_name)))

#remove unneeded columns
data$ages <- NULL 
data$prod_desc <- NULL
data$prod_long_desc <- NULL
data$prod_id <- NULL
data$n <- NULL
# data$set_name <- NULL
rm(uncommon_themes, factorCol, i)

#Exploratory analysis

ggplot(data, aes(y=reorder(factor(theme_name), +price_per_brick), x=price_per_brick)) +
  geom_bar(stat="summary", fun="mean")
#MINDSTORMS sets have an abnormally high price per brick. These are robotics sets

ggplot(data[data$theme_name != "MINDSTORMS",], 
       aes(y=reorder(factor(theme_name), +price_per_brick), x=price_per_brick)) + 
  geom_bar(stat="summary", fun="mean")

#specialized pieces like baseplates and brick separators are higher priced
ggplot(data[data$theme_name=="Classic",], 
       aes(x=price_per_brick, y=reorder(set_name, +price_per_brick))) + 
  geom_bar(stat="Summary", fun="mean")

#More challenging sets are more expensive since they are larger
ggplot(data[data$review_difficulty != "Blank",], aes(x=list_price, y=review_difficulty)) + geom_bar(stat="Summary", fun="median") + labs(x="List Price", y="Difficulty")

#create a specialized indicator for baseplates, separators, plates,and tracks
data$specialized <- grepl('Baseplate', data$set_name, fixed=T) | grepl('Separator', data$set_name, fixed=T) | grepl('Plates', data$set_name, fixed=T) | grepl('Tracks', data$set_name, fixed=T)

#excluding the robotics and specialized pieces, DUPLO has the higher price per brick which makes sense since the pieces are larger and often more specialized for younger children
ggplot(data[data$theme_name != "MINDSTORMS" & !data$specialized,], 
       aes(x=price_per_brick, y=reorder(theme_name, +price_per_brick))) + 
  geom_bar(stat="summary", fun="median") +
  labs(x="Price Per Brick", y="Theme") 

#sets for younger children have a higher price per brick since Duplo blocks are larger
ggplot(data[data$theme_name != "MINDSTORMS" & !data$specialized,], 
       aes(y=list_price, x=min_age)) + 
  geom_bar(stat="summary", fun="median") +
  labs(x="Minimum Age", y="List Price") 
#sets for older children have an overall higher list price because of the larger # of pieces
ggplot(data[data$theme_name != "MINDSTORMS" & !data$specialized,], 
       aes(y=price_per_brick, x=min_age)) + 
  geom_bar(stat="summary", fun="median") +
  labs(x="Minimum Age", y="List Price") 

#trademarked sets trend slighly more expensive
ggplot(data[!data$specialized,], aes(x=min_age, y=price_per_brick, color=trademark)) + 
  geom_point() + geom_smooth(method="lm") + ylim(0, 5)
ggplot(data, aes(x=min_age, y=price_per_brick, color=specialized)) + 
  geom_point() + geom_smooth(method="lm")

#remove for modeling purposes
rating_data <- data[, -c(5, 6, 16)]
price_data <- data[, -c(2, 4, 6, 9, 15, 16)]
write.csv(data, file="LegoData.csv")
?write.csv
rm(data)
######## rating ########

# play_star_rating = Play Experience
# star_rating = Overall rating
# val_star_rating = Value for Money

########################
# look at missing values first ####
sapply(rating_data[,c("play_star_rating", "star_rating", "val_star_rating")], function(x) sum(is.na(x))/nrow(rating_data))
# about the same missing rate (overall rating is slightly more complete), do missing ratings occur in the same records?

vis_miss(rating_data)
vis_miss(rating_data[,c("play_star_rating", "star_rating", "val_star_rating")])
# same records often miss all 3 ratings

# focusing on play experience and value for money which have slightly more missing ratings, is there distinctive feature difference between records with or w/o these ratings ####
rating_data %>%
  mutate(missing_play = ifelse(is.na(play_star_rating), 
                               'missing play star', 'valid play star'),
         missing_value = ifelse(is.na(val_star_rating), 
                                'missing value star', 'valid value star')) %>%
  select(missing_play, missing_value, list_price, num_reviews, piece_count, difficulty) %>%
  group_by(missing_play, missing_value) %>%
  summarise(avg_price = mean(list_price),
            avg_num_reviews = mean(num_reviews),
            avg_piece_count = mean(piece_count),
            avg_difficulty = mean(difficulty, na.rm = TRUE),
            count = n()) %>%
  data.frame()
# records with valid ratings tend to be more expensive, have more reviews, higher piece count, and slightly more difficult (about 0.5+ out of 5)
# records with valid play star and missing value star rating has only 1 piece count in average, they happen to be all MINDSTORMS
rating_data[!is.na(rating_data$play_star_rating) & is.na(rating_data$val_star_rating), 'theme_name']

# explore those with valid values ##### 
data2 <- rating_data[!(is.na(rating_data$play_star_rating) | is.na(rating_data$star_rating) | is.na(rating_data$val_star_rating)), ]

# play experience and overall rating is positively related ##### 
ggplot(data2, aes(play_star_rating, star_rating, color = theme_name)) +
  geom_point()

# brickheadz has low play experience but high overall rating
table(data2[data2$play_star_rating %in% c(1,2) & data2$star_rating %in% c(4,5),'theme_name'])

# play experience and value for money is positively related, except for ... ##### 
ggplot(data2, aes(play_star_rating, val_star_rating, color = theme_name)) +
  geom_point()
# MINDSTORMS 42/105 (40%)
table(data2[data2$play_star_rating == 5 & data2$val_star_rating <= 3,'theme_name'])

# overall rating and value for money is positively related ####
ggplot(data2, aes(star_rating, val_star_rating, color = theme_name)) +
  geom_point()
# friends overall rating is high while value for money is low
table(data2[data2$star_rating == 4 & data2$val_star_rating == 1,'theme_name'])

# rating prediction ####
# based on the above observations of the positive correlation among all 3 ratings and the missing rate of them, we decided to predict star_rating below
cor(data2[, c(4,5,7)])

#                       play_star_rating star_rating val_star_rating
# play_star_rating        1.0000000   0.6074752       0.4809384
# star_rating             0.6074752   1.0000000       0.7278896
# val_star_rating         0.4809384   0.7278896       1.0000000
rm(data2)

# preprocess the data
df <- rating_data
df[,c('play_star_rating', 'val_star_rating')] <- NULL
df$list_price <- NULL
df$num_reviews <- NULL

# partition the data ####
# records with valid values split into 30% / 70%
# records with missing values leaves it for prediction
current_data_valid_values <- df[!is.na(df$star_rating),]
new_data_missing_values <- df[is.na(df$star_rating),]
inTrain <- createDataPartition(y = current_data_valid_values$star_rating, 
                               p = .7,
                               list = F)
train <- current_data_valid_values[inTrain, ]
test <- current_data_valid_values[-inTrain, ]

# design how I want to design the run 
ctrl <- trainControl(method = 'cv', 
                     number = 3, 
                     classProbs = F, 
                     summaryFunction = defaultSummary,
                     allowParallel = T)

# train!
# to avoid the error of "Error in na.fail.default: missing values in object", use na.exclude() (or whatever) on the whole data frame at once, so that the remaining observations stay matched up across variables ..
set.seed(51)
lm_model <- train(star_rating ~ ., 
                  data = train, 
                  method = 'lm', 
                  trControl = ctrl, 
                  metric = 'Rsquared', 
                  na.action = na.exclude)
glm_model <- train(star_rating ~ ., 
                   data = train, 
                   method = 'glm', 
                   trControl = ctrl, 
                   family = 'gaussian',
                   metric = 'Rsquared',
                   na.action = na.exclude)
neural_net_model <- train(star_rating ~ ., 
                          data = train, 
                          method = 'nnet', 
                          trControl = ctrl, 
                          maxit = 10,
                          metric = 'Rsquared',
                          na.action = na.exclude)
gradient_model <- train(star_rating ~ .,
                        data = train,
                        method = 'gbm',
                        trControl = ctrl,
                        metric = 'Rsquared',
                        na.action = na.exclude)

# evaluate the performance
defaultSummary(data=data.frame(obs=train$star_rating, pred=predict(lm_model, newdata=train)),
               model=lm_model)
# RMSE  Rsquared       MAE 
# 0.4922309 0.1130386 0.3701272 
defaultSummary(data.frame(obs=test$star_rating, pred=predict(lm_model, newdata=test)),
               model=lm_model)
# RMSE   Rsquared        MAE 
# 0.48705787 0.08838397 0.36892697 
defaultSummary(data.frame(obs=train$star_rating, pred=predict(glm_model, newdata=train)),
               model=glm_model)
# RMSE  Rsquared       MAE 
# 0.4922309 0.1130386 0.3701272  
defaultSummary(data.frame(obs=test$star_rating, pred=predict(glm_model, newdata=test)),
               model=glm_model)
# RMSE  Rsquared       MAE 
# 0.4870033 0.1053476 0.3668354 

defaultSummary(data.frame(obs=train$star_rating, pred=predict(neural_net_model, newdata=train)),
               model=neural_net_model)
# RMSE Rsquared      MAE 
# 3.550625       NA 3.511946
options(scipen=999) # removes scientific notation in outputs
defaultSummary(data.frame(obs=test$star_rating, pred=predict(neural_net_model, newdata=test)),
               model=neural_net_model)
# RMSE Rsquared      MAE 
# 3.555973       NA 3.519242 

defaultSummary(data.frame(obs=train$star_rating, pred=predict(gradient_model, newdata=train)),
               model=gradient_model)
# RMSE  Rsquared       MAE 
# 0.4049256 0.4688578 0.3162619 
 
defaultSummary(data.frame(obs=test$star_rating, pred=predict(gradient_model, newdata=test)),
               model=gradient_model)
# RMSE  Rsquared       MAE 
# 0.4071658 0.4239960 0.3148486
# save(gradient_model, file="ratingModel.Rdata")
# gradient boosting model seems to perform the best, rsquared dropped from 0.47 (train set) to about 0.42 (test set) which is within tolerance

# predict on missing star_ratings and corece them ####
sum(is.na(rating_data$star_rating))
sum(is.na(new_data_missing_values$star_rating))

new_data_missing_values[,'star_rating'] <- predict(gradient_model, newdata=new_data_missing_values)
sum(is.na(new_data_missing_values$star_rating))

# data <- rbind(current_data_valid_values, new_data_missing_values)
# sum(is.na(data$star_rating))

# 44/1620 (2.7%) of predicted ratings are higher than the maximum value 5
hist(new_data_missing_values$star_rating)
sum(new_data_missing_values$star_rating > 5)
# coerce star_rating > 5 to 5
new_data_missing_values[new_data_missing_values$star_rating > 5, 'star_rating'] = 5
# double check if the distribution looks right
hist(new_data_missing_values$star_rating)

rm(ctrl, glm_model, gradient_model, inTrain, lm_model, neural_net_model, test, train)

#price model
library(caret)
d <- price_data
d <- d[!is.na(d$star_rating),]
d$y <- d$list_price
y <- d$list_price
d$list_price <- NULL
d <- data.frame(d$y, d[1:ncol(d)-1])
dummies <- dummyVars(d.y ~ review_difficulty + theme_name + country, data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          #" removes dots from col names
d$review_difficulty <- NULL
d$theme_name <- NULL
d$country <- NULL
d <- cbind(d, ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)                                  # clean environment


# ORIGINALLY MODELED WITH THIS - Ended up being very difficult for user prediction with removing columns, 
#so tried without removing colinearity - model performance was largely the same


# nzv <- nearZeroVar(d, freqCut = 99.5/0.5, saveMetrics = TRUE)
# d <- d[, !nzv$nzv]

# # # calculate correlation matrix using Pearson's correlation formula
# # descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
# # 
# # d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
# # names(d)[1] <- "ones"
# # #sapply(d, function(x) sum(is.na(x)))
# 
# # identify the columns that are linear combos
# comboInfo <- findLinearCombos(d)
# comboInfo
# 
# # remove columns identified that led to linear combos
# d <- d[, -comboInfo$remove]
# 
# # remove the "ones" column in the first column
# d <- d[, c(2:ncol(d))]

# Add the target variable back to our data.frame
# d <- cbind(y, d)

# rm(nzv, descrCor, comboInfo)

preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)
rm(preProcValues)

inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

linearModel <- train(y ~ ., 
                     data=train,
                     method="lm",
                     trControl=ctrl,
                     metric="Rsquared"
)

neural_net_model <- train(y ~ ., 
                          data = train, 
                          method = 'nnet', 
                          trControl = ctrl, 
                          maxit = 10,
                          metric = 'Rsquared')
gradient_model <- train(y ~ .,
                        data = train,
                        method = 'gbm',
                        trControl = ctrl,
                        metric = 'Rsquared')

# train
defaultSummary(data=data.frame(obs=train$y, pred=predict(linearModel, newdata=train))
               , model=linearModel)
# RMSE   Rsquared        MAE 
# 37.3512580  0.8461765 18.0064406 

# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(linearModel, newdata=test))
               , model=linearModel)
# RMSE   Rsquared        MAE 
# 37.9875562  0.8663865 17.6313635 

# train
defaultSummary(data=data.frame(obs=train$y, pred=predict(gradient_model, newdata=train))
               , model=gradient_model)
# RMSE   Rsquared        MAE 
# 18.3614876  0.9634864 10.9761971 

# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(gradient_model, newdata=test))
               , model=gradient_model)
# RMSE   Rsquared        MAE 
# 18.3235586  0.9690927 11.1265490 

# train
defaultSummary(data=data.frame(obs=train$y, pred=predict(neural_net_model, newdata=train))
               , model=neural_net_model)
# RMSE     Rsquared          MAE 
# 1.172974e+02 2.394464e-03 6.847700e+01 
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(neural_net_model, newdata=test))
               , model=neural_net_model)
# RMSE     Rsquared          MAE 
# 1.233198e+02 1.962367e-03 6.952097e+01 

#gradient model is the best

predictd <- predict(gradient_model, newdata=d)
#save(gradient_model, file="pricemodel.Rdata")

combinedData <- data.frame(d$y, predictd, price_data[!is.na(price_data$star_rating),"theme_name"])
names(combinedData) <- c("actual", "predicted", "theme")
windows()
ggplot(combinedData, aes(x=actual, y=predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1) + geom_smooth(method="lm") 
ggplot(price_data, aes(x=list_price)) + geom_histogram()a
#the model predicts more poorly above $450 list price. Looking at the distribution of prices, there are very few sets this size so it makes sense the model is not as accurate
#write.csv(names(d), file="columns.csv")
