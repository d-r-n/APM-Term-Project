### hypertuning
library(tidyverse)
library(dplyr)
library(caret)
#install.packages('e1071')
library(e1071)
library(xgboost)

set.seed(42)

mymodel_vars <- readRDS('Documents/GitHub/APM-Term-Project/boost_models/models/_passResult_w_positions_model_data.rds')
str(mymodel_vars)
options(na.action='na.pass')

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mymodel_vars))

## set the seed to make your partition reproducible
set.seed(42)
train_ind <- sample(seq_len(nrow(mymodel_vars)), size = smp_size)

training <- as.data.frame(mymodel_vars[train_ind, ])
testing <- as.data.frame(mymodel_vars[-train_ind, ])

# Prepare matrix for XGBoost algorithm
train_matrix <-model.matrix(label ~.-1, data = training)
test_matrix <-model.matrix(label ~.-1, data = testing)

dtrain <- xgb.DMatrix(data = train_matrix, label = training$label) 
dtest <- xgb.DMatrix(data = test_matrix, label = testing$label)

# Base XGBoost model
set.seed(42)
params <- list(booster = "gbtree",
               objective = "binary:logistic",
               max_depth = 6,
               eta = 0.26839998,
               subsample = 0.7421056,
               colsample_bytree = 0.6931169,
               min_child_weight = 5, 
               gamma= 1 #after gridsearch, found that the best gamma is 1
)

# Default model
xgb_base <- xgb.train (params = params,
                       data = dtrain,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "error",
                       eval_metric = "logloss",
                       early_stopping_rounds = 50,
                       watchlist = list(train= dtrain, test= dtest))


# Make prediction on dtest
testing$pred_label_base <- predict(xgb_base, dtest)
testing$pred_label_factor_base <- factor(ifelse(testing$pred_label_base > 0.5, 1, 0),
                                         labels=c("Not Complete","Complete"))

preds <- factor(testing$pred_label_factor_base)
test <- factor(testing$label)
levels(test) <- c("Not Complete", "Complete")
cm <- confusionMatrix(preds, test, positive = "Complete")
cm

tocsv <- data.frame(cbind(t(cm$overall),t(cm$byClass)))

# You can then use
write.csv(tocsv,"Documents/GitHub/APM-Term-Project/boost_models/output/_cm_w_pos_route_optimized.csv")

#### Initial model comments ####
# Still sensitive model (good in predicting complete passes)
# More specific than without dummies (eh in predicting incomplete passes)
num_plays_in_test = length(testing$label)
num_complete_test = nrow(testing %>% filter(label == 1))
num_not_complete_test = nrow(testing %>% filter(label == 0))
# There are 4414 plays in the testing set of which 2696 (1) are complete
# and 1718 (0) are not complete.

# If we had always guessed the positive class we would have achieved
guess = (num_complete_test/num_plays_in_test)^2+(num_not_complete_test/num_plays_in_test)^2 # 0.5240904
# so our initial model performed
0.7154-guess # 0.1913096
# 19.1 better than blindly guessing complete pass

# optimized model:
0.7211-guess # 0.1970096
# 19.7 better than blindly guessing complete pass


#############################################################
################# Hypertuning Parameters #################### 
#############################################################

# Take start time to measure time of random search algorithm
start.time <- Sys.time()

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(42)
for (iter in 1:10000){
  param <- list(booster = "gbtree",
                objective = "binary:logistic",
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)


# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(42)
  mdcv <- xgb.train(data=dtrain,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 500,
                    eval_metric = "error",
                    early_stopping_rounds= 30,
                    print_every_n = 50,
                    watchlist = list(train= dtrain, test= dtest))
  
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$test_error))
  lowest_error_list[[row]] <- lowest_error
}


# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$test_error)`)

# Prepare table
randomsearch <- as.data.frame(randomsearch) %>%
  rename(test_acc = `1 - min(mdcv$evaluation_log$test_error)`) %>%
  arrange(-test_acc)

write.csv(randomsearch,"Documents/GitHub/APM-Term-Project/boost_models/output/params_w_pos_route.csv", row.names = FALSE)

# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # Time difference of 5.62373 hours

#### BEST #### (gamma found after gridsearch)
#booster          : gbtree
#objective        : binary:logistic
#max_depth        : 6
#eta              : 0.26839998
#subsample        : 0.7421056
#colsample_bytree : 0.6931169
#min_child_weight : 5 

#gamma            : 1
