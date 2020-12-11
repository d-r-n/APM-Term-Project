library(tidyverse)
library(dplyr)
set.seed(2013)

mymodel_vars <- readRDS('Documents/GitHub/APM-Term-Project/boost_models/models/_passResult_model_data.rds')
str(mymodel_vars)
mymodel_vars$label <- as.integer(mymodel_vars$label)
str(mymodel_vars)

labels <- mymodel_vars$label
length(label)

#install.packages('xgboost')
library(xgboost)
full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = mymodel_vars %>% dplyr::select(-label)), label = as.integer(mymodel_vars$label))

#params
nrounds = 20

x = c(7) #max depth
y = c(.015) #something else to tune

search <- map_df(cross2(x, y), function(x) {
  
  depth = x[[1]]
  eta = x[[2]]
  
  print(message(glue::glue('max depth {depth} and eta {eta}')))
  
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("error", "logloss"),
      eta = eta,
      gamma = 2,
      subsample=0.8,
      colsample_bytree=0.8,
      max_depth = 7,
      min_child_weight = 0.9,
      base_score = mean(mymodel_vars$label)
    )
  
  #train
  xp_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                 nfold = 10, metrics = list("error", "logloss"),
                                 early_stopping_rounds = 3, print_every_n = 10)
  
  iter = xp_cv_model$best_iteration
  
  result <- data.frame(
    'eta' = eta,
    'iter' = iter,
    'logloss' = xp_cv_model$evaluation_log[iter]$test_logloss_mean,
    'error' = xp_cv_model$evaluation_log[iter]$test_error_mean,
    'gamma' = 2,
    'max_depth' = 7,
    'min_child_weight' = 0.9,
    'subsample' = 0.8,
    'colsample' = 0.8
  ) %>%
    as_tibble()
  
  return(result)
  
})


search %>%
  arrange(logloss) 


# best
best <- search %>% 
  arrange(logloss) %>%
  dplyr::slice(1)

message(
  glue::glue("
  error: {best$error}
  loglos: {best$logloss}
  iter: {best$iter}
  eta: {best$eta}
  gamma: {best$gamma}
  depth: {best$max_depth}
  weight: {best$min_child_weight}
             ")
)

error: 0.289332
loglos: 0.5223975
iter: 722
eta: 0.025
gamma: 2
depth: 7
weight: 0.9













