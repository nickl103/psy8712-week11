# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_import_tbl <- read_spss("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>%
  rename(`work hours`= MOSTHRS) %>%
  select(-HRS1, -HRS2)

gss_tbl <- gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
  mutate(across(everything(), as.numeric))

# Visualization
ggplot(gss_tbl,
       aes(x=`work hours`)) +
  geom_histogram()

# Analysis
holdout_indices <- createDataPartition(gss_tbl$`work hours`,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_tbl[holdout_indices,]
training_tbl <- gss_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$`work hours`)

tic() #start time
OLS_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
OLS_toc <- toc() #end time
OLS_time <- OLS_toc$toc - OLS_toc$tic
OLS_model
cv_OLS <- OLS_model$results$Rsquared
holdout_OLS <- cor(
  predict(OLS_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

tic() #start time
EN_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
EN_toc <- toc() #end time 
EN_time <- EN_toc$toc - EN_toc$tic
EN_model
cv_EN <- max(EN_model$results$Rsquared)
holdout_EN <- cor(
  predict(EN_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

tic() #start time
RF_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
RF_toc <- toc() #end time() 
RF_time <- RF_toc$toc - RF_toc$tic
RF_model
cv_RF <- max(RF_model$results$Rsquared)
holdout_RF <- cor(
  predict(RF_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

tic() #start time
XGB_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
XGB_toc <- toc() #end time 
XGB_time <- XGB_toc$toc - XGB_toc$tic
XGB_model
cv_XGB <- max(XGB_model$results$Rsquared)
holdout_XGB <- cor(
  predict(XGB_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

summary(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")
dotplot(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")

###PARALLEL MODELS 
#I just copied the code from the original models and added a p for parallel

local_cluster <- makeCluster(detectCores()- 1) #making the local cluster based on demonstration and slides
##used detectCores and my local computer has 8 so I did minus one following demonstration
registerDoParallel(local_cluster) #telling R to parallelize it based on lecture

#OLS parallel model, this doesn't parallelize according to lecture 
tic() #start time for benchmarking for models 
OLS_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
OLS_p_toc <- toc() #end time 
OLS_p_time <- OLS_p_toc$toc - OLS_p_toc$tic #calculating time elapsed based on lecture for Table later
OLS_p_model
cv_p_OLS <- OLS_p_model$results$Rsquared
holdout_p_OLS <- cor(
  predict(OLS_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

#Elastic Net Parallel Model
tic() #start time
EN_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
EN_p_toc <- toc() #end time  
EN_p_time <- EN_p_toc$toc - EN_p_toc$tic #calculating time elapsed based on lecture for Table later
EN_p_model
cv_p_EN <- max(EN_p_model$results$Rsquared)
holdout_p_EN <- cor(
  predict(EN_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

#Random Forest parallel model
tic() #start time
RF_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
RF_p_toc <- toc() #end time
RF_p_time <- RF_p_toc$toc - RF_p_toc$tic #calculating time elapsed for Table later
RF_p_model
cv_p_RF <- max(RF_p_model$results$Rsquared)
holdout_p_RF <- cor(
  predict(RF_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

###eXtreme gradient boosting parallel model 
tic() #start time
XGB_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
XGB_p_toc <- toc() #end time . 
XGB_p_time <- XGB_p_toc$toc - XGB_p_toc$tic #calculating time elapsed for table later.
XGB_p_model
cv_p_XGB <- max(XGB_p_model$results$Rsquared)
holdout_p_XGB <- cor(
  predict(XGB_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2



summary(resamples(list(OLS_p_model, EN_p_model, RF_p_model, XGB_p_model)), metric="Rsquared")
dotplot(resamples(list(OLS_p_model, EN_p_model, RF_p_model, XGB_p_model)), metric="Rsquared")

stopCluster(local_cluster)
registerDoSEQ()
# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

table1_tbl <- tibble(
  algo = c("regression","elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_OLS),
    make_it_pretty(cv_EN),
    make_it_pretty(cv_RF),
    make_it_pretty(cv_XGB)
  ),
  ho_rqs = c(
    make_it_pretty(holdout_OLS),
    make_it_pretty(holdout_EN),
    make_it_pretty(holdout_RF),
    make_it_pretty(holdout_XGB)
  )
)

table1_tbl

table2_tbl <- tibble(
  algo= c("regression", "elastic net", "random forests", "xgboost"),
  original = c(OLS_time, EN_time, RF_time, XGB_time),
  parallelized= c(OLS_p_time, EN_p_time, RF_p_time, XGB_p_time)
  )


table2_tbl

#1. Elastic Net and XGBoosts benefited the most from the parallelization as the time elapsed for both of them decreased from the original models time. 

#2 63.3 was the difference between the Random Forest Parallel Model (68.7) and the xgboost parallel model (5.40). I think the amount of hyperparameters again slowed down the analysis even for the RF parallel model. 

#3. I would do the elastic net model because it had the second highest R2 for the holdout model, while still having a decent high R2 for the 10-fold model. It was also the second fastest model for both the original and parallelized model. While the random forest model had high R2, it just takes the longest. 