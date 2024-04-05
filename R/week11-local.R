# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(stats)

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>% 
  mutate_all(~ifelse(.==0, NA, .)) %>%
  filter(!is.na(MOSTHRS)) %>% 
  rename(`work hours` = MOSTHRS) %>%
  select(-HRS1, -HRS2) %>%
  select(-where(~mean(is.na(.))>.75)) %>% 
  mutate_all(as.numeric)


#Visualization
ggplot(gss_tbl, aes(x=`work hours`)) +
  geom_histogram() +
  labs(x= "Work Hours", y= "Frequency", title= "Frequency of Work Hours Histogram") 

#Analysis
set.seed(1234)
rows <- sample(nrow(gss_tbl)) 
gss_shuffle <- gss_tbl[rows, ] 
gss_split <- round(nrow(gss_shuffle) * 0.75) 
gss_train <- gss_shuffle[1:gss_split, ] 
gss_test <- gss_shuffle[(gss_split + 1):nrow(gss_shuffle), ] 


index_folds <- createFolds(gss_train$`work hours`, 10) 

myControl <- trainControl(
  method= "cv",
  indexOut= index_folds,
  number= 10, 
  verboseIter = TRUE) 


tic() #starts time
OLS_Model <- train(`work hours` ~ .,
                   data = gss_train, 
                   method = "lm", 
                   preProcess = "medianImpute",
                   na.action = na.pass, 
                   trControl = myControl 
)
OLS_time <- toc() #ends time  & 5.5 seconds elapsed

OLS_Model_test <- predict(Ols_Model, gss_test, na.action= na.pass) 





myGrid <- expand.grid(alpha =0:1, lambda =seq(0.0001, 0.1, length = 10)) 

tic() #starts time
EN_Model <- train(`work hours` ~ ., 
                  data = gss_train, 
                  tuneGrid = myGrid,
                  method = "glmnet",
                  preProcess = "medianImpute", 
                  na.action = na.pass,
                  trControl = myControl) 
EN_time <- toc() #ends time & 2.531 seconds elapsed 
EN_Model_test <- predict(EN_Model, gss_test, na.action= na.pass) 


RF_grid <- expand.grid(mtry= 510 , splitrule= 'variance' , min.node.size= 5 ) 
tic() #starts time
RF_Model <- train(`work hours` ~ ., 
                  data= gss_train, 
                  tuneGrid= RF_grid,
                  metric= "RSquared",
                  method= "ranger", 
                  preProcess= "medianImpute",
                  na.action= na.pass,
                  trControl=myControl)
RF_time <- toc() #ends time and 22.614 seconds elapsed 
RF_Model_test <- predict(RF_Model, gss_test, na.action= na.pass) 



XGB_grid <- expand.grid(nrounds= 50, alpha =1, lambda =.1, eta=.1)
tic() #starts time
XGB_Model <- train(`work hours` ~ .,
                   data= gss_train,
                   tuneGrid= XGB_grid,
                   method= "xgbLinear", 
                   preProcess="medianImpute",
                   na.action= na.pass,
                   trControl= myControl
)
XGB_time <- toc() #ends time and 8.058 seconds elapsed 
XGB_Model_test <- predict(XGB_Model, gss_test, na.action= na.pass)





#Publication

###moved formatting to publication section 
OLS_cv_rsq <- sub("^0", "", formatC(OLS_Model$results$Rsquared[1], format='f', digits= 2))
OLS_ho_rsq <- cor(OLS_Model_test, gss_test$`work hours`)^2 
OLS_ho_rsq_f <- sub("^0", "", formatC(OLS_ho_rsq, format='f', digits= 2))

EN_cv_rsq <- sub("^0", "", formatC(EN_Model$results$Rsquared[1], format='f', digits= 2))
EN_ho_rsq <- cor(EN_Model_test, gss_test$`work hours`)^2 
EN_ho_rsq_f <- sub("^0", "", formatC(EN_ho_rsq, format='f', digits= 2))

RF_cv_rsq <- sub("^0", "", formatC(RF_Model$results$Rsquared[1], format='f', digits= 2))
RF_ho_rsq <- cor(RF_Model_test, gss_test$`work hours`)^2 
RF_ho_rsq_f <- sub("^0", "", formatC(RF_ho_rsq, format='f', digits= 2))

XGB_cv_rsq <- sub("^0", "", formatC(XGB_Model$results$Rsquared[1], format='f', digits= 2))
XGB_ho_rsq <- cor(XGB_Model_test, gss_test$`work hours`)^2 
XGB_ho_rsq_f <- sub("^0", "", formatC(XGB_ho_rsq, format='f', digits= 2))

table1_tbl <- tibble(
  algo= c("OLS Regression", "Elastic Net", "Random Forest","eXtreme Gradient Boosting"),
  cv_rsq = c(OLS_cv_rsq, EN_cv_rsq, RF_cv_rsq, XGB_cv_rsq),
  ho_rsq = c(OLS_ho_rsq_f, EN_ho_rsq_f, RF_ho_rsq_f, XGB_ho_rsq_f))

table1_tbl
 