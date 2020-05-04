library(randomForest)
require(caTools)
library(readr)
setwd("~/GitHub/Airbnb-ML/Ash_Experiments/Final")
library(ROCR)
set.seed(12345)

install.packages("xgboost")
# Load the relevant libraries
#library(quantmod)
#library(TTR)
library(xgboost)

df11<- read_csv("train_cleaned11.csv")
df12<-read_csv('test_cleaned12_revised.csv')
df11 <- train_cleaned13
df12 <- test_cleaned13

df <- df11
df2 <- df12

names(df)

#df<-df[,-c(107)]
#df2<-df2[,-c(106)]

df <-df[,-c(1)]
df2 <-df2[,-c(1)]

#df2<-read_csv("test_cleaned_final.csv")
#ncol(df)
#ncol(df2)
#ls(df2)

#df<-df[,-c("roomEntire_ home_apt","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic",
#  "propertyHotel","weekly_discount","monthly_discount",
#"weekly_suitable","monthly_suitable","density_bins")]
df<-df[,-c("cp5",'density_10bins0','propertySpecial','nature_and_views','density_10bins6','propertyHotel',
           'host_has_profile_pic','density_10bins9')]

x <- c("access","air_conditioning","accessible", "balcony","bbq","bed_linen","breakfast",
       "CheckIn24","child_friendly","coffee_machine","cp1","cp2","cp8","cp10",#"city_centrality","neighbourhood_restaurant",
       "density_10bins1","density_10bins2","density_10bins3","density_10bins4","density_10bins5",
       "density_10bins6","density_10bins7","density_10bins8","density_10bins9","event_suitable",
       "high_end_electronics","host_greeting","hottub_sauna_pool","long_term_Stay_allowed","pets_allowed",
       "private_entrance", "secure","self_check_in","weekly_suitable","tv","white_goods",
       "elevator","kitchen","nature_and_views","gym","host_about","outdoor_space","host_has_profile_pic",
       "host_identity_verified","host_is_superhost","host_response_time","instant_bookable","interaction",
       "internet","is_business_travel_ready","is_location_exact","long_stay","monthly_suitable","parking","propertyApartment",
       "propertyCommon_house","propertyHotel","propertySide_house","propertySpecial","Real_Bed",
       "require_guest_phone_verification","require_guest_profile_picture","requires_license","roomEntire_home_apt",
       "roomPrivate_room","roomShared_room","rules","security_deposit","weekly_suitable",
       "price_roomPrivate_room","price_propertyApartment","latitude","longitude",
       "average_price_per_person")

df[x] <- lapply(df[x], factor)
df2[x] <- lapply(df2[x], factor)

#df<- df[,-which(names(df) %in% c("roomEntire_ home_apt","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic",
#                                "propertyHotel","weekly_discount","monthly_discount","weekly_suitable","monthly_suitable"))]

#df2<- df2[,-which(names(df2) %in% c("roomEntire_ home_apt","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic",
#                                 "propertyHotel","weekly_discount","monthly_discount","weekly_suitable","monthly_suitable"))]



#df$density_bins <- NULL
#colnames(df)[1] 
## Randomly partition the data into 30% validation data and the remaining 70% data.
set.seed(12345)
test_instn = sample(nrow(df), 0.3*nrow(df))
df_valid <- df[test_instn,]
## Save the rest of the data as the data that isn't validation
df_train <- df[-test_instn,]




df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)

df_valid$high_booking_rate <- as.factor(df_valid$high_booking_rate)


#View(df)
#c("roomEntire.home.apt", "clustercategory","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic", "propertyHotel")
#drop<- c("roomEntire_ home_apt","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic",
#          "propertyHotel")
drop<-c("cp5",'density_10bins0','propertySpecial','nature_and_views','density_10bins6','propertyHotel',
        'host_has_profile_pic','density_10bins9')

#Yielded highest accuracy -> 0.839 to 0.8401(this model did not have Bella's
#and amenities new variables 
#weekly_suitable, weekly_discount,monthly_suitable, monthly_discount,
#Family, Check, Stove, Refrigerator, Gym, Elevator, Workspace, Air_conditioning,
#Parking, Lock, Essentials, Smoking, Cooking, Internet)


#df_valid <- df_valid[,-which(names(df_valid) %in% drop)]
#df_train <- df_train[,-which(names(df_train) %in% drop)]

df_train$latitude <- as.numeric(df_train$latitude)
df_train$longitude <- as.numeric(df_train$longitude)
##################--------------------XgBoost
#View(df_train)
labels <- df_train$high_booking_rate
ts_label <-df_valid$high_booking_rate
#df_valid1<-df_valid[,-c(106)]
#df_train1<-df_train[,-c(106)]
df_valid1<-df_valid[,-c(1)]
df_train1<-df_train[,-c(1)]
#View(df_valid1)
#View(df_valid)
#df_valid[,1]
df_train2 <- model.matrix(~.+0,data = df_train1) 
df_valid2 <- model.matrix(~.+0,data = df_valid1)

df2<-model.matrix(~.+0,data = df2)
#df2<-model.matrix(~.+0,data = df2)
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

length(labels)
nrow(df_train2)

dtrain <- xgb.DMatrix(data = df_train2,label = labels) 
dtest <- xgb.DMatrix(data = df2) #test data
dtest1<-xgb.DMatrix(data = df_valid2,label = ts_label) # validation data


#df2<-xgb.DMatrix(data = df2)
#class(labels)
#View(labels)
#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
               gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
#params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, 
#               min_child_weight=1, subsample=1, colsample_bytree=1)

#change nrounds to check best iteration
#xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, 
#                 showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#min(xgbcv$test.error.mean)

set.seed(666)
xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 520, watchlist = list(val=dtest1,train=dtrain), 
                  print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")


#model prediction



xgbpred <- predict(xgb1,dtest)
xgb_pred <- prediction(xgbpred, ts_label)
#xgb_acc = performance(xgb_pred, measure = "acc")
xgb_acc = ROCR::performance(xgb_pred, measure = "acc")
xgb_best = which.max(slot(xgb_acc, "y.values")[[1]])
xgb_max_acc = slot(xgb_acc, "y.values")[[1]][xgb_best]
xgb_max_cutoff = slot(xgb_acc,"x.values")[[1]][xgb_best]

xgbpred <- ifelse (xgbpred > xgb_max_cutoff,1,0)

#View(xgbpred)
mc = table(data.matrix(ts_label), xgbpred)
mc

acc = (mc[1] + mc[4])/sum(mc)
acc

#confusion matrix
library(caret)
confusionMatrix (xgbpred, ts_label)

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(dtrain), model = xgb1)
xgb.plot.importance (importance_matrix = mat[90:99])


##########################------------hypertune ###############3
#create tasks
install.packages("mlr")
library(mlr)
df_train <-data.frame(df_train)
df_valid <- data.frame(df_valid)
traintask <- makeClassifTask(data =df_train,target = "high_booking_rate")
testtask <- makeClassifTask(data = df_valid,target = "high_booking_rate")



#do one hot encoding`<br/> 
#traintask <- createDummyFeatures (obj = traintask,target = "high_booking_rate") 
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)
#testtask <- createDummyFeatures (obj = testtask,target = "high_booking_rate")

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(4)
# parallelMap::parallelStop()


#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc,measures=mlr::acc, 
                     par.set = params, control = ctrl, show.info = T)
mytune$y

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)