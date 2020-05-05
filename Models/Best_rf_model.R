library(randomForest)
require(caTools)
library(readr)
setwd("~/GitHub/Airbnb-ML/Ash_Experiments/Final")
library(ROCR)
set.seed(12345)


df <- read_csv("C:\\Users\\Aishwarya\\Documents\\GitHub\\Airbnb-New\\train54.csv")
names(df)
#df[,65:96] <- lapply(export_train[,65:96], factor)
#df[,1:90] <- lapply(df[,1:90], factor)


## Randomly partition the data into 30% validation data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_valid <- df[test_instn,]
## Save the rest of the data as the data that isn't validation
df_train <- df[-test_instn,]

which(is.na(df_train))

df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)
df_valid$high_booking_rate <- as.factor(df_valid$high_booking_rate)


names(df_train)
df_train$density_10bins<- as.factor(df_train$density_10bins)
df_valid$density_10bins <- as.factor(df_valid$density_10bins)
df_train$density_bins<- as.factor(df_train$density_bins)
df_valid$density_bins <- as.factor(df_valid$density_bins)

names(df_train)
View(df)
#c("roomEntire.home.apt", "clustercategory","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic", "propertyHotel")
drop<- c("roomEntire_ home_apt","require_guest_profile_picture","require_guest_phone_verification","host_has_profile_pic",
         "propertyHotel","weekly_discount","monthly_discount",
         "weekly_suitable","monthly_suitable","X1")
drop <-c("X1","X1_1","city_centrality","neighbourhood_restaurant",
         "maximum_nights","num_listings")#,"roomShared_room","host_has_profile_pic",
         #"host_identity_verified","host_response_rate",
         #"propertyHotel","require_guest_phone_verification",
         #"require_guest_profile_picture","bed_linen",
         #"coffee_machine","gym","hottub_sauna_pool","long_term_Stay_allowed",
         #"secure","interaction","cp8","cp10","cp5")
#Yielded highest accuracy -> 0.839 to 0.8401(this model did not have Bella's
#and amenities new variables 
#weekly_suitable, weekly_discount,monthly_suitable, monthly_discount,
#Family, Check, Stove, Refrigerator, Gym, Elevator, Workspace, Air_conditioning,
#Parking, Lock, Essentials, Smoking, Cooking, Internet)

df_valid <- df_valid[,-which(names(df_valid) %in% drop)]
df_train <- df_train[,-which(names(df_train) %in% drop)]

sum(is.na(df_train))

names(df_train)


#best model, with tuned hyperparameters
rf<- randomForest(high_booking_rate~.,data = (df_train),
                  n_estimators = 50, min_sample_leaf = 80, mtry = 20,
                  na.rm = TRUE)
memory.limit(size = 40000000)
names(df_train)
pred_valid = predict(rf, newdata=df_valid)
mc = table(df_valid$high_booking_rate, pred_valid)
mc
acc = (mc[1] + mc[4])/sum(mc)
acc

#----Alternatively:
library(caret)

model <- train(
  high_booking_rate~.,
  tuneLength = 1,
  data = df_train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

print(model)
pred_valid = predict(model, newdata=df_valid)
mc = table(df_valid$high_booking_rate, pred_valid)
acc = (mc[1] + mc[4])/sum(mc)
acc #0.8364771
#mtry = 21, splitrule = gini, min.node.size = 1

# Now we try increasing the tune length to 3
model <- train(
  high_booking_rate~.,
  tuneLength = 3,
  data = df_train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)
print(model)
pred_valid = predict(model, newdata=df_valid)
mc = table(df_valid$high_booking_rate, pred_valid)
acc = (mc[1] + mc[4])/sum(mc)
acc 

#---------------------------------Using caret package-----------

