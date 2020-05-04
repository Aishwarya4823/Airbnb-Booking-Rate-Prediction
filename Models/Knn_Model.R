#KNN model for Airbnb dataset

set.seed(12345)
library(class)

df = read.csv("train_cleaned.csv")

df_competition = read.csv("test_cleaned.csv")
#View(df_competition)
## Randomly partition the data into 30% testing data and the remaining 70% data.
test_instn = sample(nrow(df), 0.25*nrow(df))
df_test <- df[test_instn,]
## Save the rest of the data as the data that isn't testing
df_rest <- df[-test_instn,]
## e. Randomly partition the remaining data into 75% training data and 25% validation data.
valid_instn = sample(nrow(df_rest), 0.3*nrow(df_rest))
df_valid <- df_rest[valid_instn,]
df_train <- df_rest[-valid_instn,]
#View(df_competition)

train.HBR=df_train$high_booking_rate
valid.HBR=df_valid$high_booking_rate

#Getting the columns you want
train.X=df_train[,c( "accommodates", "amenities_count", "availability_365", "availability_60", "availability_90", "bathrooms", "Real_Bed", "bedrooms", "beds", "cancellation_policy", "cleaning_fee", "extra_people", "first_review", "guests_included", "host_about", "host_has_profile_pic", "host_identity_verified", "host_is_superhost", "host_listings_count", "host_response_rate", "host_response_time", "experience", "instant_bookable", "is_business_travel_ready", "is_location_exact", "long_stay", "minimum_nights", "price", "propertyApartment", "propertyCommon_house", "propertySide_house", "propertyHotel", "propertySpecial", "require_guest_phone_verification", "require_guest_profile_picture", "requires_license", "roomEntire.home.apt", "roomPrivate.room", "roomShared.room", "security_deposit", "flexible")]
valid.X=df_valid[,c("accommodates", "amenities_count", "availability_365", "availability_60", "availability_90", "bathrooms", "Real_Bed", "bedrooms", "beds", "cancellation_policy", "cleaning_fee", "extra_people", "first_review", "guests_included", "host_about", "host_has_profile_pic", "host_identity_verified", "host_is_superhost", "host_listings_count", "host_response_rate", "host_response_time", "experience", "instant_bookable", "is_business_travel_ready", "is_location_exact", "long_stay", "minimum_nights", "price", "propertyApartment", "propertyCommon_house", "propertySide_house", "propertyHotel", "propertySpecial", "require_guest_phone_verification", "require_guest_profile_picture", "requires_license", "roomEntire.home.apt", "roomPrivate.room", "roomShared.room", "security_deposit", "flexible")]
test.X=df_test[,c("accommodates", "amenities_count", "availability_365", "availability_60", "availability_90", "bathrooms", "Real_Bed", "bedrooms", "beds", "cancellation_policy", "cleaning_fee", "extra_people", "first_review", "guests_included", "host_about", "host_has_profile_pic", "host_identity_verified", "host_is_superhost", "host_listings_count", "host_response_rate", "host_response_time", "experience", "instant_bookable", "is_business_travel_ready", "is_location_exact", "long_stay", "minimum_nights", "price", "propertyApartment", "propertyCommon_house", "propertySide_house", "propertyHotel", "propertySpecial", "require_guest_phone_verification", "require_guest_profile_picture", "requires_license", "roomEntire.home.apt", "roomPrivate.room", "roomShared.room", "security_deposit", "flexible")]

knn.pred_tr=knn(train.X,train.X,train.HBR,k=1)
knn.pred_va=knn(train.X,valid.X,train.HBR,k=1)

grid_knn = c(1, 3, 5, 7, 9,11, 13,15,19, 21, 25, 31,35)

vals<- matrix(NA,nrow=13,ncol=3)
ind<-1
#Following function will take a lot of time
#calculate training and calidation accuracies for each k value in grid_knn
for (kval in grid_knn){ #for each value in the grid
  
  
  knn.pred_va=knn(train.X,valid.X,train.HBR,k=kval)
  knn.pred_tr=knn(train.X,train.X,train.HBR,k=kval)
  
  
  #Computing the accuracy on validation and training
  correct_va <- sum(ifelse(knn.pred_va==valid.HBR,1,0))
  accuracy_va <- (1.0*correct_va)/nrow(valid.X)
  correct_tr <- sum(ifelse(knn.pred_tr==train.HBR,1,0))
  accuracy_tr <- (1.0*correct_tr)/nrow(train.X)
  
  vals[ind,1] <- kval
  vals[ind,2] <- accuracy_va
  vals[ind,3] <- accuracy_tr
  
  ind <-ind+1
}

#View our matrix
vals  

# plotting the training and validation accuracies
plot(vals[,1],vals[,2],type='l',col='red',xlab = "Value of k",ylab = "Accuracy",ylim=c(0,1))
lines(vals[,1],vals[,3],col='dark blue')
#We see that the best model is  at k = ?



