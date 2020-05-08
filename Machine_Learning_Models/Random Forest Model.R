#Importing the required libraries
library(randomForest)
require(caTools)
library(readr)
setwd("~/GitHub/Airbnb-ML/Ash_Experiments/Final")
library(ROCR)
set.seed(12345)


df <- read_csv("C:\\Users\\Aishwarya\\Documents\\GitHub\\Airbnb-New\\Data\\train.csv")
#names(df)



## Randomly partition the data into 30% validation data and the remaining 70% data.
test_instn = sample(nrow(df), 0.3*nrow(df))
df_valid <- df[test_instn,]
## Save the rest of the data as the data that isn't validation
df_train <- df[-test_instn,]

#Check for any NAs
which(is.na(df_train))

df_train$high_booking_rate <- as.factor(df_train$high_booking_rate)
df_valid$high_booking_rate <- as.factor(df_valid$high_booking_rate)

#View(df_valid)
#names(df_train)
#df_train$density_10bins<- as.factor(df_train$density_10bins)
##df_valid$density_10bins <- as.factor(df_valid$density_10bins)
#df_train$density_bins<- as.factor(df_train$density_bins)
#df_valid$density_bins <- as.factor(df_valid$density_bins)

#names(df_train)
#View(df)
#,"city_centrality","neighbourhood_restaurant",
#"maximum_nights","num_listings"
drop <-c("X1","X1_1")

df_valid <- df_valid[,-which(names(df_valid) %in% drop)]
df_train <- df_train[,-which(names(df_train) %in% drop)]

sum(is.na(df_train))

#names(df_train)


#best model, with tuned hyperparameters
rf<- randomForest(high_booking_rate~.,data = (df_train),
                  n_estimators = 50, min_sample_leaf = 80, mtry = 45,
                  na.rm = TRUE)
memory.limit(size = 40000000)

#---------------------------------------------------
#Hyper parameter tuning
res <- tuneRF(x = subset(df_train,select = - high_booking_rate),
              y = df_train$high_booking_rate,
              ntreeTry = 500)

mtree_opt <- res[,"mtry"][which.min(res[,'OOBError'])]
mtree_opt # 14 (optimal)


#tuning for nodesize, sampsize
mtry <- c(rep(6:6,ncol(df_train)*0.8))
mtry
nodesize <-seq(3,8,2)
sampsize<-nrow(df_train)*c(0.7,0.8)

hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

oob_err <- c()
memory.limit(300000000000)
View(df_train)
for (i in 1:nrow(hyper_grid)){
  print(i)
  model <- randomForest(formula = high_booking_rate~.,
                        x = df_train[,2:43],
                        data = df_train,
                        mtry = mtry[i],
                        nodesize = nodesize[i],
                        #sampsize = sampsize[i],
  )
  
  oob_err[i] <- model$err.rate[nrow(model$err.rate),"OOB"]
}

opt_i <- which.min(oob_err)

print(hyper_grid[opt_i,])

#--------------------------------------------
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
  method = "ranger"
  #trControl = trainControl(
  #  method = "cv", 
  #  number = , 
  #  verboseIter = TRUE
  #)
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

