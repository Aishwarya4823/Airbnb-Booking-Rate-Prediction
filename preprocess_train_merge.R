#load libraries
# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)

#read data col_types = cols(zipcode = col_character()
train_x <- read_csv("airbnb_train_x.csv")
train_y <- read_csv("airbnb_train_y.csv")
# merge features and label
df <- merge(train_x, train_y, by.x = 'X1', by.y = 'X1')

# Merge df with zipcode dataframe that contains 'density' and 'density_bins'(4 bins)
zipcode <- read_csv("zipcode_combin_train.csv")
zipcode$X1 <- zipcode$X1+1
df <- merge(df, zipcode, by.x = 'X1', by.y = 'X1')


names(df)

# create density_10bins (factor)
df$density_10bins <- cut(df$density, 10, include.lowest=TRUE, labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
# convert data type
df$density_bins <- as.factor(df$density_bins) # convert to factor
df$density_10bins <- as.factor(df$density_10bins)
df$high_booking_rate <- as.factor(df$high_booking_rate)


#one hot encode these
df$density_10bins0 <- ifelse(df$density_10bins == 0,1,0)
df$density_10bins1 <- ifelse(df$density_10bins == 1,1,0)
df$density_10bins2 <- ifelse(df$density_10bins == 2,1,0)
df$density_10bins3 <- ifelse(df$density_10bins == 3,1,0)
df$density_10bins4 <- ifelse(df$density_10bins == 4,1,0)
df$density_10bins5 <- ifelse(df$density_10bins == 5,1,0)
df$density_10bins6 <- ifelse(df$density_10bins == 6,1,0)
df$density_10bins7 <- ifelse(df$density_10bins == 7,1,0)
df$density_10bins8 <- ifelse(df$density_10bins == 8,1,0)
df$density_10bins9 <- ifelse(df$density_10bins == 9,1,0)

selected_zip <- c('density', 'density_bins', 'density_10bins') # add to selected features list later


# 1. custom functions for convenience
# check function: to check for na values, unique values, and data type
check <- function(x){
  uni <- unique(x)
  na <- sum(is.na(x))
  dtype <- class(x)
  cat("NA:", na, "\n")
  cat("unique Values:", uni, "\n")
  cat(dtype)
}

# def function to remove NA, returns the dataframe df
remove <- function(x){
  df <- df[which(is.na(x) == FALSE),]
  return(df)
}

# def function to impute missing values with mean(can choose to round off the decimal digits), or choose to impute a custom "value"
# this returns the input dataframe column
impute<- function(x, roundup= FALSE, value = NULL){
  if (roundup == TRUE){
    x[is.na(x)==TRUE] <- round(mean(x, na.rm = TRUE), 0)
  }
  else{
    if (missing(value)){
      x[is.na(x)==TRUE] <- mean(x, na.rm = TRUE)
    }
    else{
      x[is.na(x)==TRUE] <- value
    }
  }
  return(x)
}



# define function that take in variable name in df and add to a list called selected
add <- function(var){
  if ((var %in% selected)==FALSE){
    selected <- c(selected, as.character(var))
    }
  return(selected)
}


# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

typeandna<-function(x){
  
  print(typeof(x))
  print(sum(is.na(x)))
}


# 2. Preprocess Variables
# create list to store selected features
selected <- c("X1", "high_booking_rate")

#Add the density variables
selected <- add('density_10bins0')
selected <- add('density_10bins1')
selected <- add('density_10bins2')
selected <- add('density_10bins3')
selected <- add('density_10bins4')
selected <- add('density_10bins5')
selected <- add('density_10bins6')
selected <- add('density_10bins7')
selected <- add('density_10bins8')
selected <- add('density_10bins9')
selected<-add("density")

# var1 Access
df$access <- ifelse(is.na(df$access)== FALSE, 1, 0)
typeof(df$access)
df$access <- as.factor(df$access)
selected <- add('access')

# var2 accommodates---------------
### Remove 9 bad recoreds
df <- remove(df$accommodates)
typeof(df$accommodates)
df$accommodates <- as.numeric(df$accommodates)
selected <- add('accommodates') # using custom function to add variable name to list, meaning that we will select this as our feature
nrow(df)

# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
corpus <- (VectorSource(df$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0

# 1:100000 to c(1:length(df$amenities))
for (i in c(1:length(df$amenities))){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df$amenities_count <- vectored
typeof(df$amenities_count)
df$amenities_count <- as.numeric(df$amenities_count)
selected<- add('amenities_count')


# 4 availablity_30--------------
sum(is.na(df$availability_30))
df$availability_30 <-impute(df$availability_30)
typeof(df$availability_30)
selected <-add('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- df$availability_365-df$availability_90
typeof(df$availability_365)
selected <-add('availability_365')

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30
typeof(df$availability_60)
selected <-add('availability_60')

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30
sum(is.na(df$availability_90))
typeof(df$availability_90)
selected <-add('availability_90')

# var8 bathrooms---------------
##replacing all character data in bathrooms column with the mean(rounded)
# check the mean
mean(df$bathrooms, na.rm = TRUE) # mean is 1.28, and we will round to 1.5
df$bathrooms <- impute(df$bathrooms, value = 1.5)
typeandna(df$bathrooms)
selected <- add('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,ifelse(df$bed_type=='Pull-out',1,0))
typeandna(df$Real_Bed)
selected <- add('Real_Bed')

# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df$bedrooms <- impute(df$bedrooms, roundup = TRUE)
typeandna(df$bedrooms)
selected <- add('bedrooms')



# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df$beds <- impute(df$beds, roundup = TRUE)
typeandna(df$beds)
selected <- add('beds')


# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df$cancellation_policy[df$cancellation_policy == 'flexible']<-1
df$cancellation_policy[df$cancellation_policy == 'moderate']<-2
df$cancellation_policy[df$cancellation_policy == 'no_refunds']<-5
df$cancellation_policy[df$cancellation_policy == 'strict']<-6
df$cancellation_policy[df$cancellation_policy == 'super_strict_30']<-8
df$cancellation_policy[df$cancellation_policy == 'super_strict_60']<-10
df$cancellation_policy <- as.integer(df$cancellation_policy)
typeandna(df$cancellation_policy)
#selected <- add('cancellation_policy')

#one-hot-encoding-cancellation_policy
df$cp1 <- ifelse(df$cancellation_policy == 1,1,0)
df$cp2 <- ifelse(df$cancellation_policy == 2,1,0)
df$cp5 <- ifelse(df$cancellation_policy == 5,1,0)
df$cp6 <- ifelse(df$cancellation_policy == 6,1,0)
df$cp8 <- ifelse(df$cancellation_policy == 8,1,0)
df$cp10 <- ifelse(df$cancellation_policy == 10,1,0)

selected <-add("cp1")
selected <-add("cp2")
selected <-add("cp5")
selected <-add("cp6")
selected <-add("cp8")
selected <-add("cp10")

# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with zero. (don't charge cleaning fees)
df$cleaning_fee = as.numeric(substring(as.character(df$cleaning_fee),2))
df$cleaning_fee<- impute(df$cleaning_fee, value = 0)
typeandna(df$cleaning_fee)
selected <- add('cleaning_fee')


# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]

nrow(df) #99988
# 18 description
library(ngram)
df$description_wcount <- sapply(df$description, wordcount)
typeandna(df$description_wcount)
selected <- add('description_wcount')



# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))
typeandna(df$extra_people)
selected <- add('extra_people')



# 21 first_review---------------
## calculate time difference between first_review and now
df$first_review <- as.Date(df$first_review,origin="1960-10-01")
df$first_review <- difftime(Sys.Date(), df$first_review)
df$first_review<-as.integer(df$first_review)
typeandna(df$first_review)
selected <- add('first_review')


# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates
typeandna(df$guests_included)
selected <- add('guests_included')

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1)
typeandna(df$host_about)
selected <- add('host_about')


# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df$host_has_profile_pic <- impute(df$host_has_profile_pic, value = TRUE)
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)
typeandna(df$host_has_profile_pic)
selected <- add('host_has_profile_pic')

# 26 host_identity_verified---------------
## impute 142 instances with commono class
df$host_identity_verified <- impute(df$host_identity_verified, value = TRUE)
df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)
typeandna(df$host_identity_verified)
selected <- add('host_identity_verified')

# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
df$host_is_superhost<- impute(df$host_is_superhost, value = 0)
typeandna(df$host_is_superhost)
selected <- add('host_is_superhost')


# 28 host_listings_count---------------
### impute na with mean(round)
df$host_listings_count <- impute(df$host_listings_count, roundup = TRUE)
typeandna(df$host_listings_count)
selected <- add('host_listings_count')


# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean, and divide by 100. 
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
df$host_response_rate <- impute(df$host_response_rate, roundup = TRUE)
df$host_response_rate <- df$host_response_rate/100
typeandna(df$host_response_rate)
selected <- add('host_response_rate')


# 33 host_response_time---------------
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[df$host_response_time == 'within an hour'] <- 4
df$host_response_time[df$host_response_time =='within a few hours'] <- 3
df$host_response_time[df$host_response_time =='within a day'] <- 2
df$host_response_time[df$host_response_time == 'a few days or more'] <- 1
df$host_response_time <- as.numeric(df$host_response_time)
typeandna(df$host_response_time)
### impute na with the mode
#### use my Mode function

md <- Mode(df$host_response_time)
df$host_response_time[is.na(df$host_response_time)==TRUE] <- md
typeandna(df$host_response_time)
selected <- add('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df$experience, this also might affect other columns with 142 NAs
df$host_since <- as.Date(df$host_since, origin="1960-10-01")
df$experience <- difftime(Sys.Date(), df$host_since)
df$experience <- as.integer(df$experience)
df<- remove(df$experience)  
typeandna(df$experience)
nrow(df) # 99846
selected <- add('experience')



# 37 house_rules---------------
## no party no pets, no smoking


# 38 instant_bookable---------------
## convert to numeric 1,0 
df$instant_bookable <- ifelse(df$instant_bookable == TRUE, 1, 0)
typeandna(df$instant_bookable)
selected <- add('instant_bookable')


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
df$is_business_travel_ready <- impute(df$is_business_travel_ready, value = 0)
typeandna(df$is_business_travel_ready)
selected <- add('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)
typeandna(df$is_location_exact)
selected <- add('is_location_exact')


# 47 maximum_nights---------------
## create new var73 long_stay: allow long-stay(more than 7 days) or not
df['long_stay'] <- ifelse(df$maximum_nights >7, 1, 0)
typeandna(df$long_stay)
selected <- add('long_stay')


# 48 minimum_nights-----------------
selected <- add('minimum_nights')
typeandna(df$minimum_nights)
Mode(df$minimum_nights)


# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
df$price <- impute(df$price)
typeandna(df$price)
selected <- add('price')


# 55 property_type---------------
## Property type: group into larger groups and onehotencode
apartment <- c("Loft, House", "Apartment", "Condominium", "Timeshare", "Aparthotel", "Serviced apartment")
common_house <- c("Townhouse", "Bed and breakfast", "Bungalow", "Villa", "Vacation home", "Chalet")
side_house <- c("Guesthouse",  "In-law", "Dorm", "Guest suite", "Cabin", "Cottage",  "Farm stay", "Nature lodge")
hotel <- c("Hotel", "Boutique hotel", "Hostel")
special <- c("Castle", "Camper/RV", "Boat" , "Treehouse", "Tiny house", "Yurt", "Cave", "Casa particular (Cuba)", "Hut", "Tipi", "Earth House","Earth house", "Train", "Barn"  ,"Island" , "Plane", "Lighthouse" ) 

df['propertyApartment'] <- ifelse((df$property_type %in% apartment), 1, 0) #var77
df['propertyCommon_house'] <- ifelse((df$property_type %in% common_house), 1, 0) #var78
df['propertySide_house'] <- ifelse((df$property_type %in% side_house), 1, 0) #var79
df['propertyHotel'] <- ifelse((df$property_type %in% hotel), 1, 0) #var80
df['propertySpecial'] <- ifelse((df$property_type %in% special), 1, 0) #var81


selected <- add('propertyApartment')
selected <- add('propertyCommon_house')
selected <- add('propertySide_house')
selected <- add('propertyHotel')
selected <- add('propertySpecial')

df$propertyApartment <-as.factor(df$propertyApartment)
df$propertyCommon_house <-as.factor(df$propertyCommon_house)
df$propertySide_house <-as.factor(df$propertySide_house)
df$propertyHotel <-as.factor(df$propertyHotel)
df$propertySpecial <-as.factor(df$propertySpecial)
typeandna(df$propertySpecial)

  

# this code block is an alternative way to onehot encode all of the property types
if(FALSE){
library(mltools)
library(data.table)
newdata <- one_hot(as.data.table(as.factor(df$property_type)))
}





# 56 require_guest_phone_verification------------
df$require_guest_phone_verification <- ifelse(df$require_guest_phone_verification==TRUE, 1, 0)
df$require_guest_phone_verification<-as.factor(df$require_guest_phone_verification)
typeandna(df$require_guest_phone_verification)
selected <- add('require_guest_phone_verification')

# 57 require_guest_profile_picture----------
df$require_guest_profile_picture <- ifelse(df$require_guest_profile_picture==TRUE, 1, 0)
df$require_guest_profile_picture <-as.factor(df$require_guest_profile_picture)
typeandna(df$require_guest_profile_picture)
selected <- add('require_guest_profile_picture')

# 58 requires_license---------------
df$requires_license <- ifelse(df$requires_license==TRUE, 1, 0)
df$requires_license <-as.factor(df$requires_license)
typeandna(df$requires_license)
selected <- add('requires_license')

# 59 room_type---------------
## onehotencode. Created new three columns: var74 roomEntire home/apt, 75 roomPrivate room, 76 roomShared room
#room <- as.factor(df$room_type) 
#temp <- as.data.frame(model.matrix(~room)) # onehotencode
#colnames(temp)[1] <- 'roomEntire_home_apt'
#df <- cbind(df, temp)

library(mltools)
library(data.table)
#Impute 1 NA
df$room_type <- impute(df$room_type, value = 'Entire home/apt') # impute one NA with common class

room_ohe <- one_hot(as.data.table(as.factor(df$room_type)))

# rename columns
colnames(room_ohe) <- c('roomEntire_home_apt', 'roomPrivate_room', 'roomShared_room')
df <- cbind(df, room_ohe)

typeandna(df$roomEntire_home_apt)

selected <- add('roomEntire_home_apt')
selected <- add('roomPrivate_room')
selected <- add('roomShared_room')




# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)
df$security_deposit <- impute(df$security_deposit, value = 1) 
typeandna(df$security_deposit)
check(df$security_deposit)
selected <- add('security_deposit')

#Bella ---------------
df$price = as.numeric(gsub("[\\$,]", "", df$price))
df$weekly_price = as.numeric(gsub("[\\$,]", "", df$weekly_price))
df$monthly_price = as.numeric(gsub("[\\$,]", "", df$monthly_price))
df$weekly_daily_price <- df$weekly_price/7
df$monthly_daily_price <- df$monthly_price/30
df$weekly_discount<-(df$weekly_daily_price-df$price)/(df$price)
df$monthly_discount<-(df$monthly_daily_price-df$price)/(df$price)

week_mean<-mean(df$weekly_discount,na.rm=TRUE) # 0.9529783
month_mean<-mean(df$monthly_discount,na.rm=TRUE) # 0.756274

week_median<-median(df$weekly_discount,na.rm=TRUE) #0.9108341
month_median<-median(df$monthly_discount,na.rm=TRUE) #0.7272727

df$weekly_suitable<-ifelse(df$weekly_discount<=(week_median+week_mean)/2,0,1)
df$monthly_suitable<-ifelse(df$monthly_discount<=(month_median+month_mean)/2,0,1)

df$weekly_suitable[is.na(df$weekly_suitable)] = 0
df$monthly_suitable[is.na(df$monthly_suitable)] = 0
df$weekly_suitable<- as.character(df$weekly_suitable)
df$monthly_suitable<- as.character(df$monthly_suitable)

df$weekly_discount[is.na(df$weekly_discount)] = 0
df$monthly_discount[is.na(df$monthly_discount)]=0
df$weekly_discount<-scale(df$weekly_discount)
df$monthly_discount<-scale(df$monthly_discount)

nrow(df) # 12208
selected <- add('monthly_discount')
selected <- add('weekly_discount')
selected <- add('monthly_suitable')
selected <- add('weekly_suitable')

#---Queenie------------------------
#Importing the libraries
library(stringr)
for (i in c(1:length(df$amenities))){
  am1 <- gsub("[^A-Za-z0-9]", ",", df$amenities[i])
  df$new_amenities[i] <- str_replace(am1, "\\,", "")
  # df$new_amenities[i] <- str_replace_all(ame2, "\\?", "") 
}

#View(df)
## Internet/Wifi/Wireless
df$Internet <- ifelse(str_detect(tolower(df$new_amenities),"internet|wifi|wireless")==TRUE, 1, 0)

## Kitchen/Dishes/Dish washer/Cooking
df$Cooking <- ifelse(str_detect(tolower(df$new_amenities),"kitchen|dishes|cooking")==TRUE, 1, 0)


## Detector
df$Detector <- ifelse(str_detect(tolower(df$new_amenities),"smoke|extinguisher|detector")==TRUE, 1, 0)
df$Detector <- impute(df$Detector, roundup =  TRUE) # impute 2NAs with mode from test data

## Smoking Allowed
df$SmokingAllowed <- ifelse(str_detect(tolower(df$new_amenities),"smoking")==TRUE, 1, 0)
df$SmokingAllowed <- impute(df$SmokingAllowed, roundup =  TRUE) # impute 2NAs with mode from test data

## Essentials
df$Essentials <- ifelse(str_detect(tolower(df$new_amenities),"essentials")==TRUE,1,0)

## Lock
df$Lock <- ifelse(str_detect(tolower(df$new_amenities),"lock|lockbox")==TRUE,1,0)

## Parking
df$Parking <- ifelse(str_detect(tolower(df$new_amenities),"parking")==TRUE,1,0)

## Air_conditioning
df$Air_conditioning <- ifelse(str_detect(tolower(df$new_amenities),"conditioning")==TRUE,1,0)

## Wokspace
df$Workspace <- ifelse(str_detect(tolower(df$new_amenities),"workspace")==TRUE,1,0)

## Elevator
df$Elevator <- ifelse(str_detect(tolower(df$new_amenities),"elevator")==TRUE,1,0)

## Gym
df$Gym <- ifelse(str_detect(tolower(df$new_amenities),"gym")==TRUE,1,0)

## Refrigerator
df$Refrigerator <- ifelse(str_detect(tolower(df$new_amenities),"refrigerator")==TRUE,1,0)

## Stove
df$Stove <- ifelse(str_detect(tolower(df$new_amenities),"stove")==TRUE,1,0)

## Check
df$Check <- ifelse(str_detect(tolower(df$new_amenities),"24|check")==TRUE,1,0)

## Family
df$Family <- ifelse(str_detect(tolower(df$new_amenities),"kid|family")==TRUE,1,0)

selected <- add('Family')
selected <- add('Check')
selected <- add('Stove')
selected <- add('Refrigerator')
selected <- add('Gym')
selected <- add('Elevator')
selected <- add('Workspace')
selected <- add('Air_conditioning')
selected <- add('Parking')
selected <- add('Lock')
selected <- add('Essentials')
selected <- add('Detector')
selected <- add('SmokingAllowed')
selected <- add('Cooking')
selected <- add('Internet')


# house_rules
df$rules <- ifelse(is.na(df$house_rules)== FALSE, 1, 0)
selected <- add('rules')

# interaction
df$interaction <- ifelse(is.na(df$interaction)== FALSE, 1, 0)
selected <- add('interaction')


#-----------------------
#Normalisation

#df$accommodates <- scale(df$accommodates)
#df$amenities_count <- scale(df$amenities_count)
#df$availability_30 <- scale(df$availability_30)
#df$availability_365 <- scale(df$availability_365)
#df$availability_60 <- scale(df$availability_60)
#df$availability_90 <- scale(df$availability_90)
#df$bathrooms <- scale(df$bathrooms)
#df$beds <- scale(df$beds)
#df$bedrooms <- scale(df$bedrooms)
#df$cleaning_fee <- scale(df$cleaning_fee)
#df$extra_people <- scale(df$extra_people)
#df$first_review <- scale(df$first_review)
#df$guests_included <- scale(df$guests_included)
#df$host_listings_count <- scale(df$host_listings_count)
#df$maximum_nights <- scale(df$maximum_nights)
#df$price <- scale(df$price)
#df$experience <- scale(df$experience)



# 67 transit---------------
library(maxent)
library(RTextTools)
df$transit <- as.character(df$transit)
matrix <- create_matrix(df$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat <- as.matrix(matrix)

flexible <- data.frame(mat)
df$flexible <- (flexible$bike+flexible$bus+flexible$buses
                +flexible$metro+flexible$line+flexible$lines+flexible$subway
                +flexible$train+flexible$trains+flexible$transportation)

selected <- add('flexible')




#Imputing some remaining columns
df$availability_30 <- impute(df$availability_30)
df$availability_60 <- impute(df$availability_60)
df$availability_90 <- impute(df$availability_90)


#Adding the interaction terms:
df$price_propertyApartment <-df$price*df$propertyApartment
df$price_roomPrivate_room <- df$price*df$roomPrivate_room

selected<-add("price_propertyApartment")
selected<-add("price_roomPrivate_room")
#Not yet finalised-------------------
#k-means addition of variable "clustercategory"
if(FALSE){
df.X <-df[,-1] 
df.y <- df[,1]
which(is.na(df.X), arr.ind=TRUE)

km.out = kmeans(x=df.X,centers=5,nstart=10)
df.clusters <- cbind(df.y, km.out$cluster)
df$clustercategory <- df.clusters[,2]}
#------------------------------

# check the features that we select to use
#selected <- c(selected, selected_zip) -> train_cleaned8
selected<-add("density")#-> for train_cleaned9
print(selected)
length(selected) # 78

#Extracting restaurants near neighbourhood
df$neighbourhood_restaurant <- ifelse(str_detect(df$neighborhood_overview,regex("FOOD|Restaurant|restaurant|food|market|grocery|stores",ignore_case = T))==TRUE,1,0)
df$neighbourhood_restaurant <- impute(df$neighbourhood_restaurant, value =  0) # impute 2NAs with mode from test data
check(df$neighbourhood_restaurant)
selected <- add("neighbourhood_restaurant")
#df$neighbourhood_restaurant
#Add city centrality 
df$city_centrality <- ifelse(str_detect(df$neighborhood_overview,regex("center|center city|downtown",ignore_case = T))==TRUE,1,0)
df$city_centrality <- impute(df$city_centrality, value =  0) # impute 2NAs with mode from test data
sum(is.na(df$city_centrality))


#Adding latitude and longitude
df$latitude <- impute(df$latitude)
df$longitude <- impute(df$longitude)
selected <- add("latitude")
selected <-add("longitude")
which(is.na(df$longitude))
df$average_price_per_person <- (df$price/df$guests_included)
check(df$average_price_per_person)
selected <- add("average_price_per_person")


#Adding security_deposit
df$security_deposit <- impute(df$security_deposit,value = 0)
selected <- add("security_deposit")


#Adding maximum_nights
df$maximum_nights <- impute(df$maximum_nights)
check(df$maximum_nights)
selected <- add("maximum_nights")


selected
# Export as CSV file--------------------------------
export_train <- df[selected] # use the selected features 
colSums(is.na(export_train))
names(export_train)
# sapply(export_train, class)
# convert data type
export_train[,64:84] <- lapply(export_train[,64:84], factor) #"monthly_suitable"~ "interaction"  to factor
#export_train[,58:76] <- lapply(export_train[,58:76], factor) -> for train_cleaned9 because their positions have changed in names(df)
export_train[c('monthly_discount', 'weekly_discount')] <- sapply(export_train[c('monthly_discount', 'weekly_discount')], as.numeric)


# which(is.na(export_train), arr.ind=TRUE)
write.csv(export_train, file="train_cleaned20.csv", row.names = FALSE) #Write dataframe as CSV

