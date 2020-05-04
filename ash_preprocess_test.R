## this is the preprocessing for the test dataset

# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(ngram)
library(stringr)
library(maxent)

set.seed(12345)
#read data 
#has 12208 obs
df_test <- read_csv("airbnb_test_x.csv")
names(df_test)
#df_test <- read_csv("airbnb_test_x.csv")

# Merge df with zipcode dataframe that contains 'density' and 'density_bins'(4 bins)
zipcode <- read_csv("zipcode_combin_test.csv")
zipcode$X1 <- zipcode$X1+1
df_test <- merge(df_test, zipcode, by.x = 'X1', by.y = 'X1')

names(df_test)

#write.csv(df_test, file="take1.csv", row.names = FALSE)

# create density_10bins (factor)
df_test$density_10bins <- cut(df_test$density, 10, include.lowest=TRUE, labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
# convert data type
df_test$density_bins <- as.factor(df_test$density_bins) # convert to factor
#check(df$density_bins)
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



# define function that take in variable name in df_test and add to a list called selected_test
add_test <- function(var){
  if ((var %in% selected_test)==FALSE){
    selected_test <- c(selected_test, as.character(var))
  }
  return(selected_test)
}




# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#one hot encode these
df_test$density_10bins0 <- ifelse(df_test$density_10bins == 0,1,0)
df_test$density_10bins1 <- ifelse(df_test$density_10bins == 1,1,0)
df_test$density_10bins2 <- ifelse(df_test$density_10bins == 2,1,0)
df_test$density_10bins3 <- ifelse(df_test$density_10bins == 3,1,0)
df_test$density_10bins4 <- ifelse(df_test$density_10bins == 4,1,0)
df_test$density_10bins5 <- ifelse(df_test$density_10bins == 5,1,0)
df_test$density_10bins6 <- ifelse(df_test$density_10bins == 6,1,0)
df_test$density_10bins7 <- ifelse(df_test$density_10bins == 7,1,0)
df_test$density_10bins8 <- ifelse(df_test$density_10bins == 8,1,0)
df_test$density_10bins9 <- ifelse(df_test$density_10bins == 9,1,0)


selected_zip <- c('density', 'density_bins', 'density_10bins') # add to selected features list later


selected_test

# 2. Preprocess Variables
# create list to store selected features
selected_test <- c('X1')


check(df_test$density_10bins)


#Adding the density columns
selected_test <- add_test('density_10bins0')
selected_test <- add_test('density_10bins1')
selected_test <- add_test('density_10bins2')
selected_test <- add_test('density_10bins3')
selected_test <- add_test('density_10bins4')
selected_test <- add_test('density_10bins5')
selected_test <- add_test('density_10bins6')
selected_test <- add_test('density_10bins7')
selected_test <- add_test('density_10bins8')
selected_test <- add_test('density_10bins9')
selected_test <-add_test("density")
selected_test <- add_test("density_bins")
selected_test <- add_test("density_10bins")

# var1 Access---------------
df_test$access <- ifelse(is.na(df_test$access)== FALSE, 1, 0)
check(df_test$access)
selected_test <- add_test('access')

unique(df_test$access)

# var2 accommodates---------------
df_test$accommodates <- impute(df_test$accommodates, value = 4)
df_test$accommodates[775] <- 4
df_test$accommodates[10274] <-4
check(df_test$accommodates)
#df_test[775,"accommodates"] <-impute(df_test$accommodates, value = 4)
#which(df_test$accommodates== "t")
#unique(df_test$accommodates)

selected_test <- add_test('accommodates')# using custom function to add_test variable name to list, meaning that we will select this as our feature

#write.csv(df_test, file="take2.csv", row.names = FALSE)

# var3 amenities---------------
#creating the var72 amenities_count from the variable amenities
corpus <- (VectorSource(df_test$amenities))
corpus <- Corpus(corpus)
#summary(corpus)
vectored <-c()
#length(corpus)
#i=0
for (i in c(1:length(df_test$amenities))){
  if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 0){
    vectored <- c(vectored,0) }
  else if (length(unlist(strsplit(corpus[[i]]$content, "[,]"))) == 1){
    vectored <- c(vectored,0)}
  else{
    vectored <- c(vectored,length(unlist(strsplit(corpus[[i]]$content, "[,]"))))
  }
}

df_test$amenities_count <- vectored
check(df_test$amenities_count)
selected_test<- add_test('amenities_count')

check(df$amenities_count)

df_test$availability_30 <- as.numeric(df_test$availability_30)
df_test$availability_60 <- as.numeric(df_test$availability_60)

df_test$availability_90 <- as.numeric(df_test$availability_90)

df_test$availability_365 <- as.numeric(df_test$availability_365)


# 4 availability_30
## impute 2 NAs
df_test$availability_30 <- impute(df_test$availability_30, value = 11)
df_test$availability_30[775] <-11
df_test$availability_30[10274] <- 11
check(df_test$availability_30)
selected_test <-add_test('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df_test$availability_365 <- (df_test$availability_365 - df_test$availability_90)
df_test$availability_365 <- impute(df_test$availability_365, value =127 )
check(df_test$availability_365)
selected_test <-add_test('availability_365')

# var6 availability_60
df_test$availability_60 <- df_test$availability_60 - df_test$availability_30
df_test$availability_60 <- impute(df_test$availability_60, value = 14)
check(df_test$availability_60)
df_test$availability_60[775] <- 14
df_test$availability_60[10274] <- 14
selected_test <-add_test('availability_60')

# var7 availability_90
df_test$availability_90 <- df_test$availability_90 - df_test$availability_60 - df_test$availability_30
df_test$availability_90 <- impute(df_test$availability_90, value = 16)

df_test$availability_90[775] <- 16
df_test$availability_90[10274] <- 16
check(df_test$availability_90)
selected_test <-add_test('availability_90')

# var8 bathrooms---------------
##replacing 36 NA data in bathrooms column with the mean(rounded) if corresponding column in trainging data.
# check the mean
df_test$bathrooms <- impute(df_test$bathrooms, value = 1.5)
which(df_test$bathrooms == "Del Rey") #775
which(df_test$bathrooms == "Downtown") #10274
df_test$bathrooms[775] <- 1.5
df_test$bathrooms[10274] <-1.5
unique(df_test$bathrooms)
selected_test <- add_test('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df_test['Real_Bed'] <- ifelse(df_test$bed_type=='Real Bed',1,ifelse(df_test$bed_type=='Pull-out',1,0))
check(df_test$Real_Bed)
selected_test <- add_test('Real_Bed')


# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df_test$bedrooms <- impute(df_test$bedrooms, value = 1)
df_test$bedrooms[df_test$bedrooms == 'within an hour'] <- 1
check(df_test$bedrooms)
selected_test <- add_test("bedrooms")



# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df_test$beds <- impute(df_test$beds, value = 2)
check(df_test$beds)
df_test$beds[775] <- 2
df_test$beds[10274] <- 2
selected_test <- add_test('beds')


# 12 cancellation_policy---------------
## map different policy category to integers (1, 2, 5, 6, 8, 10)
df_test$cancellation_policy[df_test$cancellation_policy == 'flexible'] <- 1
df_test$cancellation_policy[df_test$cancellation_policy == 'moderate'] <- 2
df_test$cancellation_policy[df_test$cancellation_policy == 'no_refunds'] <- 5
df_test$cancellation_policy[df_test$cancellation_policy == 'strict'] <- 6
df_test$cancellation_policy[df_test$cancellation_policy == 'super_strict_30'] <- 8
df_test$cancellation_policy[df_test$cancellation_policy == 'super_strict_60'] <- 10
df_test$cancellation_policy <- impute(df_test$cancellation_policy, value = 1)
#selected_test <- add_test("cancellation_policy")

#one-hot-encoding-cancellation_policy
df_test$cp1 <- ifelse(df_test$cancellation_policy == 1,1,0)
df_test$cp2 <- ifelse(df_test$cancellation_policy == 2,1,0)
df_test$cp5 <- ifelse(df_test$cancellation_policy == 5,1,0)
df_test$cp6 <- ifelse(df_test$cancellation_policy == 6,1,0)
df_test$cp8 <- ifelse(df_test$cancellation_policy == 8,1,0)
df_test$cp10 <- ifelse(df_test$cancellation_policy == 10,1,0)
check(df_test$cancellation_policy)
selected_test <-add_test("cp1")
selected_test <-add_test("cp2")
selected_test <-add_test("cp5")
selected_test <-add_test("cp6")
selected_test <-add_test("cp8")
selected_test <-add_test("cp10")

# 15 cleaning_fee (originally 18325 NA)---------------
## impute NA with mean
df_test$cleaning_fee = as.numeric(substring(as.character(df_test$cleaning_fee),2))
df_test$cleaning_fee<- impute(df_test$cleaning_fee, value = 54.72)
selected_test <- add_test("cleaning_fee")
check(df_test$cleaning_fee)
# 18 description--------------------------
df_test$description_wcount <- sapply(df_test$description, wordcount)
check(df_test$description_wcount)
selected_test <- add_test('description_wcount')




# 20 extra_people---------------
## remove dollar sign
df_test$extra_people <- as.numeric(substring(as.character(df_test$extra_people), first = 2))
df_test$extra_people <- impute(df_test$extra_people, value = 0)
check(df_test$extra_people)
selected_test <- add_test('extra_people')

# 21 first_review---------------
## calculate time difference between first_review and now
df_test$first_review <- as.Date(df_test$first_review, format='%d-%m-%Y', origin="01-10-1960")
df_test$first_review <- difftime(Sys.Date(), df_test$first_review)
df_test$first_review<- as.integer(df_test$first_review)
df_test$first_review<- impute(df_test$first_review, value = )
selected_test <- add_test('first_review')
df_test$first_review
#write.csv(df_test,file="take3.csv", row.names = FALSE)

# 22 guests_included---------------
df_test$guests_included <- impute(df_test$guests_included, value = 2) # rounded mean 
check(df_test$guests_included)
df_test$guests_included[775] <- 2
df_test$guests_included[10274] <- 2

selected_test <- add_test("guests_included")

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df_test$host_about <- ifelse(is.na(df_test$host_about)== TRUE, 0, 1)
check(df_test$host_about)
selected_test <- add_test('host_about')


# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df_test$host_has_profile_pic[775] <- "t"
df_test$host_has_profile_pic[10274] <- "t"
df_test$host_has_profile_pic <- ifelse(df_test$host_has_profile_pic == "t", 1, 0)
df_test$host_has_profile_pic <- impute(df_test$host_has_profile_pic, value = 1)
check(df_test$host_has_profile_pic)

#which(df_test$host_has_profile_pic == 1)
selected_test <- add_test('host_has_profile_pic')

# 26 host_identity_verified---------------
## impute 142 instances with commono class
df_test$host_identity_verified <- ifelse(df_test$host_identity_verified == TRUE, 1, 0)
df_test$host_identity_verified <- impute(df_test$host_identity_verified, value = TRUE)
check(df_test$host_identity_verified)
selected_test <- add_test('host_identity_verified')


# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
#df_test$host_is_superhost <- as.character(df_test$host_is_superhost)
df_test$host_is_superhost[775] <- 0
df_test$host_is_superhost[10274] <- 0
df_test$host_is_superhost <- ifelse(df_test$host_is_superhost == "t",1,0)
df_test$host_is_superhost<- impute(df_test$host_is_superhost, value = 0)
check(df_test$host_is_superhost)

selected_test <- add_test('host_is_superhost')

#write.csv(df_test,file = "take4.csv",row.names = FALSE)

# 28 host_listings_count---------------
### impute na with mean(round)
df_test$host_listings_count[775] <- 10
df_test$host_listings_count[10274] <- 10
df_test$host_listings_count <- impute(df_test$host_listings_count, value = 10)
check(df_test$host_listings_count)

selected_test <- add_test('host_listings_count')


# 32 host_response_rate (15793 NA)---------------???
## remove % sign, and impute rounded mean, and divide by 100. 
df_test$host_response_rate <- impute(df_test$host_response_rate, value = 96)
df_test$host_response_rate <- as.numeric(gsub("\\%", '', df_test$host_response_rate)) # remove % sign
df_test$host_response_rate <- impute(df_test$host_response_rate, value = 96)
df_test$host_response_rate <- df_test$host_response_rate/100
check(df_test$host_response_rate)
selected_test <- add_test('host_response_rate')


# 33 host_response_time---------------
df_test$host_response_time <- as.character(df_test$host_response_time)
df_test$host_response_time[df_test$host_response_time == 'within an hour'] <- 4
df_test$host_response_time[df_test$host_response_time =='within a few hours'] <- 3
df_test$host_response_time[df_test$host_response_time =='within a day'] <- 2
df_test$host_response_time[df_test$host_response_time == 'a few days or more'] <- 1
df_test$host_response_time <- as.numeric(df_test$host_response_time)
df_test$host_response_time[is.na(df_test$host_response_time)==TRUE] <- 4
check(df_test$host_response_time)
selected_test <- add_test('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df_test$experience, this also might affect other columns with 142 NAs
df_test$host_since <- as.Date(df_test$host_since, origin="01-10-1960")
df_test$experience <- difftime(Sys.Date(), df_test$host_since)
df_test$experience <- as.integer(df_test$experience)
df_test$experience <- impute(df_test$experience)
check(df_test$experience)
selected_test <- add_test('experience')
## imupte 21 NA with rounded mean from test data
#df_test$experience <- impute(df_test$experience, roundup = TRUE)

#write.csv(df_test,file="take5.csv",row.names = FALSE)
# 38 instant_bookable---------------
## convert to numeric 1,0 
## impute 3 NAs
df_test$instant_bookable[775] <- "f"
df_test$instant_bookable[10274] <- "f"
df_test$instant_bookable <- ifelse((df_test$instant_bookable == "t"), 1, 0)
df_test$instant_bookable <- impute(df_test$instant_bookable, value = 0)
check(df_test$instant_bookable)

selected_test <- add_test('instant_bookable')


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
check(df_test$is_business_travel_ready)
df_test$is_business_travel_ready[775] <- "f"
df_test$is_business_travel_ready[10274] <- "f"
df_test$is_business_travel_ready <- impute(df_test$is_business_travel_ready, value = 0)
df_test$is_business_travel_ready<-ifelse(df_test$is_business_travel_ready=="t",1,0)
check(df_test$is_business_travel_ready)
selected_test <- add_test('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
check(df_test$is_location_exact)
df_test$is_location_exact<- ifelse(df_test$is_location_exact== TRUE, 1, 0)
df_test$is_location_exact <- impute(df_test$is_location_exact, value = 1)
selected_test <- add_test('is_location_exact')


# 47 maximum_nights---------------
## create new var73: allow long-stay(more than 7 days) or not
check(df_test$long_stay)
df_test['long_stay'] <- ifelse(df_test$maximum_nights >7, 1, 0)
df_test$long_stay <- impute(df_test$long_stay, value = 1)
selected_test <- add_test('long_stay')


# 48 minimum_nights-----------------
## impute with mode: 1
check(df_test$minimum_nights)
df_test$minimum_nights <- impute(df_test$minimum_nights, value = 1)
selected_test <- add_test('minimum_nights')

#write.csv(df_test,file = "take6.csv",row.names = FALSE)

# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
check(df_test$price)
df_test$price = as.numeric(substring(as.character(df_test$price),2))
df_test$price <- impute(df_test$price, value = 145)
selected_test <- add_test('price')
check(df_test$price)

# 55 property_type---------------
## Property type: group into larger groups and onehotencode
apartment <- c("Loft, House", "Apartment", "Condominium", "Timeshare", "Aparthotel", "Serviced apartment")
common_house <- c("Townhouse", "Bed and breakfast", "Bed & Breakfast", "Bungalow", "Villa", "Vacation home", "Chalet", "Resort")
side_house <- c("Guesthouse",  "In-law", "Dorm", "Guest suite", "Cabin", "Cottage",  "Farm stay", "Nature lodge")
hotel <- c("Hotel", "Boutique hotel", "Hostel")
special <- c("Castle", "Camper/RV", "Boat" , "Treehouse", "Tiny house", "Yurt", "Cave", "Casa particular (Cuba)", "Hut", "Tipi", "Earth House","Earth house", "Train", "Barn"  ,"Island" , "Plane", "Lighthouse", "Other", "Camper/RV", "Tent") 

## impute 3 NA with the most common type: Apartment
check(df_test$property_type)
df_test$property_type <- impute(df_test$property_type, value = 'Apartment')

## onehotencode
df_test['propertyApartment'] <- ifelse((df_test$property_type %in% apartment), 1, 0) #var77
df_test['propertyCommon_house'] <- ifelse((df_test$property_type %in% common_house), 1, 0) #var78
df_test['propertySide_house'] <- ifelse((df_test$property_type %in% side_house), 1, 0) #var79
df_test['propertyHotel'] <- ifelse((df_test$property_type %in% hotel), 1, 0) #var80
df_test['propertySpecial'] <- ifelse((df_test$property_type %in% special), 1, 0) #var81



selected_test <- add_test('propertyApartment')
selected_test <- add_test('propertyCommon_house')
selected_test <- add_test('propertySide_house')
selected_test <- add_test('propertyHotel')
selected_test <- add_test('propertySpecial')


# 56 require_guest_phone_verification------------
df_test$require_guest_phone_verification <- impute(df_test$require_guest_phone_verification, value = FALSE)
df_test$require_guest_phone_verification <- ifelse(df_test$require_guest_phone_verification==TRUE, 1, 0)
selected_test <- add_test('require_guest_phone_verification')
check(df_test$require_guest_phone_verification)
# 57 require_guest_profile_picture----------
df_test$require_guest_profile_picture <- impute(df_test$require_guest_profile_picture, value = FALSE)
df_test$require_guest_profile_picture <- ifelse(df_test$require_guest_profile_picture==TRUE, 1, 0)
selected_test <- add_test('require_guest_profile_picture')
check(df_test$require_guest_profile_picture)


# 58 requires_license---------------
df_test$requires_license <- impute(df_test$requires_license, value = FALSE)
df_test$requires_license <- ifelse(df_test$requires_license==TRUE, 1, 0)
selected_test <- add_test('requires_license')
check(df_test$requires_license)



# 59 room_type---------------
check(df_test$room_type)
df_test$room_type <- impute(df_test$room_type, value = 'Entire home/apt') # impute one NA with common class
## onehotencode. Created new three columns: var74 roomEntire home/apt, 75 roomPrivate room, 76 roomShared room



library(mltools)
library(data.table)
room_ohe_test <- one_hot(as.data.table(as.factor(df_test$room_type)))
# rename columns
colnames(room_ohe_test) <- c('roomEntire_home_apt', 'roomPrivate_room', 'roomShared_room')
df_test <- cbind(df_test, room_ohe_test)

selected_test <- add_test('roomEntire_home_apt')
selected_test <- add_test('roomPrivate_room')
selected_test <- add_test('roomShared_room')


# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
check(df_test$security_deposit)
df_test$security_deposit <- as.character(df_test$security_deposit)
df_test$security_deposit <- as.numeric(substring(as.character(df_test$security_deposit),2))
df_test$security_deposit <- ifelse(df_test$security_deposit>0,1,0)
df_test$security_deposit <- impute(df_test$security_deposit, value = 1) 

selected_test <- add_test('security_deposit')


# 67 transit---------------
library(maxent)
library(RTextTools)
df_test$transit <- as.character(df_test$transit)
matrix_test <- create_matrix(df_test$transit, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
mat_test <- as.matrix(matrix_test)

flexible <- data.frame(mat_test)
df_test$flexible <- (flexible$bike+flexible$bus+flexible$buses
                     +flexible$metro+flexible$line+flexible$lines+flexible$subway
                     +flexible$train+flexible$trains+flexible$transportation)


selected_test <- add_test('flexible')
selected_test


#---------Bella Code------------------
check(df_test$price)
df_test$price = as.numeric(gsub("[\\$,]", "", df_test$price))
df_test$weekly_price = as.numeric(gsub("[\\$,]", "", df_test$weekly_price))
df_test$monthly_price = as.numeric(gsub("[\\$,]", "", df_test$monthly_price))
df_test$weekly_daily_price <- df_test$weekly_price/7
df_test$monthly_daily_price <- df_test$monthly_price/30
df_test$weekly_discount<-(df_test$weekly_daily_price-df_test$price)/(df_test$price)
df_test$monthly_discount<-(df_test$monthly_daily_price-df_test$price)/(df_test$price)

week_mean<-mean(df_test$weekly_discount,na.rm=TRUE) # 0.9529783
month_mean<-mean(df_test$monthly_discount,na.rm=TRUE) # 0.756274

week_median<-median(df_test$weekly_discount,na.rm=TRUE) #0.9108341
month_median<-median(df_test$monthly_discount,na.rm=TRUE) #0.7272727

df_test$weekly_suitable<-ifelse(df_test$weekly_discount<=(week_median+week_mean)/2,0,1)
df_test$monthly_suitable<-ifelse(df_test$monthly_discount<=(month_median+month_mean)/2,0,1)

df_test$weekly_suitable[is.na(df_test$weekly_suitable)] = 0
df_test$monthly_suitable[is.na(df_test$monthly_suitable)] = 0
df_test$weekly_suitable<- as.factor(df_test$weekly_suitable)
df_test$monthly_suitable<- as.factor(df_test$monthly_suitable)

df_test$weekly_discount[is.na(df_test$weekly_discount)] = 0
df_test$monthly_discount[is.na(df_test$monthly_discount)]=0
df_test$weekly_discount<-scale(df_test$weekly_discount)
df_test$monthly_discount<-scale(df_test$monthly_discount)
df_test$weekly_discount<-as.numeric(df_test$weekly_discount)
df_test$monthly_discount<-as.numeric(df_test$monthly_discount)

nrow(df_test) # 12208
selected_test <- add_test('monthly_discount')
selected_test <- add_test('weekly_discount')
selected_test <- add_test('monthly_suitable')
selected_test <- add_test('weekly_suitable')

#---Queenie------------------------
#Importing the libraries
library(stringr)
#----------------------------------------------------------------------------

#24-hour-check-in
df_test$CheckIn24 <- ifelse(str_detect((df_test$amenities),"24-hour check-in")==TRUE,1,0)
df_test$CheckIn24 <- impute(df_test$CheckIn24, roundup =  TRUE) # impute 2NAs with mode from test data
check(df_test$CheckIn24)

df_test$air_conditioning<- ifelse(str_detect((df_test$amenities),"Air conditioning|Central air conditioning")==TRUE,1,0)
df_test$air_conditioning <- impute(df_test$air_conditioning, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$high_end_electronics <- ifelse(str_detect((df_test$amenities),"Amazon Echo|Apple TV|Game console|Netflix|Projector and screen|Smart TV")==TRUE,1,0)
df_test$high_end_electronics <- impute(df_test$high_end_electronics, roundup =  TRUE) # impute 2NAs with mode from test data



df_test$bbq <- ifelse(str_detect((df_test$amenities),"BBQ grill|Fire pit|Propane barbeque")==TRUE,1,0)
df_test$bbq <- impute(df_test$bbq, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$balcony <- ifelse(str_detect((df_test$amenities),"Balcony|Patio")==TRUE,1,0)
df_test$balcony <- impute(df_test$balcony, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$nature_and_views <- ifelse(str_detect((df_test$amenities),"Beach view|Beachfront|Lake access|Mountain view|Ski-in/Ski-out|Waterfront")==TRUE,1,0)
df_test$nature_and_views <- impute(df_test$nature_and_views, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$bed_linen <- ifelse(str_detect((df_test$amenities),"Bed linens")==TRUE,1,0)
df_test$bed_linen <- impute(df_test$bed_linen, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$breakfast <- ifelse(str_detect((df_test$amenities),"Breakfast")==TRUE,1,0)
df_test$breakfast <- impute(df_test$breakfast, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$tv <- ifelse(str_detect((df_test$amenities),"TV")==TRUE,1,0)
df_test$tv <- impute(df_test$tv, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$coffee_machine <- ifelse(str_detect((df_test$amenities),"Coffee maker|Espresso machine")==TRUE,1,0)
df_test$coffee_machine <- impute(df_test$coffee_machine, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$kitchen <- ifelse(str_detect((df_test$amenities),"Cooking basics|kitchen|dishes|cooking")==TRUE,1,0)
df_test$kitchen <- impute(df_test$kitchen, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$white_goods <- ifelse(str_detect((df_test$amenities),"Dishwasher|Dryer|Washer")==TRUE,1,0)
df_test$white_goods <- impute(df_test$white_goods, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$elevator <- ifelse(str_detect((df_test$amenities),"Elevator")==TRUE,1,0)
df_test$elevator <- impute(df_test$elevator, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$gym <- ifelse(str_detect((df_test$amenities),"Exercise equipment|Gym|gym")==TRUE,1,0)
df_test$gym <- impute(df_test$gym, roundup =  TRUE) # impute 2NAs with mode from test data


df_test$child_friendly <- ifelse(str_detect((df_test$amenities),"Family/kid friendly|Children|children")==TRUE,1,0)
df_test$child_friendly <- impute(df_test$child_friendly, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$parking <- ifelse(str_detect((df_test$amenities),"parking")==TRUE,1,0)
df_test$parking <- impute(df_test$parking, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$outdoor_space <- ifelse(str_detect((df_test$amenities),"Garden|Outdoor|Sun loungers|Terrace")==TRUE,1,0)
df_test$outdoor_space <- impute(df_test$outdoor_space, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$host_greeting <- ifelse(str_detect((df_test$amenities),"Host greets you")==TRUE,1,0)
df_test$host_greeting <- impute(df_test$host_greeting, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$hottub_sauna_pool <- ifelse(str_detect((df_test$amenities),"Hot tub|Jetted tub|hot tub|Sauna|Pool|pool")==TRUE,1,0)
df_test$hottub_sauna_pool <- impute(df_test$hottub_sauna_pool, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$internet <- ifelse(str_detect((df_test$amenities),"Internet|Pocket wifi|Wifi|Wireless")==TRUE,1,0)
df_test$internet <- impute(df_test$internet, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$long_term_Stay_allowed <- ifelse(str_detect((df_test$amenities),"Long term stays allowed")==TRUE,1,0)
df_test$long_term_Stay_allowed <- impute(df_test$long_term_Stay_allowed, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$pets_allowed <- ifelse(str_detect((df_test$amenities),"Pets|pet|Cat(s)|Dog(s)")==TRUE,1,0)
df_test$pets_allowed <- impute(df_test$pets_allowed, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$private_entrance <- ifelse(str_detect((df_test$amenities),"Private entrance")==TRUE,1,0)
df_test$private_entrance <- impute(df_test$private_entrance, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$secure <- ifelse(str_detect((df_test$amenities),"Safe|Security system|Lock")==TRUE,1,0)
df_test$secure <- impute(df_test$secure, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$self_check_in <- ifelse(str_detect((df_test$amenities),"Self check-in")==TRUE,1,0)
df_test$self_check_in <- impute(df_test$self_check_in, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$smoking_allowed <- ifelse(str_detect((df_test$amenities),"Smoking allowed")==TRUE,1,0)
df_test$smoking_allowed <- impute(df_test$smoking_allowed, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$accessible <- ifelse(str_detect((df_test$amenities),"Step-free access|Wheelchair|Accessible")==TRUE,1,0)
df_test$accessible <- impute(df_test$accessible, roundup =  TRUE) # impute 2NAs with mode from test data

df_test$event_suitable <- ifelse(str_detect((df_test$amenities),"Suitable for events")==TRUE,1,0)
df_test$event_suitable <- impute(df_test$event_suitable, roundup =  TRUE) # impute 2NAs with mode from test data


selected_test <- add_test('CheckIn24')
selected_test <- add_test('air_conditioning')
selected_test <- add_test('high_end_electronics')
selected_test <- add_test('bbq')
selected_test <- add_test('balcony')
selected_test <- add_test('nature_and_views')
selected_test <- add_test('bed_linen')
selected_test <- add_test('breakfast')
selected_test <- add_test('tv')
selected_test <- add_test('coffee_machine')
selected_test <- add_test('kitchen')
selected_test <- add_test('white_goods')
selected_test <- add_test('elevator')
selected_test <- add_test('gym')
selected_test <- add_test('child_friendly')
selected_test <- add_test('parking')
selected_test <- add_test('outdoor_space')
selected_test <- add_test('host_greeting')
selected_test <- add_test('hottub_sauna_pool')
selected_test <- add_test('internet')
selected_test <- add_test('long_term_Stay_allowed')
selected_test <- add_test('pets_allowed')
selected_test <- add_test('private_entrance')
selected_test <- add_test('secure')
selected_test <- add_test('self_check_in')
selected_test <- add_test('smoking_allowed')
selected_test <- add_test('accessible')
selected_test <- add_test('event_suitable')

#write.csv(df_test,file = "take7.csv",row.names = FALSE)


# house_rules-------------------

df_test$rules <- ifelse(is.na(df_test$house_rules)== FALSE, 1, 0)
check(df_test$rules)
selected_test <- add_test('rules')

# interaction-------------------
df_test$interaction <- ifelse(is.na(df_test$interaction)== FALSE, 1, 0)
check(df_test$interaction)
selected_test <- add_test('interaction')

#Adding the interaction terms:
df_test$price_propertyApartment <-df_test$price*df_test$propertyApartment
check(df_test$price_propertyApartment)
df_test$price_roomPrivate_room <- df_test$price*df_test$roomPrivate_room
check(df_test$price_roomPrivate_room)

selected_test <-add_test("price_propertyApartment")
selected_test <-add_test("price_roomPrivate_room")

#View(df_test[,selected_test])
#-----------------------
#Normalisation
if(FALSE){
  df_test$accommodates <- scale(df_test$accommodates)
  df_test$amenities_count <- scale(df_test$amenities_count)
  df_test$availability_30 <- scale(df_test$availability_30)
  df_test$availability_365 <- scale(df_test$availability_365)
  df_test$availability_60 <- scale(df_test$availability_60)
  df_test$availability_90 <- scale(df_test$availability_90)
  df_test$bathrooms <- scale(df_test)
  df_test$beds <- scale(df_test$beds)
  df_test$bedrooms <- scale(df_test$bedrooms)
  df_test$cleaning_fee <- scale(df_test$cleaning_fee)
  df_test$extra_people <- scale(df_test$extra_people)
  df_test$first_review <- scale(df_test$first_review)
  df_test$guests_included <- scale(df_test$guests_included)
  df_test$host_listings_count <- scale(df_test$host_listings_count)
  df_test$maximum_nights <- scale(df_test$maximum_nights)
  df_test$price <- scale(df_test$price)
  df_test$experience <- scale(df_test$experience)
}

#Imputing some remaining columns
sum(is.na(df_test$availability_90))
df_test$availability_30 <- impute(df_test$availability_30)
df_test$availability_60 <- impute(df_test$availability_60)
df_test$availability_90 <- impute(df_test$availability_90)
#df_test$experience <- impute(df_test$experience)
selected
df_test$availability_30[775]

#Adding latitude and longitude
which(is.na(df_test$longitude))
df_test$latitude <- impute(df_test$latitude)
df_test$latitude[1539] <- 0
df_test$longitude <- impute(df_test$longitude)
df_test$longitude[1539] <- 0
selected_test <- add_test("latitude")
selected_test <-add_test("longitude")


df_test$average_price_per_person <- (df_test$price/df_test$guests_included)
selected_test <- add_test("average_price_per_person")

#Adding maximum_nights
df_test$maximum_nights <- impute(df_test$maximum_nights)
unique(df_test$maximum_nights)
selected_test <- add_test("maximum_nights")

#Adding security_deposit
df_test$security_deposit <- impute(df_test$security_deposit,value = 0)
check(df_test$security_deposit)
selected_test <- add_test("security_deposit")


#Adding market_popularity -> We are not using this code because it is rearranging
#the rows
#market_list <- NULL
#df_test$market[is.na(df_test$market)] <- 'Others'
#for (i in unique(df_test$market)){
#  x <- subset(df_test,df_test$market==i)
#  market_list <- c(market_list,nrow(x))
#}
#market_list <- data.frame(unique(df_test$market),market_list)


#names(market_list)[1:2] <- c('market_name','num_listings')

#df_test <- merge(df_test, market_list, by.x='market', by.y='market_name')

#write.csv(df_test,file = "take8.csv",row.names = FALSE)

#selected_test <- add_test("num_listings")

#-------Extracting if neighbourhood has restaurants


#Extracting restaurants near neighbourhood
df_test$neighbourhood_restaurant <- ifelse(str_detect((df_test$neighborhood_overview),regex("Restaurant|restaurant|food|market|grocery|stores",ignore_case = T))==TRUE,1,0)
df_test$neighbourhood_restaurant <- impute(df_test$neighbourhood_restaurant, value =  0) # impute 2NAs with mode from test data
check(df_test$neighbourhood_restaurant)
selected_test <- add_test("neighbourhood_restaurant")

#Add city centrality 
df_test$city_centrality <- ifelse(str_detect((df_test$neighborhood_overview),regex("center|center city|downtown",ignore.case = T))==TRUE,1,0)
df_test$city_centrality <- impute(df_test$city_centrality, value =  0) # impute 2NAs with mode from test data
#sum(is.na(df$city_centrality))
check(df_test$city_centrality)
selected_test <- add_test("city_centrality")


#Kmeans on the test dataset
#Not yet finalised--------------------
#For test dataset
if(FALSE){df_test.X <-df_test[,selected_test]
which(is.na(df_test.X), arr.ind=TRUE)
km.out = kmeans(x=df_test.X,centers=5,nstart=10)
}
#---------------------------------

#write.csv(df_test,file = "take10.csv",row.names = FALSE)

selected_test
# check the features that we select to use
selected_zip
selected_export <-selected_test
#selected_export <- c(selected_test, selected_zip)
#selected_export <- c(selected,"density")
print(selected_export)
length(selected_export) # 102
selected_test
#selected_export <- selected_test[-2]
selected_export <- selected_test # 77
length(selected_export) # 84 for test_10
# Export as CSV file
export <- df_test[selected_export] # use the selected_test features
# columns that contain NAs:
unique(which(is.na(export), arr.ind=TRUE)[, 'col'])
sum(is.na(export$first_review))

export$first_review[12208] 
names(export[, unique(which(is.na(export), arr.ind=TRUE)[, 'col'])])
which(is.na(export$longitude))


export[,64:95] <- lapply(export[,64:95], factor) #Cindy, I am dicey about this line as well as the next
export[c('monthly_discount', 'weekly_discount')] <- sapply(export[c('monthly_discount', 'weekly_discount')], as.numeric)

#View(export)
names(export)

write.csv(export, file="test_cleaned12)revised.csv", row.names = FALSE) #Write dataframe as CSV


