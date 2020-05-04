#load libraries
# 0. Load Libraries
library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(ngram)
library(stringr)
library(maxent)

#training set
#read data col_types = cols(zipcode = col_character()
train_x <- read_csv("airbnb_train_x.csv")
train_y <- read_csv("airbnb_train_y.csv")
# merge features and label
df <- merge(train_x, train_y, by.x = 'X1', by.y = 'X1')

# Merge df with zipcode dataframe that contains 'density' and 'density_bins'(4 bins)
zipcode_1 <- read_csv("zipcode_combin_train.csv")
zipcode_1$X1 <- zipcode_1$X1+1
df <- merge(df, zipcode_1, by.x = 'X1', by.y = 'X1')

#testing set
df_test <- read_csv("airbnb_test_x.csv")
zipcode_2 <- read_csv("zipcode_combin_test.csv")
zipcode_2$X1 <- zipcode_2$X1+1
df_test <- merge(df_test, zipcode_2, by.x = 'X1', by.y = 'X1')

library(gtools)

df_whole <- smartbind(df, df_test)
#View(df_whole)
#View(df_whole)
nrow(df_whole) #112208

#write.csv(df_whole,file = "df_whole.csv",row.names = FALSE)
colnames(df_whole)
View(df_whole)
df_whole$density_10bins <- cut(df_whole$density, 10, include.lowest=TRUE, labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
# convert data type
df_whole$density_bins <- as.factor(df_whole$density_bins) # convert to factor
df_whole$high_booking_rate <- as.factor(df_whole$high_booking_rate)

library(stringr)
(which(str_detect(rownames(df_whole),"2:")))

df_train <-df_whole[which(str_detect(rownames(df_whole),"2:")),]
nrow(df_train)
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

nrow(df) #112208

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
selected<-add("density_bins")
selected<-add("density_10bins")

# var1 Access
check(df$access)
df$access <- ifelse(is.na(df$access)== FALSE, 1, 0)
selected <- add('access')


# var2 accommodates---------------
### Remove 9 bad recoreds
df <- remove(df$accommodates)
nrow(df)
selected <- add('accommodates') # using custom function to add variable name to list, meaning that we will select this as our feature

#imputing high_booking_rate
df$high_booking_rate<-impute(df$high_booking_rate,value = 1)


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
selected<- add('amenities_count')

# 4 availablity_30--------------
selected <-add('availability_30')

# 5 availability_365---------------
#making changes to availability_60 and avail_90 to make them independent of availabi_30 and so on
df$availability_365 <- df$availability_365-df$availability_90
selected <-add('availability_365')

# var6 availability_60
df$availability_60 <- df$availability_60 - df$availability_30
selected <-add('availability_60')

# var7 availability_90
df$availability_90 <- df$availability_90 - df$availability_60 - df$availability_30
selected <-add('availability_90')

# var8 bathrooms---------------
##replacing all character data in bathrooms column with the mean(rounded)
# check the mean
mean(df$bathrooms, na.rm = TRUE) # mean is 1.28, and we will round to 1.5
df$bathrooms <- impute(df$bathrooms, value = 1.5)
selected <- add('bathrooms')

# var9 bed_type---------------
# create new var70 : Real_Bed
df['Real_Bed'] <- ifelse(df$bed_type=='Real Bed',1,ifelse(df$bed_type=='Pull-out',1,0))
selected <- add('Real_Bed')

# var10 bedrooms---------------
## impute 92 NA with mean, and roundup to no decimals
df$bedrooms <- impute(df$bedrooms, roundup = TRUE)
selected <- add('bedrooms')



# 11 beds---------------
## impute 83 NA with mean, and roundup to no decimals
df$beds <- impute(df$beds, roundup = TRUE)
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
selected <- add('cleaning_fee')


# 17 country_code---------------
## removed 3 rows where country not US
df <- df[which(df$country_code == 'US'),]
nrow(df)
# 18 description
library(ngram)
df$description_wcount <- sapply(df$description, wordcount)
selected <- add('description_wcount')



# 20 extra_people---------------
## remove dollar sign
df$extra_people <- as.numeric(substring(as.character(df$extra_people), first = 2))
selected <- add('extra_people')



# 21 first_review---------------
## calculate time difference between first_review and now
#df$first_review <- as.Date(df$first_review,origin="1960-10-01")
#df$first_review <- difftime(Sys.Date(), df$first_review)
#df$first_review<-as.integer(df$first_review)
#selected <- add('first_review')


# 22 guests_included---------------
## very clean but have to think about relationship with extra fee and accommodates
selected <- add('guests_included')

# 23 host_about---------------
## if the hosts didn't write anything about themselves, take 0, otherwise 1
df$host_about <- ifelse(is.na(df$host_about)== TRUE, 0, 1)
check(df$host_about)
selected <- add('host_about')


# 25 host_has_profile_pic---------------
### impute most common class because almost everyon has profile pic
df$host_has_profile_pic <- impute(df$host_has_profile_pic, value = TRUE)
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == TRUE, 1, 0)
check(df$host_has_profile_pic)
selected <- add('host_has_profile_pic')

# 26 host_identity_verified---------------
## impute 142 instances with commono class
df$host_identity_verified <- impute(df$host_identity_verified, value = TRUE)
df$host_identity_verified <- ifelse(df$host_identity_verified == TRUE, 1, 0)
check(df$host_identity_verified)
selected <- add('host_identity_verified')

# 27 host_is_superhost---------------
## IMPUTE 142 NA with common class 0
df$host_is_superhost <- as.character(df$host_is_superhost)
df$host_is_superhost <- ifelse(df$host_is_superhost ==TRUE,1,0)
df$host_is_superhost<- impute(df$host_is_superhost, value = 0)
check(df$host_is_superhost)
selected <- add('host_is_superhost')


# 28 host_listings_count---------------
### impute na with mean(round)
df$host_listings_count <- impute(df$host_listings_count, roundup = TRUE)
check(df$host_listings_count)
selected <- add('host_listings_count')


# 32 host_response_rate (15793 NA)---------------
## remove % sign, and impute rounded mean, and divide by 100. 
df$host_response_rate <- as.numeric(gsub("\\%", '', df$host_response_rate)) # remove % sign
df$host_response_rate <- impute(df$host_response_rate, roundup = TRUE)
df$host_response_rate <- df$host_response_rate/100
check(df$host_response_rate)
selected <- add('host_response_rate')


# 33 host_response_time---------------
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[df$host_response_time == 'within an hour'] <- 4
df$host_response_time[df$host_response_time =='within a few hours'] <- 3
df$host_response_time[df$host_response_time =='within a day'] <- 2
df$host_response_time[df$host_response_time == 'a few days or more'] <- 1
df$host_response_time <- as.numeric(df$host_response_time)
### impute na with the mode
#### use my Mode function

md <- Mode(df$host_response_time)
df$host_response_time[is.na(df$host_response_time)==TRUE] <- md
check(df$host_response_time)
selected <- add('host_response_time')

# 34 host_since---------------
## New var 71 experience
## Transform the "host_since" column to time difference
## remove 142 NA in df$experience, this also might affect other columns with 142 NAs
df$host_since <- as.Date(df$host_since, origin="1960-10-01")
df$experience <- difftime(Sys.Date(), df$host_since)
df$experience <- as.integer(df$experience)
check(df$experience)
df$experience <- scale(df$experience)
df<- remove(df$experience)  
nrow(df$experience)
selected <- add('experience')



# 37 house_rules---------------
## no party no pets, no smoking


# 38 instant_bookable---------------
## convert to numeric 1,0 
df$instant_bookable <- ifelse(df$instant_bookable == TRUE, 1, 0)
df$instant_bookable <- impute(df$instant_bookable,value = 1)
check(df$instant_bookable)
selected <- add('instant_bookable')


# 40 is_business_travel_ready---------------
## convert to 1, 0
## impute 44529 NAs as 0 (not business travel ready)
df$is_business_travel_ready<-ifelse(df$is_business_travel_ready==TRUE,1,0)
df$is_business_travel_ready <- impute(df$is_business_travel_ready, value = 0)
check(df$is_business_travel_ready)
selected <- add('is_business_travel_ready')


# 41 is_location_exact---------------
## convert to 1, 0
df$is_location_exact<- ifelse(df$is_location_exact== TRUE, 1, 0)
check(df$is_location_exact)
df$is_location_exact <- impute(df$is_location_exact,value = 1)
selected <- add('is_location_exact')


# 47 maximum_nights---------------
## create new var73 long_stay: allow long-stay(more than 7 days) or not
df['long_stay'] <- ifelse(df$maximum_nights >7, 1, 0)
df$long_stay <- impute(df$long_stay,value = 0)
check(df$long_stay)
selected <- add('long_stay')


# 48 minimum_nights-----------------
selected <- add('minimum_nights')
Mode(df$minimum_nights)
check(df$minimum_nights)
df$minimum_nights <- impute(df$minimum_nights,value = 1)

# 54 price---------------
## remove dollar sign and impute 573 NAs with mean
df$price = as.numeric(substring(as.character(df$price),2))
df$price <- impute(df$price)
check(df$price)
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





# this code block is an alternative way to onehot encode all of the property types
if(FALSE){
  library(mltools)
  library(data.table)
  newdata <- one_hot(as.data.table(as.factor(df$property_type)))
}





# 56 require_guest_phone_verification------------
df$require_guest_phone_verification <- ifelse(df$require_guest_phone_verification==TRUE, 1, 0)
check(df$require_guest_phone_verification)
df$require_guest_phone_verification <- impute(df$require_guest_phone_verification,value = 1)
selected <- add('require_guest_phone_verification')

# 57 require_guest_profile_picture----------
df$require_guest_profile_picture <- ifelse(df$require_guest_profile_picture==TRUE, 1, 0)
df$require_guest_profile_picture <- impute(df$require_guest_profile_picture,value = 1)
check(df$require_guest_profile_picture)
selected <- add('require_guest_profile_picture')
nrow(df)
# 58 requires_license---------------
df$requires_license <- ifelse(df$requires_license==TRUE, 1, 0)
check(df$requires_license)
df$requires_license <- impute(df$requires_license,value = 1)
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

selected <- add('roomEntire_home_apt')
selected <- add('roomPrivate_room')
selected <- add('roomShared_room')




# 60 security_deposit---------------
# remove $, convert numeric values into yes or no(1, 0), impute NA with mode: 1
df$security_deposit <- as.character(df$security_deposit)
df$security_deposit <- as.numeric(substring(as.character(df$security_deposit),2))
df$security_deposit <- ifelse(df$security_deposit>0,1,0)
df$security_deposit <- impute(df$security_deposit, value = 1) 
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

#----------------------------------------------------------------------------
#24-hour-check-in
df$CheckIn24 <- ifelse(str_detect((df$amenities),"24-hour check-in")==TRUE,1,0)
df$CheckIn24 <- impute(df$CheckIn24, roundup =  TRUE) # impute 2NAs with mode from test data


df$air_conditioning<- ifelse(str_detect((df$amenities),"Air conditioning|Central air conditioning")==TRUE,1,0)
df$air_conditioning <- impute(df$air_conditioning, roundup =  TRUE) # impute 2NAs with mode from test data


df$high_end_electronics <- ifelse(str_detect((df$amenities),"Amazon Echo|Apple TV|Game console|Netflix|Projector and screen|Smart TV")==TRUE,1,0)
df$high_end_electronics <- impute(df$high_end_electronics, roundup =  TRUE) # impute 2NAs with mode from test data



df$bbq <- ifelse(str_detect((df$amenities),"BBQ grill|Fire pit|Propane barbeque")==TRUE,1,0)
df$bbq <- impute(df$bbq, roundup =  TRUE) # impute 2NAs with mode from test data


df$balcony <- ifelse(str_detect((df$amenities),"Balcony|Patio")==TRUE,1,0)
df$balcony <- impute(df$balcony, roundup =  TRUE) # impute 2NAs with mode from test data


df$nature_and_views <- ifelse(str_detect((df$amenities),"Beach view|Beachfront|Lake access|Mountain view|Ski-in/Ski-out|Waterfront")==TRUE,1,0)
df$nature_and_views <- impute(df$nature_and_views, roundup =  TRUE) # impute 2NAs with mode from test data


df$bed_linen <- ifelse(str_detect((df$amenities),"Bed linens")==TRUE,1,0)
df$bed_linen <- impute(df$bed_linen, roundup =  TRUE) # impute 2NAs with mode from test data


df$breakfast <- ifelse(str_detect((df$amenities),"Breakfast")==TRUE,1,0)
df$breakfast <- impute(df$breakfast, roundup =  TRUE) # impute 2NAs with mode from test data


df$tv <- ifelse(str_detect((df$amenities),"TV")==TRUE,1,0)
df$tv <- impute(df$tv, roundup =  TRUE) # impute 2NAs with mode from test data


df$coffee_machine <- ifelse(str_detect((df$amenities),"Coffee maker|Espresso machine")==TRUE,1,0)
df$coffee_machine <- impute(df$coffee_machine, roundup =  TRUE) # impute 2NAs with mode from test data


df$kitchen <- ifelse(str_detect((df$amenities),"Cooking basics|kitchen|dishes|cooking")==TRUE,1,0)
df$kitchen <- impute(df$kitchen, roundup =  TRUE) # impute 2NAs with mode from test data


df$white_goods <- ifelse(str_detect((df$amenities),"Dishwasher|Dryer|Washer")==TRUE,1,0)
df$white_goods <- impute(df$white_goods, roundup =  TRUE) # impute 2NAs with mode from test data


df$elevator <- ifelse(str_detect((df$amenities),"Elevator")==TRUE,1,0)
df$elevator <- impute(df$elevator, roundup =  TRUE) # impute 2NAs with mode from test data


df$gym <- ifelse(str_detect((df$amenities),"Exercise equipment|Gym|gym")==TRUE,1,0)
df$gym <- impute(df$gym, roundup =  TRUE) # impute 2NAs with mode from test data


df$child_friendly <- ifelse(str_detect((df$amenities),"Family/kid friendly|Children|children")==TRUE,1,0)
df$child_friendly <- impute(df$child_friendly, roundup =  TRUE) # impute 2NAs with mode from test data

df$parking <- ifelse(str_detect((df$amenities),"parking")==TRUE,1,0)
df$parking <- impute(df$parking, roundup =  TRUE) # impute 2NAs with mode from test data

df$outdoor_space <- ifelse(str_detect((df$amenities),"Garden|Outdoor|Sun loungers|Terrace")==TRUE,1,0)
df$outdoor_space <- impute(df$outdoor_space, roundup =  TRUE) # impute 2NAs with mode from test data

df$host_greeting <- ifelse(str_detect((df$amenities),"Host greets you")==TRUE,1,0)
df$host_greeting <- impute(df$host_greeting, roundup =  TRUE) # impute 2NAs with mode from test data

df$hottub_sauna_pool <- ifelse(str_detect((df$amenities),"Hot tub|Jetted tub|hot tub|Sauna|Pool|pool")==TRUE,1,0)
df$hottub_sauna_pool <- impute(df$hottub_sauna_pool, roundup =  TRUE) # impute 2NAs with mode from test data

df$internet <- ifelse(str_detect((df$amenities),"Internet|Pocket wifi|Wifi|Wireless")==TRUE,1,0)
df$internet <- impute(df$internet, roundup =  TRUE) # impute 2NAs with mode from test data

df$long_term_Stay_allowed <- ifelse(str_detect((df$amenities),"Long term stays allowed")==TRUE,1,0)
df$long_term_Stay_allowed <- impute(df$long_term_Stay_allowed, roundup =  TRUE) # impute 2NAs with mode from test data

df$pets_allowed <- ifelse(str_detect((df$amenities),"Pets|pet|Cat(s)|Dog(s)")==TRUE,1,0)
df$pets_allowed <- impute(df$pets_allowed, roundup =  TRUE) # impute 2NAs with mode from test data

df$private_entrance <- ifelse(str_detect((df$amenities),"Private entrance")==TRUE,1,0)
df$private_entrance <- impute(df$private_entrance, roundup =  TRUE) # impute 2NAs with mode from test data

df$secure <- ifelse(str_detect((df$amenities),"Safe|Security system|Lock")==TRUE,1,0)
df$secure <- impute(df$secure, roundup =  TRUE) # impute 2NAs with mode from test data

df$self_check_in <- ifelse(str_detect((df$amenities),"Self check-in")==TRUE,1,0)
df$self_check_in <- impute(df$self_check_in, roundup =  TRUE) # impute 2NAs with mode from test data

df$smoking_allowed <- ifelse(str_detect((df$amenities),"Smoking allowed")==TRUE,1,0)
df$smoking_allowed <- impute(df$smoking_allowed, roundup =  TRUE) # impute 2NAs with mode from test data
check(df$smoking_allowed)
df$accessible <- ifelse(str_detect((df$amenities),"Step-free access|Wheelchair|Accessible")==TRUE,1,0)
df$accessible <- impute(df$accessible, roundup =  TRUE) # impute 2NAs with mode from test data

df$event_suitable <- ifelse(str_detect((df$amenities),"Suitable for events")==TRUE,1,0)
df$event_suitable <- impute(df$event_suitable, roundup =  TRUE) # impute 2NAs with mode from test data


selected <- add('CheckIn24')
selected <- add('air_conditioning')
selected <- add('high_end_electronics')
selected <- add('bbq')
selected <- add('balcony')
selected <- add('nature_and_views')
selected <- add('bed_linen')
selected <- add('breakfast')
selected <- add('tv')
selected <- add('coffee_machine')
selected <- add('kitchen')
selected <- add('white_goods')
selected <- add('elevator')
selected <- add('gym')
selected <- add('child_friendly')
selected <- add('parking')
selected <- add('outdoor_space')
selected <- add('host_greeting')
selected <- add('hottub_sauna_pool')
selected <- add('internet')
selected <- add('long_term_Stay_allowed')
selected <- add('pets_allowed')
selected <- add('private_entrance')
selected <- add('secure')
selected <- add('self_check_in')
selected <- add('smoking_allowed')
selected <- add('accessible')
selected <- add('event_suitable')



# house_rules
df$rules <- ifelse(is.na(df$house_rules)== FALSE, 1, 0)
check(df$rules)
selected <- add('rules')

# interaction
df$interaction <- ifelse(is.na(df$interaction)== FALSE, 1, 0)
check(df$interaction)
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

#Adding market_popularity
market_list <- NULL
df$market[is.na(df$market)] <- 'Others'
for (i in unique(df$market)){
  x <- subset(df,df$market==i)
  market_list <- c(market_list,nrow(x))
}
market_list <- data.frame(unique(df$market),market_list)


names(market_list)[1:2] <- c('market_name','num_listings')

df <- merge(df, market_list, by.x='market', by.y='market_name')
nrow(df)
selected <- add("num_listings")

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

selected <- add("city_centrality")
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
length(selected) # 106

nrow(df)
check(df$experience)
# Export as CSV file--------------------------------
export_train <- df[selected] # use the selected features 
colSums(is.na(export_train))
names(export_train)
# sapply(export_train, class)
# convert data type
export_train[,65:96] <- lapply(export_train[,65:96], factor) #"monthly_suitable"~ "interaction"  to factor
#export_train[,58:76] <- lapply(export_train[,58:76], factor) -> for train_cleaned9 because their positions have changed in names(df)
export_train[c('monthly_discount', 'weekly_discount')] <- sapply(export_train[c('monthly_discount', 'weekly_discount')], as.numeric)

nrow(export_train)
print(selected)
# which(is.na(export_train), arr.ind=TRUE)
write.csv(export_train, file="train_cleaned18.csv", row.names = FALSE) #Write dataframe as CSV















