# Loadind data
NIPostcodes <- read.csv("Nipostcodes.csv")

typeof(NIPostcodes)
# Show the structure of data frame
str(NIPostcodes)

# Displaying first 10 rows of data frame
head(NIPostcodes, 10)

# Counting mean missing values of NIPostcode data 
total_value <- sum(is.array(NIPostcodes) | NIPostcodes == "")
total_value

# Counting total number missing values of NIPostcode data
mean_value <- mean(is.array(NIPostcodes) | NIPostcodes == "")
mean_value


NIPostcodes[, c(3, 7, 8)] <- NULL

colnames(NIPostcodes) <- c("Business_Name", "Flat/Apartment_Num", 
                           "House_Num", "Locality", 
                           "Gaelic", "Townland", "Town", "County", 
                           "ZipCode", "Latitude", "Longitude", "Sr_Num")
names(NIPostcodes)
NIPostcodes <- NIPostcodes[c(12, 11, 10, 1:9)]
head(NIPostcodes, 1)
names(NIPostcodes)

Limavady_data <- NIPostcodes[c(7, 9, 10)]
head(Limavady_data, 5)


 Limavady_data$Locality <- dplyr:: filter(Limavady_data, grepl('LIMAVADY', Limavady_data$Locality))
Limavady_data$Locality


Limavady_Locality <- Limavady_data$Locality[grep("LIMAVADY", Limavady_data$Locality)]
Limavady_Locality
Limavady_Townland <- Limavady_data$Townland[grep("LIMAVADY", Limavady_data$Townland)]
head(Limavady_Townland, 3)
Limavady_Town <- Limavady_data$Town[grep("LIMAVADY", Limavady_data$Town)]
head(Limavady_Town, 3)
 




