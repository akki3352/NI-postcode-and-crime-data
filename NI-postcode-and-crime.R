# Loadind data
NIPostcodes <- read.csv("Nipostcodes.csv")

# Show the structure of data frame
str(NIPostcodes)

# Displaying first 10 rows of data frame
head(NIPostcodes, 10)
colnames(NIPostcodes) <- c("Business_Name", "Flat/Apartment_Num", "wq",
                           "House_Num", "Locality", 
                           "Gaelic", "wqe", "qwq","Townland", "Town", "County", 
                           "ZipCode", "Latitude", "Longitude", "Sr_Num")
names(NIPostcodes)

NIPostcodes[, c(3, 7, 8)] <- NULL
NIPostcodes[c(2, 4, 3)] 


# Counting mean missing values of NIPostcode data 
total_value <- sum(is.array(NIPostcodes) | NIPostcodes == "")
total_value

# Counting total number missing values of NIPostcode data
mean_value <- mean(is.array(NIPostcodes) | NIPostcodes == "")
mean_value

   

NIPostcodes_list <- NIPostcodes[c(12, 11, 10, 1:9)]
head(NIPostcodes_list, 1)
names(NIPostcodes_list)

Limavady_data <- NIPostcodes[c(5, 9, 10)]
head(Limavady_data, 5)

Limavady_Locality <- Limavady_data$Gaelic[grep("LIMAVADY", Limavady_data$Gaelic)]
Limavady_Locality
Limavady_Townland <- Limavady_data$ZipCode[grep("LIMAVADY", Limavady_data$ZipCode)]
head(Limavady_Townland, 3)
Limavady_Town <- Limavady_data$Latitude[grep("LIMAVADY", Limavady_data$Latitude)]
head(Limavady_Town, 3)


       