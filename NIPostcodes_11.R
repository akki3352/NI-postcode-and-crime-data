# Loadind data
NIPostcodes <- read.csv("Nipostcodes.csv", header = FALSE)

#Count the total number of rows in NIPostcodes dataset.
nrow(NIPostcodes)

# Show the structure of data frame
str(NIPostcodes)

# Displaying first 10 rows of data frame
head(NIPostcodes, 10)


# Add a suitable title for each attribute of the data.

colnames(NIPostcodes) <- c("Organization_Name", "Sub_Building_Name", 
                           "Building_Name", "Number", "Primary_Thorfare", "Alt_Thorefare",
                           "Secondary_Thorfare", "Locality", "Townland", "Town",
                           "County", "PostCode", "x_coordinate", "y_coordinate",
                           "Primary_key")
names(NIPostcodes)

#Move the primary key identifier to the start of the NIPostcodes dataset.
NIPostcodes <- NIPostcodes[c(15, 1:14)]
head(NIPostcodes, 1)

# Counting total number missing values for each column in NIPostcode data frame
colSums(is.array(NIPostcodes) | NIPostcodes == "", na.rm = TRUE)


# Counting mean missing values for each column in  NIPostcode data frame
colMeans(is.array(NIPostcodes) | NIPostcodes == "", na.rm = TRUE)

#  Replace missing entries with NA
NIPostcodes[NIPostcodes==""] <- NA

unique(NIPostcodes[, "County"])

# Modify the county attribute to be categorising by Region
# categorising all county by Region attribute

NIPostcodes$Region[NIPostcodes$County == "ANTRIM" | NIPostcodes$County == "ARMAGH" | 
                     NIPostcodes$County == "DOWN" | NIPostcodes$County == "FERMANAGH" |
                     NIPostcodes$County == "LONDONDERRY"  |
                     NIPostcodes$County == "TYRONE"] <- "Ulster" 

# Recode Region so that is ordinal and factored with the
# Region:  Ulster
# We'll srore the ordinal factored data in variable 'Region'
Region <- factor(NIPostcodes$Region, order = TRUE, 
                 levels = c("Ulster"))
# Replace NIPostcodes's Region attribute with newly ordinal foctored data
NIPostcodes$Region <- Region
head(NIPostcodes, 1)

# Create new data set Limavady_data that contain only information about
# Locality, Townland and Town
Limavady_data <- NIPostcodes[c(9, 10, 11)]
head(Limavady_data, 10)
names(Limavady_data)


# Filter Limavady_data dataset which containing the name "LIMAVADY" in all 3 columns. 
Limavady_data <- Limavady_data %>% filter(grepl("LIMAVADY", Limavady_data$Locality), 
                                          grepl("LIMAVADY", Limavady_data$Townland),
                                          grepl("LIMAVADY", Limavady_data$Town))
head(Limavady_data, 10)

# Store Limavady_data information in an external csv file called Limavady.
write.csv(Limavady_data, file = "Limavady.csv")

# Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv")
zip(zipfile = "CleanNIPostcodeData.csv", "Limavady.csv", "Nipostcodes.csv")


