install.packages("RCurl")
library(RCurl)


# Loadind data
download.file("https://raw.githubusercontent.com/akki3352/NI-postcode-and-crime-data/master/NIPostcodes.csv", destfile = "NIPOstcodes.csv")
NIPostcodes <- read.csv("NIPOstcodes.csv")


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
                           "County", "PostCode", "x_coordinate", "y_coordinate", "Primary_key")
names(NIPostcodes)
head(NIPostcodes, 3)
str(NIPostcodes)

#Move the primary key identifier to the start of the NIPostcodes dataset.
# and population_rank attribute near to County attribute.
NIPostcodes <- NIPostcodes[c(15, 1:14)]
head(NIPostcodes, 3)
str(NIPostcodes)

# Counting total number missing values for each column in NIPostcode data frame
colSums(is.array(NIPostcodes) | NIPostcodes == "", na.rm = TRUE)


# Counting mean missing values for each column in  NIPostcode data frame
colMeans(is.array(NIPostcodes) | NIPostcodes == "", na.rm = TRUE)

#  Replace missing entries with NA
NIPostcodes[NIPostcodes==""] <- NA
head(NIPostcodes, 3)
str(NIPostcodes)

unique(NIPostcodes[, "County"])

# Modify the county attribute to be categorising by Region
# categorising all county by Region attribute

NIPostcodes$Population_Rank[NIPostcodes$County == "ANTRIM"] <- "2"
NIPostcodes$Population_Rank[NIPostcodes$County == "ARMAGH"] <- "11" 
NIPostcodes$Population_Rank[NIPostcodes$County == "DOWN"] <- "4"
NIPostcodes$Population_Rank[NIPostcodes$County == "FERMANAGH"] <- "28"
NIPostcodes$Population_Rank[NIPostcodes$County == "LONDONDERRY"] <- "6"
NIPostcodes$Population_Rank[NIPostcodes$County == "TYRONE"] <- "10" 

# Recode Region so that is ordinal and factored with the
# Region:  Ulster
# We'll srore the ordinal factored data in variable 'Region'
Population_Rank <- factor(NIPostcodes$Population_Rank, order = TRUE, 
                          levels = c("2", "11", "4", "28", "6", "10"))
# Replace NIPostcodes's Region attribute with newly ordinal foctored data
NIPostcodes$Population_Rank <- Population_Rank
head(NIPostcodes, 5)
str(NIPostcodes)

# Create new data set Limavady_data that contain only information about
# Locality, Townland and Town
Limavady_data <- NIPostcodes[c(9, 10, 11)]
head(Limavady_data, 10)
names(Limavady_data)
str(Limavady_data)

install.packages("dplyr")
library(dplyr)

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


