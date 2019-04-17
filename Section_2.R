install.packages("RCurl")
library(RCurl)


# unzip the NIcrimeData.zip file using unzip ()
download.file("https://github.com/akki3352/NI-postcode-and-crime-data/raw/master/NI%20Crime%20Data.zip", destfile = "NICrimeData.zip")
unzip("NICrimeData.zip")

# You can get the crime file list using list.file ()
all_the_crime_files <- list.files("NI Crime Data/", recursive = TRUE, full = TRUE)

#  read all crime files using lapply()
all_the_crime_data <- lapply(all_the_crime_files, read.csv, header = TRUE)

# and then mergeing all crime files into a AllNICrimeData data frame using rbind

AllNICrimeData <- do.call("rbind", all_the_crime_data)

# Count and show the number of rows in the AllNICrimeData dataset.

nrow(AllNICrimeData)

# Show all columns name in data frame
colnames(AllNICrimeData)


# Remove following attribute from AllNICrimeData dataset
#  CrimeID, Reported by, Falls within, LSOA code, LSOA name,last outcome and context

AllNICrimeData <- subset(AllNICrimeData, select = c("Month", "Longitude", 
                                                    "Latitude", "Location",
                                                    "Crime.type"))
unique(AllNICrimeData[, "Crime.type"])


# Modify the crime.type attribute to be categorising by Category.crime
# categorising all crime type by Category.crime attribute

AllNICrimeData$Category.crime[AllNICrimeData$Crime.type == "Criminal damage and arson" | 
                                AllNICrimeData$Crime.type == "Shoplifting" | 
                                AllNICrimeData$Crime.type == "Burglary" | 
                                AllNICrimeData$Crime.type == "Robbery" |
                                AllNICrimeData$Crime.type == "Other theft"  | 
                                AllNICrimeData$Crime.type == "Bicycle theft" |
                                AllNICrimeData$Crime.type == "Vehicle crime" | 
                                AllNICrimeData$Crime.type == "Theft from the person"] <- "Crimes against property"
AllNICrimeData$Category.crime[AllNICrimeData$Crime.type == "Drugs"] <- "Victimless crimes"
AllNICrimeData$Category.crime[AllNICrimeData$Crime.type == "Possession of weapons" | 
                                AllNICrimeData$Crime.type == "public order" | 
                                AllNICrimeData$Crime.type == "Anti-social behaviour"] <- "Crime against humanity"
AllNICrimeData$Category.crime[AllNICrimeData$Crime.type == "Violence and sexual offences Other crime" |
                                AllNICrimeData$Crime.type == "Violence and sexual offences"] <- "Victimless crimes"

# Recode Category.crime so that is ordinal and factored with the
# Category.crime:  Crimes against property, Victimless crimes, Crime against humanity
# We'll srore the ordinal factored data in variable 'Category.crime'
Category.crime <- factor(AllNICrimeData$Category.crime, order = TRUE, 
                         levels = c("Crimes against property", "Victimless crimes", "Crime against humanity" ))

# Replace AllNICrimeData's Category.crime attribute with newly ordinal foctored data
AllNICrimeData$Category.crime <- Category.crime
head(AllNICrimeData, 1)

# Check unique values in Location column
unique(AllNICrimeData[, "Location"])

# Check any null(blank) values in Location columns 
colSums(is.array(AllNICrimeData) | AllNICrimeData == "", na.rm = TRUE)

install.packages("tidyverse")
library(tidyverse)
# Remove "Onn oe near" words from Location column using str_remove()
AllNICrimeData$Location <- str_remove(AllNICrimeData$Location, "On or near")

# Replace blank values with NA in Location column
AllNICrimeData$Location[AllNICrimeData$Location==" "] <- NA

head(AllNICrimeData, 10)
# Choosing 1000 random samples of crime data using sample_n ()
# where the Location attribute should not contain NA in AllNICrimeData data set
# Save result in new random_crime_sample data set
#install.packages("dplyr")
#library(dplyr)
random_crime_sample <- unique(sample_n(AllNICrimeData[complete.cases(AllNICrimeData), ],  1000))
head(random_crime_sample, 5)
str(random_crime_sample)

# Convert Location attribute into uppercase
random_crime_sample[, "Location"] <- toupper(random_crime_sample[, "Location"])
# Delete all whitespaces from location attribute in random_crime_sample data set
random_crime_sample$Location <- trimws(random_crime_sample$Location)

#Load CleanNIPostcodeData using getURL() function
set_config( config( ssl.verifypeer = 0L ) )
x <- getURL("https://media.githubusercontent.com/media/akki3352/NI-postcode-and-crime-data/master/CleanNIPostcodeData.csv")
CleanNIPostcodeData <- read.csv(text = x)
head(CleanNIPostcodeData, 5)
str(CleanNIPostcodeData)
CleanNIPostcodeData$Primary.THoroughfare <- trimws(CleanNIPostcodeData$Primary.THoroughfare)

# Create random_crime_sample function that 
# takes input each location attribute and
# find suitable postcode from CleanNIPostcodeData
 
find_a_postcode <- function(x){
  
  dat <- CleanNIPostcodeData[CleanNIPostcodeData$Primary.THoroughfare == x, "Postcode"]
  dat <- na.omit(dat)
  dd <- unique(dat)
  dd[which.max(tabulate(match(dat,dd)))]
}

find_a_postcode("MEADOWVALE")

# Append the data out from find_a_postcode function to the random_crime_sample data set as a Postcode attribute
random_crime_sample$Postcode <- Vectorize(find_a_postcode)(random_crime_sample$Location)

head(random_crime_sample, 5)

# Show the modified structure of random_crime_sample data set
str(random_crime_sample)

# Save the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, file = "random_crime_sample.csv")

#Extract Category.type attribute from random_crime_sample data set
# and save into a new data frame called updated_random_sample

updated_random_sample <- random_crime_sample[,-6, drop = FALSE]
head(updated_random_sample, 5)



updated_random_sample$Postcode <- trimws(updated_random_sample$Postcode)



install.packages('gtools')

library(gtools)

# Sort the chart_data data frame by postcode where the postcode contains "BT1"
chart_data <- updated_random_sample
chart_data$Postcode <-  mixedsort(updated_random_sample$Postcode)
head(chart_data)

# Sort the chart_data data frame by crime.type
chart_data$Crime.type <- sort(chart_data$Crime.type)
head(chart_data)
str(chart_data)

# here we gets the colors for unique crime type
palette <- RColorBrewer::brewer.pal(length(unique(chart_data$Crime.type)),name = 'Set1')

# Lets create a frequency table for types of Crime.
counts <- table(chart_data$Crime.type)
print(counts)

# Create a bar plot of the crime type from the chart_data data frame. 
barplot(counts,
        xlab='Offence Type',ylab='Crime Rate',
        main='Crime Statistics',
        col=counts,col.main='Blue')


 