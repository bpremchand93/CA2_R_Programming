#Reading the NIpostcodes csv file from CA_2 Project workspace and header is set to false 
#since the csv file has no header, and stringasfactor is set to false to avoid the 
#default behaviour of R for converting char to factor 

#Section 1:

postcode_dataframe <- read.csv('NIPostcodes.csv', header = FALSE, stringsAsFactors = FALSE)

#Showing the structure of postcode dataframe
str(postcode_dataframe)

#Showing the total number of rows in postcode dataset
cat("Total Number of rows in the postcode dataframe is:", nrow(postcode_dataframe))

#Display the first 10 rows of dataframe
head(postcode_dataframe, n = 10)

#Creating column name for each dataframe columns
column_names <- c("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfare", "Alt Thorfare", 
                  "Secondary Thorfare", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", 
                  "y-coordinates", "Primary Key")

#Adding Column names in the postcode dataset dataframe
colnames(postcode_dataframe) <- column_names

#Adding 'NA' in the missing entries of dataframe column
postcode_dataframe[postcode_dataframe ==""] <- NA
head(postcode_dataframe)
#Sum and mean of missing values in postcode datasets columns
cat("Sum of missing values for each column in the postcode dataframe :", colSums(is.na(postcode_dataframe)), sep = "\t")
cat("Mean of missing values for each column in the postcode dataframe :", colMeans(is.na(postcode_dataframe)), sep = "\t")


#Categorising the county variable from character to factor
postcode_dataframe$County <- as.factor(postcode_dataframe$County)

#Primary key column has kept in the first column
postcode_dataframe <- subset(postcode_dataframe, select = c(15,1:14))

#Showing the summary of postcode dataframe
summary(postcode_dataframe)

#Idententify the locality, townland and town containing the name Limavady and store in a csv file
library(sqldf)
Limavady_data <- sqldf('SELECT * FROM postcode_dataframe WHERE Town like "%LIMAVADY%"  AND Locality like "%LIMAVADY%" AND Townland like "%LIMAVADY%"')
write.csv(Limavady_data, file = "Limavady.csv", row.names = FALSE)

#Postcode_dataframe has been stored in the csv file
write.csv(postcode_dataframe, file = "CleanNIPostcodeData.csv", row.names = FALSE)

#Section 2:
#Adding Crime datasets from multiple subdirectories to a single dataframe
root <- 'NI Crime Data/NI Crime Data'
dir(root, pattern='csv$', recursive = TRUE, full.names = TRUE) %>%
  # read files into data frames
  lapply(FUN = read.csv) %>%
  # bind all data frames into a single data frame
  rbind_all %>%
  # write into a single csv file
  write.csv("AllNICrimeData.csv",row.names = F)


#Read all the crime dataset and  stringasfactor is set to false to avoid the 
#default behaviour of R for converting character to factor 
AllNICrimeData <- read.csv("ALLNICrimeData.csv", stringsAsFactors = FALSE)

#Showing the count of crime dataset dataframe
cat("Count of the Crime dataframe :",nrow(AllNICrimeData))

#Modified the structure of AllNICrimeData dataframe and factorised the crime type
AllNICrimeData <- AllNICrimeData[, !(names(AllNICrimeData) %in% c("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", "Last.outcome.category", "Context"))] 
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)

#Shown the top 10 records and structure of the crime dataset
head(AllNICrimeData , 10)
str(AllNICrimeData)

#Removing 'On or near' string from a location column and once it is modified added 'NA' in a empty columns
AllNICrimeData$Location <- gsub("On or near", "", AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location==" "] <- NA
AllNICrimeData <- na.omit(AllNICrimeData)

#Random sample created from the crime dataset which consist of 1000 random records and modifed the location column to
#uppercase for comparing with postcode dataframe.
random_crime_sample <- AllNICrimeData[sample(1:nrow(AllNICrimeData), 1000, replace = FALSE),]
random_crime_sample$Location <- toupper(random_crime_sample$Location)

#trimws function used to remove the leading and trailing spaces
random_crime_sample$Location <- trimws(random_crime_sample$Location, which = "both")

head(random_crime_sample)
#Selected the postcode dataframe should not contain 'NA' in 'Primary Thorfare' and postcode column.
postcode_dataframe_distinct <- subset(postcode_dataframe, (!is.na(postcode_dataframe$`Primary Thorfare`) | !is.na(postcode_dataframe$Postcode)))
postcode_dataframe_distinct <- select(postcode_dataframe_distinct, `Primary Thorfare`, Postcode)

#Modified the 'Primary Thorfare' column name as Location
names(postcode_dataframe_distinct)[1] <- "Location"
head(postcode_dataframe_distinct)
#sqldf library used to extract the distinct location and postcode details and the count of postcode details ordered
#in descending order using postcode dataframe.(This helps to get the location and most occurance of postcode location.)
library(sqldf)
postcode_dataframe_distinct <- sqldf("SELECT Location, Postcode, COUNT(Postcode) AS Count_Postcode
                                      FROM postcode_dataframe_distinct 
                                      GROUP BY Location, Postcode
                                      ORDER BY Count_Postcode DESC;")


#The postcode distinct dataframe consist of duplicated location name and different postcodes.
#To avoid this, selected only the distinct location(with the help of !duplicated function),
#which consists of location column with most popular postcode for the particular location.
postcode_dataframe_distinct <- postcode_dataframe_distinct[!duplicated(postcode_dataframe_distinct$Location),]

#NA row has been removed from the postcode distinct dataframe.
postcode_dataframe_distinct <- na.omit(postcode_dataframe_distinct)

#Function created to get the postcode for the crime location from the postcode distinct dataframe that is created in the previous step.
find_a_postcode <- function(df1, df2, loc){
  random_crime_sample <- as.data.frame(fn$sqldf('SELECT a.*, b.Postcode FROM "$df1" a LEFT JOIN "$df2" b where a.Location = b."$loc" '))  
  return(random_crime_sample)
}

#Showing the structure of the random_crime_sample dataframe and showing the total number of rows.
str(random_crime_sample)
nrow(random_crime_sample)

#Calling find_a_postcode function using input dataframe and location field name.
random_crime_sample <- find_a_postcode("random_crime_sample", "postcode_dataframe_distinct", "Location")

#'random_crime_sample' dataframe has been saved into 'random_crime_sample.csv'
write.csv(random_crime_sample, "random_crime_sample.csv", row.names = F)


# Extract the random_crime_sample dataframe to update_crime_sample dataframe and filtered the
# postcode contains 'BT1' and then sorted the postcode and crime type.
update_crime_sample <- random_crime_sample

#dplyr library is used to filter the 'BT1' postcode from update_crime_sample dataframe.
library(dplyr)
chart_data <- filter(update_crime_sample, grepl('BT1',update_crime_sample$Postcode))

#Order is used to sort the postcode and crime type in a ascending order by default.
chart_data <- chart_data[order(chart_data$Postcode, chart_data$Crime.type),]


#Showing the summary of the chart_data
summary(chart_data)


#colorRamppalette function used to fill colour for different crime types.
#To plot the barplot frequency of crime_type has created using table function
#barplot has been plotted using crime_type.freq and different crime types.
#par function used to set the margin of the barplot
#Different barplot attributes like main, xlab, ylab used to lable the suitable names

pal <- colorRampPalette(c( "red", "yellow", "blue", "green"))
crime_type.freq <- table(chart_data$Crime.type)


View(crime_type.freq)
par(mar=c(13.5,5, 3, 3))
barplot(crime_type.freq[order(crime_type.freq,decreasing = T)], las=2,
        main= "Number of crime for each crime type", ylab = "Count of crime category", xlab = "Crime Categories", cex.lab=1,
        ylim = c(0,100), border = NA,
        cex.names=0.55,col=pal(14))


