# read in household data file and collate data into separate columns
household_df <- read.table("C:\\Users\\nhart\\OneDrive\\Desktop\\Johns Hopkins\\Exploratory Data Analysis\\Week 1 Assignment\\household_power_consumption.txt", sep =";", header =TRUE)

# run summary functions to learn more about shape and format of data
str(household_df)
head(household_df)

# Combine "Date" and "Time" into a single string
datetime_string <- paste(household_df$Date, household_df$Time)
# Convert the combined string to POSIXct date-time object with the correct format
household_df$DateTime <- as.POSIXct(datetime_string, format="%d/%m/%Y %H:%M:%S")

# Check the first few entries of the DateTime column to ensure conversion was successful
head(household_df$DateTime)

# convert content of remaining columns into numeric format
# List of column names to convert to numeric
columns_to_convert <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
# Convert each specified column to numeric, ensuring conversion to character first
for(column_name in columns_to_convert) {
  # Convert to character first to ensure proper numeric conversion
  household_df[[column_name]] <- as.numeric(as.character(household_df[[column_name]]))
}

#filter data frame for 2007-02-01 and 2007-02-02 and create new dataframe
library(dplyr)
filtered_df <- household_df %>%
  filter(DateTime >= as.POSIXct("2007-02-01 00:00:00") & DateTime <= as.POSIXct("2007-02-02 23:59:59"))

#Generate Plot 1
plotHistGlobalActivePower <- function(data) {
  hist(filtered_df$Global_active_power,
       col = "red", # Set histogram bar color to red
       main = "Global Active Power", # Main title
       xlab = "Global Active Power (kilowatts)", # X-axis label
       ylab = "Frequency",  # Y-axis label
       breaks = "Sturges") # Default method for calculating the number of bins
}

#Display plot in R Studio plots plane
plotHistGlobalActivePower(filtered_df)

# Specify filepath for Plot 1
file_path <- ("C:\\Users\\nhart\\OneDrive\\Desktop\\Johns Hopkins\\Exploratory Data Analysis\\Week 1 Assignment")

#Specify Plot size
png("plot1.png", width = 480, height = 480)

#Generate Plot 1 for saving to PNG
plotHistGlobalActivePower(filtered_df)
#Close device
dev.off()
