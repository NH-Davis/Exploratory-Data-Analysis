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
# Optional: Check for any NA values that might have been introduced during conversion
sum(is.na(household_df[columns_to_convert]))

#filter data frame for 2007-02-01 and 2007-02-02 and create new dataframe
library(dplyr)
filtered_df <- household_df %>%
  filter(DateTime >= as.POSIXct("2007-02-01 00:00:00") & DateTime <= as.POSIXct("2007-02-02 23:59:59"))

# Generate Plot 2
plotGlobalActivePower <- function(data) {
  ggplot(filtered_df, aes(x = DateTime, y = Global_active_power)) +
    geom_line() +  # Draw lines connecting each data point
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +  # Format x-axis labels as abbreviated weekdays
    labs(x = NULL, y = "Global Active Power (kilowatts)") +  # Remove x-axis title, keep y-axis title
    theme_minimal() +  # Start with a minimal theme as a base
    theme(panel.background = element_rect(fill = "white", colour = "black"), # Set background to white and add a black border
          panel.grid.major = element_blank(),  # Remove major gridlines
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          panel.border = element_rect(colour = "black", fill=NA, size=1),  # Add a black line around the chart area
          axis.ticks = element_line(color = "black"),  # Ensure tick marks are added and black
          axis.text.x = element_text(angle = 0, hjust = 0.5),  # Display x-axis labels horizontally
          axis.line = element_line(color = "black"))  # Add black lines on axes
}
#Display plot in R Studio plots plane
plotGlobalActivePower (filtered_df)

# Specify filepath for Plot 2
file_path <- ("C:\\Users\\nhart\\OneDrive\\Desktop\\Johns Hopkins\\Exploratory Data Analysis\\Week 1 Assignment")

#Specify Plot size
png("plot2.png", width = 480, height = 480)

#Generate Plot 2 for saving to PNG
plotGlobalActivePower(filtered_df)

#Close device
dev.off()
