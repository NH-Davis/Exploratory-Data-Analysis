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
# Generate Plot 4
library(ggplot2)
library(gridExtra)

# Chart 1: Global_active_power over time
chart1 <- ggplot(filtered_df, aes(x = DateTime, y = Global_active_power)) +
  geom_line() +
  scale_x_datetime(labels = scales::date_format("%a"), date_breaks = "1 day") +
  labs(x = NULL, y = "Global Active Power") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1))  # Horizontal y-axis labels

# Chart 2: Voltage over time
chart2 <- ggplot(filtered_df, aes(x = DateTime, y = Voltage)) +
  geom_line() +
  scale_x_datetime(labels = scales::date_format("%a"), date_breaks = "1 day") +
  labs(x = NULL, y = "Voltage") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1))  # Horizontal y-axis labels

# Chart 3: Placeholder for plotEnergySubMetering 
chart3 <- plotEnergySubMetering(filtered_df) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Adjust this as per the actual function

# Chart 4: Global_reactive_power over time
chart4 <- ggplot(filtered_df, aes(x = DateTime, y = Global_reactive_power)) +
  geom_line() +
  scale_x_datetime(labels = scales::date_format("%a"), date_breaks = "1 day") +
  labs(x = NULL, y = "Global_reactive_power") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1))  # Horizontal y-axis labels
# Arrange the four charts into a single plot
grid.arrange(chart1, chart2, chart3, chart4, ncol = 2)

# Specify filepath for Plot 4
file_path <- ("C:\\Users\\nhart\\OneDrive\\Desktop\\Johns Hopkins\\Exploratory Data Analysis\\Week 1 Assignment")
#Specify Plot size
png("plot4.png", width = 480, height = 480)
#Generate Plot 4 for saving to PNG
grid.arrange(chart1, chart2, chart3, chart4, ncol = 2)
#Close device
dev.off()



