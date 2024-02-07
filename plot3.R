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

# Generate Plot 3
plotEnergySubMetering <- function(data) {
  library(ggplot2)
  ggplot(data, aes(x = DateTime)) +
    geom_line(aes(y = Sub_metering_1, colour = "Sub_metering_1")) +
    geom_line(aes(y = Sub_metering_2, colour = "Sub_metering_2")) +
    geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3")) +
    scale_colour_manual(values = c("Sub_metering_1" = "black", "Sub_metering_2" = "red", "Sub_metering_3" = "blue"),
                        name = "",
                        labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
    labs(x = NULL, y = "Energy sub metering") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.ticks = element_line(color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.line = element_line(color = "black"),
          legend.position = c(1, 1),
          legend.justification = c("right", "top"),
          legend.background = element_rect(colour = "black", fill = NA, size = 1),
          legend.direction = "vertical",
          legend.box.margin = margin(0, 0, 0, 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.just = "right",
          legend.text = element_text(hjust = 0.5),
          legend.key.width = unit(2.5, "lines"),
          legend.key.height = unit(0.85, "lines"),
          legend.spacing.x = unit(0.5, "cm"),
          legend.spacing.y = unit(0.1, "cm"))
}

#Display plot in R Studio plots plane
plotEnergySubMetering(filtered_df)

# Specify filepath for Plot 3
file_path <- ("C:\\Users\\nhart\\OneDrive\\Desktop\\Johns Hopkins\\Exploratory Data Analysis\\Week 1 Assignment")
#Specify Plot size
png("plot3.png", width = 480, height = 480)
#Generate Plot 3 for saving to PNG
plotEnergySubMetering(filtered_df)
#Close device
dev.off()


















