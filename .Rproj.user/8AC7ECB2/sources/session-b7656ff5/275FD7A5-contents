# Load required libraries
library(XML)
library(dplyr)
library(lubridate)
library(ggplot2)

# Parse the XML file
xml_file <- xmlParse("C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Data/datalog.xml")

# Extract all <record> nodes
records <- getNodeSet(xml_file, "//record")

# Convert each record to a row
data_list <- lapply(records, function(record) {
  date_time <- xmlValue(record[["date"]])
  # Get the <probe> node
  probe_node <- record[["probe"]]
  temp_value <- xmlValue(probe_node[["value"]])
  
  data.frame(
    DateTime = date_time,
    Temperature = as.numeric(temp_value),
    stringsAsFactors = FALSE
  )
})

# Combine into a single data frame
Apex.Data <- bind_rows(data_list)

# Convert DateTime to POSIXct and split into Date and Time
Apex.Data <- Apex.Data %>%
  mutate(
    DateTime = mdy_hms(DateTime),
    Date = as.Date(DateTime),
    Time = format(DateTime, "%H:%M:%S")
  ) %>%
  select(Date, Time, Temperature, DateTime)

# Print the first rows to confirm
print(head(Apex.Data))

# Plot Temperature over DateTime
p <- ggplot(Apex.Data, aes(x = DateTime, y = Temperature)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(
    title = "Apex Temperature Over Time",
    x = "Date and Time",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

print(p)

# Save cleaned data to CSV
write.csv(
  Apex.Data,
  "C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Data/apex_temperature_data.csv",
  row.names = FALSE
)



