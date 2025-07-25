# Load necessary libraries
# (Install them if not already installed using install.packages("tidyverse"))
library(tidyverse)

# Read the CSV file
data <- read.csv("Data/PolypBailoutTEST3.csv", stringsAsFactors = FALSE)

# Combine Date and Time into a single POSIXct datetime
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y%m%d %H:%M")

# Convert datetime to hours since start
data$Time_Hours <- as.numeric(difftime(data$DateTime, min(data$DateTime, na.rm = TRUE), units = "hours"))

# Reshape data to long format
data_long <- data %>%
  select(Time_Hours, starts_with("C")) %>%
  pivot_longer(cols = starts_with("C"), names_to = "Chamber", values_to = "Salinity")

# Run linear regression for each chamber and extract slopes
slopes <- data_long %>%
  group_by(Chamber) %>%
  do({
    model <- lm(Salinity ~ Time_Hours, data = .)
    data.frame(Slope = coef(model)[["Time_Hours"]])
  })

# Print the slopes
print(slopes)

# Create scatter plot with regression lines
ggplot(data_long, aes(x = Time_Hours, y = Salinity, color = Chamber)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Salinity over Time by Chamber",
       x = "Time (hours since start)",
       y = "Salinity (ppt)") +
  theme_minimal()

