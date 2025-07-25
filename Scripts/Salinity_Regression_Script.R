
# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

# Read CSV (ensure filename has no extra space)
data <- read.csv("Data/PolypBailoutTEST2.csv", stringsAsFactors = FALSE)

# Inspect column names and preview time values
print(colnames(data))
print(head(data$Polyp.A.Chambers))

# Convert time to POSIXct — assuming time is in "HH:MM" format
data$Time <- as.numeric(
  as.POSIXct(data$Polyp.A.Chambers, format = "%H:%M") -
    as.POSIXct(data$Polyp.A.Chambers[1], format = "%H:%M")
) / 3600  # convert from seconds to hours

# Reshape data to long format
long_data <- data %>%
  pivot_longer(cols = starts_with("C"), names_to = "Chamber", values_to = "Value")

# Calculate regression slopes for each chamber
slopes <- long_data %>%
  group_by(Chamber) %>%
  summarise(Slope = coef(lm(Value ~ Time))[2])

# Print slopes
print(slopes)

# Plot scatter with regression lines and slope info
ggplot(long_data, aes(x = Time, y = Value, color = Chamber)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Regression Lines for Each Chamber",
    subtitle = paste("Slopes (ppt/hr):",
                     paste(slopes$Chamber, round(slopes$Slope, 2), sep = " = ", collapse = ", ")),
    x = "Time (hours since start)",
    y = "Salinity (ppt)"
  ) +
  theme_minimal()

