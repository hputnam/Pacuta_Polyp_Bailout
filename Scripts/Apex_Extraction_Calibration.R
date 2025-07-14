#Putnam Labs 20250711
#Pacuta Polyp Bailout
#By Thatcher Johnstone-Wright

# Load required packages
library(XML)
library(dplyr)

# Define the path to the XML file
xml_file_path <- "C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Data/Probe_Calibration/202507091500day=.5datalog.xml"

# Parse the XML file
xml_data <- xmlParse(xml_file_path)

# Extract all records
records <- getNodeSet(xml_data, "//record")

# Function to extract data from one record
extract_data <- function(record) {
  datetime <- xmlValue(record[["date"]])
  datetime_parts <- strsplit(datetime, " ")[[1]]
  date <- datetime_parts[1]
  time <- datetime_parts[2]
  
  values <- sapply(c("Condx1", "Condx2", "pH_1", "pHx2", "Tmpx1", "Tmpx2"), function(name) {
    node <- getNodeSet(record, paste0("./probe[name='", name, "']/value"))
    if (length(node) > 0) {
      as.numeric(xmlValue(node[[1]]))
    } else {
      NA
    }
  })
  
  data.frame(
    Date = date,
    Time = time,
    Cond_1 = values[1],
    Cond_2 = values[2],
    pH_1 = values[3],
    pH_2 = values[4],
    Temp_1 = values[5],
    Temp_2 = values[6],
    stringsAsFactors = FALSE
  )
}

# Apply the extraction function to all records
data_list <- lapply(records, extract_data)
combined_data <- bind_rows(data_list)

# Save to CSV in the same folder
output_path <- "C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Data/Probe_Calibration/apex_cleaned_data.csv"
write.csv(combined_data, output_path, row.names = FALSE)

# Confirm save
cat("CSV saved to:", output_path)

# Load required packages
library(ggplot2)
library(readr)
library(dplyr)

# Set path to your CSV and output path 
csv_path <- "C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Data/Probe_Calibration/apex_cleaned_data.csv"
output_dir <- "C:/Users/Thatcher/OneDrive - University of Rhode Island/Summer Research 2025/Pacuta_Polyp_Bailout/Outputs/Probe_Calibration"
# Read the CSV
data <- read_csv(csv_path)

# Combine Date and Time into a single datetime object
data <- data %>%
  mutate(Datetime = as.POSIXct(paste(Date, Time), format="%m/%d/%Y %H:%M:%S"))
# Plot 1: Conductivity
plot_cond <- ggplot(data, aes(x = Datetime)) +
  geom_line(aes(y = Cond_1, color = "Cond_1")) +
  geom_line(aes(y = Cond_2, color = "Cond_2")) +
  labs(title = "Conductivity Over Time", y = "Conductivity", x = "Time") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  scale_color_manual(values = c("Cond_1" = "blue", "Cond_2" = "red"))

print(plot_cond)

ggsave(filename = file.path(output_dir, "conductivity_plot.png"), plot = plot_cond, width = 8, height = 4)

# Plot 2: pH
plot_ph<- ggplot(data, aes(x = Datetime)) +
  geom_line(aes(y = pH_1, color = "pH_1")) +
  geom_line(aes(y = pH_2, color = "pH_2")) +
  labs(title = "pH Over Time", y = "pH", x = "Time") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  scale_color_manual(values = c("pH_1" = "darkgreen", "pH_2" = "orange"))
  
print(plot_ph)
  
ggsave(filename = file.path(output_dir, "ph_plot.png"), plot = plot_ph, width = 8, height = 4)

# Plot 3: Temperature
plot_temp<- ggplot(data, aes(x = Datetime)) +
  geom_line(aes(y = Temp_1, color = "Temp_1")) +
  geom_line(aes(y = Temp_2, color = "Temp_2")) +
  labs(title = "Temperature Over Time", y = "Temperature (°C)", x = "Time") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  scale_color_manual(values = c("Temp_1" = "purple", "Temp_2" = "brown"))
  
print(plot_temp)
  
ggsave(filename = file.path(output_dir, "temperature_plot.png"), plot = plot_temp, width = 8, height = 4)

# t-test for Conductivity
t_test_cond <- t.test(data$Cond_1, data$Cond_2, paired = TRUE)
print("Conductivity t-test:")
print(t_test_cond)

# t-test for pH
t_test_ph <- t.test(data$pH_1, data$pH_2, paired = TRUE)
print("pH t-test:")
print(t_test_ph)

# t-test for Temperature
t_test_temp <- t.test(data$Temp_1, data$Temp_2, paired = TRUE)
print("Temperature t-test:")
print(t_test_temp)

# Regression Equation for Reference probe (conductivity)
data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%m/%d/%Y %H:%M:%S")
data$Time_Hours <- as.numeric(difftime(data$Datetime, min(data$Datetime), units = "hours"))
cond1_lm <- lm(Cond_1 ~ Time_Hours, data = data)
intercept <- round(coef(cond1_lm)[1], 4)
slope <- round(coef(cond1_lm)[2], 4)
r_squared <- round(summary(cond1_lm)$r.squared, 4)
cat("Regression equation: Cond_1 =",
    intercept, "+", slope, "* Time_Hours\n")
cat("R-squared:", r_squared, "\n")

# Regression for Reference Probe(pH)
ph1_lm <- lm(pH_1 ~ Time_Hours, data = data)
ph1_intercept <- round(coef(ph1_lm)[1], 4)
ph1_slope <- round(coef(ph1_lm)[2], 4)
ph1_r2 <- round(summary(ph1_lm)$r.squared, 4)
cat("pH_1 regression equation: pH_1 =", ph1_intercept, "+", ph1_slope, "* Time_Hours\n")
cat("pH_1 R-squared:", ph1_r2, "\n\n")
# Regression for Reference Probe (temp)
temp1_lm <- lm(Temp_1 ~ Time_Hours, data = data)
temp1_intercept <- round(coef(temp1_lm)[1], 4)
temp1_slope <- round(coef(temp1_lm)[2], 4)
temp1_r2 <- round(summary(temp1_lm)$r.squared, 4)
cat("Temp_1 regression equation: Temp_1 =", temp1_intercept, "+", temp1_slope, "* Time_Hours\n")
cat("Temp_1 R-squared:", temp1_r2, "\n")
