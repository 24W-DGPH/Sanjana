# Load necessary packages
install.packages("pacman")

pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary-based cleaning
  dplyr, # data management
  ggplot2, # data visualization
  readxl, # real excel data
  shiny,  # shiny app
  tidyr   # data reshaping
)

# Import messy data
Sanjana_messydata_ <- read_excel("Sanjana messydata_.xlsx")

# Rename necessary columns
names(Sanjana_messydata_)[1]<- "Study ID"
names(Sanjana_messydata_)[2]<- "Race"
names(Sanjana_messydata_)[7]<- "Day 1 GFR"
names(Sanjana_messydata_)[8]<- "Day 2 GFR"
names(Sanjana_messydata_)[9]<- "Day 3 GFR"
names(Sanjana_messydata_)[11]<- "Day 5 GFR"
names(Sanjana_messydata_)[12]<- "Diagnosis"
names(Sanjana_messydata_)[13]<- "Sediment"
names(Sanjana_messydata_)[14]<- "HD catheter"
names(Sanjana_messydata_)[15]<- "AV fistula"

# Remove unnecessary column
Sanjana_messydata_$...5 <- NULL

# Convert "Study ID" to ascending order
Sanjana_messydata_$`Study ID` <- seq_along(Sanjana_messydata_$`Study ID`)

# Standardize "Sex" column values
Sanjana_messydata_$Sex <- ifelse(Sanjana_messydata_$Sex == "Male", "M",
                                 ifelse(Sanjana_messydata_$Sex == "Female", "F", Sanjana_messydata_$Sex))

# Expand abbreviations in "Hispanic" column
Sanjana_messydata_$Hispanic <- ifelse(Sanjana_messydata_$Hispanic == "H", "Hispanic",
                                      ifelse(Sanjana_messydata_$Hispanic == "NH", "Not Hispanic", Sanjana_messydata_$Hispanic))

# Convert relevant entries to uppercase
Sanjana_messydata_$`HD catheter` <- toupper(Sanjana_messydata_$`HD catheter`)
Sanjana_messydata_$`AV fistula` <- toupper(Sanjana_messydata_$`AV fistula`)

# Convert data from wide to long format for visualization
Sanjana_messydata_long <- Sanjana_messydata_ %>%
  pivot_longer(cols = c(`Day 1 GFR`, `Day 2 GFR`, `Day 3 GFR`, `Day 5 GFR`),
               names_to = "Day",
               values_to = "GFR") %>%
  drop_na(GFR)  # Remove missing values for cleaner plots

# Convert 'Day' to a factor for correct ordering
Sanjana_messydata_long$Day <- factor(Sanjana_messydata_long$Day, 
                                     levels = c("Day 1 GFR", "Day 2 GFR", "Day 3 GFR", "Day 5 GFR"))

# Corrected ggplot Visualizations

# Boxplot of GFR Across Days
ggplot(Sanjana_messydata_long, aes(x = Day, y = GFR, fill = Day)) +
  geom_boxplot() +
  labs(title = "GFR Levels Across Days", x = "Day", y = "GFR") +
  theme_minimal()

# Violin Plot of GFR Distribution by Day
ggplot(Sanjana_messydata_long, aes(x = Day, y = GFR, fill = Day)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "GFR Distribution Across Days", x = "Day", y = "GFR") +
  theme_minimal()

# Scatter Plot of Day 1 GFR vs. Day 5 GFR
ggplot(Sanjana_messydata_, aes(x = `Day 1 GFR`, y = `Day 5 GFR`, color = Race)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Day 1 GFR vs. Day 5 GFR", x = "Day 1 GFR", y = "Day 5 GFR") +
  theme_minimal()

# Histogram of Day 1 GFR
ggplot(Sanjana_messydata_, aes(x = `Day 1 GFR`)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Histogram of Day 1 GFR", x = "Day 1 GFR", y = "Frequency") +
  theme_minimal()

# Bar Chart of Diagnosis Counts
ggplot(Sanjana_messydata_, aes(x = Diagnosis, fill = Diagnosis)) +
  geom_bar() +
  labs(title = "Count of Each Diagnosis", x = "Diagnosis", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Facet Grid for GFR by Race
ggplot(Sanjana_messydata_long, aes(x = Day, y = GFR, fill = Race)) +
  geom_boxplot() +
  facet_wrap(~Race) +
  labs(title = "GFR Levels by Race Across Days", x = "Day", y = "GFR") +
  theme_minimal()
