install.packages("tidyverse")  # Install the tidyverse package if not already installed
library("tidyverse")          # Load the tidyverse package

#Load the dataset
# Use the correct file path with either forward slashes or double backslashes
country <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\country.csv")
country
league <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\league.csv")
league
Match_1 <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\Match_1.csv")
Match_1
player <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\player.csv")
player
player_attributes <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\player_attributes.csv")
player_attributes
team <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\country.csv")
team
Team_attributes <- read.csv("C:\\Users\\Srinidhi\\Downloads\\Project_G10_2\\Team_attributes_1.csv")
Team_attributes

#handling missing values
for (col in names(Match_1)) {
  if (all(is.na(Match_1[[col]]))) {
    Match_1 <- Match_1[, !(names(Match_1) %in% col)]
  }
}

#Removing Duplicate rows
Match_1<-distinct(Match_1)
Match_1

#############################################################################################
#PLAYER PERFORMANCE ANALYSIS
#############################################################################################

#Removed time format since all of them are 00:00:00
player_attributes$date <- as.Date(strptime(player_attributes$date, format = "%Y-%m-%d"))
Team_attributes$date <- as.Date(strptime(Team_attributes$date, format = "%Y-%m-%d"))

mean_value <- mean(Team_attributes$buildUpPlayDribbling, na.rm = TRUE)
Team_attributes$buildUpPlayDribbling[is.na(Team_attributes$buildUpPlayDribbling)] <- mean_value

# Perform ordinal encoding for attacking and defensive work rates
order_levels <- c("medium", "high")

player_attributes$attacking_work_rate <- as.numeric(factor(player_attributes$attacking_work_rate))
player_attributes$defensive_work_rate <- as.numeric(factor(player_attributes$defensive_work_rate))

library(dlookr)
introduce(player_attributes) %>% t() 

plot_intro(player_attributes)

#plot_bar(player_attributes)

plot_histogram(player_attributes)

plot_density(player_attributes)


player <- player %>%
  mutate(bmi = (weight/2.205)/(height/100)^2)

hist(player$bmi)

player_attributes <- player_attributes %>%
  mutate(perception = vision* aggression)

hist(player_attributes$perception)

# Create a feature representing a player's skill level based on specific attributes
player_attributes$skill_level <- rowMeans(player_attributes[, c("dribbling", "ball_control", "finishing")])

# Create a feature based on player age group
# Assuming 'birth_date' is the column containing birthday dates
player$birthday <- as.Date(player$birthday, format="%Y-%m-%d")

# Calculate age
player$age <- as.numeric(difftime(Sys.Date(), player$birthday, units = "days") / 365.25)

# Create an age group feature
player$age_group <- cut(player$age, breaks = c(0, 20, 25, 30, 35, Inf), labels = c("Under 20", "20-25", "26-30", "31-35", "Over 35"))
hist(player$age)

#Data-pre processing - Identifying missing values
missing_values <- sum(is.na(player_attributes$overall_rating))

# Calculate overall trend only if there are no missing values
if (missing_values == 0) {
  player_attributes$overall_trend <- ifelse(player_attributes$overall_rating > mean(player_attributes$overall_rating, na.rm = TRUE), "Above Average", "Below Average")
} else {
  # Handle the case where there are missing values
  print("There are missing values in overall_rating. Please handle them before calculating the overall trend.")
}
mean_value <- mean(player_attributes$overall_rating, na.rm = TRUE)
player_attributes$overall_rating[is.na(player_attributes$overall_rating)] <- mean_value
player_attributes$date <- as.Date(player_attributes$date, format = "%Y-%m-%d")

# Create a new feature based on overall rating trends
player_attributes$overall_trend <- ifelse(player_attributes$overall_rating > mean(player_attributes$overall_rating), "Above Average", "Below Average")

# Check for missing values in overall_rating
colSums(is.na(player_attributes)) 
# Create a feature representing the ratio of physical attributes
player_attributes$physical_ratio <- player_attributes$strength / player_attributes$agility

#GOAL KEEPER
mean_value1 <- mean(player_attributes$gk_diving, na.rm = TRUE)
player_attributes$gk_diving[is.na(player_attributes$gk_diving)] <- mean_value1

mean_value2 <- mean(player_attributes$gk_handling, na.rm = TRUE)
player_attributes$gk_handling[is.na(player_attributes$gk_handling)] <- mean_value2

mean_value3 <- mean(player_attributes$gk_positioning, na.rm = TRUE)
player_attributes$gk_positioning[is.na(player_attributes$gk_positioning)] <- mean_value3

mean_value4 <- mean(player_attributes$gk_kicking, na.rm = TRUE)
player_attributes$gk_kicking[is.na(player_attributes$gk_kicking)] <- mean_value4

mean_value5 <- mean(player_attributes$gk_reflexes, na.rm = TRUE)
player_attributes$gk_reflexes[is.na(player_attributes$gk_reflexes)] <- mean_value5

player_attributes$overall_goalkeeping <- rowMeans(player_attributes[, c("gk_diving", "gk_handling", "gk_positioning", "gk_kicking", "gk_reflexes")])

player_attributes$good_shot_stopping <- player_attributes$gk_diving + player_attributes$gk_reflexes > mean(player_attributes$gk_diving + player_attributes$gk_reflexes)

player_attributes$good_control <- player_attributes$gk_handling + player_attributes$gk_positioning > mean(player_attributes$gk_handling + player_attributes$gk_positioning)

player_attributes$effective_kicking <- player_attributes$gk_kicking > mean(player_attributes$gk_kicking)

column_names <- names(player_attributes)
print(column_names)
#hist(player_attributes$overall_goalkeeping)


mean1 <- mean(player_attributes$short_passing, na.rm = TRUE)
player_attributes$short_passing[is.na(player_attributes$short_passing)] <- mean1

mean2 <- mean(player_attributes$long_passing, na.rm = TRUE)
player_attributes$long_passing[is.na(player_attributes$long_passing)] <- mean2


player_attributes$passing_score <- rowMeans(player_attributes[, c("short_passing", "long_passing")])

View(player_attributes)

mean3 <- mean(player_attributes$vision, na.rm = TRUE)
player_attributes$vision[is.na(player_attributes$vision)] <- mean3

mean4 <- mean(player_attributes$crossing, na.rm = TRUE)
player_attributes$crossing[is.na(player_attributes$crossing)] <- mean4

mean5 <- mean(player_attributes$curve, na.rm = TRUE)
player_attributes$curve[is.na(player_attributes$curve)] <- mean5

player_attributes$playmaking_ability <- rowMeans(player_attributes[, c("vision", "crossing", "curve")])

mean6 <- mean(player_attributes$ball_control, na.rm = TRUE)
player_attributes$ball_control[is.na(player_attributes$ball_control)] <- mean6

mean7 <- mean(player_attributes$dribbling, na.rm = TRUE)
player_attributes$dribbling[is.na(player_attributes$dribbling)] <- mean7

player_attributes$ball_control_score <- rowMeans(player_attributes[, c("ball_control", "dribbling")])

mean8 <- mean(player_attributes$interceptions, na.rm = TRUE)
player_attributes$interceptions[is.na(player_attributes$interceptions)] <- mean8

mean9 <- mean(player_attributes$standing_tackle, na.rm = TRUE)
player_attributes$standing_tackle[is.na(player_attributes$standing_tackle)] <- mean9

mean0 <- mean(player_attributes$sliding_tackle, na.rm = TRUE)
player_attributes$sliding_tackle[is.na(player_attributes$sliding_tackle)] <- mean0

player_attributes$defensive_score <- rowMeans(player_attributes[, c("interceptions", "standing_tackle", "sliding_tackle")])

player_attributes$midfield_performance <- rowMeans(player_attributes[, c("passing_score", "playmaking_ability", "ball_control_score", "defensive_score")])

#hist(player_attributes$midfield_performance)

View(player_attributes)

#STRIKER

mean_1 <- mean(player_attributes$volleys, na.rm = TRUE)
player_attributes$volleys[is.na(player_attributes$volleys)] <- mean_1

mean_2 <- mean(player_attributes$shot_power, na.rm = TRUE)
player_attributes$shot_power[is.na(player_attributes$shot_power)] <- mean_2

player_attributes$finishing_score <- rowMeans(player_attributes[, c("finishing", "volleys", "shot_power")])

mean_3 <- mean(player_attributes$long_shots, na.rm = TRUE)
player_attributes$long_shots[is.na(player_attributes$long_shots)] <- mean_3

player_attributes$accuracy_score <- rowMeans(player_attributes[, c("finishing", "volleys", "shot_power", "long_shots")])


mean_4 <- mean(player_attributes$acceleration, na.rm = TRUE)
player_attributes$acceleration[is.na(player_attributes$acceleration)] <- mean_4

mean_5 <- mean(player_attributes$sprint_speed, na.rm = TRUE)
player_attributes$sprint_speed[is.na(player_attributes$sprint_speed)] <- mean_5

mean_6 <- mean(player_attributes$agility, na.rm = TRUE)
player_attributes$agility[is.na(player_attributes$agility)] <- mean_6

player_attributes$speed_agility_score <- rowMeans(player_attributes[, c("acceleration", "sprint_speed", "agility")])

mean_7 <- mean(player_attributes$strength, na.rm = TRUE)
player_attributes$strength[is.na(player_attributes$strength)] <- mean_7

mean_8 <- mean(player_attributes$stamina, na.rm = TRUE)
player_attributes$stamina[is.na(player_attributes$stamina)] <- mean_8

player_attributes$strength_stamina_score <- rowMeans(player_attributes[, c("strength", "stamina")])

player_attributes$dribbling_score <- rowMeans(player_attributes[, c("dribbling", "ball_control", "agility")])

mean_9 <- mean(player_attributes$positioning, na.rm = TRUE)
player_attributes$positioning[is.na(player_attributes$positioning)] <- mean_9

player_attributes$offensive_performance <- rowMeans(player_attributes[, c("finishing", "volleys", "shot_power", "acceleration", "sprint_speed", "agility", "strength", "stamina", "dribbling", "ball_control", "positioning", "vision")])
#hist(player_attributes$offensive_performance)


# DEFENSE
library(dplyr)

# Function to calculate mode
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Install and load the DescTools package for the Mode function
install.packages("DescTools")
library(DescTools)

# Replace missing values with mean for numeric columns and mode for non-numeric columns
player_attributes <- player_attributes %>%
  mutate_at(vars(-date), funs(if (is.numeric(.)) ifelse(is.na(.), mean(., na.rm = TRUE), .) else ifelse(is.na(.), Mode(., na.rm = TRUE), .)))

# Check the result
View(player_attributes)


# Check the result
View(player_attributes)

player_attributes$defensive_rating <- rowMeans(player_attributes[, c("marking", "standing_tackle", "sliding_tackle", "interceptions")])
colSums(is.na(player_attributes)) 

player_attributes$positional_awareness <- rowMeans(player_attributes[, c("positioning", "marking", "interceptions")])

player_attributes$strength_stamina <- rowMeans(player_attributes[, c("strength", "stamina")])

player_attributes$aggressiveness <- rowMeans(player_attributes[, c("aggression", "strength")])

player_attributes$defensive_performance <- rowMeans(player_attributes[, c("marking", "standing_tackle", "sliding_tackle", "interceptions", "positioning", "aggression")])
#hist(player_attributes$defensive_performance)

player_attributes$ball_winning_ability <- rowMeans(player_attributes[, c("standing_tackle", "sliding_tackle", "interceptions", "aggression")])

player_attributes$agility_speed <- rowMeans(player_attributes[, c("agility", "sprint_speed", "acceleration")])

player_attributes$defensive_work_rate <- as.factor(player_attributes$defensive_work_rate)

player_attributes$defensive_player_profile <- ifelse(player_attributes$defensive_performance > mean(player_attributes$defensive_performance), "High", "Low")

#Create an overall performance score based on selected attributes
player_attributes$performance_score <- rowMeans(player_attributes[, c("crossing", "finishing", "heading_accuracy", "short_passing", "volleys",
                                                                      "dribbling", "curve", "free_kick_accuracy", "long_passing",
                                                                      "ball_control", "acceleration", "sprint_speed", "agility",
                                                                      "reactions", "balance", "shot_power", "jumping", "stamina",
                                                                      "strength", "long_shots", "aggression", "interceptions",
                                                                      "positioning", "vision", "penalties", "marking",
                                                                      "standing_tackle", "sliding_tackle")])

# Assuming player_attributes$offensive_performance is already calculated
# Assuming player_attributes$offensive_performance is already calculated
# Find the index of the row with the highest offensive_performance
index_max_offensive <- which.max(player_attributes$offensive_performance)

# Extract player_api_id corresponding to the highest offensive_performance
player_api_id_max_offensive <- player_attributes$player_api_id[index_max_offensive]

# Print the result
cat("Player with the highest offensive performance:", player_api_id_max_offensive, "\n")
cat("The highest offensive performance is:", player_attributes$offensive_performance[index_max_offensive], "\n")

#HIGHEST DEFENSE PERFORMER
index_max_defense <- which.max(player_attributes$defensive_performance)

# Extract player_api_id corresponding to the highest offensive_performance
player_api_id_max_defense <- player_attributes$player_api_id[index_max_defense]

# Print the result
cat("Player with the highest defense performance:", player_api_id_max_defense, "\n")
cat("The highest defensive performance is:", player_attributes$defensive_performance[index_max_defense], "\n")

#BEST GOAL KEEPER

index_max_gk <- which.max(player_attributes$overall_goalkeeping)

# Extract player_api_id corresponding to the highest offensive_performance
player_api_id_max_gk <- player_attributes$player_api_id[index_max_gk]

# Print the result
cat("Player with the highest goal keeping performance:", player_api_id_max_gk, "\n")
cat("The highest goal keeping performance is:", player_attributes$overall_goalkeeping[index_max_gk], "\n")

#BEST MIDFIELDER

index_max_mid <- which.max(player_attributes$midfield_performance)

# Extract player_api_id corresponding to the highest offensive_performance
player_api_id_max_mid <- player_attributes$player_api_id[index_max_mid]

# Print the result
cat("Player with the highest goal keeping performance:", player_api_id_max_mid, "\n")
cat("The highest goal keeping performance is:", player_attributes$midfield_performance[index_max_mid], "\n")

#####################################################################################
#CHECKING FOR OUTLIERS AND REMOVING THEM
#####################################################################################

# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming 'player_attributes' is your dataset with player attributes
# Boxplot for BMI
ggplot(player, aes(x = 1, y = bmi)) +
  geom_boxplot() +
  labs(title = "BMI Distribution Boxplot", x = "", y = "BMI") +
  theme_minimal()

# Boxplot for Perception
ggplot(player_attributes, aes(x = 1, y = perception)) +
  geom_boxplot() +
  labs(title = "Perception Distribution Boxplot", x = "", y = "Perception") +
  theme_minimal()

# Boxplot for Age
ggplot(player, aes(x = 1, y = age)) +
  geom_boxplot() +
  labs(title = "Age Distribution Boxplot", x = "", y = "Age") +
  theme_minimal()

# Boxplot for Offensive Performance
ggplot(player_attributes, aes(x = 1, y = offensive_performance)) +
  geom_boxplot() +
  labs(title = "Offensive Performance Distribution Boxplot", x = "", y = "Offensive Performance") +
  theme_minimal()

# Boxplot for Defensive Performance
ggplot(player_attributes, aes(x = 1, y = defensive_performance)) +
  geom_boxplot() +
  labs(title = "Defensive Performance Distribution Boxplot", x = "", y = "Defensive Performance") +
  theme_minimal()

# Boxplot for Overall Goalkeeping
ggplot(player_attributes, aes(x = 1, y = overall_goalkeeping)) +
  geom_boxplot() +
  labs(title = "Overall Goalkeeping Distribution Boxplot", x = "", y = "Overall Goalkeeping") +
  theme_minimal()

# Boxplot for Midfield Performance
ggplot(player_attributes, aes(x = 1, y = midfield_performance)) +
  geom_boxplot() +
  labs(title = "Midfield Performance Distribution Boxplot", x = "", y = "Midfield Performance") +
  theme_minimal()

# Boxplot for Defensive Work Rate
ggplot(player_attributes, aes(x = defensive_work_rate, y = 1)) +
  geom_boxplot() +
  labs(title = "Defensive Work Rate Distribution Boxplot", x = "Defensive Work Rate", y = "") +
  theme_minimal()

# Remove outliers for Overall Goalkeeping using IQR
Q1 <- quantile(player_attributes$overall_goalkeeping, 0.25)
Q3 <- quantile(player_attributes$overall_goalkeeping, 0.75)
IQR <- Q3 - Q1

# Define the upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers
player_attributes <- player_attributes %>%
  filter(overall_goalkeeping >= lower_bound & overall_goalkeeping <= upper_bound)

# Remove outliers for Midfield Performance using IQR
Q1_mid <- quantile(player_attributes$midfield_performance, 0.25)
Q3_mid <- quantile(player_attributes$midfield_performance, 0.75)
IQR_mid <- Q3_mid - Q1_mid

# Define the upper and lower bounds for outliers
lower_bound_mid <- Q1_mid - 1.5 * IQR_mid
upper_bound_mid <- Q3_mid + 1.5 * IQR_mid

# Remove outliers
player_attributes <- player_attributes %>%
  filter(midfield_performance >= lower_bound_mid & midfield_performance <= upper_bound_mid)

#####################################################################################
#EXTRACTING RELEVANT FEATURES
#####################################################################################

# Step 1: Feature Selection
selected_features <- player_attributes %>%
  select(player_api_id, overall_rating, potential, attacking_work_rate, defensive_work_rate,
         crossing, finishing, short_passing, volleys, dribbling, curve,
         free_kick_accuracy, long_passing, ball_control, acceleration,
         agility, reactions, jumping, stamina, vision, standing_tackle,
         sliding_tackle)
selected_features
# Step 2: Create a Performance Score
# Define a formula based on selected features and assign weights
performance_score <- function(data) {
  data %>%
    mutate(performance_score = 0.3 * overall_rating + 0.2 * potential +
             0.1 * crossing + 0.1 * finishing + 0.1 * short_passing +
             0.1 * dribbling + 0.1 * reactions + 0.1 * ball_control)
}
performance_score(player_attributes)

# Apply the performance score function
soccer_data <- performance_score(player_attributes)
soccer_data
# Step 3: Order players Based on Rating and Potential
top_players <- soccer_data %>%
  arrange(desc(overall_rating)) %>%
  select(player_api_id, overall_rating, potential, performance_score)

# Display the top players
head(top_players)


# Step 4: Calculate Mean Performance Score for Each player
average_performance <- soccer_data %>%
  group_by(player_api_id) %>%
  summarise(mean_performance_score = mean(performance_score, na.rm = TRUE))

# Display the mean performance scores
head(average_performance)

# Convert attacking_work_rate and defensive_work_rate to numeric
player_attributes$attacking_work_rate <- as.numeric(player_attributes$attacking_work_rate)
player_attributes$defensive_work_rate <- as.numeric(player_attributes$defensive_work_rate)

# Group by player_api_id and calculate mean values
performance_by_position <- soccer_data %>%
  group_by(player_api_id) %>%
  summarise(
    avg_attacking_work_rate = mean(attacking_work_rate, na.rm = TRUE),
    avg_defensive_work_rate = mean(defensive_work_rate, na.rm = TRUE),
    avg_performance_score = mean(performance_score, na.rm = TRUE)
  )

# Display performance by preferred position
head(performance_by_position)

# Assuming 'player_attributes' is your dataset with player attributes
# Adjust column names based on your actual dataset

# Define a formula for the performance score
calculate_performance_score <- function(data) {
  data %>%
    mutate(
      offensive_score = crossing + finishing + short_passing + dribbling + vision,
      defensive_score = standing_tackle + sliding_tackle,
    )
}

# Apply the performance score function
player_attributes <- calculate_performance_score(player_attributes)

#library(dlookr)
introduce(player_attributes) %>% t() 

plot_intro(player_attributes)

plot_bar(player_attributes)

plot_histogram(player_attributes)

plot_density(player_attributes)

#####################################################################################
#BAR PLOT
#####################################################################################

# Create a bar plot
barplot(table(player_attributes$offensive_performance), 
        main = "Bar Plot of Offensive Performance",
        xlab = "Offensive Performance", ylab = "Frequency",
        col = "black", border = "skyblue")


barplot(table(player_attributes$defensive_performance), 
        main = "Bar Plot of Defense Performance",
        xlab = "Defense Performance", ylab = "Frequency",
        col = "black", border = "purple")

barplot(table(player_attributes$overall_goalkeeping), 
        main = "Bar Plot of Goal Keeper Performance",
        xlab = "Goal Keeper Performance", ylab = "Frequency",
        col = "black", border = "violet")

barplot(table(player_attributes$midfield_performance), 
        main = "Bar Plot of Mid Fielder Performance",
        xlab = "Mid Fielder Performance", ylab = "Frequency",
        col = "black", border = "darkgreen")

#####################################################################################
#CREATING A HISTOGRAM 
#####################################################################################

hist(player_attributes$offensive_performance)
hist(player_attributes$midfield_performance)
hist(player_attributes$defensive_performance)
hist(player_attributes$overall_goalkeeping)

#####################################################################################
#CREATING A REPORT
#####################################################################################

#creating a report
install.packages('ggplot2')
library('ggplot2')
player_attributes
View(player_attributes)
install.packages('DataExplorer')
library(DataExplorer) # for exploratory data analysis
library(tidyverse)    # for data, data wrangling and visualization
# report without a response variable - without a dependent variable
create_report(player_attributes)

#################################################################################################
#TEAM PERFORMANCE ANALYSIS
#################################################################################################

library(ggplot2)
library(dplyr)

#Team_attributes <- read.csv("C:\\Users\\devit\\OneDrive\\Desktop\\Team_attributes_1.csv")
library(DataExplorer) # for exploratory data analysis
library(tidyverse)    # for data, data wrangling and visualization
# report without a response variable - without a dependent variable
create_report(Team_attributes)
Team_attributes
print(unique(Team_attributes$team_api_id))

#################################################################################################
#DATA PRE-PROCESSING - ENCODING THE CATEGORICAL DATA
#################################################################################################

print(sum(is.na(Team_attributes)))


library(dplyr)

# Assuming Team_attributes is your data frame
Team_attributes <- mutate_all(Team_attributes, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

print(sum(is.na(Team_attributes)))


# Define the encoding rules
encoding_rules <- list(
  original_value = c("Organised","Free Form"),
  new_value = c("1", "2")
)
View(Team_attributes)

# Encode the particular values in the specified column
Team_attributes$buildUpPlayPositioningClass <- ifelse(Team_attributes$buildUpPlayPositioningClass %in% encoding_rules$original_value,
                                                      encoding_rules$new_value[match(Team_attributes$buildUpPlayPositioningClass, encoding_rules$original_value)],
                                                      Team_attributes$buildUpPlayPositioningClass)

print(Team_attributes$buildUpPlayPositioningClass)


encoding_rules <- list(
  original_value = c("Slow","Balanced","Fast"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$buildUpPlaySpeedClass<- ifelse(Team_attributes$buildUpPlaySpeedClass %in% encoding_rules$original_value,
                                               encoding_rules$new_value[match(Team_attributes$buildUpPlaySpeedClass, encoding_rules$original_value)],
                                               Team_attributes$buildUpPlaySpeedClass)

print(Team_attributes$buildUpPlaySpeedClass)


encoding_rules <- list(
  original_value = c("Little","Normal","Lots"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$buildUpPlayDribblingClass<- ifelse(Team_attributes$buildUpPlayDribblingClass %in% encoding_rules$original_value,
                                                   encoding_rules$new_value[match(Team_attributes$buildUpPlayDribblingClass, encoding_rules$original_value)],
                                                   Team_attributes$buildUpPlayDribblingClass)

print(Team_attributes$buildUpPlayDribblingClass)



encoding_rules <- list(
  original_value = c("Short","Mixed","Long"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$buildUpPlayPassingClass<- ifelse(Team_attributes$buildUpPlayPassingClass %in% encoding_rules$original_value,
                                                 encoding_rules$new_value[match(Team_attributes$buildUpPlayPassingClass, encoding_rules$original_value)],
                                                 Team_attributes$buildUpPlayPassingClass)

print(Team_attributes$buildUpPlayPassingClass)


##################
encoding_rules <- list(
  original_value = c("Risky","Normal","Safe"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$chanceCreationPassingClass<- ifelse(Team_attributes$chanceCreationPassingClass %in% encoding_rules$original_value,
                                                    encoding_rules$new_value[match(Team_attributes$chanceCreationPassingClass, encoding_rules$original_value)],
                                                    Team_attributes$chanceCreationPassingClass)

print(Team_attributes$chanceCreationPassingClass)



###############
encoding_rules <- list(
  original_value = c("Little","Normal","Lots"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$chanceCreationCrossingClass<- ifelse(Team_attributes$chanceCreationCrossingClass %in% encoding_rules$original_value,
                                                     encoding_rules$new_value[match(Team_attributes$chanceCreationCrossingClass, encoding_rules$original_value)],
                                                     Team_attributes$chanceCreationCrossingClass)

print(Team_attributes$chanceCreationCrossingClass)



####################
encoding_rules <- list(
  original_value = c("Little","Normal","Lots"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$chanceCreationShootingClass<- ifelse(Team_attributes$chanceCreationShootingClass %in% encoding_rules$original_value,
                                                     encoding_rules$new_value[match(Team_attributes$chanceCreationShootingClass, encoding_rules$original_value)],
                                                     Team_attributes$chanceCreationShootingClass)

print(Team_attributes$chanceCreationShootingClass)



######################
encoding_rules <- list(
  original_value = c("Organised","Free Form"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$chanceCreationPositioningClass<- ifelse(Team_attributes$chanceCreationPositioningClass %in% encoding_rules$original_value,
                                                        encoding_rules$new_value[match(Team_attributes$chanceCreationPositioningClass, encoding_rules$original_value)],
                                                        Team_attributes$chanceCreationPositioningClass)

print(Team_attributes$chanceCreationPositioningClass)



#################

encoding_rules <- list(
  original_value = c("Deep","Medium","High"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$defencePressureClass<- ifelse(Team_attributes$defencePressureClass %in% encoding_rules$original_value,
                                              encoding_rules$new_value[match(Team_attributes$defencePressureClass, encoding_rules$original_value)],
                                              Team_attributes$defencePressureClass)

print(Team_attributes$defencePressureClass)

#################

encoding_rules <- list(
  original_value = c("Contain","Press","Double"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$defenceAggressionClass<- ifelse(Team_attributes$defenceAggressionClass %in% encoding_rules$original_value,
                                                encoding_rules$new_value[match(Team_attributes$defenceAggressionClass, encoding_rules$original_value)],
                                                Team_attributes$defenceAggressionClass)

print(Team_attributes$defenceAggressionClass)



###################
encoding_rules <- list(
  original_value = c("Narrow","Normal","Wide"),
  new_value = c("0", "1","2")
)


# Encode the particular values in the specified column
Team_attributes$defenceTeamWidthClass<- ifelse(Team_attributes$defenceTeamWidthClass %in% encoding_rules$original_value,
                                               encoding_rules$new_value[match(Team_attributes$defenceTeamWidthClass, encoding_rules$original_value)],
                                               Team_attributes$defenceTeamWidthClass)

print(Team_attributes$defenceTeamWidthClass)



################

encoding_rules <- list(
  original_value = c("Cover","Offside Trap"),
  new_value = c("0","1")
)


# Encode the particular values in the specified column
Team_attributes$defenceDefenderLineClass<- ifelse(Team_attributes$defenceDefenderLineClass %in% encoding_rules$original_value,
                                                  encoding_rules$new_value[match(Team_attributes$defenceDefenderLineClass, encoding_rules$original_value)],
                                                  Team_attributes$defenceDefenderLineClass)

print(Team_attributes$defenceDefenderLineClass)
write.csv(Team_attributes, file = "Team_attributes.csv", row.names = FALSE)
print(Team_attributes)

library(dplyr)
colSums(is.na(Team_attributes))
# Function to calculate mode
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Install and load the DescTools package for the Mode function
install.packages("DescTools")
library(DescTools)

# Replace missing values with mean for numeric columns and mode for non-numeric columns
Team_attributes <- Team_attributes %>%
  mutate_at(vars(-date), funs(if (is.numeric(.)) ifelse(is.na(.), mean(., na.rm = TRUE), .) else ifelse(is.na(.), Mode(., na.rm = TRUE), .)))
colSums(is.na(Team_attributes))
# Check the result
View(Team_attributes)

#################################################################################################
#TEAM'S OVERALL PERFORMANCE
#################################################################################################


library(dplyr)

team_data <- Team_attributes %>%
  group_by(team_api_id) %>%
  mutate(
    average_buildUpPlaySpeed = mean(c(buildUpPlaySpeed, as.numeric(buildUpPlaySpeedClass))),
    average_buildUpPlayDribbling = mean(c(buildUpPlayDribbling, as.numeric(buildUpPlayDribblingClass))),
    average_buildUpPlayPassing = mean(c(buildUpPlayPassing, as.numeric(buildUpPlayPassingClass))),
    average_chanceCreation = mean(c(
      chanceCreationPassing, as.numeric(chanceCreationPassingClass),
      chanceCreationCrossing, as.numeric(chanceCreationCrossingClass),
      chanceCreationShooting, as.numeric(chanceCreationShootingClass),
      as.numeric(chanceCreationPositioningClass)
    )),
    average_defence = mean(c(
      defencePressure, as.numeric(defencePressureClass),
      defenceAggression, as.numeric(defenceAggressionClass),
      defenceTeamWidth, as.numeric(defenceTeamWidthClass),
      as.numeric(defenceDefenderLineClass)
    )),
    overall_performance = mean(c(
      # average_buildUpPlaySpeed,
      average_buildUpPlayDribbling,
      average_buildUpPlayPassing,
      average_chanceCreation,
      average_defence
    ), na.rm = TRUE)
  ) %>%
  ungroup()  # This removes the grouping, giving you an ungrouped data frame



#print(team_data[, c("team_api_id", "average_buildUpPlaySpeed", "average_buildUpPlayDribbling", "average_buildUpPlayPassing", "average_chanceCreation", "average_defence", "overall_performance")])
# Assuming your data is in a CSV file named "performance_data.csv"
performance_data <- team_data[, c("team_api_id", "overall_performance")]
performance_data <- na.omit(performance_data)
performance_data <- distinct(performance_data, team_api_id, .keep_all = TRUE)
print(unique(performance_data$team_api_id))

# Check the structure of your data
str(performance_data)

#barplot(performance_data$overall_performance, names.arg = performance_data$team_api_id,
#        main = "Team Performance", xlab = "Team API ID", ylab = "Overall Performance",
#       col = "lightblue", space = 1)

barplot(performance_data$overall_performance, names.arg = performance_data$team_api_id,
        main = "Team Performance", xlab = "Team API ID", ylab = "Overall Performance",
        col = "lightblue", space = 10, las = 2) 


max_performance_team <- performance_data[which.max(performance_data$overall_performance), ]
min_performance_team <- performance_data[which.min(performance_data$overall_performance), ]

# Print the results
cat("Team with the Highest Performance:\n")
print(max_performance_team)

cat("\nTeam with the Lowest Performance:\n")
print(min_performance_team)
# Write to CSV
write.csv(performance_data, file = "team_performance_3.csv", row.names = FALSE)
# Create a scatter plot for overall performance of each team
ggplot(team_data, aes(x = team_api_id, y = overall_performance)) +
  geom_point() +
  labs(title = "Team Overall Performance Scatter Plot", x = "Team API ID", y = "Overall Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle for better readability

#################################################################################################
#CHECKING FOR OUTLIERS IN TEAM'S OVERALL PERFORMANCE
#################################################################################################

# Load required libraries
library(ggplot2)

# Create a boxplot for overall performance
ggplot(team_data, aes(x = factor(team_api_id), y = overall_performance)) +
  geom_boxplot() +
  labs(title = "Team Overall Performance Boxplot", x = "Team API ID", y = "Overall Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle for better readability
team_data
outliers <- boxplot.stats(team_data$overall_performance)$out

# Display the outliers
cat("Outliers:")
print(outliers)

# Remove outliers
team_data_no_outliers <- team_data[!team_data$overall_performance %in% outliers, ]

# Write to CSV without outliers
write.csv(team_data_no_outliers, file = "team_performance_no_outliers.csv", row.names = FALSE)

#################################################################################################
#TEAM'S RECENT PERFORMANCE
#################################################################################################

#################
recent_team_performance <- Team_attributes %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y %H:%M")) %>%
  group_by(team_api_id) %>%
  filter(date == max(date)) %>%
  ungroup()
write.csv(recent_team_performance, "latest_team_data_new.csv", row.names = FALSE)


recent_team_performance_data <- recent_team_performance %>%
  group_by(team_api_id) %>%
  mutate(
    average_buildUpPlaySpeed = mean(c(buildUpPlaySpeed, as.numeric(buildUpPlaySpeedClass))),
    average_buildUpPlayDribbling = mean(c(buildUpPlayDribbling, as.numeric(buildUpPlayDribblingClass))),
    average_buildUpPlayPassing = mean(c(buildUpPlayPassing, as.numeric(buildUpPlayPassingClass))),
    average_chanceCreation = mean(c(
      chanceCreationPassing, as.numeric(chanceCreationPassingClass),
      chanceCreationCrossing, as.numeric(chanceCreationCrossingClass),
      chanceCreationShooting, as.numeric(chanceCreationShootingClass),
      as.numeric(chanceCreationPositioningClass)
    )),
    average_defence = mean(c(
      defencePressure, as.numeric(defencePressureClass),
      defenceAggression, as.numeric(defenceAggressionClass),
      defenceTeamWidth, as.numeric(defenceTeamWidthClass),
      as.numeric(defenceDefenderLineClass)
    )),
    overall_performance_latest = mean(c(
      # average_buildUpPlaySpeed,
      average_buildUpPlayDribbling,
      average_buildUpPlayPassing,
      average_chanceCreation,
      average_defence
    ), na.rm = TRUE)
  ) %>%
  ungroup()  # This removes the grouping, giving you an ungrouped data frame


#print(team_data[, c("team_api_id", "average_buildUpPlaySpeed", "average_buildUpPlayDribbling", "average_buildUpPlayPassing", "average_chanceCreation", "average_defence", "overall_performance")])
# Assuming your data is in a CSV file named "performance_data.csv"
latest_performance_data <- recent_team_performance_data[, c("team_api_id", "overall_performance_latest")]
latest_performance_data <- na.omit(latest_performance_data)
latest_performance_data <- distinct(latest_performance_data, team_api_id, .keep_all = TRUE)

# Check the structure of your data
str(latest_performance_data)

#barplot(performance_data$overall_performance, names.arg = performance_data$team_api_id,
#        main = "Team Performance", xlab = "Team API ID", ylab = "Overall Performance",
#       col = "lightblue", space = 1)
summary(latest_performance_data$overall_performance_latest)
summary(latest_performance_data$team_api_id)

barplot(latest_performance_data$overall_performance_latest , names.arg = latest_performance_data$team_api_id,
        main = "Team latest Performance", xlab = "Team API ID", ylab = "Overall Performance",
        col = "lightblue", space = 10, las = 2) 


max_performance_team_latest <- latest_performance_data[which.max(latest_performance_data$overall_performance_latest ), ]
min_performance_team_latest <- latest_performance_data[which.min(latest_performance_data$overall_performance_latest ), ]

cat("Team with the Highest Performance:\n")
print(max_performance_team_latest)

cat("\nTeam with the Lowest Performance:\n")
print(min_performance_team_latest)

ggplot(latest_performance_data, aes(x = team_api_id, y = overall_performance_latest)) +
  geom_point() +
  labs(title = "Team Overall Performance Scatter Plot", x = "Team API ID", y = "Overall Performance latest") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the angle for better readability# Adjust the angle for better readability

individual_data <- recent_team_performance_data

individual_data$totalBuildUpPlay <- rowSums(individual_data[, c("buildUpPlaySpeed", "buildUpPlayDribbling", "buildUpPlayPassing")])
individual_data$totalChanceCreation <- rowSums(individual_data[, c("chanceCreationPassing", "chanceCreationCrossing", "chanceCreationShooting")])
individual_data$totalDefense <- rowSums(individual_data[, c("defencePressure", "defenceAggression", "defenceTeamWidth")])

# Identify the best team in each category
best_buildup_team <- individual_data[which.max(individual_data$totalBuildUpPlay), "team_api_id"][[1]]
best_chance_team <- individual_data[which.max(individual_data$totalChanceCreation), "team_api_id"][[1]]
best_defense_team <- individual_data[which.max(individual_data$totalDefense), "team_api_id"][[1]]

# Display the results
cat("Best team in build-up play:", best_buildup_team, "\n")
cat("Best team in chance creation:", best_chance_team, "\n")
cat("Best team in defense:", best_defense_team, "\n")
#########################################################################
