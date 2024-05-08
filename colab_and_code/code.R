
library(tidyverse)
library(here)
library(maps)
library(plotly)
library(broom)
library(colorspace)
world_df <- map_data("world")

electricity_access <- read_csv("data/access_to_electricity/access_to_electricity.csv", 
                               skip = 4) |>
  select(-c(3:44), -c(67:69))

countries <- c("Afghanistan", "Angola", "Bangladesh", "Benin", "Burkina Faso", "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros", "Congo, Dem. Rep.", "Djibouti", "Eritrea", "Ethiopia", "Gambia, The", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", "Lao PDR", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar", "Nepal", "Niger", "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan", "Tanzania", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "Yemen, Rep.", "Zambia")

electricity_stats <- electricity_access |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Electricity Access") |>
  filter(!is.na(`Electricity Access`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_elec_access` = mean(`Electricity Access`, na.rm = TRUE),
            `sd_elec_access` = sd(`Electricity Access`, na.rm = TRUE))

agricultural_land <- read_csv("data/agricultural_land/agricultural_land.csv", 
                              skip = 4) |>
  select(-c(3:44), -c(67:69))

agriculture_land_stats <- agricultural_land |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Agricultural Land") |>
  filter(!is.na(`Agricultural Land`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_agr_land` = mean(`Agricultural Land`, na.rm = TRUE),
            `sd_agr_land` = sd(`Agricultural Land`, na.rm = TRUE))

population_growth <- read_csv("~/Desktop/Sixth Semester/ds334_final_project/ds334_final_project/data/population_growth_annual/population_growth.csv", 
                              skip = 4) |>
  select(-c(3:44), -c(67:69))

population_growth_stats <- population_growth |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Population Growth Rate") |>
  filter(!is.na(`Population Growth Rate`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_pop_growth` = mean(`Population Growth Rate`, na.rm = TRUE),
            `sd_pop_growth` = sd(`Population Growth Rate`, na.rm = TRUE))

primary_school_enrol <- read_csv("~/Desktop/Sixth Semester/ds334_final_project/ds334_final_project/data/primary_school_enrollment/primary_school.csv", 
                                 skip = 4) |>
  select(-c(3:44), -c(67:69))

primary_school_enrol_stats <- primary_school_enrol |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Primary School Enrollment Rate") |>
  filter(!is.na(`Primary School Enrollment Rate`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_prim_school` = mean(`Primary School Enrollment Rate`, na.rm = TRUE),
            `sd_prim_school` = sd(`Primary School Enrollment Rate`,na.rm = TRUE))

total_unemployment <- read_csv("~/Desktop/Sixth Semester/ds334_final_project/ds334_final_project/data/total_unemployment/total_unemployment.csv", 
                               skip = 4) |>
  select(-c(3:44), -c(67:69))

total_unemployment_stats <- total_unemployment |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Total Unemployment") |>
  filter(!is.na(`Total Unemployment`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_total_unempl` = mean(`Total Unemployment`, na.rm = TRUE),
            `sd_total_unempl` = sd(`Total Unemployment`, na.rm = TRUE))

sanitation <- read_csv("data/basic_sanitation_services/basic_sanitation.csv", 
                       skip = 4) |>
  select(-c(3:44), -c(67:69))

sanitation_stats <- sanitation |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Sanitation") |>
  filter(!is.na(`Sanitation`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_sanit` = mean(`Sanitation`, na.rm = TRUE),
            `sd_sanit` = sd(`Sanitation`, na.rm = TRUE))

fertility_rate <- read_csv("data/fertility_rate/fertility_rate.csv", 
                           skip = 4) |>
  select(-c(3:44), -c(67:69))

fertility_rate_stats <- fertility_rate |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Fertility Rate") |>
  filter(!is.na(`Fertility Rate`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_fert_rate` = mean(`Fertility Rate`, na.rm = TRUE),
            `sd_fert_rate` = sd(`Fertility Rate`, na.rm = TRUE))

internet <- read_csv("data/internet/internet.csv", 
                     skip = 4) |>
  select(-c(3:44), -c(67:69))

internet_stats <- internet |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Internet") |>
  filter(!is.na(`Internet`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_inter` = mean(`Internet`, na.rm = TRUE),
            `sd_inter` = sd(`Internet`, na.rm = TRUE))

birth_life_exp <- read_csv("data/life_expectancy_birth/life_expectancy_birth.csv", 
                           skip = 4) |>
  select(-c(3:44), -c(67:69))

birth_life_exp_stats <- birth_life_exp |>
  pivot_longer(c(3:24), 
               names_to = "Year", 
               values_to = "Life Expectancy at Birth") |>
  filter(!is.na(`Life Expectancy at Birth`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_life_exp` = mean(`Life Expectancy at Birth`, na.rm = TRUE),
            `sd_life_exp` = sd(`Life Expectancy at Birth`, na.rm = TRUE))

full_stats_df <- agriculture_land_stats |>
  left_join(birth_life_exp_stats, by = "Country Name") |>
  left_join(electricity_stats, by = "Country Name") |>
  left_join(fertility_rate_stats, by = "Country Name") |>
  left_join(internet_stats, by = "Country Name") |>
  left_join(sanitation_stats, by = "Country Name") |>
  left_join(population_growth_stats, by = "Country Name") |>
  left_join(primary_school_enrol_stats, by = "Country Name") |>
  left_join(total_unemployment_stats, by = "Country Name") 

full_stats_df <- 
  full_stats_df |>
  mutate(Underdeveloped = ifelse(`Country Name` %in% countries, 1, 0))

library(GGally)
ggpairs(data = full_stats_df, columns = c(2, 4, 6, 8, 10, 12, 14, 16, 18))

# Get the means column for each variable
full_means_df <- full_stats_df |>
  select(contains("mean"))

numeric_columns <- sapply(full_means_df, is.numeric)

scaled_data <- scale(full_means_df[, numeric_columns])

scaled_data_with_undev <- scaled_data |>
  as.data.frame() |>
  mutate(Underdeveloped = full_stats_df$Underdeveloped) |>
  na.omit()

median_agr_land = median(scaled_data_with_undev$mean_agr_land, na.rm = TRUE)
median_life_exp = median(scaled_data_with_undev$mean_life_exp, na.rm = TRUE)
median_elec_access = median(scaled_data_with_undev$mean_elec_access, na.rm = TRUE)
median_fert_rate = median(scaled_data_with_undev$mean_fert_rate, na.rm = TRUE)
median_sanit = median(scaled_data_with_undev$mean_sanit, na.rm = TRUE)
median_population_growth = median(scaled_data_with_undev$mean_pop_growth, na.rm = TRUE)
median_primary_school = median(scaled_data_with_undev$mean_prim_school, na.rm = TRUE)
median_total_unempl = median(scaled_data_with_undev$mean_total_unempl, na.rm = TRUE)
median_internet = median(scaled_data_with_undev$mean_inter, na.rm = TRUE)

library(modelr)
## First Attempt
# Fit the logistic regression model with all the variables
model_glm <- glm(Underdeveloped ~ .,
                 data = scaled_data_with_undev, family = "binomial")

# Check the summary of the model
summary(model_glm)

# Identify numeric columns
numeric_cols <- sapply(scaled_data_with_undev, is.numeric)

# Compute range for each numeric column, removing NA values
ranges <- sapply(scaled_data_with_undev[, numeric_cols], function(x) {
  x <- na.omit(x)
  c(min(x), max(x))
})

## Second Attempt
# Fit the logistic regression model with selected variables using their range and the median for the rest
# Used the first model to come up with the most "significant" ones

model_glm <- glm(Underdeveloped ~ .,
                 data = scaled_data_with_undev, family = "binomial")

# Create the grid for prediction
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = median_agr_land,
    mean_life_exp = seq_range(c(-2.627905, 1.588857), n = 20),
    mean_elec_access = seq_range(c(-2.6785616, 0.7295024), n = 5),
    mean_fert_rate = seq_range(c(-1.274220, 3.082338), n = 20),
    mean_sanit = median_sanit, 
    mean_inter = median_internet,
    mean_pop_growth = median_population_growth,
    mean_prim_school = median_primary_school, 
    mean_total_unempl = median_total_unempl
  )

# Predict probabilities that a country is Underdeveloped
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_fert_rate, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_life_exp, 2)))) +
  facet_wrap(~as.factor(round(mean_elec_access, 2))) +
  labs(x = "Standardized Mean Fertility Rate", y = "Predicted Probability", color = "Standardized Mean Electricity Access") +
  theme_minimal()

## Third Attempt
# Fit the logistic regression model with selected variables using their range and the median for the rest
# Used the first model to include the ones that were significant but not as much

grid <- full_stats_df |>
  data_grid(
    mean_agr_land = median_agr_land,
    mean_life_exp = seq_range(c(-2.627905, 1.588857), n = 10),
    mean_elec_access = median_elec_access,
    mean_fert_rate = seq_range(c(-1.274220, 3.082338), n = 10),
    mean_sanit = median_sanit, 
    mean_inter = median_internet,
    mean_pop_growth =seq_range(c(-2.022998, 4.438928), n = 3),
    mean_prim_school = median_primary_school, 
    mean_total_unempl = median_total_unempl
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_fert_rate, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_life_exp, 2)))) +
  facet_wrap(~mean_pop_growth) +
  labs(x = "Standardized Mean Fertility Rate", y = "Predicted Probability", color = "Standardized Mean Life Expectancy") +
  theme_minimal()

## Forth Attempt
# Fit the logistic regression model with selected variables using their range and the median for the rest
# Used the first model to include the ones that were significant but not as much

grid <- full_stats_df |>
  data_grid(
    mean_agr_land = median_agr_land,
    mean_life_exp = median_life_exp,
    mean_elec_access = median_elec_access,
    mean_fert_rate = seq_range(c(-1.308798, 2.850897), n = 10),
    mean_sanit = median_sanit, 
    mean_inter = seq_range(c(-1.511381, 2.405766), n = 10),
    mean_pop_growth =seq_range(c(-2.221712, 3.779502), n = 3),
    mean_prim_school = median_primary_school, 
    mean_total_unempl = median_total_unempl
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_fert_rate, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_inter, 2)))) +
  facet_wrap(~mean_pop_growth) +
  labs(x = "Standardized Mean Fertility Rate", y = "Predicted Probability", color = "Standardized Mean Population Growth") +
  theme_minimal()

total_countries <- full_stats_df |>
  select(`Country Name`)

full_means_df <- full_means_df |>
  cbind(total_countries)

total_countries_no_na <- full_means_df |>
  na.omit() |>
  select(`Country Name`)

scaled_data_with_undev_arranged <- scaled_data_with_undev |>
  cbind(total_countries_no_na) |>
  arrange(mean_agr_land)|>
  select(mean_agr_land, `Country Name`)

predictions <- read.csv("data/predicted_dataset.csv")

predictions <- predictions |>
  arrange(mean_agr_land) |>
  cbind(`Country Name` = scaled_data_with_undev_arranged$`Country Name`)

underdeveloped_map <- world_df |>
  mutate(Underdeveloped = as.factor(ifelse(region %in% countries, 1, 0)))

plot_1 <- ggplot()+
  geom_polygon(data = world_df, mapping = aes(x = long, y = lat, group = group, label = region), fill = "grey")+
  geom_polygon(data = underdeveloped_map, mapping = aes(x = long, y = lat, group = group, fill = Underdeveloped, label = region))+
  labs(title = "Actual Representation of Underdeveloped Countries")+
  scale_fill_manual(values = c("0" = "grey", "1" = "darkblue")) + 
  theme_minimal()+
  theme(
    legend.position = "none"
  )

ggplotly(plot_1, tooltip = "label")

predictions <- predictions |>
  mutate(Underdeveloped = ifelse(Predicted_output >= 0.5, 1, 0))

predicted_full_df <- left_join(predictions, world_df, by = c("Country Name" = "region"))

plot_2 <- ggplot()+
  geom_polygon(data = world_df, mapping = aes(x = long, y = lat, group = group, label = region), fill = "grey")+
  geom_polygon(data = predicted_full_df, mapping = aes(x = long, y = lat, group = group, fill = as.factor(Underdeveloped), 
                                                       label = `Country Name`))+
  labs(title = "Representation of Underdeveloped Countries using Predictions of Neural Network")+
  scale_fill_manual(values = c("0" = "grey", "1" = "darkred")) + 
  theme_minimal()+
  theme(
    legend.position = "none"
  )

ggplotly(plot_2, tooltip = "label")

summary_actual <- scaled_data_with_undev |>
  summarise(Underdeveloped = sum(Underdeveloped), 
            Developed = n() - Underdeveloped)

summary_predicted <- predictions |>
  summarise(Underdeveloped = sum(Underdeveloped), 
            Developed = n() - Underdeveloped)

library(pander)

# Convert summaries to data frames
summary_actual_df <- as.data.frame(summary_actual)
summary_predicted_df <- as.data.frame(summary_predicted)

# Add row names for clarity
row.names(summary_actual_df) <- "Actual"
row.names(summary_predicted_df) <- "Predicted"

combined_summary <- rbind(summary_actual_df, summary_predicted_df)

# Print combined summary using pander
pander(combined_summary, caption = "Summary of Actual and Predicted Classes")












