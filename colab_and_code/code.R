library(tidyverse)
library(here)
library(maps)
library(plotly)
library(broom)
library(colorspace)
world_df <- map_data("world")

ranges <- sapply(full_stats_df[, numeric_cols], function(x) {
  x <- na.omit(x)
  c(min(x), max(x))
})

replace_na_with_median <- function(df, column_name) {
  median_val <- median(df[[column_name]], na.rm = TRUE)
  df[[column_name]][is.na(df[[column_name]])] <- median_val
  return(df)
}

electricity_access <- read_csv("data/access_to_electricity/access_to_electricity.csv", 
                               skip = 4) |>
  select(-c(3:34), -c(67:69))

countries <- c("Afghanistan", "Angola", "Bangladesh", "Benin", "Burkina Faso", "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros", "Congo, Dem. Rep.", "Djibouti", "Eritrea", "Ethiopia", "Gambia, The", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", "Lao PDR", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar", "Nepal", "Niger", "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan", "Tanzania", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "Yemen, Rep.", "Zambia")

electricity_stats <- electricity_access |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Electricity Access") |>
  filter(!is.na(`Electricity Access`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_under_elec` = mean(`Electricity Access`, na.rm = TRUE),
            `sd_under_elec` = sd(`Electricity Access`, na.rm = TRUE))

underdeveloped_electricity <- electricity_access |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Electricity Access") |>
  filter(!is.na(`Electricity Access`)) 

underdeveloped_map <- left_join(underdeveloped_electricity, world_df, by = c("Country Name"="region"))

plot_1 <- ggplot()+
  geom_polygon(data = world_df, mapping = aes(x = long, y = lat, group = group, label = region), fill = "grey")+
  geom_polygon(data = underdeveloped_map, mapping = aes(x = long, y = lat, group = group, fill = `Electricity Access`, label = `Country Name`))+
  scale_fill_continuous_sequential(palette = "Heat")+
  theme_minimal()

ggplotly(plot_1, tooltip = "label")

agricultural_land <- read_csv("data/agricultural_land/agricultural_land.csv", 
                              skip = 4) |>
  select(-c(3:34), -c(67:69)) 

agriculture_land_stats <- agricultural_land |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Agricultural Land") |>
  filter(!is.na(`Agricultural Land`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_agr_land` = mean(`Agricultural Land`, na.rm = TRUE),
            `sd_agr_land` = sd(`Agricultural Land`, na.rm = TRUE))

freshwater_withdrawals <- read_csv("data/annual_freshwater_withdrawals/freshwater_withdrawals.csv", 
                                   skip = 4) |>
  select(-c(3:34), -c(67:69))

freshwater_withdrawals_stats <- freshwater_withdrawals |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Freshwater Withdrawals") |>
  filter(!is.na(`Freshwater Withdrawals`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_water_withd` = mean(`Freshwater Withdrawals`, na.rm = TRUE),
            `sd_water_withd` = sd(`Freshwater Withdrawals`, na.rm = TRUE))

atms <- read_csv("data/atms/atms.csv", 
                 skip = 4) |>
  select(-c(3:34), -c(67:69))

atms_stats <- atms |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "ATMs") |>
  filter(!is.na(`ATMs`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_atms` = mean(`ATMs`, na.rm = TRUE),
            `sd_atms` = sd(`ATMs`,na.rm = TRUE))

precipitation <- read_csv("data/avg_precipitation/precipitation_depth.csv", 
                          skip = 4) |>
  select(-c(3:34), -c(67:69))

precipitation_stats <- precipitation |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Precipitation") |>
  filter(!is.na(`Precipitation`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_precip` = mean(`Precipitation`, na.rm = TRUE),
            `sd_precip` = sd(`Precipitation`, na.rm = TRUE))

sanitation <- read_csv("data/basic_sanitation_services/basic_sanitation.csv", 
                       skip = 4) |>
  select(-c(3:34), -c(67:69))

sanitation_stats <- sanitation |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Sanitation") |>
  filter(!is.na(`Sanitation`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_sanit` = mean(`Sanitation`, na.rm = TRUE),
            `sd_sanit` = sd(`Sanitation`, na.rm = TRUE))

broad_money <- read_csv("data/broad_money/broad_money.csv", 
                        skip = 4) |>
  select(-c(3:34), -c(67:69))

broad_money_stats <- broad_money |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Broad Money") |>
  filter(!is.na(`Broad Money`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_broad` = mean(`Broad Money`, na.rm = TRUE),
            `sd_broad` = sd(`Broad Money`, na.rm = TRUE))

agriculture_employment <- read_csv("data/employment_in_agr/employment_in_agr.csv", 
                                   skip = 4) |>
  select(-c(3:34), -c(67:69))

agriculture_employment_stats <- agriculture_employment |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Employment in Agriculture") |>
  filter(!is.na(`Employment in Agriculture`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_empl_agr` = mean(`Employment in Agriculture`, na.rm = TRUE),
            `sd_empl_agr` = sd(`Employment in Agriculture`, na.rm = TRUE))

fertility_rate <- read_csv("data/fertility_rate/fertility_rate.csv", 
                           skip = 4) |>
  select(-c(3:34), -c(67:69))

fertility_rate_stats <- fertility_rate |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Fertility Rate") |>
  filter(!is.na(`Fertility Rate`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_fert_rate` = mean(`Fertility Rate`, na.rm = TRUE),
            `sd_fert_rate` = sd(`Fertility Rate`, na.rm = TRUE))

gov_debt <- read_csv("data/gov_debt/central_gov_debt.csv", 
                     skip = 4) |>
  select(-c(3:34), -c(67:69))

gov_debt_stats <- gov_debt |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Government Debt") |>
  filter(!is.na(`Government Debt`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_gov_debt` = mean(`Government Debt`, na.rm = TRUE),
            `sd_gov_debt` = sd(`Government Debt`, na.rm = TRUE))

internet <- read_csv("data/internet/internet.csv", 
                     skip = 4) |>
  select(-c(3:34), -c(67:69))

internet_stats <- internet |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Internet") |>
  filter(!is.na(`Internet`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_inter` = mean(`Internet`, na.rm = TRUE),
            `sd_inter` = sd(`Internet`, na.rm = TRUE))

birth_life_exp <- read_csv("data/life_expectancy_birth/life_expectancy_birth.csv", 
                           skip = 4) |>
  select(-c(3:34), -c(67:69))

birth_life_exp_stats <- birth_life_exp |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Life Expectancy at Birth") |>
  filter(!is.na(`Life Expectancy at Birth`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_life_exp` = mean(`Life Expectancy at Birth`, na.rm = TRUE),
            `sd_life_exp` = sd(`Life Expectancy at Birth`, na.rm = TRUE))

poverty <- read_csv("data/poverty_headcount/poverty_headcount_ratio.csv", 
                    skip = 4) |>
  select(-c(3:34), -c(67:69))

poverty_stats <- poverty |>
  filter(`Country Name` %in% countries) |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "Poverty") |>
  filter(!is.na(`Poverty`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_pov` = mean(`Poverty`, na.rm = TRUE),
            `sd_pov` = sd(`Poverty`, na.rm = TRUE))

school_enrol <- read_csv("data/school_enrollment/school_enrollment.csv", 
                         skip = 4) |>
  select(-c(3:34), -c(67:69))

school_enrol_stats <- school_enrol |>
  filter(`Country Name` %in% countries) |>
  pivot_longer(c(3:34), 
               names_to = "Year", 
               values_to = "School Enrollment") |>
  filter(!is.na(`School Enrollment`)) |>
  group_by(`Country Name`) |>
  summarise(`mean_school_enr` = mean(`School Enrollment`, na.rm = TRUE),
            `sd_school_enr` = sd(`School Enrollment`, na.rm = TRUE))

full_stats_df <- agriculture_land_stats |>
  left_join(agriculture_employment_stats, by = "Country Name") |>
  left_join(atms_stats, by = "Country Name") |>
  left_join(birth_life_exp_stats, by = "Country Name") |>
  left_join(broad_money_stats, by = "Country Name") |>
  left_join(electricity_stats, by = "Country Name") |>
  left_join(fertility_rate_stats, by = "Country Name") |>
  left_join(freshwater_withdrawals_stats, by = "Country Name") |>
  left_join(gov_debt_stats, by = "Country Name") |>
  left_join(internet_stats, by = "Country Name") |>
  left_join(poverty_stats, by = "Country Name") |>
  left_join(precipitation_stats, by = "Country Name") |>
  left_join(sanitation_stats, by = "Country Name") |>
  left_join(school_enrol_stats, by = "Country Name") 

full_stats_df <- 
  full_stats_df |>
  mutate(Underdeveloped = ifelse(`Country Name` %in% countries, 1, 0))

library(GGally)
ggpairs(data = full_stats_df, columns = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28))


#### Logistic Regression Model Attempt

underdeveloped <- full_stats_df |>
  filter(Underdeveloped == 1)

developed <- full_stats_df |>
  filter(Underdeveloped != 1)

# Identify numeric columns
numeric_cols <- sapply(full_stats_df, is.numeric)

median_water_withd = median(full_stats_df$mean_water_withd, na.rm = TRUE)
median_agr_land = median(full_stats_df$mean_agr_land, na.rm = TRUE)
median_empl_agr = median(full_stats_df$mean_empl_agr, na.rm = TRUE)
median_life_exp = median(full_stats_df$mean_life_exp, na.rm = TRUE)
median_under_elec = median(full_stats_df$mean_under_elec, na.rm = TRUE)
median_fert_rate = median(full_stats_df$mean_fert_rate, na.rm = TRUE)
median_water_withd =  median(full_stats_df$mean_water_withd, na.rm = TRUE)
median_gov_debt = median(full_stats_df$mean_gov_debt, na.rm = TRUE)
median_precip = median(full_stats_df$mean_precip, na.rm = TRUE)
median_sanit = median(full_stats_df$mean_sanit, na.rm = TRUE)

library(modelr)
## First Attempt
# Fit the logistic regression model with selected variables
model_glm <- glm(Underdeveloped ~ mean_agr_land + mean_life_exp + mean_under_elec + mean_fert_rate + mean_precip,
                 data = full_stats_df, family = "binomial")

# Check the summary of the model
summary(model_glm)

# Create the grid for prediction
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = seq_range(c(0.529988, 83.730378), n = 3),
    mean_life_exp = seq_range(c(46.14012, 82.19024), n = 3),
    mean_under_elec = median_under_elec,
    mean_fert_rate = median_fert_rate,
    mean_precip = seq_range(c(18.100, 3219.742), n = 3),
    mean_water_withd = median_water_withd,
    mean_sanit = median_sanit
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_life_exp, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_agr_land, 2)))) +
  facet_wrap(~mean_precip)+
  labs(x = "Mean Life Expectancy", y = "Predicted Probability", color = "Mean Agricultural Land")+
  theme_minimal()

## Second Attempt
# Fit the logistic regression model with selected variables
model_glm <- glm(Underdeveloped ~ mean_agr_land + mean_life_exp + mean_under_elec + mean_fert_rate + mean_precip,
                 data = full_stats_df, family = "binomial")

# Create the grid for prediction
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = seq_range(c(0.529988, 83.730378), n = 3),
    mean_life_exp = median_life_exp,
    mean_under_elec = seq_range(c(4.210244, 100), n = 3),
    mean_fert_rate = median_fert_rate,
    mean_precip = seq_range(c(18.100, 3219.742), n = 3),
    mean_water_withd = median_water_withd,
    mean_sanit = median_sanit
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_agr_land, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_under_elec, 2)))) +
  facet_wrap(~mean_precip) +
  labs(x = "Mean Agricultural Land", y = "Predicted Probability", color = "Mean Precipitation Rate") +
  theme_minimal()

# Combination 2
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = median_agr_land,
    mean_life_exp = seq_range(c(46.14012, 82.19024), n = 3),
    mean_under_elec = median_under_elec,
    mean_fert_rate = seq_range(c(1.104000, 7.502063), n = 3),
    mean_precip = median_precip,
    mean_water_withd = seq_range(c(0.02026736, 6495.832000), n = 3),
    mean_sanit = median_sanit
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_life_exp, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_water_withd, 2)))) +
  facet_wrap(~mean_fert_rate) +
  labs(x = "Mean Life Expectancy", y = "Predicted Probability", color = "Mean Fertility Rate") +
  theme_minimal()

# Combination 3
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = seq_range(c(0.529988, 83.730378), n = 3),
    mean_life_exp = median_life_exp,
    mean_under_elec = median_under_elec,
    mean_fert_rate = median_fert_rate,
    mean_precip = seq_range(c(18.100, 3219.742), n = 3),
    mean_water_withd = median_water_withd,
    mean_sanit = seq_range(c(6.029739, 100), n = 3)
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_agr_land, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_sanit, 2)))) +
  facet_wrap(~mean_precip) +
  labs(x = "Mean Agricultural Land", y = "Predicted Probability", color = "Mean Sanitation Rate") +
  theme_minimal()

# Combination 4
grid <- full_stats_df |>
  data_grid(
    mean_agr_land = median_agr_land,
    mean_life_exp = seq_range(c(46.14012, 82.19024), n = 3),
    mean_under_elec = seq_range(c(4.210244, 100), n = 3),
    mean_fert_rate = median_fert_rate,
    mean_precip = seq_range(c(18.100, 3219.742), n = 3),
    mean_water_withd = median_water_withd,
    mean_sanit = median_sanit
  )

# Predict probabilities for the grid
aug_model <- augment(model_glm, newdata = grid, se_fit = TRUE) |>
  mutate(.predprob = plogis(.fitted))

# Visualize the predictions
ggplot(data = aug_model, aes(x = mean_life_exp, y = .predprob)) +
  geom_line(aes(color = as.factor(round(mean_under_elec, 2)))) +
  facet_wrap(~mean_precip) +
  labs(x = "Mean Life Expectancy", y = "Predicted Probability", color = "Mean Electricity Access Rate") +
  theme_minimal()

## In general, what we have seen from the above plots is that attempting to fit a Logistic Regression Model 
## give different combinations of proxies does not do a good job. 
## Therefore, a more complex model might be able to interpret the relationships between the various variables that we have. 

library(keras)
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")

library(reticulate)
py_install("pandas")

# Preprocess the data
full_means_df <- full_stats_df |>
  select(contains("mean")) 

full_means_df <- replace_na_with_median(full_means_df, "mean_agr_land")
full_means_df <- replace_na_with_median(full_means_df, "mean_empl_agr")
full_means_df <- replace_na_with_median(full_means_df, "mean_atms")
full_means_df <- replace_na_with_median(full_means_df, "mean_life_exp")
full_means_df <- replace_na_with_median(full_means_df, "mean_broad")
full_means_df <- replace_na_with_median(full_means_df, "mean_under_elec")
full_means_df <- replace_na_with_median(full_means_df, "mean_fert_rate")
full_means_df <- replace_na_with_median(full_means_df, "mean_water_withd")
full_means_df <- replace_na_with_median(full_means_df, "mean_gov_debt")
full_means_df <- replace_na_with_median(full_means_df, "mean_inter")
full_means_df <- replace_na_with_median(full_means_df, "mean_pov")
full_means_df <- replace_na_with_median(full_means_df, "mean_precip")
full_means_df <- replace_na_with_median(full_means_df, "mean_sanit")
full_means_df <- replace_na_with_median(full_means_df, "mean_school_enr")

scaled_data <- scale(full_means_df[, sapply(full_means_df, is.numeric)])
set.seed(123)

# Split the data into training and testing sets 80% and 20%, as we did in Machine Learning
train_indices <- sample(1:nrow(scaled_data), 0.8 * nrow(scaled_data))
test_indices <- setdiff(1:nrow(scaled_data), train_indices)

train_data <- scaled_data[train_indices, ]
test_data <- scaled_data[test_indices, ]

# Design the network
network = keras_model_sequential(
  layer_dense(units = 4, activation = "relu", input_shape = 14),
  layer_dense(units = 6, activation = "relu"), 
  layer_dense(units = 8, activation = "relu"), 
  layer_dense(units = 1, activation = "sigmoid")
)

network |>
  compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001), 
    metrics = c("accuracy", "loss")
  )
























