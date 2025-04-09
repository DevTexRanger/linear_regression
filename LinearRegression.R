# Texas County-Level Linear Regression Model using tidycensus
# Examining relationship between educational attainment and median household income

# Install required packages (if needed)
# install.packages(c("tidycensus", "tidyverse", "broom", "sf", "ggthemes", "pak"))

# Load required libraries
library(tidycensus)
library(tidyverse)
library(broom)
library(sf)
library(ggthemes)
library(pak)

# Set Census API key
# Uncomment and replace with your key
# census_api_key("YOUR_API_KEY_HERE", install = TRUE)

# Step 1: Retrieve Census data
# Getting median household income data
# Step 1a: Temporary fix due to Census Bureau rejecting calls to the API from https://www2.census.gov/geo/tiger/
# This installs a temporary development version of tigris from Kyle Walker’s GitHub with the FTP fix applied.
pak::pak("walkerke/tigris@ftp-patch")

# Re-run Step 1
income_data <- get_acs(
  geography = "county",
  variables = "B19013_001", # Median household income
  state = "TX",
  year = 2021,
  geometry = TRUE
)

# Getting educational attainment data (percent with bachelor's degree or higher)
education_data <- get_acs(
  geography = "county",
  variables = "DP02_0068P", # Percent with Bachelor's degree or higher
  state = "TX",
  year = 2021
)

# Step 2: Prepare the data for analysis
# Rename variables to be more descriptive
income_data <- income_data %>%
  rename(median_income = estimate,
         income_moe = moe) %>%
  select(GEOID, NAME, median_income, income_moe, geometry)

education_data <- education_data %>%
  rename(pct_bachelors_plus = estimate,
         education_moe = moe) %>%
  select(GEOID, NAME, pct_bachelors_plus, education_moe)

# Merge datasets
tx_data <- left_join(income_data, education_data, by = c("GEOID", "NAME"))

# Step 3: Explore the data
# Summary statistics
summary(tx_data$median_income)
summary(tx_data$pct_bachelors_plus)

# Check for missing values
sum(is.na(tx_data$median_income))
sum(is.na(tx_data$pct_bachelors_plus))

# Step 4: Visualize the relationship
ggplot(tx_data, aes(x = pct_bachelors_plus, y = median_income)) +
  geom_point(aes(size = income_moe), alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Educational Attainment vs. Median Household Income",
    subtitle = "Texas Counties, ACS 2017-2021",
    x = "Percent with Bachelor's Degree or Higher",
    y = "Median Household Income",
    size = "Income MOE"
  ) +
  theme_clean() +
  theme(plot.title = element_text(face = "bold"))

# Step 5: Build the linear regression model
model <- lm(median_income ~ pct_bachelors_plus, data = tx_data)

# Examine model summary
summary(model)

# Step 6: Tidy model results with broom
model_results <- tidy(model)
model_fit <- glance(model)

print(model_results)
print(model_fit)

# Step 7: Plot the regression diagnostics
par(mfrow = c(2, 2))
plot(model)

# Step 8: Create spatial visualization of residuals
# First check for NA values that might be causing issues
sum(is.na(tx_data$median_income))
sum(is.na(tx_data$pct_bachelors_plus))

# Create a complete cases dataset for the model
tx_data_complete <- tx_data %>%
  filter(!is.na(median_income) & !is.na(pct_bachelors_plus))

# Rebuild the model with only complete cases to ensure consistency
model <- lm(median_income ~ pct_bachelors_plus, data = tx_data_complete)

# Generate predictions and residuals
tx_data_complete$predicted <- predict(model, tx_data_complete)
tx_data_complete$residuals <- residuals(model)

# Map of residuals
ggplot(tx_data_complete) +
  geom_sf(aes(fill = residuals), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "red", 
    mid = "white", 
    high = "blue", 
    midpoint = 0,
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Regression Residuals by County",
    subtitle = "Positive values (blue) indicate income higher than predicted",
    fill = "Residuals"
  ) +
  theme_map() +
  theme(legend.position = "right")

# Step 9: Identify counties with large residuals
# Counties where actual income is much higher than predicted
high_residual <- tx_data_complete %>%
  arrange(desc(residuals)) %>%
  head(5) %>%
  select(NAME, median_income, pct_bachelors_plus, residuals)

# Counties where actual income is much lower than predicted
low_residual <- tx_data_complete %>%
  arrange(residuals) %>%
  head(5) %>%
  select(NAME, median_income, pct_bachelors_plus, residuals)

print("Counties with income much higher than model prediction:")
print(high_residual)

print("Counties with income much lower than model prediction:")
print(low_residual)

# Step 10: Additional analysis - population and urbanization
# This would be a good next step to improve the model
# Example: Get population data to use as control variable
pop_data <- get_acs(
  geography = "county",
  variables = "B01003_001", # Total population
  state = "TX",
  year = 2021
)

pop_data <- pop_data %>%
  rename(population = estimate) %>%
  select(GEOID, population)

# Join with main dataset
tx_data_complete <- left_join(tx_data_complete, pop_data, by = "GEOID")

# Log transform population for better distribution
tx_data_complete$log_pop <- log(tx_data_complete$population)

# Build multiple regression model with education and population
model2 <- lm(median_income ~ pct_bachelors_plus + log_pop, data = tx_data_complete)

# Compare models
summary(model)  # Original model
summary(model2) # Model with population control

# Calculate AIC to compare models
AIC(model, model2)

# Generate residuals for the second model
tx_data_complete$predicted_model2 <- predict(model2, tx_data_complete)
tx_data_complete$residuals_model2 <- residuals(model2)

# Create a residuals map for the second model
ggplot(tx_data_complete) +
  geom_sf(aes(fill = residuals_model2), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "red", 
    mid = "white", 
    high = "blue", 
    midpoint = 0,
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Regression Residuals by County (Education + Population Model)",
    subtitle = "Positive values (blue) indicate income higher than predicted by education & population",
    fill = "Residuals"
  ) +
  theme_map() +
  theme(legend.position = "right")

# Identify counties with large residuals in the second model
# Counties where actual income is much higher than predicted
high_residual_model2 <- tx_data_complete %>%
  arrange(desc(residuals_model2)) %>%
  head(5) %>%
  select(NAME, median_income, pct_bachelors_plus, population, residuals_model2)

# Counties where actual income is much lower than predicted
low_residual_model2 <- tx_data_complete %>%
  arrange(residuals_model2) %>%
  head(5) %>%
  select(NAME, median_income, pct_bachelors_plus, population, residuals_model2)

print("Counties with income much higher than model 2 prediction (education + population):")
print(high_residual_model2)

print("Counties with income much lower than model 2 prediction (education + population):")
print(low_residual_model2)

# Compare the two models' residuals - which counties changed categories?
# Create a comparative dataset
tx_data_complete <- tx_data_complete %>%
  mutate(
    residual_difference = residuals_model2 - residuals,
    category_change = case_when(
      sign(residuals) != sign(residuals_model2) ~ "Changed direction",
      abs(residual_difference) > 5000 ~ "Major change",
      abs(residual_difference) > 2000 ~ "Moderate change",
      TRUE ~ "Minor change"
    )
  )

# Counties that changed the most between models
changed_counties <- tx_data_complete %>%
  arrange(desc(abs(residual_difference))) %>%
  head(10) %>%
  select(NAME, median_income, pct_bachelors_plus, population, 
         residuals, residuals_model2, residual_difference, category_change)

print("Counties with the biggest change in residuals after accounting for population:")
print(changed_counties)
