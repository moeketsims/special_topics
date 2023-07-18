library(tidyverse)
library(lubridate)
library(tidymodels)
library(vip)

input_data <- read.csv("data.csv", sep =";") |> tibble()
df_train <- input_data |> pivot_longer(cols = starts_with("MO"),
                           names_to = "DATE",
                           values_to = "sales")|>
                          separate(DATE, into = c("Preffix", "Month", "Year"), sep = c(2, 4))|>
                          mutate(date = paste(Year, Month, "01", sep = "-"))|>
                          mutate(date = as.Date(date, format="%Y-%m-%d"))|>
                          select(-c(Preffix, Month, Year))|>filter(H03=="FMP24000")

p <- df_train |>
    ggplot(aes(date, sales))+
    geom_point()

ggsave("lineplot.png", p, dpi = 120)


# split data into test and training
data_split <- initial_split(df_train, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)


# specify the model
rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger", importance = 'impurity') %>%
  set_mode("regression")

# create a recipe
recipe <- recipe(sales ~ ., data = train_data)
# workflow
workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(recipe)

# fit the model
rf_fit <- fit(workflow, data = train_data)
# make predictions
predictions <- predict(rf_fit, new_data = test_data)|>
                bind_cols(test_data %>% select(sales))

# assess model performance
metrics <- metrics(predictions, truth = sales, estimate = .pred)

# Extract importance from the fit
ranger_model <- extract_fit_parsnip(rf_fit)$fit
importance <- ranger_model$variable.importance
importance_tidy <- enframe(importance, name = "Feature", value = "Importance")
ggplot(importance_tidy, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Feature Importance",
       x = "Feature",
       y = "Importance")
# Save the plot
ggsave("feature_importance.png", dpi = 120)