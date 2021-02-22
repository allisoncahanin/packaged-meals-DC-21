library(tidyverse)
library(tidytext)

meals <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/food_nutrient.csv", header = T)

id_list <- meals_wide_df$fdc_id
id_list

nutrient_subset <- meals[meals$fdc_id == id_list,]

nutrient_wide_df <- spread(meals, key = nutrient_id, value = amount, fill = 0)

