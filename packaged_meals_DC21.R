library(tidyverse)
library(tidytext)

meals <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/Data_Lv2_USDA_PackagedMeals.csv")


meals_df <- data_frame(meals)

meals_unnested <- meals_df %>%
  mutate(ingredients = str_to_lower(ingredients),
         ingredients = str_replace_all(ingredients, "\\(", ","), #replacing delimiters
         ingredients = str_replace_all(ingredients, "\\)", ","),
         ingredients = str_replace_all(ingredients, ":", ","),
         ingredients = str_replace_all(ingredients, ",,", ","))


meals_unnested <- unnest_regex(meals_unnested, #unnesting using comma as delimiter
               input= ingredients, 
               output = ingredient, 
               pattern = ",")

ingredients_sorted <- meals_unnested %>% 
  count(ingredient,sort = T) %>%
  filter(n > 20)
         
meals_unnested <- unnest_regex(meals_unnested, #unnesting using comma as delimiter
                               input= ingredients, 
                               output = ingredient, 
                               pattern = ",")

#need to remove periods and [ ]
#fix plural ingredients like carrot/carrots
#maybe case_when to change less than 2% salt to salt



