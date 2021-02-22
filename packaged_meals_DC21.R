# this is KT testing github connection.
library(tidyverse)
library(tidytext)

meals <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/Data_Lv2_USDA_PackagedMeals.csv")

################################################
###          EXPLORATORY ANALYSIS            ###
################################################

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
         


#need to remove periods and [ ]
#need to remove spaces at the beginning of ingredient names
#fix plural ingredients like carrot/carrots
#maybe case_when to change less than 2% salt to salt

################################################
###             EXPLORATORY PCA              ###
################################################


meals_df <- data_frame(meals)

meals_parsed <- meals_df %>%
  mutate(ingredients = str_to_lower(ingredients),
         ingredients = str_replace_all(ingredients, "\\(", ","), #replacing delimiters
         ingredients = str_replace_all(ingredients, "\\)", ","),
         ingredients = str_replace_all(ingredients, ":", ","),
         ingredients = str_replace_all(ingredients, ",,", ","))

meals_parsed <- unnest_regex(meals_parsed, input= ingredients, #unnesting
                output = ingredient, 
                pattern = ",") %>%
  add_count(ingredient) %>%
  filter(n > 15) %>%
  select(-n) %>%
  distinct(fdc_id, ingredient, .keep_all = T) %>%
  add_column(measure = 1)

meals_wide_df <- spread(meals_parsed, key = ingredient, value = measure, fill = 0) #pivot wider

meals_wide_df$gtin_upc <- meals_wide_df$serving_size <- meals_wide_df$serving_size_unit <- NULL #removing columns
meals_wide_df$household_serving_fulltext <- meals_wide_df$data_source <- meals_wide_df$available_date <- NULL
meals_wide_df$market_country <- meals_wide_df$discontinued_date <- meals_wide_df$modified_date <- NULL

library(tidymodels)

pca_rec <- recipe(~ ., data = meals_wide_df) %>% #no values computed here
  update_role(fdc_id, branded_food_category, brand_owner, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec) #values are computed
pca_prep

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>% #this works but there are way too many ingredients (need to tidy ingredients and sort for top x amount)
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

tidied_pca %>% 
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0 )) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~component, scales = "free_y") +
  labs(y = NULL, fill = "Positive?")

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = fdc_id)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

#tidying ingredients and grouping redundant ingredients with slightly different names might help plot make more sense
#maybe look for a table with fdc_id numbers and meal names to join to the meals_wide_df so names are descriptive


################################################
###            EXPLORATORY UMAP              ###
################################################

library(embed)

umap_rec <- recipe(~., data = meals_wide_df) %>%
  update_role(fdc_id, branded_food_category, brand_owner, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

umap_prep

juice(umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = fdc_id)) +
  geom_point(aes(color = branded_food_category), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)
