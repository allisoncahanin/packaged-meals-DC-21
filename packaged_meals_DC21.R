# this is KT testing github connection.
library(tidyverse)
library(tidytext)

meals <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/Data_Lv2_USDA_PackagedMeals.csv")

################################################
###          EXPLORATORY ANALYSIS            ###
################################################

meals_df <- data_frame(meals)

meals_parsed <- meals_df %>%
  mutate(ingredients = str_to_lower(ingredients),
         ingredients = str_remove_all(ingredients, regex("less than 2% of:?", ignore_case = T)), #remove non-ingredient strings
         ingredients = str_remove_all(ingredients, regex("contains 2% or less of:?", ignore_case = T)),
         ingredients = str_remove_all(ingredients, regex("contains two percent or less of:?")),
         ingredients = str_remove_all(ingredients, regex("contains 2% or less:?")),
         ingredients = str_remove_all(ingredients, regex("and 2% or less:?")),
         ingredients = str_remove_all(ingredients, regex("and less than 2%:?")),
         ingredients = str_remove_all(ingredients, regex("2% or less of")),
         ingredients = str_remove_all(ingredients, regex("cured with:?")),
         ingredients = str_remove_all(ingredients, regex("contains the following:?")),
         ingredients = str_remove_all(ingredients, regex("each of the following:?")),
         ingredients = str_remove_all(ingredients, regex("the following:?")),
         ingredients = str_remove_all(ingredients, regex("dried")),
         ingredients = str_remove_all(ingredients, regex("contains")),
         ingredients = str_remove_all(ingredients, regex("anticaking agent")),
         ingredients = str_remove_all(ingredients, regex("for anticaking")),
         ingredients = str_remove_all(ingredients, regex("to prevent caking")),
         ingredients = str_remove_all(ingredients, regex("a preservative")),
         ingredients = str_remove_all(ingredients, regex("added")),
         ingredients = str_remove_all(ingredients, regex("for color")),
         ingredients = str_remove_all(ingredients, regex("added for color")),
         ingredients = str_remove_all(ingredients, regex("a natural mold inhibitor")),
         ingredients = str_remove_all(ingredients, regex("preserve freshness")),
         ingredients = str_remove_all(ingredients, regex("to maintain freshness")),
         ingredients = str_remove_all(ingredients, regex("ingredients")),
         ingredients = str_remove_all(ingredients, regex("filling")),
         ingredients = str_remove_all(ingredients, regex("crust")),
         ingredients = str_remove_all(ingredients, regex("sauce")),
         ingredients = str_remove_all(ingredients, regex("one or more of the following:?")),
         ingredients = str_remove_all(ingredients, regex(" to ")),
         ingredients = str_remove_all(ingredients, regex("from")),
         ingredients = str_remove_all(ingredients, "\\*"), 
         ingredients = str_remove_all(ingredients, "\\."),
         ingredients = str_replace_all(ingredients, "and/or", ","),
         ingredients = str_replace_all(ingredients, "and", ","),
         ingredients = str_replace_all(ingredients, "\\(", ","), #replacing delimiters
         ingredients = str_replace_all(ingredients, "\\)", ","),
         ingredients = str_replace_all(ingredients, "\\{", ","),
         ingredients = str_replace_all(ingredients, "\\}", ","),
         ingredients = str_replace_all(ingredients, ":", ","),
         ingredients = str_replace_all(ingredients, "\\[", ","),
         ingredients = str_replace_all(ingredients, "\\]", ","),
         ingredients = str_replace_all(ingredients, ",,", ","),
         ingredients = str_replace_all(ingredients, ", ,", ","))


meals_unnested <- unnest_regex(meals_parsed, #un-nesting using comma as delimiter
               input= ingredients, 
               output = ingredient, 
               pattern = ",")

meals_tidy <- meals_unnested %>%
  mutate(ingredient = str_trim(ingredient),
         ingredient = case_when(str_detect(ingredient, "flavor") ~ "flavoring", #fix some common issues/words with same meaning
                                str_detect(ingredient, "salt") ~ "salt",
                                str_detect(ingredient, "seasoning") ~ "spice",
                                str_detect(ingredient, "oes$") ~ str_remove(ingredient, "es$"), #remove plural words
                                str_detect(ingredient, "[^s]s$") ~ str_remove(ingredient, "s$"),
                                TRUE ~ ingredient))

ingredients_sorted <- meals_tidy %>% 
  count(ingredient,sort = T) %>%
  filter(n > 20)
         

################################################
###             EXPLORATORY PCA              ###
################################################

meals_tidy <- meals_tidy %>%
  add_count(ingredient) %>%
  filter(n > 20) %>%
  select(-n) %>%
  distinct(fdc_id, ingredient, .keep_all = T) %>%
  add_column(measure = 1)

meals_wide_df <- spread(meals_tidy, key = ingredient, value = measure, fill = 0) #pivot wider

meals_wide_df$gtin_upc <- meals_wide_df$serving_size <- meals_wide_df$serving_size_unit <- NULL #removing columns
meals_wide_df$household_serving_fulltext <- meals_wide_df$data_source <- meals_wide_df$available_date <- NULL
meals_wide_df$market_country <- meals_wide_df$discontinued_date <- meals_wide_df$modified_date <- NULL

library(tidymodels)

pca_rec <- recipe(~ ., data = meals_wide_df) %>% #no values computed here
  update_role(fdc_id, branded_food_category, brand_owner, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

pca_prep <- prep(pca_rec) #values are computed
pca_prep

pca_rec %>% #create a histogram showing % of variance explained by each PCA (PCA1 is only 3%....)
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#b6dfe2") + 
  xlim(c(0, 5)) + 
  ylab("% of total variance")

tidied_pca %>% #visualization
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

tidied_pca %>% #vistualization
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

juice(pca_prep) %>% #visualization
  ggplot(aes(PC1, PC2, label = fdc_id)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)


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
