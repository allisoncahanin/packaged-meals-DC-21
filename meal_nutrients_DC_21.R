library(tidyverse)
library(tidytext)

meals_nutrient <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/food_nutrient.csv", header = T)
nutrient_names <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/nutrient.csv")
food_names <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/food.csv")


id_list <- meals_wide_df$fdc_id
id_list

meals_nutrient$id <- meals_nutrient$derivation_id <- NULL #removing unnecessary columns

nutrient_subset <- meals_nutrient[meals_nutrient$fdc_id %in% id_list,] #create a subset of nutritional values using id_list

nutrient_subset$data_points <- nutrient_subset$min <- nutrient_subset$max <- NULL #removing unnecessary columns
nutrient_subset$median <- nutrient_subset$footnote <- nutrient_subset$min_year_acquired <- NULL

nutrient_subset <- nutrient_subset %>% #join nutrient names to data frame using nutrient codes
  inner_join(nutrient_names, by = c("nutrient_id" = "id")) %>%
  select(name, everything(nutrient_subset))

nutrient_subset$nutrient_id <- NULL #removing unnecessary column

nutrient_wide_df <- spread(nutrient_subset, key = name, value = amount, fill = 0) #pivot data frame wider and fill empty cells with 0
nutrient_wide_df <- data_frame(nutrient_wide_df) 
nutrient_wide_df[is.na(nutrient_wide_df)] <- 0 #change any NA values to 0


nutrient_wide_df <- nutrient_wide_df %>% #adding brand and food category
  inner_join(meals) %>%
  select(branded_food_category, brand_owner, everything(nutrient_wide_df))

nutrient_wide_df <- nutrient_wide_df %>% #adding food description (name of food)
  inner_join(food_names) %>%
  select(description, everything(nutrient_wide_df))

nutrient_wide_df <- nutrient_wide_df %>% #text cleaning with regular expressions to remove oz/lb and pack/count
  mutate(description = str_to_title(description),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}\\.?[[:alnum:]]{0,2}[[:blank:]]?oz", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}\\.?[[:alnum:]]{0,2}[[:blank:]]?ounce", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}[[:blank:]]?ct", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}[[:blank:]]?count", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}[[:blank:]]?pack", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}[[:blank:]]?pk", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]{1,3}[[:blank:]]?lb", ignore_case = T)),
         description = str_remove(description, regex(" [[:alnum:]]/[[:alnum:]][[:blank:]]?lb", ignore_case = T)))


################################################
###             EXPLORATORY PCA              ###
################################################

library(tidymodels)

nutrient_pca_rec <- recipe(~ ., data = nutrient_wide_df) %>% #create recipe for PCA
  update_role(fdc_id, branded_food_category, brand_owner, description, new_role = "id") %>% #set roles as "id" for non-predictor variables
  step_normalize(all_predictors()) %>% #center and scale all predictors
  step_pca(all_predictors()) #train PCA (no values computed here)

nutrient_pca_prep <- prep(nutrient_pca_rec) #values are computed
nutrient_pca_prep

nutrient_tidied_pca <- tidy(nutrient_pca_prep, 2)

nutrient_tidied_pca %>% 
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

nutrient_tidied_pca %>% 
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

juice(nutrient_pca_prep) %>%
  ggplot(aes(PC1, PC2, label = description)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 1.5) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

################################################
###             EXPLORATORY UMAP             ###
################################################

library(embed)

#UMAP is a different model based on topology that looks at clustering

nutrient_umap_rec <- recipe(~ ., data = nutrient_wide_df) %>% #create recipe for UMAP
  update_role(fdc_id, branded_food_category, brand_owner, description, new_role = "id") %>% #set id roles for non-predictors
  step_normalize(all_predictors()) %>% #center and scale predictors
  step_umap(all_predictors()) #train UMAP (no values computed here)

nutrient_umap_prep <- prep(nutrient_umap_rec) #values are computed
nutrient_umap_prep

juice(nutrient_umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = description)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 1.5) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

