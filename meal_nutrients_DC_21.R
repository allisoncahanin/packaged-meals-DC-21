library(tidyverse)
library(tidytext)

meals_nutrient <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/food_nutrient.csv", header = T)
nutrient_names <- read.csv("C:/Users/allis/OneDrive/Documents/GitHub/packaged-meals-DC-21/nutrient.csv")

id_list <- meals_wide_df$fdc_id
id_list

meals_nutrient$id <- meals_nutrient$derivation_id <- NULL

nutrient_subset <- meals_nutrient[meals_nutrient$fdc_id %in% id_list,]

nutrient_subset$data_points <- nutrient_subset$min <- nutrient_subset$max <- NULL
nutrient_subset$median <- nutrient_subset$footnote <- nutrient_subset$min_year_acquired <- NULL

nutrient_subset <- nutrient_subset %>%
  inner_join(nutrient_names, by = c("nutrient_id" = "id")) %>%
  select(name, everything(nutrient_subset))

nutrient_subset$nutrient_id <- NULL

nutrient_wide_df <- spread(nutrient_subset, key = name, value = amount, fill = 0)
nutrient_wide_df <- data_frame(nutrient_wide_df)

nutrient_wide_df[is.na(nutrient_wide_df)] <- 0


nutrient_wide_df <- nutrient_wide_df %>%
  inner_join(meals) %>%
  select(branded_food_category, brand_owner, everything(nutrient_wide_df))


################################################
###             EXPLORATORY PCA              ###
################################################

library(tidymodels)

pca_rec <- recipe(~ ., data = nutrient_wide_df) %>% #no values computed here
  update_role(fdc_id, branded_food_category, brand_owner, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec) #values are computed
pca_prep

nutrient_tidied_pca <- tidy(pca_prep, 2)

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

juice(pca_prep) %>%
  ggplot(aes(PC1, PC3, label = fdc_id)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

install.packages("embed")
library(embed)

umap_rec <- recipe(~ ., data = nutrient_wide_df) %>% #no values computed here
  update_role(fdc_id, branded_food_category, brand_owner, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec) #values are computed
umap_prep

juice(umap_prep) %>% #different model based on clustering
  ggplot(aes(umap_1, umap_2, label = fdc_id)) +
  geom_point(aes(color = branded_food_category),alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

#need to do inner join to add names of food from "food" csv using the fdc_id to match
