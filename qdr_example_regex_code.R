# Example code to manipulate the ingredient strings with regular expressions

library(tidyverse)
library(tidytext)
library(readxl)

meals <- read_xlsx("Data_Lv2_USDA_PackagedMeals.xlsx", col_types = rep(c('text', 'numeric', 'text', 'date', 'text', 'date'), c(4,1,4,2,1,1)))

set.seed(111) # generate consistent random sample of length 10
ing_test <- meals$ingredients[sample(1:nrow(meals), 10)]


# Option 1: quick and dirty -----------------------------------------------



# Option 1. Get rid of parentheses and anything inside them, to leave only "top-line" ingredients
# "Quick and dirty" option that will produce a lot of spurious ingredients.

top_level_ingredients <- ing_test %>%
  str_replace_all('\\(.*\\)', '') %>% # Remove anything inside parentheses
  strsplit(',|:|\\.') %>% # Split on comma, colon, or period.
  map(trimws) # Trim leading and trailing spaces.


# Option 2: a bit more detailed -------------------------------------------


# Option 2: If anything precedes a parenthesis within the same set of commas, it is just a descriptor for the ingredients following it.
# The same could be said for brackets nested inside parentheses.
# So in this case, we need to start at the inside and work our way out, removing the descriptor and getting rid of the parentheses.
# Eventually you are left with a comma-separated list of just the actual ingredients.

# Start with [ and ] then do ( and )

# Once you split the string by every time a bracket or parentheses open and close, you get a list.
# If there are even numbered entries in this list, they are immediately preceded by a descriptor that can be removed.
# So remove everything after the last comma in the string immediately preceding an even numbered entry.

remove_descriptor <- function(x) {
  if (length(x) >= 2) {
    for (i in seq(2, length(x), by = 2)) {
      x[i - 1] <- gsub(',[^,]*$', '', x[i - 1]) # Remove everything after last comma.
    }
  }
  return(x)
}

bottom_level_ingredients <- ing_test %>%
  strsplit('(\\[)|(\\])') %>% # Every time a bracket opens or closes, split the string up.
  map(remove_descriptor) %>% # Remove the descriptor names where appropriate.
  map_chr(paste, collapse = ',') %>% # Paste the string back together without descriptors.
  strsplit('(\\()|(\\))') %>% # Repeat the above three steps for parentheses instead of brackets.
  map(remove_descriptor) %>%
  map_chr(paste, collapse = ',') %>%
  strsplit(',|:|\\.') %>% # Split by comma, colon, or period.
  map(trimws) # Remove leading and trailing white space.
