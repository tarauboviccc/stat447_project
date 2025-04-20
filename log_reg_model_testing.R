# testing second bayesian model
library(rstan)

# creating binary category matrix to indicate category presence in basket
basket_category_matrix <- online_retail %>%
  group_by(InvoiceNo, ProductCategory) %>%
  summarise(present = 1, .groups = "drop") %>%
  pivot_wider(names_from = ProductCategory, values_from = present, values_fill = 0)

# going through each product category
target_categories <- c("Kitchenware", "Home Decor", "Kids & Accessories", "Bags & Accessories")
prepare_stan_data <- function(target_category, basket_matrix) {
  y <- basket_matrix[[target_category]]
  X <- basket_matrix %>%
    select(-InvoiceNo, -all_of(target_category)) %>%
    as.matrix() 
  
  list(N = nrow(X),
       P = ncol(X),
       y = y, X = X)
}

results <- list()
for (target_category in target_categories) {
  data <- prepare_stan_data(target_category, basket_category_matrix)
  fit <- stan(file = "~/Downloads/bayes_logistic_regression.stan",
              data = data,
              chains = 4,
              iter = 2000,
              seed = 123)
  
  results[[target_category]] <- fit
}
