library(tidyverse)
df <- read_csv("~/Downloads/UBC/STAT 447/PROJECT/cleaned_online_retail.csv")
head(df)

# Step 1: Keep CustomerID when aggregating total quantity per category per invoice
basket_category_df <- df %>%
  group_by(InvoiceNo, CustomerID, ProductCategory) %>%
  summarise(TotalQuantity = sum(Quantity), .groups = "drop")

# Step 2: Pivot wider to get category columns, keeping CustomerID
basket_wide_df <- basket_category_df %>%
  pivot_wider(
    names_from = ProductCategory,
    values_from = TotalQuantity,
    values_fill = 0
  )

head(basket_wide_df)

# Step 3: Create binary flags for presence of categories (1 if purchased, 0 otherwise)
basket_binary_df <- basket_wide_df %>%
  mutate(across(
    -c(InvoiceNo, CustomerID),  # Don't modify identifiers
    ~ if_else(. > 0, 1, 0),
    .names = "{.col}_flag"
  ))

# Check result
head(basket_binary_df)

library(broom.mixed)

# Step 4: Clean column names for formula safety
basket_binary_clean <- basket_binary_df %>%
  rename_with(make.names)

# Step 5: Define categories to loop over
target_categories <- c(
  "Kitchenware", "Home.Decor", "Craft...Stationery...Lighting",
  "Home.Essentials", "Kids...Accessories", "Bags...Accessories", "Other"
)

# Step 6: Define model fitting function
fit_hier_model <- function(category, data) {
  predictors <- setdiff(
    grep("_flag$", names(data), value = TRUE),
    paste0(category, "_flag")
  )
  
  model_formula <- as.formula(
    paste0(
      category, " ~ ",
      paste(predictors, collapse = " + "),
      " + (1 | CustomerID)"
    )
  )
  
  model_data <- data %>%
    filter(.data[[category]] > 0)
  
  brm(
    formula = model_formula,
    data = model_data,
    family = negbinomial(),
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 2), class = "Intercept"),
      prior(exponential(1), class = "sd", group = "CustomerID")
    ),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    seed = 123,
    cores = 4,
    control = list(adapt_delta = 0.99),
    silent = 2
  )
}
```

```{r}
library(brms)

# Step 7: Prepare folders to store output
dir.create("pp_checks", showWarnings = FALSE)
dir.create("summaries", showWarnings = FALSE)

model_list <- list()
summary_list <- list()

# Step 8: Loop through each category and fit models
for (cat in target_categories) {
  message(paste("Fitting model for:", cat))
  
  model <- fit_hier_model(cat, basket_binary_clean)
  model_list[[cat]] <- model
  
  # Save summary
  model_summary <- summary(model)
  summary_list[[cat]] <- model_summary
  capture.output(model_summary, file = paste0("summaries/", cat, "_summary.txt"))
  
  # Save pp_check plot
  png(paste0("pp_checks/", cat, "_pp_check.png"), width = 800, height = 600)
  pp_check(model)
  dev.off()
}

# Step 9: Compile tidy coefficient table
coefs <- map_dfr(names(model_list), function(cat) {
  tidy(model_list[[cat]], effects = "fixed") %>%
    filter(term != "Intercept") %>%
    mutate(
      Category = cat,
      Exp_Estimate = exp(estimate)
    )
}, .id = "Model")

# Step 10: View or export results
print(coefs)
write_csv(coefs, "cross_category_model_coefficients.csv")

# Step 11: Run posterior predictive checks
pp_check(model_list[["Kitchenware"]])
pp_check(model_list[["Home.Decor"]])
pp_check(model_list[["Craft...Stationery...Lighting"]])
pp_check(model_list[["Home.Essentials"]])
pp_check(model_list[["Kids...Accessories"]])
pp_check(model_list[["Bags...Accessories"]])
pp_check(model_list[["Other"]])