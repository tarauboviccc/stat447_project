# required libraries
library(dplyr)
library(tidyverse)
library(lubridate) # for ymd_hms
library(ggplot2)
library(corrplot)
library(tidyr)
library(pheatmap)

# Exploratory Data Analysis (EDA)

# for testing purposes loading original retail data when needed
online_retail <- original_retail_data
glimpse(online_retail)

# looking into data structures of each column
str(online_retail)

# analyzing missing values & addressing
colSums(is.na(online_retail))

# converting InvoiceDate to format: year - month - day
online_retail <- online_retail %>%
  mutate(InvoiceDate = ymd_hms(InvoiceDate))

# removing rows with missing Customer ID & addressing in paper
online_retail <- online_retail %>%
  filter(!is.na(CustomerID))

# save data with Quantity < 0 (returns transactions) in other object for further analysis 
negative_quantity <- online_retail %>%
  filter(Quantity < 0)

# save data with Unit Price < 0 (data errors)
negative_unit_price <- online_retail %>%
  filter(UnitPrice < 0)

# remove returns transactions (Quantity < 0) as well as the data errors (Unit Price < 0)
# note: all transactions with Unit Price < 0 were removed once invalid CustomerIDs were removed
online_retail <- online_retail %>%
  filter(Quantity > 0, UnitPrice > 0)

# adding total revenue column and extracting date into year-month, and date columns for later on time series analysis
online_retail <- online_retail %>%
  mutate(TotalRevenue = Quantity * UnitPrice,
         InvoiceMonth = format(InvoiceDate, "%Y-%m"),
         InvoiceDateOnly = as.Date(InvoiceDate))

# Descriptive Statistics Part

# looking into unique customers, products, and invoices numbers
online_retail %>% summarise(n_customers = n_distinct(CustomerID),
                           n_products = n_distinct(StockCode),
                           n_invoices = n_distinct(InvoiceNo))

# top 10 most frequently purchased products
online_retail %>% count(StockCode, sort = TRUE) %>%
  slice_max(n, n = 10)

# show descriptions of top 10 purchased products
top_products <- online_retail %>% count(StockCode, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  left_join(online_retail %>% select(StockCode, Description) %>% distinct(), by = "StockCode")

# looking into monthly revenue trend
monthly_revenue <- online_retail %>%
  group_by(InvoiceMonth) %>%
  summarise(monthly_revenue = sum(TotalRevenue, na.rm = TRUE))

# visualizing monthly revenue over time
ggplot(monthly_revenue, aes(x = as.Date(paste0(InvoiceMonth, "-01")), y = monthly_revenue)) +
  geom_line(color = "#0072B2", size = 1) +
  labs(title = "Monthly Revenue Trend", x = "Month", y = "Revenue (£)") +
  theme_minimal()

# analyzing outliers & compressing Quantity var with log scale to display very large values
ggplot(online_retail, aes(x = Quantity)) +
  geom_histogram(fill = "skyblue", color = "black") +
  scale_x_log10() +
  ggtitle("Distribution of Quantity Variable")

ggplot(online_retail, aes(x = UnitPrice)) +
  geom_histogram(fill = "salmon", color = "black") +
  scale_x_log10() +
  ggtitle("Distribution of Unit Price Variable")

# creating ProductCategory variable using the keywords in the Description variable
# assistance from ChatGPT for recognizing keywords for each category cause it would take too long otherwise!
online_retail <- online_retail %>%
  mutate(ProductCategory = case_when(
    grepl("bag|jumbo shopper|lunch box|wallet|purse|tote|mini case|travel kit", Description, ignore.case = TRUE) ~ "Bags & Accessories",
    
    grepl("frame|sign|block word|metal sign|picture|plaque|poster|board|drawer knob|wood|bird ornament|heart|wicker|jar|decoration|birdcage|hook|fan", 
          Description, ignore.case = TRUE) ~ "Home Decor",
    
    grepl("napkin|mug|teacup|saucer|tray|bowl|jug|baking|pantry|recipe|kitchen|scales|jelly mould|dish|tin|cake|cutter|plate|cutlery|bread bin|cup", 
          Description, ignore.case = TRUE) ~ "Kitchenware",
    
    grepl("clock|alarm clock|doormat|hot water bottle|hand warmer|coat rack|kneeling pad|gumball|food container|jam|trinket|drawer|cabinet|tea towel|cushion|quilt|towel", 
          Description, ignore.case = TRUE) ~ "Home Essentials",
    
    grepl("gift|coaster|tool set|cosy|cosie|magnet|popcorn|doorstop|keepsake|cloche|egg holder|paper chain|snap cards|ribbons|party|bunting|banner|confetti|photo clip|clip line|wire|decor|cottage|sewing|tissue|parasol|lip gloss|umbrella|jigsaw|puzzle|snap|game|toy|modelling clay|skittles|dominoes|spinning top|cracker|bingo|blocks", 
          Description, ignore.case = TRUE) ~ "Kids & Accessories",
    
    grepl("pen|pencil|notebook|paper|folder|stationery|exercise book|chalk sticks|sticker|paint set|crayons|craft|lamp|nightlight|lights|garland|lantern|LED|t-light|candle|chilli lights", 
          Description, ignore.case = TRUE) ~ "Craft & Stationery & Lighting",
    
    TRUE ~ "Other"
  ))

# looking into distribution of new ProductCategory variable
online_retail %>% 
  count(ProductCategory, sort = TRUE)

# visualizing this distribution
ggplot(online_retail, aes(x = ProductCategory)) + 
  geom_bar(aes(fill = as.factor(ProductCategory)), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Products Categories",
       x = "Product Category",
       y = "Number of Products") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # so category names fit in

# new variable customer_purchases = representing number of purchases per customer
customer_purchases <- online_retail %>%
  count(CustomerID) %>%
  rename(PurchaseCount = n)

# applying log fn, so we can look into distribution for larger values as well
customer_purchases <- customer_purchases %>%
  mutate(LogPurchaseCount = log1p(PurchaseCount))

# visualizing distribution of customer purchases frequency Customer ID vs. log(# of purchases)
ggplot(customer_purchases, aes(x = CustomerID, y = LogPurchaseCount)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Log of Number of Purchases per Customer",
       x = "Customer IDs",
       y = "Log(Number of Purchases)") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # removing customer ids for privacy & better readability


top_50_purchases_customers <- customer_purchases %>%
  arrange(desc(PurchaseCount)) %>%
  head(50)

# plotting top 50 customers for looking more into outliers compared to stats summary
ggplot(top_50_purchases_customers, aes(x = reorder(CustomerID, PurchaseCount), y = PurchaseCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 20 Customers by Number of Purchases",
       x = "Customer ID",
       y = "Number of Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

summary(customer_purchases)

# quick check for correlations between numerical variables (not focus of bayesian), for extra exploration
numeric_columns <- online_retail %>% 
  select(where(is.numeric))

# ensuring missing values are removed pairwise
cor_matrix <- cor(numeric_columns, use = "complete.obs")
corrplot(cor_matrix, type = "upper")

# co-purchases by product category from invoice info
# creating binary matrix: InvoiceNo × ProductCategory
invoice_category_matrix <- online_retail %>%
  select(InvoiceNo, ProductCategory) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = ProductCategory, values_from = value, values_fill = 0)

# converting to matrix & compute co-occurrence
category_matrix <- as.matrix(invoice_category_matrix[,-1])

# cross-product to get category-category co-occur counts
co_occurrence_matrix <- t(category_matrix) %% category_matrix

# heatmap to look into co-purchases between categories
pheatmap(co_occurrence_matrix, cluster_rows = TRUE, cluster_cols = TRUE,
         main = "Product Categories Co-Purchase Frequency Heatmap",
         color = colorRampPalette(c("white", "lightblue", "steelblue", "darkblue"))(100))

# looking into basket sizes -  number of items per invoice
basket_sizes <- online_retail %>%
  group_by(InvoiceNo) %>%
  summarise(basket_size = n())

ggplot(basket_sizes, aes(x = basket_size)) +
  geom_histogram(fill = "salmon", color = "black") +
  scale_x_log10() + # log scale since some basket size are outlining the visualization
  labs(title = "Basket Size Distribution",
       x = "Number of Items in Basket",
       y = "Frequency") +
  theme_minimal()
