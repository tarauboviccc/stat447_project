online_retail <- original_retail_data

# required libraries
library(dplyr)
library(tidyverse)
library(lubridate) # for ymd_hms
library(ggplot2)
library(corrplot)
library(tidyr)
library(pheatmap)


# EDA
glimpse(online_retail)

# data structures of each column
str(online_retail)

# analyzing missing values
colSums(is.na(online_retail))

# converting InvoiceDate to datetime
online_retail <- online_retail %>%
  mutate(InvoiceDate = ymd_hms(InvoiceDate))

# removing rows with missing Customer ID
online_retail <- online_retail %>%
  filter(!is.na(CustomerID))

# save data with Quantity < 0 (returns)
negative_quantity <- online_retail %>%
  filter(Quantity < 0)

# save data with UnitPrice < 0 (data errors)
negative_unit_price <- online_retail %>%
  filter(UnitPrice < 0)

# remove returns transactions (Quantity < 0) as well as the data errors (UnitPrice < 0)
online_retail <- online_retail %>%
  filter(Quantity > 0, UnitPrice > 0)

# adding total revenue column and extract month, day, etc., from the date for EDA.
online_retail <- online_retail %>%
  mutate(
    TotalRevenue = Quantity * UnitPrice,
    InvoiceMonth = format(InvoiceDate, "%Y-%m"),
    InvoiceDateOnly = as.Date(InvoiceDate)
  )

# Descriptive Statistics

# number of unique customers, products, and invoices
online_retail %>%
  summarise(
    n_customers = n_distinct(CustomerID),
    n_products = n_distinct(StockCode),
    n_invoices = n_distinct(InvoiceNo)
  )

# Top 10 most frequently purchased products
online_retail %>%
  count(Description, sort = TRUE) %>%
  slice_max(n, n = 10)

# top 10 most selling product codes
online_retail %>%
  count(StockCode, sort = TRUE) %>%
  slice_max(n, n = 10)

# show both
top_products <- online_retail %>%
  count(StockCode, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  left_join(online_retail %>% select(StockCode, Description) %>% distinct(), by = "StockCode")

# Monthly revenue trend
monthly_revenue <- online_retail %>%
  group_by(InvoiceMonth) %>%
  summarise(monthly_revenue = sum(TotalRevenue, na.rm = TRUE))

ggplot(monthly_revenue, aes(x = as.Date(paste0(InvoiceMonth, "-01")), y = monthly_revenue)) +
  geom_line(color = "#0072B2", size = 1) +
  labs(
    title = "Monthly Revenue Trend",
    x = "Month",
    y = "Revenue (£)"
  ) +
  theme_minimal()

# outlier analysis
ggplot(online_retail, aes(x = Quantity)) +
  geom_histogram(bins = 100, fill = "skyblue", color = "black") +
  scale_x_log10() +
  ggtitle("Distribution of Quantity Variable")

ggplot(online_retail, aes(x = UnitPrice)) +
  geom_histogram(bins = 100, fill = "salmon", color = "black") +
  scale_x_log10() +
  ggtitle("Distribution of Unit Price Variable")

# new variable CustomerFrequency - looking into how often each customer purchases
customer_frequency <- online_retail %>%
  group_by(CustomerID) %>%
  summarise(
    n_purchases = n_distinct(InvoiceNo),  # Count of unique invoices per customer
    total_quantity = sum(Quantity),  # Total quantity purchased by the customer
    total_spent = sum(TotalRevenue)  # Total revenue spent by the customer
  )

# customer purchase frequency distribution
ggplot(customer_frequency, aes(x = reorder(as.factor(CustomerID), -n_purchases), y = n_purchases)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  labs(
    title = "Number of Purchases per Customer",
    x = "Customer ID",
    y = "Number of Purchases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # hides customer IDs for readability

# creating ProductCategory variable using the keywords in the Description variable
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

online_retail %>% count(ProductCategory, sort = TRUE)

# distributions of new ProductCategory values
ggplot(online_retail, aes(x = ProductCategory)) + 
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Products Across Categories",
       x = "Product Category",
       y = "Number of Products") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate number of purchases per customer
customer_purchases <- online_retail %>%
  count(CustomerID) %>%
  rename(PurchaseCount = n)

# Apply log transformation for better visualization of large values
customer_purchases <- customer_purchases %>%
  mutate(LogPurchaseCount = log1p(PurchaseCount))  # log1p is log(x + 1)

# Plot: Customer ID vs. Log(Number of Purchases)
ggplot(customer_purchases, aes(x = CustomerID, y = LogPurchaseCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Log Transformed Number of Purchases per Customer",
       x = "Customer ID",
       y = "Log(Number of Purchases)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

top_20_purchases_customers <- customer_purchases %>%
  arrange(desc(PurchaseCount)) %>%
  head(20)

# Plot top 20 customers
ggplot(top_20_purchases_customers, aes(x = reorder(CustomerID, PurchaseCount), y = PurchaseCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 20 Customers by Number of Purchases",
       x = "Customer ID",
       y = "Number of Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the number of purchases per customer
customer_purchases <- online_retail %>%
  count(CustomerID) %>%
  rename(PurchaseCount = n)

summary(customer_purchases$PurchaseCount)

# Looking into correlations between numerical variables
numeric_data <- online_retail %>% 
  select(where(is.numeric))

# ensuring missing values are removed pairwise
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)


# co-purchases by product category from EDA
category_invoice_matrix <- online_retail %>%
  select(InvoiceNo, ProductCategory) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = ProductCategory, values_from = value, values_fill = 0)

# co-occurrence matrix
category_matrix <- as.matrix(category_invoice_matrix[,-1])
category_co_occurrence <- t(category_matrix) %*% category_matrix

# heatmap
pheatmap(category_co_occurrence,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Co-Purchase Heatmap by Product Category")

basket_sizes <- online_retail %>%
  group_by(InvoiceNo) %>%
  summarise(basket_size = n())

ggplot(basket_sizes, aes(x = basket_size)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_log10() + # Log scale to better visualize wide range of basket sizes
  labs(
    title = "Basket Size Distribution",
    x = "Number of Items in Basket (log scale)",
    y = "Frequency"
  ) +
  theme_minimal()
