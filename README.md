# Cross-Effect Modeling in Marketing & Customer Behavior

## Team Members:
- Tara Ubovic
- Vanshika Shah

## Project Theme:
Businesses are increasingly interested in understanding the inter-relationships between their products—how the addition of new ones affects the sales of others and the broader impact on the company. The models and analysis we aim to create will help product managers make informed decisions on strategies like bundle deals, pricing optimization, and special discounts to maintain consistent sales and profit performance across products.

## Data Analysis & Models:
We will create two Bayesian models for cross-effect modeling and compare their performance. If time and capacity allow, we will explore additional models beyond those covered in class (as outlined in the appendix).

### 1. **Bayesian Hierarchical Model:**
This model captures customer behavior at multiple levels (e.g., individual customers, product categories). Using Stan and simPPLe, we model the probability of purchasing a product while accounting for variability across customer segments.

### 2. **Bayesian Logistic Regression:**
This model predicts customer purchase likelihood for a given product, considering the cross-effects between products. We will implement it using Stan and simPPLe, modeling the log-odds of purchase and evaluating the impact of introducing one product on the likelihood of purchasing another.

## Public Repository
- GitHub Repository: [stat447_project](https://github.com/tarauboviccc/stat447_project)

## Datasets:

### Dataset 1: Online Retail Sales and Customer Data
- **Data Description:** This dataset consists of financial transactions from a UK-based online retail company between 01/12/2010 and 09/12/2011. It includes sales data of unique all-occasion gifts, mostly purchased by wholesalers. Key to our analysis is the `InvoiceNo` variable, which tracks products bought together.
- **URL:** [Online Retail Dataset](https://www.kaggle.com/datasets/ulrikthygepedersen/online-retail-dataset/data)

#### Dataset Variables:
- `InvoiceNo`: Unique identifier for each transaction
- `StockCode`: Product code (unique for each product)
- `Description`: Product description
- `Quantity`: Amount of product purchased
- `InvoiceDate`: Date of the transaction
- `UnitPrice`: Price per unit
- `CustomerID`: Unique identifier for each customer
- `Country`: Customer’s location

### Dataset 2: Online Shopping Patterns and Retail Performance Dataset
- **Data Description:** This dataset contains online sales transactions from 2019 to 2021. We will use the `InvoiceNo` to examine products bought together and `CustomerID` to track past purchases, crucial for analyzing inter-product relationships.
- **URL:** [Online Sales Dataset](https://www.kaggle.com/datasets/yusufdelikkaya/online-sales-dataset)

#### Dataset Variables:
- `InvoiceNo`: Unique identifier for each transaction
- `StockCode`: Product SKU
- `Description`: Product description
- `Quantity`: Number of units sold
- `InvoiceDate`: Date and time of transaction
- `UnitPrice`: Price per unit
- `CustomerID`: Unique customer identifier
- `Country`: Customer’s country
- `Discount`: Discount applied (if any)
- `PaymentMethod`: Method of payment
- `ShippingCost`: Shipping cost
- `Category`: Product category
- `SalesChannel`: Sales platform/channel
- `ReturnStatus`: Whether the order was returned
- `ShipmentProvider`: Courier handling the shipment
- `WarehouseLocation`: Location of the warehouse
- `OrderPriority`: Priority level of the order

## Team Contribution & Division Plan
- **Data Cleaning & Analysis:** Both members will collaborate on data cleaning and selecting relevant variables for the models. We will perform exploratory data analysis together and review previous uses of these datasets.
- **Model Development:** Each team member will be responsible for one of the Bayesian models. We will compare the models and analyze their performance, discussing insights from both approaches.
- **Collaboration & Meetings:** Regular check-ins will be held to discuss progress, share insights, and assist each other. We will also attend office hours if necessary to consult with the TA or professor.
- **Documentation:** Both teammates will contribute to developing documentation, describing model choices, previous research, improvements, and general discussion.

## Appendix
- **Potential Exploration of Pigeons.jl:** If time permits, we will explore the Pigeons.jl framework for efficient distributed sampling, which may improve the scalability and computational feasibility of our Bayesian models. The goals of using this framework:
- We will compare the inference efficiency between Pigeons.jl and our current approaches (Stan & simPPLe).
- If time allows, we will experiment with larger datasets or additional Bayesian approaches using Pigeons.jl.
