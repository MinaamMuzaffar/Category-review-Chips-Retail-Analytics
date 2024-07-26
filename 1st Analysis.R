library(readxl)

QVI_purchase_behaviour <- read_excel("C:/Users/AA/Desktop/QVI_purchase_behaviour.xlsx")
View(QVI_purchase_behaviour)

QVI_transaction <- read_excel("C:/Users/AA/Desktop/QVI_transaction_data.xlsx")
View(QVI_transaction)

str(QVI_purchase_behaviour)
str(QVI_transaction)

head(QVI_purchase_behaviour)
head(QVI_transaction)

QVI_transaction$DATE <- as.Date(QVI_transaction$DATE, origin = "1899-12-30") 

summary(QVI_transaction$PROD_NAME)


product_names <- QVI_transaction$PROD_NAME[sapply(QVI_transaction$PROD_NAME, is.character)]

productWords <- data.table(unlist(strsplit(unique(product_names), " ")))

setnames(productWords, 'words')

#cleaning product names
product_names <- QVI_transaction$PROD_NAME[sapply(QVI_transaction$PROD_NAME, is.character)]
clean_words <- product_names[!grepl("[[:digit:]]|&", " ")] 
clean_words_no_digits <- gsub("\\d+", "", clean_words)
cleaned_products_name <- gsub("[^[:alnum:]]+", "", clean_words_no_digits)
word_counts <- table(cleaned_products_name)
sorted_words <- sort(names(word_counts), decreasing = TRUE)

transactionData <- QVI_transaction %>%
  filter(!grepl("salsa", tolower(cleaned_products_name))) %>%
  mutate(SALSA = NULL)  

summary(transactionData)
transactionData[transactionData$PROD_QTY == 200, ]
transactionData[transactionData$LYLTY_CARD_NBR == 226000, ]
summary(table(transactionData$LYLTY_CARD_NBR))
table(transactionData$DATE)

# Define start and end dates
start_date <- as.Date("2018-07-01")
end_date <- as.Date("2019-06-30")
date_range <- seq(from = start_date, to = end_date, by = 1)
date_table <- data.frame(DATE = date_range)

# Join the date_table with transactionData (assuming full join)
transactionData_joined <- inner_join(transactionData, date_table, by = "DATE")
transactionData_joined$DATE[is.na(transactionData_joined$DATE)] <- transactionData_joined$DATE[is.na(transactionData_joined$DATE)]

ggplot(transactionData_joined, aes(x = DATE, y = TOT_SALES)) +   geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") + scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Filter transactionData_joined for December data
december_data <- transactionData_joined %>%
  filter(month(DATE) == 12)  # Filter by month (12 for December)
ggplot(december_data, aes(x = DATE, y = TOT_SALES)) +
  geom_line() +
  labs(title = "Transactions in December",  
       x = "Day",
       y = "Number of transactions") +
  scale_x_date(breaks = "1 day") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

transactionData_joined$pack_size <- str_extract(transactionData_joined$PROD_NAME, "\\d+g$")

ggplot(transactionData_joined, aes(x = pack_size)) +
  geom_bar(stat = "count", color = "steelblue") +  # Use geom_bar with stat="count"
  labs(title = "Number of Transactions by Pack Size",
       x = "Pack Size (grams)",
       y = "Number of Transactions") +
  theme_classic()

transactionData_joined$brand <- sapply(str_split(transactionData_joined$PROD_NAME, "\\s+"), function(x) x[1])
transactionData_joined$brand <- gsub("RRD", "RED", brand)

data <- merge(transactionData_joined, QVI_purchase_behaviour, all.x = TRUE)
summary(data)

#Data exploration is now complete!

#Data analysis on customer segments 
# Assuming "transactionData_joined" is your data table (adjust if different)
library(ggplot2)
library(dplyr)

# Calculate total sales by segments
sales_by_segment <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(total_sales = sum(TOT_SALES)) 

# Create the plot
ggplot(sales_by_segment, aes(x = LIFESTAGE, y = total_sales, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity") + 
  labs(title = "Total Chip Sales by Lifestages and Premium Status",
       x = "Lifestage",
       y = "Total Sales",
       fill = "Premium Customer") +
  theme_classic() +
  scale_fill_manual(values = c("steelblue", "goldenrod", "gray"))

# Count customers by segments 
customer_counts <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(n_customers = n_distinct(LYLTY_CARD_NBR)) 

# Create the plot 
ggplot(customer_counts, aes(x = LIFESTAGE, y = n_customers, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity") +  
  # Explore other geom types like geom_col (for clustered bars)
  labs(title = "Number of Customers by Lifestages and Premium Status",
       x = "Lifestage",
       y = "Number of Customers",
       fill = "Premium Customer") +
  theme_classic() +
  scale_fill_manual(values = c("steelblue", "goldenrod","gray")) 

# Calculate average units per customer
avg_units <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg_units = sum(TOT_SALES) / n_distinct(LYLTY_CARD_NBR))  

# Create the plot
ggplot(avg_units, aes(x = LIFESTAGE, y = avg_units, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity") +  
  labs(title = "Average Units per Customer by Lifestages and Premium Status",
       x = "Lifestage",
       y = "Average Units",
       fill = "Premium Customer") +
  theme_classic() +
  scale_fill_manual(values = c("steelblue", "goldenrod","gray"))

# Calculate average sale price
avg_sale_price <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(avg_price = mean(TOT_SALES / PROD_QTY))

# Create the plot
ggplot(avg_sale_price, aes(x = LIFESTAGE, y = avg_price, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity") + 
  labs(title = "Average Sale Price by Lifestages and Premium Status",
       x = "Lifestage",
       y = "Average Price",
       fill = "Premium Customer") +
  theme_classic() +
  scale_fill_manual(values = c("steelblue", "goldenrod","gray"))


library(dplyr)  # For data manipulation
library(stats)  # For t-tests

# Filter data for relevant segments (assuming a "segment" variable exists)
midage_young_data <- data %>%
  filter(LIFESTAGE %in% c("Young Singles/Couples", "Midage Singles/Couples"))

# Function to perform t-test by segment and premium status
t_test_by_segment <- function(segment_data) {
  # Filter mainstream vs others
  mainstream <- segment_data %>% filter(segment == "Mainstream")
  others <- segment_data %>% filter(!segment %in% "Mainstream")
  
  # Check for sufficient data 
  if (nrow(mainstream) < 2 | nrow(others) < 2) {
    cat("Warning: Insufficient data for t-test in", segment_data$LIFESTAGE[1], "\n")
    return(NA)  
  }
  
  # Perform t-test 
  test_result <- t.test(mainstream$TOT_SALES, others$TOT_SALES, var.equal = TRUE)
  
# Loop through segments and perform tests
segment_tests <- lapply(split(midage_young_data, ~ LIFESTAGE), t_test_by_segment)

# Print results
for (segment in names(segment_tests)) {
  test_result <- segment_tests[[segment]]
  if (!is.na(test_result)) {
    cat("T-test results for", segment, ":\n")
    cat(test_result, "\n")
  }
}

# Filter data for Mainstream - Young Singles/Couples
target_segment <- data %>%
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream")

# Create transactions 
transactions <- as(split(target_segment, ~LYLTY_CARD_NBR), "list")

# Minimum support for frequent itemsets 
min_support <- 0.05

any(sapply(transactions, is.atomic))

# Perform apriori analysis
apriori_results <- apriori(transactions, min.support = min_support)

# Inspect frequent itemsets (top 10)
inspect(apriori_results, topn = 10)


# Group by Lifestage 
pack_size_summary <- data %>%
  group_by(LIFESTAGE) %>%
  summarise(avg_pack_size = mean(pack_size),
            median_pack_size = median(pack_size),
            sd_pack_size = sd(pack_size))

# Print results
print(pack_size_summary)
