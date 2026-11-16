# This is to clean up the dataset file a bit, take out unnecessary data points, to make it clearer for us and fix some formatting
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Load the original dataset, the file is Windows-1252 encoded (The Euro sign is byte 0x80)
ppr <- read.csv(file = "PPR-ALL-50pct.csv", stringsAsFactors = FALSE, fileEncoding = "windows-1252")
mydata <- ppr
View(mydata)
dim(mydata)
str(mydata)
summary(mydata)

# Rename columns to short, clean names
names(mydata) <- c("date", "address", "county", "eircode", "price",
                   "not_full_market", "vat_excl", "desc", "size_desc")

#Type conversion
# Have to change the Euro price text to integer
mydata$price <- as.numeric(gsub("[^0-9.]", "", mydata$price))
mydata$date  <- as.Date(mydata$date, format = "%d/%m/%Y")
range(mydata$date)
sum(is.na(mydata$price))
sum(is.na(mydata$date))

# Duplicates
sum(duplicated(mydata))
mydata <- mydata[!duplicated(mydata), ]

# Missing values
# read.csv stores empty cells in text columns as "", not NA
pct_missing <- sapply(mydata, function(x) round(100 * mean(is.na(x) | x == ""), 1))
pct_missing

# Eircode (more than half missing) and size_desc (most of them missing) are only filled in for some years,
# so drop them. Address is free text, not needed.
mydata$eircode   <- NULL
mydata$size_desc <- NULL
mydata$address   <- NULL

# Non market sales, they are not accurate representation of actual prices
table(mydata$not_full_market)
tapply(mydata$price, mydata$not_full_market, median)
mydata <- mydata[mydata$not_full_market == "No", ]
mydata$not_full_market <- NULL
View(mydata)

# Outliers
# IQR rule (1.5 x IQR) on price, same method as the course diamonds example
before <- mydata
ggplot(data = mydata, mapping = aes(x = county, y = price)) + geom_boxplot()

Q1 <- quantile(mydata$price, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$price, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value
c(lower, upper)                                   # fences in euro
sum(mydata$price < lower | mydata$price > upper)  # number of outliers

mydata <- mydata[mydata$price >= lower &
                   mydata$price <= upper &
                   !is.na(mydata$price), ]
rownames(mydata) <- NULL

# Before / after comparison
p1 <- ggplot(before, aes(x = price)) + geom_histogram(bins = 50) +
  labs(title = "Price - before outlier removal")
p2 <- ggplot(before, aes(x = log10(price))) + geom_histogram(bins = 50) +
  labs(title = "log10(Price) - before outlier removal")
p3 <- ggplot(mydata, aes(x = price)) + geom_histogram(bins = 50) +
  labs(title = "Price - after outlier removal")
p4 <- ggplot(mydata, aes(x = log10(price))) + geom_histogram(bins = 50) +
  labs(title = "log10(Price) - after outlier removal")
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#Create new useful columns
mydata$year      <- as.integer(format(mydata$date, "%Y"))
mydata$month     <- as.integer(format(mydata$date, "%m"))
mydata$quarter   <- (mydata$month - 1) %/% 3 + 1
mydata$log_price <- log10(mydata$price)

# New dwelling = 1, second-hand = 0
mydata$is_new <- as.integer(grepl("^New", mydata$desc))
mydata$desc   <- NULL

mydata$vat_excl  <- as.integer(mydata$vat_excl == "Yes")
mydata$is_dublin <- as.integer(mydata$county == "Dublin")

leinster <- c("Carlow", "Dublin", "Kildare", "Kilkenny", "Laois", "Longford",
              "Louth", "Meath", "Offaly", "Westmeath", "Wexford", "Wicklow")
munster  <- c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford")
connacht <- c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo")
mydata$province <- ifelse(mydata$county %in% leinster, "Leinster",
                          ifelse(mydata$county %in% munster,  "Munster",
                                 ifelse(mydata$county %in% connacht, "Connacht", "Ulster")))

# Double checksas final
dim(mydata)                  # expected 364,125 rows x 11 columns
colSums(is.na(mydata))
summary(mydata$price)
str(mydata)
View(mydata)

# Quick visual check
p5 <- ggplot(mydata, aes(x = province, y = price)) + geom_boxplot() +
  scale_y_log10() + labs(title = "Price by province (log scale)")
p6 <- ggplot(mydata, aes(x = factor(year))) + geom_bar() +
  labs(title = "Sales per year", x = "year")
grid.arrange(p5, p6, ncol = 2)

# save cleaned dataset
write.csv(mydata, file = "PPR_clean.csv", row.names = FALSE)

#
# Data cleaned, now onto the main requiremets of the CA
# 
# EDA, Variation and covariation
# The cleaned data is already in mydata from the steps above
# dplyr is needed for the pipe %>% (same as the course diamonds example)
install.packages("dplyr")
library(dplyr)

# How is each variable spread out?
# Question 1. What does the price distribution look like?
ggplot(data = mydata, mapping = aes(x = price)) +
  geom_histogram(binwidth = 20000, color = "black", fill = "steelblue") +
  labs(title = "Distribution of sale prices", x = "Price (EUR)", y = "Sales")
summary(mydata$price)

# Question 2. How many sales are there in each county?
mydata %>% count(county) %>% arrange(desc(n))
ggplot(data = mydata) + geom_bar(mapping = aes(x = county), fill = "steelblue") +
  coord_flip() + labs(title = "Sales per county", x = "County", y = "Number of sales")

# Question 3. How many sales happen in each month? Does seasons have any effect in house sales?
ggplot(data = mydata) + geom_bar(mapping = aes(x = factor(month)), fill = "steelblue") +
  labs(title = "Sales per month", x = "Month", y = "Number of sales")

# Qeustion 4. How many sales are new homes?
mydata %>% count(is_new) %>% mutate(percent = round(100 * n / sum(n), 1))

# Covariation: how do variables change together?
# Question 5. How has the typical (median) price changed over the years?
by_year <- mydata %>% group_by(year) %>% summarise(median_price = median(price))
by_year
ggplot(data = by_year, mapping = aes(x = year, y = median_price)) +
  geom_line(color = "steelblue") + geom_point() +
  labs(title = "Median price per year", y = "Median price (EUR)")

# Question 6. Does location matter? Median price and price spread for each county
by_county <- mydata %>% group_by(county) %>% summarise(median_price = median(price))
ggplot(data = by_county, mapping = aes(x = reorder(county, median_price), y = median_price)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Median price by county", x = "County", y = "Median price (EUR)")
ggplot(data = mydata, mapping = aes(x = reorder(county, price, FUN = median), y = price)) +
  geom_boxplot() + coord_flip() + labs(x = "County", y = "Price (EUR)")

# Question 7. Are new homes more expensive than second-hand homes?
mydata$home_type <- ifelse(mydata$is_new == 1, "New", "Second-hand")
ggplot(data = mydata, mapping = aes(x = home_type, y = price)) + geom_boxplot()
by_type_year <- mydata %>% group_by(year, home_type) %>% summarise(median_price = median(price))
ggplot(data = by_type_year, mapping = aes(x = year, y = median_price, color = home_type)) +
  geom_line() + labs(title = "New vs second-hand homes", y = "Median price (EUR)")

# Question 8. Do the number columns move together? (correlation)
num_cols <- mydata[, c("price", "year", "month", "is_new", "vat_excl", "is_dublin")]
cm <- round(cor(num_cols), 2)
cm
cm_long <- as.data.frame(as.table(cm))
ggplot(data = cm_long, mapping = aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
  labs(title = "Correlation between variables", x = "", y = "")

# is_new and vat_excl are almost the same column (0.99), so only one is used for PCA

# ENCODING PCA

# Label encoding. county text becomes the numbers 1 to 26
mydata$county_label <- as.integer(factor(mydata$county))
table(mydata$county, mydata$county_label)

# One-hot encoding. one 0/1 column for each county (26 columns)
onehot <- model.matrix(~ county - 1, data = mydata)
head(onehot[, 1:5])

# Data for PCA. numbers only
# vat_excl is left out because it copies is_new, is_dublin because countyDublin is the same column
pca_data <- data.frame(price = mydata$price,
                       year = mydata$year,
                       month = mydata$month,
                       is_new = mydata$is_new,
                       onehot)
dim(pca_data)

# PCA (scale. = TRUE puts every column on the same scale first)
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
summary(pca)

# How much of the information does each component keep?
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
round(var_explained * 100, 1)
round(cumsum(var_explained) * 100, 1)

# Scree plot
scree <- data.frame(PC = 1:length(var_explained), variance = var_explained * 100)
ggplot(data = scree, mapping = aes(x = PC, y = variance)) +
  geom_col(fill = "steelblue") + geom_line() + geom_point() +
  labs(title = "Scree plot", x = "Principal component", y = "% of variance")

# Loadings. which original columns make up each component?
# (the biggest numbers, positive or negative, matter most)
round(pca$rotation[, 1:3], 2)

# Plot the sales on the first two components (random sample of 8000 to keep it fast)
set.seed(1)
idx <- sample(nrow(mydata), 8000)
scores <- data.frame(PC1 = pca$x[idx, 1], PC2 = pca$x[idx, 2],
                     dublin = factor(mydata$is_dublin[idx]))
ggplot(data = scores, mapping = aes(x = PC1, y = PC2, color = dublin)) +
  geom_point(alpha = 0.5, size = 1)