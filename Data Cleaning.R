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

