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