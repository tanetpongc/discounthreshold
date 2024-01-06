#Simulated Scanner Data
set.seed(12345678)

library(data.table)

brands = c("A", "B", "C", "D", "E")
formats = c("hypermarket", "supermarket", "convenience")
n = 5000

# Create initial data.table
dt_scanner <- data.table(
  cust = sample(1:150, n, replace = TRUE),
  week_nr = sample(1:153, n, replace = TRUE),
  brand = sample(brands, n, replace = TRUE),
  format = sample(formats, n, replace = TRUE),
  category = "z"
)

# Generate regprice with a uniform distribution
dt_scanner[, regprice := runif(n, min = 15, max = 60)]

# Generate discount such that it's always less than regprice
dt_scanner[, discount := runif(n, min = 0, max = regprice - 5)]

# Impose correlation:
# 1. Positive correlation between discount and quantity
# 2. Negative correlation between regprice and quantity
dt_scanner[, quantity := as.integer(1 + 2*(discount > 0) - regprice/40)]

# Ensure quantity is within a reasonable range
dt_scanner[quantity < 1, quantity := 1]
dt_scanner[quantity > 10, quantity := 10]

dt_holiday <- data.table(
  week_nr = 1:153,
  holiday = sample(c(0, 1), size = 153, replace = TRUE, prob = c(0.8, 0.2)) #20% of week is holiday
)

df <- merge(dt_scanner,dt_holiday, by=c("week_nr"))

write.csv(df,"df.csv", row.names = TRUE)

rm(list=ls())
