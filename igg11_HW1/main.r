#Title: "Unlocking Car Value Secrets: Mileage Trends and Price Patterns Revealed!"
#Subsection: "How Mileage and Fuel Choices Define Resale Value: Insights You Need to Know"


car_data <- read.csv("car_price_dataset.csv")
car_data$Price_Bins <- cut(car_data$Price, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))


#Queries

#1. Average Price by Brand
tapply(car_data$Price, car_data$Brand, mean)

#2. Price Bin vs. Fuel Type
table(car_data$Price_Bins, car_data$Fuel_Type)

#3. Maximum Mileage by Transmission
tapply(car_data$Mileage, car_data$Transmission, max)

#4. Average Mileage for Hybrid Cars
mean(subset(car_data, Fuel_Type == "Hybrid")$Mileage)

#5. Median Price of Cars with More than 4 Doors
median(subset(car_data, Doors > 4)$Price)

#Plots

#1. Boxplot of Price by Brand
boxplot(car_data$Price ~ car_data$Brand, main = "Price Distribution by Brand", xlab = "Brand", ylab = "Price (in USD)", col = c("lightblue", "lightgreen", "pink", "yellow", "cyan"))

#2. Histogram of Car Prices
hist(car_data$Price, breaks = 20, main = "Distribution of Car Prices", xlab = "Price (in USD)", col = "blue", border = "black")

#3. Scatter Plot of Mileage vs Price
plot(car_data$Mileage, car_data$Price, main = "Mileage vs Price", xlab = "Mileage", ylab = "Price (in USD)", col = "darkgreen", pch = 16)

#4. Bar Plot of Count of Cars by Fuel Type
fuel_type_counts <- table(car_data$Fuel_Type)
barplot(fuel_type_counts, main = "Count of Cars by Fuel Type", xlab = "Fuel Type", ylab = "Count", col = c("red"), border = "black")

#5. Mosaic Plot of Price Bins by Transmission
price_trans_table <- table(car_data$Price_Bins, car_data$Transmission)
mosaicplot(price_trans_table, main = "Mosaic Plot: Price Bins by Transmission", xlab = "Price Bins", ylab = "Transmission", col = c("lightblue", "lightgreen", "pink", "yellow", "cyan"), border = "black")
