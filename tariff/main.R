# ========== SETUP ==========
# Load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)

# ========== SECTION 1: EXPORTS, IMPORTS, TRADE BALANCE ==========

# Load export and import data
exports <- read_excel("trad-geo-time-series-0125.xlsx", sheet = "Table 1", skip = 5)
imports <- read_excel("trad-geo-time-series-0125.xlsx", sheet = "Table 2", skip = 5)

# Filter for valid years
valid_years <- function(df) {
  df[!is.na(df$Period) & grepl("^[0-9]{4}$", df$Period), ]
}
exports <- valid_years(exports)
imports <- valid_years(imports)

# Select countries of interest
countries <- c('Period', 'Canada', 'China', 'Germany', 'European Union', 'Mexico', 'Japan')
exports_selected <- exports[, countries]
imports_selected <- imports[, countries]

# Calculate trade balance (Exports - Imports)
trade_balance <- exports_selected
trade_balance[,-1] <- exports_selected[,-1] - imports_selected[,-1]

# Reshape data for plotting
exports_long <- melt(exports_selected, id.vars = 'Period', variable.name = 'Country', value.name = 'Exports')
imports_long <- melt(imports_selected, id.vars = 'Period', variable.name = 'Country', value.name = 'Imports')
balance_long <- melt(trade_balance, id.vars = 'Period', variable.name = 'Country', value.name = 'Balance')

# Convert Period to numeric
exports_long$Period <- as.numeric(exports_long$Period)
imports_long$Period <- as.numeric(imports_long$Period)
balance_long$Period <- as.numeric(balance_long$Period)

# === Plots ===
# Plot: Exports
ggplot(exports_long, aes(x = Period, y = Exports, color = Country)) +
  geom_line() + geom_point() +
  labs(title = "U.S. Annual Exports by Country", x = "Year", y = "Exports (Millions of USD)") +
  theme_minimal()

# Plot: Imports
ggplot(imports_long, aes(x = Period, y = Imports, color = Country)) +
  geom_line() + geom_point() +
  labs(title = "U.S. Annual Imports by Country", x = "Year", y = "Imports (Millions of USD)") +
  theme_minimal()

# Plot: Trade Balance
ggplot(balance_long, aes(x = Period, y = Balance, color = Country)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "U.S. Annual Trade Balance by Country (Exports - Imports)",
       x = "Year", y = "Balance (Millions of USD)") +
  theme_minimal()

# ========== SECTION 2: TOP 10 TRADE DEFICITS (JAN 2025) ==========

# Load trade balance data
trade_data <- read_excel("ft900xlsx/exh14.xlsx", sheet = "14", skip = 6, col_names = FALSE)

# Rename and select relevant columns
colnames(trade_data) <- c("Country_Region", "Balance_Jan2025", "Balance_Dec2024",
                          "Exports_Jan2025", "Exports_Dec2024", "X1",
                          "Imports_Jan2025", "Imports_Dec2024", "X2", "X3")
trade_data <- trade_data[!is.na(trade_data$Balance_Jan2025), c("Country_Region", "Balance_Jan2025")]
trade_data$Balance_Jan2025 <- as.numeric(trade_data$Balance_Jan2025)

# Get top 10 trade deficits
trade_deficits <- head(trade_data[order(trade_data$Balance_Jan2025), ], 10)

# Plot: Top 10 Trade Deficits
ggplot(trade_deficits, aes(x = reorder(Country_Region, Balance_Jan2025), y = Balance_Jan2025)) +
  geom_bar(stat = 'identity', fill = 'coral') +
  coord_flip() +
  labs(title = 'Top 10 U.S. Trade Deficits by Country (January 2025)',
       x = 'Country/Region',
       y = 'Trade Balance (Millions of USD)') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 10))

# ========== SECTION 3: WTO TARIFF PROFILE COMPARISON ==========

# WTO tariff data
wto_data <- data.frame(
  Country = c("China", "European Union", "United States"),
  AvgMFN = c(7.5, 5.0, 3.3),
  DutyFree = c(9.7, 29.3, 47.5)
)

# Plot: Average MFN Applied Tariffs
ggplot(wto_data, aes(x = reorder(Country, -AvgMFN), y = AvgMFN, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Average MFN Applied Tariffs (All Products)",
       y = "Tariff (%)", x = "") +
  theme_minimal()

# Plot: Duty-Free Import Share
ggplot(wto_data, aes(x = reorder(Country, -DutyFree), y = DutyFree, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Duty-Free Import Share by Country",
       y = "Percent of Goods Imported Duty-Free", x = "") +
  theme_minimal()