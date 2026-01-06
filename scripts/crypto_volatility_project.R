# Load libraries
library(readr)   # For reading CSV files
library(dplyr)   # Data manipulation

# Set working directory to your project folder
setwd("C:/Users/blsha/crypto-volatility-phase")

# Import datasets
install.packages("data.table")   
library(data.table)

btc <- fread("data/raw/BTC.csv", sep=";")
eth <- fread("data/raw/ETH.csv", sep=";")
bnb <- fread("data/raw/BNB.csv", sep=";")



# Show first few rows
head(btc)
head(eth)
head(bnb)

install.packages("tidyr")
library(tidyr)





# Read CSV correctly
btc <- fread("data/raw/BTC.csv", sep=";")
eth <- fread("data/raw/ETH.csv", sep=";")
bnb <- fread("data/raw/BNB.csv", sep=";")

# Check first few rows
head(btc)

library(data.table)

btc <- fread("data/raw/BTC.csv", sep=";")
eth <- fread("data/raw/ETH.csv", sep=";")
bnb <- fread("data/raw/BNB.csv", sep=";")

head(btc)











# -------------------- Clean messy data (base R version) --------------------

clean_data_base <- function(df) {
  # If your CSV was already read with sep=";", all columns are split correctly
  # Otherwise, if everything is in 1 column, split manually using strsplit
  if (ncol(df) == 1) {
    split_cols <- strsplit(df[[1]], ";")
    df <- as.data.frame(do.call(rbind, split_cols), stringsAsFactors = FALSE)
  }
  
  # Assign column names
  colnames(df) <- c("timeOpen", "timeClose", "timeHigh", "timeLow",
                    "name", "open", "high", "low", "close",
                    "volume", "marketCap", "circulatingSupply", "timestamp")
  
  # Convert numeric columns
  numeric_cols <- c("open", "high", "low", "close", "volume")
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(gsub('"', '', df[[col]]))
  }
  
  # Keep only useful columns
  df <- df[, c("timeOpen", "open", "high", "low", "close", "volume")]
  
  return(df)
}

# Apply cleaning on each dataset
btc_clean <- clean_data_base(btc)
eth_clean <- clean_data_base(eth)
bnb_clean <- clean_data_base(bnb)

# View cleaned format
head(btc_clean)



# Rename columns to common OHLC format
colnames(btc_clean) <- c("Date", "Open", "High", "Low", "Close", "Volume")
colnames(eth_clean) <- c("Date", "Open", "High", "Low", "Close", "Volume")
colnames(bnb_clean) <- c("Date", "Open", "High", "Low", "Close", "Volume")

# Check
head(btc_clean)

btc_clean$Date <- as.Date(btc_clean$Date)
eth_clean$Date <- as.Date(eth_clean$Date)
bnb_clean$Date <- as.Date(bnb_clean$Date)



btc_clean <- btc_clean[btc_clean$Volume > 0, ]
eth_clean <- eth_clean[eth_clean$Volume > 0, ]
bnb_clean <- bnb_clean[bnb_clean$Volume > 0, ]





# Summary of BTC
summary(btc_clean)

# OHLC range example
btc_clean$Range <- btc_clean$High - btc_clean$Low

# Max, Min, Avg
max(btc_clean$Close)
min(btc_clean$Close)
mean(btc_clean$Close)

# ----------------------------
# NORMALIZED CLOSE PRICE COMPARISON
# (All start from 100)
# ----------------------------

btc_norm <- btc_clean$Close / btc_clean$Close[1] * 100
eth_norm <- eth_clean$Close / eth_clean$Close[1] * 100
bnb_norm <- bnb_clean$Close / bnb_clean$Close[1] * 100

pdf("outputs/plots/ALL_PLOTS.pdf", width = 11, height = 8.5)



plot(btc_clean$Date, btc_norm, type="l",
     col="blue", lwd=2,
     xlab="Date", ylab="Normalized Close Price (Base = 100)",
     main="Normalized Close Price Comparison (BTC vs ETH vs BNB)")

lines(eth_clean$Date, eth_norm, col="green", lwd=2)
lines(bnb_clean$Date, bnb_norm, col="red", lwd=2)

legend("topleft",
       legend=c("BTC", "ETH", "BNB"),
       col=c("blue", "green", "red"),
       lwd=2)




btc_clean$Volatility <- (btc_clean$High - btc_clean$Low) / btc_clean$Open * 100
eth_clean$Volatility <- (eth_clean$High - eth_clean$Low) / eth_clean$Open * 100
bnb_clean$Volatility <- (bnb_clean$High - bnb_clean$Low) / bnb_clean$Open * 100

# Check
head(btc_clean[, c("Date", "Volatility")])


install.packages("quantmod")
library(quantmod)

# helper: dataframe -> xts OHLC
make_ohlc_xts <- function(df) {
  df <- df %>% arrange(Date)
  xts::xts(df[, c("Open","High","Low","Close")], order.by = df$Date)
}

btc_xts <- make_ohlc_xts(btc_clean)
eth_xts <- make_ohlc_xts(eth_clean)
bnb_xts <- make_ohlc_xts(bnb_clean)

# Candlestick charts (separate)
chartSeries(btc_xts, name="BTC OHLC (Candlestick)", theme="white", type="candlesticks")
chartSeries(eth_xts, name="ETH OHLC (Candlestick)", theme="white", type="candlesticks")
chartSeries(bnb_xts, name="BNB OHLC (Candlestick)", theme="white", type="candlesticks")





# ---- LOG RETURNS -----
btc_clean$LogReturn <- c(NA, diff(log(btc_clean$Close)))
eth_clean$LogReturn <- c(NA, diff(log(eth_clean$Close)))
bnb_clean$LogReturn <- c(NA, diff(log(bnb_clean$Close)))

# View sample
head(btc_clean[, c("Date", "Close", "LogReturn")])



write.csv(btc_clean, "data/cleaned/BTC_clean.csv", row.names = FALSE)
write.csv(eth_clean, "data/cleaned/ETH_clean.csv", row.names = FALSE)
write.csv(bnb_clean, "data/cleaned/BNB_clean.csv", row.names = FALSE)




# ============================================================
#            PHASE 2: CRYPTOCURRENCY EDA & VISUALIZATION
# ============================================================

# Load Required Libraries
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. Import Cleaned Datasets (from Phase 1 output)
# ------------------------------------------------------------
btc_clean <- read.csv("data/cleaned/BTC_clean.csv")
eth_clean <- read.csv("data/cleaned/ETH_clean.csv")
bnb_clean <- read.csv("data/cleaned/BNB_clean.csv")


# Convert Date column to Date format
btc_clean$Date <- as.Date(btc_clean$Date)
eth_clean$Date <- as.Date(eth_clean$Date)
bnb_clean$Date <- as.Date(bnb_clean$Date)

# ------------------------------------------------------------
# 2. Basic Summary Statistics
# ------------------------------------------------------------

# Summary of Close Prices, Volume, Volatility & Log Returns
summary(btc_clean)
summary(eth_clean)
summary(bnb_clean)

# Standard Deviation
sd(btc_clean$Close, na.rm = TRUE)
sd(eth_clean$Close, na.rm = TRUE)
sd(bnb_clean$Close, na.rm = TRUE)

sd(btc_clean$LogReturn, na.rm = TRUE)
sd(eth_clean$LogReturn, na.rm = TRUE)
sd(bnb_clean$LogReturn, na.rm = TRUE)


#close price

plot(btc_clean$Date, btc_clean$Close, type="l", lwd=2, col="orange",
     main="BTC Close Price Over Time", xlab="Date", ylab="Close Price")

plot(eth_clean$Date, eth_clean$Close, type="l", lwd=2, col="blue",
     main="ETH Close Price Over Time", xlab="Date", ylab="Close Price")

plot(bnb_clean$Date, bnb_clean$Close, type="l", lwd=2, col="green",
     main="BNB Close Price Over Time", xlab="Date", ylab="Close Price")

# ------------------------------------------------------------
# 4. Log Returns Distribution 
# ------------------------------------------------------------

plot(btc_clean$Date, btc_clean$LogReturn, type="l", col="orange", lwd=2,
     main="BTC Log Returns Over Time", xlab="Date", ylab="Log Return")

plot(eth_clean$Date, eth_clean$LogReturn, type="l", col="blue", lwd=2,
     main="ETH Log Returns Over Time", xlab="Date", ylab="Log Return")

plot(bnb_clean$Date, bnb_clean$LogReturn, type="l", col="green", lwd=2,
     main="BNB Log Returns Over Time", xlab="Date", ylab="Log Return")


# ------------------------------------------------------------
# 5. Volatility Comparison (Boxplot)
# ------------------------------------------------------------
plot(btc_clean$Date, btc_clean$Volatility, type="l", col="orange", lwd=2,
     main="BTC Daily Intraday Volatility (%)", xlab="Date", ylab="Volatility (%)")

plot(eth_clean$Date, eth_clean$Volatility, type="l", col="blue", lwd=2,
     main="ETH Daily Intraday Volatility (%)", xlab="Date", ylab="Volatility (%)")

plot(bnb_clean$Date, bnb_clean$Volatility, type="l", col="green", lwd=2,
     main="BNB Daily Intraday Volatility (%)", xlab="Date", ylab="Volatility (%)")


# ------------------------------------------------------------
# 6. Correlation Analysis Between Assets
# ------------------------------------------------------------
# ----------------------------
# CORRELATION (Proper Date-wise Merge)
# ----------------------------

crypto_merge2 <- btc_clean %>%
  select(Date, BTC_Close = Close, BTC_Ret = LogReturn) %>%
  inner_join(eth_clean %>% select(Date, ETH_Close = Close, ETH_Ret = LogReturn), by="Date") %>%
  inner_join(bnb_clean %>% select(Date, BNB_Close = Close, BNB_Ret = LogReturn), by="Date")

# Correlation of Close Prices
cor_close <- cor(crypto_merge2 %>% select(BTC_Close, ETH_Close, BNB_Close), use="complete.obs")
print(cor_close)

# Correlation of Log Returns (more meaningful)
cor_ret <- cor(crypto_merge2 %>% select(BTC_Ret, ETH_Ret, BNB_Ret), use="complete.obs")
print(cor_ret)


# ----------------------------
# Heatmap with numbers (base R)
# ----------------------------
plot_corr_heatmap <- function(cor_mat, main_title) {
  m <- cor_mat
  n <- nrow(m)
  
  # heatmap background
  image(1:n, 1:n, t(m[n:1, ]), axes = FALSE, xlab = "", ylab = "",
        main = main_title, col = heat.colors(20))
  
  axis(1, at=1:n, labels=colnames(m), las=2)
  axis(2, at=1:n, labels=rev(rownames(m)), las=2)
  
  # add values on each cell
  for (i in 1:n) {
    for (j in 1:n) {
      val <- round(m[i, j], 2)
      text(j, n - i + 1, labels = val, cex = 1)
    }
  }
}

plot_corr_heatmap(cor_close, "Correlation Heatmap (Close Prices) - with values")
plot_corr_heatmap(cor_ret,   "Correlation Heatmap (Log Returns) - with values")



#PHASE 3
library(dplyr)
library(ggplot2)

btc_clean <- read.csv("data/cleaned/BTC_clean.csv")
eth_clean <- read.csv("data/cleaned/ETH_clean.csv")
bnb_clean <- read.csv("data/cleaned/BNB_clean.csv")

btc_clean$Date <- as.Date(btc_clean$Date)
eth_clean$Date <- as.Date(eth_clean$Date)
bnb_clean$Date <- as.Date(bnb_clean$Date)

#feature engineering 
make_features <- function(df) {
  df <- df %>% arrange(Date)
  
  df %>%
    mutate(
      # target for regression (next day return)
      target_return = lead(LogReturn, 1),
      
      # lag features
      ret_lag1 = lag(LogReturn, 1),
      ret_lag2 = lag(LogReturn, 2),
      ret_lag3 = lag(LogReturn, 3),
      
      vol_lag1 = lag(Volatility, 1),
      
      # rolling averages (simple)
      ma_7  = zoo::rollapply(Close,  7, mean, fill = NA, align = "right"),
      ma_14 = zoo::rollapply(Close, 14, mean, fill = NA, align = "right"),
      
      # direction classification target (Up = 1, Down = 0)
      direction = ifelse(target_return > 0, 1, 0)
    ) %>%
    # remove rows where features/target are NA
    filter(!is.na(target_return),
           !is.na(ret_lag1), !is.na(ret_lag2), !is.na(ret_lag3),
           !is.na(vol_lag1),
           !is.na(ma_7), !is.na(ma_14))
}

library(zoo)   # needed for rollapply

btc_feat <- make_features(btc_clean)
eth_feat <- make_features(eth_clean)
bnb_feat <- make_features(bnb_clean)

#train test
time_split <- function(df, test_ratio = 0.2) {
  n <- nrow(df)
  test_size <- floor(n * test_ratio)
  train <- df[1:(n - test_size), ]
  test  <- df[(n - test_size + 1):n, ]
  list(train = train, test = test)
}

btc_split <- time_split(btc_feat)
eth_split <- time_split(eth_feat)
bnb_split <- time_split(bnb_feat)


#MODEL NO.1 PREDICT NEXT DAT RETURNS 
train_regression <- function(train_df, test_df, coin_name) {
  
  model <- lm(target_return ~ ret_lag1 + ret_lag2 + ret_lag3 + vol_lag1 + ma_7 + ma_14, data = train_df)
  
  pred <- predict(model, newdata = test_df)
  
  # Metrics
  rmse <- sqrt(mean((test_df$target_return - pred)^2))
  mae  <- mean(abs(test_df$target_return - pred))
  
  cat("\n============================\n")
  cat("REGRESSION RESULTS:", coin_name, "\n")
  cat("============================\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE :", mae, "\n")
  
  # Plot: Actual vs Predicted
  plot(test_df$Date, test_df$target_return, type="l", lwd=2,
       main=paste(coin_name, "- Actual vs Predicted Next-Day Returns"),
       xlab="Date", ylab="Return")
  lines(test_df$Date, pred, lwd=2, lty=2)
  legend("topright", legend=c("Actual","Predicted"), lwd=2, lty=c(1,2))
  
  list(model=model, rmse=rmse, mae=mae)
}

btc_reg <- train_regression(btc_split$train, btc_split$test, "BTC")
eth_reg <- train_regression(eth_split$train, eth_split$test, "ETH")
bnb_reg <- train_regression(bnb_split$train, bnb_split$test, "BNB")


#MODEL NO, 2 CLASSIFICATION UP AND DOWN
train_classification <- function(train_df, test_df, coin_name) {
  
  clf <- glm(direction ~ ret_lag1 + ret_lag2 + ret_lag3 + vol_lag1 + ma_7 + ma_14,
             data = train_df, family = binomial)
  
  prob <- predict(clf, newdata = test_df, type = "response")
  pred_class <- ifelse(prob >= 0.5, 1, 0)
  
  acc <- mean(pred_class == test_df$direction)
  
  cat("\n============================\n")
  cat("CLASSIFICATION RESULTS:", coin_name, "\n")
  cat("============================\n")
  cat("Accuracy:", acc, "\n")
  
  # Confusion matrix
  cm <- table(Predicted = pred_class, Actual = test_df$direction)
  print(cm)
  
  # Simple probability plot
  plot(test_df$Date, prob, type="l", lwd=2,
       main=paste(coin_name, "- Predicted Probability of Up Day"),
       xlab="Date", ylab="P(Up)")
  abline(h=0.5, lty=2)
  
  list(model=clf, accuracy=acc, confusion=cm)
}

btc_clf <- train_classification(btc_split$train, btc_split$test, "BTC")
eth_clf <- train_classification(eth_split$train, eth_split$test, "ETH")
bnb_clf <- train_classification(bnb_split$train, bnb_split$test, "BNB")


results <- data.frame(
  Coin = c("BTC","ETH","BNB"),
  RMSE = c(btc_reg$rmse, eth_reg$rmse, bnb_reg$rmse),
  MAE  = c(btc_reg$mae,  eth_reg$mae,  bnb_reg$mae),
  Accuracy = c(btc_clf$accuracy, eth_clf$accuracy, bnb_clf$accuracy)
)

print(results)
write.csv(results, "outputs/Phase3_Model_Results.csv", row.names = FALSE)

dev.off()


