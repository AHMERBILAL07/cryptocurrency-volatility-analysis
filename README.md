# cryptocurrency-volatility-analysis
Cryptocurrency Volatility Analysis using R (BTC, ETH, BNB)

# Cryptocurrency Volatility Analysis using R

## Course Information
**Course:** Introduction to Data Science  
**Instructor:** Dr. Fakhar Mustafa  
**University:** COMSATS University Islamabad, Sahiwal Campus  

## Group Members
- SP23-BCS-009 — Ahmer Bilal Mustafa  
- SP23-BCS-012 — Ali Hassan  
- FA22-BCS-109 — Salman  

---

## Project Overview
This project focuses on analyzing the volatility and price behavior of major cryptocurrencies using historical daily OHLC (Open, High, Low, Close) data. The complete data science lifecycle is implemented, including data importing, cleaning, transformation, exploratory data analysis, and predictive modeling.

The project is divided into three phases:
1. Phase 1: Data Importing, Cleaning, and Transformation  
2. Phase 2: Exploratory Data Analysis (EDA) and Visualization  
3. Phase 3: Predictive Modeling and Machine Learning  

---

## Dataset Sources
- Yahoo Finance (Cryptocurrency historical prices)
- CoinMarketCap (Daily OHLC and volume data)

The datasets contain daily records for the following cryptocurrencies:
- Bitcoin (BTC)
- Ethereum (ETH)
- Binance Coin (BNB)

---

## Folder Structure
crypto-volatility-phase/
│
├── data/
│ ├── raw/
│ │ ├── BTC.csv
│ │ ├── ETH.csv
│ │ └── BNB.csv
│ │
│ └── cleaned/
│ ├── BTC_clean.csv
│ ├── ETH_clean.csv
│ └── BNB_clean.csv
│
├── docs/
│ └── DOCUMENTATION.pdf
|
├── outputs/
│ ├── plots/
|    |── ALL_PLOTS.pdf
│ └── Phase3_Model_Results.csv
│
├── scripts/
│ └── crypto_volatility_project.R
│
│
└── README.md



---

## Phase 1: Data Importing, Cleaning & Transformation
- Raw CSV files imported using `data.table::fread`
- Date and timestamp columns formatted properly
- Zero-volume trading days removed
- Relevant OHLC and Volume columns selected
- Consistent column naming applied across all datasets

### Data Transformation Steps
- Log returns computed using closing prices
- Daily intraday volatility calculated
- Normalized close prices generated (base value = 100)

Cleaned datasets are saved as CSV files for reproducibility and future phases.

---

## Phase 2: Exploratory Data Analysis (EDA)
The following visual analyses were performed:
- Time-series plots of cryptocurrency closing prices
- Log return trends over time
- Daily volatility trend analysis
- Correlation analysis between BTC, ETH, and BNB
- Correlation heatmaps with numeric values

These analyses provide insight into market behavior and relationships between cryptocurrencies.

---

## Phase 3: Predictive Modeling
Two predictive approaches were implemented:

### Regression Model
- Objective: Predict next-day log returns
- Features: Lagged returns, volatility, and moving averages
- Evaluation Metrics: RMSE and MAE

### Classification Model
- Objective: Predict price direction (Up or Down)
- Model: Logistic Regression
- Evaluation Metrics: Accuracy and confusion matrix

Final model results are saved in:
outputs/results/Phase3_Model_Results.csv


---

## How to Run the Project
1. Open RStudio
2. Set the working directory to the project root folder
3. Run the complete script:
```r

source("scripts/crypto_volatility_project.R")


All cleaned datasets, plots, and model results will be generated automatically.

## Conclusion
This project demonstrates a complete application of data science techniques on real-world financial data. Through proper data preprocessing, visualization, and predictive modeling, meaningful insights into cryptocurrency volatility and price movements are obtained. The structured repository ensures reproducibility and easy extension for future work.


