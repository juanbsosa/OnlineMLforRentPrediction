# Online Machine Learning for Rent Price Prediction in Real Time

## Overview

This project focuses on predicting one-month-ahead rental prices in the Buenos Aires real estate market using a dynamic, ensemble-based machine learning approach. It aims to handle concept drift in the data by leveraging a rolling window strategy and ensemble models. The primary models used include XGBoost with robust hyperparameter tuning and ensemble techniques to enhance prediction accuracy.

## Repository Structure

- **Code/**
  - **R Scripts**: Data preprocessing, feature engineering, and exploratory data analysis (EDA).
  - **Python Scripts**: Machine learning model implementation and evaluation.
  - **Auxiliary/**: Helper scripts for specific tasks (e.g., location mapping).
  - **deprecated/**: Older versions of the scripts for reference.

- **Data/**: Contains the datasets used for training and testing the models (not included in this repository).

- **README.md**: This file, providing an overview of the project.

- **MasterThesis.Rproj**: R project file for seamless integration with RStudio.

## Key Components

### Data Preparation

1. **Joining and Cleaning**:
   - `1.1.JoinMonthlyData.R` to `1.4.AddVariables.R`: Merge monthly data files, clean incorrect entries, and create new features.

2. **Imputation and Outlier Removal**:
   - `2.1.MissingValueImputation.R` and `2.2.RemoveOutliers.R`: Handle missing values and remove anomalies from the dataset.

### Exploratory Data Analysis (EDA)

- Scripts like `2.3.VariableDescription.R` and `2.5.ExploratoryPlots.R` generate summary statistics and visualizations to understand the dataset.

### Machine Learning Model Training

1. **XGBoost Model (`estimate_xgboost.py`)**:
   - Implements an XGBoost model with hyperparameter tuning using GridSearchCV and TimeSeriesSplit for time-series data.
   - Evaluates model performance using MAE, MSE, and MAPE.
   - Saves the trained model using `joblib` for later use.

2. **Ensemble Model (`ensemble_model.py`)**:
   - Combines multiple XGBoost models using different ensemble strategies.
   - Supports equal weighting or cross-validation-based weighting of models.
   - Outputs error metrics for evaluation.

3. **Interactive Analysis (`MasterPython.ipynb`)**:
   - Jupyter Notebook for interactive data exploration and testing of machine learning models.
   - Includes feature encoding, data visualization, and preliminary model testing.

## Dependencies

To run the Python scripts, the following libraries are required:

```bash
pandas
numpy
scipy
matplotlib
seaborn
xgboost
scikit-learn
statsmodels
joblib
