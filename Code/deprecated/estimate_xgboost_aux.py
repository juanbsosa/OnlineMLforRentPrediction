import pandas as pd
import time
import xgboost as xgb
import numpy as np
from sklearn.model_selection import GridSearchCV, TimeSeriesSplit
from sklearn.metrics import make_scorer, mean_absolute_percentage_error, mean_squared_error

def estimate_xgb(
        df,
        parameter_grid_1,
        parameter_grid_2,
        cv_strategy="random_cv",
        sample_weights=None,
        DEP_VAR = ['price_realpesos'],
        INDEP_VARS = ['house', 'covered_area', 'uncovered_area', 'bedrooms', 'bathrooms', 
       'pool', 'security', 'furnished', 'heating', 'air_conditioning',
       'parking', 'common_space', 'fitness_space',
       'distance_to_transport', 'distance_to_greenspace',
       'longitude', 'latitude'],
        error_metric = ['rmse'],
        verbose=False,
        seed_=789
        ):
    
    # Start measuring total execution time
    start_time_total = time.time()
    
    # Unique months
    unique_months = sorted(df["listing_month"].unique())

    # Create a data frame to store the results for the error metrics in all months
    error_metric_results = pd.DataFrame(
        {'month': unique_months}
        )

    # Add columns for the error metrics
    for em in error_metric:
        error_metric_results[em] = np.nan
    
    # Start iteration across months (except the first one)
    for month in unique_months[1:]:

        # Start measuring time for this loop iteration
        start_time_loop = time.time()

        if verbose:
            print(f"Starting estimation for month: {month} \n")

        # Subset df
        df_subset_train = df.loc[df['listing_month'] < month].copy()
        df_subset_test = df.loc[df['listing_month'] == month].copy()

        # Define training and test samples
        # !!! In exp 3 this will require a function
        X_train = df_subset_train[INDEP_VARS]
        y_train = df_subset_train[DEP_VAR]
        X_test = df_subset_test[INDEP_VARS]
        y_test = df_subset_test[DEP_VAR]

        # Use GridSearchCV with XGBoost for hyperparameter tuning
        # !!! In exp 3 this will require a function

        # Define the scoring function to be minimized:

        # # Custom scorer for Mean Absolute Percentage Error (MAPE)
        # if error_metric.lower() == 'mape':
        #      mape_scorer = make_scorer(mape, greater_is_better=False)

        # # Custom objective for MAPE
        # def mape_objective(y_true, y_pred):
        #     grad = -100. * (y_true - y_pred) / (y_pred * y_true)
        #     hess = 100. / (y_pred * y_true)
        #     return grad, hess

        # Initiate XGB regressor
        xgbr = xgb.XGBRegressor(objective='reg:squarederror', seed=seed_)
        
        # First round of parameter tuning

        # Define CV strategy
        if cv_strategy == "random_cv":
            cv_arg = 5
        elif cv_strategy == 'timeseries_split':
            cv_arg = TimeSeriesSplit(n_splits=5)

        grid_search_1 = GridSearchCV(
            xgbr, 
            parameter_grid_1, 
            cv=cv_arg, 
            scoring='neg_mean_squared_error', 
            verbose=-1, 
            n_jobs=-1
            )
        
        # No sample weights
        if sample_weights is None:
            grid_search_1.fit(X_train, y_train)
            
        # Sample weights according to the month
        elif sample_weights is not None:
            df_subset_train['first_month'] = unique_months[0].copy()
            df_subset_train['month_abs_diff'] = round(
                (df_subset_train['listing_month'] - df_subset_train['first_month']).dt.days/30
                )
            if sample_weights=='linear':
                s_weights = df_subset_train['month_abs_diff']
            elif sample_weights=='quadratic':
                s_weights = df_subset_train['month_abs_diff']**2
            grid_search_1.fit(X_train, y_train, 
                            sample_weight = s_weights
                              )

        # Best parameters from the first grid search
        best_params_1 = grid_search_1.best_params_
        if verbose:
            print(f"Best parameters from the first grid search: {best_params_1}")

        # Set first set of parameters
        xgbr.set_params(**best_params_1)

        # Second round of parameter tuning
        grid_search_2 = GridSearchCV(
            xgbr, 
            parameter_grid_2, 
            cv=cv_arg, 
            scoring='neg_mean_squared_error', 
            verbose=-1, 
            n_jobs=-1
            )
        
        # No sample weights
        if sample_weights is None:
            grid_search_2.fit(X_train, y_train)
        # Sample weights according to the month
        elif sample_weights is not None:
            df_subset_train['first_month'] = unique_months[0].copy()
            df_subset_train['month_abs_diff'] = round(
                (df_subset_train['listing_month'] - df_subset_train['first_month']).dt.days/30
                )
            if sample_weights=='linear':
                s_weights = df_subset_train['month_abs_diff']
            elif sample_weights=='quadratic':
                s_weights = df_subset_train['month_abs_diff']**2
            grid_search_2.fit(X_train, y_train, 
                            sample_weight = s_weights
                              )

        # Best parameters from the first grid search
        best_params_2 = grid_search_2.best_params_
        if verbose:
            print(f"Best parameters from the second grid search: {best_params_2}")

        # Get the best estimator after tuning
        best_model = grid_search_2.best_estimator_

        # Train the model with the best learning_rate
        best_model.fit(X_train, y_train)

        # Predict and calculate error metrics
        y_pred = best_model.predict(X_test)

        # mape = mean_absolute_percentage_error(y_pred, y_test)
        # error_metric_results.loc[error_metric_results['month'] == month, 'mape'] = mape

        rmse = np.sqrt(mean_squared_error(y_pred, y_test))
        error_metric_results.loc[error_metric_results['month'] == month, 'rmse'] = rmse

        # End measuring time for this loop iteration
        end_time_loop = time.time()
        loop_duration = end_time_loop - start_time_loop
        if verbose:
            print(f"Time taken for month {month}: {loop_duration:.2f} seconds\n")

    # End measuring total execution time
    end_time_total = time.time()
    total_duration = end_time_total - start_time_total
    print(f"Total execution time: {total_duration:.2f} seconds")

    return error_metric_results



# Custom scorer for Mean Absolute Percentage Error (MAPE)
def mape(y_true, y_pred):

    mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100

    return mape