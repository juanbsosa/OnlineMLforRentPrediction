import pandas as pd
import time
import xgboost as xgb
import numpy as np
from sklearn.model_selection import GridSearchCV, TimeSeriesSplit
from sklearn.metrics import make_scorer, mean_absolute_percentage_error, mean_squared_error, mean_absolute_error
from multiprocessing import Pool, cpu_count

def estimate_xgb_parallel(
        df,
        parameter_grid_1 = {
                            'learning_rate': [],
                            'n_estimators' : [],
                            'max_depth': [], 
                            'min_child_weight': []
                            },
        parameter_grid_2 = {
                            'gamma': [],
                            'subsample': [],
                            'colsample_bytree': [],
                            'reg_alpha':[]    
                            },
        sample_df=False,
        starting_month=0,
        no_tuning=False,
        cv_strategy="random_cv",
        sample_weights=None,
        DEP_VAR = ['price_realpesos'],
        INDEP_VARS = ['house', 'covered_area', 'uncovered_area', 'bedrooms', 'bathrooms', 
       'pool', 'security', 'furnished', 'heating', 'air_conditioning',
       'parking', 'common_space', 'fitness_space',
       'distance_to_transport', 'distance_to_greenspace',
       'longitude', 'latitude'],
        error_metric = ['rmse', 'mae', 'mape'],
        verbose=False,
        seed_=789
        ):
    
    # Sort df by month
    df = df.sort_values(by=['listing_month'])
    
    # Start measuring total execution time
    start_time_total = time.time()

    # Sample df for testing
    if sample_df:
        df = df.sample(n=sample_df)
    
    # Unique months
    unique_months = sorted(df["listing_month"].unique())

    # Preparing parameters for each month
    params_list = []
    for month in unique_months[starting_month + 1:]:
        params_list.append((month, df, parameter_grid_1, parameter_grid_2, 
                            INDEP_VARS, DEP_VAR, 
                            sample_weights, cv_strategy, seed_, verbose, no_tuning,
                            unique_months))

    # Use multiprocessing to process each month in parallel
    pool = Pool(processes=cpu_count()-1)
    results_list = pool.map(process_month, params_list)
    pool.close()
    pool.join()

    # Convert results to a DataFrame
    results = pd.DataFrame(results_list)

    # End measuring total execution time
    end_time_total = time.time()
    total_duration = end_time_total - start_time_total
    if verbose:
        print(f"Total execution time with parallel processing: {total_duration:.2f} seconds")

    return results


# Define the function to be executed in parallel
def process_month(params):
    # Unpack parameters
    (month, df, parameter_grid_1, parameter_grid_2, 
     INDEP_VARS, DEP_VAR, 
     sample_weights, cv_strategy, seed_, verbose, no_tuning, unique_months) = params

    # Subset df
    df_subset_train = df.loc[df['listing_month'] < month].copy()
    df_subset_test = df.loc[df['listing_month'] == month].copy()

    # Define sample weights based on month
    df_subset_train['first_month'] = unique_months[0].copy()
    df_subset_train['month_abs_diff'] = round(
        (df_subset_train['listing_month'] - df_subset_train['first_month']).dt.days / 30
    )

    # Define training and test samples
    # !!! In exp 3 this will require a function
    X_train = df_subset_train[INDEP_VARS]
    y_train = df_subset_train[DEP_VAR]
    X_test = df_subset_test[INDEP_VARS]
    y_test = df_subset_test[DEP_VAR]

    # Prepare the result for this month
    result = {
        'month': month,
    }

    # Use GridSearchCV with XGBoost for hyperparameter tuning
    # !!! In exp 3 this will require a function

    # Define CV strategy
    if cv_strategy == "random_cv":
        cv_arg = 5
    elif cv_strategy == 'timeseries_split':
        cv_arg = TimeSeriesSplit(n_splits=5)

    # Initiate XGB regressor
    xgbr = xgb.XGBRegressor(objective='reg:squarederror', tree_method = 'exact', seed=seed_)

    # No tuning
    if no_tuning:
        best_model = xgbr
    
    # Yes tuning
    else:
        # Define parameter grids for both rounds of calibration
        parameter_grids = [parameter_grid_1, parameter_grid_2]

        # Loop through each round of tuning
        for round_num, params in enumerate(parameter_grids, start=1):
            grid_search = GridSearchCV(
                estimator=xgbr, 
                param_grid=params, 
                cv=cv_arg, 
                scoring='neg_mean_squared_error', 
                verbose=0, 
                n_jobs=-1
            )

            # Fit the model with or without sample weights
            if sample_weights is None:
                grid_search.fit(X_train, y_train)
            else:
                if sample_weights == 'linear':
                    s_weights = df_subset_train['month_abs_diff']
                elif sample_weights == 'quadratic':
                    s_weights = df_subset_train['month_abs_diff'] ** 2
                grid_search.fit(X_train, y_train, sample_weight=s_weights)

            # Update the model with the best parameters
            best_params = grid_search.best_params_
            xgbr.set_params(**best_params)

            # Store best parameters for this month in the results data frame
            for key, value in best_params.items():
                result.loc[result['month'] == month, key] = value

        # Get the best estimator after tuning
        best_model = grid_search.best_estimator_

    # Fit the model to the training set
    best_model.fit(X_train, y_train)

    # Predict and calculate error metrics
    y_pred = best_model.predict(X_test)

    # Calculate error metric
    result_metrics = {}
    for em in ['rmse', 'mae', 'mape']:
        if em == 'rmse':
            result_metrics['rmse'] = np.sqrt(mean_squared_error(y_pred, y_test))
        elif em == 'mae':
            result_metrics['mae'] = np.sqrt(mean_absolute_error(y_pred, y_test))
        elif em == 'mape':
            result_metrics['mape'] = np.sqrt(mean_absolute_percentage_error(y_pred, y_test))
    result.update(result_metrics)

    return result