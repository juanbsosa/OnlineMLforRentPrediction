import pandas as pd
import time
import xgboost as xgb
import numpy as np
from sklearn.model_selection import GridSearchCV, TimeSeriesSplit
from sklearn.metrics import make_scorer, mean_absolute_percentage_error, mean_squared_error, mean_absolute_error

def estimate_xgb(
        df,
        DEP_VAR = ['price_realpesos'],
        INDEP_VARS = ['house', 'covered_area', 'uncovered_area', 'bedrooms', 'bathrooms', 
       'pool', 'security', 'furnished', 'heating', 'air_conditioning',
       'parking', 'common_space', 'fitness_space',
       'distance_to_transport', 'distance_to_greenspace',
       'longitude', 'latitude'],
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
        error_metric = ['rmse', 'mae', 'mape'],
        # path_data_out=r'G:\My Drive\MasterThesis\Output\Tables\EstimationResults\TrainingStage\auxiliary',
        # save_csv=True,
        time_window_size=False,
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

    # Create a data frame to store the results for the error metrics in all months
    results = pd.DataFrame(
        {'month': unique_months}
        )
    
    # Add column for loop iteration time
    results['loop_duration_seconds'] = np.nan

    # Add columns for the error metrics
    for em in error_metric:
        results[em] = np.nan

    # Add columns for best parameters
    if not no_tuning:
        all_param_keys = set().union(parameter_grid_1, parameter_grid_2)
        for param_key in all_param_keys:
            results[param_key] = np.nan

    # Consider window size
    skip_months = 1
    if time_window_size is not False:
        skip_months = skip_months + time_window_size - 1
    
    # Start iteration across months for testing (except the first one)
    for month in unique_months[starting_month + skip_months : ]:

        # Start measuring time for this loop iteration
        start_time_loop = time.time()

        if verbose>1:
            print(f"Starting estimation for month: {month} \n")

        if time_window_size is not False:
            first_month = month - pd.DateOffset(months=time_window_size)
            first_month = first_month.to_datetime64()
        else:
            first_month = unique_months[0]

        if verbose>2:
            print(f"Training sample goes from {first_month} to {(month - pd.DateOffset(months=1)).to_datetime64()}")

        # Subset df
        df_subset_train = df.loc[(df['listing_month'] >= first_month) & 
                                 (df['listing_month'] < month)].copy()
        df_subset_test = df.loc[df['listing_month'] == month].copy()

        

        # Define sample weights based on month
        df_subset_train['first_month'] = df_subset_train['listing_month'].min()
        df_subset_train['month_abs_diff'] = round(
            (df_subset_train['listing_month'] - df_subset_train['first_month']).dt.days / 30
        )

        # Define training and test samples
        # !!! In exp 3 this will require a function
        X_train = df_subset_train[INDEP_VARS]
        y_train = df_subset_train[DEP_VAR]
        X_test = df_subset_test[INDEP_VARS]
        y_test = df_subset_test[DEP_VAR]

        # Use GridSearchCV with XGBoost for hyperparameter tuning
        # !!! In exp 3 this will require a function

        # Define CV strategy
        if cv_strategy == "random_cv":
            cv_arg = 5
        elif cv_strategy == 'timeseries_split':
            cv_arg = TimeSeriesSplit(n_splits=5)

        # Initiate XGB regressor
        xgbr = xgb.XGBRegressor(objective='reg:squarederror', tree_method = 'exact', seed=seed_)


        # HYPERPARAMETER TUNING

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

                # Get the best parameters from the current grid search
                best_params = grid_search.best_params_
                if verbose>2:
                    print(f"Best parameters from round {round_num}: {best_params}")

                # Store best parameters for this month in the results data frame
                for key, value in best_params.items():
                    results.loc[results['month'] == month, key] = value

                # Update the model with the best parameters
                xgbr.set_params(**best_params)

            
            # Get the best estimator after tuning
            best_model = grid_search.best_estimator_

        # Fit the model to the training set
        best_model.fit(X_train, y_train)

        # Predict and calculate error metrics
        y_pred = best_model.predict(X_test)

        # Calculate error metrics
        for em in error_metric:
                if em=='rmse':
                    rmse = np.sqrt(mean_squared_error(y_pred, y_test))
                    results.loc[results['month'] == month, em] = rmse
                elif em=='mae':
                    mae = np.sqrt(mean_absolute_error(y_pred, y_test))
                    results.loc[results['month'] == month, em] = mae
                elif em=='mape':
                    mae = np.sqrt(mean_absolute_percentage_error(y_pred, y_test))
                    results.loc[results['month'] == month, em] = mae
        

        # End measuring time for this loop iteration
        end_time_loop = time.time()
        loop_duration = end_time_loop - start_time_loop
        results.loc[results['month'] == month, 'loop_duration_seconds'] = loop_duration  # Store loop duration

        if verbose>1:
            print(f"Time taken for month {month}: {loop_duration:.2f} seconds\n")

    # End measuring total execution time
    end_time_total = time.time()
    total_duration = end_time_total - start_time_total
    if verbose>0:
        print(f"Total execution time: {total_duration:.2f} seconds")

    return results