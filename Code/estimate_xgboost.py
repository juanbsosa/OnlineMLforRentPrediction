import pandas as pd
import time
import xgboost as xgb
import os
import numpy as np
import joblib
from sklearn.model_selection import GridSearchCV, TimeSeriesSplit, train_test_split
from sklearn.metrics import mean_absolute_percentage_error, mean_squared_error, mean_absolute_error, median_absolute_error
  
n_cpu = os.cpu_count()

def estimate_xgb(
        df,
        DEP_VAR,
        INDEP_VARS,

        sample_df=False,
        starting_month=0,
        ending_month=None,

        train_test_splin='time_series', # or 'random'

        tune_hyperparams=False,
        best_params_path=None,
        parameter_grid = [{
                            'learning_rate': [],
                            'n_estimators' : [],
                            'max_depth': [], 
                            'min_child_weight': [],
                            'gamma': [],
                            'subsample': [],
                            'colsample_bytree': [],
                            'reg_alpha':[]    
                            }], # list of dictionary/ies
        cv_strategy="random_cv", # or 'time_series_split'
        sample_weights=None,

        time_window_size=False,
        error_metric = ['rmse', 'mae', 'mape', 'mdae'],
        save_model_path=False,

        verbose=False,
        seed_=789

        ):
    
    # Start measuring total execution time
    start_time_total = time.time()
    
    # Sort data frame by month
    df = df.sort_values(by=['listing_month'])

    # Retrieve unique months
    unique_months = sorted(df["listing_month"].unique())

    # Subset dataframe according to starting and ending months
    if starting_month:
        df = df.loc[(df['listing_month'] >= unique_months[starting_month])].copy()
    if ending_month:
        df = df.loc[(df['listing_month'] <= unique_months[ending_month])].copy()

    # If specified, take a sample of the data frame
    if sample_df:
        df = df.sample(n=sample_df)

    # Create a data frame to store the results for the error metrics in all months
    results = pd.DataFrame(
        {'month': unique_months}
        )
    
    # Add column for loop iteration time
    results['loop_duration_seconds'] = np.nan

    # Add columns for the error metrics
    for em in error_metric:
        results[em] = np.nan

    # Add columns for best parameters, if they will be tuned
    if tune_hyperparams:
        all_param_keys = set()
        for params in parameter_grid:
            all_param_keys = all_param_keys.union(params)
        for param_key in all_param_keys:
            results[param_key] = np.nan

    # Consider window size
    skip_months = 1
    if time_window_size is not False:
        skip_months = skip_months + time_window_size - 1
    
    # Start iteration to evaluate performance in the test sample, 
    #   considering each month (except the first) as the test sample.
    for month in unique_months[starting_month + skip_months : ending_month]:

        # Start measuring time for this loop iteration
        start_time_loop = time.time()

        # Parse month string
        if isinstance(month, pd.Timestamp):
            month_parsed = month.strftime('%Y-%m')
        else:
            month_parsed = np.datetime_as_string(month, unit='M')

        # Report time frame chosen
        if verbose>1:
            print(f"Starting estimation for month: {month_parsed} \n")

        # Define first month and parse it
        if time_window_size is False:
            first_month = unique_months[0]
            # first_month_parsed = first_month.strftime('%Y-%m')
        else:
            first_month = month - pd.DateOffset(months=time_window_size)
            first_month = first_month.to_datetime64()
        
        first_month_parsed = np.datetime_as_string(first_month, unit='M')

        if verbose>2:
            end_month_parsed = (month - pd.DateOffset(months=1)).strftime('%Y-%m')
            # end_month_parsed = np.datetime_as_string((month - pd.DateOffset(months=1)).to_datetime64(), unit='M')
            print(f"Training sample goes from {first_month_parsed} to {end_month_parsed}")

        # Train-Test Split

        # Random Split
        if train_test_splin=='random':
            X = df[INDEP_VARS]
            y = df[DEP_VAR]
            X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state=seed_)

        # Train-Test Time Series Split: Subset dataframe according to the chosen timeframe
        elif train_test_splin=='time_series':
            df_subset_train = df.loc[(df['listing_month'] >= first_month) & 
                                    (df['listing_month'] < month)].copy()
            df_subset_test = df.loc[df['listing_month'] == month].copy()        

            # If specified, define sample weights based on months since the present
            if sample_weights:
                df_subset_train['first_month'] = df_subset_train['listing_month'].min()
                df_subset_train['month_abs_diff'] = round(
                    (df_subset_train['listing_month'] - df_subset_train['first_month']).dt.days / 30
                )

            # Define training and test samples
            X_train = df_subset_train[INDEP_VARS]
            y_train = df_subset_train[DEP_VAR]
            X_test = df_subset_test[INDEP_VARS]
            y_test = df_subset_test[DEP_VAR]


        # Initiate XGB regressor
        xgbr = xgb.XGBRegressor(objective='reg:squarederror', tree_method = 'exact', seed=seed_)


        # HYPERPARAMETER TUNING

        # No tuning
        if not tune_hyperparams:
            best_model = xgbr

            # Impose best parameters
            if best_params_path is not None:
                saved_model = joblib.load(best_params_path)
                best_params = saved_model.get_params()
                best_model.set_params(**best_params)

        # Yes tuning
        else:

            # Define CV strategy
            if cv_strategy == "random_cv":
                cv_arg = 5
            elif cv_strategy == 'time_series_split':
                cv_arg = TimeSeriesSplit(n_splits=5)

            # If the parameter grid is a list with more than one dictionary of parameters,
            # loop across dictionaries and select the best parameters one set at a time
            for round_num, params in enumerate(parameter_grid, start=1):

                print(f"Starting hyper-parameter tuning round number: {round_num}")

                grid_search = GridSearchCV(
                    estimator=xgbr, 
                    param_grid=params, 
                    cv=cv_arg, 
                    scoring='neg_mean_squared_error', 
                    verbose=0, 
                    n_jobs=n_cpu-2
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

                # Add the best parameters from the first grid to the second grid
                if round_num==1 and len(parameter_grid)>1:
                    aux = {key: [value]  for key, value in best_params.items()}
                    parameter_grid[1].update(aux)

                # Store best parameters for this month in the results data frame
                for key, value in best_params.items():
                    results.loc[results['month'] == month, key] = value

                

            # Get the best estimator after tuning
            best_model = grid_search.best_estimator_

        # Fit the model to the training set
        best_model.fit(X_train, y_train)

        # Save the model
        if save_model_path:

            path_end_ = ['winsize' + str(time_window_size), 
                         'sample' + str(sample_df),
                         month_parsed]
            
            if tune_hyperparams:
                hyp_tune='tuned'
                path_end_.insert(0, cv_strategy)
            else:
                hyp_tune='not_tuned'
            
            path_end_ = ('_').join(path_end_) + '.pkl'

            path_ = os.path.join(save_model_path, hyp_tune, path_end_)
            joblib.dump(best_model, path_)

        # Predict and calculate error metrics
        y_pred = best_model.predict(X_test)

        # Calculate error metrics
        for em in error_metric:
                if em=='rmse':
                    rmse = np.sqrt(mean_squared_error(y_pred, y_test))
                    results.loc[results['month'] == month, em] = rmse
                elif em=='mae':
                    mae = mean_absolute_error(y_pred, y_test)
                    results.loc[results['month'] == month, em] = mae
                elif em=='mape':
                    mape = mean_absolute_percentage_error(y_pred, y_test)
                    results.loc[results['month'] == month, em] = mape
                elif em=='mdae':
                    mdae = median_absolute_error(y_pred, y_test)
                    results.loc[results['month'] == month, em] = mdae
        

        # End measuring time for this loop iteration
        end_time_loop = time.time()
        loop_duration = end_time_loop - start_time_loop
        results.loc[results['month'] == month, 'loop_duration_seconds'] = loop_duration  # Store loop duration

        if verbose>1:
            print(f"Time taken for month {month_parsed}: {loop_duration:.2f} seconds\n")

    # End measuring total execution time
    end_time_total = time.time()
    total_duration = end_time_total - start_time_total
    if verbose>0:
        print(f"Total execution time: {total_duration:.2f} seconds\n\n\n")

    return results