import time
import pandas as pd
import numpy as np
import joblib
import os
from sklearn.metrics import mean_absolute_percentage_error, mean_squared_error, mean_absolute_error, median_absolute_error

def run_ensemble(
        df,
        DEP_VAR,
        INDEP_VARS,

        path_to_models,
        path_to_error_metrics,

        experiment_n,

        sample_df=False,
        starting_month=0,
        ending_month=None,

        equal_weights=True,
        cv_method=False,
        
        time_window_sizes=[1,3,6,12,24],
        error_metric = ['rmse', 'mae', 'mape', 'mdae'],

        verbose=False,
        seed_=789
):
    
    # Start measuring total execution time
    start_time_total = time.time()

    # Sort relevant lists
    error_metric.sort()
    time_window_sizes.sort()
    
    # Sort data frame by month
    df = df.sort_values(by=['listing_month'])

    # Retrieve unique months
    unique_months = sorted(df["listing_month"].unique())

    # Consider window size
    skip_months = 1
    if time_window_sizes is not False:
        skip_months = skip_months + max(time_window_sizes) - 1

    # Relevant unique months
    unique_months_rel = unique_months[max([starting_month, skip_months]): ending_month]

    # Subset dataframe according to starting and ending months
    df = df.loc[(df['listing_month'].isin(unique_months_rel))].copy()

    # If specified, take a sample of the data frame
    if sample_df:
        df = df.sample(n=sample_df)

    # Create a data frame to store the results for the error metrics in all months
    results = pd.DataFrame(
        {'month': unique_months_rel}
        )

    # Add columns for the error metrics
    for em in error_metric:
        results[em] = np.nan

    if equal_weights==False:
        # Create df to store previously estimated models' error metrics
        error_metrics_df = pd.DataFrame({'month': unique_months_rel})

        # Load error metrics of previously estimated models to calculate weights
        for ws in time_window_sizes:

            # Load error metrics for each window size        
            path_ = os.path.join(path_to_error_metrics, 
                                f'exp{experiment_n}{ws}_notuning.csv')
            error_metrics_model = pd.read_csv(path_)

            # Convert the 'listing_month' column to datetime format
            error_metrics_model['month'] = pd.to_datetime(error_metrics_model['month'])

            # Subset dataframe according to starting and ending months
            error_metrics_model = error_metrics_model.loc[
                (error_metrics_model['month'].isin(unique_months_rel))].copy()
            
            # Add them to general error metrics df
            for em in error_metric:
                error_metrics_df[em + str(ws)] = error_metrics_model[em].values

        # Normalize error metrics across all models to make them easily comparable
        for em in error_metric:
            # !!! Ojo con esto, funciona pero si tuviese dos metricas que empiezan igual no sirve
            vars_em = [x for x in error_metrics_df.columns if x.startswith(em)]

            # Calculate row sums
            row_sums = error_metrics_df[vars_em].sum(axis=1)

            # Normalize columns
            error_metrics_df[vars_em] = error_metrics_df[vars_em].div(row_sums, axis=0)                       

    # Start iteration to create ensemble, 
    # considering each month (except the first) as the test sample.
    for month in unique_months_rel:

        # For the weighted method, you have to start one month later because there is
        # no previous error metric to use
        if (month==unique_months_rel[0]) & (equal_weights==False) & (starting_month<skip_months):
            continue

        # Parse month string
        if isinstance(month, pd.Timestamp):
            month_parsed = month.strftime('%Y-%m')
        else:
            month_parsed = np.datetime_as_string(month, unit='M')

        # Report time frame chosen
        if verbose>1:
            print(f"Starting estimation for month: {month_parsed} \n")

        # Test set
        df_subset_test = df.loc[df['listing_month'] == month].copy()        

        # Define training and test samples
        X_test = df_subset_test[INDEP_VARS]
        y_test = df_subset_test[DEP_VAR]

        # Load models to build ensemble
        ensemble=[]

        for ws in time_window_sizes:
            # Load model and append it to list
            path_ = os.path.join(path_to_models, 
                             f'winsize{ws}_sample{sample_df}_{month_parsed}.pkl')
            model = joblib.load(path_)
            ensemble.append(model)

        # Save ensemble as pkl file
        if path_to_models:
            if equal_weights:
                weights_lab = 'equalw'
            else:
                weights_lab = 'unequalw'
            path_end_ = ['winsize', ('-').join([str(ws) for ws in time_window_sizes]),
                         weights_lab, 'sample' + str(sample_df),
                         month_parsed]            
            path_end_ = ('_').join(path_end_) + '.pkl'

            path_ = os.path.join(path_to_models, path_end_)
            joblib.dump(ensemble, path_)

        # Make prediction using each error metric as weights         
        for em in error_metric:
            
            # Select corresponding weights
            # !!! Asegurarse de que tanto los modelos en ensemble como las metricas de erorr
            # usadas como pesos esten en el mismo orden
            if equal_weights:
                weights=None
            else:
                # !!! Ojo con esto, funciona pero si tuviese dos metricas que empiezan igual no sirve
                vars_em = [x for x in error_metrics_df.columns if x.startswith(em)]
                previous_month = month - pd.DateOffset(months=1)
                previous_month_metrics = error_metrics_df[(error_metrics_df["month"]==previous_month)][vars_em]
                weights = previous_month_metrics.iloc[0,].values.flatten().tolist()

                # Cross-validation method
                if cv_method:
                    weights = [1 if w==min(weights) else 0 for w in weights]

                    #Report model chosen
                    if verbose>1:
                        print(f"Model with smallest error in this month: {previous_month_metrics.columns[weights.index(1)]} \n")
                    
                # Inverse of error metric method
                else:
                    weights = [1/w for w in weights]
                    weights = [w/sum(weights) for w in weights]


            # Make prediction
            y_pred = ensemble_predict(ensemble, X_test, weights=weights)

            # Calculate error metric and add it to results df
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

    # End measuring total execution time
    end_time_total = time.time()
    total_duration = end_time_total - start_time_total
    if verbose>0:
        print(f"Total execution time: {total_duration:.2f} seconds")

    return results



# Function to make predictions with ensemble and weight them if necessary
def ensemble_predict(models, input_data, weights=None):

    # Generate predictions from each model
    predictions = np.array([model.predict(input_data) for model in models])

    if weights is not None:
        # Ensure the weights sum to 1
        # !!! redundant
        weights = np.array(weights) / np.sum(np.array(weights))
        # Compute the weighted average
        weights = weights.flatten()
        weighted_prediction = np.average(predictions, axis=0, weights=weights)
        return weighted_prediction
    else:
        # Compute the simple average
        avg_prediction = np.mean(predictions, axis=0)
        return avg_prediction