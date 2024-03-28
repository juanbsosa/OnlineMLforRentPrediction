import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd

def plot_error_metrics(data, metrics, show_plot=True, save_output=False, output_path=None):
    """
    Plot line graphs of the specified error metrics over time with various configurations.

    :param data: DataFrame containing the data.
    :param metrics: List of strings specifying the error metrics to plot ('rmse', 'mae', or 'mape').
    :param save_output: Boolean, if True, saves the plot as a PNG file.
    :param output_path: String, the path where the PNG file should be saved.
    """
    if not all(metric in ['rmse', 'mae', 'mape'] for metric in metrics):
        raise ValueError("Invalid metric(s). Please choose from 'rmse', 'mae', or 'mape'.")

    # Convert the 'month' column to datetime for proper plotting
    data['month'] = pd.to_datetime(data['month'])

    # Colors for different metrics
    colors = {'rmse': 'tab:blue', 'mae': 'tab:green', 'mape': 'tab:red'}

    # Single metric
    if len(metrics) == 1:
        plt.figure(figsize=(10, 6))
        plt.plot(data['month'], data[metrics[0]], marker='o', color=colors[metrics[0]])
        plt.title(f"{metrics[0].upper()} over Time")
        plt.xlabel("Month")
        plt.ylabel(metrics[0].upper())
        plt.gca().xaxis.set_major_locator(mdates.MonthLocator(interval=6))
        plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        plt.xticks(rotation=45)
        plt.grid(True)
        
        if save_output and output_path:
            final_path = f"{output_path}/{metrics[0]}.png"

    # Two metrics
    elif len(metrics) == 2:
        fig, ax1 = plt.subplots(figsize=(10, 6))
        
        ax1.set_xlabel('Month')
        ax1.set_ylabel(metrics[0].upper(), color=colors[metrics[0]])
        ax1.plot(data['month'], data[metrics[0]], marker='o', color=colors[metrics[0]])
        ax1.tick_params(axis='y', labelcolor=colors[metrics[0]])
        
        ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis
        ax2.set_ylabel(metrics[1].upper(), color=colors[metrics[1]])
        ax2.plot(data['month'], data[metrics[1]], marker='o', color=colors[metrics[1]])
        ax2.tick_params(axis='y', labelcolor=colors[metrics[1]])

        ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=6))
        ax1.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        fig.autofmt_xdate(rotation=45)
        plt.title(f"{metrics[0].upper()} and {metrics[1].upper()} over Time")
        plt.grid(True)

        if save_output and output_path:
            final_path = f"{output_path}/{'_'.join(metrics)}.png"

    # Three metrics (faceted plot)
    elif len(metrics) == 3:
        fig, axs = plt.subplots(3, 1, figsize=(10, 18), sharex=True)

        for i, metric in enumerate(metrics):
            axs[i].plot(data['month'], data[metric], marker='o', color=colors[metric])
            axs[i].set_title(metric.upper())
            axs[i].grid(True)

        plt.xlabel("Month")
        axs[0].xaxis.set_major_locator(mdates.MonthLocator(interval=6))
        axs[0].xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        fig.autofmt_xdate(rotation=45)
        
        if save_output and output_path:
            final_path = f"{output_path}/{'_'.join(metrics)}.png"

    else:
        raise ValueError("Please provide up to three metrics.")

    # Save plot
    if save_output and output_path:
        plt.savefig(final_path)

    # Display plot
    if show_plot:
        plt.show()

