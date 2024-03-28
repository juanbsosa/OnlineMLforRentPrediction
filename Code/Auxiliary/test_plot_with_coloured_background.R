library(ggplot2)

# Example data
df <- data.frame(x = 1:10, y = c(3, 5, 4, 6, 8, 7, 6, 9, 5, 4), color_var = c(1, 1, 2, 2, 2, 1, 1, 2, 2, 1))

# Create a plot with changing background color
ggplot(df, aes(x = x, y = y)) +
    geom_rect(aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf, fill = factor(color_var))) +
    geom_line() +
    scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen")) +
    theme_minimal()


library(ggplot2)

# Example data
df <- data.frame(x = 1:10, y = c(3, 5, 4, 6, 8, 7, 6, 9, 5, 4), color_var = c(1, 2, 3, 4, 5, 2, 3, 2, 9, 12))

# Create a plot with changing background color based on a numeric variable
ggplot(df, aes(x = x, y = y)) +
    geom_rect(aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf, fill = color_var)) +
    geom_line() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal()

