# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Data from Exhibit 1
x1 <- c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00)
y1 <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)
x2 <- c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00)
y2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
x3 <- c(10.00, 8.00, 13.00, 9.00, 11.00, 14.00, 6.00, 4.00, 12.00, 7.00, 5.00)
y3 <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)
x4 <- c(8.00, 8.00, 8.00, 8.00, 8.00, 8.00, 8.00, 19.00, 8.00, 8.00, 8.00)
y4 <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)

# Create data frames for each dataset
df1 <- data.frame(x1, y1)
df2 <- data.frame(x2, y2)
df3 <- data.frame(x3, y3)
df4 <- data.frame(x4, y4)

# Plotting each dataset
p1 <- ggplot(df1, aes(x=x1, y=y1)) + geom_point() + ggtitle("Dataset 1") + theme_minimal()
p2 <- ggplot(df2, aes(x=x2, y=y2)) + geom_point() + ggtitle("Dataset 2") + theme_minimal()
p3 <- ggplot(df3, aes(x=x3, y=y3)) + geom_point() + ggtitle("Dataset 3") + theme_minimal()
p4 <- ggplot(df4, aes(x=x4, y=y4)) + geom_point() + ggtitle("Dataset 4") + theme_minimal()

# Arrange plots in a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

# Summary Statistics for each dataset
summary_stats <- function(x, y) {
  summary <- data.frame(
    Mean_X = mean(x),
    Mean_Y = mean(y),
    SD_X = sd(x),
    SD_Y = sd(y),
    Correlation = cor(x, y)
  )
  return(summary)
}

summary1 <- summary_stats(x1, y1)
summary2 <- summary_stats(x2, y2)
summary3 <- summary_stats(x3, y3)
summary4 <- summary_stats(x4, y4)

# Print summary statistics
print("Summary statistics for Dataset 1:")
print(summary1)
print("Summary statistics for Dataset 2:")
print(summary2)
print("Summary statistics for Dataset 3:")
print(summary3)
print("Summary statistics for Dataset 4:")
print(summary4)

# Linear regression for each dataset
lm1 <- lm(y1 ~ x1)
lm2 <- lm(y2 ~ x2)
lm3 <- lm(y3 ~ x3)
lm4 <- lm(y4 ~ x4)

# Print regression summaries
print("Regression summary for Dataset 1:")
summary(lm1)
print("Regression summary for Dataset 2:")
summary(lm2)
print("Regression summary for Dataset 3:")
summary(lm3)
print("Regression summary for Dataset 4:")
summary(lm4)

# Plot regression lines for each dataset
p1 + geom_smooth(method='lm', se=FALSE, color='red') + ggtitle("Dataset 1 with Regression Line")
p2 + geom_smooth(method='lm', se=FALSE, color='red') + ggtitle("Dataset 2 with Regression Line")
p3 + geom_smooth(method='lm', se=FALSE, color='red') + ggtitle("Dataset 3 with Regression Line")
p4 + geom_smooth(method='lm', se=FALSE, color='red') + ggtitle("Dataset 4 with Regression Line")

