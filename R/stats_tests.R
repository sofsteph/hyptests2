# we are going to make 3 functions that will be updated hypothesis test functions
# we want nice outputs and visuals for each test

# t test

t_test_2.0 <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95) {
  # Perform the t-test using the built-in function
  t_result <- t.test(x = x, y = y, alternative = alternative, mu = mu,
                     paired = paired, var.equal = var.equal, conf.level = conf.level)

  # Generate the data visualization
  plot_data <- if (is.null(y)) {
    data.frame(Value = x, Group = "Sample")
  } else {
    data.frame(Value = c(x, y),
               Group = rep(c("Group 1", "Group 2"), c(length(x), length(y))))
  }

  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting. Please install it.")
  }
  library(ggplot2)

  # Create the boxplot visualization
  boxplot <- ggplot(plot_data, aes(x = Group, y = Value)) +
    geom_boxplot(outlier.color = "red", fill = "skyblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    stat_summary(fun = mean, geom = "point", color = "darkblue", size = 3, shape = 17) +
    labs(title = "T-Test Results",
         subtitle = paste("p-value:", signif(t_result$p.value, 3)),
         x = "Group",
         y = "Values") +
    theme_minimal()

  # Create the p-value visualization
  df <- if (is.null(y)) length(x) - 1 else length(x) + length(y) - 2
  x_vals <- seq(-4, 4, length.out = 1000)
  t_dist <- dt(x_vals, df)
  p_val <- t_result$p.value
  t_stat <- t_result$statistic


  shaded_area <- ggplot(data.frame(x = x_vals, y = t_dist), aes(x, y)) +
    geom_line(color = "black") +
    geom_area(data = subset(data.frame(x = x_vals, y = t_dist),
                            x >= abs(t_stat) | x <= -abs(t_stat)),
              aes(x, y), fill = "skyblue", alpha = 0.5) +
    geom_vline(xintercept = c(-abs(t_stat), abs(t_stat)), color = "red", linetype = "dashed") +
    annotate("text", x = abs(t_stat), y = max(t_dist) * 0.8,
             label = paste("t =", signif(t_stat, 3)), color = "red", hjust = -0.1) +
    annotate("text", x = 0, y = max(t_dist) * 0.5,
             label = paste("p =", signif(p_val, 3)), color = "blue", hjust = 0.5) +
    labs(title = "t-Value Visualization",
         x = "t-value",
         y = "Density") +
    theme_minimal()

  # Print results
  print(t_result)
  print(boxplot)
  print(shaded_area)

  # Return the original t.test result for further use
  return(t_result)
}

# Chi-squared test
chi_square.2.0 <- function(observed, simulate.p.value = FALSE, rescale.p = FALSE) {
  # Perform the chi-square test
  test_result <- chisq.test(observed, simulate.p.value = simulate.p.value, rescale.p = rescale.p)

  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting. Please install it.")
  }
  library(ggplot2)

  # Visualize the contingency table as a heat map
  observed_df <- as.data.frame(as.table(observed))
  heatmap_plot <- ggplot(observed_df, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 4) +
    scale_fill_gradient(low = "skyblue", high = "darkblue") +
    labs(title = "Observed Frequencies",
         x = "Category 1",
         y = "Category 2",
         fill = "Frequency") +
    theme_minimal()

  # Visualize the chi-square distribution and p-value
  df <- test_result$parameter
  x_vals <- seq(0, max(15, test_result$statistic * 1.5), length.out = 1000)
  chi_dist <- dchisq(x_vals, df)

  p_value_plot <- ggplot(data.frame(x = x_vals, y = chi_dist), aes(x, y)) +
    geom_line(color = "black") +
    geom_area(data = subset(data.frame(x = x_vals, y = chi_dist),
                            x >= test_result$statistic),
              aes(x, y), fill = "skyblue", alpha = 0.5) +
    geom_vline(xintercept = test_result$statistic, color = "red", linetype = "dashed") +
    annotate("text", x = test_result$statistic, y = max(chi_dist) * 0.8,
             label = paste("χ² =", signif(test_result$statistic, 3)), color = "red", hjust = -0.1) +
    annotate("text", x = max(x_vals) * 0.5, y = max(chi_dist) * 0.5,
             label = paste("p =", signif(test_result$p.value, 3)), color = "blue", hjust = 0.5) +
    labs(title = "Chi-Square Distribution",
         x = "Chi-Square Value",
         y = "Density") +
    theme_minimal()

  # Print results
  print(test_result)
  print(heatmap_plot)
  print(p_value_plot)

  # Return the original chisq.test result for further use
  return(test_result)
}

# ANOVA

anova.2.0 <- function(data, formula) {
  # Perform ANOVA
  aov_result <- aov(formula, data = data)
  summary_result <- summary(aov_result)

  # Extract F-statistic and p-value
  f_stat <- summary_result[[1]][["F value"]][1]
  p_value <- summary_result[[1]][["Pr(>F)"]][1]
  df1 <- summary_result[[1]][["Df"]][1]
  df2 <- summary_result[[1]][["Df"]][2]

  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting. Please install it.")
  }
  library(ggplot2)

  # Visualize group distributions
  boxplot <- ggplot(data, aes(x = as.factor(eval(parse(text = all.vars(formula)[2]))),
                              y = eval(parse(text = all.vars(formula)[1])))) +
    geom_boxplot(fill = "skyblue", alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", color = "darkblue", size = 3, shape = 17) +
    labs(title = "Group Comparisons",
         x = "Groups",
         y = "Values") +
    theme_minimal()

  # Visualize F-distribution and p-value
  x_vals <- seq(0, max(10, f_stat * 1.5), length.out = 1000)
  f_dist <- df(x_vals, df1, df2)

  p_value_plot <- ggplot(data.frame(x = x_vals, y = f_dist), aes(x, y)) +
    geom_line(color = "black") +
    geom_area(data = subset(data.frame(x = x_vals, y = f_dist),
                            x >= f_stat),
              aes(x, y), fill = "skyblue", alpha = 0.5) +
    geom_vline(xintercept = f_stat, color = "red", linetype = "dashed") +
    annotate("text", x = f_stat, y = max(f_dist) * 0.8,
             label = paste("F =", signif(f_stat, 3)), color = "red", hjust = -0.1) +
    annotate("text", x = max(x_vals) * 0.5, y = max(f_dist) * 0.5,
             label = paste("p =", signif(p_value, 3)), color = "blue", hjust = 0.5) +
    labs(title = "F-Distribution Visualization",
         x = "F-Statistic",
         y = "Density") +
    theme_minimal()

  # Print results
  print(summary_result)
  print(boxplot)
  print(p_value_plot)

  # Return the ANOVA result
  return(aov_result)
}
