# Load required packages
library(psych)    # For psychological statistics functions
library(stats)    # For basic statistics (usually loaded by default in R)
library(boot)     # For bootstrap analysis
library(rcompanion)
library(effectsize)
library(conflicted)

# Handle conflicts
conflict_prefer("alpha", "psych")

# Function to analyze response patterns
analyze_response_patterns <- function(data) {
  output <- ""
  output <- paste0(output, "RESPONSE PATTERN ANALYSIS\n")
  output <- paste0(output, "========================\n\n")
  
  # Analyze each item
  for(col in names(data)) {
    output <- paste0(output, sprintf("\nItem %s Analysis:\n", col))
    output <- paste0(output, "-----------------\n")
    
    # Get actual response range
    actual_range <- range(data[[col]])
    unused_responses <- setdiff(1:5, unique(data[[col]]))
    
    # Calculate frequencies
    freq_table <- table(data[[col]])
    prop_table <- prop.table(freq_table) * 100
    
    # Add basic statistics
    output <- paste0(output, sprintf("Response range: %d to %d\n", actual_range[1], actual_range[2]))
    if(length(unused_responses) > 0) {
      output <- paste0(output, "Unused response options: ", 
                       paste(unused_responses, collapse = ", "), "\n")
    }
    
    # Add distribution
    output <- paste0(output, "\nResponse Distribution:\n")
    for(i in sort(as.numeric(names(freq_table)))) {
      output <- paste0(output, sprintf("Response %d: n=%d (%.1f%%)\n", 
                                       i, freq_table[i], prop_table[i]))
    }
    
    # Check for potential ceiling/floor effects
    if(prop_table[1] > 15 || prop_table[length(prop_table)] > 15) {
      output <- paste0(output, "\nNote: Potential floor/ceiling effect detected\n")
    }
  }
  return(output)
}

# Updated Function for hypothesis testing
test_perception <- function(data, item_name, hypothesis_name, conf.level = 0.95, adjusted_alpha = 0.00385) {
  # Extract the data for the item
  item_data <- as.numeric(data[[item_name]])
  n <- length(item_data)
  
  # Basic statistics
  median_score <- median(item_data)
  iqr_score <- IQR(item_data)
  
  # Hodges-Lehmann estimator for confidence interval
  hl_estimate <- wilcox.test(item_data, mu = 3, alternative = "greater", conf.int = TRUE, conf.level = conf.level)
  hl_ci <- hl_estimate$conf.int
  
  # Percentage above neutral (>3)
  percent_above <- sum(item_data > 3) / n * 100
  
  # Perform one-tailed Wilcoxon signed-rank test
  wilcox_result <- wilcox.test(item_data, mu = 3, alternative = "greater", exact = FALSE, correct = FALSE)
  
  # Extract p-value
  p_val <- wilcox_result$p.value
  
  # Adjust p-value for multiple comparisons using Bonferroni correction
  p_val_adjusted <- p_val * 13  # Multiply by number of tests
  p_val_adjusted <- min(p_val_adjusted, 1)  # Ensure p-value does not exceed 1
  
  # Calculate effect size (r)
  z_stat <- qnorm(p_val / 2, lower.tail = FALSE) * sign(wilcox_result$statistic - (n * (n + 1) / 4))
  effect_size <- abs(z_stat) / sqrt(n)
  
  # Get effect size magnitude label based on absolute value
  effect_size_label <- ifelse(effect_size < 0.3, "small",
                              ifelse(effect_size < 0.5, "medium", "large"))
  
  # Format results for printing
  cat(sprintf("\nHypothesis %s:\n", hypothesis_name))
  cat("-----------------\n")
  cat(sprintf("Median: %.2f\n", median_score))
  cat(sprintf("Interquartile Range (IQR): %.2f\n", iqr_score))
  cat(sprintf("Hodges-Lehmann estimate: %.2f\n", hl_estimate$estimate))
  cat(sprintf("95%% Confidence Interval of the Median Difference: [%.2f, Inf)\n", hl_ci[1]))
  cat(sprintf("Percentage above neutral: %.1f%%\n", percent_above))
  
  # Print adjusted p-value with scientific notation for very small values
  if(p_val_adjusted < adjusted_alpha) {
    cat(sprintf("Adjusted p-value: %.3e (significant at adjusted alpha = %.5f)\n", p_val_adjusted, adjusted_alpha))
  } else {
    cat(sprintf("Adjusted p-value: %.5f\n", p_val_adjusted))
  }
  
  cat(sprintf("Effect size (r): %.3f (%s)\n", effect_size, effect_size_label))
  
  # Add interpretation
  cat("\nInterpretation: ")
  if(!is.na(p_val_adjusted) && p_val_adjusted < adjusted_alpha) {
    cat("The median is significantly greater than the neutral point (3).\n")
  } else {
    cat("No significant difference from the neutral point (3).\n")
  }
  
  # Return results as a list
  invisible(list(
    hypothesis = hypothesis_name,
    median = median_score,
    iqr = iqr_score,
    n = n,
    hl_estimate = hl_estimate$estimate,
    hl_ci_lower = hl_ci[1],
    percent_above = percent_above,
    p_value = p_val_adjusted,
    effect_size = effect_size,
    effect_size_label = effect_size_label
  ))
}

# Main analysis function
main_analysis <- function(data, output_file = "analysis_results.txt") {
  # Open connection to output file
  sink(output_file)
  
  # Write header
  cat("ANALYSIS RESULTS\n")
  cat("================\n")
  cat("Date of analysis:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # 1. Descriptive Statistics
  cat("1. DESCRIPTIVE STATISTICS\n")
  cat("=======================\n")
  desc_stats <- describe(data)
  print(round(desc_stats[,c("n", "median", "trimmed", "mad", "min", "max", "skew", "kurtosis")], 3))
  cat("\n")
  
  # 2. Response Distribution Analysis
  cat("2. RESPONSE DISTRIBUTION ANALYSIS\n")
  cat("===============================\n")
  for(col in names(data)) {
    cat(sprintf("\n%s:\n", col))
    freq_table <- table(data[[col]])
    prop_table <- prop.table(freq_table) * 100
    print(data.frame(
      Response = as.numeric(names(freq_table)),
      Frequency = as.numeric(freq_table),
      Percentage = round(prop_table, 1)
    ))
  }
  
  # 3. Polychoric Correlations
  cat("\n3. POLYCHORIC CORRELATIONS\n")
  cat("=========================\n")
  poly_cor <- suppressWarnings(polychoric(data))
  print(round(poly_cor$rho, 3))
  
  # 4. Reliability Analysis
  cat("\n4. RELIABILITY ANALYSIS\n")
  cat("=====================\n")
  ordinal_alpha <- psych::alpha(poly_cor$rho)
  cat(sprintf("Ordinal alpha: %.3f\n", ordinal_alpha$total$raw_alpha))
  
  # 5. Hypothesis Testing
  cat("\n5. HYPOTHESIS TESTING\n")
  cat("===================\n")
  
  # Adjusted alpha level for Bonferroni correction
  adjusted_alpha <- 0.05 / 13
  
  # Test hypotheses
  hypotheses <- list(
    h1a = test_perception(data, "Q1", "H1a", adjusted_alpha = adjusted_alpha),
    h1b = test_perception(data, "Q2", "H1b", adjusted_alpha = adjusted_alpha),
    h1c = test_perception(data, "Q3", "H1c", adjusted_alpha = adjusted_alpha),
    h1d = test_perception(data, "Q4", "H1d", adjusted_alpha = adjusted_alpha),
    h2a = test_perception(data, "Q5", "H2a", adjusted_alpha = adjusted_alpha),
    h2b = test_perception(data, "Q6", "H2b", adjusted_alpha = adjusted_alpha),
    h2c = test_perception(data, "Q7", "H2c", adjusted_alpha = adjusted_alpha),
    h2d = test_perception(data, "Q8", "H2d", adjusted_alpha = adjusted_alpha),
    h2e = test_perception(data, "Q9", "H2e", adjusted_alpha = adjusted_alpha),
    h3a = test_perception(data, "Q10", "H3a", adjusted_alpha = adjusted_alpha),
    h3b = test_perception(data, "Q11", "H3b", adjusted_alpha = adjusted_alpha),
    h4a = test_perception(data, "Q12", "H4a", adjusted_alpha = adjusted_alpha),
    h4b = test_perception(data, "Q13", "H4b", adjusted_alpha = adjusted_alpha)
  )
  
  # Close the text file connection
  sink()
  
  # Create both PDF and PNG files for plots
  # PDF version
  pdf("analysis_plots_new_final.pdf", width = 12, height = 8)
  
  # Function to create plots
  create_plots <- function() {
    # 1. Response distribution plots
    par(mfrow=c(2,2))
    for(col in names(data)) {
      barplot(table(data[[col]]),
              main = sprintf("Response distribution: %s", col),
              xlab = "Response",
              ylab = "Frequency",
              col = "skyblue")
    }
    
    # 2. Differences from neutral point (3) distributions
    par(mfrow=c(2,2))
    for(col in names(data)) {
      differences <- data[[col]] - 3
      hist(differences,
           main = sprintf("Differences from neutral: %s", col),
           xlab = "Difference from neutral (3)",
           ylab = "Frequency",
           breaks = seq(min(differences)-0.5, max(differences)+0.5, by=0.5),
           col = "lightblue",
           border = "black")
      abline(v = 0, col = "red", lty = 2)
    }
    
    # 3. Box plots of differences
    par(mfrow=c(1,1))
    differences_matrix <- as.matrix(data) - 3
    boxplot(differences_matrix,
            main = "Differences from neutral point (3)",
            ylab = "Difference score",
            names = paste0("Q", 1:13),
            col = "lightblue",
            notch = FALSE)
    abline(h = 0, col = "red", lty = 2)
    
    # 4. Correlation heat map
    heatmap(poly_cor$rho,
            main = "Polychoric correlation heatmap",
            col = heat.colors(12))
  }
  
  # Create plots in PDF
  create_plots()
  dev.off()
  
  # Using 300 dpi and 2250 pixels width for full-page width figures
   # Response distributions
  png("response_distributions.png", width = 2250, height = 2250, res = 300)
  par(mfrow = c(4, 4),
      mar = c(4, 4, 4, 2),
      mgp = c(2.5, 1, 0))
  
  for (col in names(data)) {
    # Calculate frequencies
    freq_table <- table(factor(data[[col]], levels = 1:5))  # Force all levels 1-5
    max_count <- max(freq_table)
    
    # Set ylim to accommodate text labels
    ylim_upper <- max_count * 1.15  # Add 15% extra space for labels
    
    # Create histogram
    hist_obj <- hist(data[[col]],
                     main = sprintf("Distribution of responses: %s", col),
                     xlab = "Response value",
                     ylab = "Frequency",
                     breaks = seq(0.5, 5.5, by = 1),
                     col = "lightblue",
                     border = "black",
                     xlim = c(0.5, 5.5),
                     ylim = c(0, ylim_upper),
                     cex.main = 0.9,
                     cex.lab = 0.8,
                     cex.axis = 0.8)
    
    # Positions for labels
    x_positions <- 1:5
    y_positions <- as.numeric(freq_table)
    
    # Calculate label positions and properties
    threshold <- max_count * 0.25  # Increased threshold for better visibility
    label_inside <- y_positions >= threshold
    
    # Fine-tuned positioning
    label_positions <- ifelse(label_inside,
                              y_positions * 0.5,  # Center of bar for inside labels
                              y_positions + (max_count * 0.05))  # Slightly above bar
    
    label_colors <- ifelse(label_inside, "black", "black")
    
    # Add text labels with adjusted vertical position
    for (i in 1:length(x_positions)) {
      if (y_positions[i] > 0) {  # Only show non-zero values
        if (label_inside[i]) {
          # Inside bar
          text(x = x_positions[i],
               y = label_positions[i],
               labels = y_positions[i],
               col = label_colors[i],
               cex = 0.8)
        } else {
          # Above bar with small offset
          text(x = x_positions[i],
               y = y_positions[i] + (max_count * 0.03),
               labels = y_positions[i],
               col = label_colors[i],
               cex = 0.8)
        }
      }
    }
  }
  
  # Turn off the unused 16th plot position
  par(mfg = c(4, 4))
  plot.new()
  dev.off()
  
  # Difference histograms
  png("difference_histograms.png", width = 2250, height = 2250, res = 300)
  par(mfrow = c(4, 4),
      mar = c(4, 4, 4, 2),
      mgp = c(2.5, 1, 0))
  
  for (col in names(data)) {
    differences <- data[[col]] - 3
    
    # Calculate range for proper ylim
    temp_hist <- hist(differences, plot = FALSE, 
                      breaks = seq(-2.5, 2.5, by = 1))
    max_count <- max(temp_hist$counts)
    ylim_upper <- max_count * 1.15  # Add 15% space for labels
    
    # Create histogram
    hist_obj <- hist(differences,
                     main = sprintf("Differences from neutral: %s", col),
                     xlab = "Difference from neutral (3)",
                     ylab = "Frequency",
                     breaks = seq(-2.5, 2.5, by = 1),
                     col = "lightblue",
                     border = "black",
                     xlim = c(-3, 3),
                     ylim = c(0, ylim_upper),
                     cex.main = 0.9,
                     cex.lab = 0.8,
                     cex.axis = 0.8)
    
    # Calculate label positions and properties
    threshold <- max_count * 0.25
    label_inside <- hist_obj$counts >= threshold
    
    # Add text labels with improved positioning
    for (i in 1:length(hist_obj$counts)) {
      if (hist_obj$counts[i] > 0) {  # Only show non-zero values
        if (label_inside[i]) {
          # Inside bar
          text(x = hist_obj$mids[i],
               y = hist_obj$counts[i] * 0.5,
               labels = hist_obj$counts[i],
               col = "black",
               cex = 0.8)
        } else {
          # Above bar with small offset
          text(x = hist_obj$mids[i],
               y = hist_obj$counts[i] + (max_count * 0.03),
               labels = hist_obj$counts[i],
               col = "black",
               cex = 0.8)
        }
      }
    }
    
    # Add vertical line at zero
    abline(v = 0, col = "red", lty = 2)
  }
   
  # Turn off the unused 16th plot position
  par(mfg=c(4,4))
  plot.new()
  dev.off()  
 
  # Box plots
  png("difference_boxplots.png", width = 2250, height = 1500, res = 300)
  par(mar=c(4,4,4,2), mgp=c(2.5,1,0))
  differences_matrix <- as.matrix(data) - 3
  boxplot(differences_matrix,
          main = "Differences from neutral point (3)",
          ylab = "Difference score",
          names = paste0("Q", 1:13),
          col = "lightblue",
          notch = FALSE,
          cex.main = 0.9,
          cex.lab = 0.8,
          cex.axis = 0.8)
  abline(h = 0, col = "red", lty = 2)
  dev.off()
  
  # Correlation heatmap
  png("correlation_heatmap.png", width = 2250, height = 1500, res = 300)
  par(mar=c(4,4,4,2), mgp=c(2.5,1,0))
  heatmap(poly_cor$rho,
          main = "Polychoric correlation heatmap",
          col = heat.colors(12))
  dev.off()
  
  # Print file locations
  cat("\nResults have been saved to:", normalizePath(output_file))
  cat("\nPlots have been saved to:", normalizePath("analysis_plots_new_final.pdf"))
  
  # Return results
  invisible(list(
    desc_stats = desc_stats,
    poly_cor = poly_cor,
    ordinal_alpha = ordinal_alpha,
    hypothesis_results = hypotheses
  ))
}

# Survey data
data <- read.table(text = "3,3,4,4,4,5,5,4,5,5,4,4,4
3,3,4,4,4,4,4,4,4,4,5,5,5
2,1,3,4,4,4,4,3,3,4,4,4,3
3,2,4,4,4,5,5,4,4,5,4,4,5
3,2,3,4,4,4,4,4,5,5,5,4,4
2,2,3,4,5,4,3,3,3,3,4,4,3
3,2,3,4,4,4,5,4,4,4,4,4,4
3,4,3,5,4,5,5,4,4,5,5,5,5
3,4,3,4,5,4,5,5,5,4,5,4,5
3,3,4,4,4,5,4,5,4,5,4,5,4
2,3,3,4,4,4,4,4,3,3,4,4,3
2,3,3,5,4,4,5,4,4,5,5,4,3
3,3,3,4,4,4,4,4,4,4,5,4,4
2,2,4,4,4,5,4,4,4,5,4,4,4
3,3,4,5,4,5,4,5,4,4,5,5,4
2,2,3,3,4,3,4,4,3,4,3,4,3
2,3,3,4,4,5,5,4,4,5,4,5,3
4,4,4,5,5,5,5,5,5,5,5,4,5
2,3,3,4,5,4,3,4,3,4,5,4,3
3,3,4,5,4,5,5,4,4,4,5,5,4
3,3,4,4,5,4,4,5,4,5,5,5,5
3,2,3,5,4,5,4,4,4,5,5,4,5
3,3,3,5,5,5,5,5,5,5,5,4,4
1,3,4,4,5,5,5,4,4,5,5,5,5
2,2,4,4,5,5,5,5,4,5,5,4,4
3,3,4,5,4,5,5,5,5,5,5,5,5
2,2,3,4,4,5,4,4,4,5,3,4,4
2,2,4,4,4,5,4,4,5,4,5,5,4
2,3,4,4,5,5,4,5,4,5,5,5,4
2,2,3,3,4,4,3,4,3,3,5,4,3
2,2,3,5,4,5,4,4,5,4,5,4,2
3,3,4,3,4,4,5,4,5,4,4,5,4
2,2,3,4,5,4,4,3,3,5,4,4,4
2,3,4,4,4,4,4,4,4,4,4,4,3
3,2,3,3,5,4,4,4,4,4,4,4,4
3,2,3,4,4,4,3,4,4,4,5,4,4
3,3,3,3,4,5,4,4,4,4,4,4,4
2,3,3,5,5,5,4,4,4,5,5,3,4
3,3,4,4,4,4,5,4,4,4,5,5,4
3,3,4,4,4,4,4,5,4,5,4,5,4
3,2,4,4,5,5,5,5,4,4,5,5,5
2,3,4,5,4,5,4,4,4,5,5,5,4
3,3,4,4,5,5,5,5,5,5,4,5,4
2,1,2,4,3,4,3,4,3,3,4,3,3
2,2,3,4,4,4,4,4,3,4,4,4,4
3,3,3,5,4,5,5,4,5,5,5,5,5
2,3,3,4,5,5,5,5,5,5,4,5,3
3,3,3,4,5,5,5,4,5,5,5,5,3
2,2,3,4,3,5,4,4,3,4,4,5,3
3,2,3,4,5,5,4,4,5,4,5,4,4
2,3,4,5,4,4,5,4,3,4,4,4,3
2,2,3,5,5,5,4,4,4,5,5,4,4
3,3,4,4,4,4,4,5,3,5,4,4,4
2,1,3,4,4,3,4,4,3,3,5,4,4
3,2,3,4,4,5,4,4,4,5,4,5,4
1,3,3,4,4,4,3,4,3,4,4,2,3
3,3,4,5,5,5,5,5,5,5,5,4,5
2,1,3,4,4,5,4,4,4,5,4,4,4
3,2,4,4,4,3,5,4,4,5,5,4,4
3,2,4,4,4,5,4,4,4,5,4,5,4
2,3,3,4,5,4,4,4,4,5,5,4,3
2,3,3,5,4,5,5,5,4,5,5,5,4
3,3,4,5,4,4,5,4,4,4,5,5,4
4,4,5,5,4,4,4,4,4,5,5,4,5
3,3,4,5,5,4,5,5,5,4,5,4,4
3,3,3,4,5,5,4,4,4,4,4,4,4
3,2,3,4,4,5,4,4,3,4,5,4,4
2,2,3,4,4,5,4,4,4,5,5,4,4
1,3,4,4,4,5,4,3,5,4,4,4,3
1,1,2,3,3,4,4,4,3,4,3,4,4
3,3,3,4,4,5,4,4,5,5,5,5,5
4,4,4,5,5,5,5,4,5,5,5,4,4
2,3,3,4,5,5,4,3,4,4,4,4,4
3,3,3,5,5,4,5,4,4,5,5,5,4
3,3,4,4,5,5,4,5,5,5,5,4,4
1,3,3,3,4,4,3,5,4,4,5,4,3
3,3,4,5,4,4,4,4,4,4,5,4,4
3,3,4,4,5,5,5,4,4,5,4,5,4
3,2,3,4,4,5,4,4,4,5,4,4,5
3,2,4,4,4,4,5,4,4,4,5,5,4
2,2,3,5,5,5,4,5,5,5,4,4,3
3,3,3,5,3,5,4,4,3,5,4,4,4
2,3,4,5,5,5,5,4,5,5,4,5,4
2,2,3,4,3,4,4,3,4,5,4,5,4
2,2,4,3,5,4,4,4,4,5,5,5,4
2,2,3,5,5,4,4,4,4,4,5,4,5
2,2,3,3,4,4,4,4,3,5,5,4,3
3,2,4,4,5,4,3,3,4,5,4,4,4
2,3,4,5,4,4,4,4,4,4,5,5,5
2,2,3,4,4,4,3,3,4,4,4,4,4
1,3,3,4,4,3,4,4,4,4,5,4,2
2,2,3,3,5,4,4,4,4,5,4,4,4
2,2,4,3,5,4,5,4,4,4,4,4,4
3,2,4,5,4,5,5,4,4,5,5,4,4
3,2,2,4,4,4,4,4,3,4,4,4,3
4,3,4,4,5,5,4,5,4,5,4,4,4
1,1,3,3,3,4,3,4,3,4,3,3,3
1,2,3,4,3,4,4,4,3,5,4,4,4
2,2,4,5,4,4,4,5,4,5,4,5,4
3,3,4,5,4,5,4,5,4,5,5,4,5
4,3,4,4,4,4,5,4,5,4,4,5,4
3,2,3,4,4,4,4,4,4,5,4,5,4
3,3,4,4,3,5,4,5,3,5,4,5,4
3,2,3,5,4,5,4,5,3,5,4,4,4
2,2,4,5,5,5,5,4,5,5,5,4,4
2,3,4,5,5,4,5,4,4,4,5,5,4
3,3,4,5,4,4,5,5,4,5,5,4,4
4,3,4,5,5,5,5,5,5,4,5,5,5
3,3,3,5,5,5,5,4,4,5,4,4,4
2,2,3,4,4,4,4,4,3,5,4,4,4
3,3,4,4,5,4,5,4,5,5,5,4,4
2,2,2,4,5,4,4,4,3,5,4,3,4
3,3,5,5,5,5,4,5,4,5,5,5,5
2,2,3,5,4,5,4,5,5,5,5,4,4
2,3,3,4,4,4,5,4,4,4,4,4,4
2,2,3,4,4,4,5,5,4,5,5,4,3
2,3,3,4,4,4,5,4,5,5,5,4,4
2,3,4,5,4,5,5,5,5,5,5,5,4
3,3,4,4,5,5,4,4,5,5,5,5,4
4,3,3,5,5,4,5,5,5,5,5,4,5
3,2,3,5,4,4,4,4,4,5,5,5,4
3,3,4,5,5,5,4,5,5,4,5,5,5
3,3,4,4,5,5,5,4,4,5,5,4,4
2,3,4,4,5,4,4,5,4,5,5,4,4
1,3,3,3,3,3,4,3,3,4,4,3,3
2,3,4,4,5,5,5,4,5,5,5,4,5
2,2,4,5,5,5,4,4,3,5,4,4,4
3,2,3,4,5,5,5,3,4,5,4,5,4
3,3,4,5,5,5,5,3,4,5,5,4,5
2,2,3,5,5,5,4,4,4,5,5,4,4
1,2,3,4,4,4,4,4,3,4,4,4,3
2,3,3,4,4,5,4,4,4,5,4,5,4
3,3,4,4,4,5,5,4,5,5,3,5,3
4,3,3,5,5,5,5,5,4,5,4,4,4
3,4,4,5,5,5,5,4,5,5,5,5,4
2,2,3,4,4,5,3,4,4,5,3,4,5
3,3,4,5,5,5,5,4,4,5,5,5,5
2,3,4,4,4,4,4,4,3,4,4,4,4
2,2,3,3,3,3,3,4,4,4,3,4,3
3,3,4,5,5,5,5,5,4,5,5,4,5", sep=",", header=FALSE)

# Name the columns
names(data) <- paste0("Q", 1:13)

# Run the analysis
results <- main_analysis(data, "new_analysis_results_final.txt")
