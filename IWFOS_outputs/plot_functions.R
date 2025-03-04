
library(ggplot2)
# Load dplyr for data manipulation
library(dplyr)


# Function to calculate confidence interval for a proportion
Power_CI <- function(p_hat, n_simulations, confidence_level = 0.95) {
  # Validate inputs
  if(is.na(p_hat)) {
    return(list(
      lower_bound = NA,
      upper_bound = NA,
      margin_of_error = NA
    ))
  } else if (p_hat < 0 || p_hat > 1) {
    stop("p_hat must be between 0 and 1.")
  }
  if (n_simulations <= 0) {
    stop("n_simulations must be a positive number.")
  }
  if (confidence_level <= 0 || confidence_level >= 1) {
    stop("confidence_level must be between 0 and 1.")
  }
  
  # Calculate the standard error
  standard_error <- sqrt((p_hat * (1 - p_hat)) / n_simulations)
  
  # Determine the z-multiplier for the confidence level
  z_multiplier <- qnorm(1 - (1 - confidence_level) / 2)
  
  # Calculate the margin of error
  margin_of_error <- z_multiplier * standard_error
  
  # Calculate the confidence interval
  lower_bound <- p_hat - margin_of_error
  upper_bound <- p_hat + margin_of_error
  
  # Return the confidence interval as a list
  return(list(
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    margin_of_error = margin_of_error
  ))
}

SS_finder<-function(data){
  nrow_output <- nrow(data)
  N_sim <- data$n_iterations[1]
  Output <- cbind(data,
                  TWT_upp = rep(NA, nrow_output),
                  IWT_upp = rep(NA, nrow_output),
                  Nonparametric_SPM_upp = rep(NA, nrow_output),
                  ERL_upp = rep(NA, nrow_output))
  
  
  
  for (i in 1:nrow_output) {
    Output$TWT_upp[i] <- Power_CI(Output$TWT[i],
                                  n_simulations = N_sim)$upper_bound
    Output$IWT_upp[i] <- Power_CI(Output$IWT[i],
                                  n_simulations = N_sim)$upper_bound
    Output$Nonparametric_SPM_upp[i] <- Power_CI(Output$Nonparametric_SPM[i],
                                                n_simulations = N_sim)$upper_bound
    Output$ERL_upp[i] <- Power_CI(Output$ERL[i],
                                  n_simulations = N_sim)$upper_bound
    
  }
  

  # Define threshold
  threshold <- 0.80
  
  # Find minimum sample size for each combination of noise_sd and noise_fwhm
  results <- Output %>%
    group_by(noise_sd, noise_fwhm) %>%
    summarize(
      TWT_min_sample = min(sample_size[TWT_upp >= threshold], na.rm = TRUE),
      IWT_min_sample = min(sample_size[IWT_upp >= threshold], na.rm = TRUE),
      Nonparametric_SPM_min_sample = min(sample_size[Nonparametric_SPM_upp >= threshold], na.rm = TRUE),
      ERL_min_sample = min(sample_size[ERL_upp >= threshold], na.rm = TRUE),
      .groups = "drop"
    )
  # factorize noise_sd and noise_fwhm
  results$noise_sd <- factor(results$noise_sd)
  results$noise_fwhm <- factor(results$noise_fwhm)
  
  # Return the results
  return(results)
}




##################


# Function to create heatmap for a given column
create_heatmap <- function(data, value_column, title) {
  # Custom label function for "<5", "Inf" and other values
  data$custom_label <- ifelse(data[[value_column]] == 4, "<5", 
                              ifelse(data[[value_column]] == Inf, ">60",  # Cross sign for Inf
                                     as.character(data[[value_column]])))
  
  # Create the plot
  ggplot(data, aes(x = noise_fwhm, y = noise_sd, fill = !!sym(value_column))) +
    geom_tile(color = "black") +
    
    # Display cross sign or numeric values as custom labels
    geom_text(aes(label = custom_label), color = "#e9e9e9", size = 5,
              na.rm = TRUE,  fontface = "bold") +
    
    # Use a continuous color scale for the rest of the values
    scale_fill_gradient(low = "lightgrey", high = "black", na.value = "white", 
                        limits = c(5, 60),   # Set lower limit to 5 and upper limit to 60
                        breaks = c(5, 25, 45, 60),  # Custom legend breaks
                        name = "SS") +
    labs(
      x = "Noise FWHM",
      y = "Noise SD",
      fill = "Sample Size",
      title = title
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}


Data_plot <- function(dataset, TITLE){
  cont_size <- dim(dataset)[1]
  # Create a data frame with the two-sample data, excluding 'Pulse'
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), 2),  # Repeat x_values for 2 lines
    y_values = c(dataset[, 1], dataset[, 2]),  # Combine all y-values of the two columns
    # legend = factor(rep(c(colnames(dataset)[1], colnames(dataset)[2]), 
    #                     each = dim(dataset)[1]))  # Control factor levels
    legend = factor(rep(c("Group 1", "Group 2"), 
                        each = dim(dataset)[1]))  # Control factor levels
  )
  
  # Define color for both lines as black and set line types
  color_values <- setNames(c("black", "black"),
                           c("Group 1", "Group 2"))
  linetype_values <- setNames(c("longdash", "dotted"),
                              c("Group 1", "Group 2"))
  
  # Create the plot using ggplot
  ggplot(plot_data, aes(x = x_values, y = y_values, color = legend, linetype = legend)) +
    geom_line(linewidth = 1) +  # Plot lines for each group
    labs(title = TITLE, x = "Index", y = "Value") +  # Labels
    scale_color_manual(values = color_values) +  # Line colors
    scale_linetype_manual(values = linetype_values) +  # Line types
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
    theme(legend.position = "bottom") +
    # Increase the font size of the labels and axis numbers
    theme(
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
      # axis.title.x = element_text(size = 14),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      # legend.text = element_text(size = 12),
      legend.title = element_blank())
}



