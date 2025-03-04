source("IWFOS_outputs/plot_functions.R")
########################
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
##########################
#####Angle data############

# Load necessary libraries
Output_Angle <- read_excel("IWFOS_outputs/Simulation_IWFOS_Angle.xlsx")
results_Angle <- SS_finder(Output_Angle)
# filter results with noise_sd <= 5 or noise_sd <= 10
results_Angle <- results_Angle %>%
  filter(noise_sd == 5 | noise_sd == 10)
# Create heatmaps for each column


heatmap_TWT_Angle <- create_heatmap(results_Angle, "TWT_min_sample", "TWT Minimum Sample Size")
heatmap_IWT_Angle <- create_heatmap(results_Angle, "IWT_min_sample", "IWT Minimum Sample Size")
heatmap_Nonparametric_SPM_Angle <- create_heatmap(results_Angle, "Nonparametric_SPM_min_sample", "Nonparametric SPM Minimum Sample Size")
heatmap_ERL_Angle <- create_heatmap(results_Angle, "ERL_min_sample", "ERL Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_Angle)
print(heatmap_IWT_Angle)
print(heatmap_Nonparametric_SPM_Angle)
print(heatmap_ERL_Angle)

##################################
##############MF data#############
Output_MF <- read_excel("IWFOS_outputs/Simulation_IWFOS_MF.xlsx")
results_MF <- SS_finder(Output_MF)
# filter results with noise_sd <= 30 or noise_sd <= 40
results_MF <- results_MF %>%
  filter(noise_sd == 30 | noise_sd == 40)
# Create heatmaps for each column
heatmap_TWT_MF <- create_heatmap(results_MF, "TWT_min_sample", "TWT Minimum Sample Size")
heatmap_IWT_MF <- create_heatmap(results_MF, "IWT_min_sample", "IWT Minimum Sample Size")
heatmap_Nonparametric_SPM_MF <- create_heatmap(results_MF, "Nonparametric_SPM_min_sample", "Nonparametric SPM Minimum Sample Size")
heatmap_ERL_MF <- create_heatmap(results_MF, "ERL_min_sample", "ERL Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_MF)
print(heatmap_IWT_MF)
print(heatmap_Nonparametric_SPM_MF)
print(heatmap_ERL_MF)
##########################################


# Plot all the datasets using the function
# Assume plot_heatmap is a custom function for generating heatmaps.
# Replace the function calls with your actual plot_heatmap code.
plot1 <- create_heatmap(results_Angle, "TWT_min_sample", "")
plot2 <- create_heatmap(results_Angle, "IWT_min_sample", "")
plot3 <- create_heatmap(results_Angle, "Nonparametric_SPM_min_sample", "")
plot4 <- create_heatmap(results_Angle, "ERL_min_sample", "")



plot5 <- create_heatmap(results_MF, "TWT_min_sample", "")
plot6 <- create_heatmap(results_MF, "IWT_min_sample", "")
plot7 <- create_heatmap(results_MF, "Nonparametric_SPM_min_sample", "")
plot8 <- create_heatmap(results_MF, "ERL_min_sample", "")


# Create a title for each column
col1_title <- textGrob("Hip Flexion Angle", gp=gpar(fontsize=14, fontface="bold"))
col2_title <- textGrob("Muscle Force", gp=gpar(fontsize=14, fontface="bold"))

# Create a nullGrob to act as the spacer between columns
spacer <- nullGrob()


# Create row titles
row1_title <- textGrob("TWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row2_title <- textGrob("IWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row3_title <- textGrob("F-max", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row4_title <- textGrob("ERL", rot=90, gp=gpar(fontsize=12, fontface="bold"))


# Arrange the plots in two columns and four rows
# Arrange the plots with increased space between columns using a spacer
# Arrange the plots with row titles and increased space between columns
final_plot <- grid.arrange(
  spacer, col1_title, spacer, col2_title,          # Column titles and spacer
  row1_title, plot1, spacer, plot5,                # Row 1 with row title and spacer
  row2_title, plot2, spacer, plot6,                # Row 2 with row title and spacer
  row3_title, plot3, spacer, plot7,                # Row 3 with row title and spacer
  row4_title, plot4, spacer, plot8,                # Row 4 with row title and spacer
  ncol=4,                                          # Four columns (row titles, left plot, spacer, right plot)
  layout_matrix = rbind(
    c(1, 2, 3, 4),             # Titles row
    c(5, 6, 7, 8),             # First row of plots with row title
    c(9, 10, 11, 12),          # Second row of plots with row title
    c(13, 14, 15, 16),         # Third row of plots with row title
    c(17, 18, 19, 20)         # Fourth row of plots with row title

    
  ),        
  heights = c(0.15, 1, 1, 1, 1),                   # Control the height: less space for titles
  widths = c(0.1, 1, 0.15, 1)                      # Control the width: space for row titles and columns
)

# Display the final plot
grid.newpage()
grid.draw(final_plot)
#save plot
ggsave("IWFOS_outputs/plot_final.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 1200)


#####################################

source("Data_functions.R")
MF <- MF_data("both")
Angele <- Angle_data("both")

plot2<- Data_plot(MF, "Muscle Force")
plot1<- Data_plot(Angele, "Hip Flexion Angle")


#plot plot1 and 2 in a same row
data_plot <- grid.arrange(plot1, plot2, ncol=2)

#ggsave("IWFOS_outputs/plot_data.jpeg", data_plot, width = 190, height = 120, units = "mm", dpi = 1200)
