

## Aesthetics

# Function to create a dark theme for plots
dark_theme <- function() {
  
  theme(
    panel.grid.major = element_line(color = '#99a1b3'),  # Customize major grid lines
    panel.grid.minor = element_line(color = 'black'),  # Customize minor grid lines
    panel.background = element_rect(fill = 'black'),  # Set panel background color
    panel.border = element_blank(),  # Remove panel borders
    plot.background = element_rect(fill = "black", color = "black"),  # Set plot background color
    axis.text.x = element_text(color = 'white'),  # Customize x-axis tick labels
    axis.text.y = element_text(color = 'white'),  # Customize y-axis tick labels
    plot.title = element_text(face = 'bold', color = 'white', hjust = 0.5),  # Customize plot title
    plot.subtitle = element_text(face = 'bold', color = 'white'),  # Customize plot subtitle
    axis.title.x = element_text(color = 'white'),  # Customize x-axis title
    axis.title.y = element_text(color = 'white'),  # Customize y-axis title
    legend.background = element_rect(fill = "black", color = NA),  # Set legend background color
    legend.key = element_rect(color = "gray", fill = "black"),  # Customize legend key
    legend.title = element_text(color = "white"),  # Customize legend title
    legend.text = element_text(color = "white")  # Customize legend text
  )
  
}

# Define colors for consistency
single_color = "#FFD500"  # Define a single color
multiple_colors <- colorRampPalette(c('white', "#FFD500"))  # Define a color palette


select <- dplyr::select

reformat_ranking_matrix <- function(df_, skater_id_) {
  
  apply(df_, 1, function(x) which(x == skater_id_))
  
}

reformat_simulations <- function(df_, skater_id_) {
  
  apply(df_, 2, function(x) which(x == skater_id_))
  
}