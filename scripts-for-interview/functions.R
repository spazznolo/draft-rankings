

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


## Functional

# Create an alias for the 'select' function from the dplyr package
select <- dplyr::select

# Function to reformat the ranking matrix
reformat_ranking_matrix <- function(df_, skater_id_) {
  # Apply a function row-wise to find the position of skater rankings' in each ranking list
  apply(df_, 1, function(x) which(x == skater_id_))
}

# Function to reformat the simulations
reformat_simulations <- function(df_, skater_id_) {
  # Apply a function column-wise to find the position of skaters in each simulation
  apply(df_, 2, function(x) which(x == skater_id_))
}

# Function to calculate remaining probabilities
get_remaining_probs <- function(x_vec) {
  
  n = rep(0, length(x_vec))  # Initialize a vector 'n' with zeros of the same length as 'x_vec'
  
  for (i in 1:length(x_vec)) {  # Loop through each element of 'x_vec'
    
    n[i] = x_vec[i]  # Set the current element of 'n' to the corresponding element of 'x_vec'
    
    if (sum(n) >= 1) {  # Check if the sum of 'n' is greater than or equal to 1
      n[i] = 1 - sum(n[1:(i-1)])  # If the condition is met, adjust the current element of 'n' to make the sum exactly 1
      return(n)  # Return the modified 'n' vector
    }
  }
  
}
