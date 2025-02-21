
# Load necessary libraries
library(maxLik)
library(FDDPhase)
library(hypergeo)
library(raster)

# Window size for local estimation
window <- 3

# Load the noisy phase image
data <- raster("phi_raster_noisy.tif")

# Matrix to store estimated rho_c values
rho_c_matrix <- matrix(NA, nrow = nrow(data), ncol = ncol(data))

# Function to estimate rho_c using a 3x3 moving window
w3 <- function(image) {
  filtered_image <- image
  for (i in 2:(nrow(image) - 1)) {  # Loop through rows, excluding borders
    for (j in 2:(ncol(image) - 1)) {  # Loop through columns, excluding borders
      window <- image[(i - 1):(i + 1), (j - 1):(j + 1)]  # Extract 3x3 window
      a <- estim.GierullEq7(window, param)  # Estimate parameters using Gierull method
      rho_c_matrix[i, j] <- min(max(as.numeric(a$estimate[1]), 0), 0.9)  # Constrain values between 0 and 0.9
    }
  }
  return(rho_c_matrix)
}

# Initial parameter estimation
param <- c(r = 0.6, theta = 0, L = 2)

# Estimate rho_c locally using the w3 function
coherence_map <- w3(data)
coherence_map[,1] = coherence_map[,2]  # Left border
coherence_map[,ncol(coherence_map)] = coherence_map[,ncol(coherence_map)-1] # Right border
coherence_map[1,] = coherence_map[2,]  # Top border
coherence_map[nrow(coherence_map),]=coherence_map[nrow(coherence_map)-1,]
