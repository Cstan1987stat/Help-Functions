# Function to standard scale a numerical column
# - data: the data frame
# - column: the name of the numerical column to transform
standard_scale <- function(data, column){

  # Calculate the mean of the column
  average <- mean(data[[column]])
  
  # Calculate the standard deviation of the column
  standard_deviation <- sd(data[[column]])
  
  # Standardize the column:
  # Subtract the mean and divide by the standard deviation
  # - This transforms the data to have a mean of 0 and standard deviation of 1
  data[[column]] <- (data[[column]] - average) / standard_deviation

  # Return the transformed data frame
  return(data)
}
