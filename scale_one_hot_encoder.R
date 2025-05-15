# Function to one-hot encode a categorical variable and scale the encoded columns
# - data: the data frame
# - column: the name of the categorical column to transform
scale_one_hot <- function(data, column){
  
  # Get all unique categories in the column
  # - factor() ensures the variable is treated as categorical; confirm this beforehand if needed
  list_categ <- levels(factor(data[[column]]))
  
  # Get the number of categories
  num_categ <- length(list_categ)
  
  # Loop through each category
  for (cat in list_categ){
    
    # Create a new column name in the format: column_category
    new_col_name <- paste(column, cat, sep = '_')
    
    # Create a one-hot encoded column: 1 if equal to category, else 0
    data[[new_col_name]] <- as.numeric(data[[column]] == cat)
    
    # Scale the new column by dividing by the square root of the number of categories
    data[[new_col_name]] <- data[[new_col_name]] / sqrt(num_categ)
  }

  # Remove the original categorical column
  data[[column]] <- NULL

  # Return the transformed data frame
  return(data)
}
