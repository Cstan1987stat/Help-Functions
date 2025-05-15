# Creating function to one-hot encode a categorical variable and then scale it
# - Data represents the data frame
# - Column represents the categorical column you want to be transformed
scale_one_hot <- function(data, column){
  # Retrieve all of the categories for the column from data
  # - factor makes sure that the variable is categorical, but highly recommend making sure it is before hand
  list_categ <- levels(factor(data[[column]]))
  # Calculating how many categories are in column
  num_categ <- length(list_categ)
  # Looping through each category 
  for (cat in list_categ){
    # Creating a new column name with the old column name, an underscore, followed by the specific category (cat).
    new_col_name <- paste(column,cat, sep='_')
    # Creating new column in data where a 1 represents original column is equal to the specific category.
    data[[new_col_name]] <- as.numeric(data[[column]] == cat)
    # Divided the new column by the square root of the number of categories
    data[[new_col_name]] <- data[[new_col_name]] / sqrt(num_categ)
  }
  # Setting the initial column in data to NULL
  data[[column]] <- NULL
  # Return data
  return(data) 
}
