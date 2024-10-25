# Function to calculate the average (mean)
custom_mean <- function(numbers) {
  sum_of_numbers <- 0
  count <- length(numbers)
  
  # Summing up all elements
  for (value in numbers) {
    sum_of_numbers <- sum_of_numbers + value
  }
  
  # Calculating the mean
  mean_result <- sum_of_numbers / count
  return(mean_result)
}

# Function to calculate the middle value (median)
custom_median <- function(numbers) {
  count <- length(numbers)
  # Sorting the numbers in ascending order
  sorted_numbers <- sort(numbers)
  
  # If odd number of elements, return the middle one
  if (count %% 2 != 0) {
    median_result <- sorted_numbers[(count + 1) / 2]
  } else {
    # If even number of elements, return the average of the two middle ones
    median_result <- (sorted_numbers[count / 2] + sorted_numbers[(count / 2) + 1]) / 2
  }
  
  return(median_result)
}

# Function to find the mode (most frequent value)
custom_mode <- function(numbers) {
  frequency_table <- table(numbers)
  # Find the most frequent value
  mode_result <- as.numeric(names(frequency_table)[which.max(frequency_table)])
  
  return(mode_result)
}

# Testing the functions with a sample data array
data_values <- c(4, 2, 7, 7, 3, 9, 5, 9, 9, 4)

# Get the mean, median, and mode
mean_value <- custom_mean(data_values)
median_value <- custom_median(data_values)
mode_value <- custom_mode(data_values)

# Output the results
cat("Calculated Mean:", mean_value, "\n")
cat("Calculated Median:", median_value, "\n")
cat("Calculated Mode:", mode_value, "\n")
