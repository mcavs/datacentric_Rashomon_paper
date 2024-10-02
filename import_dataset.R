setwd("datacentric_Rashomon/datasets")
csv_files <- list.files(pattern = "\\.csv$")

datasets <- NULL
for (file in csv_files) {
  data <- read.csv(file)
  # Transform the response variable to a factor variable
  data$Class <- as.factor(data$Class)
  
  char_columns <- sapply(data, is.character)
  if (any(char_columns)) {
    char_column_names <- names(data)[char_columns]
    data[char_column_names] <- lapply(data[char_column_names], function(x) as.numeric(as.factor(x)))
  }
  
  datasets[[file]] <- data
  assign(gsub("\\.csv$", "", file), data)
}

