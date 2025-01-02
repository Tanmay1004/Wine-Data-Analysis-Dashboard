# Install and load the necessary package
install.packages(c("shiny", "shinythemes", "dplyr", "ggplot2", "plotly", "randomForest", "caret", "corrplot", "reshape2", "DBI", "RSQLite", "readxl","sodium"))



#install.packages("readxl")  # Skip if already installed
library(readxl)

# Read the Excel file into a data frame
wine_data <- read_excel("filepath/wine_data.xlsx")
