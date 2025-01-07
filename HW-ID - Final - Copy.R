#install.packages("DT")
library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(readxl)
library(ggplot2)
library(DT)
library(writexl)

#FIRST PART: Calculate the Leontief Inverse
################################################################################
# Load the data
data <- read_dta("D://Global/WIOT2014_October16_ROW.dta")

# Check rows where TOT == 0
data %>% filter(TOT == 0)

# 1. Remove rows where TOT == 0 and Country != "TOT"
data <- data %>%
  filter(!(TOT == 0 & Country != "TOT"))

# 2. Define list of country codes (assuming you have this list, it could be a vector)
country_list <- unique(data$Country)  # or manually specify the list

# 3. Iterate over each country in the country_list
for (c in country_list) {
  # Iterate over the 56 variables for each country (assuming the variables are named vCountry1, vCountry2, ..., vCountry56)
  for (i in 1:56) {
    var_name <- paste0("v", c, i)  # Construct the variable name, e.g., vCountry1, vCountry2
    
    # Check if the variable exists in the data frame
    if (var_name %in% names(data)) {
      # Calculate the mean of the variable
      var_mean <- mean(data[[var_name]], na.rm = TRUE)  # Ignore NA values
      
      # If the mean is 0, remove the column
      if (var_mean == 0) {
        data[[var_name]] <- NULL  # Remove the column
      }
    }
  }
}
# Loop over each country
for (c in country_list) {
  # Loop over the variable indices (57 to 61)
  for (i in 57:61) {
    # Construct the old and new variable names
    old_var <- paste0("v", c, i)   # e.g., vCountry57, vCountry58, ...
    new_var <- paste0("f", c, i)   # e.g., fCountry57, fCountry58, ...
    
    # Rename the column if it exists in the dataset
    if (old_var %in% names(data)) {
      names(data)[names(data) == old_var] <- new_var
    }
  }
}
# Extract intermediate input matrix Z for Indonesia
Z <- data %>%
  filter(Country == "IDN") %>%
  select(starts_with("vIDN")) %>%
  as.matrix()

# Extract total output vector X for Indonesia
x <- data %>%
  filter(IndustryCode == "GO") %>%
  select(starts_with("vIDN")) %>%
  as.matrix()

# Extract value-added vector VA for Indonesia
v <- data %>%
  filter(IndustryCode == "VA") %>%
  select(starts_with("vIDN")) %>%
  as.matrix()

# Compute technical coefficients matrix A
A <- Z %*% solve(diag(as.numeric(x)))

# Compute Leontief inverse matrix L
I_matrix <- diag(nrow(A))
L <- solve(I_matrix - A)

# Extract total final demand vector for Indonesia
totf <- data %>%
  filter(Country == "IDN") %>%
  mutate(
    findem = rowSums(select(., starts_with("f"))),
    interm = rowSums(select(., starts_with("v"))),
    intIDN = rowSums(select(., starts_with("vIDN"))),
    totf = findem + interm - intIDN
  ) %>%
  pull(totf)
totf <- matrix(totf, ncol = 1)

# Check the Leontief Inverse correct or not, if correct x=o
o <- L %*% totf
o<-t(o)
check <- cbind(x, o)
#print(check)

# Calculate normalize backward linkages (row sums of the Leontief inverse matrix L)
backward_linkages <- rowSums(L) / mean(rowSums(L))

# Calculate normalize forward linkages (column sums of the Leontief inverse matrix L)
forward_linkages <- colSums(L) / mean(colSums(L))

# Combine into a data frame for all sectors
linkages_table <- data.frame(
  Sector = 1:nrow(L),  # Assuming sector indices are rows of L
  BackwardLinkage = backward_linkages,
  ForwardLinkage = forward_linkages
)

# Print the table
print(linkages_table)

# Write the data frame to an Excel file
#writexl::write_xlsx(linkages_table, "Linkages_Table.xlsx")

#SECOND PART: Combined data with employment and emission
################################################################################
employment_data <- read_excel("D://Global/Socio_Economic_Accounts.xlsx", sheet = "DATA")
emissions_data <- read_excel("D://Global/IDN_AIR.xls", sheet = "2009")

employment_data <- employment_data %>%
  rename(Country = country, IndustryCode = code, employ2000 = "2000", employ2001 = "2001", employ2002 = "2002"
         , employ2003 = "2003", employ2004 = "2004", employ2005 = "2005", employ2006 = "2006", employ2007 = "2007"
         , employ2008 = "2008", employ2009 = "2009", employ2010 = "2010", employ2011 = "2011", employ2012 = "2012"
         , employ2013 = "2013",employ2014 = "2014")

aggregated_data <- employment_data %>%
  filter(grepl("EMPE", variable)) %>%  # Keep rows where 'variable' contains 'EMPE'
  group_by(Country, IndustryCode) %>%
  summarise( # Keep all columns without summarizing them
    across(starts_with("employ"), ~ first(.x), .names = "employ_{.col}"), # Keep the first value for each "employ" column
    variable = first(variable),                            # Keep the first value of 'variable'
    description = first(description),                      # Keep the first value of 'description'
    .groups = "drop"
  ) %>%
  filter(IndustryCode != "U")  # Remove rows with IndustryCode == "U"

# First, join emissions_data with the entire dataset
data <- data %>%
  left_join(emissions_data, by = "IndustryCode")

# Now, set emissions values to 0 for all countries except IDN
data <- data %>%
  mutate(across(.cols = c("CO2", "CH4", "N2O", "NOX", "SOX", 
                          "CO", "NMVOC", "NH3"),
                .fns = ~ ifelse(Country == "IDN", ., 0)))

data <- data %>%
  left_join(aggregated_data, by = c("Country", "IndustryCode")) 

#THIRD PART: Plot Graph for existing condition
################################################################################

# Matrices for employment and pollution
employ_employ2014 <- data %>%
  filter(Country == "IDN" & !is.na(TOT)) %>%
  pull(employ_employ2014) %>%
as.matrix()

pol <- data %>%
  filter(Country == "IDN" & !is.na(TOT)) %>%
  select(CO2,CH4,N2O,NOX,SOX, 
         CO, NMVOC,NH3) %>%
  as.matrix()

# Filter rows where Country is "IDN" and select the 'employ_employ_2014' column
view_data <- data %>%
  filter(Country == "IDN") 

# Compute intensity for pollution, employment, and value-added matrices
intpol <- t(pol)
intemp <- t(employ_employ2014) %*% solve(diag(as.numeric(x)))
intva <- v %*% solve(diag(as.numeric(x)))

# Top 5 Value Added (GDP) Sector
#######################################
va_vector <- as.vector(v)
tva_df <- data.frame(Sector = 1:47, ValueAdded = va_vector)

# Calculate total Value Added for all sectors
total_value_added <- sum(tva_df$ValueAdded)

# Sort the data by ValueAdded in descending order
sorted_tva_df <- tva_df %>%
  arrange(desc(ValueAdded))

# Select the top 5 sectors by ValueAdded
top_5_va <- sorted_tva_df %>%
  head(5)

top_5_va <- top_5_va %>%
  mutate(Sector = as.character(Sector))

# Calculate the sum of VAImpact for the rest of the sectors
rest_va<- sorted_tva_df %>%
  slice(6:n()) %>%  # Skip the top 5
  summarise(ValueAdded = sum(ValueAdded)) %>%
  mutate(Sector = "Rest")  # Label the rest as "Rest"

# Combine the top 5 sectors and the "Rest" category
va_pie_data <- bind_rows(top_5_va, rest_va)

# Calculate percentage contribution for each sector
va_pie_data <- va_pie_data %>%
  mutate(Percentage = (ValueAdded / total_value_added) * 100)

# Plot the pie chart
va_pie_chart <- ggplot(va_pie_data, aes(x = "", y = Percentage, fill = factor(Sector))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentage labels
  labs(title = "Top 5 Sectors and Rest by Value-Added Intensity",
       fill = "Sector") +
  theme_void() +  # Simplify the chart
  theme(legend.position = "right")

# Show the plot
print(va_pie_chart)

# Print the percentage table
print(va_pie_data)

# Write the data frame to an Excel file
#writexl::write_xlsx(va_pie_data, "va_pie_data.xlsx")

#FOURTH PART: Study Case: Indonesia Nickel Export Ban
################################################################################
# Loss opportunity due to export ban &Incoming FDI due to export ban
################################################################################
# Step 1: Initialize the 'df' column
data <- data %>%
  mutate(df = ifelse(Country == "IDN" & IndustryCode == "B", -1100, 
                   ifelse(Country == "IDN" & IndustryCode == "C25", 2500, 0)))

# Step 2: Extract 'df' values for Country == "IDN"
df <- data %>%
  filter(Country == "IDN") %>%
  pull(df) %>%
  as.matrix()

# Step 3: Calculate `e * L`
pol_L <- array(0, dim = c(nrow(intpol), nrow(L), ncol(L))) 
for (p in 1:nrow(intpol)) {  # Loop through pollutants
  for (i in 1:nrow(L)) {  # Loop through rows of L
    for (j in 1:ncol(L)) {  # Loop through columns of L
      pol_L[p, i, j] <- intpol[p, j] * L[i, j]  # Element-wise multiplication
    }
  }
}

emp_L <- matrix(0, nrow = nrow(L), ncol = ncol(L))  # Create an empty matrix of the same dimensions as `L`
for (i in 1:nrow(L)) {
  for (j in 1:ncol(L)) {
    emp_L[i, j] <- intemp[1, j] * L[i, j]  # Multiply element-wise
  }
}

va_L <- matrix(0, nrow = nrow(L), ncol = ncol(L))  # Create an empty matrix of the same dimensions as `L`
for (i in 1:nrow(L)) {
  for (j in 1:ncol(L)) {
    va_L[i, j] <- intva[1, j] * L[i, j]  # Multiply element-wise
  }
}

# Step 4: Calculate `delta` using the result of `e * L` and `df`
dpol <- matrix(0, nrow = nrow(intpol), ncol = nrow(df))  # Result matrix (pollutants × industries)
for (p in 1:nrow(intpol)) {
  dpol[p, ] <- pol_L[p, , ] %*% df  # Matrix multiplication for each pollutant
  rownames(dpol) <- rownames(intpol)
}#in tonnes except CO2 in kilotonnes
demp <- emp_L %*% df #thousands of people
dva <- va_L %*% df #million US$

# Pollution Plot
####################################
# Calculate total pollution impact for each sector (excluding CO)
# 1. Prepare the data for Pollution Impact
pol_data <- data.frame(
  Pollutant = rep(rownames(dpol), ncol(dpol)),  # Repeat pollutant names for each sector
  Sector = rep(1:ncol(dpol), each = nrow(dpol)),  # Each sector repeated for each pollutant
  polimpact = as.vector(dpol)  # Flatten the matrix to vector for each pollutant's impact
)

# View the data (this should show the pollutants for each sector, without summing them up)
view(pol_data)
#writexl::write_xlsx(pol_data, "pol_data.xlsx")

# Employment Plot
####################################
# 1. Prepare the data for Employment Impact
demp_df <- as.data.frame(demp)
demp_df$Industry <- paste0("Sector_", seq_len(nrow(demp_df)))  # Adding industry names if not present
colnames(demp_df)[1] <- "Employment"  # Rename the column

# 2. Sort and select the top 5 industries
top_5 <- demp_df %>%
  arrange(desc(`Employment`)) %>%
  slice_head(n = 5)

# 3. Create the bar chart
emp_chart <- ggplot(top_5, aes(x = reorder(Industry, -`Employment`), y = `Employment`, fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Industries by Employment Impact",
       x = "Industry",
       y = "Employment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Print the chart
print(emp_chart)

#Produce the table
writexl::write_xlsx(demp, "employment_data.xlsx")

# GDP
####################################
# 1. Prepare the data for GDP
dva_df <- as.data.frame(dva)
dva_df$Industry <- paste0("Sector_", seq_len(nrow(dva_df)))  # Adding industry names if not present
colnames(dva_df)[1] <- "GDP"  # Rename the column

# 2. Sort and select the top 5 industries
top_5 <- dva_df %>%
  arrange(desc(`GDP`)) %>%
  slice_head(n = 5)

# 3. Create the bar chart
va_chart <- ggplot(top_5, aes(x = reorder(Industry, -`GDP`), y = `GDP`, fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Industries by GDP Impact",
       x = "Industry",
       y = "GDP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Print the chart
print(va_chart)

#Produce the table
#writexl::write_xlsx(dva, "GDP_data.xlsx")







rm(list = ls())
#  Re-run for Worldwide Effect
################################################################################
#install.packages("haven")

library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(readxl)
library(ggplot2)

# Load the data
data <- read_dta("D://Global/WIOT2014_October16_ROW.dta")
#View(data)

#Check TOT and Country data type
str(data$TOT)
str(data$Country)

# Check rows where TOT == 0
data %>% filter(TOT == 0)

# 1. Remove rows where TOT == 0 and Country != "TOT"
data <- data %>%
  filter(!(TOT == 0 & Country != "TOT"))

# 2. Define list of country codes (assuming you have this list, it could be a vector)
country_list <- unique(data$Country)  # or manually specify the list

# 3. Iterate over each country in the country_list
for (c in country_list) {
  # Iterate over the 56 variables for each country (assuming the variables are named vCountry1, vCountry2, ..., vCountry56)
  for (i in 1:56) {
    var_name <- paste0("v", c, i)  # Construct the variable name, e.g., vCountry1, vCountry2
    
    # Check if the variable exists in the data frame
    if (var_name %in% names(data)) {
      # Calculate the mean of the variable
      var_mean <- mean(data[[var_name]], na.rm = TRUE)  # Ignore NA values
      
      # If the mean is 0, remove the column
      if (var_mean == 0) {
        data[[var_name]] <- NULL  # Remove the column
      }
    }
  }
}
# Loop over each country
for (c in country_list) {
  # Loop over the variable indices (57 to 61)
  for (i in 57:61) {
    # Construct the old and new variable names
    old_var <- paste0("v", c, i)   # e.g., vCountry57, vCountry58, ...
    new_var <- paste0("f", c, i)   # e.g., fCountry57, fCountry58, ...
    
    # Rename the column if it exists in the dataset
    if (old_var %in% names(data)) {
      names(data)[names(data) == old_var] <- new_var
    }
  }
}

# Step 1: Create matrices for different variables
Z <- data %>%
  filter(Country != "TOT") %>%
  select(starts_with("v")) %>%
  as.matrix()

x <- data %>%
  filter(IndustryCode == "GO") %>%
  select(starts_with("v")) %>%
  as.matrix()

v <- data %>%
  filter(IndustryCode == "VA") %>%
  select(starts_with("v")) %>%
  as.matrix()

# Convert the matrix `v` into a data frame
v_df <- as.data.frame(v)

# Ensure column names are preserved (if not, rename them)
colnames(v_df) <- colnames(v)

# Reshape from wide to long format
v_long <- v_df %>%
  pivot_longer(
    cols = everything(),       # Use all columns
    names_to = "Country",      # Create a column for country names
    values_to = "VA"           # Create a column for values
  ) %>%
  mutate(Country = gsub("\\d+", "", Country)) %>%  # Remove numeric parts
  group_by(Country) %>%        # Group by country name
  summarise(Total_VA = sum(VA, na.rm = TRUE))  # Summarize by country

# View the result
#view(v_long)

totf <- data %>%
  mutate(totf = rowSums(select(., starts_with("f")), na.rm = TRUE)) %>%
  filter(Country != "TOT") %>%
  pull(totf) %>%
  as.matrix()

# Step 2: Compute matrices `A`, `L`, and `o`
A <- Z %*% solve(diag(as.numeric(x)))
L <- solve(diag(nrow(A)) - A)
o <- L %*% totf
check <- cbind(o, t(x))  # Combined matrix for comparison

#view(check)

# Step 3: Effect of Export Ban
data <- data %>%
  mutate(df = ifelse(Country == "IDN" & IndustryCode == "B", -1100, 
                     ifelse(Country == "IDN" & IndustryCode == "C25", 2500, 0)))

df <- data %>%
  filter(Country != "TOT") %>%
  pull(df) %>%
  as.matrix()
dim(df)

intva <- v %*% solve(diag(as.numeric(x)))
colnames(intva) <- colnames(v)

va_L <- matrix(0, nrow = nrow(L), ncol = ncol(L))  # Create an empty matrix of the same dimensions as `L`
for (i in 1:nrow(L)) {
  for (j in 1:ncol(L)) {
    va_L[i, j] <- intva[1, j] * L[i, j]  # Multiply element-wise
  }
}
colnames(va_L) <- colnames(intva)
view(va_L)

dva <- va_L %*% df #million US$
rownames(dva) <- colnames(va_L)

# Step 1: Convert dva to a data frame (if not already)
dva_df <- as.data.frame(dva)

# Step 2: Add row names as a new column
dva_df$RowNames <- rownames(dva_df)

# Step 3: Extract the country prefix (e.g., vAUS, vAUT) from the row names
dva_df$Prefix <- gsub("(v[A-Z]+).*", "\\1", dva_df$RowNames)  # Extract country prefix

# Step 4: Sort the data frame by the 'Prefix' column and then by row name
dva_sorted <- dva_df[order(dva_df$Prefix, dva_df$RowNames), ]

# Step 5: Restore sorted row names and remove helper columns
rownames(dva_sorted) <- dva_sorted$RowNames  # Reset row names
dva_sorted$RowNames <- NULL
dva_sorted$Prefix <- NULL

# Step 1: Convert dva to a data frame (if not already)
dva_df <- as.data.frame(dva)

# Step 2: Add row names as a new column
dva_df$RowNames <- rownames(dva_df)

# Step 3: Extract the country prefix (e.g., vAUS, vAUT) from the row names
dva_df$Prefix <- gsub("(v[A-Z]+).*", "\\1", dva_df$RowNames)  # Extract country prefix

# Step 4: Group by Prefix and sum the values
dva_summed <- dva_df %>%
  group_by(Prefix) %>%
  summarize(Sum = sum(V1))  # Replace 'V1' with the name of your data column, if different

dva_summed$Prefix <- as.character(dva_summed$Prefix)
v_long$Country <- as.character(v_long$Country)
dva_summed <- dva_summed %>%
  left_join(v_long, by = c("Prefix" = "Country"))


#writexl::write_xlsx(dva_summed, "dva_summed.xlsx")
dva <- as.data.frame(dva)
dva_with_rownames <- rownames_to_column(dva, var = "Row_Name")
#writexl::write_xlsx(dva_with_rownames, "dva_with_rownames.xlsx")

#Plot Top 5 Countries Affected
top_5_dva <- dva_summed %>%
  arrange(desc(Sum)) %>%  # Sorting data by 'Sum' in descending order
  head(5)  # Selecting the top 5 rows

# Create the plot
ggplot(top_5_dva, aes(x = reorder(Prefix, Sum), y = Sum)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(title = "Top 5 Sectors by Sum of DVA", 
       x = "Country", 
       y = "Sum of DVA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label = round(Sum, 2)), hjust = -0.2)  
