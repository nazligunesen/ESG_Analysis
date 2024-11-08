install.packages("readxl")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggcorrplot")
install.packages("openxlsx")

library(readxl)
library(ggcorrplot)
library(openxlsx)
library(lubridate)
library(dplyr)
library(tidyr)


# Recalling the raw data
Deals <- read_excel("Deals_org.xlsx")
ESG_Scores_Target <- read_excel("ESG Scores.xlsx")
ESG_Scores_Acquiror <- read_excel("ESG Scores.xlsx")

#Tagging
# Rename columns in 'ESG_Scores_Target' dataset by adding "target" as a prefix
names(ESG_Scores_Target) <- paste0("Target ", names(ESG_Scores_Target))

# Rename columns in 'ESG_Scores_Target' dataset by adding "acquiror" as a prefix
names(ESG_Scores_Acquiror) <- paste0("Acquiror ", names(ESG_Scores_Acquiror))

# Merging ESG scores into 'Deals' based on 'Target Full Name'(Left Join)
Deals2 <- merge(Deals, ESG_Scores_Target, by.x = "Target Full Name", by.y = "Target Company Name", all.x = TRUE)

# Merging ESG scores into 'Deals' based on 'Acquiror Full Name'(Left Join)
Deals3 <- merge(Deals2, ESG_Scores_Acquiror, by.x = "Acquiror Full Name", by.y = "Acquiror Company Name", all.x = TRUE)

# View the Merged Excel file
write.xlsx(Deals3, "Deals3.xlsx")

Deals3 <- Deals3 %>%
  mutate(`2 Years Prior Announcement` = `Year Announced` - 2,
         `1 Year Prior Announcement` = `Year Announced` - 1,
         `1 Year Later Announcement` = `Year Announced` + 1,
         `2 Years Later Announcement` = `Year Announced` + 2)


# Filling in Columns that includes ESG Scores Prior and After the Announcement Date

#For Target Companies#

Deals4 <- Deals3 %>%
  rowwise() %>%
  mutate(
    `Target Environmental Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Target Environmental Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Target Environmental Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Target Environmental Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Target Environmental Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Target Environmental Pillar Score ", `Year Announced`)) else NA_real_,
    `Target Environmental Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Target Environmental Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Target Environmental Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Target Environmental Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Target Social Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Target Social Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Target Social Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Target Social Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Target Social Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Target Social Pillar Score ", `Year Announced`)) else NA_real_,
    `Target Social Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Target Social Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Target Social Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Target Social Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Target Governance Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Target Governance Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Target Governance Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Target Governance Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Target Governance Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Target Governance Pillar Score ", `Year Announced`)) else NA_real_,
    `Target Governance Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Target Governance Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Target Governance Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Target Governance Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Target Combined ESG Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Target Combined ESG Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Target Combined ESG Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Target ESG Combined Score ", `Year Announced`)) else NA_real_,
    `Target Combined ESG Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `1 Year Later Announcement`)) else NA_real_,
    `Target Combined ESG Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Target Combined ESG Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Target Combined ESG Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Target Combined ESG Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Target ESG Combined Score ", `Year Announced`)) else NA_real_,
    `Target Combined ESG Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `1 Year Later Announcement`)) else NA_real_,
    `Target Combined ESG Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Target ESG Combined Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

#For Acquiror Companies#

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Acquiror Environmental Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Acquiror Environmental Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Acquiror Environmental Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Acquiror Environmental Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Acquiror Environmental Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Acquiror Environmental Pillar Score ", `Year Announced`)) else NA_real_,
    `Acquiror Environmental Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Acquiror Environmental Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Acquiror Environmental Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Acquiror Environmental Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Acquiror Social Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Acquiror Social Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Acquiror Social Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Acquiror Social Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Acquiror Social Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Acquiror Social Pillar Score ", `Year Announced`)) else NA_real_,
    `Acquiror Social Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Acquiror Social Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Acquiror Social Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Acquiror Social Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Acquiror Governance Pillar Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Acquiror Governance Pillar Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Acquiror Governance Pillar Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Acquiror Governance Pillar Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Acquiror Governance Pillar Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Acquiror Governance Pillar Score ", `Year Announced`)) else NA_real_,
    `Acquiror Governance Pillar Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Acquiror Governance Pillar Score ", `1 Year Later Announcement`)) else NA_real_,
    `Acquiror Governance Pillar Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Acquiror Governance Pillar Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Acquiror Combined ESG Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Acquiror ESG Combined Score ", `Year Announced`)) else NA_real_,
    `Acquiror Combined ESG Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `1 Year Later Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()

Deals4 <- Deals4 %>%
  rowwise() %>%
  mutate(
    `Acquiror Combined ESG Score 2 Year Prior Announcement` = if (`2 Years Prior Announcement` >= 2008 & `2 Years Prior Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `2 Years Prior Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score 1 Year Prior Announcement` = if (`1 Year Prior Announcement` >= 2008 & `1 Year Prior Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `1 Year Prior Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score Year Announcement` = if (`Year Announced` >= 2008 & `Year Announced` <= 2023) get(paste0("Acquiror ESG Combined Score ", `Year Announced`)) else NA_real_,
    `Acquiror Combined ESG Score 1 Year After Announcement` = if (`1 Year Later Announcement` >= 2008 & `1 Year Later Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `1 Year Later Announcement`)) else NA_real_,
    `Acquiror Combined ESG Score 2 Year After Announcement` = if (`2 Years Later Announcement` >= 2008 & `2 Years Later Announcement` <= 2023) get(paste0("Acquiror ESG Combined Score ", `2 Years Later Announcement`)) else NA_real_
  ) %>%
  ungroup()


write.xlsx(Deals4, "Deals_final.xlsx") # Initial data analysis tables in the thesis are based on this data

# Filter rows with non-missing values in the columns of interest
filtered_data <- Deals4[complete.cases(Deals4$`Acquiror Combined ESG Score 1 Year After Announcement`, Deals4$`Acquiror Combined ESG Score 2 Year After Announcement`), ]

write.xlsx(filtered_data, "filtered_data.xlsx")

#Remowing accessive columns 

# Create filtered_data_2 by excluding columns 28 to 169 from filtered_data
filtered_data_2 <- filtered_data[, -c(28:170)]

# Identify numeric columns in filtered_data_2
numeric_columns <- sapply(filtered_data_2, is.numeric)

# Identify numeric columns in filtered_data_2
numeric_columns <- sapply(filtered_data_2, is.numeric)

write.xlsx(filtered_data, "filtered_data_with_NAs.xlsx")

# Replace NAs in numeric columns with the column mean
for (col in names(filtered_data_2)[numeric_columns]) {
  filtered_data_2[[col]][is.na(filtered_data_2[[col]])] <- mean(filtered_data_2[[col]], na.rm = TRUE)
}

# Check if NAs have been replaced with means
summary(filtered_data_2[numeric_columns])



#####EXPLATORY ANALYSIS####

summary(filtered_data_2)
write.xlsx(filtered_data_2, "filtered_data2.xlsx")
# Select only numeric columns from filtered_data_2
filtered_data_2_numeric <- filtered_data_2[, sapply(filtered_data_2, is.numeric)]
write.xlsx(filtered_data_2_numeric, "filtered_data2_numeric.xlsx")

#ESG Variables#
# Select columns with ESG Scores
esg_columns <- filtered_data_2_numeric[, 15:54]

# View the structure or summary of the selected columns
str(esg_columns)
summary(esg_columns)

# Convert data to long format for ggplot
gathered_data <- esg_columns %>%
  gather()

# Plot density plots for numeric variables using ggplot2
ggplot(gathered_data, aes(value, fill = key)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ key, scales = "free") +
  labs(title = "Density Plots of ESG Scores", x = "Values", y = "Density") +
  theme_minimal()

# Select columns with ESG Scores Seperately
esg_columns_target <- esg_columns[, 1:20]
esg_columns_acquiror <- esg_columns[, 21:40]

# Convert data to long format for ggplot
gathered_data_target <- esg_columns_target %>%
  gather()

gathered_data_acquiror <- esg_columns_acquiror %>%
  gather()

# Plot density plots for numeric variables using ggplot2
ggplot(gathered_data_acquiror, aes(value, fill = key)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ key, scales = "free") +
  labs(title = "Density Plots of Acquiror ESG Scores", x = "Values", y = "Density") +
  theme_minimal()



####Correlation Analysis####

target <- ncol(filtered_data_2_numeric)

# Calculate correlations of each column with the last column
correlation_values <- sapply(1:(target - 1), function(i) {
  cor(filtered_data_2_numeric[, i], filtered_data_2_numeric[, target], use = "complete.obs")  # Use complete observations for correlation
})

library(corrplot)

#Shortening the column names

shorter_column_names <- c("Year", "A_Emp", "T_Emp", "Days","T_ROE","T_ROA","T_EBIT_ROA","A_Tot_A","T_Tot_A","A_EBITDA","T_EBITDA","T_EBITDA_MARGIN","A_MV","T_MV",
                          "T_Env_P2","T_Env_P1","T_Env_DA","T_Env_A1","T_Env_A2",
                          "T_Soc_P2","T_Soc_P1","T_Soc_DA","T_Soc_A1","T_Soc_A2",
                          "T_Gov_P2","T_Gov_P1","T_Gov_DA","T_Gov_A1","T_Gov_A2",
                          "T_ESG_P2","T_ESG_P1","T_ESG_DA","T_ESG_A1","T_ESG_A2",
                          "A_Env_P2","A_Env_P1","A_Env_DA","A_Env_A1","A_Env_A2", 
                          "A_Soc_P2","A_Soc_P1","A_Soc_DA","A_Soc_A1","A_Soc_A2",
                          "A_Gov_P2","A_Gov_P1","A_Gov_DA","A_Gov_A1","A_Gov_A2",
                          "A_ESG_P2","A_ESG_P1","A_ESG_DA","A_ESG_A1","A_ESG_A2")  # Define shorter names for each column

# Assign shorter names to the columns
names(filtered_data_2_numeric) <- shorter_column_names


# Calculate the correlation matrix
correlation_matrix <- cor(filtered_data_2_numeric)

# Create a ggplot-based correlogram
ggcorrplot(correlation_matrix, type = "upper", lab = TRUE)

# Create a ggplot-based correlogram without displaying correlation values
ggcorrplot(correlation_matrix, type = "upper", lab = FALSE)

#####Identifying Highly Correlated Variables######


# Define the correlation threshold
threshold <- 0.3

target_variable_name <- "A_ESG_A1"

# Extract correlations of variables with the target variable
correlation_with_target <- correlation_matrix[target_variable_name, ]

# Filter variables with correlation greater than the threshold
highly_correlated_variables <- names(correlation_with_target[abs(correlation_with_target) > threshold])
print(highly_correlated_variables)



#Transforming Highly Correlated Variables

# Calculate new variables

filtered_data_2_old <- filtered_data_2

filtered_data_2$Acquiror_Increase_Env <- (
  ((filtered_data_2$`Acquiror Environmental Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Acquiror Environmental Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Acquiror Environmental Pillar Score 1 Year Prior Announcement`) +
    ((filtered_data_2$`Acquiror Environmental Pillar Score Year Announcement` - filtered_data_2$`Acquiror Environmental Pillar Score 1 Year Prior Announcement`) / filtered_data_2$`Acquiror Environmental Pillar Score 1 Year Prior Announcement`)
) / 2

filtered_data_2$Acquiror_Increase_Soc <- (
  ((filtered_data_2$`Acquiror Social Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Acquiror Social Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Acquiror Social Pillar Score 1 Year Prior Announcement`) +
    ((filtered_data_2$`Acquiror Social Pillar Score Year Announcement` - filtered_data_2$`Acquiror Social Pillar Score 1 Year Prior Announcement`) / filtered_data_2$`Acquiror Social Pillar Score 1 Year Prior Announcement`)
) / 2

filtered_data_2$Acquiror_Increase_Gov <- (
  ((filtered_data_2$`Acquiror Governance Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Acquiror Governance Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Acquiror Governance Pillar Score 1 Year Prior Announcement`) +
    ((filtered_data_2$`Acquiror Governance Pillar Score Year Announcement` - filtered_data_2$`Acquiror Governance Pillar Score 1 Year Prior Announcement`) / filtered_data_2$`Acquiror Governance Pillar Score 1 Year Prior Announcement`)
) / 2

filtered_data_2$Acquiror_Increase_Gov <- (
  ((filtered_data_2$`Acquiror Combined ESG Score 1 Year Prior Announcement` - filtered_data_2$`Acquiror Combined ESG Score 2 Year Prior Announcement`) / filtered_data_2$`Acquiror Combined ESG Score 1 Year Prior Announcement`) +
    ((filtered_data_2$`Acquiror Combined ESG Score Year Announcement` - filtered_data_2$`Acquiror Combined ESG Score 1 Year Prior Announcement`) / filtered_data_2$`Acquiror Combined ESG Score 1 Year Prior Announcement`)
) / 2

filtered_data_2$Target_Increase_Env <- ((filtered_data_2$`Target Environmental Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Target Environmental Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Target Environmental Pillar Score 1 Year Prior Announcement`) 

filtered_data_2$Target_Increase_Soc <- ((filtered_data_2$`Target Social Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Target Social Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Target Social Pillar Score 1 Year Prior Announcement`) 

filtered_data_2$Target_Increase_Gov <- ((filtered_data_2$`Target Governance Pillar Score 1 Year Prior Announcement` - filtered_data_2$`Target Governance Pillar Score 2 Year Prior Announcement`) / filtered_data_2$`Target Governance Pillar Score 1 Year Prior Announcement`) 

filtered_data_2$Target_Increase_ESG <- ((filtered_data_2$`Target Combined ESG Score 2 Year Prior Announcement`- filtered_data_2$`Target Combined ESG Score 1 Year Prior Announcement`)/filtered_data_2$`Target Combined ESG Score 2 Year Prior Announcement`)

# View the updated DataFrame with the new column
print(filtered_data_2)

#Removing Old Variables Used In the New Ones

# Define columns to be removed
columns_to_remove <- c("Acquiror Environmental Pillar Score 1 Year Prior Announcement", "Acquiror Environmental Pillar Score 2 Year Prior Announcement", "Acquiror Environmental Pillar Score Year Announcement",
                       "Acquiror Social Pillar Score 1 Year Prior Announcement", "Acquiror Social Pillar Score 2 Year Prior Announcement", "Acquiror Social Pillar Score Year Announcement",
                       "Acquiror Governance Pillar Score 1 Year Prior Announcement", "Acquiror Governance Pillar Score 2 Year Prior Announcement", "Acquiror Governance Pillar Score Year Announcement")


columns_to_remove3 <- c("Acquiror Combined ESG Score 1 Year Prior Announcement","Acquiror Combined ESG Score 2 Year Prior Announcement","Acquiror Combined ESG Score Year Announcement")

columns_to_remove7 <- c("Target Environmental Pillar Score 1 Year Prior Announcement", "Target Environmental Pillar Score 2 Year Prior Announcement",
                        "Target Social Pillar Score 1 Year Prior Announcement", "Target Social Pillar Score 2 Year Prior Announcement", 
                        "Target Governance Pillar Score 1 Year Prior Announcement", "Target Governance Pillar Score 2 Year Prior Announcement")

columns_to_remove8 <- c("Target Combined ESG Score 2 Year Prior Announcement", "Target Combined ESG Score 1 Year Prior Announcement")



# Remove specified columns
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove]
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove3]
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove7]
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove8]

#Removing Unrealistic Variables

# Define columns to be removed
columns_to_remove2 <- c("Target Environmental Pillar Score 1 Year After Announcement","Target Environmental Pillar Score 2 Year After Announcement",
                        "Target Social Pillar Score 1 Year After Announcement","Target Social Pillar Score 2 Year After Announcement",
                        "Target Governance Pillar Score 1 Year After Announcement","Target Governance Pillar Score 2 Year After Announcement")

columns_to_remove5 <- c("Acquiror Environmental Pillar Score 1 Year After Announcement","Acquiror Environmental Pillar Score 2 Year After Announcement",
                        "Acquiror Social Pillar Score 1 Year After Announcement","Acquiror Social Pillar Score 2 Year After Announcement",
                        "Acquiror Governance Pillar Score 1 Year After Announcement","Acquiror Governance Pillar Score 2 Year After Announcement")

columns_to_remove6 <- c("Target Combined ESG Score 1 Year After Announcement","Target Combined ESG Score 2 Year After Announcement")

# Remove specified columns
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove2]
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove5]
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove6]

# Define columns to be removed
columns_to_remove3 <- c("Acquiror Combined ESG Score 1 Year Prior Announcement","Acquiror Combined ESG Score 2 Year Prior Announcement","Acquiror Combined ESG Score Year Announcement")


#Removing Variables with No Explanatory Value

# Define columns to be removed
columns_to_remove4 <- c("Acquiror Full Name","Target Full Name")

# Remove specified columns
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove4]

#Removing Variables that would cause Multicollinearity
#Since we already have the E, S and G components involved for the announcement year we can remove the combines score

# Define columns to be removed
columns_to_remove9 <- c("Target Combined ESG Score Year Announcement")

# Remove specified columns
filtered_data_2 <- filtered_data_2[, !names(filtered_data_2) %in% columns_to_remove9]


###### LINEAR REGRESSION APPLICATION #######

#####Prep#####
# Check for NA values in each column
na_columns <- colnames(filtered_data_2)[apply(filtered_data_2, 2, function(x) any(is.na(x)))]

# Check for infinite values in each column
inf_columns <- colnames(filtered_data_2)[apply(filtered_data_2, 2, function(x) any(!is.finite(x)))]

# Combine columns with NA or infinite values
columns_with_issues <- unique(c(na_columns, inf_columns))

# Display columns with NA or infinite values
print(columns_with_issues)

# Identify numeric columns and replace infinite values with NA
numeric_columns <- sapply(filtered_data_2, is.numeric)
#filtered_data_2[, numeric_columns][!is.finite(filtered_data_2[, numeric_columns])] <- NA

# Replace NAs in numeric columns with 0
filtered_data_2[, numeric_columns] <- lapply(filtered_data_2[, numeric_columns], function(x) ifelse(is.na(x), 0, x))

# Identify character columns and remove rows with NAs
character_columns <- sapply(filtered_data_2, is.character)
filtered_data_2 <- filtered_data_2[complete.cases(filtered_data_2[, character_columns]), ]

# Identify character columns
character_columns <- sapply(filtered_data_2, is.character)

# Convert character columns to factors
filtered_data_2[, character_columns] <- lapply(filtered_data_2[, character_columns], as.factor)

# Check for NA, NaN, or Inf values in the dataset
problematic_rows <- apply(filtered_data_2, 1, function(row) any(is.na(row) | is.nan(row) | is.infinite(row)))

# View rows with problematic values
problematic_rows_indices <- which(problematic_rows)
print(problematic_rows_indices)

# Optionally, remove rows with problematic values
filtered_data_2_clean <- filtered_data_2[!problematic_rows, ]

# Check for NA, NaN, or Inf values in the dataset
problematic_rows <- apply(filtered_data_2, 1, function(row) any(is.na(row) | is.nan(row) | is.infinite(row)))

# View rows with problematic values
problematic_rows_indices <- which(problematic_rows)
print(problematic_rows_indices)



#ENCODING CATEGORICAL VARIABLES#

#1 Year Announced
# Calculate the encoded values based on the specified range
filtered_data_2_clean$`Encoded Years` <- as.numeric(filtered_data_2_clean$`Year Announced`) - 2010

#2 Target Nation
# List of EU countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
                  "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# Create a new column 'is_EU' based on whether the country is in the EU
filtered_data_2_clean$is_EU <- ifelse(filtered_data_2_clean$`Target Nation`%in% eu_countries, 1, 0)

#3Acquiror Macro Industry 
# Create a new column 'is_high_tech' based on the condition
filtered_data_2_clean$is_high_tech <- ifelse(filtered_data_2_clean$`Acquiror Macro Industry`== "High Technology", 1, 0)

#4Acquisition Techniques 
# Create a new column 'is_divestiture' based on the condition
filtered_data_2_clean$is_divestiture <- ifelse(filtered_data_2_clean$`Acquisition Techniques`== "Divestiture", 1, 0)

#5Form of the Deal 
# Create a new column 'is_merger' based on the condition
filtered_data_2_clean$is_merger <- ifelse(filtered_data_2_clean$`Form of the Deal`== "Merger", 1, 0)

#6Cross Border Flag
# Create a new column 'is_CrossBorder' based on the condition
filtered_data_2_clean$is_CrossBorder <- ifelse(filtered_data_2_clean$`Cross Border Deal Flag`== "TRUE", 1, 0)

#7Deal Attitude
# Create a new column 'is_Friendly' based on the condition
filtered_data_2_clean$is_Friendly <- ifelse(filtered_data_2_clean$`Deal Attitude`== "Friendly", 1, 0)

#8Cons. Structure
# Create a new column 'is_CashOnly' based on the condition
filtered_data_2_clean$is_CashOnly <- ifelse(filtered_data_2_clean$`Consideration Structure`== "Cash Only", 1, 0)

#8Strategic Purpose
# Create a new column 'is_Horizontal' based on the condition
filtered_data_2_clean$is_Horizontal <- ifelse(filtered_data_2_clean$`Strategic Purpose`== "Horizontal", 1, 0)


#Dropping Old Variables
variables_to_remove <- c("Year Announced","Target Nation","Acquiror Macro Industry","Acquisition Techniques","Form of the Deal","Cross Border Deal Flag","Deal Attitude","Consideration Structure","Strategic Purpose")

variables_to_remove2 <- c("High Tech Industry Group")

# Remove specified columns
filtered_data_2_clean <- filtered_data_2_clean[, !names(filtered_data_2_clean) %in% variables_to_remove]
filtered_data_2_clean <- filtered_data_2_clean[, !names(filtered_data_2_clean) %in% variables_to_remove2]

# Check for NA, NaN, or Inf values in the predictor variables and target variable
problematic_columns <- sapply(filtered_data_2_clean, function(col) any(is.na(col) | is.nan(col) | is.infinite(col)))

# Display columns with problematic values
print(names(filtered_data_2_clean)[problematic_columns])

# Identify rows with -Inf in "Target_Increase_Env"
rows_with_neg_inf <- which(filtered_data_2_clean$Target_Increase_Env == -Inf)

# Display rows with -Inf values
print(filtered_data_2_clean[rows_with_neg_inf, ])

# Replace -Inf values with NA or another appropriate value
filtered_data_2_clean$Target_Increase_Env[filtered_data_2_clean$Target_Increase_Env == -Inf] <- 0

write.xlsx(filtered_data_2_clean, "filtered_data_2_clean.xlsx") # All predictions are based on this data



#####Application#######

#Fine Tuning

# Remove specified columns from predictors
predictors <- filtered_data_2_clean[, !names(filtered_data_2_clean) %in% c("Acquiror Combined ESG Score 1 Year After Announcement", "Acquiror Combined ESG Score 2 Year After Announcement")]
str(predictors)
# Convert factor columns to character and then to numeric
filtered_data_2_clean$`Firm Value` <- as.numeric(as.character(filtered_data_2_clean$`Firm Value`))
filtered_data_2_clean$`Value of Equity at Effective Date` <- as.numeric(as.character(filtered_data_2_clean$`Value of Equity at Effective Date`))

# Perform feature selection (example: using LASSO)
library(glmnet)
set.seed(123)
lasso_model <- cv.glmnet(as.matrix(predictors), filtered_data_2_clean$`Acquiror Combined ESG Score 1 Year After Announcement`, alpha = 1)
# Plot coefficient paths
plot(lasso_model)
selected_predictors <- predict(lasso_model, type = "nonzero")

# Convert selected predictors indices to numeric vector
selected_predictors_indices <- as.numeric(unlist(selected_predictors))

# Extract selected predictor column names
selected_predictor_names <- colnames(predictors)[selected_predictors_indices]

# Use selected predictor names for further analysis
selected_predictors <- predictors[, selected_predictor_names]
print(selected_predictors)

# Create a data frame with selected predictor columns
selected_predictors_df <- predictors[, selected_predictor_names]

# View the structure or summary of the new data frame
str(selected_predictors_df)  # Displays the structure
summary(selected_predictors_df)  # Provides summary statistics


########

column_names <- colnames(filtered_data_2_clean)
print(column_names)

# Perform linear regression just with the selected variables from Lasso
model4<- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
              `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
              `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
              `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`, data = filtered_data_2_clean)


summary(model4)

# Perform linear regression just with the selected variables from Lasso + Horizontal
model4 <- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
               `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)


summary(model4)


# Perform linear regression
model3 <- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
               `Acquiror Number of Employees`+ `Target Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Return on Equity Last 12 Months`+`Return on Assets Last 12 Months`+ ,data = filtered_data_2_clean)

summary(model3)

# Perform linear regression
model5 <- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
               `Acquiror Number of Employees`+ `Target Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)

summary(model5)

#FINAL LM FIT
#After 1 Year
# Perform linear regression just with the selected variables from Lasso + Horizontal
model6 <- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
               `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)

summary(model6)


# Get the summary of the linear regression model
summary_model6 <- summary(model6)

# Extract the coefficients
coefficients_model6 <- summary_model6$coefficients

# Format coefficients to display in non-scientific format
coefficients_model6[, 1:4] <- lapply(coefficients_model6[, 1:4], function(x) {
  format(as.numeric(x), scientific = FALSE)
})

# View the updated coefficients
print(coefficients_model6)


#After 2 years
# Perform linear regression just with the selected variables from Lasso + Horizontal
model7 <- lm(`Acquiror Combined ESG Score 2 Year After Announcement` ~ 
               `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)

summary(model7)

#####SVM#####
library(e1071)


# Creating test and training sets

# Set a seed for reproducibility
set.seed(123)

# Get the number of rows in your dataset
total_rows <- nrow(filtered_data_2_clean)

# Generate random indices for selecting rows for the test set
test_indices <- sample(1:total_rows, 50)  # 50 rows for the test set

# Create the training and test datasets
test_data <- filtered_data_2_clean[test_indices, ]
train_data <- filtered_data_2_clean[-test_indices, ]

# Fit SVM regression model
svm_model <- svm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)
# Print the SVM model object
print(svm_model)
# View the details of the model
str(svm_model)  # Display the structure of the model object

# Make predictions on test data
predictions_svm <- predict(svm_model, newdata = test_data)

# Extract the actual target values from the test data
y_test <- test_data$`Acquiror Combined ESG Score 1 Year After Announcement`  
# Calculate Mean Squared Error (MSE)
mse <- mean((predictions_svm - y_test)^2)

# Calculate Mean Squared Error (MSE)
mse <- mean((predictions_svm - y_test)^2)

# Calculate R-squared (R²)
actual_mean <- mean(y_test)
ss_total <- sum((y_test - actual_mean)^2)
ss_residual <- sum((y_test - predictions_svm)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print or view the metrics
print(paste("Mean Squared Error (MSE):", mse))
print(paste("R-squared (R²):", r_squared))





# Fit SVM regression model for 2 year after
svm_model2 <- svm(`Acquiror Combined ESG Score 2 Year After Announcement` ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)
# Print the SVM model object
print(svm_model2)
# View the details of the model
str(svm_model2)  # Display the structure of the model object

# Make predictions on test data
predictions_svm2 <- predict(svm_model2, newdata = test_data)

# Extract the actual target values from the test data
y_test2 <- test_data$`Acquiror Combined ESG Score 2 Year After Announcement`  
# Calculate Mean Squared Error (MSE)
mse2 <- mean((predictions_svm2 - y_test2)^2)

# Calculate Mean Squared Error (MSE)
mse2 <- mean((predictions_svm2 - y_test2)^2)

# Calculate R-squared (R²)
actual_mean2 <- mean(y_test2)
ss_total2 <- sum((y_test2 - actual_mean2)^2)
ss_residual2 <- sum((y_test2 - predictions_svm2)^2)
r_squared2 <- 1 - (ss_residual2 / ss_total2)

# Print or view the metrics
print(paste("Mean Squared Error (MSE):", mse2))
print(paste("R-squared (R²):", r_squared2))


#####Comparison of r SQUARED####
#the adjusted R-squared value from SVM
r_squared_svm <- r_squared  

# Fit the linear regression model
model6 <- lm(`Acquiror Combined ESG Score 1 Year After Announcement` ~ 
               `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)
summary_model6 <- summary(model6)

# Extract the adjusted R-squared value from the linear regression model
adjusted_r_squared_model6 <- summary_model6$adj.r.squared
print(summary_model6$adj.r.squared)

# Create a scatter plot to compare adjusted R-squared values
plot(1, adjusted_r_squared_model6, xlim = c(0.2, 1), ylim = c(0.2, 1), 
     xlab = "Adjusted R-squared (SVM)", ylab = "Adjusted R-squared (MLR)",
     main = "Comparison of Adjusted R-squared Values")

# Add points for SVM and Linear Regression adjusted R-squared values
points(1, r_squared_svm, col = "red", pch = 16)
text(1, adjusted_r_squared_model6, "MLR", pos = 4)
text(1, r_squared_svm, "SVM", pos = 2, col = "red")

# Add a diagonal line for reference
abline(0, 1, col = "blue")

# Create a data frame for comparison
data_comparison <- data.frame(Method = c("MLR", "SVM"),
                              Adjusted_R_Squared = c(adjusted_r_squared_model6, r_squared_svm))

# Create a horizontal bar plot
library(ggplot2)

ggplot(data_comparison, aes(x = Adjusted_R_Squared, y = Method, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Adjusted_R_Squared, 3)), hjust = -0.2, size = 3, color = "black") +
  labs(x = "Adjusted R-squared", y = "Method", title = "Comparison of Adjusted R-squared Values for 1 Year After Prediction") +
  theme_minimal() +
  theme(legend.position = "none")

#For 2 years Later
#the adjusted R-squared value from SVM
r_squared_svm2 <- r_squared2  

# Fit the linear regression model
model7 <- lm(`Acquiror Combined ESG Score 2 Year After Announcement` ~ 
               `Acquiror Number of Employees`+`Number Of Days Between Date Announced And Date Effective`+
               `Acquiror Market Value 4 Weeks Prior to Announcement`+`Acquiror_Increase_Gov`+ 
               `Encoded Years`+`is_high_tech`+`is_EU`+`is_CrossBorder`+`is_Horizontal`, data = filtered_data_2_clean)
summary_model7 <- summary(model7)



# Extract the adjusted R-squared value from the linear regression model
adjusted_r_squared_model7 <- summary_model7$adj.r.squared
print(summary_model7$adj.r.squared)

# Create a scatter plot to compare adjusted R-squared values
plot(1, adjusted_r_squared_model7, xlim = c(0.2, 1), ylim = c(0.2, 1), 
     xlab = "Adjusted R-squared (SVM)", ylab = "Adjusted R-squared (MLR)",
     main = "Comparison of Adjusted R-squared Values")

# Add points for SVM and Linear Regression adjusted R-squared values
points(1, r_squared_svm2, col = "red", pch = 16)
text(1, adjusted_r_squared_model7, "MLR", pos = 4)
text(1, r_squared_svm2, "SVM", pos = 2, col = "red")

# Add a diagonal line for reference
abline(0, 1, col = "blue")

# Create a data frame for comparison
data_comparison2 <- data.frame(Method = c("MLR", "SVM"),
                               Adjusted_R_Squared2 = c(adjusted_r_squared_model7, r_squared_svm2))

# Create a horizontal bar plot
library(ggplot2)

ggplot(data_comparison2, aes(x = Adjusted_R_Squared2, y = Method, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Adjusted_R_Squared2, 3)), hjust = -0.2, size = 3, color = "black") +
  labs(x = "Adjusted R-squared", y = "Method", title = "Comparison of Adjusted R-squared Values for 2 Year After Prediction") +
  theme_minimal() +
  theme(legend.position = "none")




#####Testing H5####

#Deviation of the post-M&A ESG score from the acquirer’s pre-M&A ESG score is less than 20 percent.


# Calculate deviation in percentages
deviation <- ((filtered_data_2_old$`Acquiror Combined ESG Score 1 Year After Announcement` - filtered_data_2_old$`Acquiror Combined ESG Score 1 Year Prior Announcement`) / filtered_data_2_old$`Acquiror Combined ESG Score 1 Year Prior Announcement`) * 100

# One-sample t-test
t_test_result <- t.test(deviation, mu = 20, alternative = "less")

# Output the test result
print(t_test_result)

# Calculate deviation in percentages
deviation2 <- ((filtered_data_2_old$`Acquiror Combined ESG Score 2 Year After Announcement` - filtered_data_2_old$`Acquiror Combined ESG Score 2 Year Prior Announcement`) / filtered_data_2_old$`Acquiror Combined ESG Score 2 Year Prior Announcement`) * 100

# One-sample t-test
t_test_result2 <- t.test(deviation, mu = 20, alternative = "less")

# Output the test result
print(t_test_result2)

# Plotting a histogram of deviation percentages
hist(deviation, breaks = 10, col = "skyblue", 
     xlab = "Deviation (%)", ylab = "Frequency",
     main = "Distribution of Deviation Percentages between Acquiror Combined ESG Score 1 Year Prior vs 1 Year After Announcement")

# Adding a vertical line at 20% for reference
abline(v = 20, col = "red", lwd = 2)

# Adding a legend
legend("topright", legend = c("20% Threshold"), fill = c("red"), bty = "n")



# Plotting a histogram of deviation percentages
hist(deviation2, breaks = 10, col = "pink", 
     xlab = "Deviation (%)", ylab = "Frequency",
     main = "Distribution of Deviation Percentages between Acquiror Combined ESG Score 2 Year Prior vs 2 Year After Announcement")

# Adding a vertical line at 20% for reference
abline(v = 20, col = "red", lwd = 2)

# Adding a legend
legend("topright", legend = c("20% Threshold"), fill = c("red"), bty = "n")

###Testing H5 with initial dataset with NAs

filtered_data_wNAs <- read_excel("filtered_data_with_NAs.xlsx")

# Calculate deviation in percentages
deviation3 <- ((filtered_data_wNAs$`Acquiror Combined ESG Score 1 Year After Announcement` - filtered_data_wNAs$`Acquiror Combined ESG Score 1 Year Prior Announcement`) / filtered_data_wNAs$`Acquiror Combined ESG Score 1 Year Prior Announcement`) * 100

# One-sample t-test
t_test_result3 <- t.test(deviation3, mu = 20, alternative = "less")

# Output the test result
print(t_test_result3)

# Plotting a histogram of deviation percentages
hist(deviation3, breaks = 10, col = "forestgreen", 
     xlab = "Deviation (%)", ylab = "Frequency",
     main = "Distribution of Deviation Percentages between Acquiror Combined ESG Score 1 Year Prior vs 1 Year After Announcement")

# Adding a vertical line at 20% for reference
abline(v = 20, col = "red", lwd = 2)

# Adding a legend
legend("topright", legend = c("20% Threshold"), fill = c("red"), bty = "n")

# Calculate deviation in percentages
deviation4 <- ((filtered_data_wNAs$`Acquiror Combined ESG Score 2 Year After Announcement` - filtered_data_wNAs$`Acquiror Combined ESG Score 2 Year Prior Announcement`) / filtered_data_wNAs$`Acquiror Combined ESG Score 2 Year Prior Announcement`) * 100

# One-sample t-test
t_test_result4 <- t.test(deviation4, mu = 20, alternative = "less")

# Output the test result
print(t_test_result4)

# Plotting a histogram of deviation percentages
hist(deviation4, breaks = 10, col = "orange", 
     xlab = "Deviation (%)", ylab = "Frequency",
     main = "Distribution of Deviation Percentages between Acquiror Combined ESG Score 2 Year Prior vs 2 Year After Announcement")

# Adding a vertical line at 20% for reference
abline(v = 20, col = "red", lwd = 2)

# Adding a legend
legend("topright", legend = c("20% Threshold"), fill = c("red"), bty = "n")

