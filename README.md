Ranking CMIP6 Climate Models Using Multi-Criteria Decision-Making (MCDM) | Climate Data Analysis in R
# Step 1: Install & Load Required Packages
install.packages(c("topsis", "MCDA", "PMCMRplus", "dplyr", "ggplot2", "readr", "GameTheory"))  
library(topsis)       # For TOPSIS ranking
library(MCDA)         # For Cooperative Game Theory
library(PMCMRplus)    # For pairwise ranking analysis
library(dplyr)        # For data manipulation
library(ggplot2)      # For visualization
library(readr)        # For reading CSV files
library(GameTheory)   # For Shapley Value calculation

# Step 2: Load Data into R
# Load CMIP6 data from CSV
cmip6_data <- read_csv("cmip6_gcm_metrics.csv")

# Print the dataset
print("CMIP6 Climate Model Metrics:")
print(cmip6_data)
📌 CMIP6 Climate Model Evaluation Metrics Used:

    SSD (Sum of Squared Differences) → Minimize
    MSD (Mean Squared Difference) → Minimize
    RMSD (Root Mean Squared Deviation) → Minimize
    CC (Correlation Coefficient) → Maximize
    NRMSD (Normalized RMSD) → Minimize
    ANMBD (Annual Normalized Mean Bias Deviation) → Minimize
    ARRBD (Annual Relative Root Bias Deviation) → Minimize
    Bias → Minimize
    NSE (Nash-Sutcliffe Efficiency) → Maximize

# Step 3: Normalize the Decision Matrix

# Define correct impact signs  (use "+" for maximization and "-" for minimization)
criteria_types <- c("-", "-", "-", "+", "-", "-", "-", "-", "+")  # Adjusted to correct signs

# Normalize function (Min-Max Scaling)
normalize_matrix <- function(matrix, criteria_types) {
  norm_matrix <- matrix
  for (i in 1:ncol(matrix)) {
    if (criteria_types[i] == "-") {
      norm_matrix[, i] <- (max(matrix[, i]) - matrix[, i]) / (max(matrix[, i]) - min(matrix[, i]) + 1e-10)
    } else {
      norm_matrix[, i] <- (matrix[, i] - min(matrix[, i])) / (max(matrix[, i]) - min(matrix[, i]) + 1e-10)
    }
  }
  return(norm_matrix)
}

# Normalize CMIP6 metrics
decision_matrix <- as.matrix(cmip6_data[, 2:10])  # Extract numerical columns
norm_matrix <- normalize_matrix(decision_matrix, criteria_types)

# Step 4: Compute Entropy Weights
compute_entropy <- function(norm_matrix) {
  prob_matrix <- norm_matrix / rowSums(norm_matrix + 1e-10)
  entropy_values <- -colSums(prob_matrix * log(prob_matrix + 1e-10)) / log(nrow(norm_matrix))
  return(entropy_values)
}

# Compute entropy values and weights
entropy_values <- compute_entropy(norm_matrix)
weights_entropy <- (1 - entropy_values) / sum(1 - entropy_values)

print("Entropy Weights for Criteria:")
print(weights_entropy)

# Step 5: Rank GCMs using Multiple Methods
# 1. Entropy Weighting Ranking
final_scores_entropy <- rowSums(norm_matrix * weights_entropy)
ranking_entropy <- rank(-final_scores_entropy)  
result_entropy <- data.frame(Model = cmip6_data$Model, Score = final_scores_entropy, Rank = ranking_entropy)
print("Entropy Weighting Ranking:")
print(result_entropy[order(result_entropy$Rank), ])

# 2. Weighted Average Technique (WAT)
final_scores_wat <- rowSums(decision_matrix * weights_entropy)
ranking_wat <- rank(-final_scores_wat)
result_wat <- data.frame(Model = cmip6_data$Model, Score = final_scores_wat, Rank = ranking_wat)
print("Weighted Average Technique (WAT) Ranking:")
print(result_wat[order(result_wat$Rank), ])

# 3. Cooperative Game Theory (Shapley Value)
# Define a function to calculate Shapley Value
calculate_shapley <- function(weights, decision_matrix) {
  # Normalize the decision matrix (if not already normalized)
  norm_matrix <- normalize_matrix(decision_matrix, criteria_types)
  
  # Calculate Shapley Value (example implementation)
  shapley_values <- apply(norm_matrix, 1, function(row) {
    sum(row * weights) / length(row)
  })
  
  return(shapley_values)
}

# Compute Shapley Value
shapley_result <- calculate_shapley(weights_entropy, decision_matrix)

# Rank based on Shapley Value
ranking_coop <- rank(-shapley_result)

# Store Results
result_coop <- data.frame(Model = cmip6_data$Model, Score = shapley_result, Rank = ranking_coop)

# Print Cooperative Game Theory (Shapley) Ranking
print("Cooperative Game Theory (Shapley) Ranking:")
print(result_coop[order(result_coop$Rank), ])

# 4. TOPSIS Ranking
# Run TOPSIS with corrected criteria_types
topsis_result <- topsis(decision_matrix, weights_entropy, criteria_types)

# Extract ranking
ranking_topsis <- topsis_result$rank

# Store Results
result_topsis <- data.frame(Model = cmip6_data$Model, Score = topsis_result$score, Rank = ranking_topsis)

# Print TOPSIS Ranking
print("TOPSIS Ranking:")
print(result_topsis[order(result_topsis$Rank), ])

# Ensure All Ranking Results Exist
print(result_topsis[order(result_topsis$Rank), ])
print(result_entropy)
print(result_wat)
print(result_coop)
print(result_topsis)
# Step 6: Final Mixed-Methods Ranking
# Combine all rankings into a single data frame
combined_rankings <- data.frame(
  Model = cmip6_data$Model,
  Entropy_Rank = result_entropy$Rank,
  WAT_Rank = result_wat$Rank,
  Shapley_Rank = result_coop$Rank,
  TOPSIS_Rank = result_topsis$Rank
)

# Calculate average rank for each model
combined_rankings$Average_Rank <- rowMeans(combined_rankings[, -1])  # Exclude the "Model" column

# Rank models based on average rank
combined_rankings$Final_Rank <- rank(combined_rankings$Average_Rank)

# Sort by final rank
final_ranking <- combined_rankings[order(combined_rankings$Final_Rank), ]

# Print the final mixed-methods ranking
print("Final Mixed-Methods Ranking:")
print(final_ranking[, c("Model", "Average_Rank", "Final_Rank")])

# Step 7: Visualize the Final Ranking
ggplot(final_ranking, aes(x = reorder(Model, -Final_Rank), y = Final_Rank)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Final Mixed-Methods Ranking of Climate Models",
       x = "Model",
       y = "Final Rank") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 8: Save the Final Ranking to a CSV File
write_csv(final_ranking, "final_mixed_methods_ranking.csv")
