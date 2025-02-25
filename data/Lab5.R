library(tidyverse)

data= read.csv("data/essentia.data.csv") #gathers data for all 180 songs other than Allentown
allentown.data=read.csv("data/essentia.data.allentown.csv") #gathers data for the song Allentown

#creating the data we want for overall loudness
data %>%
  group_by(artist) |> #groups the data by the artist
  summarize(min_overall_loudness = min(overall_loudness, na.rm = TRUE),
            LF_overall_loudness = quantile(overall_loudness, probs = .25) - 1.5*IQR(overall_loudness),
            UF_overall_loudness = quantile(overall_loudness, probs = .75) + 1.5*IQR(overall_loudness),
            max_overall_loudness = max(overall_loudness, na.rm = TRUE)) |>
  rowwise()|>
  mutate(out.of.range=allentown.data$overall_loudness>max_overall_loudness || allentown.data$overall_loudness<min_overall_loudness) |> #sees if less than min or greater than max
  mutate(unusual= allentown.data$overall_loudness>UF_overall_loudness || allentown.data$overall_loudness<LF_overall_loudness) |> #sees if less than LF or more than UF
  mutate(description = case_when( out.of.range == TRUE ~ "Out of Range", unusual == TRUE ~ "Outlying", TRUE ~ "Within Range")) #writes the desired description for each scenario
 
#function that tests for the desired statistics for a given feature 
analyze_feature <- function(data, allentown.data, feature) {
  
  data %>%
    group_by(artist) |> 
    summarize(
      min_feature = min(get(feature), na.rm = TRUE),
      LF_feature = quantile(get(feature), probs = .25, na.rm=TRUE) - 1.5 * IQR(get(feature), na.rm=TRUE),
      UF_feature = quantile(get(feature), probs = .75, na.rm=TRUE) + 1.5 * IQR(get(feature), na.rm=TRUE),
      max_feature = max(get(feature), na.rm = TRUE)
    ) |> 
    rowwise() |> 
    mutate(
      out.of.range = allentown.data[[feature]] > max_feature || allentown.data[[feature]] < min_feature,
      unusual = allentown.data[[feature]] > UF_feature || allentown.data[[feature]] < LF_feature,
      description = case_when(
        out.of.range == TRUE ~ "Out of Range",
        unusual == TRUE ~ "Outlying",
        TRUE ~ "Within Range"
      )
    )
}

# Create an empty list to store results for each feature
results_list <- list()

# Loop over the columns of the data frame
for (feature in colnames(data)) {
  # Check if the feature is numeric
  if (is.numeric(data[[feature]])) {
    feature_results = analyze_feature(data, allentown.data, feature)
    # Store the results for this feature in the list
    results_list[[feature]] = feature_results
  }
}

# Combine all the results into a single data frame
final_results <- bind_rows(results_list, .id = "feature")


library(xtable)
# Select relevant columns for the table
table_data <- final_results %>%
  select(artist, feature, min_feature, LF_feature, UF_feature, max_feature, description)

#creates a subset of table_data containing all the features you want
table_data_subset = table_data %>%
  filter(feature %in% c("overall_loudness", "danceability", 
                        "Analytic", "timbreBright"))

# Create table using xtable to copy over to LaTeX
latex_table <- xtable(table_data_subset, 
                      caption = "Feature Summary for Allentown's Collaborative Track", 
                      label = "tab:feature_summary")


# Print the LaTeX table
print(latex_table, type = "latex", include.rownames = FALSE)




# Load necessary libraries
library(ggplot2)   # For creating the plots
library(patchwork)  # For combining multiple plots into one
library(dplyr)      # For data manipulation

# Function to create box plot for a given feature, highlighting "Allentown"
create_feature_plot <- function(feature) {
  # Combine data from other artists and Allentown
  all_data <- bind_rows(
    data %>% select(artist, feature) %>% mutate(type = "Other"),  # Label other artists as "Other"
    allentown.data %>% select(feature) %>% mutate(artist = "Allentown", type = "Allentown")  # Label Allentown
  )
  
  # Create the box plot
  p <- ggplot(all_data, aes(x = artist, y = get(feature), fill = type)) +
    geom_boxplot(alpha = 0.5, outlier.colour = "red", outlier.shape = 16) +  # Boxplot with red outliers
    labs(title = paste("Boxplot of", feature, "Comparison"), x = "Artist", y = feature) +
    scale_fill_manual(values = c("Other" = "gray", "Allentown" = "red")) +  # Custom colors for "Other" and "Allentown"
    theme_minimal()  # Minimal plot theme
  
  return(p)  # Return the plot
}

# Function to create a dataset of average feature values for artists and Allentown
create_avg_comparison <- function(feature) {
  artist_averages <- data %>%
    group_by(artist) %>%
    summarize(mean_value = mean(get(feature), na.rm = TRUE)) %>%
    mutate(type = "Other")  # Label other artists as "Other"
  
  allentown_avg <- allentown.data %>%
    summarize(mean_value = mean(get(feature), na.rm = TRUE)) %>%
    mutate(artist = "Allentown", type = "Allentown")  # Label Allentown as "Allentown"
  
  combined_data <- bind_rows(artist_averages, allentown_avg)  # Combine artist and Allentown data
  return(combined_data)  # Return the combined data
}

# List of features to compare
features_to_compare <- c("overall_loudness", "danceability", "Analytic", "dissonance")

# Create box plots for each feature
plot_loudness <- create_feature_plot("overall_loudness")
plot_danceability <- create_feature_plot("danceability")
plot_analytic <- create_feature_plot("Analytic")
plot_dissonance <- create_feature_plot("dissonance")

# Combine the box plots into one plot using patchwork
final_boxplot <- (plot_loudness | plot_danceability) / (plot_analytic | plot_dissonance)

# Create bar plots comparing average feature values for each feature
feature_plots <- lapply(features_to_compare, function(feature) {
  avg_comparison <- create_avg_comparison(feature)  # Get average values for the feature
  
  # Create a bar plot for average comparison
  p <- ggplot(avg_comparison, aes(x = artist, y = mean_value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +  # Bar plot
    labs(title = paste("Average", feature, "Comparison"), x = "Artist", y = paste("Average", feature)) +
    scale_fill_manual(values = c("Other" = "gray", "Allentown" = "red")) +  # Custom colors for "Other" and "Allentown"
    theme_minimal()  # Minimal plot theme
  
  return(p)  # Return the plot
})

# Combine the bar plots into one plot using patchwork
final_avg_plot <- (feature_plots[[1]] | feature_plots[[2]]) / (feature_plots[[3]] | feature_plots[[4]])

# Display the final combined boxplot and average comparison plots
final_boxplot  # Boxplot comparison
final_avg_plot  # Average comparison bar plot
