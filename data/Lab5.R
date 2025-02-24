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


