library(tidyverse)

data= read.csv("data/essentia.data.csv") #gathers data for all 180 songs other than Allentown
allentown.data=read.csv("data/essentia.data.allentown.csv") #gathers data for the song Allentown

#creating the data we want for overall loudness
data %>%
  group_by(artist) %>% #groups the data by the artist
  summarize(min_overall_loudness = min(overall_loudness, na.rm = TRUE),
            LF_overall_loudness = quantile(overall_loudness, probs = .25) - 1.5*IQR(overall_loudness),
            UF_overall_loudness = quantile(overall_loudness, probs = .75) + 1.5*IQR(overall_loudness),
            max_overall_loudness = max(overall_loudness, na.rm = TRUE)) %>%
  rowwise()|>
  mutate(out.of.range=allentown.data$overall_loudness>max_overall_loudness || allentown.data$overall_loudness<min_overall_loudness) %>% #sees if less than min or greater than max
  mutate(unusual= allentown.data$overall_loudness>UF_overall_loudness || allentown.data$overall_loudness<LF_overall_loudness) %>% #sees if less than LF or more than UF
  mutate(description = case_when( out.of.range == TRUE ~ "Out of Range", unusual == TRUE ~ "Outlying", TRUE ~ "Within Range"))
  
  