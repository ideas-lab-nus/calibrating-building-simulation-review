library(tidyverse)
library(data.table)

dat <- fread(file.path("data", "data.csv"),
             na.strings = c("", "NA")
)
geocode <- fread(file.path("data", "location_geocode.csv"),
                 na.strings = c("", "NA")
)

# split concatenated strings
cols <- c(
  "AnalyticalTechniques", "EvaluationMetrics", "SensitivityAnalysis",
  "Location", "Climate", "ObservedOutput", "ObservedInput",
  "CalibrationParameter"
)
dat[, c(cols) := lapply(.SD, stringi::stri_split_regex, "\\s*;\\s*",
                        omit_empty = TRUE
),
.SDcols = cols
]

# Table 2 ----------------------------------------------------------------------
dat_simEngine <- dat %>%
  select(No, SimulationEngine) %>%
  drop_na() %>%
  distinct() %>%
  count(SimulationEngine) %>%
  mutate(SimulationEngine = ifelse(n <= 3, "Others", SimulationEngine)) %>%
  group_by(SimulationEngine) %>%
  summarise(n = sum(n)) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 0),
    label = paste0("\n", round(n / sum(n) * 100, 0), "%")
  )

# Table 3 ----------------------------------------------------------------------
dat_metric <- dat %>%
  select(No, EvaluationMetrics) %>%
  unnest(EvaluationMetrics) %>%
  distinct() %>%
  drop_na() %>%
  count(EvaluationMetrics) %>%
  mutate(EvaluationMetrics = ifelse(n <= 2, "Others", EvaluationMetrics)) %>%
  group_by(EvaluationMetrics) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = round(n / 107, 2))  # divide by number of papers reviewed
