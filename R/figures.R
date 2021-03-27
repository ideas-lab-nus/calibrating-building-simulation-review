library(data.table)
library(gridExtra)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(tidyverse)

# read data 
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

# Figure 1 ---------------------------------------------------------------------

# summarize the percentage of instances based on their spatial and 
# temporal scales 
dat_spatial_temporal <- dat %>%
  select(No, sub, Resolution, Scale) %>%
  drop_na() %>%
  group_by(Resolution, Scale) %>%
  summarise(n = n())

# plot comparing the data temporal resolution across various spatial scale 
p_spatial_temporal_stacked <- ggplot(
  dat_spatial_temporal,
  aes(
    x = factor(Scale,
               level = c("Component / System", "Building", "Urban")
    ),
    y = n,
    fill = factor(Resolution,
                  level = c("Annual", "Monthly", "Weekly", "Daily", "Hourly", "Sub-hourly")
    )
  )
) +
  geom_bar(position = "fill", stat = "identity", width = 0.75) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Spatial Scale") +
  ylab("Percentage of Instances") +
  labs(fill = "Temporal\nScales")

# Save plot for Figure 1 of paper
pdf(file = file.path("paper", "figures", "spatial_temporal_stacked.pdf"), 
    height = 5, width = 5.5)
p_spatial_temporal_stacked
dev.off()


# Figure 2 ---------------------------------------------------------------------

# count the number of instances based on the location and scale of the study
dat_lat_lon <- dat %>%
  select(No, Location, Scale) %>%
  unnest(Location) %>%
  distinct() %>%
  drop_na() %>%
  group_by(Location, Scale) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  inner_join(geocode, by = "Location")

# plot indicating location of different case studies based on their
# geographic location. 
mp <- ggplot(dat_lat_lon, aes(
  x = lon, y = lat,
  fill = factor(Scale,  
    level = c("Component / System", "Building", "Urban")
  ),
  size = n
)) +
  borders("world", colour = "#BDBDBD", size = 0.25) +  # create a layer of borders
  geom_point(shape = 21, alpha = 0.5, color = "black") +
  scale_fill_brewer(name = "Scale", palette = "Accent") +
  scale_size(  
    name = "No. of\nInstances",
    range = c(1, 6)
  ) +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8),
    axis.text = element_blank(), axis.title = element_blank(),
    panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  xlab("longitude") +
  ylab("latitude") +
  ylim(-55, 83)

# create data frame mapping different climate zones to a color
dat_climate_col_map <- data.frame(
  Climate = c(
    "Af", "Am", "Aw", "As",
    "BWh", "BWk", "BSh", "BSk",
    "Csa", "Csb", "Csc",
    "Cwa", "Cwb", "Cwc",
    "Cfa", "Cfb", "Cfc",
    "Dsa", "Dsb", "Dsc", "Dsd",
    "Dwa", "Dwb", "Dwc", "Dwd",
    "Dfa", "Dfb", "Dfc", "Dfd",
    "ET", "EF"
  ),
  col = c(
    colorRampPalette(c("red", "darkred"))(4),
    colorRampPalette(c("yellow", "orange"))(4),
    colorRampPalette(c("lightgreen", "darkgreen"))(9),
    colorRampPalette(c("lightblue", "darkblue"))(12),
    colorRampPalette(c("lightgrey", "darkgrey"))(2)
  )
)

# count the number of instances for each climate zone
dat_climate <- dat %>%
  select(No, Climate) %>%
  unnest(Climate) %>%
  drop_na() %>%
  distinct() %>%
  group_by(Climate) %>%
  summarise(n = n()) %>%
  right_join(dat_climate_col_map, by = "Climate") %>%
  mutate_if(is.integer, ~ replace(., is.na(.), 0)) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0("\n", round(n / sum(n) * 100, 1), "%"),
    climate_group = case_when(
      str_starts(Climate, "A") ~ "equatorial",
      str_starts(Climate, "B") ~ "arid",
      str_starts(Climate, "C") ~ "warm temperate",
      str_starts(Climate, "D") ~ "snow",
      str_starts(Climate, "E") ~ "polar"
    ),
    percipitation_group = case_when(
      str_detect(Climate, ".W|.W.") ~ "desert",
      str_detect(Climate, ".S|.S.") ~ "steppe",
      str_detect(Climate, ".f|.f.") ~ "fully humid",
      str_detect(Climate, ".s|.s.") ~ "summer dry",
      str_detect(Climate, ".w|.w.") ~ "winter dry",
      str_detect(Climate, ".m|.m.") ~ "monsoonal"
    ),
    temperature_group = case_when(
      str_detect(Climate, "..h") ~ "hot arid",
      str_detect(Climate, "..k") ~ "cold arid",
      str_detect(Climate, "..a") ~ "hot summer",
      str_detect(Climate, "..b") ~ "warm summer",
      str_detect(Climate, "..d") ~ "extremely continental",
      str_detect(Climate, "..c") ~ "cool summer",
      str_detect(Climate, "..F") ~ "polar frost",
      str_detect(Climate, "..T") ~ "polar tundra"
    )
  )

# colors for different climate zones
cols <- as.character(dat_climate$col)  
names(cols) <- dat_climate$Climate

# position and size of annotations
text_x <- 18; text_y <- 31
text_size <- 2.5; text_x_int <- 3.5

# bar plot indicating number of instances across different climate zones
p_climate <- ggplot(
  dat_climate,
  aes(y = n, x = Climate, fill = Climate)
) +
  geom_bar(color = "black", stat = "identity", width = 0.75) +
  scale_fill_manual(values = cols) +
  ylab("Number of Instances") +
  xlab("Köppen Climate Classification") +
  annotate(
    geom = "text", x = text_x, y = text_y,
    label = "First letter\nA: equatorial\nB: arid\nC: warm temperate\nD: snow\nE: polar",
    size = text_size,
    hjust = 0, vjust = 1
  ) +
  annotate(
    geom = "text", x = text_x + text_x_int, y = text_y,
    label = "Second letter\nW: desert\nS: steppe\nf: fully humid\ns: summer dry\nw: winter dry\nm: monsoonal",
    size = text_size,
    hjust = 0, vjust = 1
  ) +
  annotate(
    geom = "text", x = text_x + 1.75 * text_x_int, y = text_y,
    label = "\nF: polar frost\nT: polar tundra",
    size = text_size,
    hjust = 0, vjust = 1
  ) +
  annotate(
    geom = "text", x = text_x + 2.75 * text_x_int, y = text_y,
    label = "Third letter\nh: hot arid\nk: cold arid\na: hot summer\nb: warm summer\nd: extremely continental\nc: cool summer",
    size = text_size,
    hjust = 0, vjust = 1
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    legend.key.size = unit(3, "mm"), legend.text = element_text(size = 8),
    axis.text = element_text(size = 8), axis.title = element_text(size = 8),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.25)
  )

# Save plot for Figure 2 of paper
pdf(
  file = file.path("paper", "figures", "map_climate.pdf"),
  height = 9, width = 9
)
grid.arrange(mp,
  p_climate,
  heights = c(5, 4),
  nrow = 2
)
dev.off()

# Figure 4 ---------------------------------------------------------------------

# count the number of papers based on the observed output used for the 
# calibration
dat_obs_output <- dat %>%
  select(No, "ObservedOutput") %>%
  unnest(ObservedOutput) %>%
  distinct() %>%
  drop_na() %>%
  count(ObservedOutput, sort = TRUE) %>%
  mutate(cummulative_count = cumsum(n)) %>%
  mutate(
    ratio = cummulative_count / sum(n),
    percentage = round(n / 107, digits = 2)
  ) %>%
  filter(n >= 5) %>% # filter out observed inputs with count <= 5
  mutate(
    obs_output_level = str_replace(ObservedOutput, "\\_.*", ""),
    obs_output_variable = str_replace(ObservedOutput, ".*\\_", "")
  )

# bar-plot of observed outputs ranked by the number of papers
p_obs_output_rank <- ggplot(dat_obs_output, aes(
  x = n, y = reorder(obs_output_variable, n, sum),
  fill = factor(obs_output_level,
    level = c("Building", "Zone", "Component")
  )
)) +
  geom_bar(color = "#B3B3B3", stat = "identity") +
  scale_fill_brewer(palette = "Set2", name = "Parameter\nClass") +
  xlab("Number of Papers") +
  ylab("Observed Outputs") +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank()
  )

# Save plot for Figure 4 of paper
pdf(file = file.path("paper", "figures", "obs_output_rank.pdf"), 
    height = 2.5, width = 8)
p_obs_output_rank
dev.off()

# Figure 5 ---------------------------------------------------------------------

# count the number of papers based on the observed inputs used for the 
# calibration
dat_obs_input <- dat %>%
  select(No, "ObservedInput") %>%
  unnest(ObservedInput) %>%
  distinct() %>%
  drop_na() %>%
  count(ObservedInput, sort = TRUE) %>%
  mutate(cummulative_count = cumsum(n)) %>%
  mutate(ratio = cummulative_count / sum(n)) %>%
  filter(n >= 3) %>%  # filter out observed inputs with count <= 2
  mutate(
    obs_input_level = str_replace(ObservedInput, "\\_.*", ""),
    obs_input_variable = str_replace(ObservedInput, ".*\\_", "")
  )

# bar plot of observed inputs ranked by the number of papers
p_obs_input_rank <- ggplot(dat_obs_input, aes(
  x = n, y = reorder(obs_input_variable, n, sum),
  fill = obs_input_level
)) +
  geom_bar(color = "#B3B3B3", stat = "identity") +
  scale_fill_brewer(palette = "Set2", name = "Parameter\nClass") +
  xlab("Number of Papers") +
  ylab("Observed Inputs") +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank()
  )

# Save plot for Figure 5 of paper
pdf(file = file.path("paper", "figures", "obs_input_rank.pdf"), 
    height = 5, width = 8)
p_obs_input_rank
dev.off()

# Figure 6 ---------------------------------------------------------------------

# count the number of papers based on the calibration parameters used for 
# the calibration
dat_parameter <- dat %>%
  select(No, "CalibrationParameter") %>%
  unnest(CalibrationParameter) %>%
  distinct() %>%
  drop_na() %>%
  count(CalibrationParameter, sort = TRUE) %>%
  mutate(cummulative_count = cumsum(n)) %>%
  mutate(ratio = cummulative_count / sum(n)) %>%
  filter(n >= 7) %>%
  mutate(
    param_level = str_replace(CalibrationParameter, "\\_.*", ""),
    param_variable = str_replace(CalibrationParameter, ".*\\_", "")
  )

# bar plot of calibration parameters ranked by the number of papers
p_parameter_rank <- ggplot(dat_parameter, aes(
  x = n, y = reorder(param_variable, n, sum),
  fill = param_level
)) +
  geom_bar(color = "#B3B3B3", stat = "identity") +
  scale_fill_manual(
    breaks = c("Envelope", "InternalGains", "Zone", "HVAC", "HotWater", "Geometry"),
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
    name = "Parameter\nClass"
  ) +
  xlab("Number of Papers") +
  ylab("Calibration Parameters") +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank()
  )

# Save plot for Figure 6 of paper
pdf(file = file.path("paper", "figures", "param_rank.pdf"), 
    height = 5, width = 8)
p_parameter_rank
dev.off()


# Figure 7 ---------------------------------------------------------------------

# count the number of papers based on the observed output and corresponding
# calibration parameters used for the calibration
dat_output_parameter <- dat %>%
  select(No, "ObservedOutput", "CalibrationParameter") %>%
  distinct() %>%
  unnest(ObservedOutput) %>%
  filter(str_detect(
    ObservedOutput,
    paste(dat_obs_output$ObservedOutput, collapse = "|")
  )) %>%
  unnest(CalibrationParameter) %>%
  filter(str_detect(
    CalibrationParameter,
    paste(dat_parameter$CalibrationParameter, collapse = "|")
  )) %>%
  drop_na() %>%
  distinct() %>%
  group_by(ObservedOutput, CalibrationParameter) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  mutate(cummulative_count = cumsum(n)) %>%
  mutate(ratio = cummulative_count / sum(n)) %>%
  mutate(
    param_level = str_replace(CalibrationParameter, "\\_.*", ""),
    param_variable = str_replace(CalibrationParameter, ".*\\_", ""),
    output_level = str_replace(ObservedOutput, "\\_.*", ""),
    output_variable = str_replace(ObservedOutput, ".*\\_", "")
  ) %>%
  filter(n >= 4)

# plot of observed outputs against calibration parameters 
p_output_parameter <- ggplot(
  dat_output_parameter,
  aes(
    x = factor(ObservedOutput,
      level = dat_obs_output$ObservedOutput
    ),
    y = factor(param_variable,
      level = rev(c(
        "MaterialProperties", "InfiltrationRate",
        "EPD", "OccupantDensity", "LPD", "OccupantSch", "EquipmentSch", "LightsSch",
        "HeatingSetpoint", "CoolingSetpoint", "OutdoorAir",
        "ComponentEfficiency", "OperationSch", "ComponentCapacity",
        "Usage"
      ))
    ),
    size = n,
    color = param_level,
    alpha = n
  )
) +
  geom_point() +
  scale_colour_manual(
    breaks = c("Envelope", "InternalGains", "Zone", "HVAC", "HotWater"),
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
    name = "Parameter\nClass"
  ) +
  geom_text(aes(label = n), size = 3.5, color = "black", alpha = 1) +
  scale_size(
    name = "No. of\nPapers",
    range = c(1, 10)
  ) +
  scale_alpha(guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Observed Outputs") +
  ylab("Calibration Parameters")

# Save plot for Figure 7 of paper
pdf(file = file.path("paper", "figures", "output_parameter.pdf"), 
    height = 7, width = 8)
p_output_parameter
dev.off()

# Figure 8 ---------------------------------------------------------------------

# count the number of papers that used testing data for the evaluation of 
# calibration performance
dat_test <- dat %>%
  select(y = TestData) %>%
  drop_na() %>%
  count(y) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(y, "\n", round(n / sum(n) * 100, 1), "%")
  )

# pie-chart of comparing the use of testing data
p_test <- ggplot(dat_test, aes(x = "", y = reorder(percentage, n, sum), fill = y)) +
  geom_bar(color = "black", stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("No" = "#FC8D62", "Yes" = "#66C2A5")) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Assessed calibration performance with\nout-of-sample test dataset")

# Save plot for Figure 8 of paper
pdf(file = file.path("paper", "figures", "test_data.pdf"), 
    height = 5, width = 4)
p_test
dev.off()


# Figure 9 ---------------------------------------------------------------------

# count the number of papers that used a manual vs automated calibration approach
dat_calibApproach <- dat %>%
  select(y = CalibrationApproach) %>%
  drop_na() %>%
  count(y) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 0),
    label = paste0(y, "\n", round(n / sum(n) * 100, 0), "%")
  )

# pie-chart comparing percentage of papers using manual vs automated calibration approach
p_calibApproach <- ggplot(dat_calibApproach, aes(x = "", y = reorder(percentage, n, sum), fill = y)) +
  geom_bar(color = "black", stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Manual" = "#FC8D62", "Automated" = "#66C2A5")) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

# count the number of papers based on the method of automated calibration approach
dat_calibMethod <- dat %>%
  select(y = CalibrationMethod) %>%
  dplyr::filter(y != "Manual") %>%
  drop_na() %>%
  count(y) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 0),
    label = paste0(y, "\n", round(n / sum(n) * 100, 0), "%")
  )

# bar-plot comparing number of papers for different methods of automated calibration
p_calibMethod <- ggplot(
  dat_calibMethod,
  aes(y = reorder(y, n, sum), x = n)
) +
  geom_bar(color = "black", stat = "identity", fill = "#66C2A5", width = 0.75) +
  xlab("No. of Papers") +
  ylab("Automated\nCalibration Approaches")

# Save plot for Figure 9 of paper
pdf(file = file.path("paper", "figures", "auto_manual.pdf"), 
    height = 3.5, width = 8)
grid.arrange(p_calibApproach, p_calibMethod,
  widths = c(1, 2),
  nrow = 1
)
dev.off()

# Figure 11 --------------------------------------------------------------------

# count the number of papers based on the type of analytical techniques 
dat_analytic <- dat %>%
  select(No, AnalyticalTechniques) %>%
  unnest(AnalyticalTechniques) %>%
  distinct() %>%
  mutate(AnalyticalTechniques = toupper(AnalyticalTechniques)) %>%
  drop_na() %>%
  group_by(AnalyticalTechniques) %>%
  summarise(n = n())

# bar-plot comparing number of papers across different analytical techniques
p_analytic <- ggplot(
  dat_analytic,
  aes(reorder(AnalyticalTechniques, -n, sum), y = n) # arrange bars based on count
) +
  geom_bar(color = "black", stat = "identity", fill = "#66C2A5") +
  ylab("Number of Papers") +
  xlab("Analytical Techniques")

# Save plot for Figure 11 of paper
pdf(file = file.path("paper", "figures", "analytical.pdf"),
    height = 3.5, width = 8)
p_analytic
dev.off()


# Figure 12 --------------------------------------------------------------------

# count the number of papers based on the type of sensitivity analysis, the
# general category of sensitivity analysis method, and the specific 
# sensitivity analysis algorithm
dat_sa <- dat %>%
  select(No, SensitivityAnalysis, SensitivityAnalysisType) %>%
  unnest(SensitivityAnalysis) %>%
  distinct() %>%
  mutate(sa_method = str_replace(SensitivityAnalysis, "\\_.*", ""),
         sa_algorithm = str_replace(SensitivityAnalysis, ".*\\_", "")) %>%
  filter(SensitivityAnalysis != "No") %>%
  drop_na() %>%
  group_by(SensitivityAnalysisType, sa_method) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(
    percentage = round(n / sum(n) * 100, 0),
    label = paste0(round(n / sum(n) * 100, 0), "%")
  )

# bar-plot comparing number of papers across sensitivity analysis methods 
# and highlighting if it is a global or local method
p_sa <- ggplot(dat_sa, aes(x = reorder(sa_method, -n, sum), 
                           y = n, fill = SensitivityAnalysisType)) +
  geom_bar(color = "black", stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  ylab("No. of Papers") +
  xlab("Sensitivity Analysis Methods")

# Save plot for Figure 12 of paper
pdf(file = file.path("paper", "figures", "sa.pdf"), 
    height = 4, width = 5)
p_sa
dev.off()

# Figure 13 --------------------------------------------------------------------

# count the number of papers based on the calibration approach and the 
# analytical techniques 
dat_analytic_approach <- dat %>%
  select(No, CalibrationApproach, AnalyticalTechniques) %>%
  unnest(AnalyticalTechniques) %>% 
  distinct() %>%
  mutate(AnalyticalTechniques = toupper(AnalyticalTechniques)) %>%
  drop_na() %>%
  group_by(AnalyticalTechniques, CalibrationApproach) %>%
  summarise(n = n())

# plot comparing analytical technique for different 
# calibration approach (manual vs automated)
p_analytic_approach <- ggplot(
  dat_analytic_approach,
  aes(
    x = factor(CalibrationApproach, level = c("Manual", "Automated")),
    y = reorder(AnalyticalTechniques, n, sum), size = n
  )
) +
  geom_point(alpha = 0.8, color = "#66C2A5") +
  geom_text(aes(label = n), size = 3.5) +
  scale_size(
    name = "No. of\nPapers",
    range = c(1, 10)
  ) +
  xlab("Calibration Approach") +
  ylab("Analytical Techniques")

# count the number of papers based on the calibration approach and the 
# scale of the simulation
dat_analytic_scale <- dat %>%
  select(No, Scale, AnalyticalTechniques) %>%
  unnest(AnalyticalTechniques) %>%
  distinct() %>%
  mutate(AnalyticalTechniques = toupper(AnalyticalTechniques)) %>%
  drop_na() %>%
  group_by(AnalyticalTechniques, Scale) %>%
  summarise(n = n())

# plot comparing analytical technique for different 
# simulation scale (component/system, building, urban)
p_analytic_scale <- ggplot(
  dat_analytic_scale,
  aes(
    x = factor(Scale, level = c("Component / System", "Building", "Urban")),
    y = reorder(AnalyticalTechniques, n, sum), size = n
  )
) +
  geom_point(alpha = 0.8, color = "#66C2A5") +
  geom_text(aes(label = n), size = 3.5) +
  scale_size(
    name = "No. of\nPapers",
    range = c(1, 7.5)
  ) +
  xlab("Spatial Scale") +
  ylab("Analytical Techniques")

# Save plot for Figure 13 of paper
pdf(file = file.path("paper", "figures", "approach_scale.pdf"), 
    height = 5.5, width = 8)
grid.arrange(p_analytic_scale + theme(legend.position = "none"),
  p_analytic_approach,
  widths = c(2.8, 2.7),
  nrow = 1
)
dev.off()
