# Load necessary packages
library(tidyverse)

# Sample data
data <- data.frame(
  study = paste("Study", 1:10),
  estimate1 = rnorm(10, 1, 0.5),
  lower1 = rnorm(10, 0.5, 0.2),
  upper1 = rnorm(10, 1.5, 0.2),
  estimate2 = rnorm(10, 0.8, 0.5),
  lower2 = rnorm(10, 0.3, 0.2),
  upper2 = rnorm(10, 1.3, 0.2)
)

# Prepare data for plotting
plot_data <- data %>%
  pivot_longer(cols = starts_with("estimate"),
               names_to = "group",
               values_to = "estimate") %>%
  mutate(lower = ifelse(group == "estimate1", lower1, lower2),
         upper = ifelse(group == "estimate1", upper1, upper2),
         group = recode(group, "estimate1" = "Group 1", "estimate2" = "Group 2"))

# Plot the coupled forest plot
ggplot(plot_data, aes(x = estimate, y = fct_rev(study), color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.5), height = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Estimate", y = "Study", color = "Group") +
  theme_minimal() +
  theme(legend.position = "top")

# Load necessary packages
library(ggplot2)
library(dplyr)
library(forcats) # For ordering factors

# Sample data
data <- data.frame(
  study = paste("Study", 1:10),
  estimate1 = rnorm(10, 1, 0.5),
  lower1 = rnorm(10, 0.5, 0.2),
  upper1 = rnorm(10, 1.5, 0.2),
  estimate2 = rnorm(10, 0.8, 0.5),
  lower2 = rnorm(10, 0.3, 0.2),
  upper2 = rnorm(10, 1.3, 0.2)
)

# Prepare data for plotting
plot_data <- data %>%
  pivot_longer(cols = starts_with("estimate"),
               names_to = "group",
               values_to = "estimate") %>%
  mutate(lower = ifelse(group == "estimate1", lower1, lower2),
         upper = ifelse(group == "estimate1", upper1, upper2),
         group = recode(group, "estimate1" = "Group 1", "estimate2" = "Group 2"))

# Plot with separate columns for each group
ggplot(plot_data, aes(x = estimate, y = fct_rev(study))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "blue") +
  facet_grid(. ~ group, scales = "free_x") +
  labs(x = "Estimate", y = "Study") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))

