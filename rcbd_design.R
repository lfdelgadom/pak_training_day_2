# Clean the environment
rm(list = ls())

# Load required libraries
library(pacman)
pacman::p_load(tidyverse, desplot, plotly, broom, emmeans, multcomp, multcompView)

# Read data from CSV file
data_rcbd <- read_csv("data/RCBD.csv")

# Check the first few rows and structure of the data
head(data_rcbd)
glimpse(data_rcbd)

# Convert 'variety' and 'block' columns to factors
data_rcbd$variety <- as.factor(data_rcbd$variety)
data_rcbd$block <- as.factor(data_rcbd$block)

# Create a frequency table of variety and block combinations
table(data_rcbd$block, data_rcbd$variety)

# Field layout plot using desplot
desplot(
  data = data_rcbd,
  flip = FALSE,
  form = variety ~ col + row,  # Coloring by genotype
  out1 = block,
  text = variety, cex = 1, shorten = "no",  # Displaying genotype names
  main = "Field layout", show.key = TRUE
)

# Field layout plot using ggplot2
g1 <- data_rcbd %>%
  ggplot(aes(x = col, y = row, fill = variety)) +
  geom_tile(color = "black") +
  geom_text(aes(label = variety)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ block, scales = "free_x")

# Display ggplot2 field layout
g1

# Convert ggplot2 plot to interactive plotly chart
ggplotly(g1)

# -------------------------------------------------------------------------
# Exploratory Data Analysis
# -------------------------------------------------------------------------

# Summarize yield by variety
data_rcbd %>%
  group_by(variety) %>%
  summarize(mean = mean(yield),
            std.dev = sd(yield),
            cv = std.dev / mean)

# Summarize yield by block
data_rcbd %>%
  group_by(block) %>%
  summarize(mean = mean(yield),
            std.dev = sd(yield),
            cv = std.dev / mean)

# Summarize yield by block1
data_rcbd %>% 
  filter(block == "B1") %>% 
  summarize(mean = mean(yield),
            std.dev = sd(yield),
            cv = std.dev / mean)

# Scatter plot of yield vs. variety
g2 <- data_rcbd %>%
  ggplot(aes(x = variety, y = yield)) +
  geom_point() +
  theme_bw()

# Display scatter plot
g2

# Scatter plot of yield vs. variety with color-coding for blocks
g3 <- data_rcbd %>%
  ggplot(aes(x = variety, y = yield, color = block, group = block)) +
  geom_point(size = 4) +
  theme_bw()

# Display scatter plot with color-coding
g3

# Boxplots of yield distribution by variety
g4 <- data_rcbd %>%
  ggplot(aes(x = variety, y = yield, fill = variety)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  labs(title = "Yield distribution by variety")

# Display boxplot for variety
g4

# Boxplots of yield distribution by block
g5 <- data_rcbd %>%
  ggplot(aes(x = block, y = yield, fill = block)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  labs(title = "Yield distribution by block")

# Display boxplot for block
g5

# Interactive scatter plot with color-coding for blocks
ggplotly(g3)

# -------------------------------------------------------------------------
# Modeling
# -------------------------------------------------------------------------

# Linear model without blocking
model_1 <- lm(formula = yield ~ variety, data = data_rcbd)
ao_1 <- anova(model_1)
print(ao_1)  # Display ANOVA table
gf_1 <- glance(model_1)
print(gf_1)  # Display model summary

# Mean comparisons using Tukey's test for the non-blocked model
mean_comparisons_1 <- model_1 %>%
  emmeans(pairwise ~ "variety", adjust = "tukey") %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters)

adjusted_means_1 <- mean_comparisons_1$emmeans
print(adjusted_means_1)  # Display adjusted variety means

# Linear model with blocking
model_2 <- lm(formula = yield ~ variety + block, data = data_rcbd)
ao_2 <- anova(model_2)
print(ao_2)  # Display ANOVA table
gf_2 <- glance(model_2)
print(gf_2)  # Display model summary

# Mean comparisons using Tukey's test for the blocked model
mean_comparisons_2 <- model_2 %>%
  emmeans(pairwise ~ "variety", adjust = "tukey") %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters)

adjusted_means_2 <- mean_comparisons_2$emmeans
print(adjusted_means_2)  # Display adjusted variety means

# -------------------------------------------------------------------------
# Comparing Models
# -------------------------------------------------------------------------

# ANOVA comparison between non-blocked and blocked models
anova(model_1, model_2)
# Conclusion: Adding 'block' to the model significantly improves the fit

# -------------------------------------------------------------------------
# Plots
# -------------------------------------------------------------------------

# Plot with raw data, adjusted means, error bars, and letters
plot_with_adjusted_means <- ggplot() +
  geom_point(data = data_rcbd, aes(y = yield, x = variety, color = block)) +
  geom_point(data = adjusted_means_2, aes(y = emmean, x = variety), color = "red", position = position_nudge(x = 0.1)) +
  geom_errorbar(data = adjusted_means_2, aes(ymin = lower.CL, ymax = upper.CL, x = variety), color = "red", width = 0.1, position = position_nudge(x = 0.1)) +
  geom_text(data = adjusted_means_2, aes(y = emmean, x = variety, label = .group), color = "red", position = position_nudge(x = 0.2)) +
  ylim(0, NA) + ylab("Yield in t/ha") + xlab("Variety") +
  labs(
    caption = "Black dots: Raw data. Red: Adjusted mean and 95% confidence limits. Common letter: Not significantly different (Tukey-test)"
  ) +
  theme_bw()

# -------------------------------------------------------------------------
# LSD Test
# -------------------------------------------------------------------------

# Perform LSD test for variety in the blocked model
test <- agricolae::LSD.test(model_2, "variety", alpha = 0.05)
print(test$statistics)  # Display LSD test statistics

# -------------------------------------------------------------------------
# Residual Analysis
# -------------------------------------------------------------------------

# Analyze residuals of the blocked model
residual_analysis <- augment(model_2)
residual_analysis %>%
  ggplot(aes(x = .fitted, y = .resid, color = variety)) +
  geom_point(size = 3) +
  theme_bw()
