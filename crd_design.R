# Clean the environment
rm(list = ls())

# Load required libraries using pacman for easy loading
# install.packages("pacman") # in case you do not have the library, please install it.
library(pacman)
pacman::p_load(tidyverse, desplot, plotly, broom, emmeans, multcomp, multcompView, 
               agricolae)

# Read the data from a CSV file
data <- read_csv("data/CRD.csv")
head(data)
glimpse(data)

# Convert 'variety' column to a factor
data$variety <- as.factor(data$variety)

# Create a desplot graph to visualize field layout
desplot(data = data, flip = FALSE,
        form = variety ~ col + row,             # fill color per genotype
        text = variety, cex = 1, shorten = "no", # show genotype names per plot
        main = "Field layout", show.key = FALSE)

# Create a ggplot2 graph for data visualization
g1 <- data %>% 
  ggplot(aes(x = col, y = row, fill = variety)) +
  geom_tile(color = "black") +
  geom_text(aes(label = variety)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
g1

ggsave(paste0("images\\g1", Sys.Date(), ".png"),
       plot = g1, units = "in", dpi = 300, width = 8, height = 6
)

# Convert ggplot2 graph to interactive plot using plotly
ggplotly(g1)

# -------------------------------------------------------------------------
# Response Trait Checking
# -------------------------------------------------------------------------

# Summary statistics by variety
data %>% 
  group_by(variety) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield),
            cv      = std.dev/mean)

# Scatter plot for yield by variety
g2 <- data %>% 
  ggplot(aes(x = variety, y = yield)) +
  geom_point() +
  theme_bw()
g2

# Boxplot for yield distribution by variety
g3 <- data %>% 
  ggplot(aes(x = variety, y = yield, fill = variety)) +
  geom_boxplot() +
  geom_point() +
  labs(y = "Yield (t/ha)", x = "Variety", fill = "Variety") +
  theme_bw() 
 g3

 ggsave(paste0("images\\g3", Sys.Date(), ".png"),
        plot = g3, units = "in", dpi = 300, width = 8, height = 6
 ) 
 
# Convert boxplot to interactive plot
ggplotly(g3)

# -------------------------------------------------------------------------
# Modelling
# -------------------------------------------------------------------------

# Linear model for yield by variety
model <- lm(formula = yield ~ variety, data = data)
model_agricolae <- aov(yield ~ variety, data = data)

# Summary and post-hoc test (LSD)
summary(model_agricolae)
lsd_test <- HSD.test(model_agricolae, "variety")
lsd_test$groups

# ANOVA for model
anova(model)

# Goodness of fit for model
glance(model)

# Mean comparisons with Tukey adjustment
mean_comparisons <- model %>% 
  emmeans(pairwise ~ "variety", adjust="tukey") %>% 
  pluck("emmeans") %>% 
  cld(details=TRUE, Letters=letters)

# Display adjusted variety means
mean_comparisons$emmeans

# -------------------------------------------------------------------------
# Plotting Results
# -------------------------------------------------------------------------

# Plot for visualizing raw data, adjusted means, and confidence intervals
tukey_plot <- ggplot() +
  geom_point(data = data, aes(y = yield, x = variety)) +
  geom_point(data = mean_comparisons$emmeans, aes(y = emmean, x = variety), color = "red", position = position_nudge(x = 0.1)) +
  geom_errorbar(data = mean_comparisons$emmeans, aes(ymin = lower.CL, ymax = upper.CL, x = variety), color = "red", width = 0.1, position = position_nudge(x = 0.1)) +
  geom_text(data = mean_comparisons$emmeans, aes(y = emmean, x = variety, label = .group), color = "red", position = position_nudge(x = 0.2)) + 
  ylim(0, NA) + ylab("Yield (t/ha)") + xlab("Variety") +
  labs(caption = "Black dots: Raw data. Red: Adjusted mean and 95% confidence limits. Common letter: Not significantly different (Tukey-test)") +
  theme_bw()

tukey_plot

ggsave(paste0("images\\tukey", Sys.Date(), ".png"),
    plot = tukey_plot, units = "in", dpi = 300, width = 8, height = 6
  )


# Plot for means
tukey_plot_mean <- ggplot() +
  geom_col(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = variety, fill = variety
    ), col = "black") + 
  geom_text(data = mean_comparisons$emmeans, 
            aes(y = emmean, x = variety, label = .group), 
            position = position_nudge(y = 0.6, x = 0), size = 5) + 
  ylim(0, NA) + ylab("Yield (t/ha)") + xlab("Variety") +
  labs(caption = "Common letter: Not significantly different (Tukey-test)") +
  theme_bw()

tukey_plot_mean

ggsave(paste0("images\\tukey_mean", Sys.Date(), ".png"),
       plot = tukey_plot_mean, units = "in", dpi = 300, width = 8, height = 6
)



# -------------------------------------------------------------------------
# LSD Test
# -------------------------------------------------------------------------

# LSD test for variety
test <- agricolae::LSD.test(model, "variety", alpha = 0.05)
test$statistics

# -------------------------------------------------------------------------
# Residual Analysis
# -------------------------------------------------------------------------

# Residuals from the linear model
res <- augment(model)

# Scatter plot of residuals
res %>% 
  ggplot(aes(x = .fitted, y = .resid, color = variety)) +
  geom_point(size = 3) +
  theme_bw()

# QQ-Plot for residuals
res %>% 
  ggpubr::ggqqplot(x=".resid", ggtheme = theme_bw(), ylab = "Sample Quantile", xlab = "Theoretical Quantile") +
  labs(title = "QQ-Plot")

# Histogram of residuals
res$.resid %>% hist()

# Diagnostic plots for the linear model
plot(model)
