

library(readxl)
Acropora <- read_excel("Acropora.xlsx")
View(Acropora)  

install.packages("dplyr")
library(dplyr)

## Creating a separate data level to create depth ranges with labels 
depth_ranges <- cut(Acropora$Relative_depth, breaks = c(0,3, 4.5,Inf), labels = c("Shallow", "Middle", "Deep"))

new_df <- cbind(Acropora, depth_range = depth_ranges)

##Couting the number of times bleaching levels occur at each depth range and outputting a data frame 

result_df <- new_df %>%
  group_by(depth_range, Bleaching_level) %>%
  summarize(count = n())

new_row <- data.frame(
  depth_range = "Deep",
  Bleaching_level = 3,
  count = 0
)

result_df <- rbind(result_df, new_row)

# Display the updated result_df
print(result_df)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

## Graphing the results 

desired_order <- c("Shallow", "Middle", "Deep")
result_df$depth_range <- factor(result_df$depth_range, levels = desired_order)

ggplot(result_df, aes(x = as.factor(Bleaching_level), y = count, fill = as.factor(Bleaching_level))) +
  geom_bar(stat = "identity") +
  facet_wrap(~depth_range, scales = "free_y", nrow = 3, ncol = 1) +
  labs(title = "Bleaching Levels at Depth Ranges",
       x = "Bleaching Level",
       y = "Abundance of Bleaching") +
  scale_fill_manual(values = c("0" = "burlywood3", "1" = "burlywood2", "2" = "bisque1", "3" = "white"), name="Bleaching Level")



### TRY AGAIN:
desired_order <- c("Shallow", "Middle", "Deep")
result_df$depth_range <- factor(result_df$depth_range, levels = desired_order)

ggplot(result_df, aes(x = as.factor(Bleaching_level), y = count, fill = as.factor(Bleaching_level))) +
  geom_bar(stat = "identity", color="black") +
  theme_dark() +
  facet_wrap(~depth_range, scales = "free_y", nrow = 3, ncol = 1) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y.left = element_text(colour = "black")) +
  labs(title = " ",
       x = "Bleaching Level",
       y = "Abundance of Bleaching") +
  scale_fill_manual(values = c("0" = "burlywood3", "1" = "burlywood2", "2" = "bisque1", "3" = "white"), name="Bleaching Level")


##Counting the percentage of bleached coral per depth 

result_df2 <- result_df %>%
  group_by(depth_range) %>%
  mutate(percentage_bleached = (count / sum(count)) * 100)

##Grpahing the percentage results 

ggplot(result_df2, aes(x = depth_range, y = percentage_bleached, fill = as.factor(Bleaching_level))) +
  geom_bar(stat = "identity", position = "stack", color ="black") +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y.left = element_text(colour = "black")) +
  labs(title = " ",
       x = "Depth Range",
       y = "Percentage Bleached") +
  scale_fill_manual(values = c("0" = "burlywood3", "1" = "burlywood2", "2" = "bisque1", "3" = "white"), name="Bleaching Level") +
  ylim(0, 100) + theme_dark() 

## Statistical Anlysis 

##Chi_Square Test
contingency_table <- xtabs(count ~ depth_range + Bleaching_level, data = result_df)
chi_square_result <- chisq.test(contingency_table)

print(chi_square_result)

##Anova (Didn't work)

install.packages("car")
library(car)

str(result_df)

model <- lm(count ~ depth_range, data = result_df)
anova_result <- Anova(model, type="III")
print(anova_result)

model2 <- lm(count ~ depth_range * Bleaching_level, data = result_df)

anova_result2 <- Anova(model2, type = "III")

print(anova_result2)

