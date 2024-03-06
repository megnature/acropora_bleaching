## Start with package downloads and installs needed
library(readxl)

install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(car)

########################################## Data Download and Prep #####################################################

#Data download
Acropora <- read_excel("Acropora.xlsx")
View(Acropora)  

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

####### Data Prep for average abundance data #######

#### Group by depth range and find the average depth for reach transect
transect_df <- Acropora %>%
  group_by(Transect, depth_range) %>%
  summarize(count = n())

## calulating the average abundance for each depth range 
summary_df <- transect_df %>%
  group_by(depth_range) %>%
  summarize(avg_abundance = mean(count),
            sd_abundance = sd(count))

########## Data Prep for depth range vairation at each site 
## Site data frame
site_df <- Acropora %>%
  group_by(depth_range, Bleaching_level, Site) %>%
  summarize(count = n())

site_tbl <- as_tibble(site_df)

# Calculate summary statistics for each depth range at each site
summary_stats <- site_tbl %>%
  group_by(Site, depth_range) %>%
  summarize(
    avg_abundance = mean(count),
    sd_abundance = sd(count),
    n = n()
  )


############################################# Graphing the results #########################################################

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

### Graphing the average abundance per depth range 
desired_order <- c("Shallow", "Middle", "Deep")
summary_df$depth_range <- factor(summary_df$depth_range, levels = desired_order)

ggplot(summary_df, aes(x = depth_range, y = avg_abundance, fill = depth_range)) +
  geom_bar(stat = "identity", position = "stack", color ="black") +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y.left = element_text(colour = "black")) +
  geom_errorbar(aes(ymin = avg_abundance - sd_abundance, ymax = avg_abundance + sd_abundance),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  labs(title = " ",
       x = "Depth Range",
       y = "Average Abundance (±SD)") +
  scale_fill_manual(values = c("Shallow" = "lightcyan", "Middle" = "cyan3", "Deep" = "darkcyan"), name ="Depth Range") +
  theme_dark()

### Graphing site variation 

# Create a bar plot with error bars for site comparison
ggplot(summary_stats, aes(x = depth_range, y = avg_abundance, fill = factor(Site))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y.left = element_text(colour = "black")) +
  geom_errorbar(aes(ymin = avg_abundance - sd_abundance, ymax = avg_abundance + sd_abundance),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  labs(title = " ",
       x = "Depth Range",
       y = "Average Abundance (±SD)") +
  scale_fill_manual(values = c("1" = "pink", "2" = "lightgreen"), name = "Site", labels = c("1" = "South", "2" = "North")) +
  theme_dark()+
  coord_cartesian(ylim = c(-20, max(summary_stats$avg_abundance + summary_stats$sd_abundance)))




################################## Statistical Anlysis ##############################################

##Chi_Square Test
contingency_table <- xtabs(count ~ depth_range + Bleaching_level, data = result_df)
chi_square_result <- chisq.test(contingency_table)

print(chi_square_result)

##### Abundance statistical significance with Kruskal-Wallis test

kruskal.test(avg_abundance~ depth_range, data = summary_df)

##### Site significane t-test 

# Perform a t-test comparing coral abundance between sites
t_test_result <- t.test(count ~ Site, data = site_tbl)



