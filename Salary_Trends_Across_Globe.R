# IST 719 Final Project - Data Science Salary Analysis
# Author: Mugdha Karodkar
# Date: December 2024


# Sources
# Data source: https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries
# R packages: ggplot2, scales, viridis, gridExtra, ggridges, patchwork
# Color palettes inspired by professional themes

# Poster Story: 
# The data exploration found that factors such as experience level, company size, and remote work arrangements have a significant impact 
# on data science salaries worldwide. Senior-level roles in large US companies tend to command the highest salaries, while entry-level 
# positions in smaller companies across various regions fall on the lower end of the pay scale. The analysis also reveals interesting 
# insights into salary trends over time and differences in compensation across job categories and geographical markets.

# Motivation:
# This analysis would be of interest to data science professionals, students, and industry stakeholders seeking to understand the factors
# influencing compensation in the field. By examining salary data acrsoss various dimensions such as experience level, company size, and 
# location, this study offers valuable insights for career planning, benchmarking, and talent acquisition strategies in the data science 
# domain. The findings can help individuals make informed decisions about their career paths and enable organizations to develop 
# competitive compensation packages to attract and retain top talent.

# Question: 
# How does the interaction between experience level and geographical region influence data science salaries, and which factors contribute 
# most to the observed differences in compensation across markets?

# Loading required libraries
library(tidyverse)
library(scales)
library(viridis)
library(gridExtra)
library(ggridges)
library(patchwork)

# Defining color palettes
# Professional color palettes for poster presentation
# Main palette - centered around professional blues and grays with subtle variations
palette_professional <- c(
  "United States" = "#2C3E50",    # Dark Blue Gray (Primary)
  "Europe" = "#34495E",           # Darker Blue Gray
  "Canada" = "#446CB3",           # Muted Blue
  "India" = "#5C97BF",            # Light Blue Gray
  "East/SE Asia" = "#6A89A7",     # Soft Blue
  "Australia/NZ" = "#89A7BE",     # Pale Blue
  "Other Regions" = "#95A5A6"     # Cool Gray
)

# Experience level palette (monochromatic blue scale)
palette_experience <- c(
  "Entry" = "#BDC3C7",       # Light Gray
  "Mid" = "#7F8C8D",         # Medium Gray
  "Senior" = "#34495E",      # Blue Gray
  "Executive" = "#2C3E50"    # Dark Blue Gray
)

# Company size palette (shades of blue)
palette_company <- c(
  "Small" = "#A8D0E6",  # Light Blue
  "Medium" = "#374E8C", # Medium Blue
  "Large" = "#24305E"   # Dark Blue
)

# Remote work palette (blue grays)
palette_remote <- c(
  "On-site" = "#95A5A6",     # Light Blue Gray
  "Hybrid" = "#34495E",      # Medium Blue Gray
  "Full Remote" = "#2C3E50"  # Dark Blue Gray
)

# Defining consistent theme for all visualizations 
theme_poster <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    strip.text = element_text(size = 12, face = "bold")
  )

# Set working directory
setwd("/Users/mugdhakarodkar/Library/CloudStorage/OneDrive-SyracuseUniversity/MS-SEM3/IST719/Final_Project")

# Data Import and Cleaning
ds_salaries <- read.csv("ds_salaries.csv", stringsAsFactors = FALSE)


clean_salaries <- ds_salaries %>%
  mutate(
    experience_level = factor(case_when(
      experience_level == "EN" ~ "Entry",
      experience_level == "MI" ~ "Mid", 
      experience_level == "SE" ~ "Senior",
      experience_level == "EX" ~ "Executive"
    ), levels = c("Entry", "Mid", "Senior", "Executive")),
    
    company_size = factor(case_when(
      company_size == "S" ~ "Small",
      company_size == "M" ~ "Medium",
      company_size == "L" ~ "Large"  
    ), levels = c("Small", "Medium", "Large")),
    
    remote_ratio = factor(case_when(
      remote_ratio == 0 ~ "On-site",
      remote_ratio == 50 ~ "Hybrid",
      remote_ratio == 100 ~ "Full Remote"
    ), levels = c("On-site", "Hybrid", "Full Remote")),
    
    market = case_when(
      company_location == "US" ~ "United States",
      company_location %in% c("GB", "DE", "FR", "ES", "IT", "NL", "GR") ~ "Europe",
      company_location == "CA" ~ "Canada", 
      company_location == "IN" ~ "India",
      company_location %in% c("JP", "SG", "CN", "KR") ~ "East/SE Asia",
      company_location %in% c("AU", "NZ") ~ "Australia/NZ", 
      TRUE ~ "Other Regions"
    ),
    
    salary_k = salary_in_usd/1000,
    
    job_category = case_when(
      grepl("Manager|Director|Head|Lead|Principal", job_title) ~ "Leadership",
      grepl("Machine Learning|ML|AI|Computer Vision|NLP", job_title) ~ "ML/AI Specialist",
      grepl("Data Scientist|Research Scientist", job_title) ~ "Data Scientist", 
      grepl("Data Engineer|Big Data|Cloud Data|ETL", job_title) ~ "Data Engineer",
      grepl("Data Analyst|BI|Business Intelligence|Analytics", job_title) ~ "Data Analyst",
      grepl("Architect", job_title) ~ "Data Architect",
      TRUE ~ "Other Specialists"  
    )
  )

# Data Description 
# The dataset was sourced from Kaggle() and contains salary survey data of data science 
# professionals from 2020 to 2022. It includes 607 rows and 12 columns covering features 
# such as job title, salary, experience level, company size, and work location. The data 
# was collected through a survey of data science practitioners worldwide and provides 
# insights into global salary trends in the field.

# Creating Diversified Visualizations
# A. MAIN VISUALIZATION: Global Data Science Field Salary Distribution by Experience and Region
# This plot answers the question listed above
# Enhanced Violin Plot with Optimized Text Visibility
#violin plot

# Create annotations dataset for median salary and employee counts
annotations <- clean_salaries %>%
  group_by(market, experience_level) %>%
  summarise(
    median_salary = median(salary_k),
    n_employees = n(),
    .groups = "drop"
  )
main_visualization<- ggplot(clean_salaries, aes(x = market, y = salary_k, fill = market)) +
  # Violin plot for salary distribution
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") +
  # Jitter for individual data points
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  # Add text annotations for medians and counts
  geom_text(
    data = annotations,
    aes(
      x = market,
      y = median_salary,
      label = paste0("$", round(median_salary, 0), "K\nn=", n_employees)
    ),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.9),
    size = 3.5, # Increase text size for better readability
    vjust = -1.5, # Adjust vertical placement
    hjust = 0.5, # Center horizontally
    lineheight = 1.2 # Add space between lines in annotations
  ) +
  # Facet layout by experience level
  facet_wrap(~ experience_level, scales = "free_y") +
  # Apply color palette
  scale_fill_manual(values = palette_professional) +
  # Labels and titles
  labs(
    title = "Global Data Science Salary Distribution by Experience Level",
    subtitle = "Violin plot with median annotations across regions",
    x = "Market",
    y = "Annual Salary (in $K)",
    caption = "Data source: Kaggle DS Salaries Dataset"
  ) +
  # Apply professional theme
  theme_poster +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none"
  )

# B. SALARY TRENDS VISUALIZATION
# This plot answers the question listed above
salary_trends <- clean_salaries %>%
  group_by(work_year, market) %>%
  summarise(
    median_salary = median(salary_k),
    count = n(),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = factor(work_year), y = median_salary, 
             group = market, color = market)) +
  geom_line(size = 1.2) +
  geom_point(size = 4, aes(shape = market)) +
  geom_text(aes(label = paste0("$", round(median_salary, 0), "K")),
            vjust = -1.2,  # Increase space above points
            size = 2.8,    # Adjust text size
            color = "black") +  # Change text color to black
  scale_y_continuous(
    labels = scales::dollar_format(suffix = "K"),
    breaks = seq(0, 250, 50),  # Adjust breaks for clarity
    limits = c(0, 250)  # Extend limits to fit text
  ) +
  scale_color_manual(values = palette_professional) +
  scale_shape_manual(values = c(16, 17, 18, 19, 15, 8, 12)) +
  labs(
    title = "Data Science Salary Trends Over Time",
    subtitle = "Median annual salary trends by region (2020-2022)",
    x = "Year",
    y = "Median Annual Salary (USD)",
    color = "Region",
    shape = "Region"
  ) +
  theme_poster +
  theme(
    legend.position = "right"
  )

# C. SUPPORTING VISUALIZATION 1: Using a heatmap for remote work analysis  
remote_analysis <- clean_salaries %>%
  group_by(remote_ratio, market) %>%
  summarise(
    median_salary = median(salary_k),
    mean_salary = mean(salary_k), 
    min_salary = min(salary_k),
    max_salary = max(salary_k),
    n_positions = n(),
    .groups = 'drop'
  ) %>%
  ggplot() +
  geom_tile(aes(x = remote_ratio, y = market, fill = median_salary),
            color = "white", size = 0.5) +
  geom_text(aes(x = remote_ratio, y = market,
                label = paste0("$", round(median_salary), "K\n",
                               "Range: $", round(min_salary), "K - $", round(max_salary), "K\n", 
                               "n=", n_positions)),
            color = "white",
            size = 3,
            fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#BDC3C7", "#34495E", "#2C3E50"),
    labels = scales::dollar_format(suffix = "K"),
    name = "Median Salary"
  ) +
  labs(
    title = "Remote Work Impact on Data Science Salaries",
    subtitle = "Showing median salary, salary range, and position count by region and work arrangement",
    x = "Work Arrangement", 
    y = "Region",
    caption = "Salary ranges show minimum to maximum values"
  ) +
  theme_poster +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# D. SUPPORTING VISUALIZATION 2: 
# Job Category Analysis: Dot Strip Chart with Error Bars
job_category_analysis <- clean_salaries %>%
  group_by(job_category, market) %>%
  summarise(
    median_salary = median(salary_k),
    lower = quantile(salary_k, 0.25),
    upper = quantile(salary_k, 0.75),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = median_salary, y = reorder(job_category, median_salary), color = market)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, alpha = 0.6, size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ market, scales = "free", ncol = 2) +
  scale_color_manual(values = palette_professional) +
  labs(
    title = "Data Science Job Categories by Region",
    subtitle = "Median salaries with interquartile range for each job category",
    x = "Median Salary (in $K)",
    y = "Job Category",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )


# E. DESCRIPTIVE VISUALIZATION 1: Experience Level Distribution by Market
experience_distribution <- clean_salaries %>%
  ggplot(aes(x = market, fill = experience_level)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = palette_experience) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Experience Level Distribution by Region",
    subtitle = "Proportion of experience levels across markets", 
    x = "Region",
    y = "Percentage"
  ) +
  theme_poster +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# F: DESCRIPTIVE VISUALIZATION 2: Company Size Impact - Cleveland Dot Plot with Consistent Colors

# Lollipop Plot with Annotations for Salary Distribution by Company Size

company_size_analysis <- clean_salaries %>%
  group_by(market, company_size) %>%
  summarise(
    median_salary = median(salary_k),
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(market = factor(market,
                         levels = names(sort(tapply(median_salary, market, median),
                                             decreasing = TRUE)))) %>%
  ggplot(aes(x = company_size, y = median_salary)) +
  # Add segments to represent the "lollipop sticks"
  geom_segment(aes(x = company_size, xend = company_size, 
                   y = 0, yend = median_salary, color = company_size),
               size = 1.2) +
  # Add points for "lollipop heads"
  geom_point(aes(size = count, color = company_size), alpha = 0.9) +
  # Add text annotations for median salary and count with dynamic offsets
  geom_text(aes(
    label = paste0("$", round(median_salary, 0), "K\nn=", count),
    y = ifelse(median_salary < 50, median_salary + 10, median_salary + 20) # Adjust text based on median_salary value
  ),
  size = 2.8, vjust = 0, lineheight = 1.2) +
  # Facet by region
  facet_wrap(~ market, scales = "free_y", ncol = 3) +
  # Use your defined company size palette
  scale_color_manual(values = c("#A8D0E6", "#374E8C", "#24305E"), name = "Company Size") +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "K"), expand = expansion(mult = c(0, 0.3))) + # Add extra space above
  # Labels and titles
  labs(
    title = "Salary Distribution by Company Size and Region",
    subtitle = "Lollipop plot showing median salaries with annotations for counts",
    x = "Company Size",
    y = "Median Annual Salary (USD)",
    caption = "Numbers indicate median salary and count of positions"
  ) +
  # Apply professional theme
  theme_poster +
  theme(
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(margin = margin(b = 15)), # Move facet titles up
    legend.position = "top",
    panel.spacing = unit(1.5, "lines")
  )


# Display sequence
#answering questions
main_visualization

#supportive
salary_trends # also answer key question
remote_analysis
job_category_analysis  

#descriptive
experience_distribution
company_size_analysis

# 1. Main visualization: Global Salary Distribution by Experience and Region (Answers the key question)
# 2. Supporting visualizations:
#    - Salary Trends Over Time (Provides additional context) (Answers the key question)
#    - Remote Work Impact on Data Science Salaries (Adds depth to the story)
#    - Data Science Job Categories: Salary and Demand by Region (Offers more insights)
# 3. Descriptive visualizations:
#    - Experience Level Distribution by Region (Shows data distributions)
#    - Salary Distribution by Company Size and Region (Reveals additional patterns)

#Violin Plot: "Salary Distribution by Experience and Region"
#Line Chart: "Trends in Median Salaries Over Time"
#Heatmap: "Impact of Remote Work on Salaries Across Regions"
#Dot Strip Chart: "Salary Ranges for Job Categories Across Regions"
#Bar Chart: "Proportion of Experience Levels by Region"
#Lollipop Chart: "Effect of Company Size on Salaries by Region"



