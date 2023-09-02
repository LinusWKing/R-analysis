library(dplyr)
library(ggplot2)
library(reshape2)

dataset <- read.csv("shamiri_imputed_dataset.csv") # load data

summary(dataset) # prelim stats
head(dataset)

# Values in PHQ3 & MSSS10 exceeded possible values
data <- dataset[dataset$PHQ3 <= 3 & dataset$MSSS10 <= 7, ]

summary(data)

# PHQ mean per item rounded to 3 decimal places

phq_mean_per_item <- data %>%
    select(starts_with("PHQ")) %>%
    summarise_all(list(mean))

print(round(phq_mean_per_item, digits = 3))

# PHQ mean per item rounded to 3 decimal places
gad_mean_per_item <- data %>%
    select(starts_with("GAD")) %>%
    summarise_all(list(mean))

print(round(gad_mean_per_item, digits = 3))

# PHQ1 mean per item per gender

phq1_mean_per_gender <- data %>%
    group_by(Gender) %>%
    summarise(PHQ1_mean = (mean(PHQ1)))

print(phq1_mean_per_gender)

# PHQ1 mean per item per tribe

phq1_mean_per_tribe <- data %>%
    group_by(Tribe) %>%
    summarise(PHQ1_mean = (mean(PHQ1)))

print(phq1_mean_per_tribe)

# PHQ1 mean per item per age

phq1_mean_per_age <- data %>%
    group_by(Age) %>%
    summarise(PHQ1_mean = (mean(PHQ1)))

print(phq1_mean_per_age)

# PHQ1 mean per item per school

phq1_mean_per_school <- data %>%
    group_by(School) %>%
    summarise(PHQ1_mean = (mean(PHQ1)))

print(phq1_mean_per_school)

# PHQ1 mean by category

phq1_mean_by_category <- data %>%
    group_by(Gender, Tribe, Age, School) %>%
    summarise(PHQ1_mean = mean(PHQ1))

print(mean_by_category)

# Adding an age category
median_age <- median(data$Age)
print(median_age)
# Create a new column 'age_category'
data$age_category <- ifelse(data$Age <= median_age, "Younger", "Older")

count(data, age_category)

# Reshape (unpivot) data for eaasier plotting
reshaped_phq_data <- melt(data,
    measure.vars = c(
        "PHQ1", "PHQ2", "PHQ3", "PHQ4",
        "PHQ5", "PHQ6", "PHQ7", "PHQ8"
    )
)

reshaped_gad_data <- melt(data,
    measure.vars = c(
        "GAD1", "GAD2", "GAD3", "GAD4",
        "GAD5", "GAD6", "GAD7"
    )
)

reshaped_msss_data <- melt(data,
    measure.vars = c(
        "MSSS1", "MSSS2", "MSSS3", "MSSS4",
        "MSSS5", "MSSS6", "MSSS7", "MSSS8",
        "MSSS9", "MSSS10", "MSSS11", "MSSS12"
    )
)

# Create histograms for each PHQ item using facets
ggplot(reshaped_phq_data, aes(y = value, fill = factor(value))) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(
        title = "Distribution of PHQ-8 Responses",
        x = "Frequency", y = "PHQ-8 Score"
    ) +
    facet_wrap(~variable, ncol = 1) +
    scale_fill_discrete(name = "Response Value")


# Create histograms for each GAD item
ggplot(reshaped_gad_data, aes(y = value, fill = factor(value))) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(
        title = "Distribution of GAD-7 Responses",
        x = "Frequency", y = "GAD-7 Score"
    ) +
    facet_wrap(~variable, ncol = 1) +
    scale_fill_discrete(name = "Response Value")

# Create histograms for each MSSS item
ggplot(reshaped_msss_data, aes(y = value, fill = factor(value))) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(
        title = "Distribution of MSSS-12 Responses",
        x = "Frequency", y = "MSSS-12 Score"
    ) +
    facet_wrap(~variable, ncol = 1) +
    scale_fill_discrete(name = "Response Value")+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Descriptive Statistics for PHQ-8 , GAD-7 , and MSPSS subscales scores

# Calculate respective scores
data_score <- data %>%
    mutate(
        phq_score = rowSums(select(., starts_with("PHQ"))),
        gad_score = rowSums(select(., starts_with("GAD"))),
        msss_score = rowMeans(select(., starts_with("MSSS"))),
        msss_friend_score = rowMeans(select(., c("MSSS6", "MSSS7", "MSSS9", "MSSS12"))),
        msss_sp_score = rowMeans(select(., c("MSSS1", "MSSS2", "MSSS5", "MSSS10"))),
        msss_family_score = rowMeans(select(., c("MSSS3", "MSSS4", "MSSS8", "MSSS11")))
    )

summary(data_score)

# Mean and S.D for PHQ-8
print(mean(data_score$phq_score))
print(sd(data_score$phq_score))

# Mean and S.D for GAD-7
print(mean(data_score$gad_score))
print(sd(data_score$gad_score))

# Mean and S.D for PHQ-8 and GAD-7 per gender
gender_desc_stats <- data_score %>%
    group_by(Gender) %>%
    summarise(
        PHQ_mean = mean(phq_score),
        PHQ_sd = sd(phq_score),
        GAD_mean = mean(gad_score),
        GAD_sd = sd(gad_score),
    )

print(gender_desc_stats)

age_desc_stats <- data_score %>%
    group_by(age_category) %>%
    summarise(
        PHQ_mean = mean(phq_score),
        PHQ_sd = sd(phq_score),
        GAD_mean = mean(gad_score),
        GAD_sd = sd(gad_score),
    )

print(age_desc_stats)

tribe_desc_stats <- data_score %>%
    group_by(Tribe) %>%
    summarise(
        PHQ_mean = mean(phq_score),
        PHQ_sd = sd(phq_score),
        GAD_mean = mean(gad_score),
        GAD_sd = sd(gad_score),
    )

print(tribe_desc_stats)

tribe_gender_desc_stats <- data_score %>%
    group_by(Tribe, Gender) %>%
    summarise(
        PHQ_mean = mean(phq_score),
        PHQ_sd = sd(phq_score),
        GAD_mean = mean(gad_score),
        GAD_sd = sd(gad_score),
    )

print(tribe_gender_desc_stats)


tribe_gender_age_desc_stats <- data_score %>%
    group_by(Tribe, Gender, age_category) %>%
    summarise(
        PHQ_mean = mean(phq_score),
        PHQ_sd = sd(phq_score),
        GAD_mean = mean(gad_score),
        GAD_sd = sd(gad_score),
    )

print(tribe_gender_age_desc_stats)


# Create a bar plot for depression levels
ggplot(data_score, aes(x = cut(phq_score, breaks = c(-Inf, 5, 10, 15, 20, Inf), labels = c("Minimal", "Mild", "Moderate", "Moderately Severe", "Severe")), fill = ..x..)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Frequency of Depression Levels", x = "Depression Level", y = "Percentage") +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "none")


# Create a bar plot for anxiety levels
ggplot(data_score, aes(x = cut(gad_score, breaks = c(-Inf, 5, 10, 15, Inf), labels = c("Minimal", "Mild", "Moderate", "Severe")), fill = ..x..)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    labs(title = "Frequency of Anxiety Levels", x = "Anxiety Level", y = "Percentage") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none")




# Box-plot for PHQ Score and age category
ggplot(data_score, aes(x = age_category, y = phq_score, fill = age_category)) +
  geom_boxplot() +
  labs(title = "PHQ Scores by Age Category", x = "Age Category", y = "PHQ Score")

# Box-plot for GAD Score and age category
ggplot(data_score, aes(x = age_category, y = gad_score, fill = age_category)) +
  geom_boxplot() +
  labs(title = "GAD Scores by Age Category", x = "Age Category", y = "GAD Score") 

# Scatter Plot to check for correlation between depression and anxiety
ggplot(data = data_score, aes(x = phq_score, y = gad_score)) +
    geom_point(alpha = 0.4) +
    labs(title = "Scatter Plot of PHQ-8 Scores vs. GAD-7 Scores", x = "PHQ-8 Score", y = "GAD-7 Score")

# Calculate Correlation using Kendall's method
correlation <- cor(data_score$gad_score, data_score$phq_score, method = "kendall")
cat("Correlation Coefficient:", correlation, "\n")

# Scatter Plot to check for correlation between anxiety and perceived support
ggplot(data = data_score, aes(x = gad_score, y = msss_score)) +
    geom_point(alpha = 0.4) +
    labs(title = "Scatter Plot of GAD-8 Scores vs. MSSS Scores", x = "GAD-7 Score", y = "MSSS Score")

corr <- cor(data_score$gad_score, data_score$msss_score, method="spearman")
cat("Correlation Coefficient:", corr, "\n")

# Scatter plot for MSSS Family Score vs. MSSS Score
ggplot(data_score, aes(x = msss_score, y = msss_family_score)) +
  geom_point() +
  labs(title = "Scatter Plot: MSSS Family Score vs. MSSS Score", x = "MSSS Score", y = "MSSS Family Score") 

# Scatter plot for MSSS SP Score vs. MSSS Score
ggplot(data_score, aes(x = msss_score, y = msss_sp_score)) +
  geom_point() +
  labs(title = "Scatter Plot: MSSS SP Score vs. MSSS Score", x = "MSSS Score", y = "MSSS SP Score")

# Scatter plot for MSSS Friend Score vs. MSSS Score
ggplot(data_score, aes(x = msss_score, y = msss_friend_score)) +
  geom_point() +
  labs(title = "Scatter Plot: MSSS Friend Score vs. MSSS Score", x = "MSSS Score", y = "MSSS Friend Score")





