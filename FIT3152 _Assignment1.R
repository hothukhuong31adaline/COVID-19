rm(list = ls()) 
setwd("~/MONASH/SEM 01-2024/FIT3152/Assignment 1- Argentina")
set.seed(33295379) 

cvbase = read.csv("PsyCoronaBaselineExtract.csv") 
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] 
View(cvbase)

library(ggplot2)
library(gplots)
library(dplyr)
library(tidyr)
library(tibble)
  


#Question 1
#a)
glimpse(cvbase)
# Dimension of the dataset
dim(cvbase)
# Summary of the entire dataset
summary(cvbase)


# Employment Status count
employment_frequency <- data.frame(colSums(cvbase[, grepl("employstatus", names(cvbase))], na.rm = TRUE))
colnames(employment_frequency) <- ("Total number of employment")
rownames(employment_frequency) <- c("1-24 hours", "24-39 hours", "40 hours or more", "Looking for work", "Not looking for work", "Homemaker", "Retired", "Disable", "Student", "Volunteering")
print(employment_frequency)
# Convert the data frame to a tibble for ggplot
employment_frequency <- as.data.frame(employment_frequency)
employment_frequency$Employment_Type <- rownames(employment_frequency)

# Create the histogram using ggplot2
ggplot(employment_frequency, aes(x = Employment_Type, y = `Total number of employment`, fill = Employment_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Employment Frequency",
       x = "Employment Type",
       y = "Number of Employment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))


# Social Isolation
offline_interaction <- cvbase[, c("isoFriends_inPerson", "isoOthPpl_inPerson")]
online_interaction <- cvbase[, c("isoFriends_online", "isoOthPpl_online")]
# Create histograms for offline and online interaction
p1 <- ggplot(offline_interaction, aes(x = isoFriends_inPerson)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Offline Interaction with Friends", x = "Days of In-Person Contact", y = "Count")+
  theme_minimal()

p2 <- ggplot(offline_interaction, aes(x = isoOthPpl_inPerson)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Offline Interaction with Others", x = "Days of In-Person Contact", y = "Count")+
  theme_minimal()

p3 <- ggplot(online_interaction, aes(x = isoFriends_online)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Online Interaction with Friends", x = "Days of Online Contact", y = "Count")+
  theme_minimal()

p4 <- ggplot(online_interaction, aes(x = isoOthPpl_online)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Online Interaction with Others", x = "Days of Online Contact", y = "Count")+
  theme_minimal()
# Arrange the plots using cowplot
library(cowplot)
plot_grid(p1, p2, p3, p4, ncol = 2)


# Loneliness and Life Satisfaction
loneliness <- cvbase[, c("lone01", "lone02", "lone03")]
life_satisfaction <- cvbase[, c("happy", "lifeSat", "MLQ")]
#Create histograms for loneliness and life satisfaction
p1 <- ggplot(loneliness, aes(x = lone01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Loneliness: Feeling Lonely", x = "Rating", y = "Count")+
  theme_minimal()

p2 <- ggplot(loneliness, aes(x = lone02)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Loneliness: Feeling Isolated", x = "Rating", y = "Count")+
  theme_minimal()

p3 <- ggplot(loneliness, aes(x = lone03)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Loneliness: Feeling Left Out", x = "Rating", y = "Count")+
  theme_minimal()

p4 <- ggplot(life_satisfaction, aes(x = happy)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Life Satisfaction: Happiness", x = "Rating", y = "Count")+
  theme_minimal()

p5 <- ggplot(life_satisfaction, aes(x = lifeSat)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Life Satisfaction: Life Satisfaction", x = "Rating", y = "Count")+
  theme_minimal()

p6 <- ggplot(life_satisfaction, aes(x = MLQ)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  labs(title = "Life Satisfaction: Sense of Purpose", x = "Rating", y = "Count")+
  theme_minimal()
# Arrange the plots using cowplot
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2)



# Boredom
boredom <- cvbase[, c("bor01", "bor02", "bor03")]
# Create histograms for boredom
p1 <- ggplot(boredom, aes(x = bor01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Boredom: Wish Time Would Go Faster", x = "Rating", y = "Count")+
  theme_minimal()

p2 <- ggplot(boredom, aes(x = bor02)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Boredom: Time Moving Slowly", x = "Rating", y = "Count")+
  theme_minimal()

p3 <- ggplot(boredom, aes(x = bor03)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Boredom: Feeling in Control of Time", x = "Rating", y = "Count")+
  theme_minimal()

# Arrange the plots using cowplot
plot_grid(p1, p2, p3, ncol = 2)


# Conspiracy Theories
conspiracy <- cvbase[, c("consp01", "consp02", "consp03")]
# Create histograms for conspiracy theories
p1 <- ggplot(conspiracy, aes(x = consp01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Conspiracy Theories: Undisclosed Events", x = "Rating", y = "Count")+
  theme_minimal()

p2 <- ggplot(conspiracy, aes(x = consp02)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Conspiracy Theories: Politicians' True Motives", x = "Rating", y = "Count")+
  theme_minimal()

p3 <- ggplot(conspiracy, aes(x = consp03)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Conspiracy Theories: Government Monitoring", x = "Rating", y = "Count")+
  theme_minimal()
# Arrange the plots using cowplot
plot_grid(p1, p2, p3, ncol = 2)


# Rank Order Life
# Create a long format dataframe
rank_order_long <- tidyr::gather(cvbase, key = "Rank_Order_Life", value = "Value", 
                                 starts_with("rankOrdLife"))
# Create the plot
ggplot(rank_order_long, aes(x = Value, fill = Rank_Order_Life)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Rank Order Life Variables",
       x = "Rank Order Life Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# Corona Personal Behavior 
corona_personal_behavior <- cvbase[, c("c19perBeh01", "c19perBeh02", "c19perBeh03")]
# Create histograms for corona personal behavior
p1 <- ggplot(corona_personal_behavior, aes(x = c19perBeh01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Personal Behavior: Wash Hands More", x = "Rating", y = "Count")+
  theme_minimal()

p2 <- ggplot(corona_personal_behavior, aes(x = c19perBeh02)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Personal Behavior: Avoid Crowded Spaces", x = "Rating", y = "Count")+
  theme_minimal()

p3 <- ggplot(corona_personal_behavior, aes(x = c19perBeh03)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Personal Behavior: Put Myself in Quarantine", x = "Rating", y = "Count")+
  theme_minimal()
# Arrange the plots using cowplot
plot_grid(p1, p2, p3, ncol = 2)


# Corona Radical Action
corona_radical_action <- cvbase[, c("c19RCA01", "c19RCA02", "c19RCA03")]
# Create histograms for corona radical action
p1 <- ggplot(corona_radical_action, aes(x = c19RCA01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Radical Action: Support Mandatory Vaccination", x = "Rating", y = "Count")+
  theme_minimal()

p2 <- ggplot(corona_radical_action, aes(x = c19RCA02)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Radical Action: Support Mandatory Quarantine", x = "Rating", y = "Count")+
  theme_minimal()

p3 <- ggplot(corona_radical_action, aes(x = c19RCA03)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Corona Radical Action: Support Reporting Suspected Cases", x = "Rating", y = "Count")+
  theme_minimal()
# Arrange the plots using cowplot
plot_grid(p1, p2, p3, ncol = 2)


# Corona Proximity
corona_proximity <- data.frame(colSums(cvbase[, grepl("coronaClose", names(cvbase))], na.rm = TRUE))
colnames(corona_proximity) <- ("Total number of people in proximity")
rownames(corona_proximity) <- c("Myself", "Family member", "Close friend", "Someone I know", "Someone else", "No one")
# Create a bar plot for corona proximity
ggplot(corona_proximity, aes(x = rownames(corona_proximity), y = `Total number of people in proximity`)) +
  geom_bar(stat = "identity", fill = "cyan3") +
  labs(title = "Corona Proximity",
       x = "Proximity to COVID-19 Cases",
       y = "Total Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Demographic (Gender, Age, Education)
demographic <- cvbase[, c("gender", "age", "edu")]
# Create histograms for demographic variables
p1 <- ggplot(demographic, aes(x = gender)) +
  geom_bar(fill = "cyan3") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

p2 <- ggplot(demographic, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

p3 <- ggplot(demographic, aes(x = edu)) +
  geom_bar(fill = "cyan3") +
  labs(title = "Education Distribution", x = "Education Level", y = "Count") +
  theme_minimal()
# Arrange the plots using cowplot
plot_grid(p1, p2, p3, ncol = 2)


# Country Self Report
country <- cvbase[, c("coded_country")]
unique(country)
table(country)


# Corona ProSocial Behavior
corona_prosocial_behavior <- cvbase[, c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")]
# Create a faceted histogram
ggplot(corona_prosocial_behavior, aes(x = c19ProSo01)) +
  geom_histogram(binwidth = 1, fill = "cyan3", color = "white") +
  facet_wrap(~ names(corona_prosocial_behavior), ncol = 2) +
  labs(title = "Corona Pro-Social Behavior",
       x = "Rating",
       y = "Count") +
  theme_minimal()





  #b)
# Function to check if a row has only NA values in a given set of columns
has_only_na <- function(data, cols) {
  rowSums(is.na(data[, cols])) == length(cols)
}

# Remove rows with only NA values in any category
cvbase_cleaned <- cvbase %>%
  filter(!has_only_na(., c("employstatus_1", "employstatus_2", "employstatus_3", "employstatus_4",
                           "employstatus_5", "employstatus_6", "employstatus_7", "employstatus_8",
                           "employstatus_9", "employstatus_10")),
         !has_only_na(., c("isoFriends_inPerson", "isoOthPpl_inPerson", "isoFriends_online", "isoOthPpl_online")),
         !has_only_na(., c("lone01", "lone02", "lone03")),
         !has_only_na(., c("happy", "lifeSat", "MLQ")),
         !has_only_na(., c("bor01", "bor02", "bor03")),
         !has_only_na(., c("consp01", "consp02", "consp03")),
         !has_only_na(., c("rankOrdLife_1", "rankOrdLife_2", "rankOrdLife_3", "rankOrdLife_4", "rankOrdLife_5", "rankOrdLife_6")),
         !has_only_na(., c("c19perBeh01", "c19perBeh02", "c19perBeh03")),
         !has_only_na(., c("c19RCA01", "c19RCA02", "c19RCA03")),
         !has_only_na(., c("coronaClose_1", "coronaClose_2", "coronaClose_3", "coronaClose_4", "coronaClose_5", "coronaClose_6")),
         !has_only_na(., c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")))


# Function to convert Rank Order Life variables to numeric (from A->F to 1->6)
convert_rank_order <- function(df, prefix) {
  # Get the unique values in the rank order variables
  unique_values <- c("A", "B", "C", "D", "E", "F")
  
  # Create new numeric attributes
  for (i in 1:length(grep(prefix, names(df)))) {
    var_name <- names(df)[grep(prefix, names(df))][i]
    df[[paste0(var_name, "_num")]] <- match(df[[var_name]], unique_values)
  }
  return(df)
}

cvbase_cleaned <- convert_rank_order(cvbase_cleaned, "rankOrdLife")
cvbase_cleaned$rankOrdLife_1 <- NULL
cvbase_cleaned$rankOrdLife_2 <- NULL
cvbase_cleaned$rankOrdLife_3 <- NULL
cvbase_cleaned$rankOrdLife_4 <- NULL
cvbase_cleaned$rankOrdLife_5 <- NULL
cvbase_cleaned$rankOrdLife_6 <- NULL



# Function to impute missing values with 0
impute_zero <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- ifelse(is.na(df[[col]]), 0, df[[col]])
  }
  return(df)
}

# Function to impute missing values with mode
impute_mode <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- ifelse(is.na(df[[col]]), 
                        as.numeric(names(which.max(table(df[[col]], useNA = "always")))), 
                        df[[col]])
  }
  return(df)
}


# Impute missing values with 0
cvbase_cleaned <- cvbase_cleaned %>%
  filter(!has_only_na(., c("employstatus_1", "employstatus_2", "employstatus_3", "employstatus_4",
                           "employstatus_5", "employstatus_6", "employstatus_7", "employstatus_8",
                           "employstatus_9", "employstatus_10",
                           "coronaClose_1", "coronaClose_2", "coronaClose_3", "coronaClose_4", "coronaClose_5", "coronaClose_6"))) %>%
  impute_zero(c("employstatus_1", "employstatus_2", "employstatus_3", "employstatus_4",
                "employstatus_5", "employstatus_6", "employstatus_7", "employstatus_8",
                "employstatus_9", "employstatus_10",
                "coronaClose_1", "coronaClose_2", "coronaClose_3", "coronaClose_4", "coronaClose_5", "coronaClose_6"))


# Impute other variables with mode
cvbase_cleaned <- impute_mode(cvbase_cleaned, c("isoFriends_inPerson", "isoOthPpl_inPerson", "isoFriends_online", "isoOthPpl_online",
                                                "happy", "lifeSat", "MLQ", 
                                                "bor01", "bor02", "bor03", 
                                                "consp01", "consp02", "consp03",
                                                "lone01", "lone02", "lone03", 
                                                "c19perBeh01", "c19perBeh02", "c19perBeh03",
                                                "c19RCA01", "c19RCA02", "c19RCA03",
                                                "gender", "age", "edu",
                                                "c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04",
                                                "rankOrdLife_1_num", "rankOrdLife_2_num", "rankOrdLife_3_num", 
                                                "rankOrdLife_4_num", "rankOrdLife_5_num", "rankOrdLife_6_num"))

View(cvbase_cleaned)
summary(cvbase_cleaned)







# Question 2
  # Filter out data of focus country (Argentina)
cvbase_argentina <- data.frame(filter(cvbase_cleaned, coded_country == "Argentina"))
cvbase_argentina$coded_country <- NULL
View(cvbase_argentina)

cvbase_others <- data.frame(filter(cvbase_cleaned, coded_country != "Argentina"))
cvbase_others$coded_country <- NULL
View(cvbase_others)

#a)
# Summarize the distributions for each attribute
for (col in names(cvbase_argentina)) {
  print(paste0("Variable: ", col))
  
  print("Argentina:")
  print(summary(cvbase_argentina[[col]]))
  print("Other countries:")
  print(summary(cvbase_others[[col]]))
}

  
# Initialize an empty dataframe to store the summary statistics and p-values
summary_df <- data.frame(
  Variable = character(),
  Argentina_Mean = numeric(),
  Argentina_Median = numeric(),
  Argentina_Min = numeric(),
  Argentina_Max = numeric(),
  Other_Mean = numeric(),
  Other_Median = numeric(),
  Other_Min = numeric(),
  Other_Max = numeric(),
  stringsAsFactors = FALSE
)

# Summarize the distributions for each attribute
for (col in names(cvbase_argentina)) {
  arg_summary <- summary(cvbase_argentina[[col]])
  others_summary <- summary(cvbase_others[[col]])
  
  # Extract summary statistics
  arg_mean <- arg_summary[["Mean"]]
  arg_median <- arg_summary[["Median"]]
  arg_min <- min(cvbase_argentina[[col]], na.rm = TRUE) # Compute min value
  arg_max <- max(cvbase_argentina[[col]], na.rm = TRUE) # Compute max value
  
  other_mean <- others_summary[["Mean"]]
  other_median <- others_summary[["Median"]]
  other_min <- min(cvbase_others[[col]], na.rm = TRUE) # Compute min value
  other_max <- max(cvbase_others[[col]], na.rm = TRUE) # Compute max value
  
  # Add the summary statistics and p-value to the dataframe
  summary_df <- rbind(
    summary_df,
    data.frame(
      Variable = col,
      Argentina_Mean = arg_mean,
      Argentina_Median = arg_median,
      Argentina_Min = arg_min,
      Argentina_Max = arg_max,
      Other_Mean = other_mean,
      Other_Median = other_median,
      Other_Min = other_min,
      Other_Max = other_max
    )
  )
}
View(summary_df)
write.csv(summary_df, "summary_ARG_OTHERS.csv", row.names = FALSE)




# (b) How well do participant responses (attributes) predict pro-social attitudes (c19ProSo01,2,3 and 4) for your focus country? Which attributes seem to be the best predictors? Explain your reasoning.
# Multiple linear regression model Analysis
model_argentina <- lm(cbind(c19ProSo01, c19ProSo02, c19ProSo03, c19ProSo04) ~ ., data = cvbase_argentina)
summary(model_argentina)


# Correlation Analysis
cor_argentina <- data.frame(round(cor(cvbase_argentina[, !names(cvbase_argentina) %in% c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")], 
                                      cvbase_argentina[, c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")]), 
                                  digits = 2))
View(cor_argentina)
cor_argentina

# Convert the correlation matrix to a long format data frame
cor_long <- cor_argentina %>%
  rownames_to_column("Variable1") %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(cols = -c(Variable1, row_num), 
               names_to = "Variable2",
               values_to = "Correlation")

# Create the heatmap for the correlation (Argentina countries)
ggplot(cor_long, aes(x = Variable2, y = Variable1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "cyan3", mid = "white", high = "orange", midpoint = 0) +
  scale_x_discrete(limits = unique(cor_long$Variable2)) +
  scale_y_discrete(limits = unique(cor_long$Variable1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Heatmap for Argentina",
       x = NULL, y = NULL,
       fill = "Correlation")




# (c) Repeat Question 2(b) for the other countries as a group. Which attributes are the strongest predictors? How do these attributes compare to those of your focus country?
# Multiple linear regression model 
model_others <- lm(cbind(c19ProSo01, c19ProSo02, c19ProSo03, c19ProSo04) ~ ., data = cvbase_others)
summary(model_others)

# Correlation
cor_others <- data.frame(round(cor(cvbase_others[, !names(cvbase_others) %in% c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")], 
                                      cvbase_others[, c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")]), 
                                  digits = 2))
View(cor_others)
cor_others

# Convert the correlation matrix to a long format data frame
cor_long_o <- cor_others %>%
  rownames_to_column("Variable1") %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(cols = -c(Variable1, row_num), 
               names_to = "Variable2",
               values_to = "Correlation")

# Create the heatmap for other countries
ggplot(cor_long_o, aes(x = Variable2, y = Variable1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "cyan3", mid = "white", high = "orange", midpoint = 0) +
  scale_x_discrete(limits = unique(cor_long_o$Variable2)) +
  scale_y_discrete(limits = unique(cor_long_o$Variable1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Heatmap for Other Countries",
       x = NULL, y = NULL,
       fill = "Correlation")









#Question 3:
# (a) Clustering
life_expectancy <- read.csv("C:/Users/hothu/Downloads/life_expectancy.csv", header=TRUE)
gpd_per_capita <- read.csv("C:/Users/hothu/Downloads/gpd_per_capita.csv", header=FALSE, comment.char="#")
gpd_per_capita <- read.csv("C:/Users/hothu/Downloads/gpd_per_capita.csv")
health_infrastructure <- read.csv("C:/Users/hothu/Downloads/WHO_OECD_health_infrastructure.csv")
government_effectiveness <- read.csv("C:/Users/hothu/Downloads/recent_wb_government_effectiveness.csv")
education_statistics <- read.csv("C:/Users/hothu/Downloads/recent_education_statistics.csv")
death_rate <- read.csv("C:/Users/hothu/Downloads/recent_fctb_death_rate.csv")
COVID.19.global.data <- read.csv("C:/Users/hothu/Downloads/death_rate_2022.csv")


colnames(gpd_per_capita)[1] <- "country"
colnames(COVID.19.global.data)[3] <- "country"


# Choosing indicators
life_expectancy_cols <- c("country", "lifeexpectancy.2102_2020")
gpd_per_capita_cols <- c("country", "X2020")
health_infrastructure_cols <- c("country", "total_n_of_doctors")
government_effectiveness_cols <- c("country", "latest.value_estimate_control_of_corru.364")
education_cols <- c("country", "latest.value_value_adult_literacy_rate.28134")
death_rate_cols <- c("country", "latest.value_deathrate.2066")
COVI_cols <- c("country", "Cumulative_cases", "Cumulative_deaths")

# Merging columns
country_data <- life_expectancy %>% select(all_of(life_expectancy_cols)) %>%
  left_join(gpd_per_capita %>% select(all_of(gpd_per_capita_cols)), by = "country") %>%
  left_join(health_infrastructure %>% select(all_of(health_infrastructure_cols)), by = "country") %>%
  left_join(government_effectiveness %>% select(all_of(government_effectiveness_cols)), by = "country") %>% 
  left_join(education_statistics %>% select(all_of(education_cols)), by = "country") %>%
  left_join(death_rate %>% select(all_of(death_rate_cols)), by = "country") %>%
  left_join(COVID.19.global.data %>% select(all_of(COVI_cols)), by = "country")
data.frame(country_data)
colnames(country_data) <- c("Country", "Life Expectancy", "GDP per Capita", "Health Infrastructure (Doctors)", 
                            "Government Effectiveness Score", "Education Level", "Death Rate", 
                            "COVID-19 Cumulative Cases", "COVID-19 Cumulative Deaths")
country_data <- na.omit(country_data)
country_data

# Saving the dataset as a CSV file
write.csv(country_data, "country_data.csv", row.names = FALSE)
getwd()



# Load the necessary libraries
library(cluster)

# Standardize the numeric variables
country_data_scaled <- country_data
country_data_scaled[,2:9] <- scale(country_data_scaled[,2:9])
rownames(country_data_scaled) <- country_data_scaled$Country
View(country_data_scaled)


# Perform hierarchical clustering
hc_clusters <- hclust(dist(country_data_scaled[,2:9]), method = "ward.D")
hc_clusters

# Determine the optimal number of clusters


# Cut the dendrogram to get 5 clusters
hc_clusters_5 <- cutree(hc_clusters, k = 5)

# Identify the cluster containing Argentina
argentina_cluster <- which(hc_clusters_5 == hc_clusters_5[country_data$Country == "Argentina"])

# Extract the 10 countries most similar to Argentina (excluding Argentina)
similar_countries <- country_data$Country[argentina_cluster][country_data$Country[argentina_cluster] != "Argentina"][1:30]

# Print the similar countries
print(similar_countries)

# Plot the dendrogram
plot(hc_clusters, hang = -1, cex = 0.5)
rect.hclust(hc_clusters, k = 5, border = "red")






# 3b. How well do participant responses (attributes) predict pro-social attitudes 
#(c19ProSo01,2,3 and 4) for this cluster of similar countries? 
#Which attributes are the strongest predictors? 
#How do these attributes compare to those of your focus country?

# Extract the data for the cluster of similar countries
similar_countries_data <- subset(cvbase_cleaned[cvbase_cleaned$coded_country %in% similar_countries, ], select = - coded_country)

# Correlation Analysis
cor_cluster <- data.frame(round(cor(similar_countries_data[, !names(cvbase_others) %in% c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")], 
                                   similar_countries_data[, c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")]), 
                               digits = 2))
cor_cluster

# Convert the correlation matrix to a long format data frame
cor_long_clus <- cor_cluster %>%
  rownames_to_column("Variable1") %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(cols = -c(Variable1, row_num), 
               names_to = "Variable2",
               values_to = "Correlation")
cor_long_clus

# Create the heatmap for similar countries
ggplot(cor_long_clus, aes(x = Variable2, y = Variable1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "cyan3", mid = "white", high = "orange", midpoint = 0) +
  scale_x_discrete(limits = unique(cor_long_clus$Variable2)) +
  scale_y_discrete(limits = unique(cor_long_clus$Variable1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Heatmap for Similar Countries",
       x = NULL, y = NULL,
       fill = "Correlation")


# Multiple Linear Regression (Cluster)
model_similar_countries <- lm(cbind(c19ProSo01, c19ProSo02, c19ProSo03, c19ProSo04) ~ ., data = similar_countries_data)
summary(model_similar_countries)




# Prepare the data for the Multivariate Graphic (the comparison correlation heatmaps)
data_for_graphic <- data.frame()

# Add Argentina data
argentina_long <- cor_argentina %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  mutate(Country = "Argentina")
data_for_graphic <- rbind(data_for_graphic, argentina_long)

# Add other countries data
others_long <- cor_others %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  mutate(Country = "Other Countries")
data_for_graphic <- rbind(data_for_graphic, others_long)

# Add cluster of similar countries data
cluster_long <- cor_cluster %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  mutate(Country = "Cluster of Similar Countries")
data_for_graphic <- rbind(data_for_graphic, cluster_long)
data_for_graphic

# Create the multivariate graphic
ggplot(data_for_graphic, aes(x = Variable2, y = Variable1, fill = Correlation)) +
  geom_tile() +
  facet_wrap(~ Country, ncol = 3) +
  scale_fill_gradient2(low = "cyan3", mid = "white", high = "orange", midpoint = 0) +
  scale_x_discrete(limits = unique(data_for_graphic$Variable2)) +
  scale_y_discrete(limits = unique(data_for_graphic$Variable1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 10)) +
  labs(title = "Comparison of Correlation Heatmaps: Argentina, Other Countries, and Cluster of Similar Countries",
       x = NULL, y = NULL,
       fill = "Correlation")

