# PredictingExercise

---
title: "GLM Data "
output:
  pdf_document: default
  html_document: default
date: "2023-09-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#install.packages('statmod')
#install.packages('car')
library(statmod)
library(ggplot2)
library(gridExtra)
library(car)
#library(caret)
#install.packages("cowplot")
library(cowplot)
library(xtable)
```

## Data Loading

```{r}
data <- source("http://www.openintro.org/stat/data/cdc.R")
df_before_cleaning <- data$value
library(dplyr)
```

## Data Cleaning

```{r}
num_na <- sum(is.na(df_before_cleaning))

## 0 NA values

df_before_cleaning$desired_weight_loss <-df_before_cleaning$weight-  df_before_cleaning$wtdesire 


## Only want people who have positive weight loss
rows_to_keep <- which(df_before_cleaning$desired_weight_loss >= -200)

df <- df_before_cleaning[rows_to_keep,]

```



## EDA: Categorical Variables

Ensuring that all of the categories have at like around 5 percent of the data points. 

```{r}
num_na <- sum(is.na(df))
n <- nrow(df)
data_types <- sapply(df, class)
cat("Number of NA values:", num_na, "\n")
cat("Number of Rows:", n, "\n")
print(data_types)
```

```{r}
# Calculate the table counts
table_counts_gen <- table(df$genhlth)

# Calculate the total count
total_count_gen <- sum(table_counts_gen)

# Calculate the percentages
percentage_table_gen <- (table_counts_gen / total_count_gen) * 100
percentage_table_gen
```
exerany

```{r}
#unique(df$exerany)
df$exerany <- as.factor(df$exerany)

table_counts_ex <- table(df$exerany)

# Calculate the total count
total_count_ex <- sum(table_counts_ex)

# Calculate the percentages
percentage_table_ex <- (table_counts_ex / total_count_ex) * 100
percentage_table_ex

```


```{r}
#unique(df$hlthplan)
df$hlthplan <- as.factor(df$hlthplan)
table_counts_hlt <- table(df$hlthplan)

# Calculate the total count
total_count_hlt <- sum(table_counts_hlt)

# Calculate the percentages
percentage_table_hlt <- (table_counts_hlt / total_count_hlt) * 100
percentage_table_hlt

```
```{r}
#unique(df$smoke100)
df$smoke100 <- as.factor(df$smoke100)
table_counts_smoke <- table(df$smoke100)

total_count_smoke <- sum(table_counts_smoke)

percentage_table_smoke <- (table_counts_smoke / total_count_smoke) * 100

percentage_table_smoke
```

```{r}
# Calculate the percentages
# Load the necessary libraries
library(ggplot2)

# Assuming your data frame is named 'df'

# Calculate the percentages
df_summary <- df %>%
  group_by(genhlth, exerany) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a bar chart
ggplot(df_summary, aes(x = genhlth, y = Percentage, fill = factor(exerany))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Exercise in Each Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Exercise") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

```{r}
# Load the necessary libraries
library(ggplot2)

# Assuming your data frame is named 'df'

df_summary_genhlth_ex_1<- df %>%
  group_by(genhlth, exerany) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(genhlth) %>%
  mutate(Percentage = Count / sum(Count) * 100)


# Create a bar chart
ggplot(df_summary_genhlth_ex_1, aes(x = genhlth, y = Percentage, fill = factor(exerany))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Exercise in Each Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Exercise") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
```{r}
table_hlth_smoke <- table(df$hlthplan, df$smoke100)

# Label the rows and columns
rownames(table_hlth_smoke) <- c("No Health Plan", "Has Health Plan")
colnames(table_hlth_smoke) <- c("Did Not Smoke 100 Cigs", "Smoked 100 Cigarettes")

# Print the labeled table
print(table_hlth_smoke)
```
```{r}
# Calculate the percentages
# Calculate the percentages
df_summary_genhlth_smoke100 <- df %>%
  group_by(genhlth, smoke100) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Percentage = Count / sum(Count) * 100)




ggplot(df_summary_genhlth_smoke100, aes(x = genhlth, y = Percentage, fill = factor(smoke100))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Smoking in Each Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Smoking Status") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


```{r}
# Calculate the percentages within each 'genhlth' category
df_summary_genhlth_smoke100_2 <- df %>%
  group_by(genhlth, smoke100) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(genhlth) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a bar chart
library(ggplot2)

ggplot(df_summary_genhlth_smoke100_2, aes(x = genhlth, y = Percentage, fill = factor(smoke100))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Smoking in Each Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Smoking Status") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
```{r}
# Calculate the percentages within each 'genhlth' category for having a health plan
df_summary_gen_health_hlt <- df %>%
  group_by(genhlth, hlthplan) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(genhlth) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Create a bar chart
ggplot(df_summary_gen_health_hlt, aes(x = genhlth, y = Percentage, fill = factor(hlthplan))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Having a Health Plan by General Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Health Plan Status") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
```{r}
# Calculate the percentages within each 'genhlth' category for gender
df_summary_gen_health_gender <- df %>%
  group_by(genhlth, gender) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(genhlth) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Create a bar chart
# Create a bar chart with different colors for gender
library(RColorBrewer)

# Create a bar chart with distinct colors for gender
ggplot(df_summary_gen_health_gender, aes(x = genhlth, y = Percentage, fill = factor(gender))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Gender Distribution by General Health Category",
       x = "General Health",
       y = "Percentage",
       fill = "Gender") +
  scale_fill_brewer(palette = "Set1") +  # Use a distinct color palette
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


```

## EDA:  Numerical Variables

```{r}

str(df)
df$BMI <- (df$weight * 703) / (df$height^2)
library(ggplot2)

# Create a scatterplot of BMI
library(ggplot2)

ggplot(df, aes(y = BMI)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI",
       y = "BMI")

```
```{r}

table(df$genhlth)
677/nrow(df)



```


```{r}
df$log_BMI <- log(df$BMI, base = exp(1))

# Create a box plot of natural log-transformed BMI
ggplot(df, aes(y = log_BMI)) +
  geom_boxplot() +
  labs(title = "Box Plot of Natural Log-Transformed BMI",
       y = "Natural Log(BMI)")

summary(df$log_BMI)
g1 <- ggplot(df, aes(x = BMI)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency") +
  theme_minimal()

# Create a histogram for log_BMI
g2 <- ggplot(df, aes(x = log_BMI)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Histogram of BMI (Logged)", x = "log(BMI)", y = "Frequency") +
  theme_minimal()

grid.arrange(g1,g2,ncol =1)
```
Logging BMI seems to fix it, but does not improve empirical logit plot. Will not proceed with logging it

```{r}
p1 <- emplogitplot1(formula = exerany ~ BMI, data = df, ngroups = 10,
              ylab = "Log(Odds) for Exercise Any", main = "Empirical Logit Plot for BMI")

# Create an empirical logit plot for 'log_BMI' against 'exerany'
p2 <- emplogitplot1(formula = exerany ~ log_BMI, data = df, ngroups = 10,
              ylab = "Log(Odds) for Exercise Any", main = "Empirical Logit Plot for Log BMI")
```

```{r}

g1 <- ggplot(df, aes(x = desired_weight_loss)) +
  geom_histogram(fill = "orange", color = "black", bins = 20) +
  labs(title = "Histogram of Desired Weight Loss", x = "Desired Weight Loss", y = "Frequency") +
  theme_minimal()

g1
```



```{r}
hist(df$height)
## weights seem reasonable
hist(df$desired_weight_loss)

## ages seem reasonable
```



```{r}
## heights seem reasonable
summary(df$height)
sort(decreasing = FALSE, df$height)
filter <- df[df$height <= 53, ]

## all seem reasonable
```





## Data Preperation

```{r}

variables_to_include <- c("desired_weight_loss", "genhlth", "hlthplan", "smoke100", "log_BMI", "age", "gender", "exerany", "height", "BMI")

models_for_forward <- df[variables_to_include]

table(models_for_forward$exerany)


5085/nrow(models_for_forward)

```


```{r}
starting_model <- glm(exerany ~ 1, data = models_for_forward, family = binomial)
library(MASS)
forward_bic <- stepAIC(starting_model, scope = ~ desired_weight_loss + genhlth + hlthplan + smoke100 + BMI + age+ gender + height,
                      direction = "forward",
                      trace = 0, k = log(nrow(models_for_forward)))
forward_bic$formula
```

```{r}
library(pROC)
my_response <- models_for_forward$exerany
my_predictor <- predict(forward_bic, newData = models_for_forward)
my_roc <- roc(my_response, my_predictor)
custom_colors <- c("#0072B2", "#D55E00")  # Blue and orange

# Plot the ROC curve with custom settings
plot(
  my_roc,
  main = "ROC Curve for Forward BIC Model",
  col = custom_colors,           # Set line and point colors
  lwd = 2,                       # Line width
  print.auc = TRUE,              # Print AUC on the plot
  print.auc.x = 0.5,             # X-coordinate for AUC label
  print.auc.y = 0.2,             # Y-coordinate for AUC label
  print.auc.cex = 1.2,           # AUC label font size
  print.auc.col = "darkgreen"    # AUC label color
)

# Add a legend for the colors
legend(
  "bottomright",
  legend = c("ROC Curve"),
  col = custom_colors,
  lty = 1,
  lwd = 2
)

# Define the point with sensitivity 0.625 and specificity 0.62
point_sensitivity <- 0.625
point_specificity <- 0.62

# Add lines from the ROC curve to the x-axis and y-axis
abline(h = point_sensitivity, col = "red", lty = 2)  # Line to y-axis
abline(v = point_specificity, col = "blue", lty = 2)   # Line to x-axis


```


```{r}
qres <- qresid(forward_bic)

ggplot(aes(x = height, y = qres), data = models_for_forward) +  geom_point(size = 0.5) +  geom_smooth(color = "red", linewidth = 0.8,
                    method = "loess", formula = y ~ x, se = FALSE) + labs(title = "Quantile Residual Plot of Height",
        x = "Height",
        y = "Quantile Residual")
```

```{r}

ggplot(aes(x = age, y = qres), data = models_for_forward) +  geom_point(size = 0.5) +  geom_smooth(color = "red", linewidth = 0.8,
                    method = "loess", formula = y ~ x, se = FALSE) + labs(title = "Quantile Residual Plot of Age",
        x = "Age",
        y = "Quantile Residual")
```

```{r}
plot1 <- ggplot(aes(x = height, y = qres), data = models_for_forward) +
  geom_point(size = 0.5, color = "blue") +
  geom_smooth(color = "red", linewidth = 0.8, method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Height", x = "Height", y = "Quantile Residual")

plot2 <- ggplot(aes(x = age, y = qres), data = models_for_forward) +
  geom_point(size = 0.5, color = "green") +
  geom_smooth(color = "red", linewidth = 0.8, method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Age", x = "Age", y = "Quantile Residual")

plot3 <- ggplot(aes(x = BMI, y = qres), data = models_for_forward) +
  geom_point(size = 0.5, color = "orange") +
  geom_smooth(color = "red", linewidth = 0.8, method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "BMI", x = "BMI", y = "Quantile Residual")

# Combine plots using grid.arrange
combined_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3) 

# Add a title to the combined plot
combined_plot <- grid.arrange(
  top = "Quantile Residual Check for Second Model",
  combined_plot
)

```

```{r}
vif(forward_bic)
```

```{r}
max(cooks.distance(forward_bic))

table(df$exerany)
```



```{r}
model_test_with_logged_BMI <- glm(exerany ~ genhlth + height + log_BMI + hlthplan + age, data = models_for_forward, family = "binomial")

model_test_with_logged_BMI$deviance

qres_test <- qresid(model_test_with_logged_BMI)

 ggplot(aes(x = log_BMI, y = qres_test), data = models_for_forward) +  geom_point(size = 0.5) +  geom_smooth(color = "red", linewidth = 0.8,
                    method = "loess", formula = y ~ x, se = FALSE) + labs(title = "Quantile Residual Plot of Log BMI",
        x = "Log BMI",
        y = "Quantile Residual")
 
 ## Changing  log BMI decreases deviance  but barely changes the qunatile residuals keep original model
```


```{r}
#install.packages("caret")
library(caret)

## Create a function that takes in a threshold and runs 10 fold cross validation
k_fold <- function(threshold) {
  n <- nrow(models_for_forward)
  
  # Create the folds
  pool <- rep(1:10, n)
  set.seed(100)
  folds <- sample(pool, n)
  
  # Save True Positives
  true1 <- which(models_for_forward$exerany == 1)
  
  # Save True Negatives
  true0 <- which(models_for_forward$exerany == 0)
  
  # Save amount of True Positives and Negatives
  ntrue1 <- length(true1)
  ntrue0 <- length(true0)
  
  final_predicts <- data.frame("Prediction" = rep(NA, n))
  
  for (f in 1:10) {
    # Step 1: Find the Data in Fold f
    # All the data that is in fold F
    infold <- which(folds == f)
    
    # Step 2: Create training and test
    newTrain <- models_for_forward[-infold, ]
    newTest <- models_for_forward[infold, ]
    
    # Step 3 Create model
    m1 <- glm(forward_bic$formula, data = newTrain,
              family = "binomial")
    
    # Extract the predicted probabilities from
    # the model on Fold F
    K_probs <- predict(m1, newdata = newTest, type = 'response')
    fold_predictions <- ifelse(K_probs >= threshold, 1, 0)
    
    # Store the predicted probabilities
    final_predicts$Prediction[infold] <- fold_predictions
  }
  
  # Combine the predictions and true values into a data frame

  return(final_predicts$Prediction)
}

best_threshold <- 0
accuracy_for_best <-0 
best_sensitivity <- 0
best_specificity <- 0
best_geometric_mean <- 0
best_model <- NULL
best_conf_matrix_for_geo <- NULL
best_predicted_probabilities <- NULL
for (threshold in seq(0.1, 1, by = 0.01)) {
  predictions <- k_fold(threshold)
  if (length(unique(predictions)) == 1) {
    cat('Predictions all have the same value for ', threshold,  '; skipping to the next threshold.\n')
    next  # Skip to the next threshold in the loop
  }
   conf_matrix <- table(real =  models_for_forward$exerany, model = predictions)
  sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
  # Calculate Specificity (True Negative Rate)
  specificity <- conf_matrix[1, 1] / (conf_matrix[1, 2] + conf_matrix[1, 1])
  accuracy <- (sum(predictions == models_for_forward$exerany))/(n)
  
  # Calculate geometric mean
  geometric_mean <- sqrt(sensitivity * specificity)
  
  
  # Check if the current threshold yields a higher geometric mean
  if (geometric_mean > best_geometric_mean) {
    best_threshold <- threshold
    best_sensitivity <- sensitivity
    best_specificity <- specificity
    best_geometric_mean <- geometric_mean
    best_model <- forward_bic  # Save the model
    best_predicted_probabilities <- predicted_probabilities  # Save the predicted probabilities
    best_conf_matrix_for_geo <- conf_matrix
    best_accuracy <- accuracy
  }
}
cat("Best Threshold based on Geometric Mean:", best_threshold, "\n")
cat("Best Sensitivity based on Geometric Mean:", best_sensitivity, "\n")
cat("Best Specificity based on Geometric Mean:", best_specificity, "\n")
cat("Best Geometric Mean based on Geometric Mean:", best_geometric_mean, "\n")
cat("Accuracy for best thresholdhold", best_accuracy)
best_geometric_mean
best_conf_matrix_for_geo
```




```{r}
predictions <- ifelse(forward_bic$fitted.values >=.5, 1,0)
confusion_matrix_for_half <- table(real = models_for_forward$exerany, model = predictions)
sensitivity_for_half <- confusion_matrix_for_half[2, 2] / (confusion_matrix_for_half[2, 1] + confusion_matrix_for_half[2, 2])

# Calculate Specificity (True Negative Rate)
specificity_for_half <- confusion_matrix_for_half[1, 1] / (confusion_matrix_for_half[1, 2] + confusion_matrix_for_half[1, 1])

sensitivity_for_half
specificity_for_half


```
```{r}
predicted_probabilities <- forward_bic$fitted.values

# Initialize variables to store the best threshold and metrics
best_threshold <- 0
best_sensitivity <- 0
best_specificity <- 0
best_geometric_mean <- 0
best_model <- NULL
best_conf_matrix_for_geo <- NULL
best_predicted_probabilities <- NULL

# Iterate through threshold values from 0.1 to 1
for (threshold in seq(0.1, 1, by = 0.01)) {
  # Calculate predictions based on the current threshold
  predictions <- ifelse(predicted_probabilities >= threshold, 1, 0)
  
   if (length(unique(predictions)) == 1) {
    cat('Predictions all have the same value for ', threshold,  '; skipping to the next threshold.\n')
    next  # Skip to the next threshold in the loop
  }
  
  conf_matrix <- table(real =  models_for_forward$exerany, model = predictions)
  sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
  # Calculate Specificity (True Negative Rate)
  specificity <- conf_matrix[1, 1] / (conf_matrix[1, 2] + conf_matrix[1, 1])
  
  # Calculate geometric mean
  geometric_mean <- sqrt(sensitivity *specificity)
  
  # Check if the current threshold yields a higher geometric mean
  if (geometric_mean > best_geometric_mean) {
    best_threshold <- threshold
    best_sensitivity <- sensitivity
    best_specificity <- specificity
    best_geometric_mean <- geometric_mean
    best_model <- forward_bic  # Save the model
    best_predicted_probabilities <- predicted_probabilities  # Save the predicted probabilities
    best_conf_matrix_for_geo <- conf_matrix
  }
}

# Print the best threshold and its metrics
cat("Best Threshold based on Geometric Mean:", best_threshold, "\n")
cat("Best Sensitivity based on Geometric Mean:", best_sensitivity, "\n")
cat("Best Specificity based on Geometric Mean:", best_specificity, "\n")
cat("Best Geometric Mean based on Geometric Mean:", best_geometric_mean, "\n")
best_geometric_mean
xtable(best_conf_matrix_for_geo)

best_conf_matrix_for_geo


summary(forward_bic$coefficients)
```



## Part 1

## Data Preparation/Cleaning for part 1

```{r}

#df_before_cleaning 

rows_for_first_question <- which(df_before_cleaning$desired_weight_loss > 0)

df_for_first_problem <- df_before_cleaning[rows_for_first_question,]


#min(df_for_first_problem$desired_weight_loss)

#str(df_for_first_problem)


variables_for_second <- c("desired_weight_loss", "exerany", "age", "genhlth", "hlthplan")


df_final <- df_for_first_problem[variables_for_second]


str(df_final)
```
```{r}

#unique(df$exerany)
df_final$exerany <- as.factor(df_final$exerany)

table_counts_ex_fin <- table(df_final$exerany)

# Calculate the total count
total_count_ex_fin <- sum(table_counts_ex_fin)

# Calculate the percentages
percentage_table_ex_fin <- (table_counts_ex_fin / total_count_ex_fin) * 100
percentage_table_ex_fin

```


```{r}
df_final$hlthplan <- as.factor(df_final$hlthplan)

table_counts_hlthplan_fin <- table(df_final$hlthplan)

# Calculate the total count
total_count_hlthplan_fin <- sum(table_counts_hlthplan_fin)

# Calculate the percentages
percentage_table_hlthplan_fin <- (table_counts_hlthplan_fin / total_count_hlthplan_fin) * 100
percentage_table_hlthplan_fin
```


```{r}


#df_final$h
lthplan <- as.factor(df_final$hlthplan)

table_counts_genhlth_fin <- table(df_final$genhlth)

# Calculate the total count
total_count_genhlth_fin <- sum(table_counts_genhlth_fin)

# Calculate the percentages
percentage_table_genhlth_fin <- (table_counts_genhlth_fin / total_count_genhlth_fin) * 100
percentage_table_genhlth_fin
```

```{r}

## Empircal logit plots of AGE and log weight loss 
df_final
```


```{r}
## Ashley helped with function
vis_dist <- function(xvar, df){
  colname <- df |> dplyr::select({{ xvar }}) |> colnames()
  if(is.numeric(df |> pull(colname))) {
    df |>
      ggplot(aes(x = {{ xvar }})) + 
      theme_minimal()+ 
      geom_histogram(fill = "#a62675ff") +
      labs(title = paste0(colname, " distribution"),
           x = colname) +
      theme(plot.title = element_text(size = 4),
            axis.text = element_text(size = 4),
            axis.title = element_text(size = 4))
      
  }
  else {
    df |>
    ggplot(aes(x = {{ xvar }})) +
    geom_bar(fill = "#a62675ff") +
   theme_minimal()+
    labs(title = paste0(colname, " distribution"),
           x = colname) +
    theme(plot.title = element_text(size = 4),
          axis.text.x = element_text(size = 4, angle = 30),
          axis.text.y = element_text(size = 4),
          axis.title = element_text(size = 4))
  }
}

str(df_final)

desired_weight_loss_dist  <- vis_dist(desired_weight_loss, df_final)
exerany_dist  <- vis_dist(exerany, df_final)
age_dist <- vis_dist(age, df_final)
genhlth_dist  <- vis_dist(genhlth, df_final) 
hlthplan_dist <- vis_dist(hlthplan, df_final)


grid.arrange(desired_weight_loss_dist, exerany_dist, age_dist,
             genhlth_dist, hlthplan_dist, ncol = 4)
```




```{r}
# Define a consistent color palette
# Define a consistent color palette
hist_color <- "#4682B4"  # Steel Blue color
title_color <- "#333333"  # Dark gray color
background_color <- "#F5F5F5"  # Light gray background color
df_final$log_wt_loss <- log(df_final$desired_weight_loss)

# Create the histogram for log_wt_loss
hist_log_wt_loss <- ggplot(df_final, aes(x = log_wt_loss)) +
  geom_histogram(binwidth = 0.5, fill = hist_color, color = "black") +
  labs(x = "Desired Weight Loss (Logged)", y = "Frequency") +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(plot.title = element_text(size = 11, margin = margin(b = 10), color = title_color)) +
  ggtitle("Logged Desired Weight Loss Histogram")

# Create the histogram for desired_weight_loss
hist_desired_weight_loss <- ggplot(df_final, aes(x = desired_weight_loss)) +
  geom_histogram(binwidth = 5, fill = hist_color, color = "black", position = "identity") +
  labs(x = "Desired Weight Loss (lbs)", y = "Frequency") +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(plot.title = element_text(size = 11, margin = margin(b = 10), color = title_color)) +
  ggtitle("Desired Weight Loss Histogram")

# Combine the two histograms into one figure using cowplot
combined_figure <- plot_grid(
  hist_log_wt_loss, hist_desired_weight_loss, ncol = 2,
  labels = c("A", "B"),  # Add labels A and B to each plot
  rel_widths = c(1, 1)  # Adjust the width of each plot
)

# Create a title for the combined figure
figure_title <- ggdraw() +
  draw_text("Figure 1: Histograms of Weight Loss",
            x = 0.5, y = 0.60, hjust = 0.5, vjust = 0,
            color = title_color, size = 14)

# Create a caption
#caption <- ggdraw() +
#  draw_text("Log-transforming desired weight loss addresses right skew.",
#            x = 0.5, y = 0.12, hjust = 0.5, vjust = 0,
#            color = title_color, size = 10)

# Add the title, combined figure, and caption
final_combined_figure <- plot_grid(
  figure_title,
  combined_figure,
  ncol = 1, rel_heights = c(0.1, 1, 0.1)
)

# Display the final combined figure
final_combined_figure
```


```{r}
#install.packages("Stat2Data")
library(Stat2Data)
emplogitplot1(formula = exerany ~ age, data = df_final,ngroups = 9,
              ylab = "Log(Odds) for Exercise Any", main = "Empirical Logit Plot for Age")

# Create an empirical logit plot for 'log_wt_loss' against 'exerany'
emplogitplot1(formula = exerany ~ log_wt_loss, data = df_final, ngroups = 8,
              ylab = "Log(Odds) for Exercise Any", main = "Empirical Logit Plot for Log Weight Loss")
```



```{r}
df_final
str(df_final)
str(df_before_cleaning)
lm_asso <- glm(exerany ~ age + log_wt_loss + hlthplan + genhlth, data = df_final, family = "binomial")
nrow(df_final)
#rows_for_first_question <- which(df_before_cleaning$desired_weight_loss > 0)

#df_for_first_problem <- df_before_cleaning[rows_for_first_question,]



age_desc <- "Age of respondent in years"
log_wt_loss_desc <- "Logged Desired Loss of the respondent. Equal to log(weight- wtdesire) (What would unit be if the original was in pounds"
hlthplan_desc <- "Indicator variable for whether the respondent had health coverage (1) or not (0)."
genhlth_desc <- "General health of the respondent categorized as excellent, very good, good, fair or poor."
exerany_desc <- "Indicator variable for whether the respondent exercised in the past month (1) or not (0)."
data_desc <- "All respondents who indicated they wanted to lose weight meaning their desired weight loss was greater than 0. n= 12,764"

summary(lm_asso)

xtable(lm_asso)
```


```{r}
create_table <- function(data, response_variable) {
  # Create an empty data frame to store the results
  result <- data.frame(variable = character(),
                       did_exercise = integer(),
                       did_not_exercise = integer(),
                       stringsAsFactors = FALSE)
  
  # Loop through each explanatory variable
  for (variable in names(data)) {
    # Skip the response variable
    if (variable == response_variable) {
      next
    }
    
    # Check if the variable is categorical or numeric
    if (is.factor(data[[variable]])) {
      # For categorical variables, calculate the label spread
      label_spread <- table(data[[variable]], data[[response_variable]])
      
      # Calculate the percentage of each category for the "did_exercise" column
      did_exercise_percent <- prop.table(label_spread[, "1"]) * 100
      
      # Calculate the percentage of each category for the "did_not_exercise" column
      did_not_exercise_percent <- prop.table(label_spread[, "0"]) * 100
      
      # Add a row to the result data frame with the label spread information
      result <- rbind(result,
                      data.frame(variable = variable,
                                 did_exercise = paste0(did_exercise_percent, "%"),
                                 did_not_exercise = paste0(did_not_exercise_percent, "%"),
                                 stringsAsFactors = FALSE))
    } else if (is.numeric(data[[variable]])) {
      # For numeric variables, calculate the 5-number summary
      summary_stats <- summary(data[[variable]])
      
      # Add a row to the result data frame with the summary statistics
      result <- rbind(result,
                      data.frame(variable = variable,
                                 did_exercise = paste0(summary_stats[1], ", ", summary_stats[2], ", ", summary_stats[3], ", ", summary_stats[4], ", ", summary_stats[5]),
                                 did_not_exercise = NA,
                                 stringsAsFactors = FALSE))
    }
  }
  
  return(result)
}

# Example usage:
response_variable <- "exerany"     # Replace "exerany" with your actual response variable name

result_table <- create_table(df_final, response_variable)

```

```{r}

table(df_final$genhlth)

nrow(df_final)

406/12764
model_formula <- as.formula("exerany ~ age + log_wt_loss + hlthplan + genhlth")

# Create a data frame with variable names and descriptions
variable_names <- attr(terms(model_formula), "term.labels")
variable_descriptions <- c(
  "Age of respondent in years",
  "Logged Desired Loss of the respondent. Equal to log(weight - wtdesire) (What would unit be if the original was in pounds)",
  "Indicator variable for whether the respondent had health coverage (1) or not (0).",
  "General health of the respondent categorized as excellent, very good, good, fair, or poor."
)
variable_info <- data.frame(Variable = variable_names, Description = variable_descriptions)

# Set the data description
data_desc <- "All respondents who indicated they wanted to lose weight meaning their desired weight loss was greater than 0. n = 12,764"

# Create an xtable
table_x <- xtable(variable_info, caption = "Variable Descriptions", label = "tab:variable_descriptions")

# Add the data description as a caption
attr(table_x, "caption") <- paste("Variable Descriptions\n", data_desc)

# Print the xtable
print(table_x, caption.placement = "top")
```


`

```{r}
qres_asso <- qresid(lm_asso)
plot1 <- ggplot(aes(x = age, y = qres_asso), data = df_final) +
  geom_point(size = 0.5, color = "blue") +  # Adjust point color
  geom_smooth(color = "darkred", linewidth = 0.8, method = "loess", formula = y ~ x, se = FALSE) +  # Adjust line color
  labs(title = "Quantile Residual Plot of Age",
       x = "Age",
       y = "Quantile Residual")

# Create the second plot
plot2 <- ggplot(aes(x = log_wt_loss, y = qres_asso), data = df_final) +
  geom_point(size = 0.5, color = "green") +
  geom_smooth(color = "purple", linewidth = 0.8, method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Quantile Residual Plot of Desired Weight Loss (Logged)",
       x = "Desired Weight Loss (Logged)",
       y = "Quantile Residual") +
  theme(plot.title = element_text(size = 9))

# Arrange the plots side by side
combined_figure <- plot_grid(
  plot1, plot2, ncol = 2,
  labels = c("A", "B"),  # Add labels A and B to each plot
  rel_widths = c(1, 1)  # Adjust the width of each plot
)

# Create a title for the combined figure
figure_title <- ggdraw() +
  draw_text("Figure 2: Quantile Residual Plots for First Research Question",
            x = 0.5, y = 0.60, hjust = 0.5, vjust = 0,
            color = "navy", size = 14)  # Adjust title color

# Add the title, combined figure, and caption
final_combined_figure <- plot_grid(
  figure_title,
  combined_figure,
  ncol = 1, rel_heights = c(0.1, 1, 0.1)
)

# Display the final combined figure
final_combined_figure


```




```{r}
vif(lm_asso)
```

```{r}
max(cooks.distance(lm_asso))

class(df_final$desired_weight_loss)
str(df_for_summary)
```

```{r}
generate_summary_table <- function(df, variables) {
  # Initialize empty lists to store info for exercise and not exercising
  info_for_exercise <- list()
  info_for_not_exercising <- list()

  # Loop through the variables
  for (variable in variables) {
    # Extract the variable from the data frame
    var_data <- df[[variable]]

    # Check if the variable is numeric
    if (is.numeric(var_data)) {
      # For numeric variables
      info_for_exercise[[variable]] <- paste("Mean:", round(mean(var_data), 2), "Median:", round(median(var_data), 2), "IQR:", round(IQR(var_data), 2))
      info_for_not_exercising[[variable]] <- paste("Mean:", round(mean(var_data), 2), "Median:", round(median(var_data), 2), "IQR:", round(IQR(var_data), 2))
    } else {
      # For categorical variables
      exercise_percent <- prop.table(table(var_data[df$exerany == 1])) * 100
      not_exercise_percent <- prop.table(table(var_data[df$exerany == 0])) * 100

      info_for_exercise[[variable]] <- paste(names(exercise_percent), ": ", round(exercise_percent, 2), "%", collapse = " | ")
      info_for_not_exercising[[variable]] <- paste(names(not_exercise_percent), ": ", round(not_exercise_percent, 2), "%", collapse = " | ")
    }
  }

  # Create a data frame from the lists
  summary_df <- data.frame(Variable = variables, info_for_exercise = unlist(info_for_exercise), info_for_not_exercising = unlist(info_for_not_exercising), stringsAsFactors = FALSE)
  return(summary_df)
}

# List of variables to analyze
variables_to_analyze <- c("age", "genhlth", "hlthplan", "log_wt_loss")

# Generate the summary table
summary_table <- generate_summary_table(df_for_summary, variables_to_analyze)

# Print the summary table
variables_to_analyze <- c("age", "genhlth", "hlthplan", "log_wt_loss")

# Generate the summary table
summary_table <- generate_summary_table(df_for_summary, variables_to_analyze)

# Convert the data frame to xtable
xtable_summary <- xtable(summary_table)

# Print the xtable
print(xtable_summary, caption = "Summary Table")
```


```{r}
str(models_for_forward)

variables_2 <- c("age", "genhlth", "hlthplan", "smoke100", "BMI", "gender", "height")
summary_df_model_2 <- generate_summary_table(models_for_forward, variables_2)


xtable_summary_2 <- xtable(summary_df_model_2)
xtable_summary_2
table(df_final$exerany)


```
