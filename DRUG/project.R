library(readr)
library(janitor)
library(keras)
library(tidyverse)
library(janitor)
library(plotly)

data <- read_csv("Downloads/drug200.csv")

#### CLEANING ####
data$Drug <- gsub("drug([A-Z])", "drug \\1", data$Drug)
data <- data %>% clean_names()
sum(is.na(data)) # 0

## bp = blood pressure level
## cl = cholesterol level
## na_to_k = sodium to potassium ration in blood


summary(data)
glimpse(data)

table(data['drug']) 
table(data['sex']) 
table(data['bp']) 
table(data['cholesterol']) 



# AGE & CHOLESTEROL



# SEX & DRUG
# Most People using Drug Y
sex_drug <- data %>%
            group_by(sex, drug) %>%
            count(sex)

p <-ggplot(sex_drug, aes(x=drug, y=n, fill=sex, text=n)) +
    geom_bar(stat="identity",color="black", position = position_dodge()) +
    theme(panel.grid = element_blank()) +
    labs(x="Drug Type", y="Total Gender", title="Amount of Gender Using Type of Drugs")

ggplotly(p, tooltip="text")

# AGE & NA to K  Ratio

ggplot(data, aes(age, na_to_k)) +
  stat_smooth(method = loess, aes(color=drug)) +
  labs(title="Sodium to Potassium Ratio in Blood by Drugs at every Ages", x="Age", y="Na to K")


# Make Range for Age
new_data <- data
new_data$age <- case_when(
    data$age > 60 & data$age <= 74 ~ '60-74',
    data$age > 45 & data$age <= 60 ~ '46-60',
    data$age > 30 & data$age <= 45 ~ '31-45',
    data$age >= 15 & data$age <= 30 ~ "15-30"
)

# AGE & BLOOD PRESSURE
new_data %>% group_by(age, bp) %>%
            summarise(count = n()) %>%
            ggplot(aes(age,count, fill=bp)) +
            geom_col(color="black", position = "dodge", aes(text=count)) +
            labs(title = "Blood Pressure in Each Age", x="Age", y="Total") +
            theme(panel.grid = element_blank()) 

ggplotly(tooltip = "text")



# CHOLESTEROL LEVEL & AGE
new_data %>% group_by(age, cholesterol) %>%
              summarise(count= n()) %>%
              ggplot(aes(age, count, fill=cholesterol)) +
              geom_col(color="black", position="dodge", aes(text=count)) +
              labs(title="Cholesterol Level in each Age", y="Total", x="Age") +
              theme(panel.grid = element_blank())

ggplotly(tooltip = "text")
    
  
  
# NA to K Ratio in Blood & SEX & BLOOD PRESSURE LEVEL
ggplot(data, aes(sex, na_to_k, fill=sex), text=na_to_k) +
  geom_boxplot(position = "identity") +
  labs(title="Sodium to Potassium Ratio in Blood at Each Gender") 

ggplotly(tooltip = "text")


ggplot(data, aes(bp, na_to_k, fill=bp), text=na_to_k) +
  geom_boxplot(position = "identity") +
  labs(title="Sodium to Potassium Ratio in Blood by Blood Pressure")

ggplotly(tooltip = "text")


##### DECISION TREE MODEL #####
library(rpart)
library(rpart.plot)

train_tree <- data[1:140, ]
test_tree <- data[141:200, ]
m <- rpart(drug~., data = train_tree)
rpart.plot(m)
p <- predict(m, test_tree, type= "class")
plotcp(m)

sum(diag(table(test_tree$drug, p))) / sum(table(test_tree$drug, p)) #0.9833333

########### SAME SAME ############
# AVG AGE & BLOOD PRESSURE
avg_age_bp <- data %>%  
              group_by(sex, bp) %>%
              summarise(avg_age = round(mean(age, na.rm = TRUE)),1, N=n()) %>%
              ggplot(aes(x=sex, y=avg_age, fill=bp)) +
              geom_col(position = "dodge", color="black", aes(text=avg_age)) +
              ylim(0.55) +
              labs(title="Average Age vs Blood Pressure Level")
ggplotly(tooltip = "text")


# AGE & CHOLESTEROL LEVEL
data %>% ggplot(aes(text=age)) +
          geom_boxplot(mapping= aes(x=sex, y=age, fill=cholesterol)) +
          facet_wrap(~cholesterol) +
          ggtitle(label= "Boxplot ~ Cholesterol Level by Age and Gender")

ggplotly(tooltip = "text")

# AVG POTASSIUM BY AGE
data %>% group_by(age) %>%
        summarize(avg_by_age = round(mean(na_to_k, na.rm = TRUE), 2)) %>%
        ggplot(aes(x=age, y=avg_by_age), text=avg_by_age) +
        geom_line(color="blue") +
        geom_point(color="black") +
        theme(panel.grid = element_blank()) +
        labs(x="Age", y="Average Na to K", title="Average Potassium by Age") 
        
ggplotly(text="avg_by_age")

# new cleaned_data
write.csv(data, "cleaned_data.csv", row.names = FALSE)






  
