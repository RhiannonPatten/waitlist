library(tidyverse)
library(readxl)
library(tidymodels)


data <- read_excel("data/Waitlist_baseline.xlsx", sheet = 1)
summary(data)
data %>% count(PA_guidelines, sort = TRUE)
data %>% count(high.BP, sort = TRUE)
# testing if missing index across dataset
test <- data %>% 
  filter(is.na(PA_guidelines)) %>% 
  select(high.BP, high.cholesterol, diabetes, smoke, PA_guidelines)

#keep data with nonmissing variables used in index
#high.BP, high.cholesterol, diabetes, smoke
data_clean <- data %>% filter(
  !is.na(PA_guidelines)&!is.na(high.cholesterol))
sum(is.na(data_clean$high.BP))
sum(is.na(data_clean$high.cholesterol))    
sum(is.na(data_clean$diabetes))
sum(is.na(data_clean$smoke))


#create new variable
data_clean <- data_clean %>% mutate(
  PA_guidelines_recoded = case_when(
    (PA_guidelines == "Sedentary"|PA_guidelines == "Insufficiently active") ~ 1,
    TRUE~0
  ),
  high.BP = case_when(high.BP == 2 ~ 0, TRUE ~ high.BP), 
  high.cholesterol = case_when(high.cholesterol == 2 ~ 0, TRUE ~ high.cholesterol), 
  diabetes = case_when(diabetes == 2 ~ 0, TRUE ~ diabetes)
)

data_clean%>% count(PA_guidelines_recoded)

data_clean %>% count(diabetes, sort = TRUE)

#creating index
data_clean <- data_clean %>% mutate(
  cvdrisk_index =  high.BP + high.cholesterol + diabetes + smoke + PA_guidelines_recoded
)

data_clean %>% count(cvdrisk_index, sort = TRUE)

#have a look at category 4 and 5 to see if we can merge
data_clean %>% filter(
  cvdrisk_index == 4|cvdrisk_index == 5
) %>% write_csv("data/category4_5.csv")
