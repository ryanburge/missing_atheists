library(tidymodels)

## This is cleaning up the dataset ####

set <- cces %>% 
  filter(year == 2008 | year == 2010 | year == 2012) %>% 
  mutate(target = frcode(religion == 9 ~ "Atheist",
                         TRUE ~ "All Others")) %>% 
  as_tibble() %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(male =  case_when(gender == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(married = case_when(marital_status == 1 ~ 1, TRUE  ~ 0)) %>% 
  mutate(ba = case_when(pew_bornagain == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(attend = 7 - pew_attendance) %>% 
  mutate(relimp = 5 - pew_importance) %>% 
  mutate(prayer = 8 - pew_prayers) %>% 
  mutate(attend = replace_na(attend, 0)) %>% 
  mutate(relimp = replace_na(relimp, 0)) %>% 
  mutate(prayer = replace_na(prayer, 0)) %>% 
  mutate(pid7 = replace_na(pid7, 0)) %>% 
  mutate(ideo5 = replace_na(ideo5, 0)) %>%
  mutate(educ = replace_na(educ, 0)) %>% 
  mutate(income = replace_na(income, 0)) %>% 
  select(target, white, male, married, ba, attend, relimp, prayer, pid7, ideo5, age, educ, income, year)

## Split into training and testing sets ####

train <- set %>% filter(year == 2008 | year == 2012) %>% select(-year)
test <- set %>% filter(year == 2010) %>% select(-year)

## This is training the random forest ####
ten_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(target ~ ., data = train)


## Variable Importance ####
importance <- varImp(ten_rf$fit, scale = TRUE) %>% 
  rownames_to_column() %>% 
  arrange(-Overall) %>% 
  mutate(scale = Overall/940.04581)


ten_rf %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = target, estimate = .pred_class)

### Making Guesses on the 2010 data - merging with the original dataset ####
out <- ten_rf %>%
  predict(test, type = "prob") %>% 
  select(atheist = .pred_Atheist, others = `.pred_All Others`)

new10 <- bind_cols(test, out)

## Making bar graph for comparing the two datasets ####

aaa1 <- train %>%
  filter(target == "Atheist") %>% 
  mutate(college = case_when(educ == 5 | educ == 6 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "College Degree")

aaa2 <- new10 %>% 
  filter(atheist > .21) %>% 
  mutate(college = case_when(educ == 5 | educ == 6 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "College Degree")

aaa3 <- train %>%
  filter(target == "Atheist") %>% 
  mutate(college = case_when(attend == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Never Attend")

aaa4 <- new10 %>% 
  filter(atheist > .21) %>% 
  mutate(college = case_when(attend == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Never Attend")

aaa5 <- train %>%
  filter(target == "Atheist") %>% 
  mutate(college = case_when(relimp == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Religion\nNot Important")

aaa6 <- new10 %>% 
  filter(atheist > .21) %>% 
  mutate(college = case_when(relimp == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Religion\nNot Important")

aaa7 <- train %>%
  filter(target == "Atheist") %>% 
  mutate(college = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Democrat")

aaa8 <- new10 %>% 
  filter(atheist > .21) %>% 
  mutate(college = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Democrat")

aaa9 <- train %>%
  filter(target == "Atheist") %>% 
  mean_ci(white) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "White")

aaa10 <- new10 %>% 
  filter(atheist > .21) %>% 
  mean_ci(white) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "White")

aaa11 <- train %>%
  filter(target == "Atheist") %>% 
  mean_ci(male) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Male")

aaa12 <- new10 %>% 
  filter(atheist > .21) %>% 
  mean_ci(male) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Male")

aaa13 <- train %>%
  filter(target == "Atheist") %>% 
  mutate(college = case_when(prayer == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Never Pray")

aaa14 <- new10 %>% 
  filter(atheist > .21) %>% 
  mutate(college = case_when(prayer == 1 ~ 1, TRUE ~ 0)) %>% 
  mean_ci(college) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Never Pray")

aaa15 <- train %>%
  filter(target == "Atheist") %>% 
  mean_ci(married) %>% 
  mutate(survey = "2008+2012") %>% 
  mutate(var = "Married")

aaa16 <- new10 %>% 
  filter(atheist > .21) %>% 
  mean_ci(married) %>% 
  mutate(survey = "2010") %>% 
  mutate(var = "Married")


graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = survey, y = mean, fill = survey)) +
  geom_col(color = "black") +
  facet_wrap(~ var, ncol = 4) +
  error_bar() +
  theme_gg("Poppins", legend = TRUE) +
  y_pct() +
  scale_fill_npg() +
  labs(x = "", y = "", title = "Predicted Atheists in 2010 vs. Atheist in 2008/2012", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/fig4.png", type = "cairo-png", width = 7)
  
### Figure 5 ####

cces10 <- cces %>% 
  filter(year == 2010) 

final <- bind_cols(cces10, out) %>% 
  as_tibble() %>% 
  select(religion, atheist, others, weight)

ttt1 <- final %>% 
  filter(atheist > .21) %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 5 ~ "Jewish",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           religion == 12 ~ "Something Else", TRUE ~ "All Others")) %>% 
  ct(religpew, weight) %>% 
  mutate(group = "Missing Atheists")

ttt1 %>% 
  ggplot(., aes(x = reorder(religpew, pct), y = pct, fill = religpew)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_tableau() +
  theme_gg("Poppins") +
  y_pct() + 
  labs(x = "", y = "", title = "What Did Atheists Choose in 2010?", caption = "Data: CCES 2010") +
  lab_bar(top = TRUE, type = pct, pos = .025, sz = 4.5) +
  ggsave("D://missing_atheists/images/fig5.png", type = "cairo-png", width = 7)


### How did the missing atheists add to other traditions ####

ttt2 <- final %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 3 ~ "Mormon", 
                           religion == 4 ~ "Orthodox",
                           religion == 5 ~ "Jewish",
                           religion == 6 ~ "Muslim",
                           religion == 7 ~ "Buddhist",
                           religion == 8 ~ "Hindu",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           religion == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, weight) %>% 
  mutate(group = "Full Sample")

share <- bind_cols(ttt1, ttt2) %>% 
  mutate(share = n/n1)

graph <- bind_cols(ttt1, ttt2) %>% 
  mutate(new_tot = n1 - n) %>% 
  select(religpew, new_tot, n) 

graph1 <- graph %>% 
  select(religpew, n) %>% 
  mutate(group = "Probable Atheists")


graph2 <- graph %>% 
  select(religpew, n = new_tot) %>% 
  mutate(group = "Remainder of the Sample")

final <- bind_rows(graph1, graph2)

final %>% 
  ggplot(., aes(x = religpew, y = n, fill = group)) +
  geom_col() +
  coord_flip()

