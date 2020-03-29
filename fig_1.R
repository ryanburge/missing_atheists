
library(patchwork)

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

gg1 <- cces08 %>% 
  mutate(religpew = frcode(V219 == 1 ~ "Protestant",
                           V219 == 2 ~ "Catholic",
                           V219 == 3 ~ "Mormon", 
                           V219 == 4 ~ "Orthodox",
                           V219 == 5 ~ "Jewish",
                           V219 == 6 ~ "Muslim",
                           V219 == 7 ~ "Buddhist",
                           V219 == 8 ~ "Hindu",
                           V219 == 9 ~ "Atheist",
                           V219 == 10 ~ "Agnostic",
                           V219 == 11 ~ "Nothing in Particular",
                           V219 == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = V201) %>% 
  mutate(year = 2008)

gg2 <- cces10 %>% 
  mutate(religpew = frcode(V219 == 1 ~ "Protestant",
                           V219 == 2 ~ "Catholic",
                           V219 == 3 ~ "Mormon", 
                           V219 == 4 ~ "Orthodox",
                           V219 == 5 ~ "Jewish",
                           V219 == 6 ~ "Muslim",
                           V219 == 7 ~ "Buddhist",
                           V219 == 8 ~ "Hindu",
                           V219 == 9 ~ "Atheist",
                           V219 == 10 ~ "Agnostic",
                           V219 == 11 ~ "Nothing in Particular",
                           V219 == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = V101) %>% 
  mutate(year = 2010)

gg2 <- add_row(gg2, religpew = "Atheist", n = 0, pct = 0, year = 2010)

gg3 <- cces12 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant",
                           religpew == 2 ~ "Catholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox",
                           religpew == 5 ~ "Jewish",
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist",
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist",
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing in Particular",
                           religpew == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = V103) %>% 
  mutate(year = 2012) 

gg4 <- cces14 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant",
                           religpew == 2 ~ "Catholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox",
                           religpew == 5 ~ "Jewish",
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist",
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist",
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing in Particular",
                           religpew == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = weight) %>% 
  mutate(year = 2014) 

gg5 <- cces16 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant",
                           religpew == 2 ~ "Catholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox",
                           religpew == 5 ~ "Jewish",
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist",
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist",
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing in Particular",
                           religpew == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = commonweight_vv_post) %>% 
  mutate(year = 2016) 

gg6 <- cces17 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant",
                           religpew == 2 ~ "Catholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox",
                           religpew == 5 ~ "Jewish",
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist",
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist",
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing in Particular",
                           religpew == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = weights_common) %>% 
  mutate(year = 2017) 


gg7 <- cces18 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant",
                           religpew == 2 ~ "Catholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox",
                           religpew == 5 ~ "Jewish",
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist",
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist",
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing in Particular",
                           religpew == 12 ~ "Something Else", TRUE ~ "Unclassified")) %>% 
  ct(religpew, wt = commonpostweight) %>% 
  mutate(year = 2018) 


graph <- bind_rows(gg1, gg2, gg3, gg4, gg5, gg6, gg7)


graph <- graph %>% 
  mutate(facet = frcode(religpew == "Protestant" | religpew == "Catholic" | religpew == "Mormon" | religpew == "Orthodox" ~ "Christian",
                        religpew == "Jewish" | religpew == "Muslim" | religpew == "Buddhist" | religpew == "Hindu" ~ "Other Faith",
                        religpew == "Atheist" | religpew == "Agnostic" | religpew == "Nothing in Particular" | religpew == "Something Else" ~ "Nones")) %>% 
  mutate(fill = frcode(year == 2010 ~ "yes", TRUE ~ "no"))

first <- graph %>% 
  filter(facet == "Christian") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(., aes(x = year, y = pct, fill = fill)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ religpew, ncol = 4) +
  theme_gg("Poppins") +
  y_pct() + 
  scale_fill_npg() +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  lab_bar(top = TRUE, pos = .01, type = pct, sz = 2.75) +
  labs(x = "", y = "", title = "Religious Demography from the CCES (2010-2018)", subtitle = "Christians") +
  ggsave("D://missing_atheists/images/top_row.png", type = "cairo-png", width = 10)


second <- graph %>% 
  filter(facet == "Other Faith") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(., aes(x = year, y = pct, fill = fill)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ religpew, ncol = 4) +
  theme_gg("Poppins") +
  scale_y_continuous(labels = percent, limits = c(0,.45)) +
  scale_fill_npg() +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  lab_bar(top = TRUE, pos = .01, type = pct, sz = 2.75) +
  labs(x = "", y = "", title = "", subtitle = "Other Faith") +
  ggsave("D://missing_atheists/images/middle_row.png", type = "cairo-png", width = 10)

third <- graph %>% 
  filter(facet == "Nones") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(., aes(x = year, y = pct, fill = fill)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ religpew, ncol = 4) +
  theme_gg("Poppins") +
  scale_y_continuous(labels = percent, limits = c(0,.45)) +
  scale_fill_npg() +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  lab_bar(top = TRUE, pos = .01, type = pct, sz = 2.75) +
  labs(x = "", y = "", title = "", subtitle = "No Religion") +
  ggsave("D://missing_atheists/images/bottom_row.png", type = "cairo-png", width = 10)


all3 <- first / third / second

ggsave("D://missing_atheists/images/fig1.png", type = "cairo-png", all3, width = 10, height = 12)



