regg08 <- cces %>% 
  filter(year == 2008) %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           TRUE ~ "All Others")) %>% 
  mutate(att = 7 - pew_attendance) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; 1:4=0; else = NA")) %>% 
  mutate(age = age - 18) %>% 
  mutate(age = age/82) %>% 
  mutate(ed = educ/6) %>% 
  mutate(year = as.factor(year)) %>% 
  select(religpew, att, male, white, rep, year, ed, age, pid7)

regg10 <- cces %>% 
  filter(year == 2010) %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           TRUE ~ "All Others")) %>% 
  mutate(att = 7 - pew_attendance) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; 1:4=0; else = NA")) %>% 
  mutate(age = age - 18) %>% 
  mutate(age = age/82) %>% 
  mutate(ed = educ/6) %>% 
  mutate(year = as.factor(year)) %>% 
  select(religpew, att, male, white, rep, year, ed, age, pid7)

regg12 <- cces %>% 
  filter(year == 2012) %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           TRUE ~ "All Others")) %>% 
  mutate(att = 7 - pew_attendance) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; 1:4=0; else = NA")) %>% 
  mutate(age = age - 18) %>% 
  mutate(ed = educ/6) %>% 
  mutate(age = age/82) %>% 
  mutate(year = as.factor(year)) %>% 
  select(religpew, att, male, white, rep, year, ed, age, pid7)

nm <- c("2008", "2010", "2012")
coef <- c("Male" = "male", 
          "White" = "white", 
          "Church Attendance" = "att", 
          "Age" = "age", 
          "Education" = "ed")

reg1 <- regg08 %>% filter(religpew == "Protestant") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")  
reg2 <- regg10 %>% filter(religpew == "Protestant") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 
reg3 <- regg12 %>% filter(religpew == "Protestant") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 

plol <- plot_summs(reg1, reg2, reg3, model.names = nm, omit.coefs = "(Intercept)", coefs = coef)

t1 <- plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "Independent Variable", title = "Predicting Republican Identification", subtitle = "Protestants", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/prot_reg.png", type = "cairo-png", width = 7, height = 7)


reg1 <- regg08 %>% filter(religpew == "Catholic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 
reg2 <- regg10 %>% filter(religpew == "Catholic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")              
reg3 <- regg12 %>% filter(religpew == "Catholic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")   


plol <- plot_summs(reg1, reg2, reg3, model.names = nm, coefs = coef, omit.coefs = "(Intercept)")

t2 <- plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "", title = "", subtitle = "Catholics", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/cath_reg.png", type = "cairo-png", width = 7, height = 7)

reg1 <- regg08 %>% filter(religpew == "Agnostic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 
reg2 <- regg10 %>% filter(religpew == "Agnostic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")              
reg3 <- regg12 %>% filter(religpew == "Agnostic") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")              


plol <- plot_summs(reg1, reg2, reg3, model.names = nm, coefs = coef, omit.coefs = "(Intercept)")

t3 <- plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "", title = "", subtitle = "Agnostics", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/agn_reg.png", type = "cairo-png", width = 7, height = 7)

reg1 <- regg08 %>% filter(religpew == "Nothing in Particular") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 
reg2 <- regg10 %>% filter(religpew == "Nothing in Particular") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 
reg3 <- regg12 %>% filter(religpew == "Nothing in Particular") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial") 


plol <- plot_summs(reg1, reg2, reg3, model.names = nm, coefs = coef, omit.coefs = "(Intercept)")

b1 <- plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "", title = "", subtitle = "Nothing in Particular", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/nip_reg.png", type = "cairo-png", width = 7, height = 7)


reg1 <- regg08 %>% filter(religpew == "All Others") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")  
reg2 <- regg10 %>% filter(religpew == "All Others") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")  
reg3 <- regg12 %>% filter(religpew == "All Others") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")  


plol <- plot_summs(reg1, reg2, reg3, model.names = nm, coefs = coef, omit.coefs = "(Intercept)")

b2 <-plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "", title = "", subtitle = "All Others", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/allo_reg.png", type = "cairo-png", width = 7, height = 7)


reg1 <- regg08 %>% filter(religpew == "Atheist") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")     
reg2 <- regg10 %>% filter(religpew == "Atheist") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")     
reg3 <- regg12 %>% filter(religpew == "Atheist") %>% glm(rep ~ att + ed + male + white + age, data = ., family = "binomial")     

nm2 <- c("2008", "2012")

plol <- plot_summs(reg1, reg3, model.names = nm2, coefs = coef, omit.coefs = "(Intercept)")

b3 <- plol +
  theme_gg("Poppins") +
  theme(legend.position = c(.75, .15)) +
  labs(x = "Coefficient Estimate", y = "", title = "", subtitle = "Atheists", caption = "Data: CCES 2008-2012") +
  ggsave("D://missing_atheists/images/ath_reg.png", type = "cairo-png", width = 7, height = 7)


six <- (t1 | t2 | t3) / (b1 | b2 | b3)

ggsave("D://missing_atheists/images/allsix.png", type = "cairo-png", six, width = 20, height = 16)
