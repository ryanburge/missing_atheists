
cces <- read.fst("C://new_cces.fst")


graph <- cces %>% 
  filter(year >= 2008) %>% 
  filter(pew_attendance <= 6) %>% 
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
  mutate(attend = frcode(pew_attendance == 6 ~ "Never",
                         pew_attendance == 5 ~ "Seldom", 
                         pew_attendance == 4 ~ "Yearly",
                         pew_attendance == 3 ~ "Monthly", 
                         pew_attendance == 2 ~ "Weekly",
                         pew_attendance == 1 ~ "Weekly+")) %>% 
  mutate(att = 7 - pew_attendance) %>% 
  group_by(religpew, year) %>% 
  mean_ci(att, wt = weight, ci = .84)

graph %>% 
  mutate(fill = frcode(year == 2010 ~ "yes", TRUE ~ "no")) %>% 
  filter(year == 2008 | year == 2010 | year == 2012) %>% 
  filter(religpew == "Protestant" | religpew == "Catholic" | religpew == "Atheist" | religpew == "Agnostic" | religpew == "Nothing in Particular" | religpew == "Jewish") %>% 
  ggplot(., aes(y=mean, x= fct_reorder(religpew, mean), color = as.factor(year))) +
  geom_point(position=position_dodge(width=0.5), size =2, shape = 21, stroke = 2) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, width = 0) +
  coord_flip() +
  scale_color_manual(values= c("#4DBBD5", "#E64B35", "#4DBBD5")) +
  theme_gg("Poppins", legend = TRUE) +
  labs(title = "Average Church Attendance for Major Religious Groups", x = "", y = "Mean Church Attendance", caption = "Data: CCES 2008-2012") +
  scale_y_continuous(limits = c(1,4.25), breaks = c(1,2,3,4), labels = c("Never", "Seldom", "Yearly", "Monthly")) +
  ggsave("D://missing_atheists/images/Extra/attend_means.png", type = "cairo-png", width = 10)



graph <- cces %>% 
  group_by(year) %>% 
  filter(pew_attendance == 6) %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           religion == 12 ~ "Something Else", TRUE ~ "All Others")) %>% 
  ct(religpew, wt = weight)
  


graph %>% 
  ggplot(., aes(x = 1, y = pct, fill = religpew)) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_tableau() + 
  theme_gg("Poppins") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y.left = element_text(angle=0)) +
  guides(fill = guide_legend(reverse=T)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.04, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font") +
  labs(x = "", y = "", title = "Religious Distribution of Those Who Never Attend Church", subtitle = "", caption = "") +
  ggsave("D://missing_atheists/images/fig3.png", width = 9, height = 6)

