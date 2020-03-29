graph <- cces %>% 
  mutate(imp = frcode(pew_importance == 4 ~ "Not At All Important",
                      pew_importance == 3 ~ "Not Too Important",
                      pew_importance == 2 ~ "Somewhat Important",
                      pew_importance == 1 ~ "Very Important")) %>% 
  filter(imp == "Not At All Important") %>% 
  mutate(religpew = frcode(religion == 1 ~ "Protestant",
                           religion == 2 ~ "Catholic",
                           religion == 9 ~ "Atheist",
                           religion == 10 ~ "Agnostic",
                           religion == 11 ~ "Nothing in Particular",
                           religion == 12 ~ "Something Else", TRUE ~ "All Others")) %>% 
  group_by(year) %>% 
  ct(religpew, wt = weight, show_na = FALSE)



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
  labs(x = "", y = "", title = "Religious Distribution of People Who Say Religion is Not Important", subtitle = "", caption = "") +
  ggsave("D://missing_atheists/images/fig2.png", width = 9, height = 6)