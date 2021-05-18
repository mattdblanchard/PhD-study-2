library(tidyverse)

d <- read_csv("data/item_level/gk_item_level.csv") %>% 
  rename(itemnum = Itemnum)

x <- d %>% 
  select(uid, itemnum, consensus, Ind_Stimulus.ACC, Grp_Stimulus.ACC) %>% 
  group_by(itemnum) %>% 
  mutate(p_agree = mean(consensus == "agree"),
         item.acc = mean(Ind_Stimulus.ACC)) %>% 
  filter(consensus == "agree") %>% 
  group_by(itemnum) %>% 
  mutate(grp_acc = 100*mean(Grp_Stimulus.ACC, na.rm = T),
         ind_acc = 100*mean(Ind_Stimulus.ACC, na.rm = T),
         acc_change = grp_acc - ind_acc,
         grp_change = ifelse(acc_change > 0, "increase",
                             ifelse(acc_change <= 0, "decrease", NA)))


x %>% 
  ggplot(aes(x = p_agree, y = item.acc, colour = grp_change)) +
  geom_point()

# this doesnt work as expected
# round_base(75, 10) and
# round_base(85, 10)
# compute 80
round_base <- function(x, base) {
  base*round(x/base) 
}



my_labs <- labs(y = "Diagnostic accuracy", x = "Confidence")
my_theme <-   theme(panel.background = element_blank(), # Set plot background to white
                    strip.background = element_blank(), legend.key = element_blank())


# individual items
item_order <- d %>% 
  group_by(itemnum) %>% 
  summarise(item.acc = mean(Ind_Stimulus.ACC)) %>% 
  arrange(item.acc) %>% 
  mutate(item.acc = 1:n(),
         item.acc = factor(item.acc))

d %>% 
  # drop_na(Ind_Confidence.RESP) %>% 
  mutate(Ind_Confidence.RESP = round_base(Ind_Confidence.RESP, 25)) %>%
  left_join(item_order, by = "itemnum") %>% 
  group_by(Ind_Confidence.RESP, itemnum) %>% 
  summarise(item.acc = item.acc[1],
            acc = mean(Ind_Stimulus.ACC)) %>% 
  ggplot(aes(x = Ind_Confidence.RESP, y = acc)) +
  geom_point() + geom_line() +
  facet_wrap(~item.acc, nrow = 1) +
  scale_x_continuous(breaks = c(50,100), limits = c(50,100)) +
my_labs +
my_theme

# group items
item_order <- d %>% 
  group_by(itemnum) %>% 
  summarise(item.acc = mean(Grp_Stimulus.ACC)) %>% 
  arrange(item.acc) %>% 
  mutate(item.acc = 1:n(),
         item.acc = factor(item.acc))

d %>% 
  # drop_na(Ind_Confidence.RESP) %>% 
  mutate(Grp_Confidence.RESP = round_base(Grp_Confidence.RESP, 25)) %>%
  left_join(item_order, by = "itemnum") %>% 
  group_by(Grp_Confidence.RESP, itemnum) %>% 
  summarise(item.acc = item.acc[1],
            acc = mean(Grp_Stimulus.ACC)) %>% 
  ggplot(aes(x = Grp_Confidence.RESP, y = acc)) +
  geom_point() + geom_line() +
  facet_wrap(~item.acc, nrow = 1) +
  scale_x_continuous(breaks = c(50,100), limits = c(50,100)) +
  my_labs +
  my_theme


d %>% 
  group_by(itemnum) %>% 
  mutate(support = ifelse(consensus == "agree", 2, 1)) %>% 
  group_by(support, itemnum) %>% 
  summarise(Grp_Confidence.RESP = mean(Grp_Confidence.RESP, na.rm = T)) %>% 
  # mutate(Grp_Confidence.RESP = round_base(Grp_Confidence.RESP, 25)) %>%
  left_join(item_order, by = "itemnum") %>% 
ggplot(aes(x = support, y = Grp_Confidence.RESP)) +
  geom_point() + geom_line() +
  facet_wrap(~item.acc, nrow = 1) +
  scale_x_continuous(breaks = c(1,2), limits = c(1,2)) +
  my_labs +
  my_theme
  

