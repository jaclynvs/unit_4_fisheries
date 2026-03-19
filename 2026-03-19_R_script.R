# 2026-03-19
# JVS

library(tidyverse)

data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3), X2 = c("b1", "b2"))

data1
data2

data1_2_left = left_join(data1, data2)

data1_2_left = data1 %>%  # does the same thing as the line above
  left_join(data2)

data1_2_right = data1 %>%
  right_join(data2)
data1_2_right

data1_2_inner = data1 %>%
  inner_join(data2)
data1_2_inner

data1_2_full = data1 %>%
  full_join(data2)
data1_2_full

data1_2_semi = semi_join(data1, data2)
data1_2_semi

data1_2_anti = anti_join(data1, data2, by = "ID")
data1_2_anti

# data pivots
survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle = c(2, 11, 8, 27),
                    chiton = c(1, 0, 0, 2),
                    mussel = c(0, 1, 1, 4))
survey

long = survey %>%
  pivot_longer(cols = c("barnacle", "chiton", "mussel"), names_to = "taxa", values_to = "count")
long

wide = long %>%
  pivot_wider(names_from = taxa, values_from = count)
wide

# exercise

ggplot(data = long) +
  geom_point(aes(x = quadrat_id, y = count, color = taxa)) +
  xlab("Quadrat ID") +
  ylab("Count") +
  ggtitle("Long Data") +
  theme_bw()


ggplot(data = wide) + 
  geom_point(aes(x = quadrat_id, y = barnacle), color = "red") +
  geom_point(aes(x = quadrat_id, y = chiton), color = "blue") +
  geom_point(aes(x = quadrat_id, y = mussel), color = "green") +
  xlab("Quadrat ID") +
  ylab("Count") +
  ggtitle("Wide Data") +
  theme_bw()

