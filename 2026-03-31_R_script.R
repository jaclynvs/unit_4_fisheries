# 2026-03-31
# JVS

library(tidyverse)
library(palmerpenguins)

head(penguins)

# PCAs
# need to remove all NAs and categorical values (model can only take numerical values)

pen_drop_na = penguins %>%
  drop_na()

summary(pen_drop_na)
pen_num = pen_drop_na %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)

# run our PCA

pen_pca = prcomp(pen_num, scale. = T, center = T)
class(pen_pca)
str(pen_pca)
summary(pen_pca)
head(pen_pca$x)
dim(pen_pca$x)
dim(pen_num)

summary(pen_pca)$importance[2,]
pen_pca$sdev
pen_pca$sdev^2 / sum(pen_pca$sdev^2)

plot(pen_pca)

# plot a scree plot for our PCA

pca_scree = data.frame(pc = seq(1,4), var = pen_pca$sdev^2 / sum(pen_pca$sdev^2))

ggplot(data = pca_scree, aes(x = pc, y = var)) +
  geom_bar(stat = "identity") +
  geom_point() +
  geom_line() +
  xlab("Principal Component") +
  ylab("Proportion of variance explained")

pen_pca$rotation
pen_pca$x

pen_pca_meta = cbind(pen_pca$x, pen_meta)
head(pen_pca_meta)

ggplot(data = pen_pca_meta) +
  geom_point(aes(x = PC1, y = PC2, color = species, shape = sex)) +
  coord_fixed(ratio = 1)

install.packages("ggbiplot")
library(ggbiplot)

biplot(pen_pca)
ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, ellipse = T, alpha = 0) +
  geom_point(data = pen_pca_meta, aes(x = PC1, y = PC2, color = species, shape = sex)) +
  theme_bw()

pen_pca$rotation
