options(device = "quartz")
library(tidyverse)

theme_set(theme_classic())

palmer <- read_csv(here::here("data/palmer_penguins.csv"))

ggplot(palmer, aes(species, bill_length_mm, fill=species))+
  geom_boxplot()

ggplot(palmer, aes(species, body_mass_g, fill=species))+
  geom_boxplot()+
  labs(
    title="Penguins",
    subtitle="Species vs Boddy Mass",
    caption="by @mschmidty | Source: Gorman et. al.", 
    y = "Body Mass (g)",
    x = "",
    fill = "Species"
  )

palmer%>%
  ggplot(aes(body_mass_g, bill_length_mm, color = species, shape = sex))+
  geom_point(size = 3)+
  scale_color_brewer(palette = "Dark2")

## Logistic Regression - introduction to machine learning.
set.seed(123)
library(nnet)
palmer_cl<-palmer%>%
  na.omit()

log_reg<-multinom(species~island+bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g+sex, data=palmer_cl)

summary(log_reg)

dt <- sort(sample(nrow(palmer_cl), nrow(palmer_cl)*.7))

train<-palmer_cl[dt,]
test <- palmer_cl[-dt,]

log_reg2<-multinom(species~island+bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g+sex, data=train)

test%>%
  bind_cols(predict=predict(log_reg2, test))%>%
  select(species, predict)%>%
  View()

