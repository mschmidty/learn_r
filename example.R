library(tidyverse)

palmer_p<-read_csv("C:\\Users\\mschmidt\\Downloads\\palmer_penguins.csv")

palmer_p

View(palmer_p)


smaller_palmer_p<-select(palmer_p, species, island, flipper_length_mm, sex)

filter(
  smaller_palmer_p, 
  sex=="female" | is.na(sex) & species=="Adelie"
  )

mutate(
  smaller_palmer_p, 
  new_column = "This is new content"
  )

View(mutate(palmer_p,
       flipper_prod_body_mass = flipper_length_mm*body_mass_g,
       ratio_bass_to_flipper = flipper_length_mm/body_mass_g))

palmer_p

grouped_palmers<-group_by(palmer_p, species)

summarize(grouped_palmers, body_mass_avg = mean(body_mass_g, na.rm=T))

palmer_p%>%
  mutate(flipper_prod_body_mass = flipper_length_mm*body_mass_g)%>%
  filter(flipper_prod_body_mass >=700000)

new_df<-mutate(palmer_p, flipper_prod_body_mass = flipper_length_mm*body_mass_g)

filter(new_df, flipper_prod_body_mass >=700000)

palmer_p%>%
  filter(!is.na(sex))%>%
  group_by(sex, species)%>%
  summarize(avg_body_mass = mean(body_mass_g, na.rm=TRUE))

palmer_p%>%
  ggplot(aes(species, body_mass_g, fill=species))+
  geom_boxplot()+
  labs(
    title="Body Mass by Species", 
    y="Body Mass (g)",
    x="", 
    caption = "by Mike Schmidt | Data: Palmer Penguins", 
    fill="Species"
  )

theme_set(theme_minimal())

palmer_p%>%
  ggplot(aes(body_mass_g, bill_length_mm, color=species))+
  geom_point(size=3)+
  scale_color_brewer(palette = "Dark2") +
  stat_smooth(method = "lm")

m1<-lm(body_mass_g~bill_length_mm, data=palmer_p)

summary(m1)

library(tidyverse)

select() # select columns
filter() # select rows
mutate() # add new columns and/or change existing comments
group_by()%>%
  summarize() # get summary statistics e.g.(mean, sum, min, max)

