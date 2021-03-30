# ~~~ Practice w starwars by dplyr ~~~ #

setwd("C:/Users/LENOVO/Documents/Mestrado_DASS_Micromaster/Module_7")
library(dplyr, ggplot2)
library(plotly)


df <- starwars
best <- c("Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Organa",
          "Obi-Wan Kenobi", "Anakin Skywalker", "Chewbacca", "Han Solo",
          "Jabba Desilijic Tiure", "Yoda")

# 1. start with a scatterplot

g_age_mass <- ggplot(df, aes(x=birth_year, y=mass, label=name)) +
  geom_point(size=0.5, colour="#5F815E") +
 #geom_text(aes(label=ifelse(birth_year>=200,as.character(name),'')),hjust=-0.05,vjust=-0.5) +
  xlim(0,1000) + ylim(0,1500) +
  xlabel(Idade) + ylabel(Peso) +
  theme_minimal()

p_age_mass <- ggplotly(g_age_mass)

#----- arrange data ----#
library(tidyr, purrr)
df2 <- df

df2 <- df2 %>%
  mutate(films = map(films, toString)) %>% #unlist variable
  separate(films, sep = ",",
           into = c("film", "film1", "film2", "film3", "film4", "film5", "film6"),
           convert = TRUE) #separate lists into columns

df2 <- gather(df2,"columns","films", 12:18)
df2$columns <- NULL

# 2. characters by films
char_films <- df2 %>% group_by(films, name) %>%  summarise(n = n()) %>% summarise(n = sum(n))
char_films <- char_films[-14,]

ggplot(char_films, aes(reorder(films, n), n)) +
  geom_bar(stat='identity', fill="#5F815E") + coord_flip() +
  xlab('Filmes') + ylab('N?mero de personagens') +
  theme_minimal()

# 3. PCA
df$height[is.na(df$height)] <- 0
df$mass[is.na(df$mass)] <- 0
df$birth_year[is.na(df$birth_year)] <- 0

starwars.pca <- prcomp(df[,c(2:3,7)], center = TRUE,scale. = TRUE)
summary(starwars.pca)
pca <- autoplot(starwars.pca, data = df, colour = 'gender',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)

