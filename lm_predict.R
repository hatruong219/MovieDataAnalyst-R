rm(list = ls())

library(ggplot2)
library(dplyr)
library(Hmisc)
library(psych)
movie <- read.csv('Z:\\VKU-Study\\datasetforR\\Cuoiky\\movie_data.csv', na.strings = T)
View(movie)
topDirectors = movie %>%
  subset(director_name != "") %>%
  group_by(director_name) %>%
  subset(mean_score = mean(imdb_score))%>%
  summarise(director_movies_count = n(), meanscore = mean(imdb_score))%>%
  filter(director_movies_count > 1)
View(topDirectors)
count(topDirectors)

library(ggpubr)
library(factoextra)
# Compute k-means with k = 3
data_ss = select(topDirectors, c(2,3))
data_scale = scale(data_ss)
irisdata = dist(data_scale)
fviz_nbclust(data_scale, kmeans, method = "wss")+labs(subtitle = "Ebow")

km.out <- kmeans(data_scale, centers = 3, nstart = 2)
print(km.out)
fviz_cluster(list(data= data_scale, cluster= km.out$cluster))






datamovie <- movie.subset(movie$budget>0)
View(datamovie)

summary(movie$imdb_score)[2][1]
mean(movie$imdb_score)
dim(movie)[2]

judgeAndSplit <- function(x) {
  sp = strsplit(x, '[|]')[[1]][1]
  return(sp)
}
View(movie)
genres_name = as.character(movie$genres) 
newmovie = movie %>%
  mutate(genres1 = as.factor(sapply(genres_name, judgeAndSplit)))
newmovie <- na.omit(newmovie) 
lm_datamovieaa = subset(newmovie, newmovie$country == "USA")
View(lm_datamovieaa)
lm_datamovie <- select(lm_datamovieaa, c(
  color ,
  num_critic_for_reviews,
  duration ,
  num_voted_users,    
  facenumber_in_poster,
  num_user_for_reviews,   
  content_rating ,    
  title_year ,  
  genres1,
  imdb_score
))
View(lm_datamovie)
#View(lm_datamovie)
#dim(lm_datamovie)

lm_mov <- lm(imdb_score ~ ., data=lm_datamovie)
summary(lm_mov)

lm_small <- step(lm_mov, direction = "backward", trace = FALSE)
summary(lm_small)
anova(lm_small)
###
pairs.panels(lm_datamovie, col='red')
View(lm_datamovie)

mov_fh <- data.frame(
    color = "Color",
    num_critic_for_reviews = 302,
    duration = 169,
    num_voted_users= 471220,
    facenumber_in_poster= 0,
    num_user_for_reviews= 1238, 
    content_rating ="PG-13",
    title_year = 2007,
    genres1="Action"
  )

prediction_TFH <- predict(lm_small, newdata=mov_fh, interval="confidence")
prediction_TFH

