rm(list = ls())

library(ggplot2)
library(dplyr)
library(Hmisc)
library(psych)
movie <- read.csv('Z:\\VKU-Study\\datasetforR\\Cuoiky\\movie_data.csv', stringsAsFactors = F)
str(movie)
View(movie)
judgeAndSplit <- function(x) {
  sp = strsplit(x, '[|]')[[1]][1]
  return(sp)
}
genres_name = as.character(movie$genres) 
newmovie = movie %>%
  mutate(genres1 = as.factor(sapply(genres_name, judgeAndSplit)))

newmovie <- na.omit(newmovie) 

newmovie<-subset(newmovie, budget > 1)

newmovie$coloi <- 0
newmovie$coloi[newmovie$gross - newmovie$budget > 0] <- 1
newmovie <- na.omit(newmovie)
View(newmovie)

lgm_datamovieaa = subset(newmovie, newmovie$country == "USA")
lgm_datamovie <- select(lgm_datamovieaa, c(-director_facebook_likes, 
                                         -actor_1_facebook_likes, 
                                         -actor_2_facebook_likes, 
                                         -cast_total_facebook_likes, 
                                         -movie_imdb_link, 
                                         -plot_keywords,
                                         -movie_facebook_likes,
                                         -actor_1_name,
                                         -actor_2_name,
                                         -genres,
                                         -movie_title,
                                         -director_name,
                                         -budget,
                                         -country,-language))
View(lgm_datamovie)

lgm_predict <- glm(coloi ~ ., data=lgm_datamovie, family='binomial')
summary(lgm_predict)

search = step(lgm_predict)
search

lgm_small <- step(lgm_predict, direction = "backward", trace = FALSE)

summary(lgm_small)
anova(lgm_small)
lgm_small


mov_fh <- data.frame(
  num_critic_for_reviews = 302,
  duration = 169,
  gross = 309404152,
  num_voted_users= 471220,
  facenumber_in_poster= 0,
  num_user_for_reviews= 1238, 
  content_rating ="PG-13",
  title_year = 2007,
  imdb_score = 7.1,
  aspect_ratio = 2.35,
  genres1="Action"
)

mov_fh0 <- data.frame(
  num_critic_for_reviews = 450,
  duration = 150,
  gross = 89289910,
  num_voted_users= 181792,
  facenumber_in_poster= 1,
  num_user_for_reviews= 711, 
  content_rating ="PG-13",
  title_year = 2013,
  imdb_score = 6.5,
  aspect_ratio = 2.35,
  genres1="Action"
)

mov_fh01 <- data.frame(
  num_critic_for_reviews = 322,
  duration = 122,
  gross = 130468626,
  num_voted_users= 53607,
  facenumber_in_poster= 4,
  num_user_for_reviews= 432, 
  content_rating ="PG-13",
  title_year = 2016,
  imdb_score = 7.5,
  aspect_ratio = 2.35,
  genres1="Action"
)

prediction_TFH <- predict(lgm_small, newdata=mov_fh, type = "response")
prediction_TFH

prediction_TFH <- predict(lgm_small, type = "response", newdata = lgm_datamovieaa)

table(lgm_datamovieaa$coloi,prediction_TFH>0.5)

