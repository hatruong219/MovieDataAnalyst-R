rm(list = ls())

library(ggplot2)
library(dplyr)
library(Hmisc)
library(psych)
movie <- read.csv('Z:\\VKU-Study\\datasetforR\\Cuoiky\\movie_data.csv', stringsAsFactors = F)
str(movie)
names(movie)
c(names(movie))
edit(movie)
dim(movie)
summary(movie)
head(movie, 15)
View(movie)
q5_data = movie
q5_cleaned_data = q5_data %>%
  subset(!is.na(gross)) %>%
  subset(!is.na(budget)) 
q5_cleaned_data$imdb_score_level <- round(q5_cleaned_data$imdb_score, 1)
View(q5_cleaned_data)

# 
judgeAndSplit <- function(x) {
  sp = strsplit(x, '[|]')[[1]][1]
  return(sp)
}
genres_name = as.character(movie$genres) 
movie = movie %>%
  mutate(genres1 = as.factor(sapply(genres_name, judgeAndSplit)))
View(movie)
typeLabelCount = movie %>%
  group_by(genres1) %>%
  summarise(count = n()) %>%
  as.data.frame()

movie %>%
  ggplot(aes(reorder(genres1, imdb_score, median, order = TRUE), y = imdb_score, fill = genres1)) + 
  geom_boxplot() + 
  coord_flip() + 
  geom_label(data = typeLabelCount, aes(x = genres1, y = 10, label = count),  hjust = 0, size = 3) + 
  ggtitle("Ordered imdb scores distribution by popular movie genres") + 
  guides(fill=FALSE) + 
  ylim(0, 11) +
  labs(x = "Popular movie genre", y = "IMDB score")






#start
newmovie <- na.omit(movie) 
summary(newmovie)
dim(newmovie)


newmovie1<-subset(newmovie, budget > 1)
dim(newmovie1)
View(newmovie1)
newmovie1$coloi <- 0
newmovie1$coloi[newmovie1$gross - newmovie1$budget > 0] <- 1

head(newmovie1)
attach(newmovie1)
mean(newmovie1$duration)
library(caTools)
set.seed(100)
split = sample.split(newmovie1$coloi, SplitRatio =0.7)
mauxaydung = subset(newmovie1, split==TRUE)
maukiemthu = subset(newmovie1, split==FALSE)

temp = glm(coloi ~ .,data = mauxaydung, family =binomial)
summary(temp)
search = step(temp)
search


















### Always start from the distribution of the data. 
ggplot(aes(x = num_critic_for_reviews), data = movie) + geom_histogram(bins = 20, color = 'white') + ggtitle('Histogram of Number of reviews')
summary(movie$num_critic_for_reviews)
#The distribution of the number of reviews is right skewed. Among these 5043 movies, the minimum number of review was 1 and the maximum number of reviews was 813. Majority of the movies received less than 200 reviews. 

ggplot(aes(x = imdb_score), data = movie) + geom_histogram(bins = 20, color = 'white') + ggtitle('Histogram of Scores')
summary(movie$imdb_score)
#The score distribution is left skewed, with minimum score at 1.60 and maximum score at 9.50.
ggplot(aes(x = title_year), data = movie) + geom_histogram(color='white') +
  ggtitle('Histogram of Title Year')
#Most of the movies in the dataset were produced after 2000.
boxplot(imdb_score ~ title_year, data=movie, col='indianred')
title("IMDB score vs Title year")
#However, the movies with the highest scores were produced in the 1950s, and there have been significant amount of low score movies came out in the recent years. 
### Which countries produced the most movies and which countries have the highest scores?
country_group <- group_by(movie, country)
movie_by_country <- summarise(country_group,
                              mean_score = mean(imdb_score),
                              n = n()) 

newdata <- movie_by_country[order(movie_by_country$mean_score),]
View(movie_by_country)



ggplot(aes(x = country, y = n, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=6)) +
  coord_flip() + ggtitle('Countries vs Number of Movies')
#The USA produced the most number of movies. 
ggplot(aes(x = country, y = mean_score, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=7)) +
  coord_flip() + ggtitle('Countries vs IMDB Scores')
#But that does not mean their movie are all good quality. Kyrgyzstan, Libya and United Arab Emirates might have the highest average scores.

### How about directors?
director_group <- group_by(movie, director_name, genres)
movie_by_director <- summarise(director_group,
                               mean_score = mean(imdb_score))
movie_by_director <- movie_by_director[67:4530,]
movie_by_director <- movie_by_director[with(movie_by_director, order(-mean_score)), ]
movie_by_director <- head(movie_by_director, 20)
ggplot(aes(x = mean_score, y = director_name), data = movie_by_director) +
  geom_point(aes(color = genres), size = 2) + xlab("Mean Score") + 
  ylab("Director Name")+ theme_minimal() + ggtitle('Director, Genres & Scores')
### Multiple Linear Regression - Variable Selection
#Time to do something serious work, I intend to predict IMDB scores from the other variables using multiple linear regression model. Because regression can't deal with missing values, I will eliminate all missing values.  
movie$imdb_score <- as.numeric(impute(movie$imdb_score, mean))
movie$num_critic_for_reviews <- as.numeric(impute(movie$num_critic_for_reviews, mean))
movie$duration <- as.numeric(impute(movie$duration, mean))
movie$director_facebook_likes <- as.numeric(impute(movie$director_facebook_likes, mean))
movie$actor_3_facebook_likes <- as.numeric(impute(movie$actor_3_facebook_likes, mean))
movie$actor_1_facebook_likes <- as.numeric(impute(movie$actor_1_facebook_likes, mean))
movie$gross <- as.numeric(impute(movie$gross, mean))
movie$cast_total_facebook_likes <- as.numeric(impute(movie$cast_total_facebook_likes, mean))
movie$facenumber_in_poster <- as.numeric(impute(movie$facenumber_in_poster, mean))
movie$budget <- as.numeric(impute(movie$budget, mean))
movie$title_year <- as.numeric(impute(movie$title_year, median))
movie$actor_2_facebook_likes <- as.numeric(impute(movie$actor_2_facebook_likes, mean))
movie$aspect_ratio <- as.numeric(impute(movie$aspect_ratio, mean))
summary(movie)
##
movie_sub <- subset(movie, select = c(num_critic_for_reviews, duration, director_facebook_likes, actor_1_facebook_likes, gross, cast_total_facebook_likes, facenumber_in_poster, budget, movie_facebook_likes, imdb_score))
pairs.panels(movie_sub, col='red')
### Construct the model

#Split data into training and testing.
set.seed(2017)
train_size <- 0.8 
train_index <- sample.int(length(movie_sub$imdb_score), length(movie_sub$imdb_score) * train_size)
train_sample <- movie_sub[train_index,]
test_sample <- movie_sub[-train_index,]
### Fit the model 

#I am trying out a stepwise selection of variables by backwards elimination. So I start with all candidate varibles and elimiate one at a time.
fit <- lm(imdb_score ~ num_critic_for_reviews + duration +    director_facebook_likes + actor_1_facebook_likes + gross + cast_total_facebook_likes + facenumber_in_poster + budget + movie_facebook_likes, data=train_sample)
summary(fit) 
#I am going to eliminate the variables that has little value, - gross and budget, one at a time, and fit it again.
fit <- lm(imdb_score ~ num_critic_for_reviews + duration + budget +   director_facebook_likes + actor_1_facebook_likes + cast_total_facebook_likes + facenumber_in_poster + movie_facebook_likes, data=train_sample)
summary(fit) 
fit <- lm(imdb_score ~ num_critic_for_reviews + duration +   director_facebook_likes + actor_1_facebook_likes + cast_total_facebook_likes + facenumber_in_poster + movie_facebook_likes, data=train_sample)
summary(fit) 
#From the fitted model, I find that the model is significant since the p-value is very small. The "cast_total_facebook_likes" and "facenumber_in_poster" has negative weight. This model has multiple R-squared score of 0.143, meaning that around 14.3% of the variability can be explained by this model.

#Let me make a few plots of the model I arrived at.
plot(fit)

train_sample$pred_score <- predict(fit, newdata = subset(train_sample, select=c(imdb_score, num_critic_for_reviews, duration, director_facebook_likes, actor_1_facebook_likes, cast_total_facebook_likes, facenumber_in_poster, movie_facebook_likes)))
test_sample$pred_score <- predict(fit, newdata = subset(test_sample, select=c(imdb_score, num_critic_for_reviews, duration, director_facebook_likes, actor_1_facebook_likes, cast_total_facebook_likes, facenumber_in_poster, movie_facebook_likes)))
#The theoretical model performance is defined here as R-Squared

summary(fit)


#Check how good the model is on the training set.
train_corr <- round(cor(train_sample$pred_score, train_sample$imdb_score), 2)
train_rmse <- round(sqrt(mean((train_sample$pred_score - train_sample$imdb_score)^2)))
train_mae <- round(mean(abs(train_sample$pred_score - train_sample$imdb_score)))
c(train_corr^2, train_rmse, train_mae)

#The correlation between predicted score and actual score for the training set is 14.44%, which is cery close to theoretical R-Squared for the model, this is good news. However, on average, on the set of the observations I have previously seen, I am going to make 1 score difference when estimating. 

#Check how good the model is on the test set.
test_corr <- round(cor(test_sample$pred_score, test_sample$imdb_score), 2)
test_rmse <- round(sqrt(mean((test_sample$pred_score - test_sample$imdb_score)^2)))
test_mae <- round(mean(abs(test_sample$pred_score - test_sample$imdb_score)))
c(test_corr^2, test_rmse, test_mae)

