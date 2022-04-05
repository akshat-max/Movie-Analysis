# The dataset has been taken from kaggle.
# Dataset Description - IMDB 5000   Dataset

# 1 Data Description
#   The dataset is from kaggle. 
# 1.1 Problem Statement

#   Based on the massive movie information, it would be interesting to understand what are the important factors 
#   that make a movie more successful than others. So, we would like to analyze what kind of movies are grossing
#   more and getting higher profits. We also want to show the results of this analysis in an intuitive way by 
#   visualizing outcome using ggplot2 in R.

# 2 Data Exploration

# 2.1 Load Data

# Load packages
library(ggplot2) # visualization
library(ggrepel) # contains extra geoms for ggplot2
library(ggthemes) # visualization
library(data.table) # data.frame
library(dplyr) # data manipulation
library(stringr) # character manipulation
library(readr) # read data

IMDB_Movies <- read_csv("movie_metadata.csv")
View(IMDB_Movies)
head(IMDB_Movies)
summary(IMDB_Movies)

#   We have 5043 observations of 28 variables. The response variable is "gross" and other predictor variables 
#   contain character, strings and numerical values.

# 2.2 Remove Duplicates

#   In this data, we will check for duplicate rows and delete them.

#   checking for duplicate rows
sum(duplicated(IMDB_Movies))
#   delete duplicate rows
IMDB_Movies <- IMDB_Movies[!duplicated(IMDB_Movies), ]

#   We have 4998 rows left.
#   We are working on effect of other attributes on gross of movie. The gross of movie is numeric variable

# 2.3 Remove rows containing NA values
complete.cases(IMDB_Movies)
which(complete.cases(IMDB_Movies))
#   storing rows containing NA values in new vector
no_NA <- which(!complete.cases(IMDB_Movies))
no_NA
#   removing rows from dataset
IMDB_Movies <- IMDB_Movies[-no_NA, ]

#   3,719 rows left

# 2.4 Split Genres

#   we want to see relationship between genres and gross as one movie is having multiple genres it is best to
#   if they are having any impact on gross or we can remove genres attribute.

genre_type <- IMDB_Movies %>% select(movie_title,genres,contains('name'))
genre_type <- data.frame(lapply(genre_type, as.character), stringsAsFactors=FALSE)

#   Separating our Genre variable
library(reshape)
break_genre <- colsplit(IMDB_Movies$genres,split="\\|",names=c("n1","n2","n3","n4","n5","n6","n7","n8"))
break_genre <- data.frame(lapply(break_genre, as.character), stringsAsFactors=FALSE)

for (i in 1: nrow(break_genre)) { 
  break_genre[i,duplicated(as.character(break_genre[i,]))] <- ""
}

#   In the meanwhile lets just look at genre based movie gross from boxplot visualization.

gross_in_million <- IMDB_Movies$gross/1000000
IMDB_Movies <- mutate(IMDB_Movies, gross_in_million)
movie_gross <- IMDB_Movies %>% select (movie_title,gross_in_million) 
genre_gross <- cbind(movie_gross,break_genre)
genre_gross <- melt(genre_gross,id.vars=1:2)
genre_gross <- genre_gross[-3]
genre_gross <- genre_gross %>% filter(value!="") %>% droplevels() #filtering out unknown genres

ggplot(aes(y = gross_in_million, x = value, fill=value), data=genre_gross) + geom_boxplot() + theme(axis.text.x = element_text(angle=70,hjust=1))

#   As we can see from the plot the Animation and Adventure genre movies are the highest grossing movies

# 3 Redundant Data Cleaning

# 3.1.1 Analyze aspect ratio

table(IMDB_Movies$aspect_ratio)

#   The most common aspect ratios are 1.85 and 2.35. For analyzing purpose, we group other ratios together.

#   In order to compute the mean of gross for different aspect_ratio.

mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio == 1.85])
mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio == 2.35])
mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio != 1.85 & IMDB_Movies$aspect_ratio != 2.35])

#   From the means of gross for different aspect ratios, we can see there is not much difference.
#   For aspect ratio = 1.85, average gross is 44 Million$.
#   For aspect ratio = 2.35, average gross is 58 Million$.
#   Combining both ratios average is 51 Million$.

IMDB_Movies <- subset(IMDB_Movies, select = -c(aspect_ratio))

# 3.1.2 Sort out content ratings

#   According to the history of naming these different content ratings, we find M = GP = PG, 
#   X = NC-17. We want to replace M and GP with PG, replace X with NC-17, because these two are what we 
#   use nowadays.

#   assign 'PG' rating to 'M' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'M']   <- 'PG'
#   assign 'PG' rating to 'GP'
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'GP']  <- 'PG' 
#   assign 'NC-17' rating to 'X'
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'X']   <- 'NC-17'

#   We want to replace "Approved", "Not Rated", "Passed", "Unrated" with the most common rating "R".

IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Approved']  <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Not Rated'] <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Passed']    <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Unrated']   <- 'R' 
#   convert character to factor
IMDB_Movies$content_rating <- factor(IMDB_Movies$content_rating)
table(IMDB_Movies$content_rating)

#   Now we only have 5 different content ratings.

# 3.2 Add Columns

#   We have gross and budget information. So let's add two colums: profit and percentage return on 
#   investment for further analysis.

IMDB_Movies <- IMDB_Movies %>% mutate(profit = gross - budget, return_on_investment_perc = (profit/budget)*100)

# 3.3 Remove Columns

# 3.3.1 Is the color of a movie influential?

table(IMDB_Movies$color)

#   More than 96% movies are colored, which indicates that this predictor is nearly constant. 
#   Let's remove this predictor.

#   delete attribute color
IMDB_Movies <- subset(IMDB_Movies, select = -c(color))

# 3.3.2 Is language an important factor for imdb score? What about country?

table(IMDB_Movies$language)

#   Out of 3,719 movies 3,566 are in English, which indicates this predictor is nearly constant.

IMDB_Movies <- subset(IMDB_Movies, select = -c(language))

# 3.3.3 Let's take a look at predictor country.

table(IMDB_Movies$country)

#   Around 79% movies are from USA, 8% from UK, 13% from other countries. So we group other countries 
#   together to make this categorical variable with less levels: USA, UK, Others.

#   create a new vector having country names and character 'Others'
levels(IMDB_Movies$country) <- c(levels(IMDB_Movies$country), "Others")
IMDB_Movies$country[(IMDB_Movies$country != 'USA') & (IMDB_Movies$country != 'UK')] <- 'Others'
IMDB_Movies$country <- factor(IMDB_Movies$country)
table(IMDB_Movies$country)

# 4 Data Visualization

# 4.1 Top 20 grossing movies

IMDB_Movies %>% arrange(desc(gross_in_million)) %>% top_n(20, gross) %>%
  ggplot(aes(x=budget/1000000, y=gross_in_million)) + geom_point() + geom_smooth() +
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget in million$", y = "Gross in million$", title = "Top 20 Grossing Movies") +
  theme(plot.title = element_text(hjust = 0.5))

# 4.2 Top 20 profitable movies

IMDB_Movies %>% arrange(desc(profit)) %>% top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y=profit/1000000)) + geom_point() + geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget in million$", y = "Profit in million$", title = "Top 20 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

#   These are the top 20 movies based on the Profit earned. It can be inferred from this plot that high 
#   budget movies tend to earn more profit but there are some exceptions like Star Wars: Episode IV and 
#   E.T. the Extra-Terrestrial which have made huge profit on small budget.

# 4.3 Top 20 movies on its Return on Investment

IMDB_Movies %>% arrange(desc(profit)) %>% top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point() + geom_smooth() + geom_text_repel(aes(label = movie_title)) + 
  labs(x = "Budget in million$", y = "Percent Return on Investment", title = "20 Most Profitable Movies based on its Return on Investment") +
  theme(plot.title = element_text(hjust = 0.5))

#   These are top 20 movies based on the percentage return on investment. We conclude from this plot that movies
#   made on a high budget are low on returns percentage.

# 4.4 Top 20 directors with highest grossing movies

library(formattable)
IMDB_Movies %>% group_by(director_name) %>% summarise(Average_Gross = mean(gross_in_million)) %>% 
  arrange(desc(Average_Gross)) %>% top_n(20, Average_Gross) %>% formattable(list(Average_Gross = 
  color_bar("orange")), align = 'l')

# 4.5 Effect of imdb_score on gross

plot(IMDB_Movies$imdb_score,IMDB_Movies$gross,xlab="IMDb Rating", ylab="Gross in Million$", type = "h",
     main = "Plot of elationship between IMDb Rating and Gross") 
  

  #   This is an analysis on Average Gross earnings by movies for a particular imdb score.

#   The highest grossing movies are int the range of imdb_score range of 6-9.

# 4.6 Relation between number of facebook likes and gross

library(plotly)
IMDB_Movies %>% plot_ly(x = ~movie_facebook_likes, y = ~gross, mode = "markers", alpha = 0.7, type = "scatter")


#   Most of the points are on the left bottom of the plot emphasising that facebook likes do not have very
#   much impact on movie gross.

# 4.7 Relation between number of voted users and gross

IMDB_Movies %>% plot_ly(x = ~num_voted_users, y = ~gross, mode = "markers", alpha = 0.7, type = "scatter")

#   Most of the movies in the range 500,000 user votes are grossing around 200 million$ or less.

# 4.8 Relation between content_rating and gross

IMDB_Movies %>% group_by(content_rating) %>% summarise(Average_Gross = mean(gross)) %>% 
  formattable(list(Average_Gross = color_bar("green")), align = 'l')

#   Movies with content rating "G" are highest grossing because any age group can see them. 

