library(rvest)
library(tidyverse)
library(tidytext)
library(wordcloud)

### test for update

#####################
## data collection ##
#####################

# the url with time range and rank constraints
url_imdb <- 'https://www.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2020-12-31&sort=num_votes,desc&start='

# extract the data with multiple pages
# the indices are start pages, 50 items ahead
index <- seq(1, 451, by = 50)
# used to deal with untidy years
even <- seq(2,100,2)
imdb <- NULL
for (i in index) {
  url <- paste0(url_imdb, toString(i))
  rank <- url %>%
    read_html() %>%
    html_nodes('.text-primary') %>%
    html_text()
  title <- url %>%
    read_html() %>%
    html_nodes('.lister-item-header a') %>%
    html_text()
  rating <- url %>%
    read_html() %>%
    html_nodes('.ratings-imdb-rating strong') %>%
    html_text()
  # year associated with rank, need cleaning
  year_untidy <- url %>%
    read_html() %>%
    html_nodes('.unbold') %>%
    html_text()
  year <- year_untidy[even]
  runTime <- url %>%
    read_html() %>%
    html_nodes('.runtime') %>%
    html_text()
  genre <- url %>%
    read_html() %>%
    html_nodes('.genre') %>%
    html_text()
  votes <- url %>%
    read_html() %>%
    html_nodes('.sort-num_votes-visible span:nth-child(2)') %>%
    html_text()
  gross <- url %>%
    read_html() %>%
    html_nodes('.ghost~ .text-muted+ span') %>%
    html_text()
  staff <- url %>%
    read_html() %>%
    html_nodes('.text-muted+ p') %>%
    html_text()
  description <- url %>%
    read_html() %>%
    html_nodes('.ratings-bar+ .text-muted') %>%
    html_text()
  imdb_temp <- cbind(rank, title, rating, year, runTime, genre, votes, gross, staff, description) %>%
    as.data.frame()
  imdb <- rbind(imdb, imdb_temp)
}


write.csv(imdb, file = 'imdb.csv', row.names = FALSE)



url_numbers <- 'https://www.the-numbers.com/movie/budgets/all/'

# blocked by anti-scrapying mechanism
# index <- seq(1, 6001, 100)
# 
# numbers <- NULL
# for (i in index) {
#   url <- paste0(url_numbers, toString(i))
#   numbers_temp <- url_numbers %>%
#     read_html() %>%
#     html_nodes('table') %>%
#     html_table(fill = TRUE) %>%
#     .[[1]]
#   numbers <- rbind(numbers, numbers_temp)
# }
# 
# write.csv(numbers, file = '', row.names = FALSE)


# manually copy the data and import
numbers <- read_tsv('D:/Desktop/numbers.txt')
numbers <- rbind(names(numbers), numbers)
numbers <- numbers[,-1]
names(numbers) <- c('releaseDate', 'title', 'budget', 'domesticGross', 'worldwideGross')


###################
## data cleaning ##
###################

# imdb
imdb$rank <- 1:nrow(imdb)
imdb$title <- imdb$title %>% as.character()
imdb$rating <- imdb$rating %>% as.character() %>% as.numeric()
# part of the title is in the column year
imdb$year <- imdb$year %>% as.character()
title_index <- str_detect(imdb$year, '\\(I\\)')
imdb$title[title_index] <- imdb$title[title_index] %>%
  paste0(' 1')
title_index <- str_detect(imdb$year, '\\(II\\)')
imdb$title[title_index] <- imdb$title[title_index] %>%
  paste0(' 2')
title_index <- str_detect(imdb$year, '\\(IX\\)')
imdb$title[title_index] <- imdb$title[title_index] %>%
  paste0(' 9')

imdb$year <- imdb$year %>% 
  str_remove_all('\\(I\\)|\\(II\\)|\\(IX\\)|\\(|\\)') %>%
  as.numeric()

imdb$runTime <- imdb$runTime %>%
  as.character() %>%
  str_remove_all('min') %>%
  as.numeric()
imdb$votes <- imdb$votes %>%
  str_remove_all(',') %>%
  as.numeric()
imdb$gross <- imdb$gross %>%
  as.character() %>%
  str_remove_all('M')
imdb$gross <- imdb$gross %>%
  str_sub(2,) %>% as.numeric()
imdb$genre <- imdb$genre %>%
  as.character() %>%
  str_remove_all('\n') %>%
  str_trim()
# multiple genres, keep the first one
imdb$genre <- gsub(',.*', '', imdb$genre)
# keep the first director
imdb$director <- gsub(',.*','',imdb$director)


# separate the staff to director and stars
imdb$staff <- imdb$staff %>% 
  str_replace_all('\n','') %>% 
  str_trim(side = 'both')

imdb$staff <- imdb$staff %>% as.character()
imdb$description <- imdb$description %>%
  str_replace_all('\n', '') %>% 
  str_trim()

imdb <- imdb %>% separate(col = staff, into = c('director', 'stars'), sep = '\\|')
imdb$director <- imdb$director %>%
  str_remove_all('Director:|Directors:') %>% 
  str_trim()
imdb$stars <- imdb$stars %>%
  str_remove_all('Stars:') %>% 
  str_trim()


# numbers
numbers$budget <- numbers$budget %>%
  str_remove_all(',') %>%
  str_remove_all('\\$') %>%
  as.numeric()
numbers$domesticGross <- numbers$domesticGross %>%
  str_remove_all(',') %>%
  str_remove_all('\\$') %>%
  as.numeric()
numbers$worldwideGross <- numbers$worldwideGross %>%
  str_remove_all(',') %>%
  str_remove_all('\\$') %>%
  as.numeric()

# scale the unit in numbers to be consistent with imdb
numbers[,3:5] <- numbers[,3:5]/1000000 %>% round(2)

write.csv(imdb, 'imdb_tidy.csv', row.names = F)
write.csv(numbers, 'numbers_tidy.csv', row.names = F)


###############
## analyzing ##
###############

# the rating of different genres
ggplot(data = imdb) + 
  geom_boxplot(aes(genre, rating)) + 
  ggtitle('Ratings of Different Genres') +
  theme(plot.title = element_text(hjust = 0.5))

# ratings over year
ggplot(data = imdb %>% mutate(years = year %>% as.character())) + 
  geom_boxplot(aes(years, rating)) + 
  ggtitle('Ratings over Years') +
  theme(plot.title = element_text(hjust = 0.5))

# votes indicate popularity
ggplot(data = imdb) + 
  geom_boxplot(aes(genre, votes)) + 
  ggtitle('Votes for Different Genres') +
  theme(plot.title = element_text(hjust = 0.5))

# run time and rating affect gross
ggplot(data = imdb,aes(runTime,rating)) +
  geom_point(aes(size=gross,col=genre)) +
  ggtitle('How Run Time and Rating affect Gross') +
  theme(plot.title = element_text(hjust = 0.5))


# will high investment improve the gross?
numbers[-2300,] %>%  # remove outlier
  gather(`domesticGross`:`worldwideGross`,key = 'region', value = gross) %>%
  ggplot() + geom_point(aes(budget, gross, color = region)) +
  ggtitle('Budget against Gross') + 
  theme(plot.title = element_text(hjust = 0.5))


# famous actors and directors
directors <- table(imdb$director) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1:10,]
names(directors) <- c('Director','Number of Show-ups')
directors

actor_list <- NULL
for (i in 1:500) {
  actor <- imdb$stars[i] %>%
    str_split(',') %>%
    unlist()
  actor_list <- c(actor_list, actor)
}
actors <- table(actor_list) %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  .[1:10,]
names(actors) <- c('Actor', 'Number of Show-ups')
actors

# wordcloud for description
data('stop_words')
descrip <- imdb %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  count(word, sort = T)

descrip[1:10,] %>% with(wordcloud(word,n))

# find hot movies
hot_movies <- imdb %>%
  filter(rating >= 7.8) %>%
  filter(gross >= 360)
ggplot(data = imdb, aes(rating, gross, color = genre, label = title)) + geom_point() +
  geom_text(aes(label = title), data = hot_movies, size = 3)



