library(rtweet)
library(tmaptools)
library(leaflet)
library(tidyverse)
library(dplyr)
library(tidygeocoder)
library(tidytext)
library(textdata)
library(plotly)
library(ggplot2)
library(tidyquant)
library(wordcloud)
library(ggwordcloud)

token <-create_token(
  app = "app",
  consumer_key= 'consumer_key',
  consumer_secret= 'consumer_secret',
  access_token = 'access_token',
  access_secret = 'access_secret',
  set_renv = TRUE
)

rt<-rtweet::search_tweets(
  q = '#covid19',
  n = 100,
  lang = 'en',
  include_rts = FALSE
)

rt %>% slice(1:5) %>% select(screen_name, location, description) 
rt %>% slice(1:5) %>% select(text,url) 
rt %>% slice(1:5) %>% select(hashtags) %>% unnest_wider(hashtags)
rt %>% slice(1:5) %>% select(urls_expanded_url) %>% unnest_wider(urls_expanded_url)

rt1<-stream_tweets(timeout = 5)
glimpse(rt1)

l<-lookup_coords('usa')
?lookup_coords()

place<-'usa'
coord<-geo_cascade(place)
lat<-as.numeric(coord[1])
lan<-as.numeric(coord[2])
boxp <- c(sw.lng = lan-2 , 
          sw.lat = lat-2 , 
          ne.lng = lan+2, 
          ne.lat = lat+2)
point <- c(lat = lat, lng = lan)
coords <- list(place = place, box = boxp, point = point)
class(coords) <- c("coords", "list")

rt2<-stream_tweets(lookup_coords('usa'),timeout = 5)
rt2p<-stream_tweets(coords,timeout = 5)

rt3<-search_tweets(q = '#covid19',
                   n = 1000,
                   include_rts = FALSE,
                   lang = 'en',
                   geocode = coords)
data("quakes")
quakes<-quakes[1:20,]
quakes %>%
  leaflet() %>%
  setView(quakes$long[1] ,quakes$lat[1], zoom = 3) %>%
  addTiles() %>%
  addMarkers(~long,~lat,popup = as.character(quakes$mag),label = as.character(quakes$mag)) %>%
  addCircleMarkers(~long,~lat,weight =0.5, radius = 10)
#???1/0.000621371
rt3 %>%
  select(contains("coords")) %>%
  unnest_wider(geo_coords) %>%
  filter(!is.na(...1))


rt3 %>%
  select(screen_name,text,coords_coords) %>%
  unnest_wider(coords_coords) %>%
  filter(!is.na(...1)) %>%
  set_names(c("screen_name", "text", "lon", "lat")) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(~lon,~lat,popup = as.character(rt3$text),label = as.character(rt3$screen_name))

data_prepared<- tibble(location = coords) %>%
  separate(location, into = c("lat","lon","distance"), sep = "," , remove = FALSE) %>%
  mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>%
  mutate_at(.vars = vars(-location, as.numeric))

#sentiment analysis

tweets_tokenized_tbl<- data %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text)

tweets_tokenized_tbl %>% count(word, sort = TRUE)

get_sentiments(lexicon = 'bing')
get_sentiments(lexicon = "afinn")

sentiments_bing_tbl<- tweets_tokenized_tbl %>%
  inner_join(get_sentiments('bing'))

sentiments_afinn_tbl<- tweets_tokenized_tbl %>%
  inner_join(get_sentiments('afinn'))

sentiment_by_row_id_tbl <- sentiments_bing_tbl %>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(
    data %>% select(screen_name, text) %>% rowid_to_column()
  )




label_wrap <- label_wrap_gen(width = 60)
data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted =str_glue('Row ID: {rowid}
                                  Screen Name: {screen_name}
                                  Text: 
                                  {label_wrap(text)}
                                  '))

g<- data_formatted %>% 
  ggplot(aes(rowid, sentiment)) +
  geom_line(color = "#2c3e50", alpha= 0.5) +
  geom_point(aes(text = text_formatted), color = "#2c3e50") +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
  geom_hline(aes(yintercept = mean(sentiment)), color = 'blue') +
  geom_hline(aes(yintercept =  0.95*max(sentiment)), color = "red") +
  geom_hline(aes(yintercept =  0.95*min(sentiment)), color = "red") +
  theme_tq() +
  labs(title = "Sentiment Polarity", x = "Twitter User", y = "Sentiment")

ggplotly(g, tooltips= "text") %>%
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )

sentiment_by_word_tbl <- sentiments_bing_tbl %>%
  count(word, sentiment, sort =  TRUE)

sentiment_by_word_tbl %>% 
  pivot_wider(names_from = sentiment,values_from = n, values_fill =  list(n=0)) %>%
  column_to_rownames(var = "word") %>%
  comparison.cloud(
    colors= palette_light()
  )


sentiment_by_word_tbl %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ sentiment, ncol = 2) +
  theme_tq() +
  scale_color_tq() + 
  scale_size_area(max_size = 16) + 
  labs(title = "Sentiment Word Frequency")



#------------------

data<-search_tweets(q = '@cocacola',
                    n = 3000,
                    include_rts = FALSE,
                    lang = 'en'
                    ,geocode = geo_encode('usa', km_deg(600))
)

DATA<-data %>%
  select(screen_name,text,coords_coords) %>%
  unnest_wider(coords_coords) %>%
  filter(!is.na(...1)) %>%
  set_names(c("screen_name", "text", "lon", "lat"))


tweets_tokenized_tbl<- DATA %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('bing'))%>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(
    DATA %>% select(screen_name, text) %>% rowid_to_column()
  )




d<-data %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort =  TRUE) %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ sentiment, ncol = 2) +
  theme_tq() +
  scale_color_tq() + 
  scale_size_area(max_size = 16) + 
  labs(title = "Sentiment Word Frequency")
d
