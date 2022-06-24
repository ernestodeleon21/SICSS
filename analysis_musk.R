library(academictwitteR)
library(ggplot2)
library(dplyr)
library(lubridate)

set_bearer()


# querying by string
tweetsmusk <- get_all_tweets(
  query = "musk",
  is_retweet = F,
  start_tweets = "2022-04-25T18:50:00Z",
  end_tweets = "2022-04-25T19:00:00Z",
  bearer_token = get_bearer(),
  #file = "data/blmtweets.rds",
  #data_path = "data/json_data/",
  n = 10000, 
  #need no rewteets
)

#save(tweetsmusk, file = "C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/10k_tweets_musk.rda")



#getting other 90k tweets 
tweetsmusk2 <- get_all_tweets(
  query = "musk",
  is_retweet = F,
  start_tweets = "2022-04-25T19:00:00Z",
  end_tweets = "2022-04-25T22:00:00Z",
  bearer_token = get_bearer(),
  #file = "data/blmtweets.rds",
  #data_path = "data/json_data/",
  n = 90000, 
)





bots = tweetsmusk %>% group_by(author_id) %>% dplyr::summarize(n = n())


weird = tweetsmusk %>% filter(author_id == '2263001041')


tweetcounts <- count_all_tweets(
  query = "Musk",
  start_tweets = "2022-04-25T01:00:00Z",
  end_tweets = "2022-04-25T23:59:00Z",
  bearer_token = get_bearer(),
  granularity = "minute",
  n = 100000
)


#save(tweetsmusk2, file = "C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda")


sum(tweetcounts$tweet_count)


#reformat date
tweetcounts$time <-
  parse_date_time(tweetcounts$start, orders = "ymd HMS")

as.Date("2022-04-25-20-43-00")


DateTime2 = "2022-04-25 18:43:00"
ymd_hms(DateTime2)

#plot 
p = tweetcounts %>% ggplot() +
  geom_line(aes(time, tweet_count)) + theme_bw() +
  geom_vline(xintercept=as.POSIXct(ymd_hms(DateTime2)),
                                                 color ="red",linetype=2, size =1) +
  ylab('Number of Tweets per Minute\nMentioning Musk')
  

p


path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/timeline_announcement.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")



############ Tweets for comparison period ############

tweetcounts <- count_all_tweets(
  query = "Musk",
  start_tweets = "2021-04-25T01:00:00Z",
  end_tweets = "2021-04-25T23:59:00Z",
  bearer_token = get_bearer(),
  granularity = "minute",
  n = 100000
)


#save(tweetsmusk2, file = "C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda")


sum(tweetcounts$tweet_count)


#reformat date
tweetcounts$time <-
  parse_date_time(tweetcounts$start, orders = "ymd HMS")

as.Date("2022-04-25-20-43-00")


DateTime2 = "2022-04-25 18:43:00"
ymd_hms(DateTime2)

#plot 
p = tweetcounts %>% ggplot() +
  geom_line(aes(time, tweet_count)) + theme_bw() +
  ylab('Number of Tweets per Minute\nMentioning Musk') 
  

p


path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/timeline_comparison_no_ylim.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")


#With correct ylim

#plot 
p = tweetcounts %>% ggplot() +
  geom_line(aes(time, tweet_count)) + theme_bw() +
  ylab('Number of Tweets per Minute\nMentioning Musk') +ylim(0,10000)
  

p

path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/timeline_comparison_yes_ylim.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")



######## Tweets for all of April #######

tweetcounts <- count_all_tweets(
  query = "Musk",
  start_tweets = "2022-04-10T01:00:00Z",
  end_tweets = "2022-04-30T23:59:00Z",
  bearer_token = get_bearer(),
  granularity = "hour",
  n = 10000000
)


#save(tweetsmusk2, file = "C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda")


sum(tweetcounts$tweet_count)


#reformat date
tweetcounts$time <-
  parse_date_time(tweetcounts$start, orders = "ymd HMS")

as.Date("2022-04-25-20-43-00")


DateTime2 = "2022-04-25 16:00:00"
ymd_hms(DateTime2)


DateTime3 = "2022-04-14 08:23:00"


library(scales)
#plot 
p = tweetcounts %>% ggplot() +
  geom_line(aes(time, tweet_count), size=1) + theme_bw() +
  geom_vline(xintercept=as.POSIXct(ymd_hms(DateTime2)),
                                                 color ="red",linetype=2, size =1, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct(ymd_hms(DateTime3)),
                                                 color ="red",linetype=2, size =1, alpha=0.5) +
  ylab('Number of Tweets per HOUR\nMentioning Musk') + xlab('')+
  scale_y_continuous(labels = comma) 
  

p


path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/timeline_april.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")





######## analysis #########################

colnames(tweetsmusk)


load("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/tweetsbefore.rda")
load("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda")


tweetsmusk = tweetsbefore %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsmusk2 = tweetsmusk2 %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


#bots = tweetsmusk2 %>% group_by(author_id) %>% dplyr::summarise(n=n()) 

min(tweetsmusk2$created_at)
max(tweetsmusk2$created_at)

tweets.after = tweetsmusk2



tweetsbefore = tweetsbefore %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsafter = tweetsmusk2 %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


library(stringr)
library(tidytext)
library(tidyverse)

tweetsbefore$period = 'before' 
tweetsafter$period = 'after' 



remove_reg <- "&amp;|&lt;|&gt;"

tidy_tweets_after <- tweetsafter %>% 
  dplyr::filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

tidy_tweets_before <- tweetsbefore %>% 
  dplyr::filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

tidy_tweets_after $period = 'after' 
tidy_tweets_before$period = 'before' 


tidy_tweets_combined <- bind_rows(tidy_tweets_after, tidy_tweets_before)


table(tidy_tweets_combined$lang)

tidy_tweets_combined = tidy_tweets_combined %>% dplyr::filter(lang == 'en')

frequency <- tidy_tweets_combined %>% 
  count(period, word, sort = TRUE) %>% 
  left_join(tidy_tweets_combined %>% 
              count(period, name = "total")) %>%
  mutate(freq = n/total)



library(tidyr)

frequency <- frequency %>% 
  select(period, word, freq) %>% 
  pivot_wider(names_from = period, values_from = freq) %>%
  arrange(after,before)



library(scales)


ggplot(frequency, aes(after, before)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")



word_ratios <- tidy_tweets_combined %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, period) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(before / after)) %>%
  arrange(desc(logratio))




word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (before/after)") +
  scale_fill_discrete(name = "", labels = c("before", "after"))




########## STM #####################


tweetsbefore = tweetsbefore %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsafter = tweetsmusk2 %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsbefore$period = 'before' 
tweetsafter$period = 'after' 



combined <- bind_rows(tweetsafter, tweetsbefore)

combined = combined %>% dplyr::filter(lang == 'en')



library(tidyverse)
library(tidytext)


remove_reg <- "&amp;|&lt;|&gt;"
ments = "@\\w+"
links = "http\\w+"
links2 = "http[[:alnum:]]*"
weird2= '1hrgrb1zt7qnhq1rejfhv2huquselzraxx'
punct = "[^[:alnum:][:space:]#]"

tidy_tweets <- combined %>% 
  dplyr::filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  mutate(text = str_remove_all(text, ments)) %>%
  mutate(text = str_remove_all(text, links)) %>%
  mutate(text = str_remove_all(text, links2)) %>%
  mutate(text = str_remove_all(text, weird2)) %>%
  mutate(text = str_remove_all(text, punct)) %>%

  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

colnames(tidy_tweets)

tidy_tweets %>%
    count(word, sort = TRUE)


library(drlib)

sherlock_tf_idf <- tidy_tweets %>%
    count(period, word, sort = TRUE) %>%
    bind_tf_idf(word, period, n) %>%
    arrange(-tf_idf) %>%
    group_by(period) %>%
    top_n(50) %>%
    ungroup




sherlock_tf_idf %>%
    mutate(word = reorder_within(word, tf_idf, period)) %>%
    ggplot(aes(word, tf_idf, fill = period)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ period, scales = "free", ncol = 3) +
    scale_x_reordered() +
    coord_flip() +
    theme(strip.text=element_text(size=11)) +
    labs(x = NULL, y = "tf-idf",
         title = "Highest tf-idf words in Sherlock Holmes short stories",
         subtitle = "Individual stories focus on different characters and narrative elements")


colnames(tidy_tweets)

table(tidy_tweets$period)
before= tidy_tweets %>% dplyr::filter(period == 'before') 
after= tidy_tweets %>% dplyr::filter(period == 'after') 


library(quanteda)
install.packages('stm')
library(stm)




sherlock_dfm <- before %>%
    count(id, word, sort = TRUE) %>%
    cast_dfm(id, word, n) 

sherlock_dfm

sherlock_dfm = dfm_wordstem(sherlock_dfm, language = quanteda_options("language_stemmer"))

sherlock_dfm


topfeatures(sherlock_dfm, n=100) 



sherlock_sparse <- before %>%
    count(id, word, sort = TRUE) %>%
    cast_sparse(id, word, n)


colnames(tweetsafter)

tweetsafter2 = tweetsafter %>% dplyr::filter(lang == 'en')







#Topic Modelling #############
library(tidyverse)
library(quanteda)
library(stm)
library(seededlda)



load("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/tweetsbefore.rda")

tweetsbefore = tweetsbefore %>% dplyr::filter(lang == 'en')

corpus <- corpus(tweetsbefore, docid_field = "id", text_field = "text")


corpus = tokenizers::tokenize_words(tweetsbefore$text, lowercase = T, strip_punct = T) %>%
    tokens(remove_symbols = TRUE)


dfm <- dfm(corpus)
dfm = dfm_remove(dfm, stopwords("english"))
dfm = dfm_wordstem(dfm)



topfeatures(dfm, n=20) 


dfm <- dfm_trim(dfm, min_termfreq = 0.8, termfreq_type = "quantile",max_docfreq = 0.4, docfreq_type = "prop")
topfeatures(dfm, n=20) 



lda <- seededlda::textmodel_lda(dfm, k=8) # fit lda with 8


seededlda::terms(lda,10)




dfm <- dfm_trim(dfm, min_termfreq = 0.8, termfreq_type = "quantile",max_docfreq = 0.4, docfreq_type = "prop")
ap_lda <- topicmodels::LDA(dfm, k = 8, control = list(seed = 1234))
ap_lda



library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")


library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
td_beta <- tidy(lda)

lda %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")



####################  Sentiment Analysis ############


load("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/tweetsbefore.rda")
load("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda")


tweetsmusk = tweetsbefore %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsmusk2 = tweetsmusk2 %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)




tweetsbefore = tweetsbefore %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsafter = tweetsmusk2 %>% dplyr::select(source, author_id, lang, conversation_id,created_at,
                                          public_metrics, id, text, in_reply_to_user_id,
                                          geo)


tweetsbefore$period = 'before' 
tweetsafter$period = 'after' 



combined <- bind_rows(tweetsafter, tweetsbefore)

combined = combined %>% dplyr::filter(lang == 'en')


library(tidytext)
library(textdata)

get_sentiments("afinn")







remove_reg <- "&amp;|&lt;|&gt;"
ments = "@\\w+"
links = "http\\w+"
links2 = "http[[:alnum:]]*"
weird2= '1hrgrb1zt7qnhq1rejfhv2huquselzraxx'
punct = "[^[:alnum:][:space:]#]"

tidy_tweets <- combined %>% 
  dplyr::filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  mutate(text = str_remove_all(text, ments)) %>%
  mutate(text = str_remove_all(text, links)) %>%
  mutate(text = str_remove_all(text, links2)) %>%
  mutate(text = str_remove_all(text, weird2)) %>%
  mutate(text = str_remove_all(text, punct)) %>%

  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))






nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "surprise")

tidy_tweets %>%
  #filter(period == "after") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)



library(tidyr)

m.sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(period, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


tidy_tweets %>% group_by(period) %>% summarise(words =n())



m.sentiment = m.sentiment %>% mutate(words = c(544206, 136527))

m.sentiment$norm = m.sentiment$sentiment/m.sentiment$words

m.sentiment$pos.norm = m.sentiment$positive/m.sentiment$words

m.sentiment$neg.norm = m.sentiment$negative/m.sentiment$words

m.sentiment$sent.norm = m.sentiment$pos.norm - m.sentiment$neg.norm





library(ggplot2)

ggplot(m.sentiment, aes(period, sent.norm, fill = period)) +
  geom_col(show.legend = FALSE) #+
  facet_wrap(~book, ncol = 2, scales = "free_x")
  
  
ggplot(m.sentiment, aes(period, sent.norm, fill = period)) +
  geom_col(show.legend = FALSE) 
  
  
  
m.sent = m.sentiment %>% dplyr::select(period,pos.norm, neg.norm)

m.sent.g = m.sent %>%
  pivot_longer(!period, names_to = "income", values_to = "count")

m.sent.g

m.sent.g = m.sent.g %>% mutate(income = ifelse(income == 'neg.norm', 'Negative', 'Positive'),
                               period = ifelse(period == 'before', 'Before Takeover', 'After Takeover'))

p = ggplot(m.sent.g, aes(y=count,x=period, fill = income)) +
  geom_bar(stat = 'identity', position = 'dodge') + theme_bw()+
  scale_fill_discrete(name = "Sentiment") + xlab('Period') + ylab('Percent of Words')

path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/sentiment.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")




######### Anger ##################



surprise = get_sentiments("nrc")) %>% filter(sentiment == "surprise")

library(tidyr)

m.sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(period, sentiment) 
  #pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  #mutate(sentiment = positive - negative)


total.num = tidy_tweets %>% group_by(period) %>% summarise(words =n())

m.sentiment = left_join(m.sentiment, total.num)

m.sentiment$norm = m.sentiment$n/m.sentiment$words


m.sentiment = m.sentiment %>% mutate(
                               period = ifelse(period == 'before', 'Before Takeover', 'After Takeover'))




p = ggplot(m.sentiment, aes(y=norm,x=period, 
           fill = sentiment)) +
  geom_bar(stat = 'identity', position = 'dodge') + theme_bw()+
  scale_fill_discrete(name = "Sentiment") + xlab('Period') + ylab('Percent of Words')
  
p

path = 'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/sentiment_discrete.jpg'

ggsave(path, plot = p, height = 4, width = 6, units = "in", device='jpeg', dpi=1000, antialias="none")

  
