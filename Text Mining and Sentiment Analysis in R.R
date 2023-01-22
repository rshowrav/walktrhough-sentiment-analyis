#Installing Packages in R (Only if not previously installed).
##Tidyverse will allow us to create structured data.
##Tidytext will allow us to perform tokenization and transform to tidy data structure. 
##Tm will be used to create a grouping of documents (i.e. sentiments).
##Wordcloud will allow us to build a word cloud. Lattice will be used for visualizations. 
##Text data will provide a space to download, parse, and store text datasets. 
##Scales will be used to create a ggplot.

install.packages('tidyverse')
install.packages('tidytext')
install.packages('tm')
install.packages('wordcloud')
install.packages('lattice')
install.packages('textdata')
install.packages('scales')

#Loading Necessary Libraries to R from above packages.

library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(lattice)
library(textdata)
library(scales)


#Loading data file and creating text dataset

dealer_review_data = read.csv(file='dealer_review_data.csv', header = TRUE, stringsAsFactors = FALSE)

dealer_reviews = dealer_review_data %>%  
  select(review)

tidy_dataset = dealer_reviews %>%
  unnest_tokens(word, review)

#Removing stop words from the tidy_data set

data("stop_words")

tidy_dataset2 = tidy_dataset %>%
  anti_join(stop_words)

tidy_dataset2 %>%
  count(word) %>%
  arrange(desc(n))

#Removing numberic variables, new lines, tabs, and spaces

patterndigits = '\\b[0-9]+\\b'

tidy_dataset2$word = tidy_dataset2$word %>%
  str_replace_all(patterndigits, '')

patternewline ='\n+'

tidy_dataset2$word = tidy_dataset2$word %>%
  str_replace_all(patternewline, '')

tidy_dataset2$word = tidy_dataset2$word %>%
  str_replace_all('[:space:]', '')

tidy_dataset3 = filter(tidy_dataset2,!(word == ''))

#Getting word frequency

tidy_dataset3 %>%
  count(word) %>%
  arrange(desc(n))

#Plotting word frequency

frequency = tidy_dataset3 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  mutate(proportion = (n / sum(n)*100)) %>%
  filter(proportion >= 0.5)


ggplot(frequency, aes(x = proportion, y = word)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = 'Word', x = 'Proportion')

#Getting the sentiments from nrc for postive/negative and trust/anger

get_sentiments('nrc') %>%
  distinct(sentiment)

nrc_trustanger = get_sentiments('nrc') %>%
  filter(sentiment == 'trust' | 
           sentiment == 'anger')

get_sentiments('nrc') %>%
  distinct(sentiment)

nrc_posneg = get_sentiments('nrc') %>%
  filter(sentiment == 'positive' | 
           sentiment == 'negative')

#Joining the above sentiments and creating two new data sets

newjoin1 = inner_join(tidy_dataset3, nrc_trustanger)

counts = count(newjoin1, word, sentiment)
spread1 = spread(counts, sentiment, n, fill = 0)

content_data = mutate(spread1, contentment = trust - anger, linenumber = row_number())
merger_trustanger = arrange(content_data, desc(contentment))

newjoin2 = inner_join(tidy_dataset3, nrc_posneg)

counts2 = count(newjoin2, word, sentiment)
spread2 = spread(counts2, sentiment, n, fill = 0)

content_data2 = mutate(spread2, diffsent = positive - negative, linenumber = row_number())
merger_posneg = arrange(content_data2, desc(diffsent))

#Generating a word cloud for each sentiment 
##(tidy_dataset4 for trust/anger and tidy dataset 5 for positive/negative)

trust_data = newjoin1 %>% 
  filter(sentiment == "trust")

anger_data = newjoin1 %>% 
  filter(sentiment == "anger")

postive_data = newjoin2 %>% 
  filter(sentiment == "positive")

negative_data = newjoin2 %>% 
  filter(sentiment == "negative")

wordcloud(trust_data[,1],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(2, "Greens"))

wordcloud(anger_data[,1],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(2, "Reds"))

wordcloud(postive_data[,1],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(2, "Blues"))

wordcloud(negative_data[,1],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(2, "Purples"))
