library(tidyverse)
library(tidytext)

trump <- read.csv("trump.csv")

trump$Speech <- as.character(trump$Speech)

#unnest tokens of each speech 
unnest_trump <- trump %>%
  unnest_tokens(word, Speech)

#count how many times each word appears in each speech
count_trump <- unnest_trump %>%
  count(Title, word, sort = TRUE) %>%
  ungroup()

#count the total number of words in each speech
totalwords_trump <- count_trump %>% 
  group_by(Title) %>% 
  summarize(totalwords = sum(n)) %>%
  ungroup()

#merge to one data set
words_trump <- left_join(count_trump, totalwords_trump)

#remove stop words
data(stop_words)
nostopwords_trump <- words_trump %>% 
  anti_join(stop_words, by="word")

head1 <- nostopwords_trump
head1$Title <- as.factor(head1$Title)
head1 <- filter(head1, Title==
                  c("Remarks at the Charlotte Convention Center in Charlotte, North Carolina",
                "Remarks at the Roberts Centre in Wilmington, Ohio",
                "Remarks at a Rally at the Pensacola Bay Center in Pensacola, Florida"))
head1 <- head1 %>%
  group_by(word) %>%
  summarise(Frequency = sum(n)) %>%
  ungroup()
head1 <- filter(head1, Frequency > 5)

#graph of frequent words that trump uses in swing states
g <- ggplot(head1, aes(x=reorder(word, -Frequency), y=Frequency, fill="purple")) + geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("Word") + ylab("Frequency") + ggtitle("Trump's Most Used Words in Swing States") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
g


#graph of frequent words that trump uses in non-swing states

head2 <- nostopwords_trump
head2$Title <- as.factor(head2$Title)
head2 <- filter(head2, Title==
                  c("Remarks at Luedecke Arena in Austin, Texas",
                    "Remarks at the Mississippi Coliseum in Jackson, Mississippi",
                    "Remarks at the XFinity Arena in Everett, Washington"))
head2 <- head2 %>%
  group_by(word) %>%
  summarise(Frequency = sum(n)) %>%
  ungroup()
head2 <- filter(head2, Frequency > 5)

g <- ggplot(head2, aes(x=reorder(word, -Frequency), y=Frequency, fill="purple")) + geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("Word") + ylab("Frequency") + ggtitle("Trump's Most Used Words in Non-Swing States") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
g






afinn <- filter(sentiments, lexicon=="AFINN")
tmerge <- left_join(unnest_trump, afinn)
tmerge <- mutate(tmerge, score = ifelse(is.na(score), 0, score))
ttotal <- tmerge %>%
  group_by(Title) %>%
  summarize(total=sum(score)) %>%
  ungroup()
ttidy <- left_join(trump, ttotal)

#speech 1
nc <- filter(trump, Title=="Remarks at the Charlotte Convention Center in Charlotte, North Carolina")

#split by line
#split by line, remove lines with no data or with [] prefaces
unnest_trump <- data.frame(Speech = unlist(strsplit(nc$Speech, "\n")), 
                                 stringsAsFactors= FALSE)

#add line numbers
line <- unnest_trump %>%
  mutate(line = 1:n()) %>%
  dplyr::select(line, Speech)

afinn <- filter(sentiments, lexicon=="AFINN")

#tokenize by word and merge
unnest_trump <- line %>%
  unnest_tokens(word, Speech)
tmerge <- left_join(unnest_trump, afinn)
tmerge <- mutate(tmerge, score = ifelse(is.na(score), 0, score))

#count afinn total and merge
ttotal <- tmerge %>%
  group_by(line) %>%
  summarize(total=sum(score)) %>%
  ungroup()
ttidy <- left_join(line, ttotal)
ttidy <- na.omit(ttidy)
ttidy <- mutate(ttidy, line= 1:118)

#plot
g <- ggplot(ttidy, aes(x=line, y=total)) +
  geom_line() +
  xlab("Line number") +
  ylab("Net Sentiment Score") +
  ggtitle("An Emotional Analysis of North Carolina Speech") +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dotted")
g


#speech 2
tx <- filter(trump, Title=="Remarks at Luedecke Arena in Austin, Texas")

#split by line
#split by line, remove lines with no data or with [] prefaces
unnest_trump <- data.frame(Speech = unlist(strsplit(tx$Speech, "\n")), 
                           stringsAsFactors= FALSE)
#unnest_marvinsroom <- filter(unnest_marvinsroom, lyrics != "")
#unnest_marvinsroom <- filter(unnest_marvinsroom, !grepl("]",lyrics))

#add line numbers
line <- unnest_trump %>%
  mutate(line = 1:n()) %>%
  dplyr::select(line, Speech)

afinn <- filter(sentiments, lexicon=="AFINN")

#tokenize by word and merge
unnest_trump <- line %>%
  unnest_tokens(word, Speech)
tmerge <- left_join(unnest_trump, afinn)
tmerge <- mutate(tmerge, score = ifelse(is.na(score), 0, score))

#count afinn total and merge
ttotal <- tmerge %>%
  group_by(line) %>%
  summarize(total=sum(score)) %>%
  ungroup()
ttidy <- left_join(line, ttotal)
ttidy <- na.omit(ttidy)
ttidy <- mutate(ttidy, line= 1:118)

#plot
g <- ggplot(ttidy, aes(x=line, y=total)) +
  geom_line() +
  xlab("Line number") +
  ylab("Net Sentiment Score") +
  ggtitle("An Emotional Analysis of Texas Speech") +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dotted")
g


#loughran data
loughran <- filter(sentiments, lexicon=="loughran")
tidy.const <- left_join(words_trump, loughran)
tidy.const <- rename(tidy.const, "wordcount"="n")
trump_speech <- tidy.const %>%
  group_by(Title, sentiment) %>%
  count(sentiment, sort=FALSE, wt=1/totalwords) %>%
  ungroup()
trump_speech <- filter(trump_speech, sentiment!=is.na(sentiment))
trump_speech <- arrange(trump_speech, Title, -n)


#nrc 
nrc <- filter(sentiments, lexicon=="nrc")
nrc_trump <- inner_join(words_trump, nrc)
join_trump <- left_join(words_trump, nrc)
join_trump <- rename(join_trump, "wordcount"="n")
trump_speech <- join_trump %>%
  group_by(Title, sentiment) %>%
  count(sentiment, sort=FALSE, wt=1/totalwords) %>%
  ungroup()
trump_speech <- filter(trump_speech, sentiment!=is.na(sentiment))
write.csv(trump_speech, "nrc_trump.csv")
trump_speech <- read.csv("nrctrump.csv")


g <- ggplot(trump_speech) +
  geom_bar(aes(x=State, y=n, fill=State), stat="identity") +
  facet_wrap(~Sentiment) + 
  ggtitle("Proportion of Sentiment by Swing State/Non-Swing State")
g

