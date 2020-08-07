##Final
jeans=read.csv('Womens Clothing E-Commerce Reviews.csv',stringsAsFactors = F)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
install.packages('tidyr')
install.packages('broom')
install.packages('Zelig')
install.packages('pander')
install.packages('radiant.data')
install.packages('visreg')
library(broom)
library(Zelig)
library(pander)
library(radiant.data)
library(visreg)
library(tidytext)
library(dplyr)
library(purrr)
library(stringr)
library(igraph)
install.packages('igraph')
library(igraph)
library(ggplot2)
install.packages('wordcloud2')
library(wordcloud2)
install.packages('ggraph')
library(ggraph)
install.packages('topicmodels')
library(topicmodels)
install.packages('partykit')
library(partykit)
install.packages('gtrendsR')
library(gtrendsR)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)
install.packages('e1071')
library(e1071)
install.packages('syuzhet')
library(syuzhet)

##Remove first column X
jeans$X<-NULL
str(jeans)
##Changes title
colnames(jeans) <- c('ID', 'Age', 'Title', 'Review', 'Rating', 'Recommend', 'Liked', 'Division', 'Dept', 'Class')
str(jeans)
summary(jeans)
##list NA
unlist(map(map(jeans, is.na), sum))
##1.Age distribution Ans: Age between 30-45's Women who shop more at the e-commerce store are likely to between
ggplot(data = jeans, aes(x = Age)) + geom_histogram( fill = "blue") 
#2.which Department gets however much percentage of the reviews/ratings.
ggplot(data.frame(prop.table(table(jeans$Dept))), aes(x=Var1, y = Freq*100)) + geom_bar(stat = 'identity') + xlab('Department Name') + ylab('Percentage of Reviews/Ratings (%)') + geom_text(aes(label=round(Freq*100,2)), vjust=-0.25) + ggtitle('Percentage of reviews per Departament')

#3.ratings percentage by Department
##below is not working
photo <- jeans %>% filter(!is.na(Dept), Dept != 'Trend') %>% mutate(Dept = factor(Dept)) %>% group_by(Dept) %>% count(Rating) %>% mutate(perc = n/sum(n))
photo%>% ggplot(aes(x=Rating, y = perc*100, fill = Dept)) + geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~Dept) + ylab('Percentage of reviews (%)') + geom_text(aes(label=round(perc*100,2)), vjust = -.2) + scale_y_continuous(limits = c(0,65)) 
sapply(jeans, function(x) sum(is.na(x)))
summary(jeans$Dept)
##This one works.
#3.which Department gets however much percentage of the reviews/ratings.
departament <- jeans %>% filter(!is.na(Dept), Dept != 'Trend') %>% mutate(Dept = factor(Dept)) %>% group_by(Dept) %>% count(Rating) %>% mutate(perc = n/sum(n))
departament%>% ggplot(aes(x=Rating, y = perc*100, fill = Dept)) + geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~Dept) + ylab('Porcentage of Reviews (%)') + geom_text(aes(label=round(perc*100,2)))
#4.age distribution per department
ages <- jeans %>% filter(!is.na(Age), !is.na(Dept), Dept != 'Trend') %>% select(ID, Age, Dept) %>% mutate(Age_group = ifelse(Age < 30, '18-29', ifelse(Age < 40, '30-39', ifelse(Age < 50, '40-49', ifelse(Age < 60, '50-59', ifelse(Age < 70, '60-69', ifelse(Age < 80, '70-79', ifelse(Age < 90, '80-89', '90-99')))))))) 
ages <- Age %>% mutate(Age_group = factor(Age_group), Dept = factor(Dept, levels = rev(c('Tops', 'Dresses', 'Bottoms', 'Intimate', 'Jackets'))))
ages %>% filter(Age < 80) %>% group_by(Age_group) %>% count(Dept) %>% ggplot(aes(Dept, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') + geom_text(aes(label = n), hjust = 1) + scale_y_continuous(expand = c(.1, 0)) + coord_flip() 
ages %>% filter(Age >= 80) %>% group_by(Age_group) %>% count(Dept) %>% ggplot(aes(Dept, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') + geom_text(aes(label = n), hjust = 1.2) + coord_flip()
##conclusion 1..Age 26--55--Tops most，next is age 55-65
###2. Dress age 26 to 55 the most，then, 55-65, 18-25 and 66-99

##5.Sentiment Analysis--- Top 10 Most Common Word
library(tidyr)
df<- jeans
str(df)
##
df$Review <- as.character(df$Review)
str(df)
library(dplyr)
##
df %>% 
  unnest_tokens(word, `Review`) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill='orange') +
  labs(x = NULL, title = "Most used words in Reviews") +
  coord_flip()
##Positive and Negative Word in Reviews not include stop word
df%>%
  group_by(ID)%>%
  unnest_tokens(word, `Review`)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
df%>%
  group_by(ID)%>%
  unnest_tokens(word, `Review`)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+guides(fill=F)
##Emotions in Review
library(tidytext)
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
                 header = F,
                 col.names = c('word','sentiment','num'),
                 sep = '\t',
                 stringsAsFactors = F)
nrc = nrc[nrc$num!=0,]
nrc$num = NULL
####Emotions in Review
df%>%
  group_by(ID)%>%
  unnest_tokens(output=word, input=Review)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+guides(fill=F)+labs(x = NULL, title = "Emotions in Review")+coord_flip()
####Ratings of all Reviews based on Emotion Expressed
df%>%
  group_by(ID)%>%
  unnest_tokens(output = word, input = Review)%>%
  inner_join(nrc)%>%
  group_by(ID,sentiment,Rating)%>%
  count()%>%
  group_by(sentiment, Rating)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=Rating,y=n,fill=Rating))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()
#####Word Cloud
library(RColorBrewer)
library(wordcloud)
wordcloudData = 
  df%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=Review)%>%
  anti_join(stop_words)%>%
  group_by(ID)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
library(wordcloud)
set.seed(100)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2),max.words = 20,colors=brewer.pal(9,"Spectral"))


##ignore the below 
##1.Age distribution Ans: Age between 30-45's Women who shop more at the e-commerce store are likely to between
ggplot(data = jeans, aes(x = Age)) + geom_histogram( fill = "blue") 
##2.Simple Logistic Models
A1<- glm(Recommended.IND ~ Age, family = "binomial", data = jeans)
summary(A1)
##3.Visual
visreg(A1, "Age", scale = "response")
##4. 
Rate<- jeans %>% 
  group_by(Rating) %>% 
  summarise(py1 = mean(Recommended.IND)) %>% 
  mutate(py0 = 1 - py1) %>% 
  pandoc.table()
##5.
C1<- glm(Recommended.IND ~ Rating, family = "binomial", data = jeans)
summary(C1)
##6.
visreg(C1, "Rating", scale = "response")
##7.
C3<- glm(Recommended.IND ~ Rating*Age + Positive.Feedback.Count, family = "binomial", data = jeans)
summary(C3)
##8 Age 20 and 30 groups are likely to give positive feedback as compared to older age groups
visreg(C3, "Age", by = "Positive.Feedback.Count", scale = "response")
str(jeans)
----------------------------