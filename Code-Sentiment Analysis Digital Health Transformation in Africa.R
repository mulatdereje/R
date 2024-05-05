library(readxl) #To read Excel files

#Read the data
myfile = read_xlsx("D:/Doc/Academics/Research/Conferences/African Population Conference/Dataset/analisisdigitalhealthafrica.xlsx")

print(data) #Print the data

library(tidytext) #for text mining and analysis
library(dplyr) #for data manipulation and transformation

tidy_digital_health<- myfile %>% #The %>% operator (pipe operator) is used to pass the result of the slice() function to the next operation in a data pipeline
  select(Year,Title) %>%
  unnest_tokens("word", Title) #to tokenize text data into individual words or tokens.
data("stop_words")


top_words<-
  tidy_digital_health %>%
  anti_join(stop_words) %>%  #to remove rows from one data frame that match with another data frame based on specified conditions
  filter(!(word=="in"|       #to subset rows from a data frame based on specified conditions
             word=="the"|
             word=="of"|
             word=="on")) %>%
  count(word) %>%   #to count the frequency of unique values in one or more columns of a data frame
  arrange(desc(n)) #to reorder the rows of a data frame based on one or more variables
## Joining with `by = join_by(word)`
library(ggplot2)

top_words %>%
  slice(1:20) %>%  #to extract specific rows from a data frame based on their position
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+ #provides a flexible and layered approach to creating plots.
  geom_bar(stat="identity")+ #to create bar plots
  theme_minimal()+ #sets the theme of a ggplot object to a minimalistic style
  theme(axis.text.x =    #to customize the appearance and style of a plot
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+  #the + operator is used to add additional layers, modifications, and themes to a ggplot object
  xlab("")+
  ggtitle("Most Frequent Words search: Digital Health in Africa")+ #to add a title to a plot created with ggplot
  guides(fill=FALSE)

## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
## of ggplot2 3.3.4.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.

Pub_year <- myfile$Year
hist(myfile$Year,xlab="year",ylab="",main="Papers on Digital Health in Africa by year, PubMed", col="red") #To create a histogram

tidy_health_tfidf<- myfile %>%
  select(Year,Title) %>%
  unnest_tokens("word", Title) %>%
  anti_join(stop_words) %>%
  count(word, Year) %>%
  bind_tf_idf(word, Year, n)

## Joining with `by = join_by(word)`

top_tfidf<-tidy_health_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1]
## [1] "caring"

digital_health_dictionary<-c("health","digital","transformation","digital health transformation", "Artifical Intelligence")

library(stringr) #provides a set of functions for working with strings
digital_health_pubmed<-myfile[str_detect(myfile$Title, paste(digital_health_dictionary, collapse="|")),]

library(tidytext)
head(get_sentiments("bing"))

## # A tibble: 6 × 2
##   word       sentiment
##   <chr>      <chr>    
## 1 2-faces    negative 
## 2 abnormal   negative 
## 3 abolish    negative 
## 4 abominable negative 
## 5 abominably negative 
## 6 abominate  negative

digital_health_sentiment <- tidy_digital_health %>%
  inner_join(get_sentiments("bing")) %>% #to perform an inner join operation on two data frames based on matching values in specified columns
  count(Year, sentiment) #to count the number of occurrences of unique values in one or more columns of a data frame
## Joining with `by = join_by(word)`

head(digital_health_sentiment) #to view the first few rows of a data frame or a vector.

## # A tibble: 6 × 3
##    Year sentiment     n
##   <dbl> <chr>     <int>
## 1  2009 positive      1
## 2  2015 positive      1
## 3  2016 negative      1
## 4  2016 positive      2
## 5  2017 negative      3
## 6  2017 positive      6

tidy_digital_health$date<-as.Date(tidy_digital_health$Year, format="%Y")

health_sentiment_plot <-
  tidy_digital_health %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(Year, sentiment)
## Joining with `by = join_by(word)`

ggplot(health_sentiment_plot, aes(x=Year, y=n))+
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Negative Words")+
  xlab("Year")+
  ggtitle("Negative Sentiment in Digital Health in Africa papers")+
  theme(aspect.ratio=1/4)

## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ??? Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.

health_sentiment_plot2 <-
  tidy_digital_health %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="positive") %>%
  count(Year, sentiment)

## Joining with `by = join_by(word)`

ggplot(health_sentiment_plot2, aes(x=Year, y=n))+
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Positive Words")+
  xlab("Year")+
  ggtitle("Positive Sentiment in Digital Health in Africa papers")
