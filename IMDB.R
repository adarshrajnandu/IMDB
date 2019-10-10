# hello world

print("Hello World")
a <- "Hello World"
a

# Loading the required packages

library(tidyverse)
library(rvest)

# Specifying the url for the desired website to be scrapped
url <- "https://www.imdb.com/search/title/?count=100&genres=action&release_date=2018,2018&title_type=feature"


# Reading the html code from the website
webpage <- read_html(x = url)



# Rank: The rank of the film from 1 to 100 on the list of 100 films released in 2018.
# Title: The title of the film.
# Description: The description of the film.
# Runtime: The duration of the film.
# Genre: The genre of the film,
# Rating: The IMDb rating of the film.
# Votes: Votes cast in favor of the film.
# Gross_Earning_in_Mil: The gross earnings of the film in millions.
#####################################################################

# Start by scraping the Rank field. For that, we’ll use the selector gadget to get the specific CSS selectors that encloses the rankings. 

rank_data_html <- html_nodes(x = webpage, css = '.text-primary')
rank_data_html

rank_data <- html_text(x = rank_data_html)
rank_data

# converting the data into num

rank_data <- as.numeric(x = rank_data)

#####################################################################

# Now you can select all the titles. 

title_data_html <- html_nodes(x = webpage, css = '.lister-item-header a')
title_data_html

title_data <- html_text(x = title_data_html)
title_data
class(title_data)
#####################################################################


description_data_html <- html_nodes(x = webpage, css = '.ratings-bar+ .text-muted')
description_data_html

description_data <- html_text(x = description_data_html)
description_data

#Data-Preprocessing: removing '\n'

description_data<-gsub(pattern = "\n",replacement =  "",x =  description_data)
head(description_data)
#####################################################################


runtime_data_html <- html_nodes(webpage,'.runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

runtime_data <- gsub(pattern = "min", replacement = "",x = runtime_data)
head(runtime_data)

runtime_data <- as.numeric(x = runtime_data)
####################################################################


genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- genre_data_html%>%
  html_text(genre_data_html)%>%
  str_trim()

#Let's have a look at the runtime
head(genre_data)

genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)
###################################################################


rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)

rating_data<-as.numeric(rating_data)
head(rating_data)
##################################################################


votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

votes_data <- gsub(pattern = ",",
                   replacement = "",
                   x = votes_data)
head(votes_data)

votes_data <- as.numeric(x = votes_data)
##################################################################


movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Votes = votes_data)

#Structure of the data frame

str(movies_df)

# Analyzing scraped data from the web

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))
