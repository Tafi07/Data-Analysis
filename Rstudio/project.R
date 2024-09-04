#importing .csv file 
movies <- read.csv("moviehaat.csv", header = TRUE, sep = ",")
movies
#removing missing values 
movies_1 <- movies
movies_1[!apply(movies_1 == "", 1, all),]
movies_1


#Data Formatting... To round up the murder and assualt variable
movies_1$Ratings = as.double(format(round(movies_1$Ratings, 0)))
movies_1

#descriptive statistics (mean)
mean(movies_1$Downloads)

#descriptive statistics (median)
median(movies_1$Ratings)

#descriptive statistics (range)
max(movies_1$Year)- min(movies_1$Year)

#descriptive statistics (Quantile)
quantile(movies_1$Downloads)

#scatterplot
library(ggplot2)
ggplot(movies_1 ,aes(x = Ratings, y = Downloads,
                  color=Ratings  )) +
  geom_point(alpha = 5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5)

#bardiagram

library(ggplot2)
ggplot(movies_1, aes(x=Industry)) + geom_bar(fill="blue", color="red") + 

  labs(title="Simple Bar chart", 
       x="Indsutry", 
       y="Downloads")




library(rvest)

# set the URL of the website and retrieve the HTML content
url <- "http://www.moviehaat.net/allmovies.php?page=1&entries=63&sort=DESC&w=grid"
page <- read_html(url)

# extract the movie data
titles <- page %>% html_nodes(".movies-list .movie-title") %>% html_text()
years <- page %>% html_nodes(".movies-list .movie-year") %>% html_text()
ratings <- page %>% html_nodes(".movies-list .movie-ratings") %>% html_text()
quality <- page %>% html_nodes(".movies-list .movie-quality") %>% html_text()
industry <- page %>% html_nodes(".movies-list .movie-industry") %>% html_text()
genre <- page %>% html_nodes(".movies-list .movie-genre") %>% html_text()
downloads <- page %>% html_nodes(".movies-list .movie-downloads") %>% html_text()


# create a data frame with the extracted data
movie_data <- data.frame(Title = titles, Year = years, Rating = Ratings, Quality = quality, Industry = industry, Genre = genre
                         Downloads = downloads)

# save the data frame to a CSV file
write.csv(movie_data, file = "moviehaat.csv", row.names = FALSE)













