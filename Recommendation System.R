# install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
getwd()
setwd("C:/Users/ADMIN/Documents/IMDB-Dataset")
Data_Movies <- read.csv("movies.csv",stringsAsFactors = FALSE)
Data_Ratings <- read.csv("ratings.csv")
View(Data_Movies)
View(Data_Ratings)

summary(Data_Movies)
head(Data_Movies)
str(Data_Movies)

summary(Data_Ratings)
head(Data_Ratings)
str(Data_Ratings)

Genre_of_movie <- as.data.frame(Data_Movies$genres, stringsAsFactors = FALSE)
Genre_of_movie
Genre2_of_movie <- as.data.frame(tstrsplit(Genre_of_movie[,1],'[|]',type.convert = TRUE), stringsAsFactors = FALSE)
Genre2_of_movie
colnames(Genre2_of_movie) <- c(1:10)

Genre_types <- c("Action","Adventure","Animation","Children","Comedy","Crime",
                 "Drama","Documentary","Fantasy", "Film-Noir","Horror","Musical",
                 "Mystery","Romance","Sci-Fi","Thriller","War","Western")
Genre_Matrix <- matrix(Genre_types, nrow = 10330, ncol = 18)
colnames(Genre_Matrix) <- Genre_types

for (index in 1:nrow(Genre2_of_movie)) {
  for(col in 1:ncol(Genre2_of_movie)) {
    Genre_column = which(Genre_Matrix[1,] == Genre2_of_movie[index,col])
    Genre_Matrix[index+1,Genre_column] <- 1
  }
}

Genre_Matrix1 <- as.data.frame(Genre_Matrix[-1,], stringsAsFactors = FALSE)

for (col in 1:ncol(Genre_Matrix1)) {
  Genre_Matrix1[,col] <- as.integer(Genre_Matrix1[,col])  
}
str(Genre_Matrix1)

Find_Matrix <- cbind(Data_Movies[,1:2], Genre_Matrix1[])
head(Find_Matrix)

Ratings_Matrix <- dcast(Data_Ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
Ratings_Matrix <- as.matrix(Ratings_Matrix[,-1])
Ratings_Matrix <- as(Ratings_Matrix, "realRatingMatrix")
Ratings_Matrix

Recommendation_Sys <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(Recommendation_Sys)

lapply(Recommendation_Sys, "[[", "description")

Recommendation_Sys$IBCF_realRatingMatrix$parameters

Matrix_Similarity <- similarity(Ratings_Matrix[1:5,],method = "cosine",which="User")
as.matrix(Matrix_Similarity)
Image <- image(as.matrix(Matrix_Similarity), main = "User's Similar Interests")

Movie_similarity <- similarity(Ratings_Matrix[,1:5],method = "cosine", which = "Item")
as.matrix(Movie_similarity)
Movie_Image <- image(as.matrix(Movie_similarity), main= "Similarity among Movies")

Ratings_Esteem <- as.vector(Ratings_Matrix@data)
Unique_Ratings <- unique(Ratings_Esteem)
Unique_Ratings

(Ratings_Table <- table(Ratings_Esteem))

Views_count <- colCounts(Ratings_Matrix)
Views_count
Views_count_table <- data.frame(movie = names(Views_count), views=Views_count)
Views_count_table <- Views_count_table[order(Views_count_table$views,
                                             decreasing = TRUE),]
Views_count_table$title <- NA
for(index in 1:10325){
  Views_count_table[index,3]<- as.character(subset(Data_Movies,
                                                   Data_Movies$movieId == Views_count_table[index,1])$title)
}
Views_count_table[1:12,]

ggplot(Views_count_table[1:5,],aes(x = title, y= views))+
  geom_bar(stat="identity",fill='gold' )+
  geom_text(aes(label=views),vjust=-0.5,size=2.5)+
  theme(axis.title.x = element_text(angle=30,hjust=1))+
  ggtitle("Top Films Views Count")

Heatmap<-image(Ratings_Matrix[1:20,1:30], axes= FALSE, main="Heatmap of first 20 rows and 30 columns")
Heatmap

Movies_Ratings <- Ratings_Matrix[rowCounts(Ratings_Matrix)>60,colCounts(Ratings_Matrix)>60]
Movies_Ratings

Mini_Movies <- quantile(rowCounts(Movies_Ratings),0.96)
Mini_Users <- quantile(colCounts(Movies_Ratings),0.96)
image(Movies_Ratings[rowCounts(Movies_Ratings)>Mini_Movies,
      colCounts(Movies_Ratings)>Mini_Users],
      main = "Heatmap of Top Users and Movies")

Ratings_avg <- rowMeans(Movies_Ratings)
qplot(Ratings_avg, fill=I("Red"), col=I("black"))+
  ggtitle("Average Rating visual per User")

Ratings_normalizing <- normalize(Movies_Ratings)
sum(rowMeans(Ratings_normalizing)>0.00005)
Ratings_normImage <- image(Ratings_normalizing[rowCounts(Ratings_normalizing)> Mini_Movies,
                                               colCounts(Ratings_normalizing)> Mini_Users],
                           main="Top Users Normalized Ratings")
Ratings_normImage

Mini2_Movies <- quantile(rowCounts(Movies_Ratings),0.90)
Mini2_Users <- quantile(colCounts(Movies_Ratings),0.90)

Bad_Rating_Movie <- binarize(Movies_Ratings,minRating=0,maxRating=2)
Bad_Rating_Movie
Bad_Movie_Image <- image(Bad_Rating_Movie[rowCounts(Movies_Ratings)>Mini2_Movies,
                                          colCounts(Movies_Ratings)>Mini2_Users],
                         main = "Movies and Users representation with Low Review")
Bad_Movie_Image

Avg_Rating_Movie <- binarize(Movies_Ratings,minRating=2,MaxRating=3)
Avg_Rating_Movie
Avg_Movie_Image <- image(Avg_Rating_Movie[rowCounts(Movies_Ratings)>Mini2_Movies,
                                          colCounts(Movies_Ratings)>Mini2_Users],
                         main = "Movies and Users representation with Average Review")
Avg_Movie_Image

Good_Rating_Movie <- binarize(Movies_Ratings,minRating=3)
Good_Rating_Movie
Good_Movie_Image <- image(Good_Rating_Movie[rowCounts(Movies_Ratings)>Mini2_Movies,
                                            colCounts(Movies_Ratings)>Mini2_Users],
                          main = "Movies and Users representation with Good Review")
Good_Movie_Image

Data_Sample <- sample(x=c(TRUE,FALSE),
                      size = nrow(Movies_Ratings),
                      replace = TRUE,prob=c(0.7,0.3))
Data_Sample
Train_data <- Movies_Ratings[Data_Sample,]
Test_data <- Movies_Ratings[!Data_Sample,]

Recommend_System <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
Recommend_System$IBCF_realRatingMatrix$parameters

Data_Recommendation <- Recommender(data=Train_data,
                    method="IBCF",parameter=list(k=25))
Data_Recommendation
class(Data_Recommendation)
Info<- getModel(Data_Recommendation)
Info
class(Info$sim)
dim(Info$sim)

Top_part <- 15
image(Info$sim[1:Top_part,1:Top_part],
      main= "First Row and First Column map")

Rows_Sum <- rowSums(Info$sim>0)
table(Rows_Sum)

Cols_Sum <- colSums(Info$sim>0)
qplot(Cols_Sum,fill=I("grey"),col=I("gold"),binwidth=1)+
        ggtitle("Distribution Count of column")

Best_Recommendations <- 20
Prediction_Best <- predict(object=Data_Recommendation,
                           newdata=Test_data,
                           n=Best_Recommendations)
Prediction_Best

User_1 <- Prediction_Best@items[[1]]
User_1_Movies <- Prediction_Best@itemLabels[User_1]
User_2_Movies <- User_1_Movies
for(index in 1:20){
  User_2_Movies[index]<-as.character(subset(Data_Movies,
  Data_Movies$movieId==User_1_Movies[index])$title)
}
User_2_Movies

Recommendation_System <- sapply(Prediction_Best@items,
                  function(x){as.integer(colnames(Movies_Ratings)[x])})
Recommendation_System[,1:6]

Total_items <- factor(table(Recommendation_System))
qplot(Total_items,fill=I("Green"),col=I("red"))+
  ggtitle("Total Items Distribution for IBCF")

Sort_Items <- sort(Total_items,decreasing = TRUE)
Top_Items <- head(Total_items,n=10)
Top_Table <- data.frame(as.integer(names(Top_Items)),Top_Items)
for (a in 1:10) {
  Top_Table[a,1]<-as.character(subset(Data_Movies,Data_Movies$movieId==
                                        Top_Table[a,1])$title)
}
colnames(Top_Table)<-c("Movie","No.of interests")
head(Top_Table,10)
