

library(recommenderlab)
library(Matrix)
library(data.table)
library(Xmisc)



myurl = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL






myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))




genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)


genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")


m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
rownames(genre_matrix) = movies$MovieID

remove("tmp", "genres")



genre_matrix = genre_matrix[unique(train$MovieID),]



i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)




movieAvgRat  = colMeans(as(Rmat, 'matrix'), na.rm = TRUE) 


genre_highly_rated_matrix = matrix(NA, 18, length(unique(ratings$MovieID)))
rownames(genre_highly_rated_matrix) = genre_list


Rmat_Cols  = strtoi(strip(colnames(Rmat), char="m"))

for(i in 1:18) {
  
  tmp = data.frame(id = movies$MovieID[genre_matrix[,i] == 1], rat = 1:length(movies$MovieID[genre_matrix[,i] == 1]))
  tmp[,2] =NA
  
  tmp2 = movieAvgRat[Rmat_Cols %in% movies$MovieID[genre_matrix[,i] == 1]] 
  
  for(j in tmp$id){
    if(!is.na(j)){

      tmp[which(tmp$id == j),2] = tmp2[paste0('m',j)]
    
    }
  }
  
  tmp = tmp[order(tmp$rat,decreasing = TRUE),]
  print(i)
  for(k in 1:dim(tmp)[1]){
    if(!is.na(tmp[k,2])){
      genre_highly_rated_matrix[i,k] = tmp[k,1]
    }
    
  }

}









