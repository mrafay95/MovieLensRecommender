

library(recommenderlab)
library(Matrix)


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



set.seed(100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train.id, ]
head(train)

test = ratings[-train.id, ]
head(test)


i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)
x = train$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)


rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))


### Predict recommendation from user input ###

#Initialize User Input

user_input = data.frame(
  UserID = rep(6041,14),
  MovieID = c(9,3,2,12,15,5,6,1,13,4,8,11,10,7),
  Rating = c(4,3,2,5,5,3,5,1,5,2,3,5,5,2)
)
data <- train
rownames(data) = NULL
data <- rbind(data,user_input)
i = paste0('u', data$UserID)
j = paste0('m', data$MovieID)
x = data$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat_user = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat_user) = levels(tmp$i)
colnames(Rmat_user) = levels(tmp$j)
Rmat_user = new('realRatingMatrix', data = Rmat_user)

a <- Rmat_user["u6041",]

# predicting....
recom = predict(rec_UBCF, a, type = 'ratings')



# Sorting the result by top ratings, picking top n results
result = as(recom, 'matrix')[,]
result <- result[!is.na(result)]
result <- sort(result,decreasing = T)[1:10]
#as(Rmat, 'matrix')[100:105, 1:10]






