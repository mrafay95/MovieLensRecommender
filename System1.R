

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


recom = predict(rec_UBCF, Rmat[100:105], type = 'ratings')


as(recom, 'matrix')[, 1:10]
as(Rmat, 'matrix')[100:105, 1:10]



