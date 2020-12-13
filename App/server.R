library(shiny)
library(shinydashboard)
library(stringr)



get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
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

movies$MovieID <- 1:nrow(movies)

myurl = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL



#Recommendation algorithm system 2

set.seed(100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train.id, ]


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

rm(Rmat)

# System 1

genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp_sys1 = as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)


genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")


m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp_sys1)){
  genre_matrix[i,genre_list %in% tmp_sys1[i,]]=1
}
colnames(genre_matrix) = genre_list
rownames(genre_matrix) = movies$MovieID

remove("tmp_sys1", "genres")



i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp_sys1 = data.frame(i, j, x, stringsAsFactors = T)
Rmat_sys1 = sparseMatrix(as.integer(tmp_sys1$i), as.integer(tmp_sys1$j), x = tmp_sys1$x)
rownames(Rmat_sys1) = levels(tmp_sys1$i)
colnames(Rmat_sys1) = levels(tmp_sys1$j)
Rmat_sys1 = new('realRatingMatrix', data = Rmat_sys1)



# Scheme 1:  Ranking based on highly-rated in genre

# Average rating were calculated for each movie, movies with highest rating were ranked decreasingly for each genre

movieAvgRat  = colMeans(as(Rmat_sys1, 'matrix'), na.rm = TRUE)



genre_highly_rated_matrix = matrix(NA, 18, length(unique(ratings$MovieID)))
rownames(genre_highly_rated_matrix) = genre_list


Rmat_sys1_Cols  = strtoi(strip(colnames(Rmat_sys1), char="m"))

for(i in 1:18) {

  tmp_sys1 = data.frame(id = movies$MovieID[genre_matrix[,i] == 1], rat = 1:length(movies$MovieID[genre_matrix[,i] == 1]))
  tmp_sys1[,2] =NA

  tmp2_sys1 = movieAvgRat[Rmat_sys1_Cols %in% movies$MovieID[genre_matrix[,i] == 1]]

  for(j in tmp_sys1$id){
    if(!is.na(j)){

      tmp_sys1[which(tmp_sys1$id == j),2] = tmp2_sys1[paste0('m',j)]

    }
  }

  tmp_sys1 = tmp_sys1[order(tmp_sys1$rat,decreasing = TRUE),]
  print(i)
  for(k in 1:dim(tmp_sys1)[1]){
    if(!is.na(tmp_sys1[k,2])){
      genre_highly_rated_matrix[i,k] = tmp_sys1[k,1]
    }

  }

}
rm(Rmat_sys1,train.id)


# Scheme 2:  Ranking based on popularity in genre

# For each movie the counts of ratings >=3 were made, movies with highest counts were considered the most poopular in thier  genre

temp = as(Rmat_sys1, 'matrix')
temp[which(temp < 3)] = NA
temp[which(temp >= 3)] = 1

moviePopCount = colSums(temp, na.rm = TRUE)



genre_most_popular_matrix = matrix(NA, 18, length(unique(ratings$MovieID)))
rownames(genre_most_popular_matrix) = genre_list


for(i in 1:18) {

  tmp_sys1 = data.frame(id = movies$MovieID[genre_matrix[,i] == 1], rat = 1:length(movies$MovieID[genre_matrix[,i] == 1]))
  tmp_sys1[,2] =NA

  tmp2_sys1 = moviePopCount[Rmat_sys1_Cols %in% movies$MovieID[genre_matrix[,i] == 1]]

  for(j in tmp_sys1$id){
    if(!is.na(j)){

      tmp_sys1[which(tmp_sys1$id == j),2] = tmp2_sys1[paste0('m',j)]

    }
  }

  tmp_sys1 = tmp_sys1[order(tmp_sys1$rat,decreasing = TRUE),]
  print(i)
  for(k in 1:dim(tmp_sys1)[1]){
    if(!is.na(tmp_sys1[k,2])){
      genre_most_popular_matrix[i,k] = tmp_sys1[k,1]
    }

  }

}

# Convert to dataframe
genre_highly_rated_matrix <- as.data.frame(genre_highly_rated_matrix)
genre_most_popular_matrix <- as.data.frame(genre_most_popular_matrix)

genre_most_popular_matrix <- genre_most_popular_matrix[1:10]
genre_highly_rated_matrix <- genre_highly_rated_matrix[1:10]

genre_highly_rated_matrix <- t(genre_highly_rated_matrix)
genre_most_popular_matrix <- t(genre_most_popular_matrix)

genre_highly_rated_matrix <- as.data.frame(genre_highly_rated_matrix)
genre_most_popular_matrix <- as.data.frame(genre_most_popular_matrix)

# genre_list = c("Action", "Adventure", "Animation",
#                "Children's", "Comedy", "Crime",
#                "Documentary", "Drama", "Fantasy",
#                "Film-Noir", "Horror", "Musical",
#                "Mystery", "Romance", "Sci-Fi",
#                "Thriller", "War", "Western")
# 
# genre_highly_rated_matrix <- read.csv("HR.csv",row.names =  1 )
# colnames(genre_highly_rated_matrix) <- genre_list
# genre_most_popular_matrix <- read.csv("PP.csv",row.names =  1 )
# colnames(genre_most_popular_matrix) <- genre_list

# Server function
server_f <- shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      
      # Algorithm
      user_input = data.frame(
        UserID = rep(6041,nrow(user_ratings)),
        user_ratings
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
      print(result)
      
      

      user_results = as.vector(result)
      print(user_results)
      user_predicted_ids = str_sub(names(result),2)
      print(user_predicted_ids)
      
      
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = user_predicted_ids, 
                                  Title = movies[match(user_predicted_ids, movies$MovieID), 2], 
                                  Predicted_rating =  user_results)
      
      print(recom_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    recom_result <- as.data.frame(recom_result)
    recom_result$MovieID <- as.numeric(recom_result$MovieID)

    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)),
            div(style="text-align:center; font-size: 100%", strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )       
      }))) # columns
    }) # rows
    
    
  }) # renderUI function
  
  
  
  
  # System 1 Output for top 10 most popular movies

  output$result_sys1_popular <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- genre_most_popular_matrix


    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

            div(style = "text-align:center", img(src = movies$image_url[genre_most_popular_matrix[(i - 1) * num_movies + j,input$genre]], height = 150)),
            div(style="text-align:center; font-size: 100%", strong(movies$Title[genre_most_popular_matrix[(i - 1) * num_movies + j,input$genre]])
            )

        )
      }))) # columns
    }) # rows


  }) # renderUI function

  
  # System 1 Output for top 10 highly rated movies 
  
  output$result_sys1_highlyrated <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- genre_highly_rated_matrix
    
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", img(src = movies$image_url[genre_highly_rated_matrix[(i - 1) * num_movies + j,input$genre_2]], height = 150)),
            div(style="text-align:center; font-size: 100%", strong(movies$Title[genre_highly_rated_matrix[(i - 1) * num_movies + j,input$genre_2]])
            )
            
        )       
      }))) # columns
    }) # rows
    
    
  }) # renderUI function
  
  
}) # server function
