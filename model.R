# Separate training, validation and test data

sampleDf <- df.ratings[1:20000, ]
test.ids <- sample.int(n=nrow(sampleDf), size = floor(0.2*nrow(sampleDf)))
sam.test <- sampleDf[test.ids, ]
sam.remain <- sampleDf[-test.ids, ]
valid.ids <- sample.int(n=nrow(sam.remain), size = floor(0.2*nrow(sampleDf)))
sam.valid <- sam.remain[valid.ids, ]
sam.train <- sam.remain[-valid.ids, ]
#rm(sam.remain)

eval = sam.valid[,]
#eval = sam.test

eval$cosine <- rep(0, nrow(eval))
eval$predRating <- rep(0,nrow(eval))

sam.validUsers <- unique(eval$user_id)
sam.validUsers <- sam.validUsers[order(sam.validUsers)]

# Traverse through validation sample and try to predict ratings
for (user in sam.validUsers)
{
  # Filter the books read by the user
  userBooks <- sam.train[sam.train$user_id == user, ]
  if (0 == nrow(userBooks)) {
    next
  }
  
  
  ################ Make tag vector of user ########----
  # Filter the btags of the books read
  userBTags <- df.btags[0, ]
  getBTags <- function(x) {
    userBTags <<- rbind(userBTags, df.btags[df.btags$book_id == x["book_id"], ])
  }
  pSink <- apply(userBooks, 1, getBTags)
  
  # Drop columns that are not required
  # drop <- c("count", "TF", "IDF", "weight", "weightLen")
  # userBTags <- userBTags[, !(names(userBTags) %in% drop)]
  # rm(drop)
  
  # add rating to tag weights
  userBTags$rating <- userBooks$rating[match(userBTags$book_id,
                                             userBooks$book_id)]
  userBTags$rW <- userBTags$normWeight*userBTags$rating
  
  # Make user profile in tags dimensions
  userTagProf <- data.frame(tag_id = integer(),
                            weight = double())
  count <- 1
  
  CalcUserTag <- function(x) {
    tagId = x["tag_id"]
    if (tagId %in% userTagProf$tag_id) {
      index <- match(tagId, userTagProf$tag_id)
      userTagProf$weight[index] <<- userTagProf$weight[index] + x["rW"]
    } else {
      userTagProf[count, ] <<- matrix(c(tagId, x["rW"]), ncol =2)
      count <<- count + 1
    }
  }
  pSink <- apply(userBTags, 1, CalcUserTag)
  
  # divide rated weights by total number of books read by the user
  userTagProf$weight <- userTagProf$weight/nrow(userBooks)
  # Normalize
  weightLen <- sqrt(sum(userTagProf$weight^2))
  userTagProf$weight <- userTagProf$weight/weightLen
  # rm(weightLen)
  
  # Calculate cosine similarity of user with other books
  include <- c("tag_id", "book_id", "normWeight")
  userBookSim <- df.btags[, (names(df.btags) %in% include)]
  rm(include)
  
  for (row in 1:nrow(userBooks)) {
    userBookSim <- userBookSim[!(userBookSim$book_id == 
                                   userBooks[row, ]$book_id), ]
  }
  userBookSim$userWeight <- userTagProf$weight[match(userBookSim$tag_id,
                                                     userTagProf$tag_id)]
  userBookSim["userWeight"][is.na(userBookSim["userWeight"])] <- 0
  userBookSim$tagCosine <- userBookSim$normWeight*userBookSim$userWeight
  
  ######## Make year vector for user ###########----
  userBooks$era <- df.books$year[match(userBooks$book_id, df.books$book_id)]
  userYearSpace <- as.data.frame(table(userBooks$era))
  userYearLen <- sqrt(sum(userYearSpace$Freq^2))
  userYearSpace$Freq <- userYearSpace$Freq/userYearLen

  ######## Make authors vector for user ###########----
  userBooks$auth <- df.books$authID[match(userBooks$book_id, df.books$book_id)]
  userAuthSpace <- aggregate(rating ~ auth, data = userBooks, sum)
  userAuthLen <- sqrt(sum(userAuthSpace$rating^2))
  userAuthSpace$rating <- userAuthSpace$rating/userAuthLen
  
  ######## Make language vector for user ##########----
  userBooks$lang <- df.books$language_code[match(userBooks$book_id, df.books$book_id)]
  userLangSpace <- as.data.frame(table(userBooks$lang))
  userLangLen <- sqrt(sum(userLangSpace$Freq^2))
  userLangSpace$Freq <- userLangSpace$Freq/userLangLen
  
  ########## Create recommendation for user #############----
  userRecom <- aggregate(tagCosine ~ book_id, data = userBookSim, sum)
  
  # Calculate era similarity
  userRecom$era = df.books$year[match(userRecom$book_id, df.books$book_id)]
  userRecom$eraCos = userYearSpace$Freq[match(userRecom$era, userYearSpace$Var1)]
  userRecom$eraCos[is.na(userRecom$eraCos)] <- 0
  
  # Calculate author similarity
  userRecom$auth <- df.books$authID[match(userRecom$book_id, df.books$book_id)]
  userRecom$authCos <- userAuthSpace$rating[match(userRecom$auth, userAuthSpace$auth)]
  userRecom$authCos[is.na(userRecom$authCos)] <- 0
  
  # Calculate lang similarity
  userRecom$lang <- df.books$language_code[match(userRecom$book_id, df.books$book_id)]
  userRecom$langCos <- userLangSpace$Freq[match(userRecom$lang, userLangSpace$Var1)]
  userRecom$langCos[is.na(userRecom$langCos)] <- 0
  
  # userRecom$cosine <- 0.25*userRecom$tagCosine + 0.30*userRecom$authCos +
  #   0.20*userRecom$eraCos + 0.25*userRecom$langCos #2.165
  
  userRecom$cosine <- 0.25*userRecom$tagCosine + 0.30*userRecom$authCos +
    0.25*userRecom$eraCos + 0.20*userRecom$langCos #1.830664
  
  userRecom <- userRecom[order(-userRecom$cosine), ]
  
  len <- nrow(userRecom)
  cosBreak <- c(0, userRecom$cosine[floor(0.6*len/5)], userRecom$cosine[floor(2.4*len/5)],
                userRecom$cosine[floor(4*len/5)], 
                userRecom$cosine[floor(4.9*len/5)], 1)
  
  userRecom$predRating <- cut(userRecom$cosine, breaks =  cosBreak,
                       labels=c(1:5))
  
  # qplot(userRecom$cosine,
  #       geom="histogram",
  #       binwidth=0.01)
  
  # Evaluate----
  samUserInd <- which (eval$user_id %in% user)
  for (row in samUserInd) {
    eval$cosine[row] <- userRecom[eval$book_id[row] == userRecom$book_id, ]$cosine
    eval$predRating[row] <- ceiling((eval$cosine[row]/2)*10)
    eval$predRating[row] <- userRecom[eval$book_id[row] == userRecom$book_id, ]$predRating
  }
}



testRating <- eval[eval$predRating != 0, ]
testRating$mse <- (testRating$rating - testRating$predRating)^2
print((sum(testRating$mse))/nrow(testRating))
print(sqrt((sum(testRating$mse))/nrow(testRating)))
print((sum(abs(testRating$rating - testRating$predRating)))/nrow(testRating))


userBooks$title = df.books$original_title[match(userBooks$book_id,
                                 df.books$book_id)]
print("Books read by user 1:")
df.books$original_title[match(userBooks$book_id,
                              df.books$book_id)]

