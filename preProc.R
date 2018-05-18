#path.data <- files.choose()
#path.data <- choose.files()
#path.data
path.data = "C:\\Western\\MEng\\Term3\\DA\\Project\\dataset\\goodbooks-10k-master\\goodbooks-10k-master\\"

file_booktags = "book_tags.csv"
file_books = "books.csv"
file_ratings = "ratings.csv"
file_tags = "tags.csv"
file_toread = "to_read.csv"

df.btags = read.csv(paste(path.data, file_booktags, sep = ""))
df.books = read.csv(paste(path.data, file_books, sep = ""))
df.ratings = read.csv(paste(path.data, file_ratings, sep = ""))
df.tags = read.csv(paste(path.data, file_tags, sep = ""))
df.toread = read.csv(paste(path.data, file_toread, sep = ""))
rm(path.data, file_books, file_booktags, file_ratings, file_tags, file_toread)

# str(df.btags)
# str(df.books)
# str(df.ratings)
# str(df.tags)
# str(df.toread)

# add books_id column to btags
df.btags$book_id <- df.books$book_id[match(df.btags$goodreads_book_id, 
                                           df.books$goodreads_book_id)]

# drop columns that won't be used
drop <- c("goodreads_book_id", "best_book_id", "work_id", "isbn", "isbn13",
          "image_url", "small_image_url")
df.books <- df.books[, !(names(df.books) %in% drop)]

drop <- c("goodreads_book_id")
df.btags <- df.btags[, !(names(df.btags) %in% drop)]
rm(drop)


# Pre process tags ----
# Pre process for TF and IFD
#df.btags <- df.btags[with(df.btags, order(book_id)), ]

# Calculate tag frequency
df.tagNumBooks <- as.data.frame(table(df.btags$tag_id))
df.tags$inBooks <- df.tagNumBooks$Freq[match(df.tags$tag_id, 
                                             df.tagNumBooks$Var1)]
rm(df.tagNumBooks)

# Calculate max tag count for a book
maxTagInBook = rep(0, 10000)
for (row in 1:nrow(df.btags)) {
  if (df.btags[row, "count"] > maxTagInBook[df.btags[row, "book_id"]]) {
    maxTagInBook[df.btags$book_id[row]] = df.btags$count[row]
  }
}

# Calculate TF and IDF for each tag
df.btags$TF = df.btags$count / (maxTagInBook[df.btags$book_id])
df.tags$IDF = log10(nrow(df.books) / (df.tags$inBooks))
rm(maxTagInBook)

# Calculate weight of each tag for each book
df.btags$IDF <- df.tags$IDF[match(df.btags$tag_id, 
                                           df.tags$tag_id)]
df.btags$weight <- df.btags$TF*df.btags$IDF

# remove tags with less than frequency 2
toRemove = df.tags[(df.tags$inBooks < 2), ]
df.tags <- df.tags[!(df.tags$inBooks < 2), ]
df.btags <- df.btags[!(df.btags$tag_id %in% toRemove$tag_id),]
rm(toRemove)

# Normalize book tag weights
weightBookLen = rep(0, 10000)
for (row in 1:nrow(df.btags)) {
  weightBookLen[df.btags$book_id[row]] = weightBookLen[df.btags$book_id[row]] +
    df.btags$weight[row]^(2)
}
weightBookLen = sqrt(weightBookLen)
df.weightLen <- as.data.frame(weightBookLen)
df.weightLen$book_id = c (1:length(weightBookLen))
df.btags$weightLen <- df.weightLen$weightBookLen[match(df.btags$book_id, 
                                                       df.weightLen$book_id)]
df.btags$normWeight = df.btags$weight/df.btags$weightLen

# Calculate new length. Verify length is 1
# weightBookLen = rep(0, 10000)
# for (row in 1:nrow(df.btags)) {
#   weightBookLen[df.btags$book_id[row]] = weightBookLen[df.btags$book_id[row]] +
#     df.btags$normWeight[row]^(2)
# }
# weightBookLen = sqrt(weightBookLen)
# df.weightLen <- as.data.frame(weightBookLen)
# df.weightLen$book_id = c    (1:length(weightBookLen))
# df.btags$normLen <- df.weightLen$weightBookLen[match(df.btags$book_id, 
#                                                        df.weightLen$book_id)]
rm(df.weightLen, weightBookLen)





# Pre process years ----
df.books$original_publication_year[is.na(df.books$original_publication_year)] <- 1980
yearBreaks <- c(min(df.books$original_publication_year),
                1000,
                1500,
                1800,
                1900,
                1950,
                1980,
                2000,
                2010,
                2015,
                max(df.books$original_publication_year))
df.books$year <- cut(df.books$original_publication_year, breaks =  yearBreaks,
                     labels=paste("era", 1:(length(yearBreaks) - 1), sep=""))
                     

# Pre process authors ----
df.books$authID <- as.numeric(factor(df.books$authors, 
                                     levels=unique(df.books$authors)))