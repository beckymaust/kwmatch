#' Match by Keyword
#' 
#' Matches a set of phrases in one column to its closest corresponding 
#' phrase in another column, utilizing the cosine similarity based on the 
#' tf - idf statistic. 
#'
#' @param x A column of phrases, which may correspond to multiple elements in the variation column \code{x}
#' @param variation Contains phrases which are compared to phrases in x \code{variation}
#' @param one.to.one If TRUE, keywordMatch attempts to match each phrase in x 
#' to a unique phrase in variation, otherwise each phrase in variation is matched 
#' to is most similar element in x \code{one.to.one}
#' @param rm.punctuation If TRUE, remove the punctation from all phrases \code{rm.punctuation}
#' @param rm.stopWords If TRUE, remove stop words from all phrases \code{rm.stopWords}
#'
#' @return correctly.matched A data frame with 2 columns where phrases from x are 
#' lined up with their matched phrases in variation. Pairs that have the highest
#' cosine similarity are listed first.
#'
#' @keywords match by keyword, cosine similarity
#'
#' @export
#' 
#' @examples
#' set.seed(100)
#' journal <- load(kwmatch.journal)
#' keywordMatch(journal, journal[sample(nrow(journal)),2], one.to.one = TRUE)



# need x and variation need to be vectors
keywordMatch <- function(x, variation, one.to.one = FALSE, rm.punctuation = TRUE, rm.stopWords = TRUE) {
  
  # if one.to.one is true, we want to remove any instances where
  # multiple phrases are matched to the same element
  removeDuplicates <- function(all.matches) {
    all.matches <- all.matches[!duplicated(all.matches[,1]),]
    all.matches <- all.matches[!duplicated(all.matches[,2]),]
    return(all.matches)
  }
  
  correctly.matched <- data.frame()
  
  # set variables
  x.num <- length(x)
  var.num <- length(variation)
  all.words <- rbind(data.frame(a = x), data.frame(a = variation))
  
  # create document term matrix
  # remove punctuation and stop words as needed
  corpus <- tm::Corpus(DataframeSource(data.frame(all.words)))
  if (rm.punctuation) {
    corpus <- tm::tm_map(corpus, FUN = tm::removePunctuation)
  }
  if (rm.stopWords) {
    skipWords <- function(x) tm::removeWords(x, stopwords("english"))
    corpus <- tm::tm_map(corpus, FUN = skipWords)
  }
  dtm <- tm::DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))

  #create cosine matrix
  num <- as.matrix(dtm[1:x.num,]) %*% t(as.matrix(dtm[(x.num + 1):(x.num + var.num),]))
  top <- rowSums(as.matrix(dtm[1:x.num,]) ^ 2)
  bottom <- rowSums(as.matrix(dtm[(x.num + 1):(x.num + var.num),]) ^ 2)
  denom <- sqrt(top %*% t(bottom))
  sym <- num / denom
 
 # if one.to.one is true, find the strongest matching pairs
 # then iteratively make match weaker and weaker pairs
 # if one.to.one is false, find the most closely matching phrase in x
 # for each phrase in variation
 if (one.to.one) {
   all.matches <- data.frame()
   new.sym <- sym
   while (max(new.sym) > 0) {
     matches <- which(new.sym == max(new.sym), arr.ind = TRUE)
     matches <- removeDuplicates(data.frame(matches))
     all.matches <- rbind(all.matches, matches)
     for (i in 1:nrow(matches)) {
       new.sym[matches[i,1], ] <- 0
       new.sym[ , matches[i, 2]] <- 0
     }
   }
   base <- data.frame(x)[all.matches[,1],]
   variation <- data.frame(variation)[all.matches[,2],]
   correctly.matched <- data.frame(base = base, variation = variation)
 } else {
   top.cols <- max.col(t(sym))
   correctly.matched <- data.frame(base = x, variation = data.frame(variation)[top.cols,])
 }
 
  return(correctly.matched)
  
}


