library(tm)      # Text Minning Library    
library(stringr) # Support to basic file summary
library(RWeka)   # Support to n-gram processes
library(stringi)
library(slam)
library(dplyr)
library(data.table)
#---Read Data---------------------------
blogsCon <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(blogsCon, encoding="UTF-8", skipNul = TRUE)
close(blogsCon)

twitterCon <- file("en_US/en_US.twitter.txt", "r")
twitter <- readLines(twitterCon, encoding="UTF-8", skipNul = TRUE)
close(twitterCon)

newsCon <- file("en_US/en_US.news.txt", "r")
news <- readLines(newsCon, encoding="UTF-8", skipNul = TRUE)
close(newsCon)

rm(list = c("blogsCon", "newsCon", "twitterCon")) 

#---Function Clean Data------------------------

cleanData <- function(x, cleanType = "train"){
  #remove non Unicode characters
  x <- tm_map(x, content_transformer(function(x) iconv(enc2utf8(x), sub = "")))
  #remove non ASCII characters
  x <- tm_map(x, content_transformer(function(x) iconv(x,from = "latin1", to="ASCII", sub="")))
  #low case and remove Stopwords
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeWords, stopwords("english"))
  #remove special character but not .?;"!() those will be a delimit signal when devided into ngram
  x <- tm_map(x,content_transformer(function(x) stri_replace_all_regex(x, "[^\\p{L}\\s[.?;!()\"]]+","")))
  #convert .?;!() character to " . " delimiter
  #x <- tm_map(x,content_transformer(function(x) stri_replace_all_regex(x, "[[.,?;!()]]+"," . ")))
  
  x <- tm_map(x, removeNumbers)
  #close when train open when run
  if (cleanType =="test") x <- tm_map(x, content_transformer(removePunctuation))
  #x <- tm_map(x, removeWords, profWords) # Remove profane words
  
  x <- tm_map(x, stripWhitespace)
  x
}

##---Function Split Clean and Tokenize Large Data--------------

split_clean_Tokenize_Data <- function(largeData, maxRow, ngrams, idfrom, toFolder, prefix) {
  #Split Large Character
  seqVector <- seq_along(largeData)
  sample <- split(largeData, ceiling(seqVector/maxRow))

  #Define NGramTokenizer Function
  nGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=ngrams, max=ngrams, delimiters= " \\r\\n\\t.,;:\"()?!"))
  
  time.taken = NULL

  for (i in (0+idfrom):(length(sample)+idfrom-1)) {
    start.time <- Sys.time()
    print(paste("process file #",i))
    cori <- Corpus(VectorSource(sample[i-idfrom+1]), readerControl=list(reader=readPlain, language="en_US", load=TRUE))
    #Clean Data
    cori <- cleanData(cori)
    #Create Document Matrix
    tdmi <- TermDocumentMatrix(cori, control = list(tokenize = nGramTokenizer, stripWhitespace=TRUE))
    rm(cori)
    #Extract frequency matrix 
    freqMatrix <-as.matrix(tdmi)
    rm(tdmi)
    #Save sub-dataset
    write.table(freqMatrix, paste0(toFolder,"/",prefix,i,".txt"),row.names = TRUE)
    rm(freqMatrix)
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time,0)
    print(end.time - start.time)
  }
}

##---Process Split Clean and Tokenize Large Data--------------
#Remove Stop Words dataset
#QuaGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 4,idfrom = 1, toFolder = "quagrams", prefix = "quaDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 4,idfrom = 31, toFolder = "quagrams", prefix = "quaDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 4,idfrom = 55, toFolder = "quagrams", prefix = "quaDF")

#TriGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 3,idfrom = 1, toFolder = "trigrams", prefix = "triDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 3,idfrom = 31, toFolder = "trigrams", prefix = "triDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 3,idfrom = 55, toFolder = "trigrams", prefix = "triDF")

#BinGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 2,idfrom = 1, toFolder = "bingrams", prefix = "binDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 2,idfrom = 31, toFolder = "bingrams", prefix = "binDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 2,idfrom = 55, toFolder = "bingrams", prefix = "binDF")

#Not Remove Stop Words dataset
#QuaGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 4,idfrom = 1, toFolder = "quagram", prefix = "quaDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 4,idfrom = 31, toFolder = "quagram", prefix = "quaDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 4,idfrom = 55, toFolder = "quagram", prefix = "quaDF")

#TriGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 3,idfrom = 1, toFolder = "trigram", prefix = "triDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 3,idfrom = 31, toFolder = "trigram", prefix = "triDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 3,idfrom = 55, toFolder = "trigram", prefix = "triDF")

#BinGram
split_clean_Tokenize_Data(largeData = blogs, maxRow = 30000, ngrams = 2,idfrom = 1, toFolder = "bingram", prefix = "binDF")
split_clean_Tokenize_Data(largeData = twitter, maxRow = 100000, ngrams = 2,idfrom = 31, toFolder = "bingram", prefix = "binDF")
split_clean_Tokenize_Data(largeData = news, maxRow = 30000, ngrams = 2,idfrom = 55, toFolder = "bingram", prefix = "binDF")

##---Function to arrange by first letter, aggregate and save-----------------

arrange_by_first_letter <- function(fromLetter=1, toLetter=26, 
                                     fromFile=1, toFile=57, 
                                     readFromFolder, saveToFolder, 
                                     readPrefix, savePrefix){
  
  time.taken = NULL

  for (i in letters[fromLetter:toLetter]) {
    start.time <- Sys.time()
    print(paste0("Calculate file ",savePrefix,i))
  
    df_i <- NULL
    for (j in fromFile:toFile) {
      sub.start.time <- Sys.time()
      print(paste0("   Process file ",readFromFolder,"/",readPrefix,j,".txt"))
    
      dfj <- as.matrix(read.table(file=paste0(readFromFolder,"/",readPrefix,j,".txt"),skip = 1, col.names = c("term", "freq")))
      selectRow <- grepl(paste0("^",i),dfj[,"term"])
      selectFrame <- dfj[selectRow,]
    
      if (is.null(df_i)) df_i <- selectFrame else df_i <- rbind(df_i, selectFrame)
      rm(dfj, selectRow, selectFrame)
      print(Sys.time() - sub.start.time)
    }
    
    sub.start.time <- Sys.time()
    print(paste0("   Aggregate ",savePrefix,i," before save to file"))
    df_i <- as.data.table(df_i)
    df_i$freq <- as.numeric(df_i$freq)
    df_i <- dcast.data.table(term ~ . , data=df_i, sum,value.var = "freq")
    names(df_i)<-c("term","freq")
    print(Sys.time() - sub.start.time)
  
    print(paste0("   Save ",savePrefix,i," to .txt file"))
    df_i <-as.matrix(df_i)
    write.table(df_i, paste0(saveToFolder,"/",savePrefix,i,".txt"),row.names = FALSE)
    rm(df_i)
  
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time,2)
    print(end.time - start.time)
  }
  time.taken
}

##---Process to arrange by first letter, aggregate and save-----------------
#Remove Stop Words dataset
qg <- arrange_by_first_letter(readFromFolder="quagrams",saveToFolder ="quagrams",readPrefix="quaDF",savePrefix ="qua_")
tg <- arrange_by_first_letter(readFromFolder="trigrams",saveToFolder ="trigrams",readPrefix="triDF",savePrefix ="tri_")
bg <- arrange_by_first_letter(readFromFolder="bingrams",saveToFolder ="bingrams",readPrefix="binDF",savePrefix ="bin_")

#Not Remove Stop Words dataset
qg <- arrange_by_first_letter(readFromFolder="quagram",saveToFolder ="quagram",readPrefix="quaDF",savePrefix ="qua_")
tg <- arrange_by_first_letter(readFromFolder="trigram",saveToFolder ="trigram",readPrefix="triDF",savePrefix ="tri_")
bg <- arrange_by_first_letter(readFromFolder="bingram",saveToFolder ="bingram",readPrefix="binDF",savePrefix ="bin_")

##---Function Extract frequent Terms----
extract_frequent_term <- function(lowestFreq=2,topFreq=5,fromLetter=1,toLetter=26,readFromFolder,saveToFolder,readPrefix,savePrefix){
  start.time <- Sys.time()
  
  for(i in letters[fromLetter:toLetter]){
    sub.start.time <-Sys.time()
    print(paste0("start load file ",readFromFolder,"/",readPrefix,i,".txt"))
    df <- data.table(read.table(file=paste0(readFromFolder,"/",readPrefix,i,".txt"),header = TRUE))
    
    # Eliminate terms having frequency < lowest frequency
    if (lowestFreq > 1) df <- df[!(df$freq < lowestFreq),] 
    
    # Eliminate terms having douplicated words
    print("Eliminate terms having douplicated words")
    df$term <- as.character(df$term)
    t <- unname(sapply(df$term, function(x) (sum(duplicated(unlist(strsplit(x," "))))==0)))
    df <- df[t] 
    
    # re-order dataframe
    df$inputterm <- gsub("\\s*\\w*$", "", df$term)
    df$predict <- stri_extract_last_words(df$term)
    df$term <- NULL
    setcolorder(df,c(2,3,1))
    set2keyv(df,c("inputterm","freq"))
    df <- df[order(inputterm,-freq),]
    
    df <- tbl_df(df) %>% group_by(inputterm) %>% top_n(topFreq,freq)
    
    print(paste0("Save data to ",saveToFolder,"/",savePrefix,i,".txt"))
    df <- as.matrix(df)
    write.table(df, paste0(saveToFolder,"/",savePrefix,i,".txt"),row.names = FALSE) 
    rm(df)
    print(Sys.time()-sub.start.time)
  }
  
  print(Sys.time()-start.time)
}

##---Process Extract frequent Terms (freq > lowestFreq) and additional clean----
#Remove Stop Words dataset
extract_frequent_term(readFromFolder="quagrams",saveToFolder ="quagrams",readPrefix="qua_",savePrefix ="quaFinal_")
extract_frequent_term(readFromFolder="trigrams",saveToFolder ="trigrams",readPrefix="tri_",savePrefix ="triFinal_")
extract_frequent_term(readFromFolder="bingrams",saveToFolder ="bingrams",readPrefix="bin_",savePrefix ="binFinal_")

#Not Remove Stop Words dataset
extract_frequent_term(readFromFolder="quagram",saveToFolder ="quagram",readPrefix="qua_",savePrefix ="quaFinal_")
extract_frequent_term(readFromFolder="trigram",saveToFolder ="trigram",readPrefix="tri_",savePrefix ="triFinal_")
extract_frequent_term(readFromFolder="bingram",saveToFolder ="bingram",readPrefix="bin_",savePrefix ="binFinal_")

##---Function (Obsoleted) Extract most n frequent Predict of each input Term------
extract_most_frequent_predict <- function(fromLetter=1,toLetter=26,readFromFolder,saveToFolder,readPrefix,savePrefix,topn=3){
  start.time <- Sys.time()
  
  for(i in letters[fromLetter:toLetter]){
    sub.start.time <-Sys.time()
    print(paste0("start load file ",readFromFolder,"/",readPrefix,i,".txt"))
    df <- data.table(read.table(file=paste0(readFromFolder,"/",readPrefix,i,".txt"),header = TRUE))
    df <- tbl_df(df) %>% group_by(inputterm) %>% top_n(topn,freq)
    df<-as.matrix(df)
    write.table(df,file=paste0(saveToFolder,"/",savePrefix,i,".txt"),row.names=FALSE)
  }
}

##---Process (Obsoleted) Extract most 3 frequent Predict of each input Term-----
#Remove Stop Words dataset
extract_most_frequent_predict(readFromFolder = "quagrams",saveToFolder = "finaldatas",readPrefix = "quaFinal_",savePrefix = "quaFinal_")
extract_most_frequent_predict(readFromFolder = "trigrams",saveToFolder = "finaldatas",readPrefix = "triFinal_",savePrefix = "triFinal_")
extract_most_frequent_predict(readFromFolder = "bingrams",saveToFolder = "finaldatas",readPrefix = "binFinal_",savePrefix = "binFinal_")

#Not Remove Stop Words dataset
extract_most_frequent_predict(readFromFolder = "quagram",saveToFolder = "finaldata",readPrefix = "quaFinal_",savePrefix = "quaFinal_")
extract_most_frequent_predict(readFromFolder = "trigram",saveToFolder = "finaldata",readPrefix = "triFinal_",savePrefix = "triFinal_")
extract_most_frequent_predict(readFromFolder = "bingram",saveToFolder = "finaldata",readPrefix = "binFinal_",savePrefix = "binFinal_")

##---Process Build .RData variable----------
#Function build data.table variable
buildVariable <- function(inputFolder,inputPrefix){
  variable <- list()
  for(i in letters[1:26]){
    df <- data.table(read.table(file=paste0(inputFolder,"/",inputPrefix,i,".txt"),header = TRUE))
    df$inputterm <- as.character(df$inputterm)
    df$predict <- as.character(df$predict) 
    df$freq <- as.numeric(df$freq)
    variable[[i]] <- df
  }
  variable
}

#Remove Stop Words dataset
triFinalRSW <- list()
binFinalRSW <- list()
uniFinalRSW <- list()

triFinalRSW <- buildVariable("rswdata", "quaFinal_")
binFinalRSW <- buildVariable("rswdata", "triFinal_")
uniFinalRSW <- buildVariable("rswdata", "binFinal_")

#Not Remove Stop Words dataset
triFinalALL <- list()
binFinalALL <- list()
uniFinalALL <- list()

triFinalALL <- buildVariable("finaldata", "quaFinal_")
binFinalALL <- buildVariable("finaldata", "triFinal_")
uniFinalALL <- buildVariable("finaldata", "binFinal_")

save(list = c("triFinalRSW","binFinalRSW","uniFinalRSW","triFinalALL","binFinalALL","uniFinalALL"),file = "prgData.RData")

