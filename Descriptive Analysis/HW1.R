print ("Homework 1")
print ("Wenjie Sun (ws854)")
rm(list = ls())

## Set working directory
setwd("/Users/sunevan/Dropbox/Spring 2017/Text As Data/Assignment/HW1data")

library(quanteda)
library(boot)
library(ggplot2)
library(dplyr) 
library(quantedaData)

##1(a)
df <- inaugCorpus

dff <- c(df[54],df[56])

tokens_original <- tokenize(dff,removePunct = FALSE) 
tokenz_original <- lengths(tokens_original)
typez_original <- ntype(tokens_original) 
ttr_original <- typez_original / tokenz_original 
print ("Without Removing Punctuation:") 
ttr_original


tokens <- tokenize(dff,removePunct = TRUE) 
tokenz <- lengths(tokens)
typez <- ntype(tokens) # ntypes function of tokens 
ttr <- typez / tokenz # longer doc, lower TTR (as more repetitaive tokens)
print ("After Removing Punctuation:") 
ttr


## 1(b)
dff1 <- dfm(dff, tolower = FALSE, stem = FALSE, select = NULL, remove = NULL,thesaurus = NULL, dictionary = NULL,removePunct = TRUE)
print(head(dff1))

## 1(c)
cos_sim_1 <- similarity(dff1, n = NULL, margin = "documents", method = "cosine")
print (as.matrix(cos_sim_1))

## 1(d) - Stemming 
print("Thought: After stemming, the total number of tokens remains unchanged, but the total number of types will be decreased. So, I think TTR will decrease. I think similarity will be almost the same since distance should be almost the same when you think running-running+run-run (non-stemming) & run-run(stem)")
stems <- tokens_wordstem(tokens)
ttr_stems <- ntype(stems) / lengths(stems)
print (ttr_stems)

dff2 <- dfm(dff, tolower = FALSE, stem = TRUE, select = NULL, remove = NULL,thesaurus = NULL, dictionary = NULL,removePunct = TRUE)
cos_sim_2 <- similarity(dff2, n = NULL, margin = "documents", method = "cosine")
print(as.matrix(cos_sim_2))

## 1(d) - remove stopwords
print("Thought: I think TTR will be increased since it is going to remove a lot of repeated stop words (total number of tokens will be decreased a lot). However, the type will be only decreased slightly. Similary will be decreased becasue stopwords are majorly the same")
token_remove <- tokens_remove(tokens,stopwords("english"))
ttr_removestopwords <- ntype(token_remove) / lengths(token_remove)
print(ttr_removestopwords)

dff3 <- dfm(dff, tolower = FALSE, stem = FALSE, select  = NULL, remove = stopwords("english"),thesaurus = NULL, dictionary = NULL,removePunct = TRUE)
cos_sim_3 <- similarity(dff3, n = NULL, margin = "documents", method = "cosine")
print(as.matrix(cos_sim_3))

## 1(d) - lowercase 
print("I think TTR will slightly decrease since the length remains no change, but type will be decreased slightly. Similarity should be almost the same as lower case doesn't really change the count")
token_tolower <- tokens_tolower(tokens)
ttr_lower <-  ntype(token_tolower) / lengths(token_tolower)
print(ttr_lower)

dff4 <- dfm(dff, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,thesaurus = NULL, dictionary = NULL,removePunct = TRUE)
cos_sim_4 <- similarity(dff4, n = NULL, margin = "documents", method = "cosine")
print(as.matrix(cos_sim_4))

## 1(d) -tf-idf
## remove punctation 
weighted_dfm_1 <- tfidf(dff1,normalize =  TRUE)
print(topfeatures(weighted_dfm_1))

## stemming & remove punctation 
weighted_dfm_2 <- tfidf(dff2,normalize =  TRUE)
print(topfeatures(weighted_dfm_2))

## removed stop words & remove punctation 
weighted_dfm_3 <- tfidf(dff3,normalize =  TRUE)
print(topfeatures(weighted_dfm_3))

## lowercase & remove punctation 
weighted_dfm_4 <- tfidf(dff4,normalize =  TRUE)
print(topfeatures(weighted_dfm_4))

## remove punctation, stemming, removed stop words, lowercase
dff5 <- dfm(dff, tolower = TRUE, stem = TRUE, select  = NULL, remove = stopwords("english"),thesaurus = NULL, dictionary = NULL,removePunct = TRUE)
weighted_dfm_5 <- tfidf(dff5,normalize =  TRUE)
print(topfeatures(weighted_dfm_5))

print ("It doesn't make sense. Since we only have 2 documents, so it either appers in both documents or one of them.The top features also show the similar conculsion")

## 1(e):


mltd_homebrew<-function(text, ttr = 0.72, returnvalues = F){
  require(quanteda)
  keeps<-list()
  dat<-tokenize(text,what = "word",removePunct = T) #Tokenizes the speeches
  dat<-toLower(dat) #Changes the text to lower case
  for(i in 1:length(text)){
    keeps[[i]]<-NA #Creates a vector for each speech
    temp<-list()
    type<-0
    t<-0
    for(k in 1:length(dat[[i]])){
      type<-ifelse(dat[[i]][k] %in% unlist(temp),yes = type+0, no = type+1) #Token type counter: Adds one if the new word doesn't match an old word
      temp[k]<-unlist(dat[[i]][k]) #Adds the word to a list of tokens
      if(type/length(unlist(temp))<ttr){
        t<-t+1 #Keeper counter: designed to increase as the TTR is reached
        keeps[[i]][t]<-length(unlist(temp)) #Stores a count of the number of words passed before the TTR was hit
        temp<-list() #Clears the token list
        type<-0 #Clears the type counter
      }
    }
  }
  ifelse(returnvalues == T,
         final<-list(data = keeps, 
                     means = lapply(keeps, function(x) mean(x))), #Returns lists of the means and the word counts
         final<-list(means = lapply(keeps, function(x) mean(x))) #Returns a list of the mean MLTD by document
  )
  return(final)
}
mltd_homebrew(dff)


# Q2
Q2 <- c(textA= "Whenever you find you are on the side of the majority, it is time to pause and reflect.",
        textB = "A jury consists of twelve people who determine which client has the better lawyer.")
Q2dfm <- dfm(Q2, tolower = TRUE, stem = FALSE,removePunct = TRUE)
# Since the sentence is relatively short, I think stem is not necessary. However, I think to make everything to a lowercase is better. 
Q2matrix <- as.data.frame(Q2dfm)
# Manhattan Distance
manhattansum = 0 
for(i in 1:ncol(Q2matrix)){
  manhattansum = manhattansum + abs(Q2matrix[1,i] - Q2matrix[2,i])
}

manhattan_distance = manhattansum
print(manhattan_distance)

# Euclidean
euclideansum = 0 
for(i in 1:ncol(Q2matrix)){
  euclideansum = euclideansum + (Q2matrix[1,i] - Q2matrix[2,i])^2
}

euclidean_distance = sqrt(euclideansum)
print(euclidean_distance)

# Cosine 
norm_sum = 0 
norm_a = 0
norm_b = 0 
for(i in 1:ncol(Q2matrix)){
  norm_sum = norm_sum + (Q2matrix[1,i] * Q2matrix[2,i])
  norm_a = norm_a + (Q2matrix[1,i] * Q2matrix[1,i])
  norm_b = norm_b + (Q2matrix[2,i] * Q2matrix[2,i])
}


cosine_similarity = norm_sum / (sqrt(norm_a)*sqrt(norm_b))

print(cosine_similarity)

# Q3 
#3(a)
print("One of the methodological assumption is that function word is indenpendent of the text since it is used unconsciously by the author. However, the assumption might be changed if the texts were written in different time, since writing style is often changed upon the culture and age")


#3(b)

setwd("/Users/sunevan/Dropbox/Spring 2017/Text As Data/Assignment/HW1data")
files <- list.files(full.names = TRUE)
text <- lapply(files,readLines)
text <- unlist(lapply(text,function (x) paste(x,collapse = " ")))

files <- unlist(files)
files <-gsub ('./','',files)
files <-gsub('.txt','', files)

author <- gsub("\\_.*","",files)

man_df <- data.frame( text = text)

Q3dfm <- dfm(as.matrix(man_df),select = c("the","may","it","was","which","not","be","upon"))

Q3matrix <- as.data.frame(Q3dfm)

print(Q3matrix)

#3(c)
austen <- numeric()
dickens <- numeric()
mystery <- numeric ()

for (i in 1:ncol(Q3matrix)) {
  # total number of occurance of a particular term in all documents by the same author / total number of tokens  
  austen[i] <- (Q3matrix[1,i]+ Q3matrix[2,i]+Q3matrix[3,i]+Q3matrix[4,i]+Q3matrix[5,i]) / (sum(Q3dfm[1]) + sum(Q3dfm[2]) + sum(Q3dfm[3])+sum(Q3dfm[4])+sum(Q3dfm[5]))
  dickens[i] <- (Q3matrix[6,i]+ Q3matrix[7,i]+Q3matrix[8,i]+Q3matrix[9,i]+Q3matrix[10,i]) / (sum(Q3dfm[6]) + sum(Q3dfm[7]) + sum(Q3dfm[8])+sum(Q3dfm[9])+sum(Q3dfm[10]))
  mystery[i] <- (Q3matrix[11,i]/sum(Q3dfm[11]))
}

### 
# 
library("reshape2")
dat1 <- cbind(as.data.frame(featnames(Q3dfm)),as.data.frame(austen), as.data.frame(dickens),as.data.frame(mystery))
colnames(dat1) <- c("Terms","Austen","Dickens","Mystery")
melted = melt(dat1,id.vars = "Terms")

ggplot(data=melted, aes(x=Terms, y=value, group=variable,color=variable)) + geom_line()

## 3(d)
Austen_myster = 0 
for (i in 1:nrow(dat1)) {
  Austen_myster = Austen_myster + abs (dat1[i,2]-dat1[i,4])
}
Austen_myster

Dickens_myster = 0 
for (i in 1:nrow(dat1)) {
  Dickens_myster = Dickens_myster + abs (dat1[i,3]-dat1[i,4])
}
Dickens_myster

Dickens_myster/Austen_myster 
print("Answer: more likely to be Dickens")

## Q4 
Q3dfm_1 <- dfm(as.matrix(man_df),tolower= TRUE) # the whole DFM 
Q4dfm <- Q3dfm_1[1:10]

# Zipf's law
plot(log10(1:100),log10(topfeatures(Q4dfm,100)),xlab="log10(rank)",ylab="log10(frequency)",main = "Top 100 Words")

# Heap's law
VocabSize = nfeature(Q4dfm)
Length = sum(Q4dfm)

best_b<-log(VocabSize/44)/log(Length)

HeapsLaw <- 44*(Length^best_b)
print (HeapsLaw)
print(VocabSize)
best_b

# Q5 
text1 <- text[1:10]
kwic(text1,"slave",window = 5)
print("Slave only apperas 6 times in Austen and 16 times in Dickens.")
kwic(text1,"revolution",window = 5)
print("Revolution only apperas 3 times in Austen and 13 times in Dickens.")
kwic(text1,"sacrifice",window = 5)
print("Revolution only apperas 27 times in Austen and 37 times in Dickens.")
kwic(text1,"mission",window = 5)
print("Revolution only apperas 0 times in Austen and 23 times in Dickens.")


kwic(text1,"marriage",window = 5)
print("Revolution only apperas 183 times in Austen and 17 times in Dickens.")
kwic(text1,"engagement",window = 5)
print("Revolution only apperas 166 times in Austen and 25 times in Dickens.")

print("In summary, I think Tale of Two Cities is more serious and uses more serious words. In contrast, Pride and Prejudice is more a romantic novel and use words marriage and engagement more often")

# Q6 
# 6(a)
library(boot)
data("SOTUCorpus",package = "quantedaData")
ndocs <- ndoc(SOTUCorpus)

SOTUCorpSub <- corpus_subset(SOTUCorpus,!FirstName %in% "Grover")

SOTU <- corpus_reshape(SOTUCorpSub, to = "sentences")


df <- data.frame(texts = SOTU[["texts"]]$texts, 
                 president = SOTU[["President"]]$President,
                 FirstName = SOTU[["FirstName"]]$FirstName,
                 stringsAsFactors = F) 
df$PresidentName = paste(df$FirstName, df$president, sep="_")

df<-filter(df, grepl("^\\?", df$texts)==FALSE &grepl("^\\d", df$texts) ==FALSE)
df<-filter(df, ntoken(df$texts)>3)

df_time <- data.frame(
  president = SOTU[["President"]]$President,
  FirstName = SOTU[["FirstName"]]$FirstName,
  Txtname = SOTU[["filename"]]$filename,
  stringsAsFactors = F) 
df_time$PresidentName = paste(df_time$FirstName, df_time$president, sep="_")
df_time$Year = substring(df_time[,3], 3, 6) 
#Generate a time table to map with main df
df_time1 <-
  df_time %>% 
  group_by(PresidentName) %>% 
  slice(which.min(Year))

df_time2<-  df_time1[with(df_time1, order(Year)), ]  

df_final <- merge(df, df_time1, by.x = "PresidentName", by.y = "PresidentName")

# clean up the dataframe 
df1 <- data.frame(PresidentName = df_final$PresidentName,
                  texts = df_final$texts,
                  Year = as.numeric(df_final$Year),stringsAsFactors = F)

yearmatrix = as.matrix(data.frame(Year = df_time1$Year,stringsAsFactors = F))

# FRE - Bootstraps 
president_FRE <- data.frame(matrix(ncol = 40, nrow = 100))
# name the dataframes 
colnames(president_FRE) <- yearmatrix
# boostrap -> since the sample is really big on sentence level, the boostrap is done by each president instead of across the whole cropus 
for(i in 1:100){
  for(j in 1:nrow(yearmatrix)){
    df_inuse <- filter(df1,Year == yearmatrix[j])
    bootstrapped<-sample_n(df_inuse,20,replace=TRUE)
    bootstrapped$read_FRE<-textstat_readability(bootstrapped$texts,"Flesch")
    president_FRE[i,j] <- aggregate(bootstrapped$read_FRE,by=list(bootstrapped$Year),FUN=mean)[,2]
  } 
}
president_FRE_order <- president_FRE[ , order(colnames(president_FRE))]  
# unsample FRE


president_FRE_unsample <- data.frame(matrix(ncol = 40))

for(j in 1:nrow(yearmatrix)){
  bootstrapped <- filter(df1,Year == yearmatrix[j])
  bootstrapped$read_FRE<-textstat_readability(bootstrapped$texts,"Flesch")
  president_FRE_unsample[j] <- aggregate(bootstrapped$read_FRE,by=list(bootstrapped$Year),FUN=mean)[,2]
} 

colnames(president_FRE_unsample) <- yearmatrix

president_FRE_unsample_order <- president_FRE_unsample[ , order(colnames(president_FRE_unsample))] 


# define the stand errors 
std <- function(x) sd(x)/sqrt(length(x))
president_means <-apply(president_FRE_order,2,mean)
president_ses <- apply(president_FRE_order,2,std)

coefs <- president_means
ses<-president_ses 

x.axis <- c(1:40)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(president_FRE_order)
adjust <- 0
par(mar=c(2,8,2,2))

plot( x.axis,coefs, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
      xlim=c(.5,40.5),ylim = c(min,max), main = "")

axis(2, at = seq(min,max,(max-min)/10), 
     labels = c(round(min+0*((max-min)/10),3),
                round(min+1*((max-min)/10),3),
                round(min+2*((max-min)/10),3),
                round(min+3*((max-min)/10),3),
                round(min+4*((max-min)/10),3),
                round(min+5*((max-min)/10),3),
                round(min+6*((max-min)/10),3),
                round(min+7*((max-min)/10),3),
                round(min+8*((max-min)/10),3),
                round(min+9*((max-min)/10),3),
                round(max,3)),tick = T,cex.axis = .75, mgp = c(2,.7,0))
axis(1, at = x.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8)
abline(h = x.axis, lty = 2, lwd = .5, col = "white")
segments(x.axis+2*adjust,coefs-qnorm(.975)*ses,x.axis+2*adjust,  coefs+qnorm(.975)*ses,  lwd =  1)
segments(x.axis+2*adjust-.035,coefs-qnorm(.95)*ses, x.axis+2*adjust+.035, coefs-qnorm(.95)*ses,  lwd = .9)
segments(x.axis+2*adjust-.035,coefs+qnorm(.95)*ses,x.axis+2*adjust+.035,  coefs+qnorm(.95)*ses,  lwd = .9)
points( x.axis+2*adjust,coefs,pch=21,cex=.8, bg="white")
points(x.axis+2*adjust,president_FRE_unsample_order,pch=21,cex=.8,bg="black")


# 6(b)
print ("Most of the datapoints have no significant difference. When it is unsampled, the FRE is simply the emprical mean of their 4/8 years speach. If the unsampled data is in the middle of the distribution, it means this president is very stable in terms of using the lanaguge. However, it the sampled mean is higher or lower than unsampled mean, it seems that some of his speech is more diffcult or less.")

# 6(c)
president_compare <- data.frame(matrix(ncol = 40, nrow = 2))

for(j in 1:nrow(yearmatrix)){
  bootstrapped <- filter(df1,Year == yearmatrix[j])
  bootstrapped$read_FRE<-textstat_readability(bootstrapped$texts,"Flesch")
  president_compare[1,j] <- aggregate(bootstrapped$read_FRE,by=list(bootstrapped$Year),FUN=mean)[,2]
  bootstrapped$read_DC<-textstat_readability(bootstrapped$texts,"Dale.Chall")
  president_compare[2,j] <- aggregate(bootstrapped$read_DC,by=list(bootstrapped$Year),FUN=mean)[,2]
} 

colnames(president_compare) <- yearmatrix

president_compare_order <- president_compare[ , order(colnames(president_compare))] 

plot(c(1:40),president_compare_order[1,],type = "l",title(main = "FRE - By Year"))
plot(c(1:40),president_compare_order[2,],type = "l",title(main = "Dale Chall - By Year"))
print(cor(t(president_compare_order[1,]), t(president_compare_order[2,])))
print ("Perfectly Correlated")


