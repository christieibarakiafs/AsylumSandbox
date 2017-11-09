##data acquired through the work of Clough, P. and Stevenson, M.
##[Citation: Clough, P. and Stevenson, M. Developing A Corpus of Plagiarised Short 
##Answers, Language Resources and Evaluation: Special Issue on Plagiarism and Authorship Analysis, In Press.]

#Libraries
library(knitr)
opts_chunk$set(message=FALSE, warning=FALSE)
library(textreuse)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(purrr)
library(scales)
library(numbers)

#Load datasets
theLabels <- read.csv("theLabels.csv",stringsAsFactors = F)
## file
#file <- "g0pA_taska.txt"
## create connection
#con <- file(file,open="r")
## extract lines
#this_doc <- readLines(con)
#close(con)

#Load multiple datasets and store them into my_corpus
list_names = list.files(pattern="*.txt")
my_corpus=list()
for(i in 1:length(list_names)){
  fileName <-list_names[i]
  con <-file(fileName,open="r")
  this_doc <-readLines(con)
  my_corpus[[i]]=this_doc
  close(con)
}

#Remove special characters in the dataset
rm_spec_char = function(my_corpus){
  n = length(my_corpus)
  clean_corpus = my_corpus
  for(i in 1:n){
    the_line = gsub("([^a-zA-Z0-9])"," ",my_corpus[i]) #Keep non-special characters
    the_line = gsub(" +"," ",the_line) #remove double white spaces
    the_line = tolower(the_line) #convert to lower cases
    clean_corpus[i]= the_line
  }
  return(clean_corpus)
}

##apply rm_spec_char function to my_corpus(the list)
my_corpus_cl<-lapply(my_corpus,rm_spec_char)
my_corpus_cl<-lapply(my_corpus_cl,function(x) { paste(x,collapse="")})
names(my_corpus_cl) = gsub(".txt\\b","",list_names) #give each element in the list the name; match exact ".txt" at the edge of the word

#convert the list to a text format to feed into TextReuseTextDocument()
corpus = unlist(my_corpus_cl)

#make shingles (k=3, 5) from the corpus
corpus_shin3 <- TextReuseCorpus(text = corpus,
                          tokenizer = tokenize_ngrams, n = 3,
                          progress = FALSE,
                          keep_tokens = TRUE)
corpus_shin5 <- TextReuseCorpus(text = corpus,
                                tokenizer = tokenize_ngrams, n = 5,
                                progress = FALSE,
                                keep_tokens = TRUE)

#LOCALITY SENSITIVE HASHING
# look at probability of binned together for various bin sizes and similarity values
m=360   #the number of iteration
tibble(s = c(.25, .75), h = m) %>% # look at two different similarity values
  mutate(b = (map(h, divisors))) %>% # find possible bin sizes for m
  unnest() %>% # expand dataframe
  group_by(h, b, s) %>%
  mutate(prob = lsh_probability(h, b, s)) %>%
  ungroup() -> bin_probs # get probabilities
# plot as curves
bin_probs %>%
  mutate(s = factor(s)) %>%
  ggplot() +
  geom_line(aes(x = prob, y = b, colour = s, group = s)) +
  geom_point(aes(x = prob, y = b, colour = factor(s)))
# look as some candidate b
bin_probs %>%
  spread(s, prob) %>%
  select(-h) %>%
  filter(b > 50 & b < 200)
#let's choose b=90 (why not b=60 or b=72 ??)
#Using m=360 minhashes and 90 bands, we will likely detect documents with an actual
#  Jaccard similarity of above 0.325
lsh_threshold(h = m, b = 90) # 0.325

# create the minhash function
minhash <- minhash_generator(n = m, seed = 09142017)
# add it to the corpus
corpus <- rehash(corpus, minhash, type="minhashes")
# perform lsh to get buckets
b=90   # chosen as the optimal parameter
buckets <- lsh(corpus, bands = b, progress = FALSE)
# grab candidate pairs
candidates <- lsh_candidates(buckets)
# get Jaccard similarities only for candidates
sim_lsh <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE) %>%
  arrange(desc(score)) 

#EVALUATION
#LSH (Recall & Precision)
recall_prec_output = function(corpus_kshin,m,num_tp){
  minhash <- minhash_generator(n = m, seed = 09142017)
  myCorpus <- rehash(corpus_kshin, minhash, type="minhashes")
  b <- divisors(m)   # chosen as the optimal parameter
  key <- tibble(topics=c("a","b","c","d","e"),a_label=c("a","b","c","d","e"),total_doc=rep(20,5))
  myRecall <- c()
  myPrec <- c()
  for(i in 1:length(b)){
    buckets <- lsh(myCorpus, bands = b[i], progress = FALSE)
    candidates <- lsh_candidates(buckets)
    new_df <- as.data.table(candidates)
    new_df[,c("a_label","b_label","value"):=list(str_sub(a,-1,-1),str_sub(b,-1,-1),1)]
    new_df <- new_df[,.(num = sum(value)),by=c("a","a_label","b_label")]
    new_df <- data.table(merge(key,new_df,all.x=T,by="a_label"))
    myRecall[i] <- mean(new_df[which(new_df$topics==new_df$b_label),]$num/num_tp) #different topics gets different recall, so for now just get the average recall for all topics
    sortByTopic <- new_df[,.(totalCnt=sum(num)),by=c("topics","b_label")]
    sumByTopic <- sortByTopic[,.(sums=sum(totalCnt)),by=c("topics")] #extract total predicted counts in each topic
    matchByTopic <- sortByTopic[which(sortByTopic$topics==sortByTopic$b_label),] #extract only the counts of the exact matched (a=a,b=b...etc) pairs
    mergeByTopcis <- merge(matchByTopic,sumByTopic,all.y=T,by="topics")
    myPrec[i] <-  mean(mergeByTopcis$totalCnt/mergeByTopcis$sums,na.rm=T) #return average precision of all 5 topics
  }
  return(list(recall=myRecall,precision=myPrec,bands=b))
}

# Extract the recall & precision from various k-shingles & # of iterations
shin3_m360 <- recall_prec_output(corpus_shin3,m=360,num_tp=20)
shin5_m360 <- recall_prec_output(corpus_shin5,m=360,num_tp=20)
shin3_m720 <- recall_prec_output(corpus_shin3,m=720,num_tp=20)
shin5_m720 <- recall_prec_output(corpus_shin5,m=720,num_tp=20)

# plot the Recall and Precision curves
shinK_m <- shin5_m720
title <- "LSH \n5-shingles with 720 iterations"
subtitle <- "recall"
thePar <- par(mfrow=c(2, 2)) #saved the option of plotting the graphs side by side
plot(shinK_m$bands,shinK_m$recall,type='b',pch=16,col="blue",lwd=1.5,cex=1.5,
     cex.axis=1.5,cex.lab=1.2,cex.main=1.5,ylim=c(0,1),ylab = "mean of recalls(Precisions) from 5 topics", xlab="bands",main=title)
par(new=T)
plot(shinK_m$bands,shinK_m$precision,type='b',pch=15,col="red",lwd=1.5,cex=1.5,
     axes=F,cex.axis=1.5,cex.lab=1.2,cex.main=1.5,ylab = "", xlab="")
legend("bottom", 
       legend = c("Recall", "Precision"), 
       col = c("blue", "red"), 
       pch = c(16,15), 
       bty = "n", 
       pt.cex = 1.2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = T , 
       inset = c(0.01, 0.01))
par(thePar)




#----------------------------------------------------------------------------------------
#Create shingles
##k=3
my_shingles_3 <- lapply(my_corpus_cl,function(x) {tokenize_ngrams(x,n=3)})

#Create a FUN computing jaccard similarity before hashing; served as baseline
jaccard_sim_compute<-function(my_shingles){
  the_seq <-c(1:length(my_shingles)) #check how many documents are there
  sim_matrix <- matrix(nrow=length(my_shingles),ncol=length(my_shingles)) #create a matrix to store similarity values
  
  for(i in 1:length(my_shingles)){ #start comparing doc to doc
    if(i<length(my_shingles)){ #no need to compare the last doc with others coz it's already compared with everyone else in the loop
      s = the_seq[the_seq>i]
      target <- my_shingles[[i]]
      
      for(j in s){
        other <- my_shingles[[j]]
        jacc_sim <- jaccard_similarity(target,other)
        sim_matrix[i,j] = jacc_sim
      }
    }
  }
  return(sim_matrix)
}

#Let's compute our baseline with various k-shingles!
sim_shingles3 = jaccard_sim_compute(my_shingles_3)

#Ploting the jacc sim matrix
##extract rownames/colnames
ppl_names = gsub("_taska.txt\\b","",list_names) #match exact ".txt" at the edge of the word
my_labels = c(theLabels[theLabels$Task=="a",]$Category,"org") #add labels
these_doc_names = paste(ppl_names,my_labels,sep="_")
##adding rownames/colnames to the matrix
rownames(sim_shingles3) <- these_doc_names; colnames(sim_shingles3) <- these_doc_names
##ggploting
sim_shingles3_lng = melt(sim_shingles3)#need to convert to long data
sim_shingles3_lng = sim_shingles3_lng[!is.na(sim_shingles3_lng$value),]

k = 3 #input the k of the shingles

ggplot(sim_shingles3_lng, aes(x = Var1, y = Var2)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Docs from Topic A", y="Docs from Topic A", title=paste("Jaccard Similarity Matrix with ",k," shingles",sep="")) +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#Performance metric for the 4 plagirarism levels (cut,heavy rev,light rev,non plag)
perf_stat = function(sim_metric,k,title){
  #compute descriptive stats
  sim_metric_lng = melt(sim_metric)
  sim_metric_lng = sim_metric_lng[!is.na(sim_metric_lng$value),]
  sim_metric_lng[,c("Var1","Var2")] = lapply(sim_metric_lng[,c("Var1","Var2")],as.character)
  mydf = data.table(sim_metric_lng)
  mydf[,c("Var1_lab","Var2_lab"):=.(gsub("[a-zA-Z0-9]+_","",Var1),gsub("[a-zA-Z0-9]+_","",Var2))]
  mydf_stat = mydf[,.(plag_mean=mean(value),plag_sd=sd(value)), by = list(Var1_lab,Var2_lab)]
  #plot
  g = ggplot()+geom_jitter(data=mydf,aes(x=Var1_lab,y=value),size=2,width=0.25)+
    ggtitle(paste(title,"with ",k,"-shingles",sep="")) +
    labs(x="Plagirarsim category",y="Jaccard similarity value") + 
    facet_wrap(~Var2_lab)#+ 
  #theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12, hjust=0)) +
  #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) 
  
  return(list(stats=mydf_stat,plot=g))
}

temp=perf_stat(sim_shingles3_lng,k=3,title="Jaccard similarity")

#MINHASH
# Test out the manual hashing
my_shingles_3 %>%
  map(hash_string) -> my_hashes_shin3
names(my_hashes_shin3) = names(my_shingles_3) #give each element in the list the name; match exact ".txt" at the edge of the word

# Return if an item is in a list
item_in_list <- function(item, list) {
  as.integer(item %in% list)
}
# Get the characteristic matrix
# items are all the unique hash values
# lists will be each song
# we want to keep track of where each hash is included
data.frame(item = unique(unlist(my_hashes_shin3))) %>%
  group_by(item) %>%
  mutate(list = list(names(my_shingles_3))) %>% # include the song name for each item
  unnest(list) %>% # expand the list column out
  # record if item in list
  mutate(included = (map2_int(item, my_hashes_shin3[names(my_hashes_shin3) == list], item_in_list))) %>%
  spread(list, included) -> char_matrix # tall data -> wide data (see tidyr documentation)
# inspect the first 10 lines results
as.data.frame(char_matrix)[1:20,]

#Get the signature matrix
# set seed for reproducibility
set.seed(09142017)
# get permutation order
permute_order <- sample(seq_len(nrow(char_matrix)))
# get min location of "1" for each column (apply(2, ...))
sig_matrix <- char_matrix[permute_order, -1] %>%  #each row in sig_matrix is from each permutation of "as.data.frame(char_matrix[permute_order, -1])[1:20,]"
  apply(2, function(col) min(which(col == 1))) %>%
  as.matrix() %>% t()
# function to get signature for 1 permutation
get_sig <- function(char_matrix) {
  # get permutation order
  permute_order <- sample(seq_len(nrow(char_matrix)))
  # get min location of "1" for each column (apply(2, ...))
  char_matrix[permute_order, -1] %>%
    apply(2, function(col) min(which(col == 1))) %>%
    as.matrix() %>% t()
}
# repeat many times
m <- 360 #num of repetition
for(i in 1:(m - 1)) {
  sig_matrix <- rbind(sig_matrix, get_sig(char_matrix))
}
# add labels on the column name in sig_matrix
colnames(sig_matrix) <- these_doc_names
# add jaccard similarity approximated from the minhash
#We look down each column of the signature matrix, and compare it to any other column.
# Then compare number of agreements over the total number of combinations
jacc_sim_minhash = apply(sig_matrix,2,function(x) colSums(x==sig_matrix)/nrow(sig_matrix))
# how far off is this approximation? (standardizing the two simliarity dataframe -> order them -> substract)
jacc_sim_minhash_half = jacc_sim_minhash
jacc_sim_minhash_half[lower.tri(jacc_sim_minhash_half)] <- NA
diag(jacc_sim_minhash_half) <- NA
jacc_sim_minhash_half_lng = melt(jacc_sim_minhash_half)
jacc_sim_minhash_half_lng = jacc_sim_minhash_half_lng[!is.na(jacc_sim_minhash_half_lng$value),]

jacc_sim_minhash_half_lng = jacc_sim_minhash_half_lng[order(jacc_sim_minhash_half_lng$Var2,jacc_sim_minhash_half_lng$Var1),]
sim_shingles3_lng = sim_shingles3_lng[order(sim_shingles3_lng$Var2,sim_shingles3_lng$Var1),]

summary(abs(sim_shingles3_lng$value - jacc_sim_minhash_half_lng$value))

#//doc_sim <- doc_sim %>%
#//  group_by(doc1, doc2) %>%
#//  mutate(jaccard_sim_minhash =
#//          sum(sig_matrix[, doc1] == sig_matrix[, doc2])/nrow(sig_matrix))
#// how far off is this approximation?
#//summary(abs(song_sim$jaccard_sim_minhash - song_sim$jaccard_sim))

#sig_matrix %>%
#  as_tibble() %>%
#  mutate(bin = rep(1:b, each = m/b)) %>% # add bins
#  gather(song, hash, -bin) %>% # tall data instead of wide
#  group_by(bin, song) %>% # within each bin, get the min-hash values for each song
#  summarise(hash = paste0(hash, collapse = "-")) %>%
#  ungroup() -> binned_sig



# LSH USING TEXTREUSE BUILT IN FUNCTIONS
# create the minhash function
minhash <- minhash_generator(n = m, seed = 09142017)
# add it to the corpus
corpus <- rehash(txtre_obj, minhash, type="minhashes")
# perform lsh to get buckets
buckets <- lsh(corpus, bands = b, progress = FALSE)
# grab candidate pairs
candidates <- lsh_candidates(buckets)
# get Jaccard similarities only for candidates
lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE) %>%
  arrange(desc(score)) 


#Topic modeling(?)



boo=c()
for (i in 1:length(my_hashes_shin3)){
  boo[i] = -2140221108 %in% my_hashes_shin3[[i]]
}
