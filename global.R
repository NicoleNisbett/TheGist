# Global file
#will move to MastermindV2 folder once testing is done (not a good idea sis...)
#Libraries----

#for shiny
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(dashboardthemes)
library(shinyalert)

#plotting
library(ggplot2)
library(LDAvis)
library(plotly)

#misc
library(dplyr)
library(igraph)
library(tidytext)
library(tidyr)
library(reshape2)
library(ggraph)
library(data.table)
library(readr)
library(tm)
library(corpus)
library(topicmodels)
library(readxl)
library(data.table)
library(devtools)
library(magrittr)
library(udpipe)
library(wordcloud)
library(wordcloud2)
library(quanteda)
library(lubridate)
library(BTM)
library(DT)
library(broom)
library(textdata)
library(stringr)


#source("appModules.R")
## read file

#get all files from the folder into a list
# load_data <- function(path="/Users/nicolenisbett/Documents/PhD/R/Complete Analysis/MastermindV2/Datasets") {
#   files <- list.files(path, pattern = '.csv', full.names = TRUE)
#   tables <- lapply(files, read_csv)
#   #unlisted=rbindlist(tables)
#   return(tables)
# }
# 
# debatetest=load_data()
# 
# 
# #function to change any other function into one that works for lists, from https://www.brodrigues.co/blog/2016-12-21-work-on-lists-of-datasets-instead-of-individual-datasets-by-using-functional-programming/
# to_map = function(func){
#   function(list, column, ...){
#     if(missing(column)){
#       res = purrr::map(list, (function(x) func(x, ...)))
#     }
#     else{
#       res = purrr::map(list, (function(x) func(x[column], ...)))
#     }
#     res
#   }
# }
# 
# summarymap=to_map(summary)
# summarymap(debatetest, "Likes")
# 
# hoursmap=to_map(get_hours_fb)
# #make sure to keep the second argument blank or won't work properly
# hoursmap(debatetest, ,"testing debate title")

#for each file, create all the necessary r objects to be inputted into plotting functions in server.
# for (i in list){
#   debate = "debate name"
#   #for fb files
#   x.hours = get_hours_fb(x.file, debate)
#   x.dates = get_dates_fb(x.file, debate)
#   x.comments = get_comments(x.file, debate)
#   x.corpus = clean_corpus(prep_corpus(x.file))
#   
#   #for json/twitter files
#   x.hours = get_hours_tweets(x.file, debate)
#   x.dates = get_dates_tweets(x.file, debate)
#   x.comments = get_tweet_comments(x.file, debate)
#   x.corpus = clean_tweet_corpus(prep_corpus(x.file))
#   
#   ##Then continue with the following functions
#   x.frequencies = term_stats(x.corpus)
#   
#   #for keywords and adjectives
#   x.ud = get_ud_data(x.comments)
#   x.clength = get_each_comment_length(x.comments)
#   
#   #topics
#   x.dtm = get_dtm(x.corpus)
#   x.ktopics = get_topic_model(x.dtm, k)
#   
#   #sentiment
#   x.corpus.df = get_corpus_df(x.corpus)
#   
#   ##distribution
#   x.afinn.doc.freq = get_afinn_document_frequencies(x.corpus)
#   x.bingnrc.doc.freq = get_bingnrc_document_frequencies(x.corpus, lexicon = "nrc")
#   
#   ##top comments
#   x.afinn.doc.freqfull = get_afinn_sentiment_comments(x.corpus.df, x.comments)
#   x.bingnrc.doc.freqfull = get_sentiment_comments(x.corpus.df, x.comments, lexicon = "nrc")
#   
#   ##top words
#   x.afinn.word.freq = get_afinn_word_frequencies(x.corpus.df)
#   x.bingnrc.word.freq = get_bingnrc_word_frequencies(x.corpus.df, lexicon = "nrc")
# 
#   }

#DEFINING FUNCTIONS

#topiclist=c("animal", "abuse", "visa","bsl tweets", "ATC tweets", "diabetes")

#uploads=data.frame("File Name"="" , "Assigned Name"="", "Number of Comments"="")


#Time/dates----
get_hours_fb=function(file, debate="debate"){
  timeline = data.frame(dmy_hm(file[[2]]))
  hours = data.frame(format(timeline, "%H"))
  
  hours = data.frame(table(hours))
  colnames(hours)[1]=debate
  
  return(hours)
}#=x.hours
get_hours_tweets=function(file, debate){
  timeline = data.frame(strptime(file$created_at, "%a %b %e %H:%M:%S +0000 %Y"))
  hours = data.frame(format(timeline, "%H"))
  
  hours = data.frame(table(hours))
  colnames(hours)[1]=debate
  
  return(hours)
}

get_dates_fb=function(file, debate){
  timeline = data.frame(dmy_hm(file[[2]]))
  fbdates = format(timeline, "%Y-%m-%d")
  
  fbdates = data.frame(table(fbdates))
  fbdates[,1]=as.Date(as.Date(fbdates[,1]), format="%Y-%m-%d")
  
  colnames(fbdates)=c("Date", debate)
  
  return(fbdates)}



#=x.dates
get_dates_tweets=function(file, debate){
  timeline = data.frame(strptime(file$created_at, "%a %b %e %H:%M:%S +0000 %Y"))
  twdates = data.frame(format(timeline, "%y-%m-%d"))
  
  twdates = data.frame(table(twdates))
  
  twdates[,1]=as.Date(twdates[,1], format="%y-%m-%d")
  colnames(twdates)=c("Date", debate)
  return(twdates)
}

display_dates=function(file){
  
  dateplot= ggplot(data=file, aes_string(x=file[,1], y=file[,2])) +geom_line() + ylab("Number of Comments") +ggtitle("Comments by Date")
  
  return(dateplot)
}

#general functions----

## get comments
get_comments = function(file, name){
  comments=tidy(file$Message)
  
  #create the comment numbers
  comment_list=paste0((name), "comment", 1:nrow(file))
  comments[,2]=comment_list
  colnames(comments)=(c("message", "document"))
  return(comments)
}#=x.comments

#if fileInput=JSON
get_tweet_comments = function(file, name){
  tweets=file[,c(26,4)]
  
  #create the comment numbers
  tweet_list=paste0((name), "tweet", 1:nrow(file))
  tweets[,3]=tweet_list
  colnames(tweets)=(c("user", "message", "document"))
  return(tweets)
  
}

#ud_model <- udpipe_load_model(ud_model$file_model)

ud_model <- udpipe_download_model(language = "english", overwrite = FALSE)
ud_en <- udpipe_load_model(ud_model$file_model)


get_ud_data=function(comments){
  comments[]<-lapply(comments, gsub, pattern= '[^ -~]', replacement='')
  comments$message=tolower(comments$message)
  
  x=as.data.frame(udpipe_annotate(ud_en, x = comments$message, doc_id = comments$document))
  return(x)
}


display_adjectives=function(file, max){
  astats <- subset(file, upos %in% c("ADJ")) 
  astats <- txt_freq(astats$lemma)
  astats$key <- factor(astats$key, levels = rev(astats$key))
  plot=ggplot(data = head(astats, max), aes(key, freq)) +
    geom_bar(stat = 'identity') +
    ylab("Freq") +
    xlab('')+
    coord_flip()+
    theme(axis.text.y = element_text(face = "bold",size =15))+
    ggtitle("Most occurring adjectives")
  
  return(plot)
}

display_keywords=function(file, max){
  fstats <- keywords_rake(x = file, term = "lemma", group = "doc_id", 
                          relevant = file$upos %in% c("NOUN", "ADJ"))
  fstats$key <- factor(fstats$keyword, levels = rev(fstats$keyword))
  plot=ggplot(data = head(subset(fstats, freq > 3), max), aes(key, rake)) +
    geom_bar(stat = 'identity') +
    ylab("RAKE") +
    xlab('')+
    coord_flip()+
    theme(axis.text.y = element_text(face = "bold",size =15))+
    ggtitle("Keywords identified by RAKE")
  
  return(plot)
}

get_each_comment_length=function(file){
  dtm= file %>% unnest_tokens(word, message) %>% count(word, document, sort = TRUE) %>% cast_dtm(document, word, n)
  #find row totals
  rowTotals= as.data.frame(apply(dtm , 1, sum))
  rowTotals[,2]=rownames(rowTotals)
  
  colnames(rowTotals)=c("Length","Comment")
  rowTotals=rowTotals[!(rowTotals$Length==1),]
  return(rowTotals)
  
}

## get corpus
prep_corpus=function(file){
  #rearrange so file is [id, message]
  df.forCorpus=data.frame(file$document, file$message)
  #rename columns for input into corpus
  colnames(df.forCorpus)=c("doc_id", "text")
  #create corpus
  preCorpus=VCorpus(DataframeSource(df.forCorpus))
  
  return(preCorpus)
}

clean_corpus <- function(corpus){
  remove.urls <- function (x) gsub ("http[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.urls))
  remove.weird.characters <- function (x) gsub("[^0-9A-Za-z///' ]", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.weird.characters))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  
  return(corpus)
}#=x.corpus

clean_tweet_corpus <- function(corpus){
  # Remove retweets
  remove.rt <- function (x) gsub ("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.rt))
  # Remove @ people
  remove.at.people <- function (x) gsub("@\\w+", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.at.people))
  
  remove.urls <- function (x) gsub ("https[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.urls))
  
  remove.weird.characters <- function (x) gsub("[^0-9A-Za-z///' ]", "", x)
  corpus <- tm_map(corpus, content_transformer(remove.weird.characters))
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  #remove.emptys <- function(x) gsub("/^$\n/", '', x)
  #corpus<- tm_map(corpus, content_transformer(remove.emptys))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  #corpus<-tm_filter(corpus, )
  return(corpus)
}

##get frequencies
# input to frequencies function is 
#term_stats(corpus)#=x.frequencies

display_term_frequencies=function(file, debate,max, rogue=NULL){
  if(is.null(rogue)){ 
    print("All terms good to go")
    frequency_plot=ggplot(data = file[1:(max),], aes(x=reorder(term, -count), y=count)) +
      geom_bar(stat = 'identity', position=position_dodge2(reverse=TRUE)) +
      ylab('Frequency') +
      xlab('')+
      #scale_x_discrete(rev(levels(as.factor(file$term))))+
      ggtitle(paste(debate,': Top', max, 'Words'))+
      theme(axis.text.y = element_text(face = "bold",size =15))+
      coord_flip()
  }
  
  else{
    #remove rogue term
    print(paste0("Removing rogue terms: " ,(rogue)))
    file=file[!file$term %in% (rogue),]
    #then do the plot
    frequency_plot=ggplot(data = file[1:(max),], aes(reorder(term, -count), count)) +
      geom_bar(stat = 'identity') +
      ylab('Frequency') +
      xlab('')+
      coord_flip()+
      theme(axis.text.y = element_text(face = "bold",size =15))+
      ggtitle(paste0(debate,': Top', max, 'Words'))
  }
  return(frequency_plot)
}

## get bigrams (takes comments file as input)
display_bigrams = function(tibble, min, debate){
  for (line in (tibble)){
    ngrams=(tibble %>% unnest_tokens(bigrams, message, token="ngrams", n=2, to_lower = TRUE) %>% filter(!str_detect(bigrams, "[[:digit:]xxwww]")))
    bigrams_separated = ngrams %>% separate(bigrams, c("word1", "word2"), sep = " ")
    bigrams_filtered = bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    bigrams_ordered=bigrams_filtered %>% count(word1, word2, sort = TRUE)
  }
  bigrams_ordered=na.omit(bigrams_ordered)
 
  #print(head(bigrams_ordered))
  
  
  ##net
  arrow= grid::arrow(type = "closed", length = unit(3, "mm"))
  net = (bigrams_ordered %>% 
           filter(n >= (min)) %>%
           graph_from_data_frame() %>%
           ggraph(layout = "fr") +
           geom_edge_arc(aes(colour=n, start_cap = label_rect(node1.name),end_cap = label_rect(node2.name)), curvature = 0.5, arrow=NULL, linemitre = 1) +
           geom_node_text(aes( label = name, fontface="bold"), check_overlap = TRUE)+
           scale_edge_colour_gradient(high = "#00FF99", low = "#003300")+
           ggtitle(debate)+
           theme(panel.background = element_rect(fill = "#EDEBEA", colour = NA),
                 plot.background = element_rect(fill =  "#EDEBEA"),
                 panel.grid.major =  element_blank(),
                 panel.grid.minor =  element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 legend.background = element_rect(fill =  "#EDEBEA", colour = NA),
                 legend.text = element_text(colour = "black"),
                 text = element_text(colour="black")
           )
         #theme_dark()
         #coord_fixed()+
         #theme_void()
  )
  return(net)
}

# dtm
get_dtm=function(corpus, term=NULL){
  dtm=DocumentTermMatrix(corpus)
  
  if(is.null(term)){ 
    print("Missing Term!")
    #remove empty rows
    rowTotals=apply(dtm, 1, sum)
    dtm=dtm[rowTotals>0,]
    
    #return(dtm)
  }
  
  else{
    #remove most frequent word
    mostFrequentTerm=which(colnames(dtm) %in% c(term)) 
    dtm=dtm[,-mostFrequentTerm]
    
    #remove empty rows
    rowTotals=apply(dtm, 1, sum)
    dtm=dtm[rowTotals>0,]
    
    #return(dtm)
  }
  
  
  return(dtm)
}#=x.dtm

get_dtm_bigram=function(corpus, term=NULL){
  BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  
  if(is.null(term)){ 
    print("Missing Term!")
    #create dtm
    dtm_bigram <- DocumentTermMatrix(corpus, control = list(tokenize =BigramTokenizer, removePunctuation = TRUE,stopwords = stopwords("english")))
    
    #remove empty rows
    rowTotals=apply(dtm_bigram, 1, sum)
    dtm_bigram=dtm_bigram[rowTotals>0,]
    
    #return(dtm)
  }
  
  else{
    #remove specific words
    remove.specific.terms <- function (x) gsub(term, "", x)
    corpus.clean <- tm_map(corpus, content_transformer(remove.specific.terms))
    
    #create dtm
    dtm_bigram <- DocumentTermMatrix(corpus.clean, control = list(tokenize =BigramTokenizer, removePunctuation = TRUE,stopwords = stopwords("english")))
    # #remove most frequent bigram
    # mostFrequentBigram=which(colnames(dtm_bigram) %in% c(term)) 
    # dtm_bigram=dtm_bigram[,-mostFrequentBigram]
    
    #then remove empty rows
    rowTotals=apply(dtm_bigram, 1, sum)
    dtm_bigram=dtm_bigram[rowTotals>0,]
    
    #return(dtm)
  }
  
  return(dtm_bigram)
  
}


get_readability=function(comments){
  #create new dataframe
  new.table=data.frame(doc_id=comments$document, text=as.character(comments$message))
  new.table$text=as.character(new.table$text)
  #calculate readability
  scores=textstat_readability(quanteda::corpus(new.table), measure = c("Flesch", "Flesch.Kincaid"))
  scores$Flesch=round(scores$Flesch)
  scores$Flesch.Kincaid=round(scores$Flesch.Kincaid)
  return(scores)
}


estimate_ages=function(ud, read.scores){
  #get pos tags
  POStable=as.matrix(table(ud$doc_id, ud$upos))
  POStableSums=rowSums(POStable)
  POStable2=round(POStable/POStableSums, 2)
  
  #get nouns etc
  NDA=POStable2[,"NOUN"] + POStable2[,"DET"]+ POStable2[,"ADJ"]
  NDA=data.frame(NDA)
  NDA$document=rownames(NDA)
  
  #get pronouns etc
  PI=POStable2[,"PRON"] + POStable2[,"INTJ"]
  PI=data.frame(PI)
  PI$document=rownames(PI)
  
  #merge with readability?
  all= read.scores %>% inner_join(NDA, by="document") %>% inner_join(PI, by="document")
  
  return(all)
}




# LDA----
get_topic_model=function(dtm, k=10){
  model = LDA((dtm), k=(k), control = list(seed = 1234), method="VEM")
  return(model)
}#=x.ktopics

display_each_topic_distribution=function(ldafit, debate){
  ggplot(data= data.frame(table(topics(ldafit))), aes(x=Var1, y=Freq)) + 
    geom_bar(stat='identity') +
    ylab("Number of submissions") +
    xlab("Topic Number") +
    ggtitle(paste(debate,":", "Topic Distribution", sep = " ")) +
    theme(legend.position = "none")
  
}

display_top_topics=function(ldafit, n, debate){
  #tibble of words with their probability of being in each topic
  corpus_topics_function=melt(ldafit@beta)
  corpus_topics_function2=transmute(corpus_topics_function, topic=Var1,term=ldafit@terms[Var2], beta=value)
  #gets top 10 words in each topic
  corpus_top_terms_function <- corpus_topics_function2 %>%
    group_by(topic) %>%
    top_n((n), beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  #barplot
  corpus_top_terms_funtion_plot = corpus_top_terms_function %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y", ncol=4) +
    theme(axis.text.x = element_blank(),  axis.ticks = element_blank(), axis.text.y = element_text(face = "bold",size =10))+
    ggtitle(debate)+
    scale_fill_hue(l=40)+
    coord_flip()
  return(corpus_top_terms_funtion_plot)
}

# myColourHue= function(n=10) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 45, c = 100)[1:n]
# }
get_lemma_from_web = function(){
  # download the list
  # url <- "http://www.lexiconista.com/Datasets/lemmatization-en.zip"
  # tmp <- tempfile()
  # download.file(url, tmp)
  # print("lemmas downloaded")
  # 
  # extract the contents
  con <- "lemmatization-en.txt"
  tab <- read.delim(con, header=FALSE, stringsAsFactors = FALSE)
  names(tab) <- c("lemma", "term")
  print("contents extracted")
  
  lemma_list <- new_stemmer(tab$term, tab$lemma)
  assign('lemma_list', lemma_list, envir = .GlobalEnv)
  print("lemma list created and saved")
}
get_lemma = function(file){
  get_lemma_from_web()
  
  lemma1=text_tokens(file$word, stemmer=lemma_list)
  lemma2=data.table(lemma1)
  #lemma2=ldply(lemma1, data.frame)
  
  print(c("lemma 2 has", nrow(lemma2), "rows"))
  joined_lemma=cbind(file$document, lemma2)
  print(c("tidy lemma 2 has", nrow(joined_lemma), "rows"))
  
  colnames(joined_lemma)=c("document", "word")
  joined_lemma$word=as.character(joined_lemma$word)
  #assign('fb_lemma', tidy_lemma2, envir = .GlobalEnv)
  return(joined_lemma)
}

get_btm_model=function(comments, k){
  table = data.table(comments %>% unnest_tokens(word, message)) %>% anti_join(stop_words)
  
  lemmas = get_lemma(table)
  
  #This bit takes ages!
  fit = BTM(lemmas, k)
  
  return(fit)
}

display_top_topics_btm=function(btmfit, n, debate){
  #tibble of words with their probability of being in each topic
  corpus_topics_function=melt(t(data.table(btmfit[["phi"]])))
  corpus_topics_function2=transmute(corpus_topics_function, topic=Var1, term=dimnames(btmfit[["phi"]])[[1]][Var2], beta=value)
  #gets top 10 words in each topic
  corpus_top_terms_function <- corpus_topics_function2 %>%
    group_by(topic) %>%
    top_n((n), beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  #barplot
  corpus_top_terms_funtion_plot = corpus_top_terms_function %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y", ncol = 4) +
    theme(axis.text.x = element_blank(),  axis.ticks = element_blank(), axis.text.y = element_text(face = "bold",size =10))+
    ggtitle(debate)+
    scale_fill_hue(l=40)+
    coord_flip()
  return(corpus_top_terms_funtion_plot)
}


get_topic_wordcloud=function(ldafit){
  betamatrix=melt(ldafit@beta)
  betamatrix2=transmute(betamatrix, topic=Var1,term=ldafit@terms[Var2], beta=value)
  
  beta.tdm=as.matrix(betamatrix2%>% cast_tdm(term, topic, beta))
  
  #plo=RColorBrewer::brewer.pal(10, "Paired")
  
  comparison.cloud(beta.tdm, max.words = 100, colors = myColourHue(), scale = c(2,.25), title.size = 1.5)
  
}

get_associations_cloud2=function(dtm, term, corr){
  table_for_cloud= data.frame(findAssocs(dtm, term, corr))
  table_for_cloud[,2]=table_for_cloud[,1]
  table_for_cloud[,1]=rownames(table_for_cloud)
  
  #make background color grey
  par(bg="#F0F0F0")
  wordcloud2(table_for_cloud, size = 0.5)
}

topicmodels2LDAvis <- function(ldafit, ...){
  post <- topicmodels::posterior(ldafit)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- ldafit@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    mds.method = jsPCA2, reorder.topics = FALSE
  )
}

jsPCA2 <- function(phi) {
  # first, we compute a pairwise distance between topic distributions
  # using a symmetric version of KL-divergence
  # http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
  jensenShannon2 <- function(x, y) {
    m <- 0.5*(x + y)
    lhs <- ifelse(x == 0, 0, x * (log(x) - log(m)))
    rhs <- ifelse(y == 0, 0, y * (log(y) - log(m)))
    0.5 * sum(lhs) + 0.5 * sum(rhs)
    #sum(ifelse(x==0,0,x * log(x/m)) + 0.5*sum(y*log(y/m)))
  }
  dist.mat <- proxy::dist(x = phi, method = jensenShannon2)
  #print(as.matrix(dist.mat))
  print(which(as.matrix(dist.mat) ==Inf, arr.ind = T))
  #remove Inf values
  dist.mat[!is.finite(dist.mat)] <- 0
  #check that they're gone - should be empty
  print(which(as.matrix(dist.mat) ==Inf, arr.ind = T))
  # then, we reduce the K by K proximity matrix down to K by 2 using PCA
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[,1], y = pca.fit[,2])
}

get_topic_idf=function(ldafit, comments){
  top_topics=data.frame(topics(ldafit))
  top_topics$comment = rownames(top_topics)
  colnames(top_topics)[1]="Topic"
  
  joined = left_join(x=comments, y=top_topics, by = c("document"= "comment"))
  
  topic_idf= joined%>% unnest_tokens(word, message, to_lower = TRUE) %>% anti_join(stop_words) %>% count(word, Topic, sort=TRUE) %>%  bind_tf_idf(word, Topic, n)
  
  return(topic_idf)
  
}


#Sentiment----
get_corpus_df=function(corpus){
  df=data.frame(text=sapply(corpus, as.character), stringsAsFactors = FALSE)
  df[,2]=rownames(df)
  colnames(df)[2]="comment"
  df$length=sapply(df$text, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
  
  return(df)
}#=x.corpus.df
###frequencies of each document in each category 
get_afinn_sentiment_comments= function(corpus_df, fullfile){
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  frequencies=tibble %>%
    inner_join(get_sentiments("afinn")) %>%
    count(comment,value, sort = TRUE) %>%
    ungroup()
  merged=merge(x=frequencies, y=corpus_df,by.x="comment", by.y="comment")    
  merged=merge(x=merged, y=fullfile, by.x= "comment", by.y = "document")
  
  return(merged)
}
get_sentiment_comments= function(corpus_df,fullfile, lexicon){
  
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  frequencies=tibble %>%
    inner_join(get_sentiments(lexicon)) %>%
    count(comment,sentiment, sort = TRUE) %>%
    ungroup()
  merged=merge(x=frequencies, y=corpus_df,by.x="comment", by.y="comment")   
  #merge again to include full text as a variable.
  merged=merge(x=merged, y=fullfile, by.x= "comment", by.y = "document")
  return(merged)
  #return(head(merged))
}



###frequencies of each word in each category (use as frequency inputs to other functions)
get_afinn_word_frequencies = function(corpus_df){
  #make a dataframe from the corpus
  # corpus_df=data.frame(text = sapply((corpus), as.character), stringsAsFactors = FALSE)
  # corpus_df[,2]=rownames(corpus_df)
  # colnames(corpus_df)[2]="comment"
  
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  #get frequencies
  tibble %>%
    inner_join(get_sentiments("afinn")) %>%
    count(word,value, sort = TRUE) %>%
    ungroup()
  
}
get_bingnrc_word_frequencies = function(corpus_df, lexicon){
  #make a dataframe from the corpus
  # corpus_df=data.frame(text = sapply((corpus), as.character), stringsAsFactors = FALSE)
  # corpus_df[,2]=rownames(corpus_df)
  # colnames(corpus_df)[2]="comment"
  
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  #get frequencies
  tibble %>%
    inner_join(get_sentiments(lexicon)) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup()
  
}


## Distributions (takes doc. frequencies as input)

get_afinn_document_frequencies = function(corpus){
  #make a dataframe from the corpus
  corpus_df=data.frame(text = sapply((corpus), as.character), stringsAsFactors = FALSE)
  corpus_df[,2]=rownames(corpus_df)
  colnames(corpus_df)[2]="comment"
  
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  #get frequencies
  tibble %>%
    inner_join(get_sentiments("afinn")) %>%
    count(comment,value, sort = TRUE) %>%
    ungroup()
  
}
plot_each_afinn_doc_distributions=function(frequencies, debate){
  ggplot(data= as.data.frame(table(frequencies$value)/nrow(frequencies)*100), aes(x=Var1, y=Freq, fill=as.numeric(Var1))) + 
    geom_bar(stat='identity') +
    ylab("Percentage of submissions") +
    xlab("Sentiment category") +
    ggtitle(paste(debate,":", "Afinn Sentiment Distribution", sep = " ")) +
    theme(legend.position = "none")
  
  
}


get_bingnrc_document_frequencies = function(corpus, lexicon){
  #make a dataframe from the corpus
  corpus_df=data.frame(text = sapply((corpus), as.character), stringsAsFactors = FALSE)
  corpus_df[,2]=rownames(corpus_df)
  colnames(corpus_df)[2]="comment"
  
  #convert to 1-word-per-row
  tibble = corpus_df %>% unnest_tokens(word, text)
  
  #get frequencies
  tibble %>%
    inner_join(get_sentiments(lexicon)) %>%
    count(comment,sentiment, sort = TRUE) %>%
    ungroup()
  
}
plot_bingnrc_distributions=function(frequencies, debate, lexicon){
  ggplot(data= as.data.frame(table(frequencies$sentiment)/nrow(frequencies)*100), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat='identity') +
    ylab("Percentage of submissions") +
    xlab("Sentiment category") +
    ggtitle(paste(debate,":", lexicon, "Sentiment Distribution", sep = " ")) +
    theme(legend.position = "none")
  
  
}


## Top comments (takes doc. frequencies as input)
display_afinn_comments=function(df,debate){
  df$percentage=(df$n/df$length)*100
  
  qrange=df[df$length < quantile(df$length, 0.75) & df$length > quantile(df$length, 0.25),]
  
  print(table(qrange$value)/nrow(qrange))
  
  qrange %>%
    group_by(value) %>%
    top_n(7) %>%
    ungroup() %>%
    mutate(comment = reorder(comment, percentage)) %>%
    ggplot(aes(comment, percentage, fill = value, label=message)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~value, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) +
    ggtitle(debate)+
    coord_flip()
  
}
display_topic_comments=function(df,debate){
  df$percentage=(df$n/df$length)*100
  
  qrange=df[df$length < quantile(df$length, 0.75) & df$length > quantile(df$length, 0.25),]
  
  #grange=left_join(qrange, fireworks.comments, by=c("comment" = "document" ))
  #print(head(qrange))
  print(ncol(qrange))
  
  print(table(qrange$sentiment)/nrow(qrange))
  
  qrange %>%
    group_by(sentiment) %>%
    top_n(7) %>%
    ungroup() %>%
    mutate(comment = reorder(comment, percentage)) %>%
    ggplot(aes(comment, percentage, fill = sentiment, label=message)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) +
    ggtitle(debate)+
    coord_flip()
  
}

## Top words (takes word frequencies as input)
plot_afinn_words = function(sentiment_frequencies,debate){
  sentiment_frequencies %>%
    group_by(value) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = value)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~value, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) +
    ggtitle(debate)+
    theme(axis.text.y = element_text(face = "bold",size =10))+
    coord_flip()
}
plot_bingnrc_words = function(sentiment_frequencies,debate){
  sentiment_frequencies %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) +
    ggtitle(debate)+
    theme(axis.text.y = element_text(face = "bold",size =10))+
    coord_flip()
}

get_sentiments_wordcloud = function(sentiment_frequencies, lexicon="bing"){
  sentiment_frequencies %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("red", "green"),
                     max.words = 100, scale=c(6,.5), title.size = 2)
  
}
