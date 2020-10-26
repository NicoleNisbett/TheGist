

##Server----


server=shinyServer(function(session,input,output){
  #Welcome Panel Output----
  #reads the csv file selected by user, saves it as 'data'
  my_inputdata <- reactive({
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    data <- read_csv(inFile$datapath)
    assign("newInputData",data,envir=.GlobalEnv)
    data
  })
  
  # Create the preview table
  output$contents <- renderDT({
    head(my_inputdata())    
    #head(testinputdata)
  })
  
  #makes 'save file' button only appear when file has been loaded
  output$ui.action <- renderUI({
    if (is.null(my_inputdata())) return()
    actionButton("saveButton", "Save file to app")
  })
  
  #uploads=data.frame("File Name" = character(), "Assigned Name" = character(), "Number of Comments" = integer())

  #create summary table of the names of files saved, updates after new file 
  # Whenever a field is filled, aggregate all form data
  uploadData <- reactive({
    data2=data.frame("File Name" =input$file1[["name"]], "Assigned Name" = input$shinyalert, "Number of Comments" = nrow(newInputData))
    #print(data2)
    return(data2)
    
  })
  
  #save and update the summary table - should only do once ok button is pressed
  saveData <- function(data2) {
    #add the new file info (uploads) to the existing summary table (data2)
     if (exists("uploads")) {
       print("uploads already exists")
       print(head(uploads))
       print(head(data2))
       uploads <<- rbind(uploads, data2)
       print(head(uploads))

     } else {
       print("creating uploads now")
       uploads<<- data2
    }
  }
  #retrieves the summary table
  loadData <- function() {
    if (exists("uploads")) {
      uploads
    }
  }
  
  ##OBJECT CREATION----
  #files = list.files(path = "/Users/nicolenisbett/Documents/PhD/R/Complete Analysis/MastermindV2/Datasets/", pattern = '.csv', full.names = TRUE)
  
  process_input_files<-function(x=newInputData){
    
    # Load csv and create necessary objects
    k = 10
    x.file =x
    debate = input$shinyalert
    #for fb files
    #x.hours = get_hours_fb(x.file, debate)
    #need to change this date one after - will only work for diabetes
    #x.dates = get_dates_fb(x.file, debate)
    x.comments = get_comments(x.file, debate)
    x.corpus = clean_corpus(prep_corpus(x.comments))
    
    print("general objects created")
    
    #for json/twitter files
    # x.hours = get_hours_tweets(x.file, debate)
    # x.dates = get_dates_tweets(x.file, debate)
    # x.comments = get_tweet_comments(x.file, debate)
    # x.corpus = clean_tweet_corpus(prep_corpus(x.file))
    
    ##Then continue with the following functions
    x.frequencies = term_stats(x.corpus)
    print("frequency objects created")
    
    #for keywords and adjectives
    x.ud = get_ud_data(x.comments)
    x.clength = get_each_comment_length(x.comments)
    
    #topics (only compute this once k is chosen by user input)
    x.dtm = get_dtm(x.corpus)
    x.topics = get_topic_model(x.dtm, k)
    x.topicsIDF = get_topic_idf(x.topics, x.comments)
    
    print("topic objects created")
    
    #sentiment
    x.corpus.df = get_corpus_df(x.corpus)
    
    ##distribution
    x.afinn.doc.freq = get_afinn_document_frequencies(x.corpus)
    x.bingnrc.doc.freq = get_bingnrc_document_frequencies(x.corpus, lexicon = "nrc")
    
    ##top comments
    x.afinn.doc.freqfull = get_afinn_sentiment_comments(x.corpus.df, x.comments)
    x.bingnrc.doc.freqfull = get_sentiment_comments(x.corpus.df, x.comments ,lexicon = "nrc")
    
    ##top words
    x.afinn.word.freq = get_afinn_word_frequencies(x.corpus.df)
    x.bingnrc.word.freq = get_bingnrc_word_frequencies(x.corpus.df, lexicon = "nrc")
    
    print("sentiment objects created")
    
    object.list=list(x.file=x.file, 
                     #x.hours=x.hours, x.dates=x.dates, 
                     x.comments=x.comments, x.corpus=x.corpus, x.frequencies=x.frequencies, x.ud=x.ud, x.clength=x.clength, x.dtm=x.dtm, x.topics=x.topics,x.topicsIDF=x.topicsIDF, x.corpus.df=x.corpus.df, x.afinn.doc.freq=x.afinn.doc.freq, x.bingnrc.doc.freq=x.bingnrc.doc.freq, x.afinn.doc.freqfull=x.afinn.doc.freqfull, x.bingnrc.doc.freqfull=x.bingnrc.doc.freqfull, x.afinn.word.freq=x.afinn.word.freq, x.bingnrc.word.freq=x.bingnrc.word.freq)
    #make all the names of the objects have the assigned name from the user
    
    names(object.list)=gsub("x", debate, names(object.list))
    print("object list created")
    # print(names(object.list))
    #save the list in global env.
    assign(paste0(debate, ".list"), object.list, envir = .GlobalEnv)

  }
  
  # When the OK button is clicked, save the upload summary table data and create the r objects
  observeEvent(input$shinyalert, {
    saveData(uploadData())
    process_input_files()
  }
  )
  # 
  #once the save button is pressed, save csv to global env./ or to Datasets folder 
  observeEvent(input$saveButton, {
    
    #assign(input$file1[["name"]],my_inputdata(),envir=.GlobalEnv)
    shinyalert("File Saved!",paste("Please type a unique name to refer to dataset:", input$file1[["name"]], "in the app" ), type="input", inputType = "text", confirmButtonCol = "#336600", closeOnEsc = FALSE, closeOnClickOutside = FALSE)
    #print(input$shinyalert)
    
    #write_csv(x=my_inputdata(),path = paste0("/Users/nicolenisbett/Documents/PhD/R/Complete Analysis/MastermindV2/Datasets/",input$file1[["name"]]))
    
    #print(input$file1[["name"]])
    
  })
  
  #connects to ui to print summary table
  #Show the previous uploads
  #(update with current upload when OK is clicked)
  output$uploads <- DT::renderDataTable({
    input$shinyalert
    loadData()
  })
  
  
  
  #General Panel Outputs---- 
  
  #takes a list of the assigned names of datasets defined by user on upload. use this as the options inthe dropdown menus
  #dropdown.options=as.character(uploads$Assigned.Name)
  
  
  # output$testingpage <- renderPlot({
  #   testingpage.hours=get_hours_fb(testinputdata, "test")
  #   
  #    ggplot(testingpage.hours, aes(x= test, y=Freq, group=1))+geom_line()+ggtitle("Comments per hour")
  #   
  # })
  
  output$testingpage1 <- renderText({
    
    #switch needs to be the .dates (or [[3]]) bit of each of the created lists.
    #rewind. just want it to show the names of each .list file - but how do I get the switch to update with the created files?? something like: input$testingID = grep(paste0(input$testingID,".list"), names(.GlobalEnv),value=TRUE)  or "MentalHealth"=object.list, "Diabetes"=input.list, "Again5"=Again5.list or input$testingID, thisInput = grep(paste0(thisInput,".list"), names(.GlobalEnv),value=TRUE)
    thisInput=input$testingID
    testingpage.switch<-switch(input$testingID, get(paste0(thisInput, ".list"),envir = globalenv()))
    print(thisInput)
    #print(paste0(input$testingID,".list"))
    names(testingpage.switch)
    
    # names(Santo1.list)
    #ls(globalenv())[ls(globalenv())%like%input$testingID]
    #oggetto<<-grep(input$testingID,ls(globalenv()),value = T)
    
    #oggetto<<-input$testingID
    
    #names(oggetto)
    
    #ggplotly(display_dates(testingpage.switch))%>% config(displayModeBar = F)
  })
  
  output$testingpage2 <- renderPlotly({
    thisInput=input$testingID
    
    testingpage2.switch<-switch(input$testingID, 
                                get(paste0(thisInput, ".list"),envir = globalenv())[[paste0(thisInput, ".dates")]])
    #print(testingpage2.switch)
    
    
    ggplotly(display_dates(testingpage2.switch))
    
  })
  

  output$eachTimes<- renderPlotly({
    hoursInput=input$HoursDatesID
    hours.switch<-switch(input$HoursDatesID,
                         get(paste0(hoursInput, ".list"),envir = globalenv())[[paste0(hoursInput, ".hours")]])
    
    ggplotly(ggplot(data=hours.switch,aes(x= hours.switch[,1], y=Freq, group=1) ) + geom_line() + ylab("Number of Comments") +xlab(hoursInput) +ggtitle("Comments by Hour"))%>% config(displayModeBar = F) 
  })
  
  
  output$eachDates<- renderPlotly({
    
    datesInput=input$HoursDatesID
    dates.switch<-switch(input$HoursDatesID,
                         get(paste0(datesInput, ".list"),envir = globalenv())[[paste0(datesInput, ".dates")]])

    ggplotly(display_dates(dates.switch))%>% config(displayModeBar = F)
    
  })
  
  
  # output$Frequencies2= output$Frequencies= renderPlot({
  #   
  #   #display_term_frequencies(fireworks.frequencies, "Fireworks Debate")
  #   
  #   usingdata.frequencies<-switch(input$DebateFrequencyID,
  #                                 "animal" = animal.frequencies, "fireworks"=fireworks.frequencies, "visa"=visa.frequencies, "abuse" = abuse.frequencies, "bsl tweets" = bsltweets.frequencies, "ATC tweets" = atctweets.dedup.frequencies )
  #   
  #   display_term_frequencies(usingdata.frequencies, paste(input$DebateFrequencyID,"Debate"), input$maxfreqID)
  #   
  #   
  # }) 
  # 
  # frequencies.rswitch=reactive({
  #   hoursInput=input$HoursDatesID
  #   hours.switch<-switch(input$HoursDatesID,
  #                        get(paste0(hoursInput, ".list"),envir = globalenv())[[2]])
  #   
  #   usingdata.keywords<-switch(input$DebateFrequencyID,
  #                              "animal" = get_ud_data(animal.comments), "fireworks"=fireworks.x, "visa"=visa.x, "abuse" = abuse.x, "bsl tweets" = bsltweets.x, "ATC tweets" = atctweets.dedup.x)
  # })
  # 
  output$Adjectives= renderPlot({
    adjectivesInput=input$AdjectivesID
    adjectives.switch=switch(input$AdjectivesID, 
                             get(paste0(adjectivesInput, ".list"), envir = globalenv())[[paste0(adjectivesInput, ".ud")]])

    display_adjectives(adjectives.switch, input$maxfreqID)

  })

  output$Keywords= renderPlot({
    keywordsInput=input$AdjectivesID
    keywords.switch=switch(input$AdjectivesID, 
                             get(paste0(keywordsInput, ".list"), envir = globalenv())[[paste0(keywordsInput, ".ud")]])
    
    display_keywords(keywords.switch, input$maxfreqID)
    
  })
  
  output$CommentLengths1 =renderPlotly({
    length1Input=input$CommentLengthID
    length1.switch<-switch(input$CommentLengthID,
                         get(paste0(length1Input, ".list"),envir = globalenv())[[paste0(length1Input, ".comments")]])
    
    
    ggplotly(ggplot(data=get_each_comment_length(length1.switch), aes(x=" ",y=Length))+
               geom_boxplot() + 
               ylab('Number of words') +
               ggtitle("Number of Words per Comment"))%>% config(displayModeBar = F)
  }) 
  
  output$CommentLengths2 =renderPlotly({
    
    length2Input=input$CommentLength2ID
    length2.switch<-switch(input$CommentLength2ID,
                           get(paste0(length2Input, ".list"),envir = globalenv())[[paste0(length2Input, ".comments")]])
    
    ggplotly(ggplot(data=get_each_comment_length(length2.switch), aes(x=" ",y=Length))+
               geom_boxplot() + 
               ylab('Number of words') +
               ggtitle("Number of Words per Comment"))%>% config(displayModeBar = F)
  })
  
  
  output$Bigrams= renderPlot({
    bigramInput=input$BigramID
    bigram.switch<-switch(input$BigramID,
                           get(paste0(bigramInput, ".list"),envir = globalenv())[[paste0(bigramInput, ".comments")]])
    
    display_bigrams(bigram.switch, input$minbigramID, paste(input$BigramID,"Debate"))
    
  }, height = 600)
  
  #Topic Panel Outputs#----
  
  output$TopicDistributions= renderPlot({
    topicdistInput=input$TopicDistID
    topicdist.switch<-switch(input$TopicDistID,
                          get(paste0(topicdistInput, ".list"),envir = globalenv())[[paste0(topicdistInput, ".topics")]])
    
    
    display_each_topic_distribution(topicdist.switch,input$TopicDistID)
    

  })
  
  output$SerVis <- renderVis({
    servisInput=input$ServisDebate
    servisSwitch<-switch(input$ServisDebate,
                         get(paste0(servisInput, ".list"),envir = globalenv())[[paste0(servisInput, ".topics")]])
    
    topicmodels2LDAvis(servisSwitch)
  })
  
  
  output$Associations = renderPrint({
    assocInput=input$TopicAssocID
    assocSwitch<-switch(input$TopicAssocID,
                         get(paste0(assocInput, ".list"),envir = globalenv())[[paste0(assocInput, ".dtm")]])
    
    termInput=input$TopicTerm

      head(findAssocs(assocSwitch,termInput, input$corrID))

    }
  )
  
  output$AssociationsCloud = renderWordcloud2({
    assocInput=input$TopicAssocID
    assocSwitch<-switch(input$TopicAssocID,
                        get(paste0(assocInput, ".list"),envir = globalenv())[[paste0(assocInput, ".dtm")]]) 
    
    get_associations_cloud2(assocSwitch, input$TopicTerm , input$corrID)
  })
  
  # plotType <- function(x, type) {
  #   switch(type,
  #          "Barplot" = display_top_topics(x, 10, paste(input$DebateTopicID,"Topics")),
  #          "Word Cloud" = get_topic_wordcloud(x)
  #   )
  # }
  
  output$plot <- renderPlot({
    
    plotInput=input$TopicWordsID
    plotSwitch<-switch(input$TopicWordsID,
                       get(paste0(plotInput, ".list"),envir = globalenv())[[paste0(plotInput, ".topics")]])
    
    
    plotType(plotSwitch, input$TopicOption)
  })
  
  # output$Topicnumber = renderPlot({
  #   dtmSwitch=switch(input$DebateTopicNumID,"animal" = animal.dtm, "fireworks"=fireworks.dtm, "visa"=visa.dtm, "abuse" = abuse.dtm, "bsl tweets" = bsltweets.dtm, "ATC tweets" = atctweets.dedup.dtm, "diabetes"= diabetes.dtm )
  #   
  #   optimal_topics(dtmSwitch)
  #   
  # })
  output$TopicIDF= renderPlot({
    
    topicIDFInput=input$TopicWordsID
    topicIDFSwitch<-switch(input$TopicWordsID,
                             get(paste0(topicIDFInput, ".list"),envir = globalenv())[[paste0(topicIDFInput, ".topicsIDF")]]) 
    
    topicIDFSwitch %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(Topic) %>% 
      top_n(10) %>% 
      ungroup() %>%
      ggplot(aes(word, tf_idf, fill = as.factor(Topic))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~Topic, ncol = 4, scales = "free") +
      coord_flip()
    
  })
  
  output$Topicwords= renderPlot({
    
    topicwordsInput=input$TopicWordsID
    topicwordsSwitch<-switch(input$TopicWordsID,
                        get(paste0(topicwordsInput, ".list"),envir = globalenv())[[paste0(topicwordsInput, ".topics")]]) 
    
    display_top_topics(topicwordsSwitch, 10, paste(input$TopicWordsID,"Topics"))
    
  })
  
  
  
  # output$Topicbigrams= renderPlot({
  #   
  #   usingdata.topics.bigram<-switch(input$DebateTopicBigramID,
  #                                   "animal" = animal.10topics.bigram, "fireworks"=fireworks.10topics.bigram, "visa"=visa.10topics.bigram, "abuse" = abuse.10topics.bigram, "bsl tweets" = bsltweets.10topics.bigram, "ATC tweets" = atctweets.dedup.10topics.bigram, "diabetes"= diabetes.10topics.bigram.clean)
  #   
  #   
  #   display_top_topics(usingdata.topics.bigram, 10, paste(input$DebateTopicBigramID,"Topics"))
  #   
  # })
 
  
  
  #Sentiment Panel Outputs#----
  
 SentDist.rswitch<-reactive({  
   sentdistInput=input$SentDistID
  sentdist.switch<-switch(input$SentDistID,
                            get(paste0(sentdistInput, ".list"),envir = globalenv())[[paste0(sentdistInput, ".corpus")]])
    
  sentdist.switch
  })
  
  
  output$SentimentDistributionsAfinn= renderPlot({
    
    plot_each_afinn_doc_distributions(get_afinn_document_frequencies(SentDist.rswitch()), paste(input$SentDistID,"Debate"))
    
  })
  
  output$SentimentDistributionsNRC= renderPlot({
    
    plot_bingnrc_distributions(get_bingnrc_document_frequencies(SentDist.rswitch(),"nrc"),paste(input$SentDistID,"Debate"), "nrc")
    
  })
  
  output$SentimentDistributionsBing= renderPlot({
    
    plot_bingnrc_distributions(get_bingnrc_document_frequencies(SentDist.rswitch(),"bing"), paste(input$SentDistID,"Debate"), "bing")
    
  })
  
SentCommentsCor.rswitch<-reactive({  
    
    sentcommentsInput=input$SentCommentsID
    sentCommentsCor.switch<-switch(input$SentCommentsID,
                            get(paste0(sentcommentsInput, ".list"),envir = globalenv())[[paste0(sentcommentsInput, ".corpus.df")]])
    
    sentCommentsCor.switch
  })
  
SentComments.rswitch=reactive({
    
    sentcommentsInput=input$SentCommentsID
    sentComments.switch<-switch(input$SentCommentsID,
                                   get(paste0(sentcommentsInput, ".list"),envir = globalenv())[[paste0(sentcommentsInput, ".comments")]])
    
    sentComments.switch
    
  })
  
  output$TopCommentsAfinn= renderPlotly({
    
    ggplotly(display_afinn_comments(get_afinn_sentiment_comments(SentCommentsCor.rswitch(), SentComments.rswitch()), paste(input$SentCommentsID,"Debate"))) %>% hide_legend()%>% config(displayModeBar = F)
    
  })
  
   observe({
    textInput=input$SentCommentsID
    updateSelectInput(session, "your_choices", label = "Select comment number", choices=get(paste0(textInput, ".list"),envir = globalenv())[[paste0(textInput, ".comments")]]$document)

   })

  
  output$text <- renderText({ 
    comment.file=get(paste0(input$SentCommentsID, ".list"),envir = globalenv())[[paste0(input$SentCommentsID, ".comments")]]

    comment.file$message[comment.file$document==input$your_choices]

  })

  
  output$TopCommentsBing= renderPlotly({
    
    ggplotly(display_topic_comments(get_sentiment_comments(SentCommentsCor.rswitch(), SentComments.rswitch(),"bing"),paste(input$SentCommentsID,"Debate"))) %>% hide_legend()%>% config(displayModeBar = F)
    
  })
  
  output$TopCommentsNRC= renderPlotly({
    
    ggplotly(display_topic_comments(get_sentiment_comments(SentCommentsCor.rswitch(), SentComments.rswitch(),"nrc"),paste(input$SentCommentsID,"Debate"))) %>% hide_legend() %>% config(displayModeBar = F)
    
  })
  
  SentWords.rswitch<-reactive({  
    
    sentwordsInput=input$SentWordsID
    sentWords.switch<-switch(input$SentWordsID,
                                   get(paste0(sentwordsInput, ".list"),envir = globalenv())[[paste0(sentwordsInput, ".corpus.df")]])
    
    sentWords.switch
  })

  
  output$TopwordsAfinn= renderPlot({
    
    plot_afinn_words(get_afinn_word_frequencies(SentWords.rswitch()), paste(input$SentWordsID,"Debate"))
    
  })
  
  output$TopwordsNRC= renderPlot({
    
    plot_bingnrc_words(get_bingnrc_word_frequencies(SentWords.rswitch(), "nrc"), paste(input$SentWordsID,"Debate"))
    
  })
  
  output$TopwordsBing= renderPlot({
    par(bg="#F0F0F0")
    par(mar = rep(0, 4))
    
    get_sentiments_wordcloud(get_bingnrc_word_frequencies(SentWords.rswitch(), "bing"), "bing")
    
    
    #plot_bingnrc_words(get_bingnrc_word_frequencies(mydata3(), "bing"), paste(input$DebateCorpus2ID,"Debate"))
    
  })
  
  
})

