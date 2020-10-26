
#source("global.R", local=TRUE)

attach(".RData")


#add global file which includes all of the defined functions. source files stored in my private github

# Define UI for application that draws a histogram
#header----
header<-dashboardHeader(title="TheGist")

#sidebar----
sidebar=dashboardSidebar((
  
  sidebarMenu(
    menuItem("Data", tabName = "Welcome"),
    tags$hr(),
    
    menuItem("General Text Analysis", tabName = "General"), 
    tags$hr(),
    
    menuItem("Topic Extraction", tabName = "Topics"),
    tags$hr(),
    
    menuItem("Sentiment Analysis", tabName = "Sentiment"),
    tags$hr()
    )
))

#Main Panels====



welcomePanel=fluidRow(
  h3(strong("Welcome to The Gist"), align="center"),
  p("This application is designed to give you a 'gist' of large volumes of textual data. This data can come in the form of Facebook comments, tweets, survey responses, or emails. 
    Key words and adjectives, key topics, and overall sentiments of the data can be extracted and analysed through the menu bar on the left."),
  DT::dataTableOutput("uploads", width = 300), tags$hr(),
  tabsetPanel(type="pills",
              tabPanel("CSV",
                       p("Please upload your comment file in .csv format. If this is from Facebook and you own the account, the comments can be downloaded from www.Socialfy.pw. Ensure the following columns are included: Message, Date/Time"),
                       fileInput("file1", "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",".csv"),
                                 buttonLabel = "Browse"),
                       useShinyalert(),
                       uiOutput("ui.action"),
                       DTOutput("contents")
              )
  ))

#takes a list of the assigned names of datasets defined by user on upload. use this as the options inthe dropdown menus
#dropdown.options=as.character(uploads$Assigned.Name)

generalPanel=fluidRow( 
  h3(strong("General Text Analysis"), align="center"),
  h5("This section provides an analysis of the most popular times and dates for posting comments, the individual words and word pairs frequencies, and a comparison of the distribution of comment lengths for two different debates. Switch between the tabs."),
  br(),
  tabsetPanel(
  
    tabPanel("Word Frequencies",
             h4(strong("Frequency Analysis"), align="left"),
             h5("This page provides the most frequent words, adjectives, and key phrases used in an online debate. Use the slider to select the number of words to appear in the chart. "),
             br(),
             box(width = 12,
                 column(selectInput("AdjectivesID", "Select Debate", choices =uploads$Assigned.Name), width=6),
                 column(sliderInput("maxfreqID", "Select Maximum Number", min=1, max=30, value = 10), width=6)),
             #plotOutput("Frequencies2")%>% withSpinner(),
             box(title= "Top Adjectives", plotOutput("Adjectives")%>% withSpinner()),
             box(title = "Top Key Words", plotOutput("Keywords")%>% withSpinner())
    ),
    tabPanel("Comment Lengths",
             h4(strong("Comparisons of Comment Length Distributions"), align="left"),
             h5("This page provides comparisons of the distributions of comment lengths for different debates. Select the two debates you wish to compare."),
             br(),
             box(selectInput("CommentLengthID", "Select Debate 1", choices =uploads$Assigned.Name),
                 plotlyOutput("CommentLengths1")%>% withSpinner()),
             box(selectInput("CommentLength2ID", "Select Debate 2",choices =uploads$Assigned.Name),
                 plotlyOutput("CommentLengths2")%>% withSpinner())),
    tabPanel("Bigram Network",
             h4(strong("Bigram Networks"), align="left"),
             h5("This page provides a network of the most frequent word pairs (bigrams) used in a debate. Use the slider to select the minimum word pair frequency to display in the network. If this error message appears 'Error:replacement has 1 row, data has 0', please reduce the value on the slider."),
             br(),
             box(width=12,
                 column(selectInput("BigramID", "Select Debate", choices =uploads$Assigned.Name), width=6),
                 column(sliderInput("minbigramID", "Select Minimum Frequency", min=1, max=100, value = 15), width = 6)),
             plotOutput("Bigrams")%>% withSpinner())
  )
  
)


topicPanel=fluidRow(
  h3(strong("Topic Models"), align="center"),
  tabsetPanel(
    
    tabPanel("Distribution",
             h4(strong("Distributions of comments in topics"), align="left"),
             selectInput("TopicDistID", "Select Debate", choices =uploads$Assigned.Name),
             br(),
             plotOutput("TopicDistributions")%>% withSpinner()),
    # tabPanel("TopicIDF",
    #          h4(strong("Distributions of comments in topics test"), align="left"),
    #          selectInput("TopicIDFID", "Select Debate", choices =uploads$Assigned.Name),
    #          br(),
    #          plotOutput("TopicIDF")%>% withSpinner()),
    tabPanel("Topics by Words",
             h4(strong("Top words in topics"), align="left"),
             p("This page provides the words most associated with each topic. There are three different options for visualisation on this page."),
             column(6,
             tags$b("LDA"),
             p("uses the Latent Dirichlet Allocation model (Blei et. al., 2003) to extract topics from text and plots the words most associated with each topic. It works best using longer texts (the average length of comments for each discussion can be found in the General tab.")), 
             column(6,
             tags$b("LDA-IDF"),
             p("also uses the Latent Dirichlet Allocation model but displays the keywords in each topic using a TF-IDF measure which gives precedence to the words most unique to that topic.")),
             br(),
             box(width=12,
                  selectInput("TopicWordsID", "Select Debate", choices =uploads$Assigned.Name))
             , 
             
             #box(title = "LDA Topics", plotOutput("Topicwords", height = "600px")%>% withSpinner()),
             tabBox(title = "Model Algorithm", selected = "LDA", width=12,
                    tabPanel("LDA", plotOutput("Topicwords")%>% withSpinner()),
                    tabPanel("LDA-IDF", plotOutput("TopicIDF")%>% withSpinner())
             )
    ),
    
    # tabPanel("Topics by Bigrams",
    #          h4(strong("Top word pairs in topics"), align="left"),
    #          p("This page provides the phrases most associated with each topic."),
    #          br(),
    #          selectInput("DebateTopicBigramID", "Select Debate",choices =uploads$Assigned.Name),
    #          
    #          plotOutput("Topicbigrams", height = "600px")%>% withSpinner()),
    # 
    tabPanel("Word Associations",
             h4(strong("Topic Word Association Cloud"), align="left"),
             p("This page displays words within topics which are most associated with each other. Choose a discussion from the dropdown box and type a word to view other words associated with it. If no words appear, please reduce the minimum correlation using the slider."),
             br(),
             selectInput("TopicAssocID", "Select Debate", choices =uploads$Assigned.Name),
             box(title = "Word Associations", width=12, 
                 column(textInput("TopicTerm","Type term (case sensitive)"), width = 6), 
                 column(sliderInput("corrID", "Select Correlation", min=0.0, max=1.0, value = 0.5), width = 6)), 
             verbatimTextOutput("Associations"),
             wordcloud2Output("AssociationsCloud")%>% withSpinner()),
    # tabPanel("Topic Number Selection",
    #          h4(strong("Topic Number Selection"), align="left"),
    #          br(),
    #          box(width = 12, 
    #              column(selectInput("DebateTopicNumID", "Select Debate", choices = c("animal", "abuse", "visa","bsl tweets", "ATC tweets", "diabetes")), width=6),
    #              column(numericInput("Topicchoice", "Choose maximum number of topics", value = 4, min = 2, max = 20, step = 2), width = 6)
    #          ),
    #          plotOutput("Topicnumber")%>% withSpinner()
    # ),
    tabPanel("Topic Visualisations", 
             h4(strong("LDAvis: LDA Topic Model Visualisation"), align="left"),
             p("This page provides an alternative topic model visualisation (Sievert and Shirley, 2014) which is split into two sections:"),
             column(6,
             tags$b("Left"),
             p("showing topic distances from each other based on the types of words in each")),
             column(6,
             tags$b("Right"),
             p("showing the top 30 words pairs in each topic (red bar) and overall in the dataset (blue bar). I recommend setting the relevance metric to 0.6 to get a more representative list of words in each topic.")),
             p("This visualisation is interactive, hover over each topic number to view the words in each topic, or select each word to view which topics it is relevant to."),
             br(),
             #DropdownMenuUI("debates"),
             selectInput("ServisDebate", "Select Debate", choices =uploads$Assigned.Name),
             br(),
             visOutput("SerVis") %>% withSpinner()
             
    )
    
    
  )
)



sentimentPanel=fluidRow(
  h3(strong("Sentiment Analysis"), align="center"),
  p("In this section, you can analyse debates based on the sentiment of the words and comments used. Three sentiment lexicons are available for comparison:"), 
  column(4,
  tags$b("Afinn"),
  p("uses a 10-point scale ranging from -5 (very negative) to 5 (very positive).")), 
  column(4,
  tags$b("NRC"),
  p("uses categorical scale to measure 2 sentiments (positive and negative), and 6 emotions (anger, anticipation, trust, joy, fear, and disgust).")),
  column(4,
  tags$b("Bing"),
  p("uses a binary scale to measure positive and negative. ")),
  
  br(),
  column(12,

  tabsetPanel(
    tabPanel("Distribution",
             h4(strong("Sentiment Distributions"), align="left"),
             p("This page provides analysis of the number of comments in each sentiment category, for each debate. First select the debate, then use the tabs on the right-hand side to switch between the different sentiment lexicons."),
             br(),
             box(selectInput("SentDistID", "Select Debate", choices =uploads$Assigned.Name)),
             tabBox(
               title="Sentiment Lexicons", side = "left", height = "250px", width = NULL, selected = "Afinn", 
               tabPanel("Afinn", plotOutput("SentimentDistributionsAfinn")%>% withSpinner()), 
               tabPanel("NRC", plotOutput("SentimentDistributionsNRC")%>% withSpinner()), 
               tabPanel("Bing", plotOutput("SentimentDistributionsBing")%>% withSpinner())
             )),
    
    tabPanel("Sentiment by Comments",
             h4(strong("Top Comments"), align="left"),
             p("This page provides analysis of the comments most associated with each sentiment category, for each debate. First select the debate, then use the tabs underneath to switch between the different sentiment lexicons. Hover over the graphs to see a preview of the comment text, or type in the comment number in the right-hand box to view to whole text."),
             br(),
             box(selectInput("SentCommentsID", "Select Debate", choices =uploads$Assigned.Name)),
             box(selectInput("your_choices", label = "Select comment number", choices =uploads$Assigned.Name)),
             span(textOutput("text"), style="color:darkgreen"),
             tabBox(
               title="Sentiment Lexicons", side = "left", height = "250px", width = NULL, selected = "Afinn", 
               #downloadButton("downloadData", "Download"),
               tabPanel("Afinn",   plotlyOutput("TopCommentsAfinn", height = "800px")%>% withSpinner()), 
               tabPanel("NRC", plotlyOutput("TopCommentsNRC", height = "800px")%>% withSpinner()), 
               tabPanel("Bing", plotlyOutput("TopCommentsBing", height = "800px")%>% withSpinner())
             )),
    
    tabPanel("Sentiment by Words",
             h4(strong("Top Words"), align="left"),
             p("This page provides analysis of the words most associated with each sentiment category, for each debate. First select the debate, then use the tabs on the right-hand side to switch between the different sentiment lexicons."),
             strong("In the Bing wordcloud, red text are negative words and green text are positive words."),
             br(),
             box(selectInput("SentWordsID", "Select Debate", choices =uploads$Assigned.Name)),
             tabBox(
               title="Sentiment Lexicons", side = "left", height = "250px", width = NULL, selected = "Afinn", 
               #downloadButton("downloadData", "Download"),
               tabPanel("Afinn",   plotOutput("TopwordsAfinn", height = "800px")%>% withSpinner()), 
               tabPanel("NRC", plotOutput("TopwordsNRC", height = "800px")%>% withSpinner()), 
               tabPanel("Bing", plotOutput("TopwordsBing", height = "800px")%>% withSpinner())
             ))
    
  ))
)

#Custom Theme----

### creating custom theme object
theme_commons <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Tahoma"
  ,appFontColor = "rgb(45,45,45)"
  ,bodyBackColor = "rgb(237,235,234)"
  
  ### header
  ,logoBackColor = "rgb(0,101,72)"
  
  ,headerButtonBackColor = "rgb(0,101,72)"
  ,headerButtonIconColor = "rgb(0,255,155)"
  ,headerButtonBackColorHover = "rgb(237,235,234)"
  ,headerButtonIconColorHover = "rgb(0,255,155)"
  
  ,headerBackColor = "rgb(0,101,72)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(0,101,72)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 5
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "rgb(255,255,255)"
  
  ,sidebarUserTextColor = "rgb(253,226,207)"
  
  ,sidebarSearchBackColor = "rgb(237,235,234)"
  ,sidebarSearchIconColor = "rgb(0,51,0)"
  ,sidebarSearchBorderColor = "rgb(0,255,155)"
  
  ,sidebarTabTextColor = "rgb(253,226,207)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(253,226,207)"
  ,sidebarTabTextColorSelected = "rgb(0,51,0)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(237,235,234)"
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "rgb(0,51,0)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px"
  
  #boxes
  ,boxBackColor = "rgb(0,101,72)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(237,235,234)"
  ,boxPrimaryColor = "rgb(67,139,3)"
  ,boxSuccessColor = "rgb(112,173,71)"
  ,boxWarningColor = "rgb(237,125,49)"
  ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor = "rgb(253,226,207)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(100,100,100)"
  ,tabBoxTabTextColorSelected = "rgb(0,101,72)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = "rgb(0,101,72)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(67,139,3)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(0,51,0)"
  ,buttonBorderRadius = 10
  
  ,buttonBackColorHover = "rgb(253,226,207)"
  ,buttonTextColorHover = "rgb(0,101,72)"
  ,buttonBorderColorHover = "rgb(0,51,0)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(238,238,238)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)
#Body====
body=dashboardBody(
  #shinyDashboardThemes(
  #theme = "grey_light"),
  theme_commons,
  tabItems(
    tabItem(tabName = "Welcome", welcomePanel),
    
    tabItem(tabName = "General", generalPanel),
   
    tabItem(tabName = "Topics", topicPanel),
    
    tabItem(tabName = "Sentiment", sentimentPanel)
    
    #tabItem(tabName = "Networks", networkPanel),
    
    #tabItem(tabName = "Downloads", downloadPanel)
    
  ))
#define ui----
ui <- dashboardPage(title="TheGist", header=header, sidebar=sidebar, body=body)


