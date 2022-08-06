library(shiny)
library(twitteR)

ui <- fluidPage(
  textInput("handle", "Search Tweets:"),
  sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=1500, value = 5),
  downloadButton("download", "Download File"),
  dataTableOutput("table")
)


server <- function(input, output) {

  consumerKey = "My key"
  consumerSecret = "My secret"
  accessToken = "My token"
  accessSecret = "My secret"

  my_oauth <- setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                                  access_token = accessToken, access_secret = accessSecret)

  output$table <- renderDataTable({
    TweetFrame<-function(searchTerm, maxTweets)
    {
      twtList<-searchTwitter(searchTerm,n=maxTweets)
      twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
      twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
      return(twtList1)

    }
    entity1<-reactive({entity1<-TweetFrame(input$handle, input$maxTweets)})
    output$table <- renderDataTable({tab<-entity1()[1]})
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                       content = function(file){
                                         write.csv(entity1(), file)
                                       }
                                       )
  })
}

shinyApp(ui = ui, server = server)