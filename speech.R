library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)

#My functions
###############################################################################
myfunc.webcrape <- function(input_url, input_css = "//null",input_xpath = "//null", cleanup_label = c("%null"))
{
  if(input_css == "//null" && input_xpath == "//null")
  {
    stop("Please input css or xpath")
    #return()
  }
  if(input_css != "//null" && input_xpath != "//null")
  {
    stop("Please choose between css or xpath not both")
    #return()
  }
  web_url = input_url
  contents = read_html(web_url)
  if(input_xpath != "//null")
  {
    contents = html_nodes(contents, xpath = input_xpath)
  }
  else
  {
    contents = html_nodes(contents, input_css)
  }
  if(cleanup_label[1] != "%null")
  {
    for(i in 1:length(cleanup_label))
    {
      contents = contents[- grep(cleanup_label[i], contents)] 
    }
  }
  contents = html_text(contents)
  return(contents)
}


myfunc.wordcloud <- function(input_words, remove_words = c("%null"))
{
  test_pure = paste(input_words, collapse = ". ")
  test1 = VCorpus(VectorSource(test_pure))
  test1 = tm_map(test1, stripWhitespace)
  test1 = tm_map(test1, tolower)
  test1 = tm_map(test1, removeWords, stopwords("english"))
  if(remove_words[1] != "%null")
  {
    test1 = tm_map(test1, removeWords, remove_words) 
  }
  test1 <- tm_map(test1, PlainTextDocument)
  test1 <- tm_map(test1, stemDocument)
  wordcloud(test1, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
}
###############################################################################

#par(mfrow=c(1,2))

#trump speech
###############################################################################
trump_url = "http://www.politico.com/story/2016/07/full-transcript-donald-trump-nomination-acceptance-speech-at-rnc-225974"
trump_speech = myfunc.webcrape(trump_url, input_xpath = '//*[contains(@class, "story-text")]//p', cleanup_label = c("timestamp"))
myfunc.wordcloud(trump_speech, remove_words=c("will"))
###############################################################################

#Hillary speech
###############################################################################
hillary_url = "http://www.nytimes.com/2016/07/29/us/politics/hillary-clinton-dnc-transcript.html?_r=2"
hillary_speech = myfunc.webcrape(hillary_url, input_xpath = '//*[contains(@class, "story-body-supplemental")]//div[1]//p', cleanup_label = c("Following is a transcript"))
myfunc.wordcloud(hillary_speech, remove_words=c("will", "'ve", "'re", "'ll"))
###############################################################################


