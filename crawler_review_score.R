library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
lego_review = read_html("http://www.imdb.com/title/tt1490017/reviews?ref_=tt_urv")
reviews = html_nodes(lego_review, xpath = '//*[@id="tn15content"]//p')
reviews = html_text(reviews)
reviews = reviews[-length(reviews)]
reviews = reviews[- grep("review may contain spoilers", reviews)]
scores = html_attr(html_nodes(lego_review, "#tn15content img"), "alt")
scores = scores[!is.na(scores)]
scores = scores[-1]
scores = scores[-length(scores)]
scores = sapply(strsplit(scores, split='/', fixed=TRUE), function(x) (x[1]))
revscor = data.frame(scores, reviews)

#wordcloud
test1 = VCorpus(VectorSource(reviews[1]))
test1 = tm_map(test1, stripWhitespace)
test1 = tm_map(test1, tolower)
test1 = tm_map(test1, removeWords, stopwords("the"))
test1 <- tm_map(test1, PlainTextDocument)
test1 <- tm_map(test1, stemDocument)
wordcloud(test1, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))