library(rvest)
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
