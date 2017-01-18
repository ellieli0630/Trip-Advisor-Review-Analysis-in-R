ta <- read.csv("~/Desktop/taAllCities.csv")
str(ta)
ta$reviewDate <- as.Date(ta$reviewDate, "%m/%d/%Y")
ta$rating <- as.factor(ta$rating)
ta$reviewText <- as.character(ta$reviewText)
ta$service <- as.factor(ta$service)
ta$vibe <- as.factor(ta$vibe)
ta$desert <- as.factor(ta$desert)
ta$bathroom <- as.factor(ta$bathroom)
ta$drink <- as.factor(ta$drink)
ta$cost <- as.factor(ta$cost)
ta$music <- as.factor(ta$music)
ta$location <- as.factor(ta$location)
ta$parking <- as.factor(ta$parking)
ta$lunch <- as.factor(ta$lunch)
ta$breakfast <- as.factor(ta$breakfast)
ta$dinner <- as.factor(ta$dinner)
ta$ranking <- as.factor(ta$ranking)
str(ta)

library(corrgram)
corrgram(ta, order=FALSE, lower.panel=panel.pie, upper.panel=NULL, text.panel=panel.txt, main="", cex.labels=0.8)

#models
#poisson
ta$rating <- as.numeric(ta$rating)
poisson <- glm(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer+avgHelpfulnessOfReviewer, family="poisson", data = ta)
poisson
#marginal effects
library(mfx)
poissonmfx(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer+ avgHelpfulnessOfReviewer, data = ta)
#OLS run on log
ta$lograting<-log(ta$rating)
ols<- lm(lograting~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, data =ta )
summary(ols)
ols1<- lm(lograting~ service+vibe+desert+bathroom+cost+music+parking+lunch+breakfast+totalReviewsOfReviewer+avgHelpfulnessOfReviewer, data =ta )
summary(ols1)

#Logit
ta$ratingBinary <- ifelse(ta$rating > 3, 1, 0)
logit <- glm(ratingBinary ~ cost + service, data = ta, family = "binomial")
summary(logit)
logitCost <- glm(ratingBinary ~ cost, data = ta, family = "binomial")
summary(logitCost)
logitService <- glm(ratingBinary ~ service, data = ta, family = "binomial")
summary(logitService)

#dollar amount
ta$dollarChar <- as.character(ta$dollars)
ta$dollarAmt <- as.factor(ifelse(tolower(substr(ta$dollarChar, 1, 1)) == 'n', 0, nchar(as.vector(ta$dollars))))

bostonlocal <- ta[ which( ta$reviewerType=="L"|ta$city=="Boston"), ]
bostontraveller <-ta[ which( ta$reviewerType=="T"|ta$city=="Boston"), ]
sftraveller <-ta[ which( ta$reviewerType=="T"|ta$city=="San Francisco"), ]
sflocalr <-ta[ which( ta$reviewerType=="L"|ta$city=="San Francisco"), ]
slclocal <-ta[ which( ta$reviewerType=="L"|ta$city=="Salt Lake City"), ]
sltraveller <-ta[ which( ta$reviewerType=="T"|ta$city=="Salt Lake City"), ]

sftraveller$rating<-as.numeric(sftraveller$rating)
sftravelleravgrating<-mean(sftraveller$rating)
sflocalr$rating<-as.numeric(sflocalr$rating)
sflocalavgrating<-mean(sflocalr$rating)
sltraveller$rating<-as.numeric(sltraveller$rating)
sltravelleravgrating<-mean(sltraveller$rating)
slclocal$rating<-as.numeric(slclocal$rating)
slclocalavgrating<-mean(slclocal$rating)

