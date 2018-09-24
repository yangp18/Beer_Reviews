# Yang Peng
# 09/23/2018

library(ggplot2)
library(ggcorrplot)
library(randomForest)

# read dataset
beer.reviews <- read.csv("../data/beer_reviews.csv")


# descriptive analysis
dim(beer.reviews)
head(beer.reviews)    
View(beer.reviews)
str(beer.reviews)
attach(beer.reviews)

# five number summarys 
summary(beer_abv)
summary(review_overall)
summary(review_aroma)
summary(review_appearance)
summary(review_palate)
summary(review_taste)


# data cleaning
# check missing or empty values for Q1
sum(is.na(beer_abv)|beer_abv=="")
sum(is.na(brewery_name)|brewery_name=="")
# remove NA values and save into d2
d2 <- na.omit(beer.reviews)#remove rows contains NA value
dim(d2)
(dim(beer.reviews)[1]-dim(d2)[1])/dim(beer.reviews)[1] #check the percent of the removed rows. 
# It shows that the percentage is smaller than 5%, so we used the cleaned dataset d2 for the following analysis.

# check the length of unique brewery_name and brewery_id
length(unique(d2$brewery_name))
length(unique(d2$brewery_id))
# It turns out the number of unique brewey_id is greater than the number of unique brewery_name,
# which means different id may have same brewery_name. It can be caused by cutting off by the length of charater.
length(unique(d2$beer_name))
length(unique(d2$beer_beerid))
# Same as beer_id and beer_name. So here I used brewery_id as well as beer_beerid as the index of different categories.



#1. Which brewery produces the strongest beers by ABV%?

# which(colnames(beer.reviews)=="beer_abv")
# find the average beer_adv group by brewery_id
a <- aggregate(d2$beer_abv, list(d2$brewery_id), mean,na.omit=T)
dim(a)
# find the unique brewery name which has the max beer_abv
ii <- a$Group.1[a$x==max(a$x)]
unique(d2$brewery_name[d2$brewery_id==ii])#[1] Schorschbräu

hist(a$x)
# we can see from the barplot that only one brewery has tha noticeable highest abv, which is Schorschbräu.
barplot(a$x)



# 2. If you had to pick 3 beers to recommend using only this data, which would you pick?
# find the review frequency table group by beer_beerid
fq <- table(d2$beer_beerid)
# The length of beer_beerid which has the max frequency is 49012. 
# Solely depends on the number of review to determine the top 3 popular is not enough.
length(names(fq==max(fq)))
quantile(fq)# There are 75% types of beer_beerid has less than 10 reviews.
mean(fq)# mean is  30.98892

ggplot(as.data.frame(fq), aes(x=fq)) + geom_histogram()

# Use review_overall to determine the popularity
table(d2$review_overall)#there are  89017 reviews has 5.
# find the average review_overall group by beer_beerid
b <- aggregate(d2$review_overall, list(d2$beer_beerid), mean,na.omit=T)
c <- b[b$x==max(b$x),]
# find how many beer_beerid has the max review_overall. 
dim(c)[1]#502
# still cannot get the top 3.


# find the number of reviews of those beer which has the max review_overall

num <- NULL
j <- 0
for(i in 1:dim(c)[1]){
  num[i] <- dim(d2[d2$beer_beerid==c[i,]$Group.1,])[1]
  
  if(num[i]>1){
    j <- j+1
    print(num[i])
  }
 
  
}
print(j)
length(num[num>1])
length(num[num>5])
# It shows that majority of 502 beer has 1 review. And 35 of them which has more than one review has less than 5 reviews.

# Then, I want to find the intersection of those beer has the max mean review_overall and has the max mean of sum of other four subreview scores.
subreviews <- rowSums(d2[, c(5,6,9,10)])
d2$subrevtol <- subreviews
dim(d2)
f <- aggregate(d2$subrevtol, list(d2$beer_beerid), mean,na.omit=T)
f[1:10,]
g <- f[f$x==max(f$x),]
dim(g)
g2 <- intersect(g$Group.1,c$Group.1)
length(unique(g2))

# find the number of reviews of those beer in the intersection
num <- NULL
j <- 0
for(i in 1:length(g2)){
  num[i] <- dim(d2[d2$beer_beerid==g2[i],])[1]
  
  if(num[i]>1){
    j <- j+1
    print(num[i])
  }
  
  
}
print(j)
length(num[num>1])
length(num[num>5])
# It shows that all of them only has one review which doesn't make much sense to be the top 3 beer.

# plot for "mean review_overall" vs "review frequency". It turns out higher mean review_overall also has corresponding higher review counts.
t <- as.data.frame(cbind(b[,2],fq))
colnames(t) <- c("mean review_overall","review frequency")
ggplot(t, aes(x=`mean review_overall`, y=`review frequency`)) + geom_point(shape=1,color="blue")



# A good beer should has a reasonable amount of reviews and has a high review score as well.
# In other words, the number of the reviews n is be large, the mean of the review_overall is large and the variance of review_overall is small consist of the criterion of ranking beers.
# So, here I used the lower bound of 95% confidence interval as the measurement of ranking beers.
# x_bar-t*sd/sqrt(n)
# the number of reviews cannot be too small, based on the scatter plot above, here I use 100 as the threshold.

ss <- split.data.frame(d2[,c(4,13)],d2$beer_beerid)

getCI <- function(x){
  if(dim(x)[1]<100){
    lowci=0
  }else{
    b <- t.test(x$review_overall)
    lowci <- b$conf.int[1]
  }
  
}

top3 <- sort(sapply(ss,getCI),decreasing = T)[1:3]
str(top3)
unique(d2$beer_name[d2$beer_beerid %in% names(top3)])
dim(d2[d2$beer_beerid == names(top3)[3],])[1]
# Citra DIPA               Heady Topper             Trappist Westvleteren 12   


# 3. Which of the factors (aroma, taste, appearance, palette) are most important in determining the overall quality of a beer?
reviewMt <- cbind(d2$review_overall,d2$review_aroma,d2$review_appearance,d2$review_palate,d2$review_taste)
revcor <-round(cor(reviewMt),2)
colnames(revcor) <- rownames(revcor) <- c("overall","aroma","appearance","palate","taste")
ggcorrplot(revcor, hc.order = TRUE,lab = TRUE)
# taste 

# fit a linear model for diff reviews
lmfit <- lm(d2$review_overall~d2$review_aroma+d2$review_appearance+d2$review_palate+d2$review_taste)
summary(lmfit)
# based on the p-value and t-statistic can also get the same conclusion.

plot(lmfit,1)
abline(lmfit)  

# since the residual plot appears a downward trend. Then, I add review_time in the model to see if R-square is improved and the patterns in residual plot is disappeared.
lmfit2 <- lm(d2$review_overall~d2$review_time+d2$review_aroma+d2$review_appearance+d2$review_palate+d2$review_taste)
summary(lmfit2)
plot(lmfit2,1)
abline(lmfit)  
# R-square improved trivially and didn't remove the residual pattern

# One may guess maybe it is because of the range of reviews. review is from 0 to 5. So I transformed the review_overall to be from -Inf to Inf.
y1 <- d2$review_overall/5
y <- log((y1+0.001)/(1.001-y1))
lmfit3 <- lm(y~d2$review_aroma+d2$review_appearance+d2$review_palate+d2$review_taste)
summary(lmfit3)
# R-square reduced to 35% which doesn't make sense.So, we still regard lm is the most appropriate one.

### random forest
rf <- randomForest(d2$review_overall~d2$review_aroma+d2$review_appearance+d2$review_palate+d2$review_taste, 
                    importance=TRUE, 
                    ntree=20)
rf
varImpPlot(rf)

# 4. Lastly, if I typically enjoy a beer due to its aroma and appearance, which beer style should I try?
subtol2 <- d2$review_aroma+d2$review_appearance
d2$subtol2 <- subtol2

ff <- aggregate(d2$subtol2, list(d2$beer_style), mean,na.omit=T)
ff[1:10,]
gg <- ff[ff$x==max(ff$x),]
dim(gg)
gg
dim(d2[d2$beer_style==gg$Group.1,])[1]
#  American Double / Imperial Stout 


