#                                  R-Practice
#worksheet-1
#problem-3
fact=1
fac<-function(n){
  for(i in 1:n){
    fact=fact*i
  }
  return(fact)
}
fac(5)


#problrm-4
eular<-function(n){
  e=(1+(1/n))^n
  print(e)
}
eular(5)

#problem-5
seat<-read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat


#worksheet-2
#problem-1
msc<-seat[seat$Roll.No>10000000, ]
msc.count<-count(msc)
bs<-seat[seat$Roll.No<10000000, ]
bs.count<-count(bs)
paste("We have ",bs.count,"BS students and",msc.count,"MSc students in Data Science lab")
student<-seat[seat$Roll.No==231080049,]
student

#problem-2
#1
cricket <- read.csv("battingbowling.csv")
cricket
allrounders<-cricket[cricket$Batting>25 & cricket$Bowling<40, ]
allrounders
 
#2
allrounders_count<-table(allrounders$Team)
allrounders_count
most_allrounders<-which.max(allrounders_count)
most_allrounders

#3
least_allrounders<-which.min(allrounders_count)
least_allrounders

#problem-3(not working)
#plot(x=1:10, y=1:10, type="l", xlab = "x", ylab = 'y', main = "Y=X Plot")
plot(1:10,type="l",col="red")

#problem-4
eular<-function(n){
  e=(1+(1/n))^n
  return(e)
}
n=1:1e3
fx<-eular(n)
fx
plot(n,fx,type="l")
abline(h=exp(1),col="red")


#worksheet-3
#problem-1
#a
x=rbinom(n=1000,size=1,prob=0.5)
prop=mean(x)
prop
#b
y<-rbinom(n=1000,size=1,prob=0.3)
prop<-mean(y)
prop
#problem-2
#a
col=c("red","green","blue")
balls<-sample(col,size="1",prob=c(3/7,2/7,2/7))
balls

#b
p_vec<-numeric(length=3)
A<-matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2),nrow=3,ncol=3)
for( k in 1:ncol){
  p_vec[k]=norm(matrix[ ,k],type="2")
}
p_vec
p_vec<-p_vec/sum(p_vec)
sample(1:3,size=1,prob=p_vec)


#c
runif(1,min=0,max=5)

#problem-3
#a
exceed <- function()
{
  count <- 0
  sum=0
  while(sum<1)
  {
    sum=sum+runif(1,min=0,max=1)
    count=count+1
  }
  return(count)
}
exceed()
#b
store <- numeric(length = 1000)
for(r in 1:1000)
{
  store[r] <-exceed()
}
store
#c
mean(store)
exp(1)

#problem-4
#a
candles<-function(age){
  count<-0
  remain<-age
  while(remain>0){
    remain=remain-sample(1:remain,1)
    count<-count+1
  }
  return(count)
}
candles(25)

#b
store<-numeric(length=1000)
for (i in 1:1000){
  store[i]<-candles(25)
}
store

#c
mean(store)

#d
candles(30)
store<-numeric(length=1000)
for (i in 1:1000){
  store[i]<-candles(30)
}
store
mean(store)


A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)

# p_vec will store p_i eventually
p_vec <- numeric(length = 3)

# running a loop for each column
# to find the norm (the numerator)
for(k in 1:ncol(A))
{
  p_vec[k] <- norm(A[ ,k], type = "2")
}
# divide by the sum
p_vec
p_vec <- p_vec/sum(p_vec)

sample(1:3, size = 1, prob = p_vec)


#worksheet-6
#problem-1
library(tidyverse)
library(rvest)
html <- read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
post_doc<-html%>%html_elements(".head2")%>%html_text()
post_doc

#problem-2
html <- read_html("https://www.imdb.com/chart/top/")
movies<-html%>%html_elements(".ipc-title.ipc-title--base.ipc-title--title.ipc-title-link-no-icon.ipc-title--on-textPrimary.sc-b51a3d33-7.huNpFl.cli-title")%>%html_text()
movies<-sapply(1:250, function(k) strsplit(movies[k],as.character(k))[[1]][2])
movies<-substring(movies,3)
movies

#problem3
#(a) movie name
#already done


#(b)-movie Year
year<-html%>%html_elements(".sc-b51a3d33-6.faLXbD.cli-title-metadata-item")%>%html_text()
year<-strsplit(year,' ')
year<-clean.info[seq(1,749,3)]
year
 

#(c)-movie rating
rating<-html%>%html_elements(".ipc-rating-star.ipc-rating-star--base.ipc-rating-star--imdb.ratingGroup--imdb-rating")%>%html_text()
rating<-substring(rating,1,3)
rating

#(d)-movie number of votes
votes<-html%>%html_elements(".ipc-rating-star.ipc-rating-star--base.ipc-rating-star--imdb.ratingGroup--imdb-rating")%>%html_text()
votes<-substring(votes,5, )
votes



#worksheet-7
#problem-1
#movie name and rating already done 
#unweighted mean

titles<-html%>%html_elements(".ipc-title-link-wrapper")%>%html_attr("href")
titles<-titles[1:250]
titles<-strsplit(titles,"/")
titles<-sapply(titles,function(k) k[3])
titles
url<-paste0("https://www.imdb.com/title/",titles,"/ratings/?ref_=tt_ov_rt")
url
for(i in 1:250){
print(i)
html<-read_html(url[i])
unweighted_mean<-html%>%html_elements(".sc-32706d9c-1.dRWuAv")%>%html_text()
unweighted_mean<-substring(unweighted_mean,1,3)
}
data<-data.frame(movies,unweighted_mean,rating)
data

#problem-2
img<-250
img<-paste("https://www.imdb.com/title/",titles,sep="")
img

#problem-4
html <- read_html("https://www.boxofficemojo.com/chart/top_lifetime_gross")
collection<-html%>%html_table()
data<-collection[[1]]
#removing commas
gross<-data$`Lifetime Gross`
gross<-substring(gross,2, )
gross <- gsub(",","", gross)
data$`Lifetime Gross`<-gross
data


#