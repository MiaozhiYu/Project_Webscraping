# setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Webscraping/Miaozhi_WebScraping/')
data_rating = read.csv('./data/rating_descriptive.csv')
head(data_rating)
names(data_rating)
attach(data_rating)
data_rating$pages = as.numeric(gsub('pages','',pages))
data_rating$country = gsub('England','United Kingdom',data_rating$country)

library('dplyr')
library(ISOcodes)
library(ggmap)
library(wordcloud)
library(shinyBS)
library(DT)
library(SnowballC)
library(tm)
library(stringr)
library(textcat)
library(memoise)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

data_rating$publish.date = as.numeric(substrRight(gsub('\\D','',publish.date),4))
data_rating$publish.date[8]=-800
data_rating$publish.date[25]=-800
data_rating$publish.date[39]=-430                                      
data_rating$publish.date[55]=-2100
data_rating$publish.date[61]=-431
data_rating$publish.date[64]=-17

time_period = function(x){
  index = 1:length(x)
  period = 1:length(x)
  for(i in index){
    if(is.na(x[i])){
      period[i]=NA
    }else{
     if(x[i]<1600){
       period[i] = 16
     }else{
       if(x[i]<1700){
         period[i]=17
       }else{
         if(x[i]<1800){
         period[i]=18
         }else{
           if(x[i]<1900){
             period[i]=19
           }else{
             if(x[i]<2000){
               period[i]=20
             }else{
               period[i]=21
             }
           }
         }
       }
     }
    }
  }
  return(period)
}


periods = function(x){
  index = 1:length(x)
  period = 1:length(x)
  for(i in index){
    if(is.na(x[i])){
      period[i]=NA
    }else{
      if(x[i]<1600){
        period[i] = 'Before 17th century'
      }else{
        if(x[i]<1700){
          period[i]='17th century'
        }else{
          if(x[i]<1800){
            period[i]='18th century'
          }else{
            if(x[i]<1900){
              period[i]='19th century'
            }else{
              if(x[i]<2000){
                period[i]='20th century'
              }else{
                period[i]='21th century'
              }
            }
          }
        }
      }
    }
  }
  return(period)
}

century = time_period(data_rating$publish.date)
period = periods(data_rating$publish.date)
data_rating = cbind(data_rating,century,period)

new = group_by(data_rating[,c('title','century','country')],country,century) %>% summarise(count=n())
#geo_loc = geocode(as.character(new$country))
#geo_loc[9,]=NA
#geo_loc[10,]=geocode('England')
#geo_loc[8,]=geocode('poland')
#geo_loc[1,]=geocode('German')
#write.csv(geo_loc,'geo.csv')
geo_loc = read.csv('./data/geo.csv')
new = data.frame(new,lon=geo_loc$lon,lat=geo_loc$lat)
new$lon[7]=-74.297333
new$lat[7]=4.570868

reccomen = group_by(data_rating[,c('type','period','country','book.image')],country,type,period) %>% summarise(count=n())

color_axis = "{
                 values:[1,4,8],
colors:['#ABC8E2', '#375D81', '#183152']
}"

grepNonASCII <- function(x) {
  asc <- iconv(x, "latin1", "ASCII")
  ind <- is.na(asc) | asc != x
  which(ind)
}

#text=read.table('text.txt')
#textCorpus <- Corpus(VectorSource(text))
# jeopCorpus <- Corpus(VectorSource(text))
# jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
# jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
# jeopCorpus <- tm_map(jeopCorpus, removeWords, c("latin1", "ASCII"))
# jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
# 
# jeopCorpus <- tm_map(jeopCorpus, stemDocument)
# #wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
# a = grepNonASCII(jeopCorpus)
# a

#  setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Webscraping/Miaozhi_WebScraping/reviews_good')
#  dir()
# 
#  reviews = list()
# 
#  for(book in dir()){
#    reviews[[book]]=read.csv(book,stringsAsFactors = F)
#  }
#  setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Webscraping/Miaozhi_WebScraping')
# saveRDS(reviews,'reviews.RDS')

reviews = readRDS('./data/reviews.RDS')

getTermMatrix <- memoise(function(book) {
  # sprintf("F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Webscraping/Miaozhi_WebScraping/reviews_good/%s",
  text = reviews[[book]]
  a=text$review[textcat(text$review)=='english']
  jeopCorpus <- Corpus(VectorSource(a))
  print(jeopCorpus)
  jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  jeopCorpus <- tm_map(jeopCorpus, removeWords, c("latin1", "ASCII",'book','read','one','novel','the','The'))
  jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
  jeopCorpus <- tm_map(jeopCorpus, stemDocument)
  myDTM = TermDocumentMatrix(jeopCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
  return(jeopCorpus)
})


#wordcloud(jeopCorpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
