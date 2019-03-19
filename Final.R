#Final
setwd("C:/Users/bharila/Desktop/Fall19/Stats Prog/Final")
songs<-read.csv("Song-Data.txt",sep="\t")

#Data Structures
musical<-songs[songs$Speechiness<0.33,]
mixed<-songs[songs$Speechiness>=0.33&songs$Speechiness<=0.66,]
speechlikegroup<-c("Mixed", "Musical")
numSongs<-c(nrow(mixed),nrow(musical))
danceability<-c(round(mean(mixed$Danceability),7),round(mean(musical$Danceability),7))
table(mixed$Key)#no value for zero so we put round(0,4) below
ckey<-c(round(0,4), round(prop.table(table(musical$Key))[[1]]*100,4))

ans2<-cbind(speechlikegroup,numSongs,danceability,ckey)
colnames(ans2)<-c("Speech-Like Group", "Number of Songs", "Average Danceability", "Percent of Songs in Key of C")
ans2
#functions
makecentredmatrices<-function(AorB){
  len<-nrow(AorB)#should be =ncol
  AorBdot<-matrix(nrow=len,ncol=len)
  AorBdot[]<-mean(AorB)
  AorBj<-matrix(nrow=len,ncol=len)
  AorBi<-matrix(nrow=len,ncol=len)
  for(j in 1:len){
    sumHere=0;
    for(i in 1:len){
      sumHere<-sumHere+AorB[i,j]
    }
    for(i in 1:len){
      AorBj[i,j]<-sumHere
    }
  }
  AorBj<-AorBj/len
  
  for(i in 1:len){
    sumHere=0;
    for(j in 1:len){
      sumHere<-sumHere+AorB[i,j]
    }
    for(j in 1:len){
      AorBi[i,j]<-sumHere
    }
  }
  AorBi<-AorBi/len
  AorB-AorBj-AorBi+AorBdot
}

Cfunction<-function(x,y){
  n<-length(x)#should be =leny
  A<-matrix(nrow=n,ncol=n)
  B<-matrix(nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      A[i,j]<-abs(x[i]-x[j])
    }
  }
  for(i in 1:n){
    for(j in 1:n){
      B[i,j]<-abs(y[i]-y[j])
    }
  }
  aijbar<-makecentredmatrices(A)
  bijbar<-makecentredmatrices(B)
  sumtotal=0
  for(i in 1:n){
    for(j in 1:n){
      sumtotal=sumtotal+aijbar[i,j]*bijbar[i,j]
    }
  }
  sqrt(sumtotal/n)
}

rho<-function(x,y){
  Cfunction(x,y)/sqrt(Cfunction(x,x)*Cfunction(y,y))
}

rho(songs$Energy,songs$Tempo)
#graphics
library(ggplot2)
songs$minor<-!(songs$Key==0|songs$Key==2|songs$Key==4|songs$Key==6|songs$Key==8|songs$Key==10)
songs$SwitchedMode<-songs$Mode==0
ggplot(data=songs, mapping=aes(x=Tempo, y=Danceability, color=SwitchedMode, size=songs$Valence)) + # Aesthetics
  scale_color_manual(values=c("#0000CD","#1E90FF"),
                     name="Type of Scale",
                     labels=c("Major", "Minor"))+
  scale_size(name   = "Valence (Positivity) of Track",
             breaks = c(0.25,0.50,0.75),
             labels = expression(0.25,0.50,0.75),
             guide="legend")+
  guides(size = guide_legend(override.aes = list(linetype=0, shape = 19)))+
  geom_point(alpha=0.5) + 
  geom_smooth(se=FALSE) +
  scale_x_continuous(name="Tempo of Track (beats per minute)") + # Define x-scale
  scale_y_continuous(name="Dance-ability of Track", limits = c(0, 1)) + # Define y-scale
  theme(axis.line = element_blank(), legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        panel.grid = element_line(colour = "#DCDCDC"),
        panel.background = element_rect(fill="white"))
#Strings 
library(stringr)
songsClean<-unlist(lapply(songs$Title, str_remove_all, " - .*| \\(.*\\)"))

#First, establish that the median number of words in a song title is 2.
charTitle<-lapply(songsClean, as.character)
wordsTitles<-(lapply(charTitle,strsplit, " "))
numOfWords<-function(x){
  length(unlist(x))
}
numWordsTitles<-unlist(lapply(wordsTitles, numOfWords))
median(numWordsTitles)

#Second, according to a particular website, the letter "E" is the most 
#frequently used letter in the alphabet. Show that it is the most commonly 
#used letter in the titles of the songs, used 109 times.
titles<-tolower(songsClean)
numLetters<-vector()
for(i in 1:length(letters)){
  numLetters[i]<-sum(unlist(lapply(titles, str_count,letters[i])))
}
letters[which.max(numLetters)]
#Third, determine the second most commonly used letter, 
#which shows up 106 times. 
letters[which.max(numLetters[-5])+1]#+1 accounts for 
                                    #the fact that e was removed 
#DataIO
library(readr)
library(rvest)
library(cld3)
setwd("C:/Users/bharila/Desktop/Fall19/Stats Prog/Final/lyrics")
numWordsEnglish<-0
numWordsSpanish<-0
numTracksEn<-0
numTracksSp<-0

songids<-songs$ID
langs<-songs$Language
for(i in 1:length(songids)){
  url <- paste(songids[i],".html", sep="")
  lyric.pg <- read_html(url)
  lyric.node<-html_nodes(lyric.pg, "#lyric-body-text")
  if(length(lyric.node)==0){
    lyric.node<-html_nodes(lyric.pg, "#content_h")
  }
  lyrics <- gsub(pattern = '<.*?>', replacement = " ", lyric.node)
  lyrics<-str_remove_all(lyrics,"\\[[[:alnum:]]+[[:blank:]]+[[:alnum:]]+\\]")
  lyrics<-str_remove_all(lyrics,"[[:punct:]]*")
  lyrics<-str_remove_all(lyrics,"\r\n")
  words<-unlist(lapply(lyrics,strsplit," "))
  words<-words[!words==""]
  words<-tolower(words)
  numUnique<-length(unique(words))
  
  lang<- langs[i]
  if(lang=="English"){
    numWordsEnglish=numWordsEnglish+numUnique
    numTracksEn=numTracksEn+1
  } else{
    numWordsSpanish=numWordsSpanish+numUnique
    numTracksSp=numTracksSp+1
  }
}
avgUniqueWordCountEn=numWordsEnglish/numTracksEn
avgUniqueWordCountSp=numWordsSpanish/numTracksSp

io.df <- data.frame(Language=character(),
                 NumberOfTracks=integer(), 
                 AverageUniqueWordCount=numeric(), 
                 stringsAsFactors=FALSE) 
colnames(io.df)<-c("Language", "Number Of Tracks","Average Unique Word Count")
io.df[1,1]<-"English"
io.df[2,1]<-"Spanish"
io.df[1,2]<-numTracksEn
io.df[2,2]<-numTracksSp
io.df[1,3]<-avgUniqueWordCountEn
io.df[2,3]<-avgUniqueWordCountSp
#Randomization-Based Inference
library(dplyr)
set.seed(20180912)
m<-5000
test.stats <- replicate(m, {
  df.resample <- sample_n(songs, size=nrow(songs), replace=TRUE)
  grp<-group_by(df.resample,songs$Mode)
  info<-summarize(grp, quantile=quantile(Valence, probs=0.5), Var=var(Valence), N=n())
  as.numeric(summarize(info, Stat = diff(quantile)/(sum(N)/2)))
})
round(mean(abs(test.stats) != 0), 4)
#Machine Learning
set.seed(20180912)
inTrain <- createDataPartition(y = songs$Danceability,
                               p = 0.70,
                               list = FALSE,
                               groups = 4)

Train.df <- songs %>%
  slice(inTrain)

Test.df <- songs %>%
  slice(-inTrain)

CIrf.cv <- trainControl(method="cv",
                        number=5)
CIrf.tuning.grid <- expand.grid(mtry=seq(from=2, to=8, by=0.1))

library(party)
library(e1071)

fit.CIrf <- train(Danceability ~ Energy+Key+Loudness+
                    Mode+Speechiness+Acousticness+Instrumentalness
                    +Liveness+Valence+Tempo+Duration+Language,
                  data=Train.df,
                  method="cforest",
                  trControl=CIrf.cv,
                  tuneGrid=CIrf.tuning.grid)

yhat.CIrf <- predict(fit.CIrf, newdata=Test.df)
library(ModelMetrics)
rmse(Test.df$Danceability, yhat.CIrf)

uniqueArtist<-length(unique(songs$Artist))
table(songs$Time)
