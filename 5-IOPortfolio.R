# Load Necessary Packages
pkgs <- c("dplyr", "ggplot2", "httr", "jsonlite", "readr", "rvest", 
          "stringr", "tidyr")
for(pkg in pkgs) library(pkg, character.only=TRUE)
library("stringr")
library("XML")
setwd("C:/Users/bharila/Documents/StatisticalProgrammingR/Portfolios")
  
#read faculty directory json
jsonfile<-fromJSON(file("facultyDirectory.json"))

users<-jsonfile$users[[1]]
title<-users$title
filename<-users$filename

#put filenames in 3 corresponsing vectors
#for differnt professor types
#get alumnus information
associate<-vector()
assistant<-vector()
prof<-vector()
#alumni<-vector()

for(i in 1:length(title)){
  thistitle<-title[i]
  if(is.na(thistitle)){
    #do nothing
  }
  else if(str_detect(thistitle,"[[:alnum:]]*Assistant Professor[[:alnum:]]*")){
    associate<-c(filename[i], associate)
  }
  else if(str_detect(thistitle,"[[:alnum:]]*Associate Professor[[:alnum:]]*")){
    assistant<-c(filename[i], assistant)
  }
  else if(str_detect(thistitle,"[[:alnum:]]*Professor[[:alnum:]]*")){
    prof<-c(filename[i], prof)
  }
  else{
    #not a teacher or alum
  }
}

#gets paragraphs for each teacher
#puts them in 3 different vectors
#corresponding to the type of teacher
setwd("C:/Users/bharila/Documents/StatisticalProgrammingR/Portfolios/Faculty Portfolios")

associate.para<-vector()
assistant.para<-vector()
prof.para<-vector()
alumni<-vector()

for(i in 1:length(associate)){
  url <- paste(associate[i],".html", sep="")
  if(file.exists(url)){
    prof.pg <- read_html(url)
    para.df <- html_nodes(prof.pg, "div.region.content-container-region")
    alumniInfo <- html_nodes(prof.pg, "div.accordion-item")
    checkvector<-xml_text(alumniInfo, trim=TRUE)
    if(length(checkvector)!=0){
      for(j in 1:length(checkvector)){
        if(!is.null(checkvector[j])&!is.na(checkvector[j])){
          if(str_detect(checkvector[j],"[[:alnum:]]*Academic Degrees[[:alnum:]]*")){
            if(str_detect(checkvector[j],"[[:alnum:]]*Rose-Hulman Institute of Technology[[:alnum:]]*")){
              alumni<-c(associate[i],alumni)
            }
          }
        }
      }
    }
    ans<-xml_text(para.df[2], trim=TRUE)
    associate.para <- c(associate.para,ans)
  }
}

for(i in 1:length(assistant)){
  url <- paste(assistant[i],".html", sep="")
  if(file.exists(url)){
    prof.pg <- read_html(url)
    para.df <- html_nodes(prof.pg, "div.region.content-container-region")
    alumniInfo <- html_nodes(prof.pg, "div.accordion-item")
    checkvector<-xml_text(alumniInfo, trim=TRUE)
    if(length(checkvector)!=0){
      for(j in 1:length(checkvector)){
        if(str_detect(checkvector[j],"[[:alnum:]]*Academic Degrees[[:alnum:]]*")){
          if(str_detect(checkvector[j],"[[:alnum:]]*Rose-Hulman Institute of Technology[[:alnum:]]*")){
            alumni<-c(assistant[i],alumni)
          }
        }
      }
    }
    ans<-xml_text(para.df[2], trim=TRUE)
    assistant.para <- c(assistant.para,ans)
  }
}

for(i in 1:length(prof)){
  url <- paste(prof[i],".html", sep="")
  if(file.exists(url)){
    prof.pg <- read_html(url)
    para.df <- html_nodes(prof.pg, "div.region.content-container-region")
    alumniInfo <- html_nodes(prof.pg, "div.accordion-item")
    checkvector<-xml_text(alumniInfo, trim=TRUE)
    if(length(checkvector)!=0){
      for(j in 1:length(checkvector)){
        if(!is.null(checkvector[j])&!is.na(checkvector[j])){
          if(str_detect(checkvector[j],"[[:alnum:]]*Academic Degrees[[:alnum:]]*")){
            if(str_detect(checkvector[j],"[[:alnum:]]*Rose-Hulman Institute of Technology[[:alnum:]]*")){
              alumni<-c(prof[i],alumni)
            }
          }
        }
      }
    }
    ans<-xml_text(para.df[2], trim=TRUE)
    prof.para <- c(prof.para,ans)
  }
}

#put all alumni contact information in one vector
alumniInfo<-vector()
for(i in 1:length(alumni)){
  thisfilename<-alumni[i]
  for(j in 1:length(filename)){
    if(!is.null(thisfilename)){
      if(!is.na(thisfilename)){
        if(!is.na(filename[j])){
          if(filename[j]==thisfilename){
            alumnus<-users[j,]
            info<-paste(alumnus$name,
                        alumnus$department,
                        alumnus$email,
                        alumnus$phone, "\n", sep="\n")
            alumniInfo<-c(alumniInfo,info)
          }
        }
      }
    }
  }
}

#get paragraph lengths
assistant.length <- sapply(strsplit(assistant.para, " "), length)
associate.length <- sapply(strsplit(associate.para, " "), length)
prof.length <- sapply(strsplit(prof.para, " "), length)

#tidy data dataframe
max.len = max(length(associate.length), length(assistant.length), length(prof.length))
associate.length = c(associate.length, rep(NA, max.len - length(associate.length)))
assistant.length = c(assistant.length, rep(NA, max.len - length(assistant.length)))
prof.length = c(prof.length, rep(NA, max.len - length(prof.length)))

df2 <- data.frame(associate.length,assistant.length,prof.length)

proflen.df <- gather(df2,
                      key=Type, value=Length,
                          c(associate.length,assistant.length,prof.length))
proflen.df$Type<-as.factor(proflen.df$Type)
library(plyr)
proflen.df$Type<-mapvalues(proflen.df$Type, from = c("associate.length", "assistant.length", "prof.length"), 
          to = c("Associate Professor", "Assistant Professor", "Professor"))
proflen.df<-na.omit(proflen.df)

#plot graphic
ggplot(data=proflen.df, mapping=aes(x=proflen.df$Length, fill=proflen.df$Type)) +
  geom_density(alpha=0.5) +
  labs(x="Length of description", y="Density", fill="") +
  theme_bw()

#Print alumni info
cat(alumniInfo)