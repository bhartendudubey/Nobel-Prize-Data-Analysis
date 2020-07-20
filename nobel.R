library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(kableExtra)
library(knitr)
library(ggrepel)
library(scales)
library(gridExtra)
library(tidytext)
library(wordcloud)
library(lubridate)
library(igraph)
library(ggraph)
library(maps)

# Reading the Dataset
nobel<-read.csv("D:/groot/Summer/archive.csv") 

str(nobel)
head(nobel,3)

## Pre-defining functions
percent <-function(col,tab=nobel){
  tab %>% 
    filter_(!is.na(col))%>%
    group_by_(col)%>%
    summarise(tot=n())%>%
    mutate(percent=round(tot/sum(tot)*100))%>%
    arrange(desc(tot))
}

ccount <-function(col,tab=nobel){
  tab %>% 
    filter(!is.na(col))%>%
    group_by_(col)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
}

nobel$Age <- nobel$Year - as.integer(substr(nobel$Birth.Date, 1,4))


# Exploratory Data Analysis

kable(nobel %>% group_by(Sex)%>%
        count())


g1<-ccount(col="Sex")%>%filter(!Sex=="")%>%ggplot(aes(x=Sex,y=cnt,fill=Sex)) + geom_bar(stat='identity',alpha=0.5) + theme_fivethirtyeight()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +scale_fill_manual(values=palette(rainbow(3)))+labs(title="Prize won by Male/Female")+geom_text_repel(aes(label=cnt))

g1

g2<-nobel %>% group_by(Category,Sex)%>%filter(!Sex=="")%>% summarize(c=n())%>%mutate(percent=c/sum(c))%>%ggplot(aes(Category,percent,fill=Sex))+geom_bar(stat="identity",alpha=0.5,position="Dodge")+scale_y_continuous(labels = percent_format())+theme_fivethirtyeight()+scale_fill_manual(values=rainbow(5))+labs(title="Prize won by Category and Gender")

g2


ccount(col="Category")%>%filter(!Category=="")%>%ggplot(aes(x=reorder(Category,-cnt),y=cnt,fill=Category)) + geom_bar(stat='identity',alpha=0.5) + theme_fivethirtyeight()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +scale_fill_manual(values=rainbow(n=10))+labs(title="Prize by Category")+geom_text_repel(aes(label=cnt)) 

nobel %>% group_by(Year,Category)%>%summarise(ct=n())%>%ggplot(aes(x=ct))+geom_bar(fill=rainbow(n=1),alpha=0.5)+ theme_fivethirtyeight()+
  theme(axis.text.x=element_text(vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +labs(title="Laureates per Prize")

kable(nobel %>% group_by(Full.Name)%>%summarize(Prize_Count=n())%>%filter(Prize_Count>1)%>%arrange(desc(Prize_Count))%>%head(5),caption="Laureates won Prize more than once")

ggplot(nobel,aes(Year,Age,size=Age,col=cut(Age,5)))+geom_point(alpha=0.5)+geom_smooth(color="orange",se=FALSE)+theme_fivethirtyeight()+
  theme(axis.text.x=element_text(vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +labs(title="Age and Year at which Nobel Prize was Won")+
  geom_text_repel(aes(label=ifelse(Age<=25 |Age>87,as.character(Full.Name),"")),size=5)

nobel %>% ggplot(aes(Category,Age,fill=Category))+geom_boxplot(alpha=0.5)+geom_jitter()+theme_fivethirtyeight()+scale_fill_manual(values=rainbow(6))+
  theme(axis.text.x=element_text(vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +labs(title="Age Distribution by Category")


nobel %>%ggplot(aes(Year,Age,col=Category))+geom_point()+facet_wrap(~Category)+geom_smooth()+scale_color_manual(values=rainbow(n=6))

prop_female_winners <- nobel %>% mutate(female_winner=ifelse(Sex=="Female",TRUE,FALSE),male_winner=ifelse(Sex=="Male",TRUE,FALSE),
                                        decade=floor(Year/10)*10)%>%
  group_by(decade,Category)%>%summarize(fproportion=mean(female_winner,na.rm=TRUE),mproportion=mean(male_winner,na.rm=TRUE))

prop_female_winners %>%ggplot(aes(decade,fproportion,color=Category,group=Category))+geom_line()+geom_point()+scale_y_continuous(labels=scales::percent,limits=c(0.0,1.0),expand=c(0,0))+scale_x_continuous(breaks = seq(1900, 2020, 10))+theme_fivethirtyeight()

prop_female_winners %>%ggplot(aes(decade,mproportion,color=Category,group=Category))+geom_line(size=1)+geom_point()+scale_y_continuous(labels=scales::percent,limits=c(0.0,1.0),expand=c(0,0))+scale_x_continuous(breaks = seq(1900, 2020, 10))+theme_fivethirtyeight()

kable(nobel %>% select(Age,Full.Name,Year,Category)%>%arrange(Age)%>%head(5))

kable(nobel %>% select(Age,Full.Name,Year,Category)%>%arrange(desc(Age))%>%head(5))

kable(nobel %>% filter(Sex=="Female")%>%group_by(Category)%>%select(Year,Full.Name,Category,Birth.Country)%>%top_n(1,desc(Year)))

nobel$Birth.Date<-as.Date(nobel$Birth.Date)
nobel$Death.Date<-as.Date(nobel$Death.Date)
nobel$lifespan<-year(nobel$Death.Date)-year(nobel$Birth.Date)
ggplot(nobel,aes(x=lifespan))+geom_histogram(binwidth = 2,fill="red4",alpha=0.5)+ theme_fivethirtyeight()+
  theme(plot.title = element_text(size=12)) +labs(title="Lifespan of Nobel Laureates")

ggplot(nobel,aes(x=lifespan,fill=Category))+geom_histogram(binwidth = 2,alpha=0.5)+ theme_fivethirtyeight()+
  theme(plot.title = element_text(size=12)) +labs(title="Lifespan of Nobel Laureates by Category")


nobel %>% ggplot(aes(Category,lifespan,fill=Category))+geom_boxplot(alpha=0.5)+geom_jitter()+theme_fivethirtyeight()+scale_fill_manual(values=rainbow(6))+
  theme(axis.text.x=element_text(vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +labs(title="Lifespan Distribution by Category")


nobel %>%select(Category,Awarded_age=Age,lifespan)%>%gather(key="s",value="value",2:3)%>%ggplot(aes(Category,value,fill=s))+geom_boxplot()+geom_jitter(size=0.4)+scale_fill_manual(values=rainbow(n=2))+theme_fivethirtyeight()+
  theme(plot.title = element_text(size=12)) +labs(title="Comparing Lifespan and Awarded Age by Category",fill="")


nobel %>%filter(!Sex=="")%>% group_by(Year,Sex)%>%summarize(c=n())%>%arrange(Year)%>%ggplot(aes(x=Year,c,col=Sex,group=Sex))+geom_line(size=1,alpha=0.5)+scale_x_continuous(breaks = seq(1900, 2020, 5))+theme_fivethirtyeight()+theme(axis.text.x = element_text(angle=90))+labs(title="Prizes per Year")+scale_color_manual(values=rainbow(n=2))

top_10<-ccount(col="Birth.Country")%>%filter(!Birth.Country=="")%>%head(5)


nobel %>%filter(Birth.Country %in% top_10$Birth.Country)%>% group_by(Year,Birth.Country)%>%summarize(c=n())%>%arrange(Year)%>%ggplot(aes(x=Year,c,col=Birth.Country,group=1))+geom_line(size=1,alpha=0.5)+scale_x_continuous(breaks = seq(1900, 2020, 5))+theme_fivethirtyeight()+theme(axis.text.x = element_text(angle=90),legend.position = "none")+labs(title="Top Countries -Prizes per Year")+facet_wrap(~Birth.Country)+scale_y_continuous(breaks=seq(0,10,2))

nobel %>%select(Laureate.Type) %>% ccount(col="Laureate.Type")%>%ggplot(aes(x=Laureate.Type,y=cnt,fill=Laureate.Type)) + geom_bar(stat='identity',alpha=0.5) + theme_fivethirtyeight()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +scale_fill_manual(values=palette(rainbow(3)))+labs(title="Prize by Laureate Type")+geom_text_repel(aes(label=cnt))

ccount(col="Birth.Country")%>%filter(!Birth.Country=="" & cnt>4)%>%ggplot(aes(x=reorder(Birth.Country,cnt),y=cnt,fill=Birth.Country)) + geom_bar(stat='identity',alpha=0.5) + theme_fivethirtyeight()+
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +scale_fill_manual(values=rainbow(n=50))+labs(title="Prize by Country")+geom_text(aes(label=cnt))+coord_flip()

prop_usa_winners <- nobel %>% 
  mutate(usa_born_winner=ifelse(Birth.Country=="United States of America",TRUE,FALSE),
         decade= floor(Year/10)*10)%>% group_by(decade)%>%summarize(proportion=mean(usa_born_winner==TRUE,na.rm=TRUE))

prop_usa_winners %>%ggplot(aes(decade,proportion))+geom_line(color="red")+geom_point(color="orange")+scale_y_continuous(labels=scales::percent,limits=c(0.0,1.0),expand=c(0,0))+theme_fivethirtyeight()+labs(title="USA - Year wise analysis")

prop_india_winners <- nobel %>% 
  mutate(india_born_winner=ifelse(Birth.Country=="India",TRUE,FALSE), decade= floor(Year/10)*10)%>% group_by(decade)%>%summarize(proportion=mean(india_born_winner==TRUE,na.rm=TRUE))

prop_india_winners %>%ggplot(aes(decade,proportion))+geom_line(color="blue")+geom_point(color="orange")+scale_y_continuous(labels=scales::percent,limits=c(0.0,1.0),expand=c(0,0))+theme_fivethirtyeight()+labs(title="India - Year wise analysis")



nobel_peace <- nobel %>% 
  filter(( Category == 'Peace') & Sex != '')%>%
  filter(!is.na(Birth.Country)) %>%
  group_by(Birth.Country) %>%
  rename(region = Birth.Country) %>%
  summarise(value = n())

kable(nobel_peace %>%arrange(desc(value))%>% head(10),caption =" Top Countries  who got nobel prize for Peace")

worldMap <- map_data('world')
allCountries <- data.frame(region=unique(worldMap$region), stringsAsFactors = F)

peace_country<- right_join(nobel_peace, allCountries)

peace_country$value[is.na(peace_country$value)] <- 0

#laureateBorn <- laureateBorn %>% arrange(region)

ggplot(peace_country, aes(map_id = region)) + 
  geom_map(aes(fill = log1p(value)), map = worldMap, color='black', size=0.15) + 
  expand_limits(x = worldMap$long, y = worldMap$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient2(low="blue",mid = "purple",
                       high = "red" ,
                       name="Number of Laureates",
                       #breaks=Breaks,
                       labels=c('1', '2', '5', '10', '15', '25')) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .75)) + 
  ggtitle("Peace Laureates per Country of Birth")


nobel_all<- nobel %>% 
  filter(Sex != '' & Category=="Medicine")%>%
  filter(!is.na(Birth.Country)) %>%
  group_by(Birth.Country) %>%
  rename(region = Birth.Country) %>%
  summarise(value = n())
nobel_all%>%arrange(desc(value))

worldMap <- map_data('world')
allCountries <- data.frame(region=unique(worldMap$region), stringsAsFactors = F)

country<- right_join(nobel_all, allCountries)

country$value[is.na(peace_country$value)] <- 0

#laureateBorn <- laureateBorn %>% arrange(region)

ggplot(country, aes(map_id = region)) + 
  geom_map(aes(fill = log1p(value)), map = worldMap, color='black', size=0.15) + 
  expand_limits(x = worldMap$long, y = worldMap$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="blue",
                      high = "red" ,
                      name="Number of Laureates",
                      #breaks=Breaks,
                      labels=c('1','10' , '20','50','70','80')
  ) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .75)  )  + 
  ggtitle("Medicine - Nobel Laureates per Country of Birth")



nobel_all<- nobel %>% 
  filter(Sex != '' & Category=="Physics")%>%
  filter(!is.na(Birth.Country)) %>%
  group_by(Birth.Country) %>%
  rename(region = Birth.Country) %>%
  summarise(value = n())
nobel_all%>%arrange(desc(value))

worldMap <- map_data('world')
allCountries <- data.frame(region=unique(worldMap$region), stringsAsFactors = F)

country<- right_join(nobel_all, allCountries)

country$value[is.na(peace_country$value)] <- 0

#laureateBorn <- laureateBorn %>% arrange(region)

ggplot(country, aes(map_id = region)) + 
  geom_map(aes(fill = log1p(value)), map = worldMap, color='black', size=0.15) + 
  expand_limits(x = worldMap$long, y = worldMap$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="yellow",
                      high = "green" ,
                      name="Number of Laureates",
                      #breaks=Breaks,
                      labels=c('1','10' , '20','50','70','80')
  ) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .75)  )  + 
  ggtitle("Physics - Nobel Laureates per Country of Birth")

nobel$Motivation<-as.character(nobel$Motivation)
motive<-nobel %>%unnest_tokens(word,Motivation)%>%anti_join(stop_words,by="word")
motive%>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = rainbow(n=10)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words by Nobel Laureates") +
  coord_flip()


word_counts <- motive %>%
  count(word, sort = TRUE) 

wordcloud(words = word_counts$word, freq = word_counts$n, min.freq = 1,
          max.words=300,  
          colors=rainbow(100))
