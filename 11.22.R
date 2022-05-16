#패키지
library('ggplot2')
library(ggrepel)
library(gridExtra)
library(grid)
library(tidyr)
library(gtable)
setwd("C:/data")
library(dplyr)
#10대 앉아서 보낸시간
df<-read.csv('10대학습시간외앉아서보낸시간.csv')

##전체
df1<-df%>%gather('학습시간','학습시간외',key='type',value="time")

col=c("학습시간외"='Chartreuse',"학습시간"='lightgray')

ggplot(df1,aes(x=년도,y=time,group=type))+
  theme_bw()+
  xlab("년도")+
  ylab("시간")+
  ggtitle("10대 앉아서 보낸시간")+
  
  geom_line(aes(color=type),size=2,alpha=0.7)+
  geom_point(aes(color=type),shape=21,fill="white",size=3,stroke=3)+
  scale_color_manual(values =c("학습시간외"='Chartreuse',"학습시간"='lightgray'))+
  
  geom_text(aes(label=time),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  scale_x_continuous(breaks=c(2013:2020))

#학습목적 앉아있는 시간만

ggplot(df[,c(1,3)],aes(x=년도,y=학습시간,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("시간")+
  ggtitle("10대 학습시간 앉아서 보낸시간")+
  geom_line(color='gray',size=2,alpha=0.7)+
  geom_point(shape=21,color='gray',fill="white",size=3,stroke=3)+
  geom_text(aes(label=학습시간),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  scale_x_continuous(breaks=c(2013:2020))

#학습시간외
ggplot(df[,c(1,2)],aes(x=년도,y=학습시간외,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("시간")+
  ggtitle("청소년 학습시간 외 앉아서 보낸시간")+
  geom_line(color='Chartreuse',size=2,alpha=0.7)+
  geom_point(shape=21,color='Chartreuse',fill="white",size=3,stroke=3)+
  geom_text(aes(label=학습시간외),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  scale_x_continuous(breaks=c(2013:2020))
################################################################################
#누적막대그래프
df<-read.csv('규칙적_체육활동_참여_여부_및_빈도_10대남녀.csv')


통계_col<-c("a. 전혀하지않는다"="lightgray",
          "b. 월3회 이하"='lightgray',
          "c. 주1회"='lightgray',
          "d. 주2회"='lightgray',
          "e. 주3회"='lightgray',
          'f. 주4회'='lightgray',
          'g. 주5회'='lightgray',
          'h. 주6회'='lightgray',
          'i. 매일'='Chartreuse4')

ggplot(df,aes(년도,X10대,fill=통계))+
  geom_bar(stat='identity',color='white')+scale_fill_brewer(palette = 'RdYlGn')+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("청소년 규칙적 체육활동 참여 여부")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=y,label=X10대),vjust=1.2)+
  scale_x_continuous(breaks=c(2012:2020))




df$selected=ifelse(df$통계 %in% 'i. 매일',paste(df$X10대,'%'),NA)
df$yy<-df$y+5


ggplot(df,aes(년도,X10대,fill=통계))+
  geom_bar(stat='identity',color='white')+scale_fill_manual(values = 통계_col)+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("청소년 규칙적 체육활동 참여 여부")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=yy,label=selected),size=10, colour='Chartreuse4',fontface="bold")+
  scale_x_continuous(breaks=c(2012:2020))
####################################33
df<-read.csv("운동량공공체육시설수회귀.csv")

df

m<-lm(운동비율~운동시설수,data=df)

plot(df$운동비율~df$운동시설수)
abline(m)

summary(m)

cor(df$운동비율,df$운동시설수)


####################################33
df<-read.csv("인구_십만명당_체육시설수_시도__20211105131736.csv")

m<-lm(df$청소년주60분주5회이상~df$인구십만명당체육시설수)

plot(df$인구십만명당체육시설수,df$청소년주60분주5회이상);abline(m)
summary(m)

cor(df$운동비율,df$운동시설수)

df1<-df[df$년도==2016|df$년도==2018,]

m<-lm(df1$규칙적운동실천~df1$인구십만명당체육시설수)
plot(df1$규칙적운동실천,df1$인구십만명당체육시설수);abline(m)
summary(m)
#########################################################################33
df<-read.csv("규칙적체육활동_19.csv")
df1<-df[df$년도!=2008|df$년도!=2010,]

m<-lm(df1$규칙적운동_19~df1$십만명당체육시설수)
summary(m)
plot(df$십만명당체육시설수,df$규칙적운동_19);abline(m)

m<-lm(df$규칙적운동~df$십만명당체육시설수)
summary(m)
plot(df$십만명당체육시설수,df$규칙적운동);abline(m)
#############################################################
df<-read.csv('KS_OPN_SCHUL_ALSFC_INFO_202110.csv')

df2017<-data.frame(table(df[df$BASE_YEAR==2017,2]))
df2018<-data.frame(table(df[df$BASE_YEAR==2018,2]))
df2019<-data.frame(table(df[df$BASE_YEAR==2019,2]))
df2020<-data.frame(table(df[df$BASE_YEAR==2020,2]))

df2017$년도<-2017
df2018$년도<-2018
df2019$년도<-2019
df2020$년도<-2020

df1<-rbind(df2017,df2018,df2019,df2020)

write.csv(df1,"시도_개방학교.csv")

##########################################################################33

df<-read.csv('인구_십만명당_체육시설수_시도__2005.csv')

m<-lm(df$주3일.이상.고강도.신체활동.실천율~df$인구십만명당.체육시설수)
options(scipen=999)
summary(m)
plot(x=df$인구십만명당.체육시설수,y=df$주3일.이상.고강도.신체활동.실천율,main='청소년 신체활동 실천율과 체육시설수',xlab='주3일 이상 고강도 신체활동 실천율',ylab='인구십만명당 체육시설수');abline(m,col='red',)

ggplot(df,aes(x=인구십만명당.체육시설수,y=주3일.이상.고강도.신체활동.실천율))+
  geom_point()+
  geom_abline(intercept= 26.83936, slope=0.06620, color='red', size = 1.2)+
  ggtitle("청소년 신체활동과 체육시설수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text( size = 13))
  
  
  
########################################################3
df<-read.csv('문화체육관광부_공공체육시설.csv')

#과체중률
df$년도<-as.factor(df$년도)

ggplot(df,aes(x=년도,y=공공체육시설수,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("")+
  ggtitle("전국 공공체육시설 수 현황")+
  geom_line(color="orange2",size=2)+
  geom_point(shape=23,color="white",fill="orange2",size=5,stroke=3)+
  geom_text(aes(label=공공체육시설수),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))
#################################################################
df<-read.csv('주로_참여하는_체육활동_이용시설_123순위__20211122193250.csv')

#색
col<-c("공공 체육시설"='Antique White 1',
       '민간 체육시설'='dark gray',
       '기타 체육시설'='dark gray',
       '학교 체육시설'='Goldenrod 1',
       '자가시설'='dark gray')
#1
a<-ggplot(df,aes(x=reorder(통계분류, -X10대),y=X10대,fill=통계분류))+
  geom_bar(stat='identity',width = 0.7)+
  scale_fill_manual(values =col)+
  #scale_fill_gradient2(low="Peach Puff", high="red")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("주로 참여하는 체육활동 이용시설_ 10대")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position ='none')+
  geom_text(aes(y=X10대,label=paste(X10대,"%")),vjust=1.2, size = 6)



#색
col<-c("공공 체육시설"='Slate Gray 1',
       '민간 체육시설'='dark gray',
       '기타 체육시설'='dark gray',
       '학교 체육시설'='Royal Blue 1',
       '자가시설'='dark gray')

b<-ggplot(df,aes(x=reorder(통계분류, -남성),y=남성,fill=통계분류))+
  geom_bar(stat='identity',width = 0.7)+
  scale_fill_manual(values =col)+
  #scale_fill_gradient2(low="Peach Puff", high="red")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("주로 참여하는 체육활동 이용시설_ 10대_남성")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position ='none')+
  geom_text(aes(y=남성,label=paste(남성,"%")),vjust=1.2, size = 6)

grid.arrange(a,b ,nrow=2, ncol=1) 
############################################################################333
df<-read.csv('자치구단위 서울생활인구(10대여자).csv')

df$여자10대생활인구<-rowSums(df[,c(4,5)])

df1<-df%>%
  group_by(자치구코드)%>%
  summarise(생활인구평균=mean(여자10대생활인구))

write.csv(df1,'여자생활인구(17시이후).csv')
################################################
df<-read.csv('산책로 좌표.csv')
df1<-data.frame(table(df$SIGNGU_NM))

write.csv(df1,'시군구별 산책로수수.csv')
