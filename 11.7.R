setwd("C:/data")
df<-read.csv('규칙적_체육활동_참여_여부_및_빈도_10대남녀.csv')
df$년도<-as.factor(df$년도)

#패키지
library('ggplot2')
install.packages('ggrepel')
library(ggrepel)
library(gridExtra)
library(grid)


#########얼마나안해??

#묶은 막대 그래프
##색
통계_col<-c("a. 전혀하지않는다"="Red1",
          "b. 월3회 이하"='Rosy Brown3',
          "c. 주1회"='Misty Rose3',
          "d. 주2회"='Sea shell3',
          "e. 주3회"='Gold1',
          'f. 주4회'='Light goldenrod1',
          'g. 주5회'='Lemon chiffon1',
          'h. 주6회'='Light yellow1',
          'i. 매일'='green')
#1
ggplot(df,aes(년도,X10대,fill=통계))+
  geom_bar(stat='identity',position='dodge',color='grey')+scale_fill_manual(values = 통계_col)+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 규칙적 체육활동 참여 여부")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))

#2
ggplot(df,aes(년도,X10대,fill=통계))+
  geom_bar(stat='identity',position='dodge',color='white')+scale_fill_brewer(palette = 'RdYlGn')+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 규칙적 체육활동 참여 여부")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))


#누적막대그래프
ggplot(df,aes(년도,X10대,fill=통계))+
  geom_bar(stat='identity',color='white')+scale_fill_brewer(palette = 'RdYlGn')+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 규칙적 체육활동 참여 여부")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=y,label=X10대),vjust=1.2)+
  scale_x_continuous(breaks=c(2012:2020))


#######################왜안해?
df1<-read.csv('체육활동비참여이유_10대여자.csv')
df2<-read.csv('체육활동비참여이유_10대남자.csv')


##10대 여자 bar
#1
ggplot(df1,aes(x=reorder(x, -y),y=y,fill=y))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low="Snow2", high="Orange Red1")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 비참여 이유 : 10대 여자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=1.1, size = 5)


##10대 남자 bar
ggplot(df2,aes(x=reorder(x, -y),y=y,fill=y))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low="Azure 2", high="Dodger Blue 1")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 비참여 이유 : 10대 남자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
        geom_text(aes(y=y,label=y),vjust=1.1, size = 5)


###############종합
#색
col<-c('체육활동가능시간부족'='lightgray',
       '체육활동에 대한 관심부족'='Turquoise 1',
       '체육에 소질이 없음'='lightgray',
       '체육활동정보 부족'='lightgray',
       '체육시설 접근성 낮음'='lightgray',
       '체육활동지출비용부담'='lightgray',
       '동반 참여자 부재'='lightgray',
       '건강과 체력에 자신이 있어서'='lightgray',
       '소득수준 낮음'='lightgray',
       '체육 프로그램 부족'='lightgray',
       '건강상의 문제'='lightgray')
#여자
a<-ggplot(df1,aes(x=reorder(x, -y),y=y,fill=x))+
  geom_bar(stat='identity',alpha = 0.7)+
  scale_fill_manual(values =col)+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 비참여 이유 : 10대 여자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=1.1, size = 5)

b<-ggplot(df2,aes(x=reorder(x, -y),y=y,fill=x))+
  geom_bar(stat='identity',alpha = 0.7)+
  scale_fill_manual(values =col)+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 비참여 이유 : 10대 남자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=1.1, size = 5)

grid.arrange(b,a, nrow=2, ncol=1)        
        
#plot.background = element_rect(fill = "")


##################################선행조건
df1<-read.csv('체육활동선행조건10대여자.csv')
df2<-read.csv('체육활동선행조건10대남자.csv')


##10대 여자 bar
#색
col<-c('체육활동 가능시간 증가'='gray',
       '체육시설 접근성 확대'='gray',
       '체육 활동에 대한 관심 증가'='lightgreen',
       '소득수준 증가'='lightgray',
       '건강상태 개선'='lightgray',
       '체육활동 지출비용 여유'='lightgray',
       '체육 지도자 수준 향상'='lightgray',
       '동반 참여자 확보'='lightgray',
       '체육활동 프로그램 확대'='lightgray',
       '체육활동 정보 접근성 확대'='lightgray')
#1

a<-ggplot(df1,aes(x=reorder(x, -y),y=y,fill=x))+
  geom_bar(stat='identity',alpha = 0.7)+
  scale_fill_manual(values =col)+
  #scale_fill_gradient2(low="Peach Puff", high="red")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 참여증진 선행조건 : 10대 여자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position ='none')+
  geom_text(aes(y=y,label=y),vjust=1.2, size = 5)


#2
ggplot(df1,aes(x=reorder(x, -y),y=y,fill=y))+
  geom_bar(stat='identity')+
  #scale_fill_manual(values =col)+
  scale_fill_gradient2(low="Peach Puff", high="red")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 참여증진 선행조건 : 10대 여자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position ='none')+
  geom_text(aes(y=y,label=y),vjust=-0.3, size = 5)

##10대 남자 bar
#1
b<-ggplot(df2,aes(x=reorder(x, -y),y=y,fill=x))+
  geom_bar(stat='identity',alpha = 0.7)+
  scale_fill_manual(values =col)+
  #scale_fill_gradient(low="Azure 2", high="Dark Blue")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 참여증진 선행조건 : 10대 남자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=1.2, size = 5)

b
#2
ggplot(df2,aes(x=reorder(x, -y),y=y,fill=y))+
  geom_bar(stat='identity')+
  #scale_fill_manual(values =col)+
  scale_fill_gradient(low="Azure 2", high="Dark Blue")+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("체육활동 참여증진 선행조건 : 10대 남자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=-0.3, size = 5)

grid.arrange(b,a ,nrow=2, ncol=1) 
