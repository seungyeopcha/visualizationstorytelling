#패키지
library('ggplot2')
library(ggrepel)
library(gridExtra)
library(grid)
library(tidyr)
library(gtable)
setwd("C:/data")

#성적상하 여가시간
setwd("C:/data")
df<-read.csv('성적상하여가시간.csv')
df$년도<-as.factor(df$년도)

#묶은막대
#상
ggplot(df,aes(x=년도,y=상,fill=항목))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlGn')+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 평일 여가시간_성적 : 상")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=상,label= 상),vjust=1.2,position=position_dodge(width=0.9))

#하
ggplot(df,aes(x=년도,y=하,fill=항목))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlGn')+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 평일 여가시간_성적 : 하")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=하,label=하),vjust=1.2,position=position_dodge(width=0.9))
  

  
#누적막대

#상
a<-ggplot(df,aes(x=년도,y=상,fill=항목))+
  geom_bar(stat='identity')+
  scale_fill_brewer(palette = 'RdYlGn')+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 평일 여가시간_성적 : 상")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=상_y,label=상),vjust=1.2)



#하
b<-ggplot(df,aes(x=년도,y=하,fill=항목))+
  geom_bar(stat='identity')+
  scale_fill_brewer(palette = 'RdYlGn')+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 평일 여가시간_성적 : 하")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=하_y,label=하),vjust=1.2)

grid.arrange(a,b, nrow=1, ncol=2)

#누적막대 2
df1<-df %>% gather('상','하', key='성적',value='비율')
df1<-df1[,-c(3,4)]
write.csv(df1,"성적상하누적막대.csv") 
df1<-read.csv('성적상하누적막대.csv')
df1$성적<-as.factor(df1$성적)

ggplot(df1,aes(x=년도,y=비율,fill=항목))+
  geom_bar(stat='identity')+
  scale_fill_brewer(palette = 'RdYlGn')+
  guides(fill=guide_legend(title=NULL))+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 평일 성적별 여가시간")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  geom_text(aes(y=y,label=비율),vjust=1.2)+
  facet_grid(~성적)

################################################################################################
#평일여가시간
df<-read.csv('10대평일여가시간.csv')
df1<-read.csv('10대여가시간남녀.csv')
df2<-df1 %>% gather('남자','여자', key='성별',value='비율')

# 10대 전체
ggplot(df[c(19:24),],aes(x="",y=전체,fill=항목))+
  geom_bar(stat='identity',color="white",alpha = 0.7)+
  scale_fill_brewer(palette = 'BuPu')+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_minimal()+
  ggtitle("2020년 10대 평일 여가시간 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(label=paste0(round(전체,1), '%')),size=6,
            position=position_stack(vjust=0.5))


# 남녀 같이
ggplot(df2[df$년도==2020,],aes(x="",y=비율,fill=항목))+
  facet_wrap(~성별)+
  geom_bar(stat='identity',color="white",alpha = 0.7)+
  scale_fill_brewer(palette = 'Greys',)+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_bw()+
  ggtitle("2020년 10대 평일 여가시간 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(label=paste0(round(비율,1), '%')),size=5,
            position=position_stack(vjust=0.5))

#남녀 따로
##남자
ggplot(df1[c(19:24),],aes(x="",y=남자,fill=항목))+
  geom_bar(stat='identity',color="white",alpha = 0.7)+
  scale_fill_brewer(palette = 'Blues')+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_minimal()+
  ggtitle("2020년 10대 남자 평일 여가시간 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(label=paste0(round(남자,1), '%')),size=6,
            position=position_stack(vjust=0.5))

##여자
ggplot(df1[c(19:24),],aes(x="",y=여자,fill=항목))+
  geom_bar(stat='identity',color="white",alpha = 0.7)+
  scale_fill_brewer(palette = 'Reds')+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_minimal()+
  ggtitle("2020년 10대 여자 평일 여가시간 비율")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(label=paste0(round(여자,1), '%')),size=6,
            position=position_stack(vjust=0.5))

##########################################
#10대 앉아서 보낸시간
df<-read.csv('10대학습시간외앉아서보낸시간.csv')

ggplot(df[c(1:8),c(1:2)],aes(x=년도,y=시간,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("시간")+
  ggtitle("10대 학습시간 외 앉아서 보낸시간")+
  geom_line(color='Chartreuse',size=2,alpha=0.7)+
  geom_point(shape=21,color='Chartreuse',fill="white",size=3,stroke=3)+
  geom_text(aes(label=시간),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  scale_x_continuous(breaks=c(2013:2020))

#10대 여자 관심운동
df<-read.csv('주로참여하는체육활동종목_1순위_10대여자.csv')

col<-c('1. 체력단련 및 생활운동'='light coral',
       '2. 구기 및 라켓류'='lightgray',
       '5. 기타 스포츠'='lightgray',
       '3. 무도/격투기'='lightgray',
       '4. 레저 스포츠'='lightgray')

ggplot(df,aes(x="",y=비율,fill=운동))+
  geom_bar(stat='identity',color="white",alpha=0.7)+
  #scale_fill_manual(values = col)+
  scale_fill_brewer(palette = "Reds",direction = -1)+
  #scale_fill_gradient(high="Peach Puff", low="lightcoral")+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_minimal()+
  ggtitle("주로참여하는체육활동종목_1순위_10대여자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text_repel(aes(label=paste0(round(비율,1), '%')),size=6,
            position=position_stack(vjust=0.6,))
   
#10대 남자 관심운동
df1<-read.csv('주로참여하는체육활동종목_1순위_10대남자.csv')

ggplot(df1,aes(x="",y=비율,fill=운동))+
  geom_bar(stat='identity',color="white",alpha=0.7)+
  scale_fill_brewer(palette = 'Blues',direction = -1)+
  coord_polar(theta='y',start = 0)+
  xlab("")+
  ylab("")+
  theme_minimal()+
  ggtitle("주로참여하는체육활동종목_1순위_10대남자")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = 'bottom')+
  guides(fill=guide_legend(title=NULL))+
  geom_text_repel(aes(label=paste0(round(비율,1), '%')),size=6,
                  position=position_stack(vjust=0.5,))


#########################
#여자+전체 비만율
df<-read.csv('청소년비만_여학생.csv')
colnames(df)[2:3]<-c('여학생 비만율','전체 비만율')
df1<-df%>%gather('여학생 비만율','전체 비만율',key='성별',value="값")


colors<-c("여학생 비만율"="lightcoral","전체 비만율"='gray')

ggplot(df1,aes(x=년도,y=값,color=성별))+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 비만율_여학생")+
  geom_line(size=2)+
  geom_point(shape=19,size=2,stroke=3)+
  geom_text(aes(label=값),position=,vjust=-1.2,fontface='bold',color='black')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  guides(fill=guide_legend(title=NULL),)+
  scale_x_continuous(breaks=c(2006:2020))+
  scale_color_manual(values = colors)

###############################
#여자+전체 앉아있는시간
df<-read.csv('10대학습시간외앉아서보낸시간.csv')
df1<-df%>%gather('여학생','전체',key='성별',value="값")

colors<-c("여학생"="lightcoral","전체"='gray')

ggplot(df1,aes(x=년도,y=값,color=성별))+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 학습시간 외 앉아서 보낸시간 _여학생")+
  geom_line(size=2)+
  geom_point(shape=19,size=2,stroke=3)+
  geom_text(aes(label=값),position=,vjust=-1.2,fontface='bold',color='black')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  guides(fill=guide_legend(title=NULL),)+
  scale_x_continuous(breaks=c(2013:2020))+
  scale_color_manual(values = colors)


#여자+남자 앉아있는시간
df<-read.csv('10대학습시간외앉아서보낸시간.csv')
df2<-df[,-2]%>%gather('여학생','남학생',key='성별',value="값")

colors<-c("여학생"="lightcoral","남학생"='skyblue')

ggplot(df2,aes(x=년도,y=값,color=성별))+
  theme_bw()+
  xlab("년도")+
  ylab("비율")+
  ggtitle("10대 학습시간 외 앉아서 보낸시간")+
  geom_line(size=2)+
  geom_point(shape=19,size=2,stroke=3)+
  geom_text(aes(label=값),position=,vjust=-1.2,fontface='bold',color='black')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  guides(fill=guide_legend(title=NULL),)+
  scale_x_continuous(breaks=c(2013:2020))+
  scale_color_manual(values = colors)



#10대 앉아서 보낸시간_여학생
df<-read.csv('10대학습시간외앉아서보낸시간.csv')

ggplot(df[,c(1,3)],aes(x=년도,y=여학생,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("시간")+
  ggtitle("10대 여자 학습시간 외 앉아서 보낸시간")+
  geom_line(color='Chartreuse',size=2,alpha=0.7)+
  geom_point(shape=21,color='Chartreuse',fill="white",size=3,stroke=3)+
  geom_text(aes(label=여학생),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  scale_x_continuous(breaks=c(2013:2020))


####################################################3
#10대 왜안해 bar
#색
df<-read.csv('체육활동_비참여_이유_123_순위__10대.csv')

col<-c('체육활동가능시간부족'='Yellow',
       '체육활동에 대한 관심부족'='lightgray',
       '체육에 소질이 없음'='lightgray',
       '체육활동정보 부족'='lightgray',
       '체육시설 접근성 낮음'='lightgray',
       '체육활동지출비용부담'='lightgray',
       '동반 참여자 부재'='lightgray',
       '건강과 체력에 자신이 있어서'='lightgray',
       '소득수준 낮음'='lightgray',
       '체육 프로그램 부족'='lightgray',
       '건강상의 문제'='lightgray')
#10대
ggplot(df,aes(x=reorder(x, -y),y=y,fill=x))+
  geom_bar(stat='identity',alpha = 0.7)+
  #scale_fill_brewer(palette ='Set3')+
  scale_fill_manual(values =col)+
  theme_minimal()+
  xlab("")+
  ylab("비율")+
  ggtitle("10대 체육활동 비참여 이유")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13),
        axis.text.x=element_text(angle=30, hjust=0.7,face = "bold", size = 12),
        legend.position = 'none')+
  geom_text(aes(y=y,label=y),vjust=1.1, size = 5)

#########남녀
df1<-read.csv('체육활동비참여이유_10대여자.csv')
df2<-read.csv('체육활동비참여이유_10대남자.csv')

#여자
col<-c('체육활동가능시간부족'='lightgray',
       '체육활동에 대한 관심부족'='lightcoral',
       '체육에 소질이 없음'='lightcoral',
       '체육활동정보 부족'='lightcoral',
       '체육시설 접근성 낮음'='lightgray',
       '체육활동지출비용부담'='lightgray',
       '동반 참여자 부재'='lightgray',
       '건강과 체력에 자신이 있어서'='lightgray',
       '소득수준 낮음'='lightgray',
       '체육 프로그램 부족'='lightgray',
       '건강상의 문제'='lightgray')

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

a

#남자
col<-c('체육활동가능시간부족'='lightgray',
       '체육활동에 대한 관심부족'='lightgray',
       '체육에 소질이 없음'='lightgray',
       '체육활동정보 부족'='lightgray',
       '체육시설 접근성 낮음'='Deep Sky Blue',
       '체육활동지출비용부담'='Deep Sky Blue',
       '동반 참여자 부재'='lightgray',
       '건강과 체력에 자신이 있어서'='lightgray',
       '소득수준 낮음'='Deep Sky Blue',
       '체육 프로그램 부족'='lightgray',
       '건강상의 문제'='lightgray')

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

b

grid.arrange(b,a, nrow=2, ncol=1)        

#######################
#규칙적 체육활동 효과
df<-read.csv('규칙적체육활동효과_일생생활도움.csv')
df1<-read.csv('규칙적_체육활동의_효과_신체적_건강_유지__20211005223113.csv')
df2<-read.csv('규칙적_체육활동의_효과__의료비_절감__20211005223101.csv')
df3<-read.csv('규칙적_체육활동의_효과__정신적_건강_유지__20211005223130.csv')

#일상생활
df$통계분류<-factor(df$통계분류,levels = df$통계분류)

a<-ggplot(df,aes(x=통계분류,y=소계,fill=통계분류))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlBu')+
  xlab("")+
  ylab("비율")+
  ggtitle("규칙적체육활동효과_일생생활도움")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=소계,label= 소계),vjust=1.2,position=position_dodge(width=0.9))

#신체건강
df1$통계분류<-factor(df1$통계분류,levels = df1$통계분류)

b<-ggplot(df1,aes(x=통계분류,y=소계,fill=통계분류))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlBu')+
  xlab("")+
  ylab("비율")+
  ggtitle("규칙적체육활동효과_신체적건강유지")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=소계,label= 소계),vjust=1.2,position=position_dodge(width=0.9))

#의료비절감
df2$통계분류<-factor(df2$통계분류,levels = df2$통계분류)

c<-ggplot(df2,aes(x=통계분류,y=소계,fill=통계분류))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlBu')+
  xlab("")+
  ylab("비율")+
  ggtitle("규칙적체육활동효과_의료비절감")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=소계,label= 소계),vjust=1.2,position=position_dodge(width=0.9))

#정신건강
df3$통계분류<-factor(df3$통계분류,levels = df3$통계분류)

d<-ggplot(df3,aes(x=통계분류,y=소계,fill=통계분류))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'RdYlBu')+
  xlab("")+
  ylab("비율")+
  ggtitle("규칙적체육활동효과_정신건강")+
  theme_bw()+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))+
  geom_text(aes(y=소계,label= 소계),vjust=1.2,position=position_dodge(width=0.9))


grid.arrange(a,b,c,d ,nrow=2, ncol=2) 





