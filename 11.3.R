setwd("C:/data")
df<-read.csv('청소년비만.csv')
#패키지

library('lubridate')
library('ggplot2')

#
df$년도<-as.factor(df$년도)

#비만율
df2<-df[-1,]
ggplot(df2,aes(x=년도,y=비만율,group=1),)+
  theme_bw()+
  xlab("년도")+
  ylab("비만율")+
  ggtitle("청소년 비만율 추이")+
  geom_line(color='red',size=2)+
  geom_point(shape=21,color='red',fill="white",size=3,stroke=3)+
  geom_text(aes(label=비만율),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))

#과체중률
ggplot(df2,aes(x=년도,y=과체중률,group=1))+
  theme_bw()+
  xlab("년도")+
  ylab("과체중률")+
  ggtitle("청소년 과체중률 추이")+
  geom_line(color="lightcoral",size=2)+
  geom_point(shape=21,color="lightcoral",fill="white",size=3,stroke=3)+
  geom_text(aes(label=과체중률),position=,vjust=-1.2,fontface='bold')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20),
        axis.title = element_text(face = "bold", size = 13))


##규칙적 운동 참여율
