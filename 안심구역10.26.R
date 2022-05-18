setwd("C:/data")
df<-read.csv("연령층별비만율.csv")
df$시간<-c(1:9)


x<-c(1:6)



lm<-lm(df$청넌층~df$시간)
x[1]<-coef(lm)[2]

lm<-lm(df$중년층~df$시간)
x[2]<-coef(lm)[2]

lm<-lm(df$장년층~df$시간)
x[3]<-coef(lm)[2]

lm<-lm(df$노년층~df$시간)
x[4]<-coef(lm)[2]

lm<-lm(df$고령층~df$시간)
x[5]<-coef(lm)[2]

lm<-lm(df$계~df$시간)
x[6]<-coef(lm)[2]


x
colnames(df)[2:7]

dflm<-data.frame(colnames(df)[2:7],x)

#write.csv(dflm,"비만율기울기.csv")

##############################비만_암 상관관계
df99<-read.csv("비만_암조발생률.csv")

df2<-merge(df99,df[-9,c(7,8)],key=시간)

#####기울기가 비슷한가?
x<-c(1:8)

lm<-lm(df2$식도.C15.~df2$시간)
x[1]<-coef(lm)[2]

lm<-lm(df2$위.C16.~df2$시간)
x[2]<-coef(lm)[2]

lm<-lm(df2$대장.C18.C20.~df2$시간)
x[3]<-coef(lm)[2]

lm<-lm(df2$담낭.및.기타.담도.C23.C24.~df2$시간)
x[4]<-coef(lm)[2]

lm<-lm(df2$유방.C50.~df2$시간)
x[5]<-coef(lm)[2]

lm<-lm(df2$갑상선.C73.~df2$시간)
x[6]<-coef(lm)[2]

lm<-lm(df2$다발성.골수종.C90.~df2$시간)
x[7]<-coef(lm)[2]

lm<-lm(df2$계~df2$시간)
x[8]<-coef(lm)[2]

dflm2<-data.frame(colnames(df2)[3:10],x)
#write.csv(dflm2,"비만_암기울기.csv")
#######상관관계
x<-c(1:7)

x[1]<-cor(df2$식도.C15.,df2$계)
x[2]<-cor(df2$위.C16.,df2$계)
x[3]<-cor(df2$대장.C18.C20.,df2$계)
x[4]<-cor(df2$담낭.및.기타.담도.C23.C24.,df2$계)
x[5]<-cor(df2$유방.C50.,df2$계)
x[6]<-cor(df2$갑상선.C73.,df2$계)
x[7]<-cor(df2$다발성.골수종.C90.,df2$계)

dfcor2<-data.frame(colnames(df2)[3:9],x)
#write.csv(dfcor2,"비만_암상관계수.csv")
#############그래프
par(mfrow=c(2,2))
plot(df2$식도.C15~df2$계);res=lm(df2$식도.C15~df2$계);abline(res);title(main='식도')              
plot(df2$담낭.및.기타.담도.C23.C24.~df2$계);res=lm(df2$담낭.및.기타.담도.C23.C24.~df2$계);abline(res);title(main='담낭 및 기타 담도') 
plot(df2$유방.C50.~df2$계);res=lm(df2$유방.C50.~df2$계);abline(res);title(main='유방') 
plot(df2$다발성.골수종.C90.~df2$계);res=lm(df2$다발성.골수종.C90.~df2$계);abline(res);title(main='다발성골수종') 

###################################################################
df2<-df2[,c(1,2,3,6,7,9,10)]
df2<-gather(data=df2,key='암',value='조발생률',"식도.C15.","담낭.및.기타.담도.C23.C24.","유방.C50.","다발성.골수종.C90.")

ggplot(df2,aes(x=계,y=조발생률))+
  xlab('비만율')+
  ylab("암 조발생률")+
  ggtitle("비만율 암 상관관계 그래프")+
  geom_point(shape=19, size=3)+
  geom_smooth(method = 'lm')+
  facet_wrap( ~ 암, ncol=2)

###############################################




############ 비만과 질병 사망률
df88<-read.csv("사망원인_236항목__성_연령_5세_별_사망자수__사망률_20211027145603.csv")
df3<-merge(df88,df[,c(7,8)],key=시간)

#상관관계
x<-c(1:15)

for(i in c(3:17)){
  x[(i-2)]<-cor(df3[i],df3$계)
}

x
dfcor3<-data.frame(colnames(df3)[3:17],x)
write.csv(dfcor3,"비만_질병사망률상관관계.csv")

#그래프
par(mfrow=c(3,5))

for(i in c(3:17)){
  y<-plot(df3[,i]~df3$계);res=lm(df3[,i]~df3$계);abline(res);title(main=colnames(df3)[i])
}


##################비만과 질병유병률
df77<-read.csv("비만_유병률_고혈압당뇨병고콜레스테롤혈증.csv")
df4<-merge(df77,df[,c(7,8)],key=시간)


#상관관계
x<-c(1:4)

for(i in c(3,4,6)){
  x[(i-2)]<-cor(df4[i],df4$계)
}
x[3]<-cor(df4[-1,5],df4[-1,7])
x

dfcor4<-data.frame(colnames(df4)[3:6],x)
#write.csv(dfcor4,"비만_질병유병률상관관계.csv")


#그래프
par(mfrow=c(2,2))

plot(df4[,3]~df4$계);res=lm(df4[,3]~df4$계);abline(res);title(main=colnames(df4)[3])
plot(df4[,4]~df4$계);res=lm(df4[,4]~df4$계);abline(res);title(main=colnames(df4)[4])
plot(df4[,5]~df4$계);res=lm(df4[,5]~df4$계);abline(res);title(main=colnames(df4)[5])
plot(df4[,6]~df4$계);res=lm(df4[,6]~df4$계);abline(res);title(main=colnames(df4)[6])
