#데이터 불러오기
setwd("C:\\Users\\82109\\Desktop\\교육통계 실습과제 2")
getwd()

data<-read.csv("test05.csv")
data

#결측치 만들기
data$PEDU<-ifelse(data$PEDU<0, NA, data$PEDU)
data$MEDU<-ifelse(data$MEDU<0, NA, data$MEDU)
data$INCOME<-ifelse(data$INCOME<0, NA, data$INCOME)
data$REGION<-ifelse(data$REGION<0, NA, data$REGION)
data$SECTOR<-ifelse(data$SECTOR<0, NA, data$SECTOR)
data$Y1H7_2_1<-ifelse(data$Y1H7_2_1<0, NA, data$Y1H7_2_1)
data$Y1S11_5<-ifelse(data$Y1S11_5<0, NA, data$Y1S11_5)
data$Y1S11_6<-ifelse(data$Y1S11_6<0, NA, data$Y1S11_6)
data$Y1S15<-ifelse(data$Y1S15<0, NA, data$Y1S15)
data$Y1S19_2<-ifelse(data$Y1S19_2<0, NA, data$Y1S19_2)
data$Y1S19_3<-ifelse(data$Y1S19_3<0, NA, data$Y1S19_3)
data$Y1S19_5<-ifelse(data$Y1S19_5<0, NA, data$Y1S19_5)
data$Y1S19_7<-ifelse(data$Y1S19_7<0, NA, data$Y1S19_7)
data$Y1KOR_S<-ifelse(data$Y1KOR_S<0, NA, data$Y1KOR_S)
data$Y1MAT_S<-ifelse(data$Y1MAT_S<0, NA, data$Y1MAT_S)
data$Y2S5_15<-ifelse(data$Y2S5_15<0, NA, data$Y2S5_15)
data$Y2S5_16<-ifelse(data$Y2S5_16<0, NA, data$Y2S5_16)
data$Y2S5_17<-ifelse(data$Y2S5_17<0, NA, data$Y2S5_17)
data$Y2S5_18<-ifelse(data$Y2S5_18<0, NA, data$Y2S5_18)
data$Y2S5_19<-ifelse(data$Y2S5_19<0, NA, data$Y2S5_19)
data$Y2S5_20<-ifelse(data$Y2S5_20<0, NA, data$Y2S5_20)
data$Y2S5_21<-ifelse(data$Y2S5_21<0, NA, data$Y2S5_21)
data$Y2S5_22<-ifelse(data$Y2S5_22<0, NA, data$Y2S5_22)
data$Y2S5_23<-ifelse(data$Y2S5_23<0, NA, data$Y2S5_23)
data$Y2S27_1<-ifelse(data$Y2S27_1<0, NA, data$Y2S27_1)
data$Y2S27_2<-ifelse(data$Y2S27_2<0, NA, data$Y2S27_2)
data$Y2S27_3<-ifelse(data$Y2S27_3<0, NA, data$Y2S27_3)
data$Y2S27_4<-ifelse(data$Y2S27_4<0, NA, data$Y2S27_4)
data$Y2S27_5<-ifelse(data$Y2S27_5<0, NA, data$Y2S27_5)
data$Y2S27_6<-ifelse(data$Y2S27_6<0, NA, data$Y2S27_6)
data$Y2S27_7<-ifelse(data$Y2S27_7<0, NA, data$Y2S27_7)
data$Y2S27_8<-ifelse(data$Y2S27_8<0, NA, data$Y2S27_8)
data$Y2S27_9<-ifelse(data$Y2S27_9<0, NA, data$Y2S27_9)
data$Y2S27_10<-ifelse(data$Y2S27_10<0, NA, data$Y2S27_10)
data$Y2S2_24<-ifelse(data$Y2S2_24<0, NA, data$Y2S2_24)
data$Y2S2_25<-ifelse(data$Y2S2_25<0, NA, data$Y2S2_25)
data$Y2S2_26<-ifelse(data$Y2S2_26<0, NA, data$Y2S2_26)
data$Y2S2_28<-ifelse(data$Y2S2_28<0, NA, data$Y2S2_28)
data$Y2S2_29<-ifelse(data$Y2S2_29<0, NA, data$Y2S2_29)
data$Y2S2_30<-ifelse(data$Y2S2_30<0, NA, data$Y2S2_30)
data$Y2S14_2<-ifelse(data$Y2S14_2<0, NA, data$Y2S14_2)
data$Y2S14_3<-ifelse(data$Y2S14_3<0, NA, data$Y2S14_3)
data$Y2S14_5<-ifelse(data$Y2S14_5<0, NA, data$Y2S14_5)
data$Y2S14_7<-ifelse(data$Y2S14_7<0, NA, data$Y2S14_7)
data$Y2P31_1<-ifelse(data$Y2P31_1<0, NA, data$Y2P31_1)
data$Y2KOR_S<-ifelse(data$Y2KOR_S<0, NA, data$Y2KOR_S)

#새 변수 생성
data$s_school<-ifelse(data$CLASS1<=2, 1, 0)
data$s_school
data$reading_1<-rowMeans(data[c("Y1S19_2", "Y1S19_3", "Y1S19_5", "Y1S19_7")], na.rm=T)
data$reading_1
data$reading_2<-rowMeans(data[c("Y2S14_2", "Y2S14_3", "Y2S14_5", "Y2S14_7")], na.rm=T)
data$reading_2
data$enjoy_sch<-rowMeans(data[c("Y2S5_15", "Y2S5_16", "Y2S5_17", "Y2S5_18", "Y2S5_19", "Y2S5_20", "Y2S5_21", "Y2S5_22", "Y2S5_23")], na.rm=T)
data$enjoy_sch
data$self<-rowMeans(data[c("Y2S2_24", "Y2S2_25", "Y2S2_26", "Y2S2_28", "Y2S2_29", "Y2S2_30")], na.rm=T)
data$self
data$support<-rowMeans(data[c("Y2S27_1", "Y2S27_2","Y2S27_3","Y2S27_4","Y2S27_5","Y2S27_6","Y2S27_7","Y2S27_8","Y2S27_9","Y2S27_10")],na.rm=T)
data$support
data$parent_study<-rowMeans(data[c("Y2S27_1", "Y2S27_2", "Y2S27_3", "Y2S27_4", "Y2S27_5", "Y2S27_6", "Y2S27_7", "Y2S27_8")], na.rm=T)
data$parent_study
data$parent_emo<-rowMeans(data[c("Y2S27_9", "Y2S27_10")], na.rm=T)
data$parent_emo

#1
relation<-data.frame(data$Y1S11_5, data$Y1S11_6)
head(relation)
table(relation)
mosaicplot(~data$Y1S11_6+data$Y1S11_5, data = relation, color = TRUE,
           main = "학업 성적 vs 인성 교육")
summary(data$Y1S11_6)
table(data$Y1S11_5, data$Y1S11_6)

#install.packages("gmodels")
library(gmodels)

CrossTable(data$Y1S11_5, data$Y1S11_6, chisq = T, 
           expected=T, dnn=c("학업성적 중요시", "인성교육 중요시"),
           prop.r=F, prop.c=F, prop.t=F, 
           fisher = T)
#install.packages("rcompanion")
library(rcompanion)
cramerV(data$Y1S11_6, data$Y1S11_5)

relation<-data.frame(data$Y1S11_5, data$Y1S11_6)
head(relation)
table(relation)
mosaicplot(~data$Y1S11_6+data$Y1S11_5, data = relation, color = TRUE,
+            main = "학업 성적 vs 인성 교육")
data$Y1S11_6<-as.factor(data$Y1S11_6)
summary(data$Y1S11_6)
 #install.packages("rcompanion")
library(rcompanion)
cramerV(data$Y1S11_6, data$Y1S11_5)

#1-2번
data$Y1S11_5_re<-ifelse(data$Y1S11_5<3, 2, data$Y1S11_5)
data$Y1S11_5_re
data$Y1S11_6_re<-ifelse(data$Y1S11_6<3, 2, data$Y1S11_6)
data$Y1S11_6_re
summary(data$Y1S11_5_re)
summary(data$Y1S11_6_re)
table(data$Y1S11_6_re)
table(data$Y1S11_5_re, data$Y1S11_6_re)

relation<-data.frame(data$Y1S11_5_re, data$Y1S11_6_re)
head(relation)
table(relation)
mosaicplot(~data$Y1S11_6_re+data$Y1S11_5_re, data = relation, color = TRUE,
           main = "학업 성적 vs 인성 교육")
#install.packages("gmodels")
library(gmodels)

CrossTable(data$Y1S11_5_re, data$Y1S11_6_re, chisq = T, 
           expected=T, dnn=c("학업성적 중요시", "인성교육 중요시"),
           prop.r=F, prop.c=F, prop.t=F, 
           fisher = T)


#install.packages("rcompanion")
library(rcompanion)
cramerV(data$Y1S11_6_re, data$Y1S11_5_re)

#2-1번
#코드
data$enjoy_sch
data$enjoy_sch_re<-ifelse(data$enjoy_sch<2, 1, ifelse(data$enjoy_sch<3, 2, ifelse(data$enjoy_sch<4, 3, 4)))
data$enjoy_sch_re
table(data$enjoy_sch_re)
table(data$Y1S11_5_re)

mosaicplot(~data$Y1S11_5_re+data$enjoy_sch_re, data = relation, color = TRUE,
           main = "학업 성적 vs 학교생활만족도")

summary(data$Y1S11_5_re)
summary(data$enjoy_sch_re)

table(data$Y1S11_5_re, data$enjoy_sch_re)

#install.packages("gmodels")
library(gmodels)

CrossTable(data$Y1S11_5_re, data$enjoy_sch_re, chisq = T, 
           expected=T, dnn=c("학업성적 중요시", "학교생활만족도"),
           prop.r=F, prop.c=F, prop.t=F, 
           fisher = T)

#2-2번
#코드
mosaicplot(~data$Y1S11_6_re+data$enjoy_sch_re, data = relation, color = TRUE,
           main = "인성교육 중요시 vs 학교생활만족도")

summary(data$Y1S11_6_re)

summary(data$enjoy_sch_re)

table(data$Y1S11_6_re, data$enjoy_sch_re)

#install.packages("gmodels")
library(gmodels)

CrossTable(data$Y1S11_6_re, data$enjoy_sch_re, chisq = T, 
           expected=T, dnn=c("학업성적 중요시", "학교생활만족도"),
           prop.r=F, prop.c=F, prop.t=F, 
           fisher = T)
library(rcompanion)
cramerV(data$enjoy_sch_re, data$Y1S11_6_re)

#3번
#코드
#3번 문항 분석
#가정 배경 변수
#양적 변수
#income
data<-transform(data, INCOME_log = log(INCOME+1))
cor(data$INCOME_log, data$Y2KOR_S, use='complete.obs')
cor.test(data$INCOME_log, data$Y2KOR_S)
plot(data$INCOME_log, data$Y2KOR_S)

data<-transform(data, INCOME_log = log(INCOME+1))
data$INCOME_new<-ifelse(data$INCOME_log<3, NA, data$INCOME_log)
cor(data$INCOME_new, data$Y2KOR_S, use='complete.obs')
cor.test(data$INCOME_new, data$Y2KOR_S)
plot(data$INCOME_new, data$Y2KOR_S)

#parent_study
boxplot(data$parent_study)
cor(data$parent_study, data$Y2KOR_S, use='complete.obs')
cor.test(data$parent_study, data$Y2KOR_S)
plot(data$parent_study, data$Y2KOR_S)

#parent_emo
boxplot(data$parent_emo)
cor(data$parent_emo, data$Y2KOR_S, use='complete.obs')
cor.test(data$parent_emo, data$Y2KOR_S)
plot(data$parent_emo, data$Y2KOR_S)

#서열성 범주변수
data$PEDU_num<-ifelse(data$PEDU==1, 6, ifelse(data$PEDU==2, 9, ifelse(data$PEDU==3, 12, ifelse(data$PEDU==4, 14, ifelse(data$PEDU==5, 16, ifelse(data$PEDU==6, 18, data$PEDU))))))
data$MEDU_num<-ifelse(data$MEDU==1, 6, ifelse(data$MEDU==2, 9, ifelse(data$MEDU==3, 12, ifelse(data$MEDU==4, 14, ifelse(data$MEDU==5, 16, ifelse(data$MEDU==6, 18, data$MEDU))))))
data$PEDU_num
data$MEDU_num

cor(data$PEDU_num, data$Y2KOR_S, use='complete.obs')
cor.test(data$PEDU_num, data$Y2KOR_S)
plot(data$PEDU_num, data$Y2KOR_S)

cor(data$MEDU_num, data$Y2KOR_S, use='complete.obs')
cor.test(data$MEDU_num, data$Y2KOR_S)
plot(data$MEDU_num, data$Y2KOR_S)

#학교 특성 변수 분석
#범주 변수 2개인 것
# SECTOR
cor(data$SECTOR, data$Y2KOR_S, use='complete.obs')
cor.test(data$SECTOR, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$SECTOR)

table(data$SECTOR)

#s_school
cor(data$s_school, data$Y2KOR_S, use='complete.obs')
cor.test(data$s_school, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$s_school)

#범주 변수 3개 이상
#더미코딩
table(data$REGION)
data$D1<-ifelse(data$REGION==1, 1, 0)
data$D2<-ifelse(data$REGION==2, 1, 0)
data$D3<-ifelse(data$REGION==3, 1, 0)
data$D4<-ifelse(data$REGION==4, 1, 0)

data$C1<-ifelse(data$COEDU==1, 1, 0)
data$C2<-ifelse(data$COEDU==2, 1, 0)
data$C3<-ifelse(data$COEDU==3, 1, 0)

cor(data$D1, data$Y2KOR_S, use='complete.obs')
cor.test(data$D1, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$D1)

cor(data$D2, data$Y2KOR_S, use='complete.obs')
cor.test(data$D2, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$D2)

cor(data$D3, data$Y2KOR_S, use='complete.obs')
cor.test(data$D3, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$D3)

cor(data$D4, data$Y2KOR_S, use='complete.obs')
cor.test(data$D4, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$D4)


cor(data$C1, data$Y2KOR_S, use='complete.obs')
cor.test(data$C1, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$C1)

cor(data$C2, data$Y2KOR_S, use='complete.obs')
cor.test(data$C2, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$C2)

cor(data$C3, data$Y2KOR_S, use='complete.obs')
cor.test(data$C3, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$C3)

# 학생 개인 특성 변수
# 양적 변수
# 2학년 #ㅐ의 독서에 대한 즐거움
cor(data$reading_2, data$Y2KOR_S, use='complete.obs')
cor.test(data$reading_2, data$Y2KOR_S)
plot(data$reading_2, data$Y2KOR_S)

#학업 자아개념
cor(data$self, data$Y2KOR_S, use='complete.obs')
cor.test(data$self, data$Y2KOR_S)
plot(data$self, data$Y2KOR_S)

#1학년 때의 국어 성적
cor(data$Y1KOR_S, data$Y2KOR_S, use='complete.obs')
cor.test(data$Y1KOR_S, data$Y2KOR_S)
plot(data$Y1KOR_S, data$Y2KOR_S)

#범주 변수
#학생의 성별
cor(data$GENDER, data$Y2KOR_S, use='complete.obs')
cor.test(data$GENDER, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$GENDER)

#국어 사교육 참여 여부
data$Y2P31_1
cor(data$Y2P31_1, data$Y2KOR_S, use='complete.obs')
cor.test(data$Y2P31_1, data$Y2KOR_S)
boxplot(data$Y2KOR_S~data$Y2P31_1)

####회귀분석

model1<-lm(data$Y2KOR_S~data$MEDU_num)
summary(model1)

model2<-lm(data$Y2KOR_S~data$PEDU_num)
summary(model2)

model3<-lm(data$Y2KOR_S~data$parent_study)
summary(model3)

#
model4<-lm(data$Y2KOR_S~data$s_school)
summary(model4)

model4<-lm(data$Y2KOR_S~data$D2)
summary(model4)

model4<-lm(data$Y2KOR_S~data$D4)
summary(model4)

model4<-lm(data$Y2KOR_S~data$C2)
summary(model4)

model4<-lm(data$Y2KOR_S~data$C3)
summary(model4)

#
model4<-lm(data$Y2KOR_S~data$reading_2)
summary(model4)

model4<-lm(data$Y2KOR_S~data$self)
summary(model4)

model4<-lm(data$Y2KOR_S~data$Y1KOR_S)
summary(model4)

model4<-lm(data$Y2KOR_S~data$GENDER)
summary(model4)


#4번
#코드
lm<-lm(Y2KOR_S~MEDU_num+D2+D3+D4+GENDER, data = data)
summary(lm)

install.packages('lm.beta')
library(lm.beta)

multiple.reg<-lm.beta(lm)
summary(multiple.reg)

plot(lm)
library(car)
vif(lm)

#5번
#코드
lm<-lm(Y2KOR_S~Y1KOR_S+GENDER+MEDU_num+self, data = data)
summary(lm)
vif(lm)

lm1<-lm(Y2KOR_S~GENDER+MEDU_num+self, data = data)
summary(lm1)
vif(lm1)

library(lm.beta)

multiple.reg<-lm.beta(lm)
summary(multiple.reg)


multiple.reg<-lm.beta(lm1)
summary(multiple.reg)

cor(data$Y1KOR_S, data$self, use = 'complete.obs')
cor.test(data$Y1KOR_S, data$self)


cor(data$Y1KOR_S, data$MEDU_num, use = 'complete.obs')
cor.test(data$Y1KOR_S, data$MEDU_num)

cor(data$Y1KOR_S, data$GENDER, use = 'complete.obs')
cor.test(data$Y1KOR_S, data$GENDER)

