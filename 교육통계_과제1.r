#0. 데이터 가공
#데이터 불러오기
setwd("C:\\Users\\82109\\Desktop\\교육통계 실습과제")
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
data$Y1S11_5<-ifelse(data$Y1S11_6<0, NA, data$Y1S11_6)
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
#1번. 

#(1) 1학년과 2학년 때 학생들의 독서에 대한 즐거움 정도가 차이가 있는지 살펴보시오. 

qqnorm(data$reading_1)
qqline(data$reading_1)
qqnorm(data$reading_2)
qqline(data$reading_2)

data$diff<-data$reading_1-data$reading_2
hist(data$diff)
boxplot(data$diff)
summary(data$diff)

paired<-t.test(data$reading_1, data$reading_2, paired=TRUE, alternative = c(“two.sided”))
print(paired)

#(2) 또한 1학년과 2학년 때의 국어성적에도 차이가 있는지 살펴보시오
qqnorm(data$Y1KOR_S)
qqline(data$Y1KOR_S)
qqnorm(data$Y2KOR_S)
qqline(data$Y2KOR_S)

data$diff<-data$Y1KOR_S-data$Y2KOR_S
data$diff
hist(data$diff)
summary(data$diff)
boxplot(data$diff)

paired<-t.test(data$Y1KOR_S, data$Y2KOR_S, paired=TRUE, alternative = c("two.sided"))
print(paired)
#2번. 

#(1) 학생들이 다니고 있는 학교가 소규모 학교인지 여부에 따라서 학생들의 2학년 때 학교교육 만족도와 학업자아개념, 국어 성적의 차이가 있는지를 살펴보시오. 

data$s_school <- as.factor(data$s_school)
levels(data$s_school)
levels(data$s_school) <- c("small", "big")
data$s_school
data$GENDER<-as.factor(data$GENDER)
levels(data$GENDER) <- c("male", "female")
data$GENDER

summary(data[data$s_school=="small",]$enjoy_sch)
summary(data[data$s_school=="big",]$enjoy_sch)
boxplot(data$enjoy_sch~data$s_school)

summary(data[data$s_school=="small",]$self)
summary(data[data$s_school=="big",]$self)
boxplot(data$self~data$s_school)


summary(data[data$s_school=="small",]$Y2KOR_S)
summary(data[data$s_school=="big",]$Y2KOR_S)
boxplot(data$Y2KOR_S~data$s_school)

qqnorm(data[data$s_school=="small",]$enjoy_sch)
qqline(data[data$s_school=="small",]$enjoy_sch)
qqnorm(data[data$s_school=="big",]$enjoy_sch)
qqline(data[data$s_school=="big",]$enjoy_sch)
qqnorm(data[data$s_school=="small",]$self)
qqline(data[data$s_school=="small",]$self)
qqnorm(data[data$s_school=="big",]$self)
qqline(data[data$s_school=="big",]$self)
qqnorm(data[data$s_school=="small",]$Y2KOR_S)
qqline(data[data$s_school=="small",]$Y2KOR_S)
qqnorm(data[data$s_school=="big",]$Y2KOR_S)
qqline(data[data$s_school=="big",]$Y2KOR_S)
var<-var.test(enjoy_sch~s_school, data)
var
var1<-var.test(self~s_school, data)
var1
var2<-var.test(Y2KOR_S~s_school, data)
var2


model1 <- t.test(enjoy_sch~s_school, data, var.equal=T)
model1
model1 <- t.test(self~s_school, data, var.equal=T)
model1
model1 <- t.test(Y2KOR_S~s_school, data, var.equal=T)
model1

#2-2번
summary(data[data$GENDER=="male",]$enjoy_sch)
summary(data[data$GENDER=="female",]$enjoy_sch)
boxplot(data$enjoy_sch~data$GENDER)

summary(data[data$GENDER=="male",]$self)
summary(data[data$GENDER=="female",]$self)
boxplot(data$enjoy_sch~data$s_school)

summary(data[data$GENDER=="male",]$Y2KOR_S)
summary(data[data$GENDER=="female",]$Y2KOR_S)
boxplot(data$enjoy_sch~data$s_school)

#가설 확인
qqnorm(data[data$GENDER=="male",]$enjoy_sch)
qqline(data[data$GENDER=="male",]$enjoy_sch)

qqnorm(data[data$GENDER=="female",]$enjoy_sch)
qqline(data[data$GENDER=="female",]$enjoy_sch)

qqnorm(data[data$GENDER=="male",]$self)
qqline(data[data$GENDER=="male",]$self)

qqnorm(data[data$GENDER=="female",]$self)
qqline(data[data$GENDER=="female",]$self)

qqnorm(data[data$GENDER=="male",]$Y2KOR_S)
qqline(data[data$GENDER=="male",]$Y2KOR_S)

qqnorm(data[data$GENDER=="female",]$Y2KOR_S)
qqline(data[data$GENDER=="female",]$Y2KOR_S)


var<-var.test(enjoy_sch~GENDER, data)
var
var1<-var.test(self~GENDER, data)
var1
var2<-var.test(Y2KOR_S~GENDER, data)
var2

#독립 표본 t검정
model1 <- t.test(enjoy_sch~GENDER, data, var.equal=T)
model1
model1 <- t.test(self~GENDER, data, var.equal=T)
model1
model1 <- t.test(Y2KOR_S~GENDER, data, var.equal=T)
model1

#3번. 
#(1) 학교가 남녀공학인지 아니면 남학교 혹은 여학교인지에 따라서 학생들의 학교교육 만족도에 차이가 있는지 살펴보시오. 
data$CODEU <- as.factor(data$COEDU)
levels(data$CODEU)
levels(data$CODEU) <- c("coedu", "boy", "girl")

data$REGION <- as.factor(data$REGION)
levels(data$REGION)
levels(data$REGION) <- c("XL", "L", "M", "S")

summary(data[data$CODEU=="coedu",]$enjoy_sch)
summary(data[data$CODEU=="boy",]$enjoy_sch)
summary(data[data$CODEU=="girl",]$enjoy_sch)
boxplot(data$enjoy_sch~data$CODEU)

qqnorm(data[data$CODEU=="coedu",]$enjoy_sch)
qqline(data[data$CODEU=="coedu",]$enjoy_sch)
qqnorm(data[data$CODEU=="boy",]$enjoy_sch)
qqline(data[data$CODEU=="boy",]$enjoy_sch)
qqnorm(data[data$CODEU=="girl",]$enjoy_sch)
qqline(data[data$CODEU=="girl",]$enjoy_sch)

var<-var.test(enjoy_sch~COEDU, data)
var

w <- aov(enjoy_sch~factor(CODEU), data)
summary(w)

tukey.post<-TukeyHSD(w)
tukey.post

#(2) 또한 지역규모에 따라서 학교교육 만족도에 차이가 있는지도 추가로 살펴보시오. 

summary(data[data$REGION=="XL",]$enjoy_sch)
summary(data[data$REGION=="L",]$enjoy_sch)
summary(data[data$REGION=="M",]$enjoy_sch)
summary(data[data$REGION=="S",]$enjoy_sch)
boxplot(data$enjoy_sch~data$REGION)

qqnorm(data[data$REGION=="XL",]$enjoy_sch)
qqline(data[data$REGION=="XL",]$enjoy_sch)

qqnorm(data[data$REGION=="L",]$enjoy_sch)
qqline(data[data$REGION=="L",]$enjoy_sch)

qqnorm(data[data$REGION=="M",]$enjoy_sch)
qqline(data[data$REGION=="M",]$enjoy_sch)

qqnorm(data[data$REGION=="S",]$enjoy_sch)
qqline(data[data$REGION=="S",]$enjoy_sch)

var<-var.test(enjoy_sch~REGION, data)
var

w <- aov(enjoy_sch~factor(REGION), data)
summary(w)

#4번. 2학년 때의 학교교육 만족도가 지역규모와 학생 자신의 학생회 활동 경험 여부에 따라서 차이가 있는지를 살펴보되(이원분산분석 2*4 요인구조)

data$council<-as.factor(data$Y1S15)
levels(data$council)
levels(data$council) <-c("yes", "no")
data$council

summary(data[data$REGION=="XL",]$enjoy_sch)
summary(data[data$REGION=="L",]$enjoy_sch)
summary(data[data$REGION=="M",]$enjoy_sch)
summary(data[data$REGION=="S",]$enjoy_sch)

summary(data[data$council=="yes",]$enjoy_sch)
summary(data[data$council=="no",]$enjoy_sch)

boxplot(data$enjoy_sch~data$REGION)
boxplot(data$enjoy_sch~data$council)

qqnorm(data[data$council=="yes",]$enjoy_sch)
qqline(data[data$council=="yes",]$enjoy_sch)
qqnorm(data[data$council=="no",]$enjoy_sch)
qqline(data[data$council=="no",]$enjoy_sch)
qqnorm(data[data$REGION=="XL",]$enjoy_sch)
qqline(data[data$REGION=="XL",]$enjoy_sch)
qqnorm(data[data$REGION=="L",]$enjoy_sch)
qqline(data[data$REGION=="L",]$enjoy_sch)
qqnorm(data[data$REGION=="M",]$enjoy_sch)
qqline(data[data$REGION=="M",]$enjoy_sch)
qqnorm(data[data$REGION=="S",]$enjoy_sch)
qqline(data[data$REGION=="S",]$enjoy_sch)

library(car)
leveneTest(data$enjoy_sch~factor(data$REGION))
leveneTest(data$enjoy_sch~factor(data$council))

tapply(data$enjoy_sch, data$REGION, mean, na.rm=TRUE)
tapply(data$enjoy_sch, data$REGION, sd, na.rm=TRUE)


twanova1 <- aov(enjoy_sch~factor(council)*factor(REGION), data)
summary(twanova1)

#5번. 1학년 때 수학교과에 있어서 수준별 이동수업을 하는지 여부가 학교의 설립 구분과 지역규모에 따라서 차이가 있는지 살펴보시오. - 기술통계와 그래프만
#- 학교 설립 구분 -> 수학교과 수준별 이동수업 여부
#- 지역 규모 -> 수학교과 수준별 이동수업 여부

data$SECTOR<-as.factor(data$SECTOR)
levels(data$SECTOR)
levels(data$SECTOR)<-c("public", "private")
summary(data$SECTOR)

data$mathlevel<-as.factor(data$Y1H7_2_1)
levels(data$mathlevel) <-c("N", "Y")
summary(data$mathlevel)

summary(data$REGION)

tapply(data$Y1H7_2_1, data$SECTOR, mean, na.rm=TRUE)
tapply(data$Y1H7_2_1, data$SECTOR, sd, na.rm=TRUE)

tapply(data$Y1H7_2_1, data$REGION, mean, na.rm=TRUE)
tapply(data$Y1H7_2_1, data$REGION, sd, na.rm=TRUE)

fac <- table(data$SECTOR, data$mathlevel)
fac

fac2 <- table(data$REGION, data$mathlevel)
fac2

barplot(fac, beside = T, legend=T, args.legend=list(x='topright'))
barplot(fac2, beside=T, legend=T, args.legend = list(x = 'topright'))

fac3 <- table(data$REGION, data$SECTOR, data$mathlevel)
fac3

#6번. 선생님이 학업성적을 얼마나 중요하게 생각하는지, 
#그리고 인성교육을 얼마나 중요하게 생각하는지에,
#따라서 학생들의 수학 성적의 평균이 다르게 나타나는지 
#이원분산분석을 사용하여 살펴보시오. 

data$Y1S11_5
data$Y1S11_6
data$Y1MAT_S

summary(data[data$Y1S11_5==1,]$Y1MAT_S)
summary(data[data$Y1S11_5==2,]$Y1MAT_S)
summary(data[data$Y1S11_5==3,]$Y1MAT_S)
summary(data[data$Y1S11_5==4,]$Y1MAT_S)
summary(data[data$Y1S11_5==5,]$Y1MAT_S)

summary(data[data$Y1S11_6==1,]$Y1MAT_S)
summary(data[data$Y1S11_6==2,]$Y1MAT_S)
summary(data[data$Y1S11_6==3,]$Y1MAT_S)
summary(data[data$Y1S11_6==4,]$Y1MAT_S)
summary(data[data$Y1S11_6==5,]$Y1MAT_S)

qqnorm(data[data$Y1S11_5==1,]$Y1MAT_S)
qqline(data[data$Y1S11_5==1,]$Y1MAT_S)
qqnorm(data[data$Y1S11_5==2,]$Y1MAT_S)
qqline(data[data$Y1S11_5==2,]$Y1MAT_S)
qqnorm(data[data$Y1S11_5==3,]$Y1MAT_S)
qqline(data[data$Y1S11_5==3,]$Y1MAT_S)
qqnorm(data[data$Y1S11_5==4,]$Y1MAT_S)
qqline(data[data$Y1S11_5==4,]$Y1MAT_S)
qqnorm(data[data$Y1S11_5==5,]$Y1MAT_S)
qqline(data[data$Y1S11_5==5,]$Y1MAT_S)

qqnorm(data[data$Y1S11_6==5,]$Y1MAT_S)
qqline(data[data$Y1S11_6==5,]$Y1MAT_S)
qqnorm(data[data$Y1S11_6==4,]$Y1MAT_S)
qqline(data[data$Y1S11_6==4,]$Y1MAT_S)
qqnorm(data[data$Y1S11_6==3,]$Y1MAT_S)
qqline(data[data$Y1S11_6==3,]$Y1MAT_S)
qqnorm(data[data$Y1S11_6==2,]$Y1MAT_S)
qqline(data[data$Y1S11_6==2,]$Y1MAT_S)
qqnorm(data[data$Y1S11_6==1,]$Y1MAT_S)
qqline(data[data$Y1S11_6==1,]$Y1MAT_S)

library(car)
leveneTest(data$Y1MAT_S~factor(data$Y1S11_6))
leveneTest(data$Y1MAT_S~factor(data$Y1S11_5))

twanova1 <- aov(Y1MAT_S~factor(Y1S11_6)*factor(Y1S11_5), data)
summary(twanova1)

twanova1 <- aov(Y1MAT_S~factor(Y1S11_6)*factor(Y1S11_5), data)
summary(twanova1)
TukeyHSD(twanova1)

bonferroni.post<-pairwise.t.test(data$Y1MAT_S, data$Y1S11_6)
bonferroni.post
