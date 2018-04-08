##  5장 소스코드

# ab.test.imp 데이터에 ab.test.goal 데이터를 결합시키기
# 데이터를 읽어들이기
ab.test.imp <- read.csv("section5-ab_test_imp.csv",header=T, stringsAsFactors=F)   # 배너광고의 표시횟수정보
ab.test.goal <- read.csv("section5-ab_test_goal.csv",header=T, stringsAsFactors=F) # 배너광고의 클릭횟수정보
# ab.test.imp에 ab.test.goal를 결합시키기
ab.test.imp <- merge(ab.test.imp, ab.test.goal, by="transaction_id", all.x=T, suffixes=c("",".g"))
head(ab.test.imp)

# 
# 클릭 플래그를 추가
ab.test.imp$is.goal <- ifelse(is.na(ab.test.imp$user_id.g),0,1)
head(ab.test.imp)

# 
# 클릭율을 계산하기
library(plyr)
ddply(ab.test.imp, .(test_case), summarize,
      cvr=sum(is.goal)/length(user_id))

# χ2 검정을 실행하기
chisq.test(ab.test.imp$test_case, ab.test.imp$is.goal)

# 
# 날짜별, 테스트 케이스별로 클릭율을 산출하기
ab.test.imp.summary <-
  ddply(ab.test.imp, .(log_date, test_case), summarize,
        imp=length(user_id),
        cv=sum(is.goal),
        cvr=sum(is.goal)/length(user_id))
# 테스트 케이스별로 클릭율을 산출하기
ab.test.imp.summary <-
  ddply(ab.test.imp.summary, .(test_case), transform,
        cvr.avg=sum(cv)/sum(imp))
head(ab.test.imp.summary)

# 테스트 케이스별 클릭율의 시계열추이 그래프
library(ggplot2)
library(scales)
ab.test.imp.summary$log_date <- as.Date(ab.test.imp.summary$log_date)
limits <- c(0, max(ab.test.imp.summary$cvr))
ggplot(ab.test.imp.summary,aes(x=log_date,y=cvr, col=test_case,lty=test_case, shape=test_case)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  geom_line(aes(y=cvr.avg,col=test_case)) +
  scale_y_continuous(label=percent, limits=limits)