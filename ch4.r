##  4장 소스코드

# CSV 파일을 읽어들이기
dau <- read.csv("section4-dau.csv", header = T, stringsAsFactors = F)
head(dau)
user.info <- read.csv("section4-user_info.csv", header = T, stringsAsFactors = F)
head(user.info)

# DAU에 user.info를 결합시키기
dau.user.info <- merge(dau, user.info, by = c("user_id", "app_name"))
head(dau.user.info)

# 월 항목을 추가
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7)
# 세그먼트 분석（성별로 집계）
table(dau.user.info[, c("log_month", "gender")])

# 세그먼트 분석(연령대별로 집계）
table(dau.user.info[, c("log_month", "generation")])

# 세그먼트 분석（성별과 연령대를 조합해 집계）
library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id",
      length)

# 세그먼트 분석（단말기별로 집계）
table(dau.user.info[,c("log_month","device_type")])

# 세그먼트 분석 결과를 시각화하기

# 날짜별로 단말기별 유저수를 산출하기
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
# 날짜별 데이터 형식으로 변환하기
dau.user.info.device.summary$log_date <- as.Date(dau.user.info.device.summary$log_date)
# 시계열의 트렌드 그래프 그리기
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)
