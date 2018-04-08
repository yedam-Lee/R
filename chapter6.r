##  6장 소스코드

# CSV 파일 읽어들이기
ad.data <- read.csv("./ad_result.csv", header = T, stringsAsFactors = F)
ad.data

# TV 광고의 광고비용과 신규 유저수의 산점도를 그리기
library(ggplot2)
library(scales)

ggplot(ad.data, aes(x = tvcm, y = install)) + geom_point() + 
  xlab("TV 광고비") + ylab("신규 유저수") + 
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = comma)

# 잡지매체의 광고비용과 신규 유저수의 산점도를 그리기
ggplot(ad.data, aes(x = magazine, y = install)) + geom_point() + 
  xlab("잡지 광고비") + ylab("신규 유저수") + 
  scale_x_continuous(label = comma) + 
  scale_y_continuous(label = comma)

# 회귀분석 실행
fit <- lm(install ~ ., data = ad.data[, c("install", "tvcm", "magazine")])
fit

# 회귀분석 결과를 해석하기
summary(fit)


