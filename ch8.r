##  8장 소스코드
install.packages("plyr")
install.packages("foreach")
install.packages("ykmeans")
install.packages("ggplot2")
install.packages("scales")
install.packages("caret")
install.packages("fmsb")

# 데이터를 읽어들이기 위한 함수 작성
library(plyr)
library(foreach)
readTsvDates <- function(base.dir, app.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  x <- ldply(foreach(day = dates, combine = rbind) %do% {
    read.csv(sprintf("%s/%s/%s/data.tsv", base.dir, app.name, day),
             header = T,
             sep = "\t", stringsAsFactors = F)
  })
  x
}

# DAU를 읽어들이는 함수
readDau <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates("sample-data/section8/daily/dau", app.name,
                       date.from, date.to)
  data
}


# DPU를 읽어들이는 함수
readDpu <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates("sample-data/section8/daily/dpu", app.name,
                       date.from, date.to)
  data
}


# 행동데이터를 읽어들이는 함수
readActionDaily <- function(app.name, date.from, date.to = date.from) {
  data <- readTsvDates("sample-data/section8/daily/action", app.name,
                       date.from, date.to)
  data
}


# 데이터 읽어들이기

# DAU
dau <- readDau("game-01", "2013-05-01", "2013-10-31")
head(dau)
# DPU
dpu <- readDpu("game-01", "2013-05-01", "2013-10-31")
head(dpu)
# Action
user.action <- readActionDaily("game-01", "2013-10-31", "2013-10-31")
head(user.action)

# DAU에 DPU를 결합시키기

# 과금 데이터 결합
dau2 <- merge(dau, dpu[ ,c("log_date", "user_id", "payment"), ],
              by = c("log_date", "user_id"), all.x = T)
head(dau2)
# 과금 플래그를 추가하기
dau2$is.payment <- ifelse(is.na(dau2$payment), 0, 1)
head(dau2)
# 비과금 레코드의 과금액에 0을 넣기
dau2$payment <- ifelse(is.na(dau2$payment), 0, dau2$payment)
head(dau2)

# 월별로 집계하기

# 월 항목을 작성하기
dau2$log_month <- substr(dau2$log_date, 1, 7)

# 월별 집계하기
mau <- ddply(dau2, .(log_month, user_id), summarize, payment
             = sum(payment),
             access_days = length(log_date))
head(mau)

# 랭킹 범위 결정
library(ykmeans)
library(ggplot2)
library(scales)
# A47항목이 랭킹 포인트
head(user.action)
user.action2 <- ykmeans(user.action, "A47", "A47", 3)
head(user.action2)
# 각 클러스터의 유저수
table(user.action2$cluster)

# 랭킹 포인트의 분포
ggplot(arrange(user.action2, desc(A47)),
       aes(x = 1:length(user_id), y = A47,
           col = as.factor(cluster), shape = as.factor(cluster))) +
  geom_point() +  #geom_line
  xlab("user") +
  ylab("Ranking point") +
  scale_y_continuous(label = comma) +
  ggtitle("Ranking Point") +
  theme(legend.position = "none")
# 랭킹 상위 유저만 고르기
user.action.h <- user.action2[user.action2$cluster >= 2,
                              names(user.action)]
head(user.action.h)

# 주성분 분석 실행

# 데이터 전처리에 편리한 함수를 이용하기 위해
# 기계학습 라이브러리를 사용함
library(caret)

user.action.f <- user.action.h[, -c(1:4)]
row.names(user.action.f) <- user.action.h$user_id
head(user.action.f)

# 정보량이 0에 가까운 변수를 삭제
nzv <- nearZeroVar(user.action.f)
user.action.f.filterd <- user.action.f[,-nzv]

# 변수간에 상관이 높은 것을 삭제
user.action.cor <- cor(user.action.f.filterd)
highly.cor.f <- findCorrelation(user.action.cor,cutoff=.7)
user.action.f.filterd <- user.action.f.filterd[,-highly.cor.f]

# 주성분 분석 실행
# pca
user.action.pca.base <- prcomp(user.action.f.filterd, scale = T)
str(user.action.pca.base)
user.action.pca.base$x
user.action.pca.base$rotation

# 클러스터링 실행하기
user.action.pca <- data.frame(user.action.pca.base$x)
keys <- names(user.action.pca)
user.action.km <- ykmeans(user.action.pca, keys, "PC1", 3:6)
table(user.action.km$cluster)

ggplot(user.action.km,
       aes(x=PC1,y=PC2,col=as.factor(cluster), shape=as.factor(cluster))) +
  geom_point()

# 클러스터별 평균을 산출하기
user.action.f.filterd$cluster <- user.action.km$cluster
user.action.f.center <-
  ldply(lapply(sort(unique(user.action.f.filterd$cluster)),
               function(i) {
                 x <- user.action.f.filterd[user.action.f.filterd$cluster == i,
                                            -ncol(user.action.f.filterd)]
                 apply(x, 2, function(d) mean(d))
               }))

# 레이더 차트용 데이터 작성하기
library(fmsb)
# 레이더 차트에 맞게 데이터를 만들어주는 함수
createRadarChartDataFrame <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max) + 1
  dfmin <- apply(df, 2, min) - 1
  as.data.frame(rbind(dfmax, dfmin, df))
}
# 상관이 높은 변수 제외하기
df <- user.action.f.center[, -(ncol(user.action.f.center) - 1)]
df.cor <- cor(df)
df.highly.cor <- findCorrelation(df.cor, cutoff = 0.91)
# 해석하기 쉬운 숫자가 되도록 직접 조절
df.filterd <- df[, -df.highly.cor]
# 레이터 차트용 데이터 작성하기
df.filterd <- createRadarChartDataFrame(scale(df.filterd))
names(df.filterd)

names(df.filterd) <- c("레벨", "지원횟수", "지원받은 횟수", "보스를 쓰러뜨린 횟수",
                       "싸움횟수", "게임이용횟수")


# 역자주: 아래는 일본어가 깨질때를 위한 코드인데 딱히 필요없는 부분이므로 주석처리합니다.
# 일본어가 깨질 때 적절한 폰트를 지정하기
par(family="HiraKakuProN-W3")
radarchart(df.filterd, seg = 5, plty = 1:5, plwd = 4, pcol = rainbow(5))
legend("topright", legend = 1:5, col = rainbow(5), lty = 1:5)

# 클러스터별 KPI를 산출하기

user.action.f.filterd$user_id <-
  as.numeric(rownames(user.action.f.filterd))
user.action.kpi <- merge(user.action.f.filterd, mau,by = "user_id")
ddply(user.action.kpi, .(cluster), summarize,
      arpu = round(mean(payment)),
      access_days = round(mean(access_days)))
