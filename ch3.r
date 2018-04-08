##  3장 소스코드（2014/08/22 새 버전의 ggplot2에서 히스토그램이 출력되지 않는 문제를 수정）

# CSV 파일을 읽어들이기
dau <- read.csv("section3-dau.csv", header = T, stringsAsFactors = F)
head(dau)
dpu <- read.csv("section3-dpu.csv", header = T, stringsAsFactors = F)
head(dpu)
install <- read.csv("section3-install.csv", header = T, stringsAsFactors= F)
head(install)
str(dau)
summary(dau)

# DAU 데이터에 Install 데이터를 결합시키기
dau.install <- merge(dau, install, by = c("user_id", "app_name"))
head(dau.install)

# 위 데이터에 다시 DPU 데이터를 결합시키기

dau.install.payment <- merge(dau.install, dpu, by = c("log_date",
                                                      "app_name", "user_id"), all.x = T)
head(dau.install.payment)


head(na.omit(dau.install.payment))


# 비과금 유저의 과금액에 0을 넣기

dau.install.payment$payment[is.na(dau.install.payment$payment)] <- 0
head(dau.install.payment)

# 월차로 집계하기

# 월 항목 추가
dau.install.payment$log_month <-substr(dau.install.payment$log_date, 1, 7)
dau.install.payment$install_month <- substr(dau.install.payment$install_date, 1, 7)
install.packages("plyr")
library("plyr")
mau.payment <- ddply(dau.install.payment,
                     .(log_month, user_id, install_month), # 그룹화
                     summarize, # 집계 명령
                     payment = sum(payment) # payment 합계
)
head(mau.payment)

# 신규 유저인지 기존 유저인지 구분하는 항목을 추가

# 신규 유저와 기존 유저 식별
mau.payment$user.type <- ifelse(mau.payment$install_month == mau.payment$log_month,
                                "install", "existing")
mau.payment.summary <- ddply(mau.payment,
                             .(log_month, user.type), # 그룹화
                             summarize, # 집계 명령어
                             total.payment = sum(payment) # payment 합계
)
head(mau.payment) 
head(mau.payment.summary)

# 그패프로 데이터를 시각화하기 （geom_bar()　->　geom_bar(stat="identity")로 수정 2014/08/22）
library("ggplot2")
library("scales")
ggplot(mau.payment.summary, aes(x = log_month, y = total.payment,
                                fill = user.type)) + geom_bar(stat="identity") + scale_y_continuous(label = comma)

# old_theme = theme_update(
# axis.title.x = theme_text(family="HiraKakuProN-W3"),
# axis.title.y = theme_text(family="HiraKakuProN-W3", angle=90),
# plot.title = theme_text(family="HiraKakuProN-W3", size=14.4))

ggplot(mau.payment[mau.payment$payment > 0 & mau.payment$user.type == "install", ], 
       aes(x = payment, fill = log_month)) + geom_histogram(position = "dodge", binwidth = 20000)
