## 단순지수평활 1
library(forecast) # 예측을 위한 패키지 
library(data.table)
library(ggplot2)
library(lmtest)
library(TTR)

rain=scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))

rainseries

plot.ts(rainseries)

rainforecasts = HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
# 단순지수평활 
# Holtwinters( data, alpha = , beta = , gamma = )
# alpha (단순지수평활에서는 w) : 평균성분, level
# beta : 추세성분, trend
# gamma : 계절성분
# 아무런 값을 지정하지 않으면 알아서 최적의 값을 찾아준다.
# l.start : 평활 초기값 ( 지정하지 않으면 첫 번째 값을 사용 )
rainforecasts
# alpha = w 가 0.024로 0에 가깝다.
# 과거 값에 더 많은 영향을 받기 때문에 smoothing 효과가 아주 커진다.

head(rainforecasts$fitted)
plot(rainforecasts)
rainforecasts$SSE

par(mfrow=c(3,1))
plot(HoltWinters(rainseries, alpha=0.3,
                 beta=FALSE, gamma=FALSE), main="Alpha=0.3")
plot(HoltWinters(rainseries, alpha=0.7,
                 beta=FALSE, gamma=FALSE), main="Alpha=0.7")
plot(HoltWinters(rainseries, alpha=1,
                 beta=FALSE, gamma=FALSE), main="Alpha=1")

alpha1=HoltWinters(rainseries, alpha=1, beta=FALSE, gamma=FALSE)
alpha1$SSE
alpha07=HoltWinters(rainseries, alpha=0.7, beta=FALSE, gamma=FALSE)
alpha07$SSE
alpha03=HoltWinters(rainseries, alpha=0.3, beta=FALSE, gamma=FALSE)
alpha03$SSE
# alpha = 0.3일 때 SSE가 가장 작다.

rainforecasts35 = HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=35)
rainforecasts35

par(mfrow=c(1,1))
plot(rainforecasts35)

rainforecasts2 <- forecast(rainforecasts, h=5)
# forecast : 관측된 시점의 다음 시점의 관측값을 예측하기 위한 함수 
# h=5 : 앞으로 5개의 시점을 예측하라는 뜻 
rainforecasts2
# 예측값과 신뢰구간이 주어진다.
plot(rainforecasts2)
