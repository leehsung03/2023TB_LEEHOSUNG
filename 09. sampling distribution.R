#데이터에서 크기 2인 표본추출
salary=c(45,50,50,55,55,60,60,65,70,90) #연봉데이터
com2=combn(salary,2) #연봉데이터에서 2개씩 선택해서 데이터로 생성, com2에 저장
print(com2)

#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#[1,]   45   45   45   45   45   45   45   45   45    50    50    50    50    50    50    50    50    50    50    50
#[2,]   50   50   55   55   60   60   65   70   90    50    55    55    60    60    65    70    90    55    55    60
#[,21] [,22] [,23] [,24] [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38] [,39]
#[1,]    50    50    50    50    55    55    55    55    55    55    55    55    55    55    55    60    60    60    60
#[2,]    60    65    70    90    55    60    60    65    70    90    60    60    65    70    90    60    65    70    90
#[,40] [,41] [,42] [,43] [,44] [,45]
#[1,]    60    60    60    65    65    70
#[2,]    65    70    90    70    90    90
 

#표본의 크기2로 추출된 데이터의 평균을 구한 뒤 막대그래프 작성
com2t=t(com2)#데이터의 전치치
z=rowMeans(com2t)#전치된 데이터의 행평균 계산
barplot(table(z),xlab="연봉평균",ylab="도수",main="연봉평균의 분포")

#표본평균의 기댓값 구하기
meanofz=mean(z) #z의 평균
print(meanofz)

#[1] 60

#표본의 크기를 늘려 정규분포에 가까워지는지 확인하기
#sample size=2
data50=c(1:50)#데이터
com2=combn(data50,2)#데이터에서 2개씩 선택하여 데이터 생성
com2t=t(com2)#데이터의 전치
z2=rowMeans(com2t)#전치된 데이터의 행평균 계산
meanofz2=mean(z2)#기댓값을 계산하여 저장
print(meanofz2)
barplot(table(z2),xlab="평균2",ylab="도수",main="표본크기2의 도수분포포")

#sample size=3
com3=combn(data50,3)#데이터에서 3개씩 선택하여 데이터 생성
com3t=t(com3)#데이터의 전치
z3=rowMeans(com3t)#전치된 데이터의 행평균 계산
meanofz3=mean(z3)#기댓값을 계산하여 저장
print(meanofz3)
barplot(table(z3),xlab="평균3",ylab="도수",main="표본크기3의 도수분포포")

#sample size=5
com5=combn(data50,5)#데이터에서 5개씩 선택하여 데이터 생성
com5t=t(com5)#데이터의 전치
z5=rowMeans(com5t)#전치된 데이터의 행평균 계산
meanofz5=mean(z5)#기댓값을 계산하여 저장
print(meanofz5)
barplot(table(z5),xlab="평균3",ylab="도수",main="표본크기5의 도수분포포")

#표본추출방법: 단순무작위추출법
sampling=c(1:100)
print(sampling)
sample(x=sampling, size=10)

#[1]   1   2   3   4   5   6   7   8   9  10  11  12  13
#[14]  14  15  16  17  18  19  20  21  22  23  24  25  26
#[27]  27  28  29  30  31  32  33  34  35  36  37  38  39
#[40]  40  41  42  43  44  45  46  47  48  49  50  51  52
#[53]  53  54  55  56  57  58  59  60  61  62  63  64  65
#[66]  66  67  68  69  70  71  72  73  74  75  76  77  78
#[79]  79  80  81  82  83  84  85  86  87  88  89  90  91
#[92]  92  93  94  95  96  97  98  99 100
#> sample(x=sampling, size=10)
#[1] 53 99 54 25 49 65 82 14 33 29

#표본추출방법: 층화추출법
install.packages("sampling")
library(sampling)
sam2=strata(data=iris,stratanames=c("Species"),size=c(3,3,3),method='srswor') #iris데이터 사용하여 표본추출, Species층을 기준으로
sample2=getdata(data=iris,m=sam2) #추출된데이터 저장
print(sample2)

#Sepal.Length Sepal.Width Petal.Length Petal.Width
#7            4.6         3.4          1.4         0.3
#21           5.4         3.4          1.7         0.2
#25           4.8         3.4          1.9         0.2
#68           5.8         2.7          4.1         1.0
#81           5.5         2.4          3.8         1.1
#94           5.0         2.3          3.3         1.0
#133          6.4         2.8          5.6         2.2
#140          6.9         3.1          5.4         2.1
#144          6.8         3.2          5.9         2.3
#Species ID_unit Prob Stratum
#7       setosa       7 0.06       1
#21      setosa      21 0.06       1
#25      setosa      25 0.06       1
#68  versicolor      68 0.06       2
#81  versicolor      81 0.06       2
#94  versicolor      94 0.06       2
#133  virginica     133 0.06       3
#140  virginica     140 0.06       3
#144  virginica     144 0.06       3
> 
