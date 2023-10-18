library(dplyr)
library(ggplot2)
str(congestion)
summary(congestion)
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))
ggplot(congestion1, aes(y=s0530))+
geom_boxplot()
summary(congestion1$s0530)
#먼저 파생변수를 만들어 봅시다. 모든 시간대 혼잡도의 평균을 만들어 봅시다.
#1.지하철역의 하루 평균 혼잡도
congestion$day_mean <- rowMeans(congestion[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])
#지하철 호선별 하루 평균 혼잡도
congestion1 %>%
  group_by(line) %>%
  summarise(line_mean = mean(day_mean))
# line line_mean
#<dbl>     <dbl>
#  1     1      25.0
#2     2      32.7
#3     3      25.9
#4     4      28.5
#5     5      25.2
#6     6      20.8
#7     7      30.9
#8     8      24.9

#지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도
#기술통계분석 결과를 포함합니다.
congestion1 %>%
  summarise(s0700_mean = mean(s0700, na.rm = TRUE))

congestion1 %>%
  summarise(s0730_mean = mean(s0730, na.rm = TRUE))

congestion1 %>%
  summarise(s0800_mean = mean(s0800, na.rm = TRUE))

congestion1 %>%
  summarise(s0830_mean = mean(s0830, na.rm = TRUE))

congestion1 %>%
  summarise(s0900_mean = mean(s0900, na.rm = TRUE))
#18.7+24.6+32.3+30.1+29.4   평균 27.02
#기술통계분석 결과를 포함합니다.
a1 = c(congestion1$s0700, congestion1$s0730, congestion1$s0800, congestion1$s0830, congestion1$s0900)

#
summary(a1, na_rm=True)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   10.80   18.60   27.02   33.88  185.50 

#평균혼잡도가가장높은시간대를막대그래프로그리기
v = c(18.7, 24.6,32.3,30.1,29.4)
barplot(v)
name = c('s0700','s0730','s0800','s0830','s0900')
barplot(v,names=name) # barplot(v~name)

#평균혼잡도 상위 4개 호선의 역별 기여도

library(dplyr)

# 추가 열을 비교하기 위해 필요한 열 이름을 나열
columns_to_compare <- c("day_mean", "s0700", "s0730", "s0800", "s0830", "s0900")

# "congestion" 데이터 프레임을 이용하여 "line" 별로 열들을 비교
result <- congestion %>%
  group_by(line, station) %>%
  summarize(across(all_of(columns_to_compare), mean)) %>%
  arrange(desc(day_mean)) %>%
  head(4)

# 결과 출력
print(result)

#line station day_mean s0700 s0730 s0800 s0830 s0900
#<dbl> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#1     4 남태령      47.3  34.6  39.2  48.2  48.9  58.2
#2     2 서초        46.4  28.8  34.9  46.8  53.0  55.1
#3     2 방배        46.4  31.0  36.2  47.6  54.0  54.6
#4     8 송파        46.0  32.2  41.6  56.0  57.0  53.2

#출발시간8시의지하철혼잡도범주화/범주별빈도분석
congestion1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%  
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s80_grade=="caution")%>%  
  select(line, s80_grade,n,pct)%>%  arrange(desc(pct))  
#line s80_grade     n   pct
#<dbl> <chr>     <int> <dbl>
#1     7 caution       6   2.4
#2     2 caution       6   2  
#3     8 caution       2   1.9
#4     3 caution       3   1.5
#5     4 caution       2   1.3
#6     5 caution       2   0.6

#지하철 호선별 퇴근시간(18:00~20:00)대의 평균혼잡도
congestion1 %>%
  summarise(s1800_mean = mean(s1800, na.rm = TRUE))

congestion1 %>%
  summarise(s1830_mean = mean(s1830, na.rm = TRUE))

congestion1 %>%
  summarise(s1900_mean = mean(s1900, na.rm = TRUE))

congestion1 %>%
  summarise(s1930_mean = mean(s1930, na.rm = TRUE))

congestion1 %>%
  summarise(s2000_mean = mean(s2000, na.rm = TRUE))
#40.8+38.3+30.3+25.3+23.6    평균31.66

#기술통계분석 결과를 포함합니다.
a2 = c(congestion1$s1800, congestion1$s1830, congestion1$s1900, congestion1$s1930, congestion1$s2000)


summary(a2, na_rm=True)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   17.20   27.60   31.65   40.50  155.50 


#평균혼잡도가가장높은시간대를막대그래프로그리기
v = c(40.8,38.3,30.3,25.3,23.6)
barplot(v)
name = c('s1800','s1830','s1900','s1930','s2000')
barplot(v,names=name) # barplot(v~name)

#평균혼잡도 상위 4개 호선의 역별 기여도
library(dplyr)

# 추가 열을 비교하기 위해 필요한 열 이름을 나열
columns_to_compare <- c("day_mean", "s1800", "s1830", "s1900", "s1930", "s2000")

# "congestion" 데이터 프레임을 이용하여 "line" 별로 열들을 비교
result <- congestion %>%
  group_by(line, station) %>%
  summarize(across(all_of(columns_to_compare), mean)) %>%
  arrange(desc(day_mean)) %>%
  head(4)

# 결과 출력
print(result)

#<dbl> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#1     4 남태령      47.3  75.9  83.6  51.2  41.5  37.2
#2     2 서초        46.4  65.6  66.5  53.3  42.2  41.9
#3     2 방배        46.4  64.0  67.3  52.3  41.0  42.3
#4     8 송파        46.0  71.6  62.4  52.4  40.9  38.3



#출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>% 
  mutate(s180_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%  
  group_by(line, s180_grade) %>%
  summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s180_grade=="bad")%>%  
  select(line, s180_grade,n,pct)%>%  arrange(desc(pct))  
# Groups:   line [1]
#line s180_grade     n   pct
#<dbl> <chr>      <int> <dbl>
#  1     7 bad            1   0.4
