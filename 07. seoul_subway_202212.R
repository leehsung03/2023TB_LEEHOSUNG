install.packages("dplyr")
install.packages("ggplot2")

                 
                 
library(dplyr)
library(ggplot2)
str(subway)
summary(subway)

subway$day<-substr(subway$Date,7,8)
class(subway$day)
subway$day<-as.integer(subway$day)

subway$day<-substr(subway$Date,7,8)
class(subway$day)
subway$day<-as.integer(subway$day)

table(subway$Line)
subway$Line<-ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)
table(subway$Station)
subway$total_passenger<-subway$on_board+subway$getting_off
str(subway)
#전체노선에서1개역의하루평균승차, 하차승객수를알아봅시다.#1.지하철역의하루평균승차/하차승객수
subway%>%  summarise(on_m=mean(on_board), off_m=mean(getting_off))
#승차승객이가장많았던역을찾아서요일, 노선, 역이름, 승차승객수만을출력합니다. 아래명령어를실행해봅시다.
max(subway$on_board)
#승차승객의최대값이확인됩니다. 이데이터를활용해서명령어를완성해봅시다.
subway%>%
  filter(on_board==94732)%>%
  select(Date, Line, Station, on_board)
#하루평균전체승객수가많은역10곳의이름과노선, 하루평균전체승객수를구해서새객체passenger10에입력하고상위3개역을확인해봅시다. 아래명령어를실행합니다.
passenger10 <-subway %>%
  group_by(Station)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)

head(passenger10, 3)
#앞서만들어놓은passenger10을이용하여막대그래프를그려봅시다. 오늘은ggplot패키지를이용하여작성합니다. 아래명령어를실행해봅시다.
ggplot(data=passenger10, aes(x=reorder(Station, m), y=m))+
  geom_col()+
  coord_flip()
subway %>%
  group_by(Date) %>%
  summarise(total=sum(total_passenger)) %>%
  arrange(desc(total)) %>%
  head(3)
#1호선에서하루평균전체승객이가장많았던역과요일, 승차승객수, 하차승객수, 전체승객수를알아봅시다. 아래명령어를실행합니다.
subway %>%
  filter(Line=="1호선") %>%
  filter(total_passenger==max(total_passenger)) %>%
  select(Date, Station, on_board, getting_off, total_passenger)
#전체지하철노선에서전체승객비율이높은노선을알아봅시다. 아래명령어를실행해봅시다.
line_pct<-subway %>%
  group_by(Line) %>%
  summarise(total=sum(total_passenger)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)
#앞에서구한line_pct데이터를이용하여지하철1~9호선, 분당선의이용승객운행비율을소수점한자리까지구하고막대그래프를그려봅시다. 다음페이지의명령어를실행합니다
  line_pct10 <-line_pct%>%
  filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선","9호선","분당선" ))
ggplot(data = line_pct10, aes(x=reorder(Line,pct),y=pct))+
  geom_col()+
  coord_flip()+
  ggtitle("수도권지하철노선별이용비율")+
  xlab("노선")+
  ylab("이용비율")
#일별데이터를이용하여전체승객수를선그래프로그려봅시다. 다음페이지의명령어를실행합니다.
line_graph<-subway %>%
  group_by(day) %>%
  summarise(s=sum(total_passenger))
ggplot(data = line_graph, aes(x=day, y=s, group=1))+
  geom_line()+
  ggtitle("수도권지하철일별이용승객수")+
  xlab("일")+ylab("이용승객")


