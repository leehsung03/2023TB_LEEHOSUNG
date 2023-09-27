#과제2
table(X2023_STB_survey $Gender)
#과제3
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)
#과제4
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
#과제5
barplot(table(X2023_STB_survey $Nationality))
#과제6
barplot(table(X2023_STB_survey $'residential area'),col=pal1, xlab= "residential area", ylab= "number", xlim=c(0,100), horiz=TRUE)
#과제7
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)
#과제8
pie(table(X2023_STB_survey $Grade))
#과제9
hist(X2023_STB_survey$`Age`, main="histogram", col=terrain.colors(12))
#과제10
boxplot(X2023_STB_survey$`Grade`, X2023_STB_survey$`Age`, main="histogram2", col="yellow", names = c("Grade","Age"))
summary(X2023_STB_survey, na.rm=T)
Gender         Age            Grade        Nationality   
Min.   :1.0   Min.   :19.00   Min.   :2.000   Min.   :3.000  
1st Qu.:1.0   1st Qu.:21.00   1st Qu.:2.000   1st Qu.:3.000  
Median :1.5   Median :22.00   Median :3.000   Median :3.000  
Mean   :1.5   Mean   :22.52   Mean   :3.125   Mean   :3.438  
3rd Qu.:2.0   3rd Qu.:23.25   3rd Qu.:4.000   3rd Qu.:4.000  
Max.   :2.0   Max.   :27.00   Max.   :4.000   Max.   :5.000  
residential area
Min.   : 6.000  
1st Qu.: 7.000  
Median : 7.000  
Mean   : 7.208  
3rd Qu.: 7.000  
Max.   :11.000  
#과제11
plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="Grade value", ylab="Age value", main="survey")