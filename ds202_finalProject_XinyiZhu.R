
# Xinyi Zhu- question 5&6
library(plyr)
library(ggplot2)

 # How are SAT scores distributed and affect the enrollment rate of college students?
data <- readxl::read_xlsx("/Users/jessie/Desktop/ds202/ds202_finalProject/IPEDS_data.xlsx")
summary(data)
library(plyr)
df <- rename(data, c("Percent of freshmen submitting SAT scores"="%freshmanSubmittedSAT", 
                     "SAT Critical Reading 25th percentile score"="25thSAT_CR",
               "SAT Critical Reading 75th percentile score" ="75thSAT_CR", 
               "SAT Math 25th percentile score" ="25thSAT_M",
               "SAT Math 75th percentile score" ="75thSAT_M", 
               "SAT Writing 25th percentile score" ="25thSAT_W",
               "SAT Writing 75th percentile score" ="75thSAT_W", 
               "Estimated enrollment, full time"= "Enroll_ft"))


colSums(is.na(data))
data[rowSums(is.na(data)) == 0,]
View(df)
summary(df$`25thSAT_CR`)
summary(df$`%freshmanSubmittedSAT`)
boxplot(df$`%freshmanSubmittedSAT`) + title("Percentage of freshman submitted SAT for each school")
ggplot(df, aes(x=`%freshmanSubmittedSAT`)) + 
  geom_histogram(color ='black', fill = 'purple') + 
  ggtitle("Percentage of freshman submitted SAT for each school")

qqnorm(df$`25thSAT_CR`)
ggplot(df, aes(x=`25thSAT_CR`)) + 
  geom_histogram(color ='black', fill = 'purple') + 
  ggtitle("SAT Critical Reading") +xlab('SAT Critical Reading')

qqnorm(df$`25thSAT_M`)
ggplot(df, aes(x=`25thSAT_M`)) + 
  geom_histogram(color ='black', fill = 'purple') + 
  ggtitle("SAT Math") +xlab('SAT Math')

df$Enroll_ft
ggplot(df, aes(x=`25thSAT_CR`, y=Enroll_ft)) + geom_point() + ggtitle('SAT critical reading VS Enrollment')+
  xlab('SAT critical reading') + ylab('Enrollment')


ggplot(df, aes(x=`25thSAT_M`, y=Enroll_ft)) + geom_point() + ggtitle('SAT Math VS Enrollment')+
  xlab('SAT Math') + ylab('Enrollment')

ggplot(df, aes(x=`25thSAT_W`, y=Enroll_ft)) + geom_point() + ggtitle('SAT Writing VS Enrollment')+
  xlab('SAT Writing') + ylab('Enrollment')


ggplot(df, aes(x=`Estimated undergraduate enrollment, full time`/`Estimated undergraduate enrollment, total`)) +
  geom_histogram(color ='black', fill = 'purple') + ggtitle('Pecentage of Student Accept an Offer')+
  xlab('full time undergraduate enrollment') 


