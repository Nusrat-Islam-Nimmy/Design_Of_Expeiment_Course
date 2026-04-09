setwd("C:\\Users\\USER\\Desktop\\R_Programming\\Design of Experiment")
df <- read.csv("Data_RBD5.csv")
tail(df)
head(df)


r=5
k=5

df$Treatment <- as.factor(df$Treatment)
str(df)

Grand_Total <- sum(df$Yeild)
Grand_Total
Grand_mean <- Grand_Total/(r*k)
Grand_mean
Block_Total <- aggregate(Yeild~Block,df,sum)
Block_Total
Treatment_Total <- aggregate(Yeild~Treatment,df,sum)
Treatment_Total
Total_SS <- sum(df$Yeild^2)-(Grand_Total^2)/(r*k)
Total_SS
Block_SS <- sum(Block_Total$Yeild^2)/5-(Grand_Total^2)/(r*k)
Block_SS
Treatment_SS <- sum(Treatment_Total$Yeild^2)/5-(Grand_Total^2)/(r*k)
Treatment_SS
Error_SS <- (Total_SS-Block_SS-Treatment_SS)
Error_SS

Block_MS <- Block_SS/(r-1)
Block_MS

Treatment_MS <- Treatment_SS/(k-1)
Treatment_MS
Error_MS <- Error_SS/((r-1)*(k-1))
Error_MS
F_Cal <- Block_MS/Error_MS
F_Cal <- round(F_Cal,3)
F_Cal
F_Tab <- qf(p=0.05,df1=r-1,df2=k-1,lower.tail = FALSE)
F_Tab <- round(F_Tab,3)
F_Tab

F_Cal <- Treatment_MS/Error_MS
F_Cal <- round(F_Cal,3)
F_Cal
F_Tab <- qf(p=0.05,df1=r-1,df2=(r-1)*(k-1),lower.tail = FALSE)
F_Tab <- round(F_Tab,3)
F_Tab





            