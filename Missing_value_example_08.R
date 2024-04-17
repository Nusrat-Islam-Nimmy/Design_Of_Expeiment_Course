setwd("C:/Users/USER/Desktop/R_Programming/Design of Experiment")
df <- read.csv("Missing_value_example_08.csv")
df

df$Block <- as.factor(df$Block)
df$Treatment <- as.factor(df$Treatment)
str(df)

r=4
k=6
    
Block_total <- aggregate(Yield ~ Block, df, sum)
Block_total

Treatment_total <- aggregate(Yield ~ Treatment, df, sum)
Treatment_total

G <- sum(df$Yield, na.rm = TRUE) # G=Grand total of the availble Observations
G

B <- Block_total$Yield[2:2]  # B=Total of the remaining values in the affected block
B

T <- Treatment_total$Yield[2:2] # T=Total of the remaining values in the affected treatment
T

x_bar <- (r*B+k*T-G)/((r-1)*(k-1)) # x_bar= Missing value
x_bar

Total_SS <- sum(df$Yield^2, na.rm = TRUE)+x_bar^2-((G+x_bar)^2)/(r*k)
Total_SS


B1 <- Block_total$Yield[1:1]
B1
B2 <- Block_total$Yield[2:2]
B2
B3 <- Block_total$Yield[3:3]
B3
B4 <- Block_total$Yield[4:4]
B4

Block_SS <- sum(B1^2+B3^2+B4^2+(B2+x_bar)^2)/6-((G+x_bar)^2)/(r*k)
Block_SS


T1 <- Treatment_total$Yield[1:1]
T1
T2 <- Treatment_total$Yield[2:2]
T2
T3 <- Treatment_total$Yield[3:3]
T3
T4 <- Treatment_total$Yield[4:4]
T4
T5 <- Treatment_total$Yield[5:5]
T5
T6 <- Treatment_total$Yield[6:6]
T6

Treatment_SS <- sum(T1^2+T3^2+T4^2+T5^2+T6^2+(T2+x_bar)^2)/4-((G+x_bar)^2)/(r*k)
Treatment_SS

Error_SS <- (Total_SS-Block_SS-Treatment_SS)
Error_SS

Block_MS <- Block_SS/(r-1) 
Block_MS

Treatment_MS <- Treatment_SS/(k-1) 
Treatment_MS

Error_MS <- Error_SS/((r-1)*(k-1))-1
Error_MS

F_Cal_Treat <- Treatment_MS/Error_MS
F_Cal_Treat <- round(F_Cal_Treat, 2)
F_Cal_Treat

F_tab <- qf(p=0.05, df1=k-1, df2=((r-1)*(k-1))-1, lower.tail = FALSE) 
F_tab <- round(F_tab, 2)
F_tab


##ANOVA Table##
ANOVA <- data.frame(SV=c("Block", "Treatment", "Error"),
                  DF=c((r-1), (k-1),((r-1)*(k-1))-1),
                    SS=c(Block_SS, Treatment_SS, Error_SS),
                    MS=c(Block_MS,Treatment_MS, Error_MS),
                    F_Cal=c('--', F_Cal_Treat, '--'), F_Tab=c('--',F_tab,'--')
)
ANOVA

