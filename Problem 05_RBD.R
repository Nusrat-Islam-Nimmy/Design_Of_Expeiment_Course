# setwd("G:\\Other computers\\My Computer\\E\\Courses\\STAT-3203 Design of Experiment\\Lab")
setwd("E:\\Courses\\STAT-3203 Design of Experiment\\Lab")   #Setting working directory
df <- read.csv("Example 5.csv") # Read/open data file
head(df)  #Checking first 6 rows
names(df) #Checking the variable names

df$Block <- as.factor(df$Block)
df$Treatment <- as.factor(df$Treatment)  ## Define as Factor
str(df)  # Checking the data structure


######################## Analysis of Variance (ANOVA) ################################
r=5 # No. of block
k=5 #No. of Treatments

Grand_total <- sum(df$Yield)   #Grand Total
Grand_total

Grand_mean <- Grand_total/(r*k) #Grand mean
Grand_mean

Block_total <- aggregate(Yield ~ Block, df, sum)  #Block wise total
Block_total

Treatment_total <- aggregate(Yield ~ Treatment, df, sum)  #Treatment wise total
Treatment_total

Total_SS <- sum(df$Yield^2)-(Grand_total^2/(r*k)) #Total sum of squares
Total_SS

Block_SS <- sum(Block_total$Yield^2)/k-(Grand_total^2/(r*k)) #Treatment sum of squares
Block_SS

Treatment_SS <- sum(Treatment_total$Yield^2)/r-(Grand_total^2/(r*k)) #Treatment sum of squares
Treatment_SS

Error_SS <- Total_SS-Block_SS-Treatment_SS #Error sum of squares
Error_SS

Block_MS <- Block_SS/(r-1) #Treatment mean sum of squares
Block_MS

Treatment_MS <- Treatment_SS/(k-1) #Treatment mean sum of squares
Treatment_MS

Error_MS <- Error_SS/((r-1)*(k-1)) #Error mean sum of squares
Error_MS

F_Cal_Treat <- Treatment_MS/Error_MS  #Calculated F Value
F_Cal_Treat <- round(F_Cal_Treat, 2)
F_Cal_Treat

F_tab <- qf(p=0.05, df1=k-1, df2=(r-1)*(k-1), lower.tail = FALSE) #F Critical Value
F_tab <- round(F_tab, 2)
F_tab
#################### Output in a tabular form  ####################
ANOVA <- data.frame(SV=c("Block", "Treatment", "Error"),
                    DF=c((r-1), (k-1),(r-1)*(k-1)),
                    SS=c(Block_SS, Treatment_SS, Error_SS),
                    MS=c(Block_MS,Treatment_MS, Error_MS),
                    F_Cal=c('--', F_Cal_Treat, '--'), F_Tab=c('--',F_tab,'--')
                    )
ANOVA
