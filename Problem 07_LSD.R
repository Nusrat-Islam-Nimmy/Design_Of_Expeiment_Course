setwd("G:\\Other computers\\My Computer\\E\\Courses\\STAT-3203 Design of Experiment\\Lab")
# setwd("E:\\Courses\\STAT-3203 Design of Experiment\\Lab")   #Setting working directory
df <- read.csv("Example 7.csv") # Read/open data file
head(df)  #Checking first 6 rows
names(df) #Checking the variable names


df$Row <- as.factor(df$Row)
df$Column <- as.factor(df$Column)
df$Fertilizer <- as.factor(df$Fertilizer)  ## Define as Factor
str(df)  # Checking the data structure


######################## Analysis of Variance (ANOVA) ################################
r=5 # No. of row/column/treatment

Grand_total <- sum(df$Yield)   #Grand Total
Grand_total

Grand_mean <- Grand_total/(r^2) #Grand mean
Grand_mean

Row_total <- aggregate(Yield ~ Row, df, sum)  #Row wise total
Row_total

Column_total <- aggregate(Yield ~ Column, df, sum)  #Column wise total
Column_total

Fertilizer_total <- aggregate(Yield ~ Fertilizer, df, sum)  #Treatment wise total
Fertilizer_total

Total_SS <- sum(df$Yield^2)-(Grand_total^2/(r^2)) #Total sum of squares
Total_SS

Row_SS <- sum(Row_total$Yield^2)/r-(Grand_total^2/(r^2)) #Treatment sum of squares
Row_SS

Column_SS <- sum(Column_total$Yield^2)/r-(Grand_total^2/(r^2)) #Treatment sum of squares
Column_SS

Fertilizer_SS <- sum(Fertilizer_total$Yield^2)/r-(Grand_total^2/(r^2)) #Treatment sum of squares
Fertilizer_SS

Error_SS <- Total_SS-Row_SS-Column_SS-Fertilizer_SS #Error sum of squares
Error_SS

Row_MS <- Row_SS/(r-1) #Row mean sum of squares
Row_MS

Column_MS <- Column_SS/(r-1) #Treatment mean sum of squares
Column_MS

Fertilizer_MS <- Fertilizer_SS/(r-1) #Treatment mean sum of squares
Fertilizer_MS

Error_MS <- Error_SS/((r-1)*(r-2)) #Error mean sum of squares
Error_MS

F_Cal_Fertilizer <- Fertilizer_MS/Error_MS  #Calculated F Value
F_Cal_Fertilizer

F_tab <- qf(p=0.05, df1=r-1, df2=(r-1)*(r-2), lower.tail = FALSE) #F Critical Value
F_tab
#################### Output in a tabular form  ####################
ANOVA <- data.frame(SV=c("Row", "Column","Fertilizer", "Error"),
                    DF=c((r-1), (r-1),(r-1),(r-1)*(r-2)),
                    SS=round(c(Row_SS, Column_SS, Fertilizer_SS, Error_SS),2),
                    MS=round(c(Row_MS, Column_MS, Fertilizer_MS, Error_MS),2),
                    F_Cal=c(' ', ' ', round(F_Cal_Fertilizer,2), ' '), F_Tab=c(' ',' ',round(F_tab,2),' ')
                    )
ANOVA
