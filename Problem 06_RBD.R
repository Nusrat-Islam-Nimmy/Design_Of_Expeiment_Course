# setwd("G:\\Other computers\\My Computer\\E\\Courses\\STAT-3203 Design of Experiment\\Lab")
setwd("E:\\Courses\\STAT-3203 Design of Experiment\\Lab")   #Setting working directory
df <- read.csv("Example 6.csv") # Read/open data file
head(df)  #Checking first 6 rows
names(df) #Checking the variable names

df$Block <- as.factor(df$Block)
df$Variety <- as.factor(df$Variety)  ## Define as Factor
str(df)  # Checking the data structure
df

######################## Analysis of Variance (ANOVA) ################################
r=4 # No. of block
k=3 # No. of Treatments
p=3 # No. of observations per cell

Grand_total <- sum(df$Yield)   #Grand Total
Grand_total

Grand_mean <- Grand_total/(r*k*p) #Grand mean
Grand_mean

Block_Variety_total <- aggregate(Yield ~ Block+Variety, df, sum)  #Block wise total
Block_Variety_total

Block_total <- aggregate(Yield ~ Block, df, sum)  #Block wise total
Block_total

Variety_total <- aggregate(Yield ~ Variety, df, sum)  #Treatment wise total
Variety_total

Total_SS <- sum(df$Yield^2)-(Grand_total^2/(r*k*p)) #Total sum of squares
Total_SS

Block_SS <- sum(Block_total$Yield^2)/(k*p)-(Grand_total^2/(r*k*p)) #Block sum of squares
Block_SS

Variety_SS <- sum(Variety_total$Yield^2)/(r*p)-(Grand_total^2/(r*k*p)) #Treatment sum of squares
Variety_SS

Total_SS_Block_Variety <- sum(Block_Variety_total$Yield^2)/p-(Grand_total^2/(r*k*p))
Total_SS_Block_Variety

Inter_SS <- Total_SS_Block_Variety-Block_SS-Variety_SS
Inter_SS

Error_SS <- Total_SS-Total_SS_Block_Variety #Error sum of squares
Error_SS

Block_MS <- Block_SS/(r-1) #Treatment mean sum of squares
Block_MS

Variety_MS <- Variety_SS/(k-1) #Treatment mean sum of squares
Variety_MS
Inter_MS <- Inter_SS/((r-1)*(k-1))
Inter_MS

Error_MS <- Error_SS/(r*k*(p-1)) #Error mean sum of squares
Error_MS

F_Cal_Variety <- Variety_MS/Error_MS  #Calculated F Value
F_Cal_Variety <- round(F_Cal_Variety, 2)
F_Cal_Variety

F_tab_Variety <- qf(p=0.05, df1=k-1, df2=r*k*(p-1), lower.tail = FALSE) #F Critical Value
F_tab_Variety <- round(F_tab_Variety, 2)
F_tab_Variety

F_Cal_Inter <- Inter_MS/Error_MS  #Calculated F Value
F_Cal_Inter <- round(F_Cal_Inter, 2)
F_Cal_Inter

F_tab_Inter <- qf(p=0.05, df1=(r-1)*(k-1), df2=r*k*(p-1), lower.tail = FALSE) #F Critical Value
F_tab_Inter <- round(F_tab_Inter, 2)
F_tab_Inter


#################### Output in a tabular form  ####################
ANOVA <- data.frame(SV=c("Block", "Variety", "Interaction", "Error"),
                    DF=c((r-1), (k-1),(r-1)*(k-1), (r*k*p-1)),
                    SS=c(Block_SS, Treatment_SS,Inter_SS, Error_SS),
                    MS=c(Block_MS,Treatment_MS, Inter_MS, Error_MS),
                    F_Cal=c(' ', F_Cal_Variety, F_Cal_Inter,' '), F_Tab=c(' ',F_tab_Variety, F_tab_Inter,' ')
                    )
ANOVA
