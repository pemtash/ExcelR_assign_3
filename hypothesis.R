############################   1. Cutlets data #############################

cutlet <- read.csv('C:\\Users\\user\\Desktop\\EXCELR ASSIGNMENT\\ASSIGNMENT\\assignment3 hypothesis\\Cutlets.csv')
cutlet
View(cutlet)
attach(cutlet)
shapiro.test(Unit.A)
shapiro.test(Unit.B)
# f test to compare two variances
var.test(Unit.A,Unit.B)


############################  2.labTaT  (ANOVA)   ###############################

lab <- read.csv("C:\\Users\\user\\Desktop\\EXCELR ASSIGNMENT\\ASSIGNMENT\\assignment3 hypothesis\\LabTAT.csv")
View(lab)
attach(lab)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)
stacked_data = stack(lab)
View(stacked_data)
attach(stacked_data)
bartlett.test(values~ind, data=stacked_data) #Bartltt test for homogeneity of variances
anova_results <- aov(values~ind,data = stacked_data)
summary(anova_results)
#since p-value =  < 0.05 reject null hypothesis 
#Alternate hypothesis: means are not equal


############################  3.buyer ratio (chi-square test)   ###############################

buyer_ratio <- read.csv("C:\\Users\\user\\Desktop\\EXCELR ASSIGNMENT\\ASSIGNMENT\\assignment3 hypothesis\\BuyerRatio.csv")
View(buyer_ratio)
attach(buyer_ratio)
str(buyer_ratio)
table1 <- table(Drinks,Person)
buyer_ratio=data.frame(lapply(buyer_ratio, as.character), stringsAsFactors=FALSE)
Stacked_Data <- stack(buyer_ratio,select=c(East=East,West=West,North=North,South=South ))
attach(Stacked_Data)
chisq.test(ind, values)


############################  4.CustomerOrderForm.mtw   ###############################
library(readxl)
customer_order <- read.csv("C:\\Users\\user\\Desktop\\EXCELR ASSIGNMENT\\ASSIGNMENT\\assignment3 hypothesis\\Costomer+OrderForm.csv")
View(customer_order)
attach(customer_order)
is.vector(Phillippines)
customer_order <- data.frame(lapply(customer_order, as.character), stringsAsFactors=FALSE)
str(customer_order)
table('Error free','Defective')
stack(d, select = c(x1,x2))
Stacked_Data <- stack(customer_order,select=c(Phillippines=Phillippines,Indonesia=Indonesia,Malta=Malta,India=India ))
View(Stacked_Data)
attach(Stacked_Data)
t2 <- prop.table(table(Defective))
t1 <- table(Country)
chisq.test(ind, values)
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 
