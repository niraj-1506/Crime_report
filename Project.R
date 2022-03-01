#Read data from csv file
library(ggplot2)
library(data.table) 
library(ggrepel)
library(scales)
set.seed(42)
print(getwd())
setwd("/Users/nr/Desktop/Maths")
data <- read.csv("Cases_Info.csv", header=TRUE, fileEncoding="UTF-8-BOM")
data
print("No. of Variables")
print(ncol(data)) #prints number of columns
print("No. of observations")
print(nrow(data)) #prints number of rows

#replacing NA values with 0
data[is.na(data)] <- 0
data

#summary of crimes in India
summary(data$Total.Cases.Pending.Trial)


#Total Crimes with 100% pending rate
Crimes100 <- data[which(data$Pendency.Percentage == 100),c("Crime.Head","Total.Cases.Pending.Trial")]
Crimes100
help("barplot")
help("ggplot")
ggplot(Crimes100, aes(Crime.Head, Total.Cases.Pending.Trial, fill = Crime.Head)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=Total.Cases.Pending.Trial), vjust = -0.3, size=3.5)+
  labs(title = "Cases with 100% pendancy rate")+
  theme(axis.text.x = element_text(angle = 90))

#Crimes with 100% conviction rate
Conviction100 <- data[which(data$Conviction.Rate == 100),c("Crime.Head","Total.Cases.Pending.Trial")]
Conviction100
ggplot(Conviction100, aes(Crime.Head, Total.Cases.Pending.Trial, fill = Crime.Head)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=Total.Cases.Pending.Trial), vjust = -0.3, size=3.5)+
  labs(title = "Cases with 100% Conviction Rate")+
  theme(axis.text.x = element_text(angle = 90))

#Total Murder Cases
Murder <- data[data$Crime.Head %like% "Murder",]
Murder
ggplot(Murder, aes(Crime.Head, Total.Cases.Pending.Trial, fill = Crime.Head)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=Total.Cases.Pending.Trial), vjust = -0.3, size=3.5)+
  labs(title = "Different Murder Cases")+
  theme(axis.text.x = element_text(angle = 90))

Murder_con_pred = data.frame(Crimes = c(Murder$Crime.Head,Murder$Crime.Head),
                     Category = c("Conviction.Rate", "Conviction.Rate", 
                                  "Conviction.Rate", "Conviction.Rate",
                                  "Conviction.Rate", "Pendency.Percentage",
                                  "Pendency.Percentage", "Pendency.Percentage",
                                  "Pendency.Percentage", "Pendency.Percentage"),
                     Percentage = c(Murder$Conviction.Rate[1],Murder$Conviction.Rate[2],
                                     Murder$Conviction.Rate[3],Murder$Conviction.Rate[4],
                                     Murder$Conviction.Rate[5],Murder$Pendency.Percentage[1],
                                     Murder$Pendency.Percentage[2],Murder$Pendency.Percentage[3],
                                     Murder$Pendency.Percentage[4],Murder$Pendency.Percentage[5])
                     )

ggplot(Murder_con_pred, aes(factor(Crimes), Percentage, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Murder Conviction VS Pending")+
  theme(axis.text.x = element_text(angle = 90))

piecol = colnames(data)[!(colnames(data) %in% c("Si..No.","Crime.Head","Cases.Registered",
                                       "Cases.Pending.Trial.from.the.Previous.Year",
                                       "Total.Cases.Pending.Trial",
                                       "Cases.Disposed.off.without.Trial",
                                       "Cases.Convicted","Cases.which.Trials.were.Completed"              
                                       ,"Cases.Disposed.off.by.Courts"
                                       ,"Conviction.Rate"                                
                                       ,"Pendency.Percentage",
                                       "Cases.Pending.Trial.at.End.of.the.Year"))]
n = c()
n[1] = sum(data$Cases.Abated.by.Court)
n[2] = sum(data$Cases.Withdrawn.from.Prosecution)
n[3] = sum(data$Cases.Compounded.or.Compromised)
n[4] = sum(data$Cases.Disposed.off.by.Plea.Bargaining)
n[5] = sum(data$Cases.Quashed)
n[6] = sum(data$Cases.Stayed.or.Sent.to.Record.Room)
n[7] = sum(data$Cases.Convicted.Out.of.Cases.from.Previous.Year)
n[8] = sum(data$Cases.Convicted.Out.of.Cases.during.the.Year)
n[9] = sum(data$Cases.Discharged)
n[10] = sum(data$Cases.Acquitted)
n
options(ggrepel.max.overlaps = Inf)
piedata <- data.frame(CasesHandled = piecol, TotalCasespie = n)
pie_labels <- paste0(round(100 * n/sum(n), 2), "%")
ggplot(piedata, aes(x="", y=TotalCasespie, fill=CasesHandled)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text_repel(aes( label = pie_labels), position = position_stack(vjust = 0.5)) + 
  labs(title = "Cases handled Each Way")+
  theme_void()

#Crimes Against Women
AgainstWomen <- data[data$Crime.Head %like% "Women",c("Crime.Head", "Total.Cases.Pending.Trial")] 
AgainstWomen
AgainstWomen <- aggregate(Total.Cases.Pending.Trial~(Crime.Head),AgainstWomen,sum)
ggplot(AgainstWomen, aes(Crime.Head, Total.Cases.Pending.Trial, fill = Crime.Head)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crimes Against Women")+
  geom_text(aes(label=Total.Cases.Pending.Trial), vjust = -0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90))

