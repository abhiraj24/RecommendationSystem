setwd("C:/Users/User/Desktop/product_recommendation_engine")

#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)


library(data.table)
#read excel into R dataframe
DT<-fread("Cat_dog_one.csv")

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(DT,c("TransactionId","Transdate"),
                         function(df1)paste(df1$DSCS_Description,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
View(transactionData)

#set column InvoiceNo of dataframe transactionData  
transactionData$TransactionId <- NULL
#set column Date of dataframe transactionData
transactionData$Transdate <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
View(transactionData)



write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

summary(tr)


if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")


#Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.001,maxlen=10))

inspect(association.rules[(1:10)])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.00009, conf=0.001,minlen=2))
inspect(shorter.association.rules[(1:10)])


ruledf = data.frame(
  lhs = labels(lhs(shorter.association.rules)),
  rhs = labels(rhs(shorter.association.rules)), 
  shorter.association.rules@quality)

ruledf<-data.table(ruledf)

ruledf$lhs<-gsub("[{}]", "", (ruledf$lhs))

ruledf$rhs<-gsub("[{}]", "", (ruledf$rhs))
write.csv(ruledf,file = "Rules.csv")

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plotly_arules(association.rules)
plot(subRules,method="two-key plot")
top10subRules <- head(subRules, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")
saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")


# Filter top 20 rules with highest lift
subRules2<-head(association.rules, n=20, by="lift")
plot(subRules2, method="paracoord")
plotly_arules(subRules2)




