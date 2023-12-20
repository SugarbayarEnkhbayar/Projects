# https://archive.ics.uci.edu/ml/datasets/student+performance

# https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce
# https://medium.com/analytics-vidhya/create-association-rules-for-the-market-basket-analysis-for-the-given-threshold-using-r-and-45af41400e1
# http://rstudio-pubs-static.s3.amazonaws.com/408101_434bb6c131f8413d8f22c64b8efb7036.html
# https://rpubs.com/askovron/mba-associationrules
# https://rpubs.com/airam/usl_p3
# https://rpubs.com/kkrynska/AssociationRules

# https://rpubs.com/esobolewska/chords
# https://rpubs.com/honkalimonka/UL3
# 
# Packages
library(readr)
library(tidyverse)
library(arules)
library(arulesViz)
# Data 

#for (i in 1:31) {
#  df[,i]<-factor(df[,i],levels = unique(df[,i]),labels = unique(df[,i]))
#}
#write.csv(df,"df.csv")
setwd('C:/Users/User/Desktop/Unsupervised Projects-Sugarbayar/3-IDK')
df=read.csv("df.csv")
for (i in 2:32) {
  x=colnames(df)[i]
  df[,i]<-paste0(x,'-',df[,i])
}
df=df %>% select(-age,-absences,-X)
write.csv(df,"df1.csv",row.names = F)
trans=read.transactions("df1.csv",format='basket',sep=",",skip=0)
summary(trans)
length(trans)
trans
inspect(head(trans))
size(trans)
length(trans)
# cleaning the data from rare observations
trans1=trans[,itemFrequency(trans)>0.05]
trans1
# we can get all levels in dataset and their frequency
sort(itemFrequency(trans1, type="relative"))  
itemFrequencyPlot(trans1,type='relative',topN=15)

# what is the profile of students who are passed final exam 'final-success'?
rules<-apriori(data=trans1, parameter=list(supp=0.1,conf = 0.5), 
                       appearance=list(default="lhs", rhs='final-success'), control=list(verbose=F))
summary(rules) # see how many rules and what are the parameters of support and confidence
#rules.clean<-rules[!is.redundant(rules)]
#rules.clean<-rules.clean[is.significant(rules.clean, trans1)]
#rules.clean<-rules.clean[is.maximal(rules.clean)]
rules.clean<-rules
rules.final<-sort(rules.clean, by="support", decreasing=TRUE)
summary(rules.final)
rules.final<-sort(rules.final, by="support", decreasing=TRUE)
inspect(head(rules.final,10))
#Support defines the frequency of features combination in dataset – it is obvious that shorter phrases appear more often than longer ones. 
#Coverage (also cover, LHS-support) is the support of the left-hand-side of the rule. Confidence – when 1 (100%) it means that for sure (100%) if we see somebody very happy in Sommerville, 
#he/she estimates schools as good, takes own decisions, thinks that police always help, is similar to others, can easily have affordable house and sees the city as beautiful 
#(remember that this relation does not have to be opposite). When lift > 1 we see features appearing together more often than separately (lift defines how many times more often). 
plot(head(rules.final,10), method="graph", engine="htmlwidget") # 
aff.items<-dissimilarity(trans1, which="items", method="affinity")
hc<-hclust(aff.items, method="ward.D2")
plot(hc, main="Dendrogram for Items") 
aff.rules<-dissimilarity(rules.final, method="affinity", args=list(transactions=trans1))
hc<-hclust(aff.rules, method="ward.D2")
plot(hc, main="Dendrogram for Rules (Affinity)") 

rules<-apriori(data=trans1, parameter=list(supp=0.1,conf = 0.5), 
               appearance=list(default="lhs", rhs='final-fail'), control=list(verbose=F))
summary(rules) # see how many rules and what are the parameters of support and confidence
#rules.clean<-rules[!is.redundant(rules)]
#rules.clean<-rules.clean[is.significant(rules.clean, trans1)]
#rules.clean<-rules.clean[is.maximal(rules.clean)]
rules.clean<-rules
rules.final<-sort(rules.clean, by="support", decreasing=TRUE)
summary(rules.final)
rules.final<-sort(rules.final, by="support", decreasing=TRUE)
inspect(head(rules.final,10))
#Support defines the frequency of features combination in dataset – it is obvious that shorter phrases appear more often than longer ones. 
#Coverage (also cover, LHS-support) is the support of the left-hand-side of the rule. Confidence – when 1 (100%) it means that for sure (100%) if we see somebody very happy in Sommerville, 
#he/she estimates schools as good, takes own decisions, thinks that police always help, is similar to others, can easily have affordable house and sees the city as beautiful 
#(remember that this relation does not have to be opposite). When lift > 1 we see features appearing together more often than separately (lift defines how many times more often). 
plot(head(rules.final,10), method="graph", engine="htmlwidget") # 
aff.items<-dissimilarity(trans1, which="items", method="affinity")
hc<-hclust(aff.items, method="ward.D2")
plot(hc, main="Dendrogram for Items") 
aff.rules<-dissimilarity(rules.final, method="affinity", args=list(transactions=trans1))
hc<-hclust(aff.rules, method="ward.D2")
plot(hc, main="Dendrogram for Rules (Affinity)") 


###########
library(arulesCBA)
trans2=df
for (i in 1:29) {
  trans2[,i]<-factor(trans2[,i],levels = unique(trans2[,i]))
}
trans21=trans2 %>% filter(final=='final-success')
trans22=trans2 %>% filter(final=='final-fail')
data.disc=discretizeDF.supervised(data=trans21,final~.,methods = 'chi2')
summary(data.disc)
data.trans<-transactions(data.disc)
trans2<-data.trans[, itemFrequency(data.trans)>0.05]
data.ass<-mineCARs(final~ ., transactions=trans2, support=0.04, confidence=0.7)
data.ass=sort(data.ass,by='support',decreasing = TRUE)
summary(data.ass)
inspect(head(data.ass,20))

















