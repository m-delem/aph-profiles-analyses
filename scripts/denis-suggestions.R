occ = c("No answer", "Unemployed", "Student", "Science and Engineering", 
        "Health", "Business, Administration", "Information, Communications", 
        "Social, Cultural, Legal", "Teaching")
clA = c(1, 1, 12, 2, 1, 7, 4, 2, 0.1)
clB = c(1, 0.1, 9, 2, 1, 3, 9, 3, 5)
clC = c(0.1, 1, 11, 2, 6, 9, 1, 1, 2)
# je mets des 0.1 au lieu des zéro pour avoir l'effet d'interaction.

df = data.frame(
  Occupation = rep(occ, 3),
  Cluster    = unlist(lapply(c("A","B","C"), rep, 9)),
  freq       = c(clA, clB, clC)
)


library(ggplot2)
library(ANOFA)


w <- anofa(freq ~ Occupation + Cluster, df)
summarize(w)

anofaPlot(w, 
    confidenceLevel=.66, ## càd erreur type
    errorbarParams=list(color="gray",alpha=.5) 
) + theme_bw() + 
theme(axis.text.x = element_text(angle = 45, vjust = 1., hjust=1))


#########################
#Correlations (Figure 2)
#########################

corrtime<-subset(start,select=c(T1selfcrit,T1rigid,T1ansen,T1asi,T1sanx,T2sanx,T1satt,T2satt,gradec))

names(corrtime)<-c("Self-Critical Perfectionism","Rigid Perfectionism","Anxiety Sensitivity","Statistics Anxiety Sensitivity",
                   "Statistics Anxiety Time 1","Statistics Anxiety Time 2","Statistics Attitudes Time 1","Statistics Attitudes Time 2",
                   "Grades")

correl<-cor(corrtime,use = "pairwise.complete.obs")

#To add in the * values for the figure so they look pretty, a manual change to the corrplot function is necessary
#replace line 445-448 in the trace  with the following function  ((c'est juste le +.25 qui change!))
#place_points = function(sig.locs, point) {
#text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#labels = point, col = pch.col, cex = pch.cex, 
#lwd = 2)
#trace(corrplot,edit=TRUE)

res1<-cor.mtest(corrtime,conf.level=.95)
corrplot(
  correl,
  method="color",
  type="upper", 
  addCoef.col="black", 
  tl.col="black",
  p.mat = res1$p,
  diag=FALSE, 
  tl.srt=70,
  insig = "label_sig",
  sig.level = c(0.001,0.01,.05), 
  pch.cex = .8
)