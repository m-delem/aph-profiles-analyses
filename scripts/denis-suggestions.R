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