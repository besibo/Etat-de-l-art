####################################
##  Graphique coûts des séquençages
####################################

getwd()
setwd("/Users/pierre-louisstenger/Documents/Cours fac/Master/S3/Stage M2/R/Moore")
# Importation des données
cost <- read.table('costseq.txt',header=T, dec=",")
cost

library(ggplot2)

gg <- ggplot(data = cost, aes(x=Annees, y=Couts)) + geom_point() + geom_smooth(method="loess")
gg <- gg + xlab("Années") + ylab("Coût") + ggtitle("Blabla")
gg

# ===============
# = Moore's law =
# ===============

# Get costs according to Moore's law and the real cost in 2000
# The cost should be divided by 2 every 2 years
y <- cost$Couts[1]/(2^(0:6))
x <- seq(2000, 2012, by=2)

# Compute the missing values (i.e. values for years 2001, 2003, 2005, etc)
out <- lm(log(y)~x)
y <- exp(predict(out, data.frame(x=2000:2015)))
x <- 2000:2015

moore <- data.frame(y,x)
ggplot(data = moore, aes(x=x, y=y)) + geom_line() + geom_point()

# =========================================
# = plotting both curve on the same graph =
# =========================================

all <- data.frame(Cost = c(cost$Couts,moore$y), Year = rep(2000:2015, 2), Source = rep(c("Observé","Loi de Moore"), each=16))
all


gg2 <- ggplot(data = all, aes(x=Year, y=Cost, colour=Source)) + geom_point() + geom_line() + geom_smooth(data=all[1:16], )
gg2 <- gg2 + xlab("Year") + ylab("Cost") + ggtitle("Blabla")
gg2

gg3 <- ggplot(data = all, aes(x=Year, y=Cost, colour=Source))
gg3

gg4 <- ggplot(data = all, aes(x=Year, y=Cost, colour=Source)) + geom_line() + scale_y_log10()
gg4 <- gg4 + xlab("Années") + ylab("Coût") + ggtitle("Évolution des coûts des séquenceurs entre 2000 et 2015 comparée à la loi de Moore")
gg4

Sweave("Etat de l'art.rnw")



telecharger R deamon


