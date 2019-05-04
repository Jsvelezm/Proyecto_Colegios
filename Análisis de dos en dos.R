library("FactoMineR")
library("factoextra")
library("gplots")

data(housetasks)
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)



base <- read_excel("BF_CON_ETQ.xlsx")
#View(Autoevaluacion_6_11)
#ncol(ae611)
base = base[base$B==2,]
