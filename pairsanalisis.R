library("FactoMineR")
library("factoextra")
library("gplots")
library(readxl)
library(questionr)



vv=matrix(c(0,0,0,0),ncol=2)
base <- read_excel("BF_CON_ETQ.xlsx")
#View(Autoevaluacion_6_11)
#ncol(ae611)
base = base[base$B==2,]
clo=c( "C","SEXO" , "B",  "P2",       "P3" ,      "P4",       "P5",       "P6"      ,  "P8",       "P9" ,      "P10",      "P11",      "P12",     "P13" ,"P15",      "P16",      "P17"  ,   "P18",      "P19", "P21"  ,    "P22"  ,   "P23",      "P24",      "P25","P27",      "P28"  ,    "P29"  ,    "P30"  ,    "P31"  )

[1:round(length(clo)/2)]
for(n1 in clo){
  for(n2 in clo){
if(n1 != n2){
    try({
    var=c(n1,n2)
t1 <-(table(base[var]))




dt <- as.table(cbind(table(base[var]),rowSums(table(base[var]))))
dt <- as.table(rbind(dt,colSums(dt)))
a=length(colnames(dt))-1
b=length(rownames(dt))-1
colnames(dt) <- c( colnames(dt)[1:a],"Total Colunmas")
rownames(dt)<- c( rownames(dt)[1:b],"Total Filas")

# 2. Graph

chisq <- chisq.test(t1)
if (cramer.v(t1)
 > 0.2)
{print(c(n1,n2))}
vv=rbind(vv,t(c(n1,n2)))
    }, T )}

}}

[1] "P30" "P11"
[1] "P30" "P21"
[1] "P30" "P27"
[1] "P30" "P28"


dt <- table(vv[3:nrow(vv),1], vv[3:nrow(vv),2])
balloonplot(t(dt), main ="Cruce de variables", xlab =paste(var[2]), ylab=var[1],
            label = T,label.size=0.8 ,show.margins = FALSE,cum.margins=TRUE)


#warnings()
var=c("SEXO","C")
t1 <-(table(base[var]))
t2 <- t1




dt <- as.table(cbind(table(base[var]),rowSums(table(base[var]))))
dt <- as.table(rbind(dt,colSums(dt)))
a=length(colnames(dt))-1
b=length(rownames(dt))-1
colnames(dt) <- c( colnames(dt)[1:a],"Total Colunmas")
rownames(dt)<- c( rownames(dt)[1:b],"Total Filas")

# 2. Graph
balloonplot(t(dt), main ="Cruce de variables", xlab =paste(var[2]), ylab=var[1],
            label = T,label.size=0.8 ,show.margins = FALSE,cum.margins=TRUE)



