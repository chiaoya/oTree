rm(list = ls())

# write.csv(XP, file = "XP.csv")
# write.csv(ST, file = "ST.csv")

XP <- read.csv("XP.csv", sep = ",")
ST <- read.csv("ST.csv", sep = ",")

ST$X <- NULL
STagg = aggregate(ST$ST, 
                  by= list(ST$Y, ST$XX),
                  FUN = sum)
STagg <- STagg[STagg$Group.2 == "EG" | STagg$Group.2 == "MOL" | STagg$Group.2 == "RT",]
XP$X <- NULL

names(STagg)[names(STagg) == "Group.1"] <- "Y"
names(STagg)[names(STagg) == "Group.2"] <- "XX"
names(STagg)[names(STagg) == "x"] <- "ST"

DT <- merge(XP, STagg, by = c("XX", "Y"))
# list levels of factor v1 in DT
levels(DT$XX)
levels(DT$Reg)

# show elements and count elements in the vector
Reglist <- unique(DT$Reg)
XXlist <- unique(DT$XX)

DT$L_XP = c(NA,DT$XP[-length(DT$XP)]) # create a new var with data displaced by 1 unit
DT$L_ST = c(NA,DT$ST[-length(DT$ST)]) # create a new var with data displaced by 1 unit

DT$LnXP = log(DT$XP) # create log PP
DT$LnST = log(DT$ST) # create log ST

DT$L_LnXP = c(NA,DT$LnXP[-length(DT$LnXP)]) # create a new var with data displaced by 1 unit
DT$L_LnST = c(NA,DT$LnST[-length(DT$LnST)]) # create a new var with data displaced by 1 unit

DT0 <- DT[!DT$Y == "1990",] # eliminate Y=1990
#DT0 <- DT0[!DT0$Y > "2017",] # eliminate Y>2017, only activate when needed

DT0$rXP <- DT0$XP/DT0$L_XP
DT0$rST <- DT0$ST/DT0$L_ST

DT0$dLnXP <- DT0$LnXP-DT0$L_LnXP
DT0$dLnST <- DT0$LnST-DT0$L_LnST

DT1 <- DT0[, -c(3:13)]

library(ggplot2)
library(DataCombine)

# === MOL ===
MOL <- DT1[DT1$XX == "MOL",]
MOL <- MOL[!MOL$Y > "2017",]

m_MOL <- lm(dLnST ~ dLnXP, data = MOL)
summary(m_MOL)

# === RT ===
RT <- DT1[DT1$XX == "RT",]
RT <- slide(RT, Var = "dLnXP", slideBy = 1)
RT <- RT[!RT$Y > "2017",] # eliminate Y=2030
m_RT <- lm(dLnST ~ dLnXP1, data = RT)
summary(m_RT)

m_MOL
m_RT

# === EG ===
EG <- ST[ST$XX == "EG" ,]
EG$Weight <- EG %>% 
  group_by(Y, Reg) %>% 
  summarise(value = sum(ST))
EG

