### Installing necessary packages for all analyses
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("mirt")
install.packages("TAM")
install.packages("PerFit")

library(openxlsx)
library(tidyverse)
library(ggplot2) 
library(ggpubr)
library(mirt)
library(TAM)
library(PerFit)


##########################################################################
######################   1. Data Management    ###########################
##########################################################################
setwd("C:/Users/wenzhong/Documents/Thesis/R") # set working directory
getwd() # get working directory

#load the original data
READ <- read.xlsx("Test Data May 2021.xlsx", sheet = "Reading")
LISTENING <- read.xlsx("Test Data May 2021.xlsx", sheet = "Listening")

#save the data into R format
saveRDS(READ, file = "READ.Rds")
saveRDS(LISTENING, file = "LISTENING.Rds")

#load the data from R format
READ = readRDS(file = "READ.Rds")#in thesis project
LISTENING = readRDS(file = "LISTENING.Rds")

######################       1.1 Response data       ######################
#reading test
#item responses in each module
READ_v1 <- READ[,c(1:5,7:14)]
READ_v2a <- READ[,c(1:5,17:24)]
READ_v2b <- READ[,c(1:5,27:35)]
READ_a1a2 <- READ[,c(1:5,38:55)] #include the final level(classification)
READ_a2b1 <- READ[,c(1:5,57:78,84,85)] #include the final level(classification)
READ_b1b2 <- READ[,c(1:5,87:99,105:119)] #include the final level(classification)
#change the column names
colnames(READ_v1)[6:13] <- c(paste0("Item_v1_",1:7),"Score_v1")
colnames(READ_v2a)[6:13] <- c(paste0("Item_v2a_",1:7),"Score_v2a")
colnames(READ_v2b)[6:14] <- c(paste0("Item_v2b_",1:8),"Score_v2b")
colnames(READ_a1a2)[6:23] <- c(paste0("Item_a1a2_",1:16),"Score_a1a2","karaker")
colnames(READ_a2b1)[6:29] <- c(paste0("Item_a2b1_",1:22),"Score_a2b1","karaker")
colnames(READ_b1b2)[6:33] <- c(paste0("Item_b1b2_",1:26),"Score_b1b2","karaker")
#combine all the responses
READ_response <- cbind(READ_v1[,c(2,3,5,6:12)],READ_v2a[,6:12],READ_v2b[,6:13],READ_a1a2[,6:21],READ_a2b1[,6:27],READ_b1b2[,6:31])
#code the responses of 99 and 9 into NA and 0
for(i in 4:89){READ_response[,i] <- gsub(99,NA,READ_response[,i]) # items that are not seen and not completed
READ_response[,i] <- gsub(9,0,READ_response[,i]) # items that are seen but not completed
}
#delete the polytomous item
READ_response <- READ_response[,-c(63,76)]
#change the responses into numbers
READ_response[,4:87] <- as.numeric(unlist(READ_response[,4:87]))

#listening test
#item responses in each module
LISTENING_v1 <- LISTENING[,c(1:4,6:13)]
LISTENING_v2a <- LISTENING[,c(1:4,16:23)]
LISTENING_v2b <- LISTENING[,c(1:4,26:34)]
LISTENING_a1a2 <- LISTENING[,c(1:4,37:54)] #include the final level(classification)
LISTENING_a2b1 <- LISTENING[,c(1:4,56:82)] #include the final level(classification)
LISTENING_b1b2 <- LISTENING[,c(1:4,84:102,111:112)] #include the final level(classification)
#change the column names
colnames(LISTENING_v1)[5:12] <- c(paste0("Item_v1_",1:7),"Score_v1")
colnames(LISTENING_v2a)[5:12] <- c(paste0("Item_v2a_",1:7),"Score_v2a")
colnames(LISTENING_v2b)[5:13] <- c(paste0("Item_v2b_",1:8),"Score_v2b")
colnames(LISTENING_a1a2)[5:22] <- c(paste0("Item_a1a2_",1:16),"Score_a1a2","karaker")
colnames(LISTENING_a2b1)[5:31] <- c(paste0("Item_a2b1_",1:25),"Score_a2b1","karaker")
colnames(LISTENING_b1b2)[5:25] <- c(paste0("Item_b1b2_",1:19),"Score_b1b2","karaker")
#combine all the responses
LISTENING_response <- cbind(LISTENING_v1[,c(2:4,5:11)],LISTENING_v2a[,5:11],LISTENING_v2b[,5:12],
                            LISTENING_a1a2[,5:20],LISTENING_a2b1[,5:29],LISTENING_b1b2[,5:23])
#code the responses of 99 and 9 into NA and 0
for(i in 4:85){LISTENING_response[,i] <- gsub(9,0,LISTENING_response[,i])} #no 99 in listening
LISTENING_response[,4:85] <- as.numeric(unlist(LISTENING_response[,4:85]))

#save the data into R format
saveRDS(READ_response, file = "READ_response.Rds")
saveRDS(LISTENING_response, file = "LISTENING_response.Rds")

################  1.2 Test-taker information data #################

#Reading
READ_info <- READ[,c(1:5,14,24,35,54,55,78,84,85,99,118,119)]#including test-taker No., test center No., test date, test spend,language, sum score in each module, final level
READ_info$'Test-taker' <- gsub("Test-taker ","",READ_info$'Test-taker')
str(READ_info)

#combine the final results
for (i in 1:nrow(READ_info)) {
  if (is.na(READ_info[i,10]) & is.na(READ_info[i,13])){
    READ_info$Level[i] <- READ_info[i,16]
  }
  if (is.na(READ_info[i,10]) & is.na(READ_info[i,16])){
    READ_info$Level[i] <- READ_info[i,13]
  }
  if (is.na(READ_info[i,13]) & is.na(READ_info[i,16])){
    READ_info$Level[i] <- READ_info[i,10]
  }
}

READ_info$Level <- factor(READ_info$Level, levels = c("Under A1","A1","A2","B1","B2"))
READ_info <- READ_info[,c(-10,-13,-16)] #delete the separate results column
table(READ_info$Level)

#change the column names
colnames(READ_info)[6:13] <- list("Stage1", "Stage2A", "Stage2B", "Stage3a1a2", 
                                  "SuperItem1", "Stage3a2b1", "SuperItem2","Stage3b1b2")
summary(READ_info)

#delete the score of polytomous items in stage3a2b1 and stage3b1b2
for (i in 1:nrow(READ_info)) {
  if (is.na(READ_info$SuperItem1[i])){
    READ_info$`Stage 3 a2b1`[i] <- READ_info$`Stage 3 a2b1`[i]
  } else {
    READ_info$`Stage 3 a2b1`[i] <- as.numeric(READ_info$`Stage 3 a2b1`[i])-as.numeric(READ_info$`SuperItem1`[i])
  }
}


for (i in 1:nrow(READ_info)) {
  if (is.na(READ_info$SuperItem2[i]) | READ_info$SuperItem2[i]==99){
    READ_info$`Stage 3 b1b2`[i] <- READ_info$`Stage 3 b1b2`[i]
  } else {
    READ_info$`Stage 3 b1b2`[i] <- as.numeric(READ_info$`Stage 3 b1b2`[i])-as.numeric(READ_info$`SuperItem2`[i])
  }
}

READ_info <- READ_info[,c(-10,-12)] #delete superitem column
summary(READ_info)

#get sum score
READ_info$SumScore <- apply(READ_info[,6:11],1,sum,na.rm = T)

#assign path ways
for (i in 1:nrow(READ_info)){
  if (all(is.na(READ_info[i,c(8,10,11)]))==TRUE){
    READ_info$path[i]="Path 1"
  } else if (all(is.na(READ_info[i,c(8,9,11)]))==TRUE){
    READ_info$path[i]="Path 2"
  } else if (all(is.na(READ_info[i,c(7,9,11)]))==TRUE){
    READ_info$path[i]="Path 3"
  } else {
    READ_info$path[i]="Path 4"
  }
}
READ_info$path <- factor(READ_info$path, levels = c("Path 1","Path 2","Path 3","Path 4"))
summary(READ_info)
table(READ_info$path)

#Listening
LISTEN_info <- LISTENING[,c(1:4,13,23,34,53,54,81,82,111,112)]
LISTEN_info$'Test-taker' <- gsub("Test-taker ","",LISTEN_info$'Test-taker')
str(LISTEN_info)

#combine the final results
for (i in 1:nrow(LISTEN_info)) {
  if (is.na(LISTEN_info[i,9]) & is.na(LISTEN_info[i,11])){
    LISTEN_info$Level[i] <- LISTEN_info[i,13]
  }
  if (is.na(LISTEN_info[i,9]) & is.na(LISTEN_info[i,13])){
    LISTEN_info$Level[i] <- LISTEN_info[i,11]
  }
  if (is.na(LISTEN_info[i,11]) & is.na(LISTEN_info[i,13])){
    LISTEN_info$Level[i] <- LISTEN_info[i,9]
  }
}

LISTEN_info$Level <- factor(LISTEN_info$Level, levels = c("Under A1","A1","A2","B1","B2"))
table(LISTEN_info$Level)
LISTEN_info <- LISTEN_info[,c(-9,-11,-13)] #delete the separate results column

summary(LISTEN_info)

#change the column names, no super item
colnames(LISTEN_info)[5:10] <- list("Stage1", "Stage2A", "Stage2B", "Stage3a1a2", 
                                    "Stage3a2b1", "Stage3b1b2")
summary(LISTEN_info)

#get sum score
LISTEN_info$SumScore <- apply(LISTEN_info[,5:10],1,sum,na.rm = T)

#assign path ways
for (i in 1:nrow(LISTEN_info)){
  if (all(is.na(LISTEN_info[i,c(7,9,10)]))==TRUE){
    LISTEN_info$path[i]="Path 1"
  } else if (all(is.na(LISTEN_info[i,c(7,8,10)]))==TRUE){
    LISTEN_info$path[i]="Path 2"
  } else if (all(is.na(LISTEN_info[i,c(6,8,10)]))==TRUE){
    LISTEN_info$path[i]="Path 3"
  } else {
    LISTEN_info$path[i]="Path 4"
  }
}
LISTEN_info$path <- factor(LISTEN_info$path, levels = c("Path 1","Path 2","Path 3","Path 4"))
summary(LISTEN_info)
table(LISTEN_info$path)

#save the data into R format
saveRDS(READ_info, file = "READ_info.Rds")
saveRDS(LISTEN_info, file = "LISTEN_info.Rds")

############### 1.3 plot the final results and sum scores #################
############  1.3.1 Plot Final results ##########
#creat a result table
Reading <- as.data.frame(READ_info$Level)
Listening <- as.data.frame(LISTEN_info$Level)
Reading$Test <- "Reading"
Listening$Test <- "Listening"
colnames(Reading)[1] <- colnames(Listening)[1] <- "Level"
Results <- rbind(Reading,Listening)

#plot
#together
ggplot(data = Results, aes(x= Level,fill=Test))+
  geom_bar(stat = 'count', position = "dodge") +
  scale_fill_brewer(palette="Pastel1") +
  theme(text = element_text(size = 20))

#by path
level_R <- ggplot(data = READ_info, aes(x= Level,fill=path))+
  geom_bar(stat = 'count', position = "dodge",width = .8) +
  scale_fill_brewer(palette="Pastel1") +
  theme(text = element_text(size = 14))+
  labs(title = "Reading")

level_L <- ggplot(data = LISTEN_info, aes(x= Level,fill=path))+
  geom_bar(stat = 'count', position = "dodge",width = .8) +
  scale_fill_brewer(palette="Pastel1") +
  theme(text = element_text(size = 14))+
  labs(title = "Listening")

ggarrange(level_R,level_L,ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")


############  1.3.2 Plot Sum Score ##########
#Reading
box_sum_R <- ggplot(data = READ_info, aes(x = path, y = SumScore)) +
  stat_boxplot(geom = "errorbar", width = 0.5, na.rm = T) +
  geom_boxplot(stat = "boxplot", outlier.colour = "#ff0000", outlier.size = 1.5, outlier.shape = 8, na.rm = T) +
  labs(x = "Path", y = "Sum Score",title = "Reading")+
  ylim(0,45)
box_sum_R

#Listening
box_sum_L <- ggplot(data = LISTEN_info, aes(x = path, y = SumScore)) +
  stat_boxplot(geom = "errorbar", width = 0.5, na.rm = T) +
  geom_boxplot(stat = "boxplot", outlier.colour = "#ff0000", outlier.size = 1.5, outlier.shape = 8, na.rm = T) +
  labs(x = "Path", y = "Sum Score",title = "Listening")+
  ylim(0,45)
box_sum_L

ggarrange(box_sum_R,box_sum_L,ncol = 2, nrow = 1)

##########################################################################
######################    2. Modeling and evaluation    ##################
##########################################################################
##load the response data from R format
READ_response = readRDS(file = "READ_response.Rds")#in thesis project
LISTENING_response = readRDS(file = "LISTENING_response.Rds")
#load the information data
READ_info = readRDS(file = "READ_info.Rds")
LISTEN_info = readRDS(file = "LISTEN_info.Rds")

###########   2.1 Reading  ###########
#modeling with mirt 
READM1 <- mirt(READ_response[,4:87], 1, itemtype = "2PL", SE=TRUE,
               technical = list(NCYCLES = 4000)) #increased the number of "quadrature points" to be the same between mirt and TAM to get the same estimation accuracy
#save the parameters
READPars <- as.data.frame(coef(READM1, IRTpars = TRUE, simplify = TRUE, digits = Inf)$items[,c('a', 'b', 'g')])
#estimate the ability
READAbi <- as.data.frame(fscores(READM1, method = "EAP", full.scores.SE = TRUE))
#assign pathways
READAbi$path <- READ_info$path

## model fit
#modeling with TAM first
tammod_R <- tam.mml.2pl(resp = READ_response[,4:87], irtmodel = "2PL",
                        control =  list(nodes = seq(-6,6, len = 61)))
#model fit statistics
Modelfit_R <- tam.modelfit(tammod_R)
Modelfit_R$statlist

############   2.2 Listening  ###########
#modeling with mirt
LISTENM1 <- mirt(LISTENING_response[,4:85], 1, itemtype = "2PL", SE=TRUE,
                 technical = list(NCYCLES = 4000))#increased the number of "quadrature points" to be the same between mirt and TAM to get the same estimation accuracy
#save the parameters into dataframe
LISTENPars <- as.data.frame(coef(LISTENM1,IRTpars = TRUE, simplify = TRUE, digits = Inf)$items[,c('a', 'b', 'g')])
#estimate the ability
LISTENAbi <- as.data.frame(fscores(LISTENM1, method = "EAP", full.scores.SE = TRUE))
#assign pathways
LISTENAbi$path <- LISTEN_info$path

## model fit
#modeling with TAM first
tammod_L <- tam.mml.2pl(resp = LISTENING_response[,4:85], irtmodel = "2PL",
                        control =  list(nodes = seq(-6,6, len = 61)))
#model fit statistics
Modelfit_L <- tam.modelfit(tammod_L)
Modelfit_L$statlist

###############  2.3 Plot test information curves  #############
###########   2.3.1 Reading, each module   ###########
#calculate test info by stage
Theta <- matrix(seq(-4,4,.01),ncol = 1)
Stage_1_R <- testinfo(READM1,Theta = Theta, which.items = c(1:7) )
Stage_2A_R <- testinfo(READM1,Theta = Theta, which.items = c(8:14) )
Stage_2B_R <- testinfo(READM1,Theta = Theta, which.items = c(15:22) )
Stage_3_A1A2_R <- testinfo(READM1,Theta = Theta, which.items = c(23:38) )
Stage_3_A2B1_R <- testinfo(READM1,Theta = Theta, which.items = c(39:59) )
Stage_3_B1B2_R <- testinfo(READM1,Theta = Theta, which.items = c(60:84) )

#plot
par(mfrow=c(2,2))
#Reading stage 1 and 2A and 2B
plot(Theta,Stage_1_R,type = 'l',ylim= c(0,12), xlab = expression(theta), 
     ylab = "Test Information", main = "Reading",cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_2A_R,type = 'l',ylim= c(0,12), xlab = expression(theta), col = "red",
     ylab = "Test Information", cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_2B_R,type = 'l',ylim= c(0,12), xlab = expression(theta),col = "red",lty = 2,
     ylab = "Test Information", cex.main=1.5)
legend(-4,12,legend = c("Stage 1", "Stage 2A","Stage 2B"),lty = c(1,1,2), col = c("black","red","red"),box.lty=0, cex = 0.8)
#Reading stage 3
plot(Theta,Stage_3_A1A2_R,type = 'l',ylim= c(0,45), xlab = expression(theta),col = "blue",lty=3,
     ylab = "Test Information", main = "Reading",cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_3_A2B1_R,type = 'l',ylim= c(0,45), xlab = expression(theta),col = "blue",lty=2,
     ylab = "Test Information", cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_3_B1B2_R,type = 'l',ylim= c(0,45), xlab = expression(theta),col = "blue",
     ylab = "Test Information", cex.main=1.5)
legend(-4,45,legend = c("Stage 3 A1A2", "Stage 3 A2B1","Stage 3 A2B1"),lty = c(3:1),col = "blue",box.lty=0, cex = 0.8)

############   2.3.2 Listening, each module  ###########
#calculate test info

Stage_1_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(1:7) )
Stage_2A_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(8:14) )
Stage_2B_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(15:22) )
Stage_3_A1A2_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(23:38) )
Stage_3_A2B1_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(39:63) )
Stage_3_B1B2_L <- testinfo(LISTENM1,Theta = Theta, which.items = c(64:82) )

#plot
#Listening stage 1 and 2A and 2B
plot(Theta,Stage_1_L,type = 'l',ylim= c(0,12), 
     xlab = expression(theta),ylab = "Test Information", main = "Listening",cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_2A_L,type = 'l',ylim= c(0,12), col = "red",
     xlab = expression(theta),ylab = "Test Information")
par(new=TRUE)
plot(Theta,Stage_2B_L,type = 'l',ylim= c(0,12), lty=2, col = "red",
     xlab = expression(theta),ylab = "Test Information")
legend(-4,12,legend = c("Stage 1", "Stage 2A","Stage 2B"),lty = c(1,1,2), col = c("black","red","red"),box.lty=0, cex = 0.8)

#Listening stage 3
plot(Theta,Stage_3_A1A2_L,type = 'l',ylim= c(0,25), col = "blue", lty =3,
     xlab = expression(theta),ylab = "Test Information", main = "Listening",cex.main=1.5)
par(new=TRUE)
plot(Theta,Stage_3_A2B1_L,type = 'l',ylim= c(0,25), col = "blue", lty =2,
     xlab = expression(theta),ylab = "Test Information")
par(new=TRUE)
plot(Theta,Stage_3_B1B2_L,type = 'l',ylim= c(0,25), col = "blue",
     xlab = expression(theta),ylab = "Test Information")
legend(-4,25,legend = c("Stage 3 A1A2", "Stage 3 A2B1","Stage 3 A2B1"),lty = c(3:1),col = "blue",box.lty=0, cex = 0.8)

##########################################################################
#########################     3. Perfit indices    #######################
##########################################################################
##load the data from R format
READ_response = readRDS(file = "READ_response.Rds")#in thesis project
LISTENING_response = readRDS(file = "LISTENING_response.Rds")

################## 3.1 lz* ##################
##seperate modeling (4 path ways) 
##sign group membership to each test taker
READ_response$path <- READ_info$path
LISTENING_response$path <- LISTEN_info$path

READ_response$'Test-taker' <- READ_info$`Test-taker`
LISTENING_response$'Test-taker' <- LISTEN_info$`Test-taker`

#use the estimated item parameters and ability from the estimation using the whole data

########### (1) Reading ##########
#path1
READ1_lzstar <- lzstar(READ_response[READ_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",
                       IP = READPars[c(1:14,23:38),], Ability = READAbi[READAbi$path=="Path 1",1])
READ1_lzstar_cutoff <- cutoff(READ1_lzstar, ModelFit = "Parametric",IP = READPars[c(1:14,23:38),], 
                              Ability = READAbi[READAbi$path=="Path 1",1],UDlvl = -1.64)
READ1_lzstar_flagged <- data.frame(flagged.resp(READ1_lzstar,READ1_lzstar_cutoff,scores=FALSE)$PFSscores)
READ1_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(READ1_lzstar_flagged)){
  READ1_lzstar_flagged$FlaggedIDT[i] = READ_response[READ_response$path=="Path 1",][READ1_lzstar_flagged$FlaggedID[i],89]
}

#path 2
READ2_lzstar <- lzstar(READ_response[READ_response$path=="Path 2",c(4:17,42:62)],NA.method = "NPModel",
                       IP = READPars[c(1:14,39:59),], Ability = READAbi[READAbi$path=="Path 2",1])
READ2_lzstar_cutoff <- cutoff(READ2_lzstar, ModelFit = "Parametric",IP = READPars[c(1:14,39:59),],
                              Ability = READAbi[READAbi$path=="Path 2",1], UDlvl = -1.64 )
READ2_lzstar_flagged <- data.frame(flagged.resp(READ2_lzstar,READ2_lzstar_cutoff,scores=FALSE)$PFSscores)
READ2_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(READ2_lzstar_flagged)){
  READ2_lzstar_flagged$FlaggedIDT[i] = READ_response[READ_response$path=="Path 2",][READ2_lzstar_flagged$FlaggedID[i],89]
}

#path 3
READ3_lzstar <- lzstar(READ_response[READ_response$path=="Path 3",c(4:10,18:25,42:62)],NA.method = "NPModel",
                       IP = READPars[c(1:7, 15:22, 39:59),], Ability = READAbi[READAbi$path=="Path 3",1])
READ3_lzstar_cutoff <- cutoff(READ3_lzstar, ModelFit = "Parametric",IP = READPars[c(1:7, 15:22, 39:59),],
                              Ability = READAbi[READAbi$path=="Path 3",1],UDlvl = -1.64 )
READ3_lzstar_flagged <- data.frame(flagged.resp(READ3_lzstar,READ3_lzstar_cutoff,scores=FALSE)$PFSscores)
READ3_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(READ3_lzstar_flagged)){
  READ3_lzstar_flagged$FlaggedIDT[i] = READ_response[READ_response$path=="Path 3",][READ3_lzstar_flagged$FlaggedID[i],89]
}

#path 4
READ4_lzstar <- lzstar(READ_response[READ_response$path=="Path 4",c(4:10,18:25,63:87)],NA.method = "NPModel",
                       IP = READPars[c(1:7, 15:22, 60:84),], Ability = READAbi[READAbi$path=="Path 4",1])
READ4_lzstar_cutoff <- cutoff(READ4_lzstar, ModelFit = "Parametric",IP = READPars[c(1:7, 15:22, 60:84),],
                              Ability = READAbi[READAbi$path=="Path 4",1],UDlvl = -1.64 )
READ4_lzstar_flagged <- data.frame(flagged.resp(READ4_lzstar,READ4_lzstar_cutoff,scores=FALSE)$PFSscores)
READ4_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(READ4_lzstar_flagged)){
 READ4_lzstar_flagged$FlaggedIDT[i] = READ_response[READ_response$path=="Path 4",][READ4_lzstar_flagged$FlaggedID[i],89]
}

########### (2) Listening ##########

#path1
LISTEN1_lzstar <- lzstar(LISTENING_response[LISTENING_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",
                         IP = LISTENPars[c(1:14,23:38),], Ability = LISTENAbi[LISTENAbi$path=="Path 1",1])
LISTEN1_lzstar_cutoff <- cutoff(LISTEN1_lzstar, ModelFit = "Parametric",IP = LISTENPars[c(1:14,23:38),],
                                Ability = LISTENAbi[LISTENAbi$path=="Path 1",1], UDlvl = -1.64 )
LISTEN1_lzstar_flagged <- data.frame(flagged.resp(LISTEN1_lzstar,LISTEN1_lzstar_cutoff,scores=FALSE)$PFSscores)
LISTEN1_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(LISTEN1_lzstar_flagged)){
 LISTEN1_lzstar_flagged$FlaggedIDT[i] = LISTENING_response[LISTENING_response$path=="Path 1",][LISTEN1_lzstar_flagged$FlaggedID[i],87]
}

#path 2
LISTEN2_lzstar <- lzstar(LISTENING_response[LISTENING_response$path=="Path 2",c(4:17,42:66)],NA.method = "NPModel",
                         IP = LISTENPars[c(1:14,39:63),], Ability = LISTENAbi[LISTENAbi$path=="Path 2",1])
LISTEN2_lzstar_cutoff <- cutoff(LISTEN2_lzstar, ModelFit = "Parametric",IP = LISTENPars[c(1:14,39:63),],
                                Ability = LISTENAbi[LISTENAbi$path=="Path 2",1], UDlvl = -1.64 )
LISTEN2_lzstar_flagged <- data.frame(flagged.resp(LISTEN2_lzstar,LISTEN2_lzstar_cutoff,scores=FALSE)$PFSscores)
LISTEN2_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(LISTEN2_lzstar_flagged)){
 LISTEN2_lzstar_flagged$FlaggedIDT[i] = LISTENING_response[LISTENING_response$path=="Path 2",][LISTEN2_lzstar_flagged$FlaggedID[i],87]
}

#path 3
LISTEN3_lzstar <- lzstar(LISTENING_response[LISTENING_response$path=="Path 3",c(4:10,18:25,42:66)],NA.method = "NPModel",
                         IP = LISTENPars[c(1:7,15:22,39:63),], Ability = LISTENAbi[LISTENAbi$path=="Path 3",1])
LISTEN3_lzstar_cutoff <- cutoff(LISTEN3_lzstar, ModelFit = "Parametric",IP = LISTENPars[c(1:7,15:22,39:63),],
                                Ability = LISTENAbi[LISTENAbi$path=="Path 3",1],UDlvl = -1.64 )
LISTEN3_lzstar_flagged <- data.frame(flagged.resp(LISTEN3_lzstar,LISTEN3_lzstar_cutoff,scores=FALSE)$PFSscores)
LISTEN3_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(LISTEN3_lzstar_flagged)){
 LISTEN3_lzstar_flagged$FlaggedIDT[i] = LISTENING_response[LISTENING_response$path=="Path 3",][LISTEN3_lzstar_flagged$FlaggedID[i],87]
}

#path 4
LISTEN4_lzstar <- lzstar(LISTENING_response[LISTENING_response$path=="Path 4",c(4:10,18:25,67:85)],NA.method = "NPModel",
                         IP = LISTENPars[c(1:7,15:22,64:82),], Ability = LISTENAbi[LISTENAbi$path=="Path 4",1])
LISTEN4_lzstar_cutoff <- cutoff(LISTEN4_lzstar, ModelFit = "Parametric",IP = LISTENPars[c(1:7,15:22,64:82),],
                                Ability = LISTENAbi[LISTENAbi$path=="Path 4",1],UDlvl = -1.64 )
LISTEN4_lzstar_flagged <- data.frame(flagged.resp(LISTEN4_lzstar,LISTEN4_lzstar_cutoff,scores=FALSE)$PFSscores)
LISTEN4_lzstar_flagged$PFS <- "lzstar"
#get the true FlaggedID
for (i in 1: nrow(LISTEN4_lzstar_flagged)){
 LISTEN4_lzstar_flagged$FlaggedIDT[i] = LISTENING_response[LISTENING_response$path=="Path 4",][LISTEN4_lzstar_flagged$FlaggedID[i],87]
}

########### (3) Plot ##########

#plot 4 path together (with normal distribution)
par(mfrow=c(2,4))
#reading
p1 <- plot(READ1_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = READ1_lzstar_cutoff, 
           title = "Path 1, Reading",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(READ1_lzstar[["PFscores"]][["PFscores"]]), sd=sd(READ1_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")
p2 <- plot(READ2_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = READ2_lzstar_cutoff, 
           title = "Path 2, Reading",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(READ2_lzstar[["PFscores"]][["PFscores"]]), sd=sd(READ2_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")
p3 <- plot(READ3_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = READ3_lzstar_cutoff, 
           title = "Path 3, Reading",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(READ3_lzstar[["PFscores"]][["PFscores"]]), sd=sd(READ3_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")
p4 <- plot(READ4_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = READ4_lzstar_cutoff, 
           title = "Path 4, Reading",ylim=c(0,0.4),Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(READ4_lzstar[["PFscores"]][["PFscores"]]), sd=sd(READ4_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")
#listening
p5 <- plot(LISTEN1_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = LISTEN1_lzstar_cutoff, 
           title = "Path 1,Listening",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(LISTEN1_lzstar[["PFscores"]][["PFscores"]]), sd=sd(LISTEN1_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")

p6 <- plot(LISTEN2_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = LISTEN2_lzstar_cutoff, 
           title = "Path 2,Listening",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(LISTEN2_lzstar[["PFscores"]][["PFscores"]]), sd=sd(LISTEN2_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")

p7 <- plot(LISTEN3_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = LISTEN3_lzstar_cutoff, 
           title = "Path 3,Listening",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(LISTEN3_lzstar[["PFscores"]][["PFscores"]]), sd=sd(LISTEN3_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")

p8 <- plot(LISTEN4_lzstar, Type="Both", Blvl=.05, CIlvl = 0.95, cutoff.obj = LISTEN4_lzstar_cutoff, 
           title = "Path 4,Listening",Xcex=1.1, Tcex=1.3,Xlab=expression(italic(l)[italic(z)]*"*"*" , (cutoff=-1.64)"))
curve(dnorm(x, mean=mean(LISTEN4_lzstar[["PFscores"]][["PFscores"]]), sd=sd(LISTEN4_lzstar[["PFscores"]][["PFscores"]])), 
      lwd=2, add=TRUE, yaxt="n", lty= "dashed")

############3.2 SHa(1/2)*   ############

#prepare the functions to calculate SHa and SHb
source("SHaSHbfunction.R")

############  (1) Reading ##########
#####path 1
READ1_SHa12 <- SHa12(READ_response[READ_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",IP = READPars[c(1:14,23:38),], 
                     Ability = READAbi[READAbi$path=="Path 1",1])
READ1_SHa12_PF <-cbind(READ1_SHa12$PFscores, READ1_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ1_SHa12_PF$Flagged <- READ1_SHa12$PFscores > 1.64 
table(READ1_SHa12_PF$Flagged)
#get the flagged ID from the row names
READ1_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(READ1_SHa12_PF))#in the whole test
READ1_SHa12_PF$flaggedID <- c(1:nrow(READ1_SHa12_PF)) #only in path 1
colnames(READ1_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
READ1_SHa12_PF_flagged <- READ1_SHa12_PF[READ1_SHa12_PF$PFS == TRUE,]
READ1_SHa12_PF_flagged$PFS <- "SHa12"

#####path 2
READ2_SHa12 <- SHa12(READ_response[READ_response$path=="Path 2",c(4:17,42:62)],NA.method = "NPModel",
                     IP = READPars[c(1:14,39:59),], 
                     Ability = READAbi[READAbi$path=="Path 2",1])
READ2_SHa12_PF <-cbind(READ2_SHa12$PFscores, READ2_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ2_SHa12_PF$Flagged <- READ2_SHa12$PFscores > 1.64 
table(READ2_SHa12_PF$Flagged) 
#get the flagged ID from the row names
READ2_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(READ2_SHa12_PF)) #in the whole test
READ2_SHa12_PF$flaggedID <- c(1:nrow(READ2_SHa12_PF)) #only in path 2
colnames(READ2_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedID","FlaggedIDT") 
READ2_SHa12_PF_flagged <- READ2_SHa12_PF[READ2_SHa12_PF$PFS == TRUE,]
READ2_SHa12_PF_flagged$PFS <- "SHa12"

#####path 3
READ3_SHa12 <- SHa12(READ_response[READ_response$path=="Path 3",c(4:10,18:25,42:62)],NA.method = "NPModel",
                     IP = READPars[c(1:7, 15:22, 39:59),], 
                     Ability = READAbi[READAbi$path=="Path 3",1])
READ3_SHa12_PF <-cbind(READ3_SHa12$PFscores, READ3_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ3_SHa12_PF$Flagged <- READ3_SHa12$PFscores > 1.64 
table(READ3_SHa12_PF$Flagged)
#get the flagged ID from the row names
READ3_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(READ3_SHa12_PF))#in the whole test
READ3_SHa12_PF$flaggedID <- c(1:nrow(READ3_SHa12_PF)) #only in path 3
colnames(READ3_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
READ3_SHa12_PF_flagged <- READ3_SHa12_PF[READ3_SHa12_PF$PFS == TRUE,]
READ3_SHa12_PF_flagged$PFS <- "SHa12"

#####path 4
READ4_SHa12 <- SHa12(READ_response[READ_response$path=="Path 4",c(4:10,18:25,63:87)],NA.method = "NPModel",
                     IP = READPars[c(1:7, 15:22, 60:84),], 
                     Ability = READAbi[READAbi$path=="Path 4",1])
READ4_SHa12_PF <-cbind(READ4_SHa12$PFscores, READ4_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ4_SHa12_PF$Flagged <- READ4_SHa12$PFscores > 1.64 
table(READ4_SHa12_PF$Flagged)
#get the flagged ID from the row names
READ4_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(READ4_SHa12_PF))#in the whole test
READ4_SHa12_PF$flaggedID <- c(1:nrow(READ4_SHa12_PF)) #only in path 4
colnames(READ4_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
READ4_SHa12_PF_flagged <- READ4_SHa12_PF[READ4_SHa12_PF$PFS == TRUE,]
READ4_SHa12_PF_flagged$PFS <- "SHa12"

############  (2) Listening ##########
#####path 1
LISTEN1_SHa12 <- SHa12(LISTENING_response[LISTENING_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",
                       IP = LISTENPars[c(1:14,23:38),], 
                       Ability = LISTENAbi[LISTENAbi$path=="Path 1",1])
LISTEN1_SHa12_PF <-cbind(LISTEN1_SHa12$PFscores, LISTEN1_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN1_SHa12_PF$Flagged <- LISTEN1_SHa12$PFscores > 1.64 
table(LISTEN1_SHa12_PF$Flagged)
#get the flagged ID from the row names
LISTEN1_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN1_SHa12_PF))#in the whole test
LISTEN1_SHa12_PF$flaggedID <- c(1:nrow(LISTEN1_SHa12_PF)) #only in path 1
colnames(LISTEN1_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN1_SHa12_PF_flagged <- LISTEN1_SHa12_PF[LISTEN1_SHa12_PF$PFS == TRUE,]
LISTEN1_SHa12_PF_flagged$PFS <- "SHa12"

#####path 2
LISTEN2_SHa12 <- SHa12(LISTENING_response[LISTENING_response$path=="Path 2",c(4:17,42:66)],NA.method = "NPModel",
                       IP = LISTENPars[c(1:14,39:63),], 
                       Ability = LISTENAbi[LISTENAbi$path=="Path 2",1])
LISTEN2_SHa12_PF <-cbind(LISTEN2_SHa12$PFscores, LISTEN2_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN2_SHa12_PF$Flagged <- LISTEN2_SHa12$PFscores > 1.64 
table(LISTEN2_SHa12_PF$Flagged)
#get the flagged ID from the row names
LISTEN2_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN2_SHa12_PF))#in the whole test
LISTEN2_SHa12_PF$flaggedID <- c(1:nrow(LISTEN2_SHa12_PF)) #only in path 2
colnames(LISTEN2_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN2_SHa12_PF_flagged <- LISTEN2_SHa12_PF[LISTEN2_SHa12_PF$PFS == TRUE,]
LISTEN2_SHa12_PF_flagged$PFS <- "SHa12"

#####path 3
LISTEN3_SHa12 <- SHa12(LISTENING_response[LISTENING_response$path=="Path 3",c(4:10,18:25,42:66)],NA.method = "NPModel",
                       IP = LISTENPars[c(1:7,15:22,39:63),], 
                       Ability = LISTENAbi[LISTENAbi$path=="Path 3",1])
LISTEN3_SHa12_PF <-cbind(LISTEN3_SHa12$PFscores, LISTEN3_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN3_SHa12_PF$Flagged <- LISTEN3_SHa12$PFscores > 1.64 
table(LISTEN3_SHa12_PF$Flagged)
#get the flagged ID from the row names
LISTEN3_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN3_SHa12_PF))#in the whole test
LISTEN3_SHa12_PF$flaggedID <- c(1:nrow(LISTEN3_SHa12_PF)) #only in path 3
colnames(LISTEN3_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN3_SHa12_PF_flagged <- LISTEN3_SHa12_PF[LISTEN3_SHa12_PF$PFS == TRUE,]
LISTEN3_SHa12_PF_flagged$PFS <- "SHa12"

#####path 4
LISTEN4_SHa12 <- SHa12(LISTENING_response[LISTENING_response$path=="Path 4",c(4:10,18:25,67:85)],NA.method = "NPModel",
                       IP = LISTENPars[c(1:7,15:22,64:82),], 
                       Ability = LISTENAbi[LISTENAbi$path=="Path 4",1])
LISTEN4_SHa12_PF <-cbind(LISTEN4_SHa12$PFscores, LISTEN4_SHa12$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN4_SHa12_PF$Flagged <- LISTEN4_SHa12$PFscores > 1.64 
table(LISTEN4_SHa12_PF$Flagged)
#get the flagged ID from the row names
LISTEN4_SHa12_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN4_SHa12_PF))#in the whole test
LISTEN4_SHa12_PF$flaggedID <- c(1:nrow(LISTEN4_SHa12_PF)) #only in path 4
colnames(LISTEN4_SHa12_PF) <- c("SHa12","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN4_SHa12_PF_flagged <- LISTEN4_SHa12_PF[LISTEN4_SHa12_PF$PFS == TRUE,]
LISTEN4_SHa12_PF_flagged$PFS <- "SHa12"


################### 3.3 SHb(3)* #####################
#####  (1) Reading  #####
#####path 1
READ1_SHb3 <- SHb3(READ_response[READ_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",IP = READPars[c(1:14,23:38),], 
                   Ability = READAbi[READAbi$path=="Path 1",1])
READ1_SHb3_PF <-cbind(READ1_SHb3$PFscores, READ1_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ1_SHb3_PF$Flagged <- READ1_SHb3$PFscores > 1.64 
table(READ1_SHb3_PF$Flagged)
#get the flagged ID from the row names
READ1_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(READ1_SHb3_PF))#in the whole test
READ1_SHb3_PF$flaggedID <- c(1:nrow(READ1_SHb3_PF)) #only in path 1
colnames(READ1_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
READ1_SHb3_PF_flagged <- READ1_SHb3_PF[READ1_SHb3_PF$PFS == TRUE,]
READ1_SHb3_PF_flagged$PFS <- "SHb3"

#####path 2
READ2_SHb3 <- SHb3(READ_response[READ_response$path=="Path 2",c(4:17,42:62)],NA.method = "NPModel",
                   IP = READPars[c(1:14,39:59),], 
                   Ability = READAbi[READAbi$path=="Path 2",1])
READ2_SHb3_PF <-cbind(READ2_SHb3$PFscores, READ2_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ2_SHb3_PF$Flagged <- READ2_SHb3$PFscores > 1.64 
table(READ2_SHb3_PF$Flagged)
#get the flagged ID from the row names
READ2_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(READ2_SHb3_PF))#in the whole test
READ2_SHb3_PF$flaggedID <- c(1:nrow(READ2_SHb3_PF)) #only in path 2
colnames(READ2_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
READ2_SHb3_PF_flagged <- READ2_SHb3_PF[READ2_SHb3_PF$PFS == TRUE,]
READ2_SHb3_PF_flagged$PFS <- "SHb3"

#####path 3
READ3_SHb3 <- SHb3(READ_response[READ_response$path=="Path 3",c(4:10,18:25,42:62)],NA.method = "NPModel",
                   IP = READPars[c(1:7, 15:22, 39:59),], 
                   Ability = READAbi[READAbi$path=="Path 3",1])
READ3_SHb3_PF <-cbind(READ3_SHb3$PFscores, READ3_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ3_SHb3_PF$Flagged <- READ3_SHb3$PFscores > 1.64 
table(READ3_SHb3_PF$Flagged)
#get the flagged ID from the row names
READ3_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(READ3_SHb3_PF))#in the whole test
READ3_SHb3_PF$flaggedID <- c(1:nrow(READ3_SHb3_PF)) #only in path 3
colnames(READ3_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
READ3_SHb3_PF_flagged <- READ3_SHb3_PF[READ3_SHb3_PF$PFS == TRUE,]
READ3_SHb3_PF_flagged$PFS <- "SHb3"

#####path 4
READ4_SHb3 <- SHb3(READ_response[READ_response$path=="Path 4",c(4:10,18:25,63:87)],NA.method = "NPModel",
                   IP = READPars[c(1:7, 15:22, 60:84),], 
                   Ability = READAbi[READAbi$path=="Path 4",1])
READ4_SHb3_PF <-cbind(READ4_SHb3$PFscores, READ4_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
READ4_SHb3_PF$Flagged <- READ4_SHb3$PFscores > 1.64 
table(READ4_SHb3_PF$Flagged)
#get the flagged ID from the row names
READ4_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(READ4_SHb3_PF))#in the whole test
READ4_SHb3_PF$flaggedID <- c(1:nrow(READ4_SHb3_PF)) #only in path 4
colnames(READ4_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
READ4_SHb3_PF_flagged <- READ4_SHb3_PF[READ4_SHb3_PF$PFS == TRUE,]
READ4_SHb3_PF_flagged$PFS <- "SHb3"

#######  (2) Listening  #####
#####path 1
LISTEN1_SHb3 <- SHb3(LISTENING_response[LISTENING_response$path=="Path 1",c(4:17,26:41)],NA.method = "NPModel",
                     IP = LISTENPars[c(1:14,23:38),], 
                     Ability = LISTENAbi[LISTENAbi$path=="Path 1",1])
LISTEN1_SHb3_PF <-cbind(LISTEN1_SHb3$PFscores, LISTEN1_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN1_SHb3_PF$Flagged <- LISTEN1_SHb3$PFscores > 1.64 
table(LISTEN1_SHb3_PF$Flagged)
#get the flagged ID from the row names
LISTEN1_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN1_SHb3_PF))#in the whole test
LISTEN1_SHb3_PF$flaggedID <- c(1:nrow(LISTEN1_SHb3_PF)) #only in path 1
colnames(LISTEN1_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN1_SHb3_PF_flagged <- LISTEN1_SHb3_PF[LISTEN1_SHb3_PF$PFS == TRUE,]
LISTEN1_SHb3_PF_flagged$PFS <- "SHb3"

#####path 2
LISTEN2_SHb3 <- SHb3(LISTENING_response[LISTENING_response$path=="Path 2",c(4:17,42:66)],NA.method = "NPModel",
                     IP = LISTENPars[c(1:14,39:63),], 
                     Ability = LISTENAbi[LISTENAbi$path=="Path 2",1])
LISTEN2_SHb3_PF <-cbind(LISTEN2_SHb3$PFscores, LISTEN2_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN2_SHb3_PF$Flagged <- LISTEN2_SHb3$PFscores > 1.64 
table(LISTEN2_SHb3_PF$Flagged)
#get the flagged ID from the row names
LISTEN2_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN2_SHb3_PF))#in the whole test
LISTEN2_SHb3_PF$flaggedID <- c(1:nrow(LISTEN2_SHb3_PF)) #only in path 2
colnames(LISTEN2_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN2_SHb3_PF_flagged <- LISTEN2_SHb3_PF[LISTEN2_SHb3_PF$PFS == TRUE,]
LISTEN2_SHb3_PF_flagged$PFS <- "SHb3"

#####path 3
LISTEN3_SHb3 <- SHb3(LISTENING_response[LISTENING_response$path=="Path 3",c(4:10,18:25,42:66)],NA.method = "NPModel",
                     IP = LISTENPars[c(1:7,15:22,39:63),], 
                     Ability = LISTENAbi[LISTENAbi$path=="Path 3",1])
LISTEN3_SHb3_PF <-cbind(LISTEN3_SHb3$PFscores, LISTEN3_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN3_SHb3_PF$Flagged <- LISTEN3_SHb3$PFscores > 1.64 
table(LISTEN3_SHb3_PF$Flagged)
#get the flagged ID from the row names
LISTEN3_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN3_SHb3_PF))#in the whole test
LISTEN3_SHb3_PF$flaggedID <- c(1:nrow(LISTEN3_SHb3_PF)) #only in path 3
colnames(LISTEN3_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN3_SHb3_PF_flagged <- LISTEN3_SHb3_PF[LISTEN3_SHb3_PF$PFS == TRUE,]
LISTEN3_SHb3_PF_flagged$PFS <- "SHb3"

#####path 4
LISTEN4_SHb3 <- SHb3(LISTENING_response[LISTENING_response$path=="Path 4",c(4:10,18:25,67:85)],NA.method = "NPModel",
                     IP = LISTENPars[c(1:7,15:22,64:82),], 
                     Ability = LISTENAbi[LISTENAbi$path=="Path 4",1])
LISTEN4_SHb3_PF <-cbind(LISTEN4_SHb3$PFscores, LISTEN4_SHb3$Ability)
#using the critical value from the standard normal distribution (e.g., .1.64 at a = .05)
LISTEN4_SHb3_PF$Flagged <- LISTEN4_SHb3$PFscores > 1.64 
table(LISTEN4_SHb3_PF$Flagged)
#get the flagged ID from the row names
LISTEN4_SHb3_PF$flaggedIDT <- gsub("Resp.","",row.names(LISTEN4_SHb3_PF))#in the whole test
LISTEN4_SHb3_PF$flaggedID <- c(1:nrow(LISTEN4_SHb3_PF)) #only in path 3
colnames(LISTEN4_SHb3_PF) <- c("SHb3","Ability","PFS","FlaggedIDT","FlaggedID") 
LISTEN4_SHb3_PF_flagged <- LISTEN4_SHb3_PF[LISTEN4_SHb3_PF$PFS == TRUE,]
LISTEN4_SHb3_PF_flagged$PFS <- "SHb3"

###################   3.4 Plot SHa(1/2)* and SHb(3)*   #####################
#####  (1) Reading  #####
#### plot using ggplot2
p1_SHa12 <- ggplot(READ1_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ1_SHa12_PF$SHa12), sd=sd(READ1_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 1")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2_SHa12 <- ggplot(READ2_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ2_SHa12_PF$SHa12), sd=sd(READ2_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 2")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3_SHa12 <- ggplot(READ3_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ3_SHa12_PF$SHa12), sd=sd(READ3_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 3")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4_SHa12 <- ggplot(READ4_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ4_SHa12_PF$SHa12), sd=sd(READ4_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 4")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1_SHb3 <- ggplot(READ1_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ1_SHb3_PF$SHb3), sd=sd(READ1_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2_SHb3 <- ggplot(READ2_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ2_SHb3_PF$SHb3), sd=sd(READ2_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3_SHb3 <- ggplot(READ3_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ3_SHb3_PF$SHb3), sd=sd(READ3_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4_SHb3 <- ggplot(READ4_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(READ4_SHb3_PF$SHb3), sd=sd(READ4_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1_SHa12, p2_SHa12, p3_SHa12, p4_SHa12, p1_SHb3,p2_SHb3,p3_SHb3,p4_SHb3,ncol = 4, nrow = 2)

#######  (2) Listening  #####
p1_SHa12_L <- ggplot(LISTEN1_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN1_SHa12_PF$SHa12), sd=sd(LISTEN1_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 1")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2_SHa12_L <- ggplot(LISTEN2_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN2_SHa12_PF$SHa12), sd=sd(LISTEN2_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 2")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3_SHa12_L <- ggplot(LISTEN3_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN3_SHa12_PF$SHa12), sd=sd(LISTEN3_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 3")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4_SHa12_L <- ggplot(LISTEN4_SHa12_PF, aes(x = SHa12))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN4_SHa12_PF$SHa12), sd=sd(LISTEN4_SHa12_PF$SHa12)), lty="dashed")+
  ggtitle("Path 4")+ 
  xlab(expression(italic(SHa(1/2))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1_SHb3_L <- ggplot(LISTEN1_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN1_SHb3_PF$SHb3), sd=sd(LISTEN1_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2_SHb3_L <- ggplot(LISTEN2_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN2_SHb3_PF$SHb3), sd=sd(LISTEN2_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3_SHb3_L <- ggplot(LISTEN3_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN3_SHb3_PF$SHb3), sd=sd(LISTEN3_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p4_SHb3_L <- ggplot(LISTEN4_SHb3_PF, aes(x = SHb3))+
  geom_rect(aes(xmin=1.64,xmax=Inf,ymin=0,ymax=Inf),fill = "lightpink",alpha=0.05)+
  geom_histogram(aes(y = ..density..),bins = 15,fill = "lightblue",colour= "black",) +
  geom_density()+
  geom_vline(xintercept = 1.64, lwd = 1)+
  stat_function(fun = dnorm, args = list(mean=mean(LISTEN4_SHb3_PF$SHb3), sd=sd(LISTEN4_SHb3_PF$SHb3)), lty="dashed")+
  xlab(expression(italic(SHb(3))*"*"*", (cutoff=1.64)"))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5, size=20),
        axis.title.y=element_text(size = 14, vjust=+0.2),
        axis.title.x=element_text(size = 14, vjust=-0.2),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(p1_SHa12_L, p2_SHa12_L, p3_SHa12_L, p4_SHa12_L, p1_SHb3_L,p2_SHb3_L,p3_SHb3_L,p4_SHb3_L,ncol = 4, nrow = 2)

##########################################################################
######################          4. Comparison       ######################
##########################################################################

######## 4.1 Reading #########
###Combine Reading flagged 
READ_lzstar_flagged <- rbind(READ1_lzstar_flagged,READ2_lzstar_flagged,
                             READ3_lzstar_flagged,READ4_lzstar_flagged)
READ_SHa12_flagged <- rbind(READ1_SHa12_PF_flagged,READ2_SHa12_PF_flagged,
                            READ3_SHa12_PF_flagged,READ4_SHa12_PF_flagged)
READ_SHb3_flagged <- rbind(READ1_SHb3_PF_flagged,READ2_SHb3_PF_flagged,
                           READ3_SHb3_PF_flagged,READ4_SHb3_PF_flagged)
#combine all the flagged data
READ_full_flagged <- READ_SHb3_flagged %>% 
  full_join(READ_SHa12_flagged,by = "FlaggedIDT") %>% 
  full_join(READ_lzstar_flagged,by = "FlaggedIDT")
str(READ_full_flagged)
##order the column,delete ability
READ_full_flagged <- READ_full_flagged[,c(4,2,12,11,10,8,6,9,3,1,5)]

table(READ_full_flagged[,c(3,6)],useNA = "always") #lzstar with SHa(1/2)*
table(READ_full_flagged[,c(3,9)],useNA = "always") #lzstar with SHb(3)*
table(READ_full_flagged[,c(6,9)],useNA = "always") #SHa(1/2)* with SHb(3)*

######### 4.2 Listening   ###########
###Combine Listening flagged 
LISTEN_lzstar_flagged <- rbind(LISTEN1_lzstar_flagged,LISTEN2_lzstar_flagged,
                               LISTEN3_lzstar_flagged,LISTEN4_lzstar_flagged)
LISTEN_SHa12_flagged <- rbind(LISTEN1_SHa12_PF_flagged,LISTEN2_SHa12_PF_flagged,
                              LISTEN3_SHa12_PF_flagged,LISTEN4_SHa12_PF_flagged)
LISTEN_SHb3_flagged <- rbind(LISTEN1_SHb3_PF_flagged,LISTEN2_SHb3_PF_flagged,
                             LISTEN3_SHb3_PF_flagged,LISTEN4_SHb3_PF_flagged)
#combine all the flagged data
LISTEN_full_flagged <- LISTEN_SHb3_flagged %>% 
  full_join(LISTEN_SHa12_flagged,by = "FlaggedIDT") %>% 
  full_join(LISTEN_lzstar_flagged,by = "FlaggedIDT")
str(LISTEN_full_flagged)
##order the column,delete ability
LISTEN_full_flagged <- LISTEN_full_flagged[,c(4,2,12,11,10,8,6,9,3,1,5)]

table(LISTEN_full_flagged[,c(3,6)],useNA = "always") #lzstar with SHa(1/2)*
table(LISTEN_full_flagged[,c(3,9)],useNA = "always") #lzstar with SHb(3)*
table(LISTEN_full_flagged[,c(6,9)],useNA = "always") #SHa(1/2)* with SHb(3)*


##########################################################################
################     5. analysis of flagged candidates   #################
##########################################################################

################ 5.1 reading ################
#lzstar
READ_lzstar_analysis <- inner_join(READ_response,READ_lzstar_flagged, by = c("Test-taker"="FlaggedIDT"))
table(READ_lzstar_analysis$Test.center)
table(table(READ_lzstar_analysis$Test.center))
#extreme example's response (minimum)
READ_lzstar_analysis[READ_lzstar_analysis$PFscores == min(READ_lzstar_analysis$PFscores),]

###SHa12
READ_SHa12_analysis <- inner_join(READ_response,READ_SHa12_flagged, by = c("Test-taker"="FlaggedIDT"))
table(READ_SHa12_analysis$Test.center)
table(table(READ_SHa12_analysis$Test.center))
#extreme example's response (maximum)
READ_SHa12_analysis[READ_SHa12_analysis$SHa12 == max(READ_SHa12_analysis$SHa12),]

###SHb3
READ_SHb3_analysis <- inner_join(READ_response,READ_SHb3_flagged, by = c("Test-taker"="FlaggedIDT"))
table(READ_SHb3_analysis$Test.center)
table(table(READ_SHb3_analysis$Test.center))
#extreme example's response (maximum)
READ_SHb3_analysis[READ_SHb3_analysis$SHb3 == max(READ_SHb3_analysis$SHb3),]

################ 5.2 listening ################
#lzstar
LISTEN_lzstar_analysis <- inner_join(LISTENING_response,LISTEN_lzstar_flagged, by = c("Test-taker"="FlaggedIDT"))
table(LISTEN_lzstar_analysis$Test.center)
table(table(LISTEN_lzstar_analysis$Test.center))
#extreme example's response (minimum)
LISTEN_lzstar_analysis[LISTEN_lzstar_analysis$PFscores == min(LISTEN_lzstar_analysis$PFscores),]

###SHa12
LISTEN_SHa12_analysis <- inner_join(LISTENING_response,LISTEN_SHa12_flagged, by = c("Test-taker"="FlaggedIDT"))
table(LISTEN_SHa12_analysis$Test.center)
table(table(LISTEN_SHa12_analysis$Test.center))
#extreme example's response (maximum)
LISTEN_SHa12_analysis[LISTEN_SHa12_analysis$SHa12 == max(LISTEN_SHa12_analysis$SHa12),]

###SHb3
LISTEN_SHb3_analysis <- inner_join(LISTENING_response,LISTEN_SHb3_flagged, by = c("Test-taker"="FlaggedIDT"))
table(LISTEN_SHb3_analysis$Test.center)
table(table(LISTEN_SHb3_analysis$Test.center))
#extreme example's response (maximum)
LISTEN_SHb3_analysis[LISTEN_SHb3_analysis$SHb3 == max(LISTEN_SHb3_analysis$SHb3),]













