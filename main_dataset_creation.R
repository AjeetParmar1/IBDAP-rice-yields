library(tidyverse)
library(dplyr)
library(readxl)
library(glmnet)
library(reshape2)
library(ggplot2)
library(rpart)


# reads the dataset and skips unnecessary information
dataset <- read.csv("new_crops.csv", header = TRUE, row.names=NULL, skip = 14)
#dimensions of the dataset
dim(dataset) 
# # January dataset parsing
# dataset_jan <- dataset[961:963, ]
# dataset_jan <- subset(dataset_jan, select = -c(START.DATE, END.DATE, SOURCE, 
#                                                ORDINAL.DATE))
# dataset_jan <- sapply(dataset_jan, as.numeric)
# dataset_jan <- colMeans(dataset_jan)
# dataset_jan$variable <- rownames(dataset_jan)
# Sdataset <- read.csv("data/Sjan.csv", header = TRUE)
# Sdataset <- merge(dataset_jan, Sdataset)
# Ldataset <- read.csv("data/Ljan.csv", header = TRUE)
# Ldataset <- merge(dataset_jan, Ldataset)
# JHdataset <- read.csv("data/JHjan.csv", header = TRUE)
# JHdataset <- merge(dataset_jan, JHdataset)
# 
# #February dataset parsing: 
# dataset_feb <- dataset[964:967, ]
# dataset_feb <- subset(dataset_feb, select = -c(START.DATE, END.DATE, SOURCE, 
#                                                ORDINAL.DATE))
# dataset_feb <- sapply(dataset_feb, as.numeric)
# dataset_feb <- colMeans(dataset_feb)
# dataset_feb$variable <- rownames(dataset_feb)
# Sdataset_tmp <- read.csv("data/Sfeb.csv", header = TRUE)
# Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
# Sdataset_tmp <- merge(dataset_feb, Sdataset_tmp)
# Sdataset <-rbind(Sdataset, Sdataset_tmp)
# 
# Ldataset_tmp <- read.csv("data/Lfeb.csv", header = TRUE)
# Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
# Ldataset_tmp <- merge(dataset_feb, Ldataset_tmp)
# Ldataset <-rbind(Ldataset, Ldataset_tmp)
# 
# JHdataset_tmp <- read.csv("data/JHfeb.csv", header = TRUE)
# JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
# JHdataset_tmp <- merge(dataset_feb, JHdataset_tmp)
# JHdataset <-rbind(JHdataset, JHdataset_tmp)


rice_yields <- read.csv("rice_yields.csv", header = TRUE)

#March dataset parsing: 
dataset_mar <- dataset[968:971, ]
dataset_mar <- subset(dataset_mar, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_mar <- sapply(dataset_mar, as.numeric)
dataset_mar <- colMeans(dataset_mar)
dataset_mar$variable <- rownames(dataset_mar)
Sdataset_tmp <- read.csv("data/Smar.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_mar, Sdataset_tmp)
# Sdataset <-rbind(Sdataset, Sdataset_tmp)
Sdataset <- Sdataset_tmp
Sdataset["Rice yield"] <- rep(c(rice_yields$S[2]), each = nrow(Sdataset))

Ldataset_tmp <- read.csv("data/Lmar.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_mar, Ldataset_tmp)
# Ldataset <-rbind(Ldataset, Ldataset_tmp)
Ldataset <- Ldataset_tmp
Ldataset["Rice yield"] <- rep(c(rice_yields$L[2]), each = nrow(Ldataset))

JHdataset_tmp <- read.csv("data/JHmar.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_mar, JHdataset_tmp)
JHdataset <-rbind(JHdataset, JHdataset_tmp)
JHdataset <- JHdataset_tmp
JHdataset["Rice yield"] <- rep(c(rice_yields$JH[2]), each = nrow(JHdataset))

#April dataset parsing: 
dataset_apr <- dataset[972:975, ]
dataset_apr <- subset(dataset_apr, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_apr <- sapply(dataset_apr, as.numeric)
dataset_apr <- colMeans(dataset_apr)
dataset_apr$variable <- rownames(dataset_apr)
Sdataset_tmp <- read.csv("data/Sapr.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_apr, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[3]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)

Ldataset_tmp <- read.csv("data/Lapr.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_apr, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[3]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)


JHdataset_tmp <- read.csv("data/JHapr.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_apr, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[3]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)


#May dataset parsing: 
dataset_may <- dataset[976:978, ]
dataset_may <- subset(dataset_may, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_may <- sapply(dataset_may, as.numeric)
dataset_may <- colMeans(dataset_may)
dataset_may$variable <- rownames(dataset_may)
Sdataset_tmp <- read.csv("data/Smay.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_may, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[4]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)


Ldataset_tmp <- read.csv("data/Lmay.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_may, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[4]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)


JHdataset_tmp <- read.csv("data/JHmay.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_may, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[4]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)


#June dataset parsing: 
dataset_jun <- dataset[979:982, ]
dataset_jun <- subset(dataset_jun, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_jun <- sapply(dataset_jun, as.numeric)
dataset_jun <- colMeans(dataset_jun)
dataset_jun$variable <- rownames(dataset_jun)
Sdataset_tmp <- read.csv("data/Sjun.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_jun, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[5]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)

Ldataset_tmp <- read.csv("data/Ljun.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_jun, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[5]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)

JHdataset_tmp <- read.csv("data/JHjun.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_jun, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[5]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)

#July dataset parsing: 
dataset_jul <- dataset[983:986, ]
dataset_jul <- subset(dataset_jul, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_jul <- sapply(dataset_jul, as.numeric)
dataset_jul <- colMeans(dataset_jul)
dataset_jul$variable <- rownames(dataset_jul)
Sdataset_tmp <- read.csv("data/Sjul.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_jul, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[6]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)

Ldataset_tmp <- read.csv("data/Ljul.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_jul, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[6]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)

JHdataset_tmp <- read.csv("data/JHjul.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_jul, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[6]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)

#August dataset parsing: 
dataset_aug <- dataset[987:990, ]
dataset_aug <- subset(dataset_aug, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_aug <- sapply(dataset_aug, as.numeric)
dataset_aug <- colMeans(dataset_aug)
dataset_aug$variable <- rownames(dataset_aug)
Sdataset_tmp <- read.csv("data/Saug.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_aug, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[7]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)

Ldataset_tmp <- read.csv("data/Laug.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_aug, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[7]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)

JHdataset_tmp <- read.csv("data/JHaug.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_aug, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[7]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)

#September dataset parsing: 
dataset_sep <- dataset[987:990, ]
dataset_sep <- subset(dataset_sep, select = -c(START.DATE, END.DATE, SOURCE, 
                                               ORDINAL.DATE))
dataset_sep <- sapply(dataset_sep, as.numeric)
dataset_sep <- colMeans(dataset_sep)
dataset_sep$variable <- rownames(dataset_sep)
Sdataset_tmp <- read.csv("data/Ssep.csv", header = TRUE)
Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
Sdataset_tmp <- merge(dataset_sep, Sdataset_tmp)
Sdataset_tmp["Rice yield"] <- rep(c(rice_yields$S[8]), each = nrow(Sdataset_tmp))
Sdataset <-rbind(Sdataset, Sdataset_tmp)

Ldataset_tmp <- read.csv("data/Lsep.csv", header = TRUE)
Ldataset_tmp <- subset(Ldataset_tmp, select = -c(Precipitation..mm.hr.) )
Ldataset_tmp <- merge(dataset_sep, Ldataset_tmp)
Ldataset_tmp["Rice yield"] <- rep(c(rice_yields$L[8]), each = nrow(Ldataset_tmp))
Ldataset <-rbind(Ldataset, Ldataset_tmp)

JHdataset_tmp <- read.csv("data/JHsep.csv", header = TRUE)
JHdataset_tmp <- subset(JHdataset_tmp, select = -c(Precipitation..mm.hr.) )
JHdataset_tmp <- merge(dataset_sep, JHdataset_tmp)
JHdataset_tmp["Rice yield"] <- rep(c(rice_yields$JH[8]), each = nrow(JHdataset_tmp))
JHdataset <-rbind(JHdataset, JHdataset_tmp)

write.csv(Sdataset,"finalSdata.csv", row.names = FALSE)
write.csv(Ldataset,"finalLdata.csv", row.names = FALSE)
write.csv(JHdataset,"finalJHdata.csv", row.names = FALSE)

fin_dataset <- rbind(JHdataset, Ldataset, Sdataset)
write.csv(fin_dataset,"finalcombdata.csv", row.names = FALSE)
