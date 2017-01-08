set.seed(042416)
library(data.table)
library(dplyr)
library(quanteda)
library(stringi)
library(ggplot2)

vocab <- nrow(x[[1]])
disCount <- 0.75

kn4 <- data.table(x[[4]])
kn4$prevW <- x[[3]]$w[x[[4]]$child]
kn4prev <- kn4 %>%
  group_by(w, prevW) %>%
  summarise(countPrevW = n())
kn4prev <- kn4prev %>%
  group_by(w) %>%
  summarise(uniquePrevW = n())

kn3next <- kn4 %>%
  group_by(child, w) %>%
  summarise(countNextW = n())
kn3next <- kn3next %>%
  group_by(child) %>%
  summarise(uniqueNextW = n())

kn3 <- data.table(x[[3]])
kn3$prevW <- x[[2]]$w[x[[3]]$child]
kn3prev <- kn3 %>%
  group_by(w, prevW) %>%
  summarise(countPrevW = n())
kn3prev <- kn3prev %>%
  group_by(w) %>%
  summarise(uniquePrevW = n())

kn2next <- kn3 %>%
  group_by(child, w) %>%
  summarise(countNextW = n())
kn2next <- kn2next %>%
  group_by(child) %>%
  summarise(uniqueNextW = n())

kn2 <- data.table(x[[2]])
kn2$prevW <- x[[1]]$w[x[[2]]$child]
kn2prev <- kn2 %>%
  group_by(w, prevW) %>%
  summarise(countPrevW = n())
kn2prev <- kn2prev %>%
  group_by(w) %>%
  summarise(uniquePrevW = n())

kn1next <- kn2 %>%
  group_by(child, w) %>%
  summarise(countNextW = n())
kn1next <- kn1next %>%
  group_by(child) %>%
  summarise(uniqueNextW = n())

x[[1]]$dCountF <- x[[1]]$countF - disCount
x[[1]]$pDisc <- (x[[1]]$dCountF > 0) * x[[1]]$dCountF/sum(x[[1]]$countF)
lambda0 <- (1 - sum(x[[1]]$pDisc)) * vocab/nrow(x[[1]])
x[[1]]$pKN <- x[[1]]$pDisc + (lambda0/vocab)

x[[2]]$dCountF <- x[[2]]$countF - disCount
x[[2]]$pDisc <- (x[[2]]$dCountF > 0) * x[[2]]$dCountF/sum(x[[2]]$countF)
x[[2]]$pCont <- kn2prev$uniquePrevW[match(x[[2]]$w, kn2prev$w)]/sum(kn2prev$uniquePrevW)
x[[2]]$lambda1 <- disCount * kn1next$uniqueNextW[match(x[[2]]$child, kn1next$child)]/x[[1]]$countF[x[[2]]$child]
x[[2]]$pKN<- x[[2]]$pDisc + (x[[2]]$lambda1 * x[[2]]$pCont)

x[[3]]$dCountF <- x[[3]]$countF - disCount
x[[3]]$pDisc <- (x[[3]]$dCountF > 0) * x[[3]]$dCountF/sum(x[[3]]$countF)
x[[3]]$pCont <- kn3prev$uniquePrevW[match(x[[3]]$w, kn3prev$w)]/sum(kn3prev$uniquePrevW)
x[[3]]$lambda2 <- disCount * kn2next$uniqueNextW[match(x[[3]]$child, kn2next$child)]/sum(x[[3]]$countF)
x[[3]]$pKN <- x[[3]]$pDisc + (x[[3]]$lambda2 * x[[3]]$pCont)

x[[4]]$dCountF <- x[[4]]$countF - disCount
x[[4]]$pDisc <- (x[[4]]$dCountF > 0) * x[[4]]$dCountF/sum(x[[4]]$countF)
x[[4]]$pCont <- kn4prev$uniquePrevW[match(x[[4]]$w, kn4prev$w)]/sum(kn4prev$uniquePrevW)
x[[4]]$lambda3 <- disCount * kn3next$uniqueNextW[match(x[[4]]$child, kn3next$child)]/sum(x[[4]]$countF)
x[[4]]$pKN <- x[[4]]$pDisc + (x[[4]]$lambda3 * x[[4]]$pCont)



