# Title : Fuzzy Naive Bayes Classifier R code
# Author: Necla Kayaalp
# Date created: 5 February 2016 
# Modified by Goknur Giner Last modified: 17 February 2016

# install.packages('evtree')
library(evtree)
data("StatlogHeart")
A <- StatlogHeart
names(A)
n <- nrow(A)

membership.false <- function(x) {
  if (29 <= x && x < 60) {
    (x - 29)/31
  } else if (60 <= x && x < 77) {
    1
  } else {
    0
  }
}

membership.true <- function(x) {
  if (42 <= x && x < 48){
    (x - 42) / 6
  } else if (48 <= x && x < 71) {
    1
  } else {
    0
  }
}

membership.normal1 <- function(x) {
  if (126 <= x && x < 226) {
    (x - 126) / 100
  } else if (226 <= x && x < 354) {
    1
  } else {
    0
  }
}

membership.ST.twave.abnormality <- function(x) {
  if (197 <= x && x < 210) {
    (x - 197) / 13 
  } else if (210 <= x && x < 327) {
    1
  } else {
    0
  } 
}

membership.left.venticular.hypertophy <- function(x) {
  if (149 <= x && x < 215) {
    (x - 149) / 66 
  } else if (215 <= x && x < 564) {
    1
  } else {
    0
  }
}

membership.upsloping <- function(x) {
  if (96 <= x && x <= 202) {
    1
  } else {
    0
  }
}

membership.flat <- function(x) {
  if (71 <= x && x < 75) {
    (x - 71) / 4 
  } else if (75 <= x && x < 190) {
    1
  } else {
    0
  }
}

membership.downsloping <- function(x) {
  if (96 <= x && x <= 194) {
    1
  } else {
    0
  }
}

membership.typilangina <- function(x) {
  if (94 <= x && x < 170) {
    (x - 94) / 76 
  } else if (170 <= x && x <= 180) {
    1
  } else {
    0
  }
}

membership.atypilangina <- function(x) {
  if (94 <= x && x < 155){
    (x - 94) / 61 
  } else if (155 <= x && x <= 192){
    1
  } else {0
  }
}

membership.nonanginalpain <- function(x) {
  if (100 <= x && x < 145) {
    (x - 100) / 45 
  } else if (145 <= x && x <= 200){
    1
  } else {0
  }
}

membership.asymptomatic <- function(x) {
  if (108 <= x && x < 130) {
    (x - 108) / 22 
  } else if (130 <= x && x <= 165){
    1
  } else {
    0
  }
}

membership.normal2 <- function(x) { 
  if (0 <= x && x < 1.8) {
    x / 1.8 
  } else if (1.8 <= x && x <= 3.6) {
    1
  } else {
    0
  }
}

membership.fixed.defect <- function(x) {
  if (0 <= x && x < 1.3) {
    x/1.3 
  } else if (1.3 <= x && x <= 2.3) {
    1
  } else {
    0
  }
}

membership.reversible.defect <- function(x) {
  if (0<=x && x<1.0){
    (x)/1.0
  } else if (1.0 <= x && x <= 6.2) {
    1
  } else {
    0
  }
}

###### age~fasting_blood_sugar
A$membership_age <- ifelse(A$fasting_blood_sugar =="yes", 
  sapply(A$age,membership.true), sapply(A$age,membership.false))
  
#####thal~oldpeak
A$membership_oldpeak <- ifelse(A$thal=="normal", 
  sapply(A$oldpeak, membership.normal1), ifelse(A$thal=="fixed defect", 
    sapply(A$oldpeak, membership.fixed.defect), 
    sapply(A$oldpeak, membership.reversible.defect)))
  
##### resting_electrocardiographic_results~serum_colestoral
A$membership_serum <- ifelse(A$resting_electrocardiographic_results=="0", 
  sapply(A$serum_colestoral, membership.normal1), 
  ifelse(A$resting_electrocardiographic_results=="1", 
    sapply(A$serum_colestoral, membership.ST.twave.abnormality), 
    sapply(A$serum_colestoral, membership.left.venticular.hypertophy)))

head(A)
