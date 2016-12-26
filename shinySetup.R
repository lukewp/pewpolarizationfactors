library(dplyr)
library(tidyr)
library(scales)

## Assumes the following are in the environment:
## rawinput (DF)
## model.rank (NMF)


model.rank.basis <- data.frame(basis(model.rank))
model.rank.basis$row.id <- rownames(model.rank.basis)

model.rank.predictions <- data.frame(predict(model.rank, "rows", prob=TRUE))
model.rank.predictions$row.id <- rownames(model.rank.predictions)

## Merging rawinput w/ NMF output:
explanation <- merge(rawinput, model.rank.predictions, value=row.id, all.x=TRUE)
explanation <- merge(explanation, model.rank.basis, value=row.id, all.x=TRUE)

## To CSV:
# write.csv(explanation, "./outputs/polarization_2014_explanation.csv", na="", row.names = FALSE)
# write.csv(names(explanation), "./outputs/polarization_2014_explanation_header.csv")

model.rank.coefs <- data.frame(t(coef(model.rank)))
model.rank.coefs <- merge(data.frame(predict(model.rank, prob=TRUE)), model.rank.coefs, by=0)

## reportsetup object:
reportsetup <- explanation %>%
  mutate(predict = as.factor(predict),
         state = as.factor(state),
         usr = as.factor(usr),
         density = as.ordered(density),
         sex = as.factor(sex),
         # age = as.ordered(age),
         age.r = as.ordered(round(age/10,0)), # Splitting into buckets by 10
         educ = as.factor(educ),
         hisp = as.factor(hisp),
         race = as.factor(racecmb),
         marital = as.factor(marital),
         parent = as.factor(parent),
         citizen = as.factor(citizen),
         relig = as.factor(relig),
         attend = as.ordered(attend),
         income = as.ordered(income),
         reg = as.factor(reg),
         party = as.factor(party),
         ideo = as.factor(ideo),
         q26f1.r = as.ordered(round(q26f1/10,0)), # Splitting into buckets by 10
         q26f2.r = as.ordered(round(q26f2/10,0)) # Splitting into buckets by 10
  )

usrdist <- reportsetup %>%
  group_by(predict, usr) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

densitydist <- reportsetup %>%
  group_by(predict, density) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

sexdist <- reportsetup %>%
  group_by(predict, sex) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

agedist <- reportsetup %>%
  group_by(predict, age.r) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

educdist <- reportsetup %>%
  group_by(predict, educ) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

hispdist <- reportsetup %>%
  group_by(predict, hisp) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

racedist <- reportsetup %>%
  group_by(predict, race) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

maritaldist <- reportsetup %>%
  group_by(predict, marital) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

parentdist <- reportsetup[which(is.na(reportsetup$parent)==FALSE), ] %>%
  group_by(predict, parent) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

citizendist <- reportsetup[which(is.na(reportsetup$citizen)==FALSE), ] %>%
  group_by(predict, citizen) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

religdist <- reportsetup %>%
  group_by(predict, relig) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

attenddist <- reportsetup %>%
  group_by(predict, attend) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

incomedist <- reportsetup %>%
  group_by(predict, income) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

regdist <- reportsetup %>%
  group_by(predict, reg) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

partydist <- reportsetup %>%
  group_by(predict, party) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

ideodist <- reportsetup %>%
  group_by(predict, ideo) %>%
  summarise(n = sum(weight) # weight variable) 
  ) %>%
  mutate(prop = prop.table(n))

statedist <- reportsetup[which(is.na(reportsetup$predict)==FALSE), ] %>%
  group_by(state, predict) %>%
  summarise(
    n=sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

statedisttable <- spread(statedist[c("state","predict","prop")], key = predict, value = prop)