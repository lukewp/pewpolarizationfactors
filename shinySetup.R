library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(caret)

library(rpart)
library(rpart.plot)
library(rattle)

library(randomForest)
library(randomForestSRC)

library(pROC)

## Assumes the following are in the environment:
## rawinput (DF) -- the survey instrument itself
## model.rank (NMF) -- the 3-rank NMF model, output of the NMF process

model.rank.basis <- data.frame(basis(model.rank))
model.rank.basis$row.id <- rownames(model.rank.basis)

model.rank.predictions <- data.frame(predict(model.rank, "rows", prob=TRUE))
model.rank.predictions$row.id <- rownames(model.rank.predictions)

## Merging survey results w/ NMF output:
explanation <- merge(rawinput, model.rank.predictions, value=row.id, all.x=TRUE)
explanation <- merge(explanation, model.rank.basis, value=row.id, all.x=TRUE)

## To CSV:
# write.csv(explanation, "./outputs/polarization_2014_explanation.csv", na="", row.names = FALSE)
# write.csv(names(explanation), "./outputs/polarization_2014_explanation_header.csv")

## For coefs table:
model.rank.coefs <- data.frame(t(coef(model.rank)))
model.rank.coefs <- merge(data.frame(predict(model.rank, prob=TRUE)), model.rank.coefs, by=0)

## reportsetup object (built up over survey results, back-ends visualizations):
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

## For Tab map:
## Weighted factor distribution by state:
statefactordist <- reportsetup[which(is.na(reportsetup$predict)==FALSE), ] %>%
  group_by(state, predict) %>%
  summarise(
    n=sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

statedisttable <- spread(statefactordist[c("state","predict","prop")], key = predict, value = prop)

## Bringing in 2012 and 2016 election results:
statevotes <- read_csv("./data/statevotes.csv")
statedisttable <- merge(statedisttable, statevotes, value=state, all.x=TRUE)

statedisttable$O2016 <- 1 - statedisttable$D2016 - statedisttable$R2016
statedisttable$O2012 <- 1 - statedisttable$D2012 - statedisttable$R2012
statedisttable$O2008 <- 1 - statedisttable$D2008 - statedisttable$R2008

## Adding state-level summaries for self-reported parties to state table:
statepartydist <- reportsetup[which(is.na(reportsetup$party)==FALSE), ] %>%
  group_by(state, party) %>%
  summarise(
    n=sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

statedisttable <- merge(statedisttable, spread(statepartydist[ , c("state","party","prop")], key = party, value = prop)[c("state", "Republican", "Democrat", "Independent")], value=state, all.x = TRUE)


## Adding a representation of factors based on self-reported registration:
statefactorregdist <- reportsetup[which(is.na(reportsetup$predict)==FALSE & reportsetup$reg=="Are you ABSOLUTELY CERTAIN that you are registered to vote at your current address [OR]"), ] %>%
  mutate(predictreg = paste(predict, "reg", sep="")) %>%
  group_by(state, predictreg) %>%
  summarise(
    n=sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

statedisttable <- merge(statedisttable, spread(statefactorregdist[ , c("state","predictreg","prop")], key = predictreg, value = prop), value=state, all.x = TRUE)

## Model explanations of elections:
# Factor mlms:
# 2-way DV:
factor2008.mfit <- lm(cbind(D2008, R2008) ~ 0 + `1` + `2` + `3`, data = statedisttable)
factor2012.mfit <- lm(cbind(D2012, R2012) ~ 0 + `1` + `2` + `3`, data = statedisttable)
factor2016.mfit <- lm(cbind(D2016, R2016) ~ 0 + `1` + `2` + `3`, data = statedisttable)
# 3-way DV:
# factor2012.3mfit <- lm(cbind(D2012, O2012, R2012) ~ 0 + `1` + `2` + `3`, data = statedisttable)
# factor2016.3mfit <- lm(cbind(D2016, O2016, R2016) ~ 0 + `1` + `2` + `3`, data = statedisttable)


# Factor mlms limited to self-reported voter reg:
# 2-way DV:
# factorreg2012.mfit <- lm(cbind(D2012, R2012) ~ 0 + `1reg` + `2reg` + `3reg`, data = statedisttable)
# factorreg2016.mfit <- lm(cbind(D2016, R2016) ~ 0 + `1reg` + `2reg` + `3reg`, data = statedisttable)
# 3-way DV:
# factorreg2012.3mfit <- lm(cbind(D2012, O2012, R2012) ~ 0 + `1reg` + `2reg` + `3reg`, data = statedisttable)
# factorreg2016.3mfit <- lm(cbind(D2016, O2016, R2016) ~ 0 + `1reg` + `2reg` + `3reg`, data = statedisttable)

# Party-based mlms:
# 2-way DV:
party2012.mfit <- lm(cbind(D2012, R2012) ~ 0 + Democrat + Independent + Republican, data = statedisttable)
party2016.mfit <- lm(cbind(D2016, R2016) ~ 0 + Democrat + Independent + Republican, data = statedisttable)
# 3-way DV:
# party2012.3mfit <- lm(cbind(D2012, O2012, R2012) ~ 0 + Democrat + Independent + Republican, data = statedisttable)
# party2016.3mfit <- lm(cbind(D2016, O2016, R2016) ~ 0 + Democrat + Independent + Republican, data = statedisttable)

# Create the df for the heplot:
mlmdf <- data.frame(factor2016.mfit$fitted.values, row.names = statedisttable$state)

## Factor-based 2016 observed-vs-predicted plot prep:
factord2016.fit <- lm(formula = D2016 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factord2016.fit, interval="predict"))
tmpdf$factord16 <- tmpdf$fit
tmpdf$factord16min <- tmpdf$lwr
tmpdf$factord16max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factord16", "factord16min", "factord16max")], by="state")
rm(tmpdf)

factorr2016.fit <- lm(formula = R2016 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factorr2016.fit, interval="predict"))
tmpdf$factorr16 <- tmpdf$fit
tmpdf$factorr16min <- tmpdf$lwr
tmpdf$factorr16max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factorr16", "factorr16min", "factorr16max")], by="state")
rm(tmpdf)

## Factor-based 2012 observed-vs-predicted plot prep:
factord2012.fit <- lm(formula = D2012 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factord2012.fit, interval="predict"))
tmpdf$factord12 <- tmpdf$fit
tmpdf$factord12min <- tmpdf$lwr
tmpdf$factord12max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factord12", "factord12min", "factord12max")], by="state")
rm(tmpdf)

factorr2012.fit <- lm(formula = R2012 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factorr2012.fit, interval="predict"))
tmpdf$factorr12 <- tmpdf$fit
tmpdf$factorr12min <- tmpdf$lwr
tmpdf$factorr12max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factorr12", "factorr12min", "factorr12max")], by="state")
rm(tmpdf)

## Factor-based 2008 observed-vs-predicted plot prep:
factord2008.fit <- lm(formula = D2008 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factord2008.fit, interval="predict"))
tmpdf$factord08 <- tmpdf$fit
tmpdf$factord08min <- tmpdf$lwr
tmpdf$factord08max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factord08", "factord08min", "factord08max")], by="state")
rm(tmpdf)

factorr2008.fit <- lm(formula = R2008 ~ 0 + `1` + `2` + `3`, data = statedisttable)
tmpdf <- data.frame(predict(factorr2008.fit, interval="predict"))
tmpdf$factorr08 <- tmpdf$fit
tmpdf$factorr08min <- tmpdf$lwr
tmpdf$factorr08max <- tmpdf$upr
tmpdf <- merge(tmpdf, data.frame(statedisttable$state), by=0)
tmpdf$state <- tmpdf$statedisttable.state
statedisttable <- merge(statedisttable, tmpdf[c("state", "factorr08", "factorr08min", "factorr08max")], by="state")
rm(tmpdf)

### Decision Tree for factors:
train.flag <- createDataPartition(y=explanation$predict, p=0.5, list=FALSE)
training <- explanation[train.flag, ]
validation <- explanation[-train.flag, ]

## Random Forest:
# modfit <- randomForest(predict~q100+q105a+q105b+q126+q25a+q25b+q25c+q25d+
#                        q25f+q25g+q25h+q25i+q25j+q25k+q25l+q25m+q25n+q40+
#                        q50aa+q50bb+q50dd+q50ee+q50q+q50r+q50u+q51kk, 
#                        data = training, 
#                        importance = TRUE, 
#                        keep.forest = TRUE, 
#                        ntree = 40, # for this dataset, 40 trees gets us to a good place.
#                        do.trace = TRUE) 
# 
# modfit.explain <- printRandomForests(modfit)
# table(predict(modfit, newdata=training), training$predict)
# table(predict(modfit, newdata=validation), validation$predict)


# Simple regression tree based on IV cols from NMF model:
modrt <- train(predict~q100+q105a+q105b+q126+q25a+q25b+q25c+q25d+
                  q25f+q25g+q25h+q25i+q25j+q25k+q25l+q25m+q25n+q40+
                  q50aa+q50bb+q50dd+q50ee+q50q+q50r+q50u+q51kk, method="rpart", data=training) 

# Plot of decision tree:
fancyRpartPlot(modrt$finalModel)
table(predict(modrt, newdata=training), training$predict)
table(predict(modrt, newdata=validation), validation$predict)


# # Bad RT model based on demo cols, excluded from NMF model -- notably cannot predict factor 2 at all:
# modrt1 <- train(predict~density+sex+age+educ+hisp+racecmb+
#                   marital+relig+attend+income+reg+party+ideo, 
#                 method="rpart", data=training) 
# 
# fancyRpartPlot(modrt1$finalModel)
# table(predict(modrt1, newdata=training), training$predict)
# table(predict(modrt1, newdata=validation), validation$predict)

# # Random Forest on demos -- still not as good as basic tree on NMF IVs:
# modfit1 <- randomForest(predict~density+sex+age+educ+hisp+racecmb+
#                           marital+relig+attend+income+reg+party+ideo,
#                        data = training,
#                        importance = TRUE,
#                        keep.forest = TRUE,
#                        ntree = 400,
#                        do.trace = TRUE)
# modfit1.explain <- printRandomForests(modfit1)
# table(predict(modfit1, newdata=training), training$predict)
# table(predict(modfit1, newdata=validation), validation$predict)


### BUILD VARIABLE SUMMARIES BY PREDICTED FACTOR (all the histograms):
## For demographics tab:
usrdist <- reportsetup %>%
  group_by(predict, usr) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

densitydist <- reportsetup %>%
  group_by(predict, density) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

sexdist <- reportsetup %>%
  group_by(predict, sex) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

agedist <- reportsetup %>%
  group_by(predict, age.r) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

educdist <- reportsetup %>%
  group_by(predict, educ) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

hispdist <- reportsetup %>%
  group_by(predict, hisp) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

racedist <- reportsetup %>%
  group_by(predict, race) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

birth_hispdist <- reportsetup[which(is.na(reportsetup$birth_hisp)==FALSE), ] %>%
  group_by(predict, birth_hisp) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

usborndist <- reportsetup[which(is.na(reportsetup$usborn)==FALSE), ] %>%
  group_by(predict, usborn) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

maritaldist <- reportsetup %>%
  group_by(predict, marital) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

parentdist <- reportsetup[which(is.na(reportsetup$parent)==FALSE), ] %>%
  group_by(predict, parent) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

citizendist <- reportsetup[which(is.na(reportsetup$citizen)==FALSE), ] %>%
  group_by(predict, citizen) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

religdist <- reportsetup %>%
  group_by(predict, relig) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

chrdist <- reportsetup[which(is.na(reportsetup$chr)==FALSE), ] %>%
  group_by(predict, chr) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

borndist <- reportsetup[which(is.na(reportsetup$born)==FALSE), ] %>%
  group_by(predict, born) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

attenddist <- reportsetup %>%
  group_by(predict, attend) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

incomedist <- reportsetup %>%
  group_by(predict, income) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

inchidist <- reportsetup[which(is.na(reportsetup$inchi)==FALSE), ] %>%
  group_by(predict, inchi) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

regdist <- reportsetup %>%
  group_by(predict, reg) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

partydist <- reportsetup %>%
  group_by(predict, party) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

ideodist <- reportsetup %>%
  group_by(predict, ideo) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

## For Tab survey1:
qa1dist <- reportsetup[which(is.na(reportsetup$qa1)==FALSE), ] %>%
  group_by(predict, qa1) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qc1dist <- reportsetup[which(is.na(reportsetup$qc1)==FALSE), ] %>%
  group_by(predict, qc1) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qc1adist <- reportsetup[which(is.na(reportsetup$qc1a)==FALSE), ] %>%
  group_by(predict, qc1a) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qb2dist <- reportsetup[which(is.na(reportsetup$qb2)==FALSE), ] %>%
  group_by(predict, qb2) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qb3dist <- reportsetup[which(is.na(reportsetup$qb3)==FALSE), ] %>%
  group_by(predict, qb3) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qb4dist <- reportsetup[which(is.na(reportsetup$qb4)==FALSE), ] %>%
  group_by(predict, qb4) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qb5dist <- reportsetup[which(is.na(reportsetup$qb5)==FALSE), ] %>%
  group_by(predict, qb5) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa6dist <- reportsetup[which(is.na(reportsetup$qa6)==FALSE), ] %>%
  group_by(predict, qa6) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa8dist <- reportsetup[which(is.na(reportsetup$qa8)==FALSE), ] %>%
  group_by(predict, qa8) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9adist <- reportsetup[which(is.na(reportsetup$qa9a)==FALSE), ] %>%
  group_by(predict, qa9a) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9bdist <- reportsetup[which(is.na(reportsetup$qa9b)==FALSE), ] %>%
  group_by(predict, qa9b) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9cdist <- reportsetup[which(is.na(reportsetup$qa9c)==FALSE), ] %>%
  group_by(predict, qa9c) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9ddist <- reportsetup[which(is.na(reportsetup$qa9d)==FALSE), ] %>%
  group_by(predict, qa9d) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9edist <- reportsetup[which(is.na(reportsetup$qa9e)==FALSE), ] %>%
  group_by(predict, qa9e) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9fdist <- reportsetup[which(is.na(reportsetup$qa9f)==FALSE), ] %>%
  group_by(predict, qa9f) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qa9gdist <- reportsetup[which(is.na(reportsetup$qa9g)==FALSE), ] %>%
  group_by(predict, qa9g) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

q11adist <- reportsetup %>%
  group_by(predict, q11a) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

q11bdist <- reportsetup %>%
  group_by(predict, q11b) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

q11c_bdist <- reportsetup[which(is.na(reportsetup$q11c_b)==FALSE), ] %>%
  group_by(predict, q11c_b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11e_bdist <- reportsetup[which(is.na(reportsetup$q11e_b)==FALSE), ] %>%
  group_by(predict, q11e_b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11h_bdist <- reportsetup[which(is.na(reportsetup$q11h_b)==FALSE), ] %>%
  group_by(predict, q11h_b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11i_bdist <- reportsetup[which(is.na(reportsetup$q11i_b)==FALSE), ] %>%
  group_by(predict, q11i_b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11j_bdist <- reportsetup[which(is.na(reportsetup$q11j_b)==FALSE), ] %>%
  group_by(predict, q11j_b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11atdist <- reportsetup[which(is.na(reportsetup$q11at)==FALSE), ] %>%
  group_by(predict, q11at) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q11btdist <- reportsetup[which(is.na(reportsetup$q11bt)==FALSE), ] %>%
  group_by(predict, q11bt) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb12dist <- reportsetup[which(is.na(reportsetup$qb12)==FALSE), ] %>%
  group_by(predict, qb12) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25adist <- reportsetup %>%
  group_by(predict, q25a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25bdist <- reportsetup %>%
  group_by(predict, q25b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25cdist <- reportsetup %>%
  group_by(predict, q25c) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25ddist <- reportsetup %>%
  group_by(predict, q25d) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25fdist <- reportsetup %>%
  group_by(predict, q25f) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25gdist <- reportsetup %>%
  group_by(predict, q25g) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25hdist <- reportsetup %>%
  group_by(predict, q25h) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25idist <- reportsetup %>%
  group_by(predict, q25i) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25jdist <- reportsetup %>%
  group_by(predict, q25j) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25kdist <- reportsetup %>%
  group_by(predict, q25k) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25ldist <- reportsetup %>%
  group_by(predict, q25l) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25mdist <- reportsetup %>%
  group_by(predict, q25m) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25ndist <- reportsetup %>%
  group_by(predict, q25n) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25odist <- reportsetup[which(is.na(reportsetup$q25o)==FALSE), ] %>%
  group_by(predict, q25o) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q25pdist <- reportsetup[which(is.na(reportsetup$q25p)==FALSE), ] %>%
  group_by(predict, q25p) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb26dist <- reportsetup[which(is.na(reportsetup$qb26)==FALSE), ] %>%
  group_by(predict, qb26) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc26dist <- reportsetup[which(is.na(reportsetup$qc26)==FALSE), ] %>%
  group_by(predict, qc26) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

oftvotedist <- reportsetup %>%
  group_by(predict, oftvote) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q26f1dist <- reportsetup[which(is.na(reportsetup$q26f1.r)==FALSE), ] %>%
  group_by(predict, q26f1.r) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q26f2dist <- reportsetup[which(is.na(reportsetup$q26f2.r)==FALSE), ] %>%
  group_by(predict, q26f2.r) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb27dist <- reportsetup[which(is.na(reportsetup$qb27)==FALSE), ] %>%
  group_by(predict, qb27) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb27adist <- reportsetup[which(is.na(reportsetup$qb27a)==FALSE), ] %>%
  group_by(predict, qb27a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

int1dist <- reportsetup %>%
  group_by(predict, int1) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

int2dist <- reportsetup[which(is.na(reportsetup$int2)==FALSE), ] %>%
  group_by(predict, int2) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

int3mdist <- reportsetup[which(is.na(reportsetup$int3m)==FALSE), ] %>%
  group_by(predict, int3m) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

## For tab survey2:
qc28adist <- reportsetup[which(is.na(reportsetup$qc28a)==FALSE), ] %>%
  group_by(predict, qc28a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28bdist <- reportsetup[which(is.na(reportsetup$qc28b)==FALSE), ] %>%
  group_by(predict, qc28b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28cdist <- reportsetup[which(is.na(reportsetup$qc28c)==FALSE), ] %>%
  group_by(predict, qc28c) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28ddist <- reportsetup[which(is.na(reportsetup$qc28d)==FALSE), ] %>%
  group_by(predict, qc28d) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28edist <- reportsetup[which(is.na(reportsetup$qc28e)==FALSE), ] %>%
  group_by(predict, qc28e) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28fdist <- reportsetup[which(is.na(reportsetup$qc28f)==FALSE), ] %>%
  group_by(predict, qc28f) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28gdist <- reportsetup[which(is.na(reportsetup$qc28g)==FALSE), ] %>%
  group_by(predict, qc28g) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc28hdist <- reportsetup[which(is.na(reportsetup$qc28h)==FALSE), ] %>%
  group_by(predict, qc28h) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qa29adist <- reportsetup[which(is.na(reportsetup$qa29a)==FALSE), ] %>%
  group_by(predict, qa29a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qa29bdist <- reportsetup[which(is.na(reportsetup$qa29b)==FALSE), ] %>%
  group_by(predict, qa29b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q40dist <- reportsetup %>%
  group_by(predict, q40) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb40adist <- reportsetup[which(is.na(reportsetup$qb40a)==FALSE), ] %>%
  group_by(predict, qb40a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb40bdist <- reportsetup[which(is.na(reportsetup$qb40b)==FALSE), ] %>%
  group_by(predict, qb40b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q41dist <- reportsetup %>%
  group_by(predict, q41) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q42dist <- reportsetup %>%
  group_by(predict, q42) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q43dist <- reportsetup[which(is.na(reportsetup$q43)==FALSE), ] %>%
  group_by(predict, q43) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc48adist <- reportsetup[which(is.na(reportsetup$qc48a)==FALSE), ] %>%
  group_by(predict, qc48a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc48bdist <- reportsetup[which(is.na(reportsetup$qc48b)==FALSE), ] %>%
  group_by(predict, qc48b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc48cdist <- reportsetup[which(is.na(reportsetup$qc48c)==FALSE), ] %>%
  group_by(predict, qc48c) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc49adist <- reportsetup[which(is.na(reportsetup$qc49a)==FALSE), ] %>%
  group_by(predict, qc49a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc49bdist <- reportsetup[which(is.na(reportsetup$qc49b)==FALSE), ] %>%
  group_by(predict, qc49b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc49cdist <- reportsetup[which(is.na(reportsetup$qc49c)==FALSE), ] %>%
  group_by(predict, qc49c) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50qdist <- reportsetup[which(is.na(reportsetup$q50q)==FALSE), ] %>%
  group_by(predict, q50q) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50rdist <- reportsetup[which(is.na(reportsetup$q50r)==FALSE), ] %>%
  group_by(predict, q50r) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50sdist <- reportsetup[which(is.na(reportsetup$q50s)==FALSE), ] %>%
  group_by(predict, q50s) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50tdist <- reportsetup[which(is.na(reportsetup$q50t)==FALSE), ] %>%
  group_by(predict, q50t) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50udist <- reportsetup[which(is.na(reportsetup$q50u)==FALSE), ] %>%
  group_by(predict, q50u) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50vdist <- reportsetup[which(is.na(reportsetup$q50v)==FALSE), ] %>%
  group_by(predict, q50v) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50wdist <- reportsetup[which(is.na(reportsetup$q50w)==FALSE), ] %>%
  group_by(predict, q50w) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50ydist <- reportsetup[which(is.na(reportsetup$q50y)==FALSE), ] %>%
  group_by(predict, q50y) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50zdist <- reportsetup[which(is.na(reportsetup$q50z)==FALSE), ] %>%
  group_by(predict, q50z) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50aadist <- reportsetup[which(is.na(reportsetup$q50aa)==FALSE), ] %>%
  group_by(predict, q50aa) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50bbdist <- reportsetup[which(is.na(reportsetup$q50bb)==FALSE), ] %>%
  group_by(predict, q50bb) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50dddist <- reportsetup[which(is.na(reportsetup$q50dd)==FALSE), ] %>%
  group_by(predict, q50dd) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50eedist <- reportsetup[which(is.na(reportsetup$q50ee)==FALSE), ] %>%
  group_by(predict, q50ee) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50ffdist <- reportsetup[which(is.na(reportsetup$q50ff)==FALSE), ] %>%
  group_by(predict, q50ff) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50ggdist <- reportsetup[which(is.na(reportsetup$q50gg)==FALSE), ] %>%
  group_by(predict, q50gg) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q50hhdist <- reportsetup[which(is.na(reportsetup$q50hh)==FALSE), ] %>%
  group_by(predict, q50hh) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51iidist <- reportsetup[which(is.na(reportsetup$q51ii)==FALSE), ] %>%
  group_by(predict, q51ii) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51jjdist <- reportsetup[which(is.na(reportsetup$q51jj)==FALSE), ] %>%
  group_by(predict, q51jj) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51kkdist <- reportsetup[which(is.na(reportsetup$q51kk)==FALSE), ] %>%
  group_by(predict, q51kk) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51lldist <- reportsetup[which(is.na(reportsetup$q51ll)==FALSE), ] %>%
  group_by(predict, q51ll) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51mmdist <- reportsetup[which(is.na(reportsetup$q51mm)==FALSE), ] %>%
  group_by(predict, q51mm) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51oodist <- reportsetup[which(is.na(reportsetup$q51oo)==FALSE), ] %>%
  group_by(predict, q51oo) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q51ppdist <- reportsetup[which(is.na(reportsetup$q51pp)==FALSE), ] %>%
  group_by(predict, q51pp) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

# For tab survey3:
q53dist <- reportsetup[which(is.na(reportsetup$q53)==FALSE), ] %>%
  group_by(predict, q53) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb54dist <- reportsetup[which(is.na(reportsetup$qb54)==FALSE), ] %>%
  group_by(predict, qb54) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb55dist <- reportsetup[which(is.na(reportsetup$qb55)==FALSE), ] %>%
  group_by(predict, qb55) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc56dist <- reportsetup[which(is.na(reportsetup$qc56)==FALSE), ] %>%
  group_by(predict, qc56) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc57dist <- reportsetup[which(is.na(reportsetup$qc57)==FALSE), ] %>%
  group_by(predict, qc57) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc58adist <- reportsetup[which(is.na(reportsetup$qc58a)==FALSE), ] %>%
  group_by(predict, qc58a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc58bdist <- reportsetup[which(is.na(reportsetup$qc58b)==FALSE), ] %>%
  group_by(predict, qc58b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q100dist <- reportsetup[which(is.na(reportsetup$q100)==FALSE), ] %>%
  group_by(predict, q100) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q101dist <- reportsetup[which(is.na(reportsetup$q101)==FALSE), ] %>%
  group_by(predict, q101) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q102dist <- reportsetup[which(is.na(reportsetup$q102)==FALSE), ] %>%
  group_by(predict, q102) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q102adist <- reportsetup[which(is.na(reportsetup$q102a)==FALSE), ] %>%
  group_by(predict, q102a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q105adist <- reportsetup[which(is.na(reportsetup$q105a)==FALSE), ] %>%
  group_by(predict, q105a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q105bdist <- reportsetup[which(is.na(reportsetup$q105b)==FALSE), ] %>%
  group_by(predict, q105b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q105ddist <- reportsetup[which(is.na(reportsetup$q105d)==FALSE), ] %>%
  group_by(predict, q105d) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q106dist <- reportsetup[which(is.na(reportsetup$q106)==FALSE), ] %>%
  group_by(predict, q106) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb106dist <- reportsetup[which(is.na(reportsetup$qb106)==FALSE), ] %>%
  group_by(predict, qb106) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb107dist <- reportsetup[which(is.na(reportsetup$qb107)==FALSE), ] %>%
  group_by(predict, qb107) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb108dist <- reportsetup[which(is.na(reportsetup$qb108)==FALSE), ] %>%
  group_by(predict, qb108) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb109dist <- reportsetup[which(is.na(reportsetup$qb109)==FALSE), ] %>%
  group_by(predict, qb109) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb110dist <- reportsetup[which(is.na(reportsetup$qb110)==FALSE), ] %>%
  group_by(predict, qb110) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc111dist <- reportsetup[which(is.na(reportsetup$qc111)==FALSE), ] %>%
  group_by(predict, qc111) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc112dist <- reportsetup[which(is.na(reportsetup$qc112)==FALSE), ] %>%
  group_by(predict, qc112) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc115dist <- reportsetup[which(is.na(reportsetup$qc115)==FALSE), ] %>%
  group_by(predict, qc115) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc116dist <- reportsetup[which(is.na(reportsetup$qc116)==FALSE), ] %>%
  group_by(predict, qc116) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc116adist <- reportsetup[which(is.na(reportsetup$qc116a)==FALSE), ] %>%
  group_by(predict, qc116a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc117dist <- reportsetup[which(is.na(reportsetup$qc117)==FALSE), ] %>%
  group_by(predict, qc117) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q121dist <- reportsetup[which(is.na(reportsetup$q121)==FALSE), ] %>%
  group_by(predict, q121) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q121adist <- reportsetup[which(is.na(reportsetup$q121a)==FALSE), ] %>%
  group_by(predict, q121a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q121bdist <- reportsetup[which(is.na(reportsetup$q121b)==FALSE), ] %>%
  group_by(predict, q121b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q122dist <- reportsetup[which(is.na(reportsetup$q122)==FALSE), ] %>%
  group_by(predict, q122) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q122adist <- reportsetup[which(is.na(reportsetup$q122a)==FALSE), ] %>%
  group_by(predict, q122a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q122bdist <- reportsetup[which(is.na(reportsetup$q122b)==FALSE), ] %>%
  group_by(predict, q122b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q123dist <- reportsetup[which(is.na(reportsetup$q123)==FALSE), ] %>%
  group_by(predict, q123) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q123adist <- reportsetup[which(is.na(reportsetup$q123a)==FALSE), ] %>%
  group_by(predict, q123a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q123bdist <- reportsetup[which(is.na(reportsetup$q123b)==FALSE), ] %>%
  group_by(predict, q123b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q124dist <- reportsetup[which(is.na(reportsetup$q124)==FALSE), ] %>%
  group_by(predict, q124) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q124adist <- reportsetup[which(is.na(reportsetup$q124a)==FALSE), ] %>%
  group_by(predict, q124a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q124bdist <- reportsetup[which(is.na(reportsetup$q124b)==FALSE), ] %>%
  group_by(predict, q124b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q125dist <- reportsetup[which(is.na(reportsetup$q125)==FALSE), ] %>%
  group_by(predict, q125) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q125adist <- reportsetup[which(is.na(reportsetup$q125a)==FALSE), ] %>%
  group_by(predict, q125a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q125bdist <- reportsetup[which(is.na(reportsetup$q125b)==FALSE), ] %>%
  group_by(predict, q125b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q126dist <- reportsetup[which(is.na(reportsetup$q126)==FALSE), ] %>%
  group_by(predict, q126) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q126adist <- reportsetup[which(is.na(reportsetup$q126a)==FALSE), ] %>%
  group_by(predict, q126a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

q126bdist <- reportsetup[which(is.na(reportsetup$q126b)==FALSE), ] %>%
  group_by(predict, q126b) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc127dist <- reportsetup[which(is.na(reportsetup$qc127)==FALSE), ] %>%
  group_by(predict, qc127) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc128dist <- reportsetup[which(is.na(reportsetup$qc128)==FALSE), ] %>%
  group_by(predict, qc128) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

## For tab survey4:
qc135dist <- reportsetup[which(is.na(reportsetup$qc135)==FALSE), ] %>%
  group_by(predict, qc135) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qc135adist <- reportsetup[which(is.na(reportsetup$qc135a)==FALSE), ] %>%
  group_by(predict, qc135a) %>%
  summarise(n = sum(weight) # weight variable)
  ) %>%
  mutate(prop = prop.table(n))

qb139dist <- reportsetup[which(is.na(reportsetup$qb139)==FALSE), ] %>%
  group_by(predict, qb139) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))

qb139adist <- reportsetup[which(is.na(reportsetup$qb139a)==FALSE), ] %>%
  group_by(predict, qb139a) %>%
  summarise(n = sum(weight) # weight variable
  ) %>%
  mutate(prop = prop.table(n))
