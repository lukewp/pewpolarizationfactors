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
write.csv(explanation, "./outputs/polarization_2014_explanation.csv", na="", row.names = FALSE)
write.csv(names(explanation), "./outputs/polarization_2014_explanation_header.csv")
