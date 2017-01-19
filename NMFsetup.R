## Run this one second!

# install.packages("rgl")
library(rgl)
# If this fails on MacOS, install XQuartz.

# install.packages("clusterSim")
library(clusterSim) # TODO: can probably replace the one thing I'm using this package for with rescale() ...
# If this is erroring out on an X11 error, and you're on an ubuntu box, run the following:
#!> sudo apt-get install r-cran-rgl
# you may also need to use X11 forwarding ...

# Loading and Prepping for NMF:
rawinput <- survey

## Produce clean matrix:
# now all the vars:
factorvars <- (c("row.id", "q100.ca", "q105a.cd", "q105b.ca"
                 , "q126.ca", "q50aa.c2", "q50bb.c1", "q50dd.c1"
                 , "q50dd.c2", "q50ee.c2", "q51kk.c1", "q100.cd"
                 , "q105a.ca", "q105b.cd", "q126.cd", "q25a.c1"
                 , "q25a.c2", "q25b.c1", "q25b.c2", "q25c.c1"
                 , "q25c.c2", "q25d.c1", "q25d.c2", "q25f.c1"
                 , "q25f.c2", "q25g.c1", "q25g.c2", "q25h.c1"
                 , "q25h.c2", "q25i.c1", "q25i.c2", "q25j.c1"
                 , "q25j.c2", "q25k.c1", "q25k.c2", "q25l.c1"
                 , "q25l.c2", "q25m.c1", "q25m.c2", "q25n.c1"
                 , "q25n.c2", "q40.c", "q50aa.c1", "q50bb.c2"
                 , "q50ee.c1", "q50q.c1", "q50q.c2", "q50r.c1"
                 , "q50r.c2", "q50u.c1", "q50u.c2", "q51kk.c2"))

factormatrix <- rawinput[factorvars]

# grab the vars that shouldn't be summed:
nonnumericvars <- names(factormatrix) %in% c("row.id")

factormatrix_clean <- factormatrix[which(rowSums(factormatrix[!nonnumericvars]) > 1), ]
factormatrix_clean <- factormatrix_clean[complete.cases(factormatrix_clean), ]

# set clean matrix to have rownames from original file:
rownames(factormatrix_clean) <- factormatrix_clean$row.id

# remove row.id variable from DF (otherwise NMF will break):
factormatrix_clean$row.id <- NULL

# Rescaling to 0-1:
factormatrix_clean <- data.Normalization(factormatrix_clean, type="n4")
