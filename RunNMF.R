## This is the third script to run -- must be tweaked (uncommented here and there) to create a 
## new NMF object. By default, it'll just load the .RData file described at the bottom.

# Factorizing the matrix:
# Libraries:
# install.packages("foreach")
# install.packages("doParallel")
# install.packages("NMF")
# install.packages("Biobase")
# 
# # If this is throwing a warning or erroring out, and you're on an ubuntu box, run the following:
# #!> sudo apt-get install r-bioc-biobase
# 
# library(foreach)
# library(doParallel)
# library(NMF)
# install.extras("NMF")
# 
## ASSUME factormatrix_clean exists and is already prepped.

# Choose method according to input matrix
# Exploratory NMF Run, perofrming test runs to examine fit of sparse models of ranks 2-20:
# estim.r <- nmf(factormatrix_clean, 2:20, .opt = "v3Pt")
# save(estim.r, file='./outputs/estim.r.RData')
# plot(estim.r)
# consensusmap(estim.r)

# Random baselining NMF run:
# factormatrix_clean.random <- randomize(factormatrix_clean)
# estim.r.random <- nmf(factormatrix_clean.random, 2:20, .opt = "v3Pt")
# save(estim.r.random, file='./outputs/estim.r.random.RData')
# 
# # Compare runs:
# plot(estim.r, estim.r.random)

# Run the full-scale estimate:
# factormatrix_clean.3rank <- nmf(factormatrix_clean, 3, .opt = ".v3Pt")
# save(factormatrix_clean.3rank, file='./outputs/factormatrix_clean.3rank.RData')

## Diagnostics:
# 1. check iterative progress ... want to see this dramatically flatten:
# plot(factormatrix_clean.3rank)

# 2. consensus map: within the chart, this should be pretty dramatically either red or blue: light coloration is an indicator of the model's ambiguity, or of mixed-ranking (which may be totally accurate for hte dataset):
# consensusmap(factormatrix_clean.3rank)

# 3. summary of model:
# summary(factormatrix_clean.3rank)
# summary(featureScore(factormatrix_clean.3rank))

# 4. fit evaluation:
# fit(factormatrix_clean.3rank)
# summary(fit(factormatrix_clean.3rank))
# summary(factormatrix_clean.3rank, target = factormatrix_clean)

# 5. Basis Map: Darker colors indicate higher coefficients, and the clusters on the left are colored to indicate the dominant rank within a cluster.
# basismap(factormatrix_clean.3rank)

# 6. Coefficient Map: (read the same as basis map)
# coefmap(factormatrix_clean.3rank)

# 7. Get products:
# write.csv(file = './outputs/test_matrix_3rank_basis.csv', x = basis(factormatrix_clean.3rank))
# write.csv(file = './outputs/test_matrix_3rank_coefs.csv', x = coef(factormatrix_clean.3rank))
# write.csv(file = './outputs/test_matrix_3rank_factor_predictions.csv', x = predict(factormatrix_clean.3rank, prob=TRUE))
# write.csv(file = './outputs/test_matrix_3rank_obs_predictions.csv', x = predict(factormatrix_clean.3rank, what="rows", prob=TRUE))

load("./outputs/factormatrix_clean.3rank.RData")
model.rank <- factormatrix_clean.3rank
