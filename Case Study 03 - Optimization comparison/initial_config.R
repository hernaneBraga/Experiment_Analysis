install.packages("ExpDE")
install.packages("smoof")

suppressPackageStartupMessages(library(smoof))
dim <- 10
fn <- function(X){
  if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
  Y <- apply(X, MARGIN = 1,
             FUN = smoof::makeRosenbrockFunction(dimensions = dim))
  return(Y)
}
# testing the function on a matrix composed of 2 points
X <- matrix(runif(2*dim), nrow = 2)
fn(X)

# FOR INSTANCE: set dim = 10
# dim <- 10
selpars <- list(name = "selection_standard")
stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
popsize = 5 * dim

# Equipe A
## Config 1
recpars1 <- list(name = "recombination_arith")
mutpars1 <- list(name = "mutation_rand", f = 4)

suppressPackageStartupMessages(library(ExpDE))
# Run algorithm on problem:
out1 <- ExpDE(mutpars = mutpars1,
             recpars = recpars1,
             popsize = popsize,
             selpars = selpars,
             stopcrit = stopcrit,
             probpars = probpars,
             showpars = list(show.iters = "dots", showevery = 20))
# Extract observation:
out1$Fbest

## Config 2
recpars2 <- list(name = "recombination_bin", cr = 0.7)
mutpars2 <- list(name = "mutation_best", f = 3)

# Run algorithm on problem:
out2_results10 = c()
for (i in 1:10){
  out2 <- ExpDE(mutpars = mutpars2,
                recpars = recpars2,
                popsize = popsize,
                selpars = selpars,
                stopcrit = stopcrit,
                probpars = probpars,
                showpars = list(show.iters = "dots", showevery = 20))
  out2_results10[i] = out2$Fbest
}
out2 <- ExpDE(mutpars = mutpars2,
              recpars = recpars2,
              popsize = popsize,
              selpars = selpars,
              stopcrit = stopcrit,
              probpars = probpars,
              showpars = list(show.iters = "dots", showevery = 20))
# Extract observation:
out2$Fbest