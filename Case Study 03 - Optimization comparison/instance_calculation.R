suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(ExpDE))

# size_groups = 3

# instances <- c(2,3,4,38,39,40,75,76,77,112,113,114,148,149,150)
# instances <- c(2,6,11,15,20,24,29,33,38,42,47,51,56,60,65,69,74,78,83,
#                +87,92,96,101,105,110,114,119,123,128,132,137,141,146,150)
instances <- c(2,6,11,15,20,24,29,33,38,42,47,51,56,60,65,69,74,78,83,87,92,96,101)
instances <- c(105,110,114,119,123,128,132,137,141,146,150)

inst.data = data.frame(
  "instancia"=c(1),
  "dim"=c(1),
  "config"=c(1),
  "rep"=c(1),
  "resultado"=c(1)
)

rep = 30

## Config 1
recpars1 <- list(name = "recombination_arith")
mutpars1 <- list(name = "mutation_rand", f = 4)

## Config 2
recpars2 <- list(name = "recombination_bin", cr = 0.7)
mutpars2 <- list(name = "mutation_best", f = 3)

inst_count = 1
param_line = 1
for (dim in instances){
  fn <- function(X){
    if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
    Y <- apply(X, MARGIN = 1,
               FUN = smoof::makeRosenbrockFunction(dimensions = dim))
    return(Y)
  }
  # testing the function on a matrix composed of 2 points
  X <- matrix(runif(2*dim), nrow = 2)
  print(fn(X))
  
  selpars <- list(name = "selection_standard")
  stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
  probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
  popsize = 5 * dim
  
  print(paste("Instancia ", inst_count, "Dim ", dim))
  for (i in c(1:rep)){
    print(paste("Config 1 Rep:", i))
    out <- ExpDE(mutpars = mutpars1,
                  recpars = recpars1,
                  popsize = popsize,
                  selpars = selpars,
                  stopcrit = stopcrit,
                  probpars = probpars,
                  showpars = list(show.iters = "dots", showevery = 20))
    print(out$Fbest)
    inst.data[param_line,] <- list(inst_count,dim,1,i,out$Fbest)
    param_line <- param_line+1
  }
  for (i in c(1:rep)){
    print(paste("Config 2 Rep:", i))
    out <- ExpDE(mutpars = mutpars2,
                  recpars = recpars2,
                  popsize = popsize,
                  selpars = selpars,
                  stopcrit = stopcrit,
                  probpars = probpars,
                  showpars = list(show.iters = "dots", showevery = 20))
    print(out$Fbest)
    inst.data[param_line,] <- list(inst_count,dim,2,i,out$Fbest)
    param_line <- param_line+1
  }
  inst_count <- inst_count + 1
}

write.csv2(inst.data,".\\data\\inst_data.csv", row.names = FALSE)
