suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(ExpDE))

size_groups = 3

instances <- c(2,3,4,38,39,40,75,76,77,112,113,114,148,149,150)
instances <- matrix(instances,nrow=size_groups)

parameters.data = data.frame(
  "bloco"=c(1),
  "instancia"=c(1),
  "dim"=c(1),
  "config"=c(1),
  "rep"=c(1),
  "resultado"=c(1)
)

rep = 5

## Config 1
recpars1 <- list(name = "recombination_arith")
mutpars1 <- list(name = "mutation_rand", f = 4)

## Config 2
recpars2 <- list(name = "recombination_bin", cr = 0.7)
mutpars2 <- list(name = "mutation_best", f = 3)

group_count = 1
inst_count = 1
param_line = 1
for (group in instances){
  
  for (dim in group){
    print(dim)
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
    
    # config_data = c()
    # config2_data = c()
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
      parameters.data[param_line,] <- list(group_count,inst_count,dim,1,i,out$Fbest)
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
      parameters.data[param_line,] <- list(group_count,inst_count,dim,2,i,out$Fbest)
      param_line <- param_line+1
    }
    inst_count <- inst_count + 1
  }
  group_count <- group_count + 1
}

write.csv2(parameters.data,".\\data\\param_data.csv", row.names = FALSE)
