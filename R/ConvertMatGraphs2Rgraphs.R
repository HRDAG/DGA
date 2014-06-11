library(R.matlab)
#read in matlab files, output models in R format.
# what I would like is something that is like graph <- list()
#graphs[[i]]$C are the cliques of the i'th model
#graphs[[i]]$S are the separators of the i'th model

num_lists <- 3
mod <- readMat(paste('~/Dropbox/Projects/Capture-Recapture/DGA/R/input/decomp_', num_lists,'_way.mat', sep=''))
g <- mod$Graphs

remove.empties <- function(x){
  emp <- sapply(x, length)
  if(sum(emp)==0){
    x <- NULL
  }else{
    if(sum(emp==0)==0){
      x <- x
    }else{
      x <- x
      get.rid <- which(emp==0)
      for(k in 1:length(get.rid)){
        x[[get.rid[k]]] <- NULL 
        get.rid <- get.rid - 1
      }
    }
  }
  return(x)
}

graphs <- list()
j <- 1
for(i in seq(1, length(g),2)){
  graphs[[j]] <- list()
  graphs[[j]]$C <- g[[i]][1,,]
  graphs[[j]]$S <- remove.empties(g[[i+1]][1,,])
  j <- j+1
}

save(graphs, file = paste("~/Dropbox/Projects/Capture-Recapture/DGA/R/output/graphs", num_lists, '.Rdata', sep=''))



for(i in 1:822){
  cl <- sapply(graphs[[i]]$S, length)
  if(length(cl)>0){
    m <- max(cl)
    if(m > 2){ print(i)}
  }
}