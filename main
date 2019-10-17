##------ Thu Oct 17 10:33:36 2019 ------##

# =======================================
# Title: Generation of problem instances for the SSP-NPM
# =======================================
# Author: D. Calmels
# =======================================
# =======================================
# Description: Generates problem instances for the job sequencing and tool switching problem (SSP),
# and especially for non-identical parallel machines (NPM)

# param m,ma Machines
# param t Tools
# param j,job Jobs
# param cap Tool Capacities of the Machines // random integers from the interval [5, 7]
# param sw Tool Switching Time of the Machines // random integers from the interval [2, 4]
# param p Processing time of job on machine // random integers from the interval [1, 10]
# param dens Job-Tool-Matrix Density // "s" <- sparse (few tools per job), "d" <- dense (many tools per job)
# param var Number of tool-matrix variations for a single problem type
# data.matrix Matrix containing all parameters for the generated problem instance

# m	  j	          t	    Job-Tool-Matrix Density
# 2	  {5,10}	    10	  {sparse, dense}
# 2	  {5,10,15}	  15	  {sparse, dense}
# 3	  {15,25}	    15	  {sparse, dense}
# 4	  {25}	      15	  {sparse, dense}
# 4	  {25}	      20	  {sparse, dense}

# =======================================

instance <- 0
resamp <- function(x, ...) {
  if (length(x) == 1)
    x
  else
    sample(x, ...)
}

# set working directory
setwd("../Instances")

# =======================================
machines <- c(2:4)
var <- c(1:10) # number of matrix variations per problem type
# =======================================

# =======================================
# Start
# =======================================
for (m in machines) {
  if (m == 2)
    tools <- c(10, 15)
  if (m == 3)
    tools <- 15
  if (m == 4)
    tools <- c(15, 20)
  
  for (t in tools) {
    if (t == 10)
      jobs <- c(5, 10)
    if (t == 15 && m == 2)
      jobs <- c(5, 10, 15)
    if (t == 15 && m == 3)
      jobs <- c(15, 25)
    if (t == 15 && m == 4)
      jobs <- 25
    if (t == 20)
      jobs <- 25
    
    for (j in jobs) {
      for (dens in c("s", "d")) {
        #initial empty matrix
        data.matrix <- matrix(data = NA,
                              ncol = j,
                              nrow = t + m + 3)
        data.matrix[(m + 4):(t + m + 3), 1:j] <- 0
        # basic information
        data.matrix[1, 1] <- m
        data.matrix[1, 2] <- j
        data.matrix[1, 3] <- t
        # capacities
        cap = sample(c(5:7), m, replace = T)
        data.matrix[2, 1:m] <- cap
        # switching times
        sw = sample(c(2:4), m, replace = T)
        data.matrix[3, 1:m] <- sw
        
        # job tool matrix // loop over 10 variations
        for (loop in var) {
          instance <- instance + 1
          # generate processing times
          for (i in 1:j) {
            for (l in 4:(3 + m)) {
              data.matrix[l, i] <- sample(c(1:10), size = 1, replace = T)
            }
          }
          # for each job generate an auxiliary list that contains the required tools // tlist
          tlist <- list()
          tlist[1:j] <- 0
          for (job in 1:j) {
            
            # =======================================
            # sparse matrices
            # =======================================
            if (dens == "s") {
              # minimum capacity // min_cap
              min_cap <- min(data.matrix[2, 1:m])
              # compute required tools // r
              r <-
                sample(
                  x = c(1:t),
                  size = round(runif(
                    n = 1,
                    min = ceiling(min_cap / 4),
                    max = ceiling(min_cap / 2)
                  )),
                  replace = F
                )
              
              # as long as the required tool sample is a subset of any other sample, a new sample is generated
              for (sample in 2:job) {
                while (sum(setdiff(r, tlist[[sample]])) == 0) {
                  r <- sample(
                    x = c(1:t),
                    size = round(runif(
                      n = 1,
                      min = ceiling(min_cap / 4),
                      max = ceiling(min_cap / 2)
                    )),
                    replace = F
                  )
                }
              }
              
              # otherwise the tool loading is assigned to the auxiliary list tlist
              tlist[[job]] <- as.vector(r)
              
              # and the tools are entered in the matrix
              for (tool in r) {
                data.matrix[(3 + m + tool), job] = 1
              }
            }
            
            # =======================================
            # dense matrices
            # =======================================
            if (dens == "d") {
              max_cap <- max(data.matrix[2, 1:m])
              # compute required tools
              ### all groups
              r <- sample(
                x = c(1:t),
                size = round(runif(
                  n = 1,
                  min = ceiling(max_cap / 2),
                  max = max_cap
                )),
                replace = F
              )
              
              # as long as the required tool sample is a subset of any other sample, a new sample is generated
              for (sample in 2:job) {
                while (sum(setdiff(r, tlist[[sample]])) == 0) {
                  r <- sample(
                    x = c(1:t),
                    size = round(runif(
                      n = 1,
                      min = ceiling(max_cap / 2),
                      max = max_cap
                    )),
                    replace = F
                  )
                }
              }
              
              # otherwise it is assigned to the auxiliary list tlist
              tlist[[job]] <- as.vector(r)
              
              # and the tools are entered in the matrix
              for (tool in r) {
                data.matrix[(3 + m + tool), job] <- 1
              }
            }
          }
          remove(job)
          
          # =======================================
          # csv file generation // f1le
          # =======================================
          f1le = paste0(
            "./ins",
            instance,
            "_m=",
            m,
            "_j=",
            j,
            "_t=",
            t,
            "_dens=",
            dens,
            "_var=",
            loop,
            ".csv",
            sep = ""
          )
          if (file.exists(f1le) == T) {
            print("file already exists")
          }
          if (file.exists(f1le) == F) {
            write.table(
              data.matrix,
              paste(
                "./ins",
                instance,
                "_m=",
                m,
                "_j=",
                j,
                "_t=",
                t,
                "_dens=",
                dens,
                "_var=",
                loop,
                ".csv",
                sep = ""
              ),
              sep = ";",
              row.names = F,
              col.names = F
            )
          }
          data.matrix[(m + 4):(t + m + 3), 1:j] <- 0
        }
        remove(data.matrix, tlist)
      }
    }
  }
}

# =======================================
# End
# =======================================
