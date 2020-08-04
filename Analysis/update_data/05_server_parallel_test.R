library(doParallel)
ncores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
ncores
registerDoParallel(ncores)


iters <- 100000

# Start time
start <- Sys.time()

# Parallel Loop
result <- foreach(icount(iters)) %dopar% mean(rnorm(1e6))

# Time
print(Sys.time() - start)