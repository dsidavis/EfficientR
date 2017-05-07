load("SessionState.rda")
source("simulation-fxns.r")

################################################################################
## Simulations
################################################################################
cat("Starting simulations\n")
nsims = 2 #1000
nsteps = 1000
Rprof("prof")
tm = system.time({sims = sim_fxn(nsims, nsteps, start_TS, vert_list, exit_latent, exit_inf, j_out)})
Rprof(NULL)

## sims_II = sim_par_fxn(1000, 270, start_TS, vert_list, exit_latent, exit_inf)






