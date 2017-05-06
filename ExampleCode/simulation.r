load("SessionState.rda")
source("simulation-fxns.r-orig")

################################################################################
## Simulations
################################################################################
cat("Starting simulations\n")
Rprof("prof")
nsims = 2 #1000
nsteps = 1000
sims = sim_fxn(nsims, nsteps, start_TS, vert_list, exit_latent, exit_inf, j_out)
Rprof(NULL)
## sims_II = sim_par_fxn(1000, 270, start_TS, vert_list, exit_latent, exit_inf)

## Saving simulation object
## -----------------------------------------------------------------------------
# saveRDS(sims, sprintf("Data/simulations/%s_seed-%s-in-nd-%s.rds",
                      # Sys.Date(), seed_inf_no, j[seed_row]))


seed_inf_no = 1
seed_row = 890
saveRDS(sims, sprintf("%s_seed-%s-in-nd-%s.rds",
                      Sys.Date(), seed_inf_no, seed_row))
# 
# 
# sim1 = sims[1:1000]
# sim2 = sims[1001:2000]
# 
# saveRDS(sim1, sprintf("Data/simulations/%s_seed-%s-in-nd-%s-1to1000.rds",
#                       Sys.Date(), seed_inf_no, seed_row))
# 
# saveRDS(sim2, sprintf("Data/simulations/%s_seed-%s-in-nd-%s.rd-1001to2000s",
#                       Sys.Date(), seed_inf_no, seed_row))



# ## miscellaneous
# length(sims[[1]])
# t = sims[[1]][[100]]
# table(t$I)
# which(t$I > 0)
# which(t$E > 0)



