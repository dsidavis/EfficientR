################################################################################
## This code:
## 1. Reads in an igraph object (with commuting proportions previously
##      calculated)
## 
## 2. Calculates and attaches  "Effective Population" to graph
##
## 3. Seeds a node with specified number of infectious individuals (taking them
##      out from the Susceptibles and adding to Infectious compartment
##
## 4. Creates the FOI structure/scaffold used to quickly calculate FOI
##      (FOI = Force of Infection, acting on an individual in a node)
##
## 5. Simulates simplified SEIR models
################################################################################


## Parameter values used in python. UPDATE: See parameter reference below-------
## "p_asymptomatic": 0.2,
##   "asym_downscaler": 1,
##   "r0": 1.31,
##   "p_exit_latent": 0.37037037037037035, = 2.7
##   "p_recovery": 0.3389830508474576, = 2.95

##   "starting_node": "n890",
##   "seeds": 1,

##   "vaccinated_node": "n890",
##   "p_vaccinated": 1.0,
##   "p_vaccine_effectiveness": 0.6,
  
##   "nb_simulations": 3,
##   "timesteps_per_simul": 1000,
  
##   "commuting_return_rate": 3,
## "p_travel_allowed": 1.0,



## Libraries
## -----------------------------------------------------------------------------
library(igraph)
library(parallel)

## -----------------------------------------------------------------------------
## Parameter reference:
## -----------------------------------------------------------------------------
## Ref: Tuite, Ashleigh R., Amy L. Greer, Michael Whelan, Anne-Luise Winter,
## Brenda Lee, Ping Yan, Jianhong Wu, et al. 2010. “Estimated Epidemiologic
## Parameters and Morbidity Associated with Pandemic H1N1 Influenza.”
## CMAJ: Canadian Medical Association Journal =
## Journal de l’Association Medicale Canadienne 182 (2): 131–36.
## -----------------------------------------------------------------------------


## Load functions
## -----------------------------------------------------------------------------
source("simulation-fxns.r-orig")


if(!file.exists("SessionState.rda")) {
    
r0 = 1.31
latent_period = 2.62
exit_latent = 1/latent_period
inf_period = 3.38
exit_inf = 1/inf_period
mu = exit_inf
tau = 3 # Return rate
beta = r0/inf_period

## p_no_traveling= 0.045
## p_travel = 1 - p_no_traveling
## p_travel = 1    # no restructions on traveling



################################################################################
## Read in graph data
################################################################################

g = read.graph("2016-08-18_graph-pruned-by-ind.graphml",
               format = "graphml")

# ## Not really needed, but helpful initially in understanding graph structure
# verts = igraph::as_data_frame(g, "vertices")
# edges = igraph::as_data_frame(g, "edges")

## neighbors(g, "0", mode = "all") # e.g. of finding neighbors for node "0"



################################################################################
## Calculate effective population and other required variables
################################################################################


## Calculate effective population and add it as a vertex attribute to graph
## -----------------------------------------------------------------------------
g = effective_pop_fxn(g)


## Calculate sigma by tau values (to avoid recalculation each time)
## -----------------------------------------------------------------------------
## sigma_by_tau refers to the vertex attribute sigma (the proportion of a
##      node's population that is commuting) divided by the tau value (return
##      rate) specified by us

## sigmaProp_by_tau is the edge attribute (commuting_prop) which specifies the
##      proportion of a node's population commuting to another specified node)
##      divided by tau

g = add_sigmas_fxn(g, tau)

## ## For e.g., considering the first edge, we see that its name is "0|4"
## str(E(g)[1])

## ## The proportion of individuals of node "0" commuting to "4" is:
## edge_attr(g, "commuting_prop", index = paste0(0, "|", 4))
## cp = edge_attr(g, "commuting_prop", index = paste0(0, "|", 4))

## ## The above proportion divided by tau is sigmaProp_by_tau:
## cp/3

## ## Which is what we assigned as an edge attribute in the graph
## edge_attr(g, "sigmaProp_by_tau", index = paste0(0, "|", 4))





################################################################################
## Initializing the Time Step list and data frame
################################################################################

## This is a dataframe containing the node names along with their respective
##      compartment values (SEIR). It is used to calculate FOI and passed into
##      the simulations.
##      To begin with, S is the node population, and EIR are all 0
start_TS = start_TS_fxn(g)



################################################################################
## Seeding node of interest
################################################################################

## Takes in the inititalized start_TS dataframe, the node to be seeded, along
##      with the number of infectious individuals. It removes individuals from
##      the S to I compartments.

start_TS = seed_nd_fxn(start_TS, "890", 1)

## ## checking;
## seed_row = which(start_TS$name == "890")
## start_TS[seed_row, ]



################################################################################
## Force of infection: May the force be with you ^^
################################################################################

## Getting vert specific info for FOI:
## -----------------------------------------------------------------------------
## This is done to simplify foi calculations. "vert_info" contains:
## 
##      1. name: node name
##         ---- 
## 
##      2. eff_pop: effective population
##         ------ 
## 
##      3. sigma_by_tau: the total commuting proportion for a node (sigma)
##         ------------  divided by the return rate (tau)
## 
##      4. b_by_n: the beta coefficient divided by the effective population
##         ------
## 
##      5. sigma_by_tau_p1:  sigma_by_tau + 1
##         ---------------  (p1 stands for plus 1)


vert_info = get_vertInfo_fxn(g, beta)


## j_in information for lambda_jj part of FOI formula
## -----------------------------------------------------------------------------
## For each incoming edge to j (the "i"s in lambda_jj part of FOI formula),
## essentially, the second component info for lambda_jj:
##      1. name of neighbors commuting to node
##      2. sigma_by_tau_p1: sigma_by_tau + 1
##      3. sigmaProp_by_tau: proportion of neighbor pop commuting to node

j_in = j_in_fxn(vert_info, g)



## j_out information for lambda_ji part of FOI formula
## -----------------------------------------------------------------------------
## For each outgoing edge from j (the "i"s in the lambda_ji part of FOI formula):
##      1. name of neighbors to which node is commuting to
##      2. sigmaProp_by_tau: proportion of node pop commuting to neighbor

j_out = j_in_fxn(vert_info, g, m = "out")



## component 1 and 2 (sub)
## -----------------------------------------------------------------------------
## essentially the scaffold, to which "I" information is added to yield FOI
comp1_sub = 1/vert_info$sigma_by_tau_p1
comp2_sub = lapply(setNames(j_in, names(j_in)), comp2_sub_fxn)




## Defining vert_list, which contains all information needed for FOI
## -----------------------------------------------------------------------------
vert_list = list(vert_info = vert_info,
                 comp1_sub = comp1_sub,
                 comp2_sub = comp2_sub,
                 j_in = j_in,
                 j_out = j_out)


start_TS$foi = foi_fxn(start_TS, vert_list, j_out)
} else
   load("SessionState.rda")

stop("not running the remainder")

################################################################################
## Simulations
################################################################################
nsims = 2 #1000
nsteps = 1000
cat("Starting simulations\n")
Rprof("prof")
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



