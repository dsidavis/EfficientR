################################################################################
## Calculating Sigma (total commuting prop for each node
################################################################################
sigma_graph_fxn = function(g) {
    edges = igraph::as_data_frame(g, "edges")
    from = unique(edges$from)
    sigma = lapply(from, function(from, edges) {
                            df = edges[ edges$from %in% from, ]
                            s = sum(df$commuting_prop)
                            df = data.frame(from = from,
                                            sigma = s,
                                            stringsAsFactors = FALSE)
        return(df)
    }, edges)
    sigma_df = do.call(rbind, sigma)
    return(sigma_df)
}




################################################################################
## Effective population
################################################################################

## Based on the formula written for simplified model (on green page)
## -----------------------------------------------------------------------------

effective_pop_II = function(i, j, g, tau = 3) {
    ## Populations of neighboring incoming nodes 'i'
    N_i = vertex_attr(g, "pop", i)

    ## Commuting proportion of 'i'
    sigma_i = vertex_attr(g, "sigma", i)

    ## Commuting proportion from 'i' to 'j'
    sigma_ij = edge_attr(g, "commuting_prop", paste0(i, "|", j))

    ## Second component of formula:
    N_ij = N_i * ((sigma_ij/tau) / (1 + (sigma_i/tau)))
    N_ij = as.data.frame(N_ij)
    return(N_ij)
}

    

effective_pop_I = function(j, g, tau = 3) {
    ## Populations of node 'j'
    N_j = vertex_attr(g, "pop", j)

    ## Commuting proportion of 'j'
    sigma_j = vertex_attr(g, "sigma", j)

    ## Solve first component of Nj* formula
    N_jj = N_j / (1 + (sigma_j/tau))

    ## Identify neighboring incoming nodes, 'i'
    i = names(neighbors(g, j, "in"))

    ## Calulate the individual second components of formula,
    ## which are then summed up
    N_ij = lapply(i, effective_pop_II, j, g)
    N_ij = do.call(rbind, N_ij)
    sum_N_ij = sum(N_ij)

    ## N_eff_j = Nj*, the effectove population
    N_eff_j = N_jj + sum_N_ij
    return(N_eff_j)
}

 
effective_pop_fxn = function(g) {
    j = vertex_attr(g, "name")
    eff_pop = lapply(j, effective_pop_I, g)
    eff_pop = do.call(rbind, eff_pop)
    eff_pop = round(eff_pop)
    ## Include the eff pop as a vertex attribute in graph
    g = set_vertex_attr(g, "eff_pop", value = eff_pop)
    return(g)
}



################################################################################
## Add sigma and sigmaProp_by_tau
################################################################################

add_sigmas_fxn = function(g, tau){
    ## calculate sigma_by_tau and sigmaProp_by_tau
    sigma_by_tau = vertex_attr(g, "sigma")/tau
    sigmaProp_by_tau = edge_attr(g, "commuting_prop")/tau

    ## add the two as vertex and edge attribute respectively
    g = set_vertex_attr(g, "sigma_by_tau", value = sigma_by_tau)
    g = set_edge_attr(g, "sigmaProp_by_tau", value = sigmaProp_by_tau)
    return(g)
}




################################################################################
## Initializing start_TS dataframe
################################################################################

start_TS_fxn = function(g){
    verts = igraph::as_data_frame(g, "vertices")
    S = round(vertex_attr(g, "pop"))
    start_TS = data.frame(name = verts$name,
                 S = S,
                 E = 0,
                 I = 0,
                 R = 0,
                 stringsAsFactors = FALSE)
    return(start_TS)
}



################################################################################
## Seed node function
################################################################################

seed_nd_fxn = function(df, nd, inf){
    seed_row = which(df$name %in% nd)
    df[seed_row, c("S", "I")] = c(df$S[seed_row] -  inf, inf)
    return(df)
}

    






################################################################################
## FOI Take II!
################################################################################


get_vertInfo_fxn = function(g, beta){
    df = igraph::as_data_frame(g, "vertices")
    df = df[  , c("name", "eff_pop", "sigma_by_tau")]
    df$b_by_n = beta/df$eff_pop
    df$sigma_by_tau_p1 = df$sigma_by_tau + 1
    return(df)
}



## net_neighbors_fxn
## -----------------------------------------------------------------------------
## Gets neighbor info specific to whether the edge is incoming or outgoing
##      i.e.  2 neighbor modes: "in" or "out"
## 
## I. For incoming edges, it provides:
##    -------------------------------
##      1. name of neighbors commuting to node
##      2. sigma_by_tau_p1: sigma_by_tau + 1
##      3. sigmaProp_by_tau: proportion of neighbor pop commuting to node
##
## II. For outgoing edges, it provides:
##     -------------------------------
##      1. name of neighbors to which node is commuting to
##      2. sigmaProp_by_tau: proportion of node pop commuting to neighbor
##

net_neighbors_fxn = function(vert, g, m, vert_info){
    neigh_name = names(neighbors(graph = g, v = vert, mode = m))
    if(length(neigh_name) != 0){
        if(m == "in"){
            from = neigh_name
            to = vert
            sigma_by_tau_p1 = vert_info$sigma_by_tau_p1[
                vert_info$name %in% neigh_name ]
            sigmaProp_by_tau = edge_attr(g, "sigmaProp_by_tau",
                                         paste0(from, "|", to))
            df = data.frame(name = neigh_name,
                            sigma_by_tau_p1 = sigma_by_tau_p1,
                            sigmaProp_by_tau = sigmaProp_by_tau,
                            stringsAsFactors = FALSE)
        } else {
            from = vert
            to = neigh_name
            sigmaProp_by_tau = edge_attr(g, "sigmaProp_by_tau",
                                         paste0(from, "|", to))
            df = data.frame(name = neigh_name,
                            sigmaProp_by_tau = sigmaProp_by_tau,
                            stringsAsFactors = FALSE)
        }   
    } else {
        df = data.frame(NULL)
    }
    return(df)
}



## j_in function (wraps up the net_neighbors_fxn for incoming edges)
## -----------------------------------------------------------------------------
j_in_fxn = function(vert_info, g, m = "in"){
    j = vert_info$name
    j_in = lapply(setNames(j, j), net_neighbors_fxn, g, m = m, vert_info)
    return(j_in)
}



## j_out function (wraps up the net_neighbors_fxn for outgoing edges)
## -----------------------------------------------------------------------------
# Get rid of this as it is identical to j_in_fxn except m = "in"/"out" so make
# a parameter and remove this 
#j_out_fxn = function(vert_info, g){
#    j = vert_info$name
#    j_out = lapply(setNames(j, j), net_neighbors_fxn, g, m = "out", vert_info)
#    return(j_out)
#}


## component 2 sub (minus I)
## -----------------------------------------------------------------------------
comp2_sub_fxn = function(j_in){
    if(length(j_in) != 0){
        name = j_in$name
        comp2_sub = j_in$sigmaProp_by_tau/j_in$sigma_by_tau_p1
        df = data.frame(name = name,
                        comp2_sub = comp2_sub,
                        stringsAsFactors = FALSE)
    } else {
        df = data.frame(NULL)
    }
    
    return(df)
}


## component 2 with i
## -----------------------------------------------------------------------------
#comp2_i_fxn.indices = list()
#comp2_i_fxn.indices[[ length(comp2_i_fxn.indices) + 1L]] <<- c(match(c("I", "name"), names(vert_info)), match(c("name", "comp2_sub"), names(comp)))
#recover()

genAddTime =
function(num)
{
    times = numeric(num)
    ctr = 1
    add = function(start, end = Sys.time()) {
        times[ctr] <<- end - start
        ctr <<- ctr + 1
    }
    list(addTime = add, times = function() times)
}

comp2_i_fxn.timer = genAddTime(68742)

comp2_i_fxn = function(comp, vI)#, vname)
{
#   cur = Sys.time(); on.exit(comp2_i_fxn.timer$addTime(cur))
    df = vI[comp$name] * comp$comp2_sub
    sum(df, na.rm = TRUE)
}


## calculate l_ji part
## -----------------------------------------------------------------------------
l_ji_fxn.indices = list()

l_ji_fxn.timer = genAddTime(68742)

l_ji_fxn = function(j_out, l_in_node, name){
#    cur = Sys.time(); on.exit(l_ji_fxn.timer$addTime(cur))
#l_ji_fxn.indices[[ length(l_ji_fxn.indices) + 1L]] <<- match(c("name", "sigmaProp_by_tau"), names(j_out))
    local_foi = l_in_node[ j_out$name ]
    df = j_out$sigmaProp_by_tau * local_foi
    sum(df, na.rm = TRUE)
}



## FOI
## -----------------------------------------------------------------------------
foi_fxn.indices = list()
# Goes back into function after vert_info$I = 
# foi_fxn.indices[[length(foi_fxn.indices) + 1L]] <<- c(match(c("vert_info", "comp1_sub", "comp2_sub"), names(vert_list)),
#                                                       match("I", names(df_TS)),
#                                                       match(c("I", "sigma_by_tau_p1", "name", "b_by_n"), names(vert_info)))

foi_fxn = function(df_TS, vert_list, j_out){

    vert_info = vert_list[[1]] # vert_info
    comp1_sub = vert_list[[2]] # comp1_sub
    comp2_sub = vert_list[[3]] # comp2_sub
    
    vert_info$I = df_TS[[4]] # $I

                      #$I
    comp1_i = vert_info[[6]] * comp1_sub                       # $I
    comp2_i = sapply(comp2_sub, comp2_i_fxn, structure(vert_info[[6]], names = vert_info$name))

    ## onwards to FOI
                            # $b_by_n 
    l_in_node_val = (vert_info[[4]]* (comp1_i + comp2_i))/vert_info[[5]] # $sigma_by_tau_p1

    l_ji  = sapply(j_out, l_ji_fxn, l_in_node_val, vert_info[[1]])

    l_in_node_val + l_ji
}

## ## Testing that the empty dfs are the same for both comp and j_in
## l = lfun(comp2_i)

## lfun = function(x) {
##     df = lapply(x, length)
##     df = do.call(rbind, df)
##     return(df)
## }

## l_j_in = lfun(j_in)
## which(l_j_in == 0) == which(l == 0)





################################################################################
## Functions to transition between SEIR compartments
################################################################################

S_to_E = function(S, p)
    rbinom(n = 1, size = S, prob = p)



E_to_I = function(E, p)
    rbinom(n = 1, size = E, prob = p)



I_to_R = function(I, p)
    rbinom(n = 1, size = I, prob = p)





################################################################################
## Sim Take II
################################################################################

sim_lapply_fxn = function(sim, nsteps, start_TS, vert_list, exit_latent, exit_inf, j_out){
    ## browser()
    TS = vector("list", nsteps)
    prev_TS = start_TS
    cat("-------------------------------------------------------\n")
    cat("******** Simulation ", sim, "\n") 

    for(i in 1:nsteps){
        cat("\r\t\t\tTimestep: ", i, "/", nsteps, sep = "")
if(FALSE) {
        E = mapply(S_to_E, prev_TS$S, prev_TS$foi)            
        I = sapply(prev_TS$E, E_to_I, exit_latent)
        R = sapply(prev_TS$I, I_to_R, exit_inf)
} else {
        n = nrow(prev_TS)    
        E = rbinom(n, prev_TS$S, prev_TS$foi)
        I = rbinom(n, prev_TS$E, exit_latent)
        R = rbinom(n, prev_TS$I, exit_inf)        
       }
#browser()        
        new_S = prev_TS$S - E
        new_E = prev_TS$E + E - I
        new_I = prev_TS$I + I - R
        new_R = prev_TS$R + R

        new_TS = data.frame(name = prev_TS$name,
                            S = new_S,
                            E = new_E,
                            I = new_I,
                            R = new_R,
                            stringsAsFactors = FALSE)                          

        if((sum(new_E) + sum(new_I)) == 0){
            TS[[i]] = new_TS
            break
        }

        new_TS$foi = foi_fxn(new_TS, vert_list, j_out)
        TS[[i]] = new_TS
        prev_TS = new_TS
    }
    cat("\n")
    return(TS)
}

    
    
sim_fxn = function(nsims, nsteps, start_TS, vert_list, exit_latent, exit_inf, j_out) {
    set.seed(0)
    sim_res = lapply(1:nsims, sim_lapply_fxn, nsteps, start_TS = start_TS,
                     vert_list = vert_list,
                     exit_latent = exit_latent,
                     exit_inf = exit_inf,
                     j_out = j_out)
    return(sim_res)
}

    
 
## Parallelized simulations


sim_parlapply_fxn = function(sim, nsteps, start_TS, vert_list, exit_latent, exit_inf){
    ## browser()
    TS = list()
    prev_TS = start_TS
    ## cat("-------------------------------------------------------\n")
    ## cat("******** Simulation ", sim, "\n") 
    print(paste0("Simulation: ", sim))
    for(i in 1:nsteps){
        ## cat("\r\t\t\tTimestep: ", i, "/", nsteps, sep = "")
        E = mapply(S_to_E, prev_TS$S, prev_TS$foi)
        I = do.call(rbind, lapply(prev_TS$E, E_to_I, exit_latent))
        R = do.call(rbind, lapply(prev_TS$I, I_to_R, exit_inf))
        new_S = prev_TS$S - E
        new_E = prev_TS$E + E - I
        new_I = prev_TS$I + I - R
        new_R = prev_TS$R + R

        new_TS = data.frame(name = prev_TS$name,
                            S = new_S,
                            E = new_E,
                            I = new_I,
                            R = new_R,
                            stringsAsFactors = FALSE)                          

        if((sum(new_TS$E) + sum(new_TS$I)) == 0){
            TS[[i]] = new_TS
            break
        }

        new_TS$foi = foi_fxn(new_TS, vert_list)
        TS[[i]] = new_TS
        prev_TS = new_TS
    }
    ## cat("\n")
    return(TS)
}




sim_par_fxn = function(nsims, nsteps, start_TS, vert_list, exit_latent, exit_inf
                      ) {
    set.seed(0)
    require(parallel)
    sim_res = mclapply(1:nsims, sim_parlapply_fxn, nsteps, start_TS = start_TS,
                        vert_list = vert_list,
                        exit_latent = exit_latent,
                        exit_inf = exit_inf
                        )
    return(sim_res)
}
