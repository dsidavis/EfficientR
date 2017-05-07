l_ji_fxn = function(j_out, l_in_node){
    nodes = j_out$name
    sigmaProp_by_tau = j_out$sigmaProp_by_tau
    local_foi = l_in_node[ nodes ]
    df = sigmaProp_by_tau * local_foi
    sum(df, na.rm = TRUE)
}


foi_fxn = function(df_TS, vert_list, j_out){
    vert_info = vert_list[['vert_info']]
    comp1_sub = vert_list[['comp1_sub']]
    comp2_sub = vert_list[['comp2_sub']]
    
    vert_info$I = df_TS$I
    comp1_i = vert_info$I * comp1_sub
    comp2_i = vapply(comp2_sub, comp2_i_fxn, 0, structure(vert_info$I, names = vert_info$name))

    ## onwards to FOI
    l_in_node_val =  (vert_info$b_by_n * (comp1_i + comp2_i))/vert_info$sigma_by_tau_p1

    l_in_node = structure(l_in_node_val, names = vert_info$name)

    l_ji  = vapply(j_out, l_ji_fxn, 0, l_in_node)

    l_in_node + l_ji
}

comp2_i_fxn = function(comp, vI)
{
    df = vI[comp$name] * comp$comp2_sub
    sum(df, na.rm = TRUE)
}