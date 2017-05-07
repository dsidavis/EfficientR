l_ji_fxn = function(j_out, l_in_node){
    nodes = j_out$name
    sigmaProp_by_tau = j_out$sigmaProp_by_tau
    local_foi = l_in_node$l_in_node[ match(nodes, l_in_node$name) ]
    df = sigmaProp_by_tau * local_foi
    df = sum(df, na.rm = TRUE)
    return(df)
}
