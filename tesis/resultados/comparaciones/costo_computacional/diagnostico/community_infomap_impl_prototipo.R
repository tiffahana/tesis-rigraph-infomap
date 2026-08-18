function (graph, e_weights = NULL, v_weights = NULL, nb_trials = 10) 
{
    ensure_igraph(graph)
    if (is.null(e_weights) && "weight" %in% edge_attr_names(graph)) {
        e_weights <- E(graph)$weight
    }
    if (!is.null(e_weights) && !all(is.na(e_weights))) {
        e_weights <- as.numeric(e_weights)
    }
    else {
        e_weights <- NULL
    }
    if (is.null(v_weights) && "weight" %in% vertex_attr_names(graph)) {
        v_weights <- V(graph)$weight
    }
    if (!is.null(v_weights) && !all(is.na(v_weights))) {
        v_weights <- as.numeric(v_weights)
    }
    else {
        v_weights <- NULL
    }
    nb_trials <- as.numeric(nb_trials)
    on.exit(.Call(R_igraph_finalizer))
    res <- .Call(R_igraph_community_infomap, graph, e_weights, v_weights, nb_trials)
    res
}
