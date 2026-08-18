function (graph, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE) 
{
    res <- community_infomap_impl(graph = graph, e_weights = e.weights, v_weights = v.weights, nb_trials = nb.trials)
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "infomap"
    res$membership <- res$membership + 1
    if (modularity) {
        res$modularity <- modularity(graph, res$membership, weights = e.weights)
    }
    class(res) <- "communities"
    res
}
