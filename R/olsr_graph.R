#' @importFrom cli cli_progress_bar cli_progress_update
#'     cli_progress_done pb_spin pb_current
#'
#' @importFrom dplyr tibble left_join pull bind_rows
get_relatives_graph <-
    function(ontology, ids, terms, relation)
{
    relations <- c("ancestors", "descendants")

    stopifnot(
        is_ontology(ontology),
        is_character(ids),
        is.null(terms) ||
            (is.data.frame(terms) && ("obo_id" %in% names(terms))),
        is_string(relation), relation %in% relations
    )
    relative <- ifelse(identical(relation, "ancestors"), "parents", "children")

    cli_progress_bar(
        format = "{cli::pb_spin} {cli::pb_current} relatives",
        total = NA
    )

    ## initialize queue of ids, node_ids visited, and edges tibble
    queue <- ids
    node_ids <- character()
    edges <- tibble(from = character(), to = character())

    ## process all nodes in the graph, including new parent / child
    while (length(queue)) {
        cli_progress_update()

        ## process next element in queue
        id <- head(queue, 1)
        queue <- tail(queue, -1)
        parents <- get_relatives("mondo", id, relative)
        if (!NROW(parents))
            next

        ## update 'edges'
        parent_ids <- pull(parents, "obo_id")
        edges <- bind_rows(edges, tibble(from = id, to = parent_ids))

        ## update queue
        queue <- union(queue, setdiff(parent_ids, node_ids))
        node_ids <- union(node_ids, parent_ids)
    }
    cli_progress_done()

    node_ids <- union(ids, node_ids)
    nodes <- tibble(obo_id = node_ids)
    if (!is.null(terms)) {
        ## add annotations from 'term'
        nodes <- left_join(nodes, terms, by = "obo_id")
    }

    ## construct return object -- a list of edge and node tibbles,
    ## with metadata
    olsr_graph(relation, ontology, ids, edges, nodes)
}

#' @rdname graph
#'
#' @title Working with Ancestor and Descendant Term Graphs
#'
#' @description `get_ancestors_graph()` and `get_descendants_graph()`
#'     returns a graph representation of all ancestors or descendants
#'     of term identifiers of interest.
#'
#' @param ontology character(1) name of the ontology the ids are from.
#'
#' @param ids character() OBO identifiers for which the graph is to be
#'     constructed.
#'
#' @param terms a tibble from, e.g., `terms()`, containing a column
#'     `obo_id` and additional columns to annotated nodes of
#'     the returned graph.
#'
#' @details
#'
#' Ancestor and descendant graphs navigate from the ids of interest
#' via parents or children, recursively, until either the ontology
#' root or only leaf nodes remain. For descendant graphs, this can
#' involve hundreds or even thousands of calls to the OLS API, and can
#' be time consuming (e.g., a minute or so) on first invocation. API
#' calls are memoized, so fast on subsequent invocations.
#'
#' @return `get_ancestors_graph()` and `get_descendants_graph()`
#'     return an `olsr_graph` object recording the ontology, ids,
#'     edges, and nodes implied by the ancestor or descendant
#'     graph. Nodes are annotated with information from `terms`, if
#'     available.
#'
#' @examples
#' cl <- get_terms("cl")
#' b_cell <- "CL:0000236"
#' anc <- get_ancestors_graph("cl", b_cell, cl)     # 11 nodes
#' anc
#'
#' @export
get_ancestors_graph <-
    function(ontology, ids, terms = NULL)
{
    get_relatives_graph(ontology, ids, terms, "ancestors")
}

#' @rdname graph
#'
#' @examples
#' desc <- get_descendants_graph("cl", b_cell, cl)  # 64 nodes
#' desc
#'
#' @export
get_descendants_graph <-
    function(ontology, ids, terms = NULL)
{
    get_relatives_graph(ontology, ids, terms, "descendants")
}

#' @rdname graph
#'
#' @description `olsr_graph()` constructs an `olsr_graph` from
#'     *R* objects.
#'
#' @param relation character(1) type of graph, either `"ancestors"` or
#'     `"descendants"`.
#'
#' @param ontology character(1) name of ontology; must be present in
#'     the `id` column of `get_ontologies().
#'
#' @param ids character() of identifiers used to construct the graph.
#'
#' @param nodes tibble() with column `obo_id`, one for each node in
#'     the graph. Arbitrary additional columns are allowed.
#'
#' @param edges tibble() with columns `from` and `to`, containing OBO
#'     identifiers at the start and end of each node in the graph.
#'
#' @details
#'
#' `olsr_graph()` provides a simple representation to coordinate
#' information for ancestor and descendant graphs. The main purpose of
#' this class is as a 'way-point' for further analysis of the graph,
#' e.g., using the igraph package
#' <https://CRAN.R-project.org/package=igraph> for analytic work or
#' the visNetwork <https://CRAN.R-project.org/package=visNetwork> for
#' interactive visualization.
#'
#' @return `olsr_graph()` returns an object of class `olsr_graph`.
#'
#' @export
olsr_graph <-
    function(relation, ontology, ids, edges, nodes)
{
    stopifnot(
        is_string(relation),
        relation %in% c("ancestors", "descendants"),
        is_ontology(ontology),
        is_character(ids),
        is.data.frame(edges), all(c("from", "to") %in% names(edges)),
        is.data.frame(nodes), "obo_id" %in% names(nodes),
        all(unlist(edges[c("from", "to")]) %in% nodes$obo_id)
    )

    structure(
        list(
            ontology = ontology,
            ids = ids,
            nodes = nodes,
            edges = edges
        ),
        class = c(glue("{relation}_graph"), "olsr_graph", "list")
    )
}

olsr_graph_elt <-
    function(x, elt)
{
    stopifnot(
        inherits(x, "olsr_graph"),
        is_string(elt),
        elt %in% names(x)
    )
    x[[elt]]
}

#' @rdname graph
#'

#' @description `olsr_graph_edges()`, `olsr_graph_nodes()`,
#'     `olsr_graph_ontology()`, and `olsr_graph_ids()` extract
#'     elements of an `olsr_graph`.
#'
#' @examples
#' olsr_graph_ontology(anc)
#' olsr_graph_ids(anc)
#' olsr_graph_nodes(anc)
#' olsr_graph_edges(anc)
#'
#' @export
olsr_graph_ontology <- function(x) olsr_graph_elt(x, "ontology")

#' @rdname graph
#'
#' @export
olsr_graph_ids <- function(x) olsr_graph_elt(x, "ids")

#' @rdname graph
#'
#' @export
olsr_graph_nodes <- function(x) olsr_graph_elt(x, "nodes")

#' @rdname graph
#'
#' @export
olsr_graph_edges <- function(x) olsr_graph_elt(x, "edges")

#' @rdname graph
#'
#' @description `olsr_graph_as_visNetwork()` converts an `olsr_graph` to
#'     the format used by the visNetwork package for interactive
#'     visualization.
#'
#' @param graph an `olsr_graph` object.
#'
#' @details
#'
#' `olsr_graph_as_visNetwork()` renames the `obo_id` column of `nodes`
#' to `id`, consistent with visNetwork. The `id` column is copied to
#' `title`, so that it is used in mouse-over text; a `label` (the
#' short description of each term in the tibble returned by `terms()`)
#' is used to label the node on the graph. Add a column `color` to
#' `nodes` to color individual nodes
#'
#' @return `olsr_graph_as_visNetwork()` returns an object created by a
#'     call to `visNetwork::visNetwork()`. This object can be used for
#'     simple interactive visualization directly, or as input to
#'     subsequent visNetwork formatting.
#'
#' @examples
#' vis <- olsr_graph_as_visNetwork(anc)
#' if (interactive())   # visualize in RStudio  or browser
#'     vis
#'
#' @importFrom dplyr rename
#'
#' @export
olsr_graph_as_visNetwork <-
    function(graph)
{
    requireNamespace("visNetwork", quietly = TRUE)

    ## colorspace::qualitative_hcl(2, "Set 2")
    nodes <-
        olsr_graph_nodes(graph) |>
        ## visNetwork expects the node identifier to have column name
        ## 'id'
        rename(id = "obo_id") |>
        mutate(
            ## 'title' is used as the mouse-over text
            title = .data$id
        )

    visNetwork::visNetwork(nodes, olsr_graph_edges(graph)) |>
        visNetwork::visEdges(arrows = "from")
}

igraph_as_tibble <-
    function(graph, what = c("edges", "vertices"))
{
    requireNamespace("igraph", quietly = TRUE)
    what <- match.arg(what)

    igraph::as_data_frame(graph, what = what) |>
        as_tibble()
}

igraph_edges_as_tibble <-
    function(graph)
{
    igraph_as_tibble(graph, "edges")
}

igraph_nodes_as_tibble <-
    function(graph)
{
    igraph_as_tibble(graph, "vertices") |>
        rename(obo_id = "name")
}

#' @rdname graph
#'
#' @description `olsr_graph_as_igraph()` and `igraph_as_olsr_graph()`
#'     transform `olsr_graph` objects to and from `igraph` objects.
#'
#' @details
#'
#' `olsr_graph_as_igraph()` includes information on relation
#' (ancestors or descendants), ontology, and ids in the `igraph`
#' object as a graph-level attribute available with `graph_attr(ig,
#' "olsr_graph")`. Workflows that use `olsr_graph_as_igraph()`,
#' perform igraph manipulations, and then use `igraph_as_olsr_graph()`
#' generally preserve this information.
#'
#' @return `olsr_graph_as_igraph()` returns an `igraph` object;
#'     `igraph_as_olsr_graph()` returns an `olsr_graph` object.
#'
#' @examples
#' ig <- olsr_graph_as_igraph(anc)
#' ig
#' igraph_as_olsr_graph(ig)
#'
#' @importFrom utils head tail
#'
#' @export
olsr_graph_as_igraph <-
    function(graph)
{
    requireNamespace("igraph", quietly = TRUE)

    stopifnot(inherits(graph, "olsr_graph"))

    vertices <-
        olsr_graph_nodes(graph) |>
        ## igraph requires first column to be identifier
        select("obo_id", everything())
    edges <- olsr_graph_edges(graph)

    ig <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices)
    igraph::graph.attributes(ig) <-
        list(
            olsr_graph = list(
                relation = sub("_.*", "", head(class(graph), 1L)),
                ontology = olsr_graph_ontology(graph),
                ids = olsr_graph_ids(graph)
            )
        )
    ig
}

#' @rdname graph
#'
#' @param igraph an object of class `igraph`, from the igraph package.
#'
#' @export
igraph_as_olsr_graph <-
    function(igraph)
{
    requireNamespace("igraph", quietly = TRUE)

    stopifnot(igraph::is_igraph(igraph))

    nodes <- igraph_nodes_as_tibble(igraph)
    edges <- igraph_edges_as_tibble(igraph)

    relation <- ontology <- ids <- character()
    olsr <- igraph::graph_attr(igraph, "olsr_graph")
    if (!is.null(olsr)) {
        olsr_graph(olsr$relation, olsr$ontology, olsr$ids, edges, nodes)
    } else {
        olsr_graph(edges = edges, nodes = nodes)
    }
}

#' @rdname graph
#'
#' @param x for `print.olsr_graph()`, an object of class `olsr_graph`.
#'
#' @param ... for `print.olsr_graph()`, ignored.
#'
#' @export
print.olsr_graph <-
    function(x, ...)
{
    class <- head(class(x), 1)
    n_nodes <- NROW(olsr_graph_nodes(x))
    n_edges <- NROW(olsr_graph_edges(x))
    ontology <- olsr_graph_ontology(x)
    ids <- olsr_graph_ids(x)
    id_string <- toString(select_some(ids, 4))

    cat(glue(
        "class: {class} ({n_nodes} nodes x {n_edges} edges)\n",
        "ontology: {ontology}\n",
        "ids ({ length(ids) }): {id_string}"
    ), "\n", sep = "")
}
