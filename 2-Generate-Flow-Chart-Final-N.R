( n_initial <- nrow(dat_real) )
( n_missing <- sum(is.na(dat_real$chronic_opiate_use)) )
( n_after <- nrow(dat_da) )
( n_excluded <- sum(dat_da$post_op_opiates_only == 1, na.rm = TRUE) )
( n_final <- nrow(dat_da_final) )

steps <- data.frame(
  id = 1:4,   # ← now 4 IDs
  label = c(
    paste0("Total number of Patients who have had irinotecan\n for GI malignancy in the last 5 years: N = ", n_initial),
    paste0("Excluded:\n",
           "• Missing status of chronic opiate use: n = ", n_missing, "\n",
           "Remaining N = ", n_after),
    paste0("Further exclusion:\n", 
           "• Patients who used opiates only after operation:\n",
           "n = ", n_excluded, " dropped\n",
           "Remaining N = ", (n_after - n_excluded)),
    paste0("Final analytic sample\nN = ", n_final)
  ),
  stringsAsFactors = FALSE
)

# 2. Create node data frame
nodes <- create_node_df(
  n = nrow(steps),
  label = steps$label,
  style = "filled, rounded",
  color = "black",
  fillcolor = "MistyRose",
  fontname = "Helvetica"
)

# 3. Define the edges (flow of subjects)
#    Here we simply link 1→2→3→4→5 in order.
edges <- create_edge_df(
  from = 1:(nrow(steps)-1),
  to = 2:nrow(steps),
  rel = "included_in"
)

# 4. Assemble and render the graph
graph <- create_graph(
  nodes_df = nodes,
  edges_df = edges,
  attr_theme = NULL
)

# render_graph(graph, layout = "tree")
render_graph(graph)

# Render the graph
graph_plot <- render_graph(graph)

# Convert to SVG
svg <- export_svg(graph_plot)

# Save as high-resolution PNG
rsvg_png(
  charToRaw(svg),
  file = "patient_flow_diagram.png",
  width = 2652.631,
  height = 1500
)
