( vars <- colnames(dat_da_final)[-c(1, 6, 12:14)] )

dat_desc_stats <- dat_da_final

source("create_summary_table_generic.R")
# Create the summary statistics table
summary_stats_table <- create_summary_table(dat_da_final, "Chronic_opiate_use", vars)

# Format the table using flextable
ft <- flextable(summary_stats_table)

# Customize the header
ft <- set_header_labels(
  ft, 
  Variable = "Variable",
  `Total` = "Total",
  `No` = "No", 
  `Yes` = "Yes", 
  `P-value` = "P-value"
)

# Apply styles
ft <- bold(ft, i = ~ Variable != "" & !grepl("^  ", Variable), part = "body")  # Bold variable names
ft <- align(ft, align = "center", part = "header")
ft <- align(ft, j = 2:5, align = "center", part = "body")
ft <- bg(ft, i = ~ grepl("^  ", Variable), bg = "#F5F5F5", part = "body")  # Alternate row colors for levels
ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "gray", width = 0.8))
ft <- autofit(ft)

# Export to Word document
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "Table-1-Chronic-Opiate-Use.docx")