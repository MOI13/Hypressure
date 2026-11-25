# ==============================================================================
# TABLE STYLE
# three-line (booktabs) layout, no shading, Arial 10 pt, title above, notes below.
# ==============================================================================

set_flextable_defaults(
  font.family    = "Arial",
  font.size      = 10,
  theme_fun      = theme_booktabs,
  padding.top    = 2,
  padding.bottom = 2
)

## gtsummary tables: HR (95% CI) in one column, JAMA number formatting
theme_gtsummary_journal(journal = "jama")


## Finish a flextable: indent level rows, rule under spanning headers, notes below
journal_table <- function(ft, title = NULL, notes = NULL, indent_rows = NULL) {
  ft <- ft %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    align(j = 1, align = "left", part = "header") %>%
    align(j = 1, align = "left", part = "body") %>%
    align(j = 2:ncol_keys(ft), align = "center", part = "header") %>%
    align(j = 2:ncol_keys(ft), align = "center", part = "body")
  
  ## spanning header row gets its own thin rule
  if (nrow_part(ft, "header") > 1) {
    ft <- ft %>% 
      hline(i = 1, j = 2:ncol_keys(ft), part = "header", border = fp_border_default(width = 0.75))
  }
  
  if (!is.null(indent_rows)) {
    ft <- ft %>% padding(i = indent_rows, j = 1, padding.left = 15, part = "body")
  }
  
  if (!is.null(notes)) {
    ft <- ft %>%
      add_footer_lines(values = notes) %>%
      fontsize(size = 9, part = "footer") %>%
      align(align = "left", part = "footer")
  }
  
  ## footnotes never get lines, only the rule under the last body row
  if (nrow_part(ft, "footer") > 0) {
    ft <- ft %>%
      border(part = "footer", border = fp_border_default(width = 0)) %>%
      hline_bottom(part = "body", border = fp_border_default(width = 1))
  }
  
  if (!is.null(title)) {
    ft <- ft %>% set_caption(caption = title)
  }
  
  ft %>%
    fix_border_issues() %>%
    autofit()
}