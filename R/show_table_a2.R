#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# EUNIS habitat types ####
# Show table A2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-03-18



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(gt)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Load data ###
data <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?",
      treatment = col_factor(
        levels = c("control", "cut_summer", "cut_autumn", "grazing")
      )
    )
) %>%
  select(plot, treatment, esy) %>%
  group_by(treatment, esy) %>%
  count() %>%
  group_by(treatment) %>%
  mutate(
    total_n = sum(n),
    ratio = n / total_n,
    ratio = round(ratio, digits = 2)
  ) %>%
  mutate(
    treatment = fct_recode(
      treatment,
      "Control" = "control",
      "Summer cut" = "cut_summer",
      "Autumn cut" = "cut_autumn",
      "Grazing" = "grazing"
    )
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(table <- data %>%
   gt(
     groupname_col = "treatment"
   ) %>%
   opt_table_lines("none") %>% ### Set general options ###
   tab_options(
     table.font.style = "Arial",
     table.font.size = px(11),
     table.font.color = "black",
     column_labels.font.weight = "bold",
     row_group.font.weight = "bold",
     data_row.padding = px(4),
     table.align = "left",
     column_labels.border.top.style = "solid",
     table_body.border.top.style = "solid",
     table_body.border.bottom.style = "solid",
     column_labels.border.top.color = "black",
     table_body.border.top.color = "black",
     table_body.border.bottom.color = "black",
     column_labels.border.top.width = px(2),
     table_body.border.top.width = px(1),
     table_body.border.bottom.width = px(2)
   ) %>%
   tab_style(
     locations = cells_column_labels(),
     style = cell_borders(
       sides = "top", color = "black", style = "solid", weight = px(1)
     )
   ) %>%
   tab_style(
     locations = cells_column_labels(),
     style = cell_text(align = "center")
   ) %>%
   tab_style(
     locations = cells_body(),
     style = cell_text(align = "center")
   ) %>%
   tab_style(
     locations = cells_column_labels(columns = "esy"),
     style = cell_text(align = "left")
   ) %>%
   tab_style(
     locations = cells_body(columns = "esy"),
     style = cell_text(align = "left")
   ) %>%
   cols_label( ### Rename column names ###
     esy = md("EUNIS habitat type"),
     n = md("Plots [#]"),
     total_n = md("Plots per treatment [#]"),
     ratio = md("Ratio")
   )
 )



### Save ###
write_csv(data, here("outputs", "tables", "table_a2_habitat_types.csv"))
gtsave(table, here("outputs", "tables", "table_a2_habitat_types.html"))
