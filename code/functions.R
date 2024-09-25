## gt style file --------------------------------------- (2023-04-10 10:03) @----
gt_theme <- function(data) {
  data %>%
    opt_row_striping() |>
    opt_table_lines(extent = "none") |>
    # opt_table_font(
    #   font = list(
    #     google_font(name = "Source Sans Pro")
    #   )
    # ) |>
    # opt_table_font(stack = "Source Sans Pro") |>
    tab_options(
      table.font.color = "black",
      table.font.names = "Source Sans Pro",
      data_row.padding = px(2),
      table.font.size = px(14), # ?12
      column_labels.font.size = px(14),  # ?12
      table.align = "left",
      table_body.border.bottom.width = px(1.7),
      table.border.top.width = px(1.7),
      table_body.border.bottom.color = "#9A9EA1",
      table.border.top.color = "#9A9EA1",
      table_body.border.bottom.style = "solid",
      table.border.top.style = "solid",
      column_labels.border.bottom.color = "#9A9EA1",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1.3),
      column_labels.font.weight = "bold",
      column_labels.padding = px(3),
      heading.align = "left",
      footnotes.padding = px(0),
      footnotes.font.size = px(12),
      source_notes.padding = px(0),
    ) |>
    opt_horizontal_padding(scale = 2) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) |>
    opt_footnote_marks(marks = "letters") |>
    opt_footnote_spec(spec_ref = "^x", spec_ftr = "^x") |>
    opt_css(
      css = "
    #one .gt_footnote_marks {
    font-style: normal;
    font-weight: normal;
    font-size: 85%;
    vertical-align: 0px;
    }
  "
    )
}

vert_lab_fun <- function(label){
  paste0("<vertical-text>", label, "</vertical-text>")
}

# simple figure and table caption labels and numbering (flexibility for html)
table_n <- 0
figure_n <- 0

table_ref <- function() {
  table_n <<- table_n + 1
  paste0("Table ", table_n, ". ")
}

figure_ref <- function() {
  figure_n <<- figure_n + 1
  paste0("Figure ", figure_n, ". ")
}

trim_image <- function(file_png) {
  file_png <- paste0("assets/", file_png)
  image <- magick::image_read(file_png)
  image_trimmed <- magick::image_trim(image)  # Adjust fuzz value if needed
  magick::image_write(image_trimmed, path = file_png)
}



# anchor_table_ref <- function() {
#   table_n + 1
# }

# `r paste0("tab", knitr::current_input(), anchor_table_ref())`
