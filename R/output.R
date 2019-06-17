render_html_to_md_base64 <- function(rmd_file = "../data_table_tutorial/data_table.Rmd") {
  out_1 <- rmarkdown::render(rmd_file, output_format = "html_document")
  out_path <- gsub("\\.html", "\\.md", out_1)
  rmarkdown::pandoc_convert(out_1, to = "markdown_strict", output = out_path)
  st <- readr::read_file(out_path)
  img_cnt <- 0
  img_cnt <- length(stringr::str_match_all(st, "<img src=\"(.*?)\""))
  if (img_cnt > 0) {
    for (i in 1:img_cnt) {
      r <- paste0("![](", stringr::str_match(st, "<img src=\"(.*?)\"")[2], ")") 
      st_r <- paste(stringr::str_replace(st, "(<img src=\".*?\".*)\\n?", r), "\\n\\n")
      readr::write_file(st_r, out_path)
      st <- readr::read_file(out_path)
    }
  }
}

# not replacing more than one occurence of img tags at the moment