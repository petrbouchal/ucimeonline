t_reports <- list(
  tar_file(siteconf, "_site.yml"),
  tar_file(sitefiles, "sitefiles"),
  # https://github.com/jdblischak/workflowr/issues/238#issuecomment-782024069

  tar_file(rmd_index, "index.Rmd"),
  tar_file(html_index, command = {!! tar_knitr_deps_expr("index.Rmd")
    rmd_index
    siteconf
    sitefiles
    rmarkdown::render_site("index.Rmd")
    "docs/index.html"}),

  tar_file(rmd_doc, "README.Rmd"),
  tar_file(html_doc, command = {!! tar_knitr_deps_expr("README.Rmd")
    rmd_doc
    siteconf
    sitefiles
    rmarkdown::render_site("README.Rmd")
    "docs/README.html"}),

  tar_file(rmd_codebook, "codebook.Rmd"),
  tar_file(html_codebook, command = {!! tar_knitr_deps_expr("index.Rmd")
    rmd_index
    siteconf
    sitefiles
    rmarkdown::render_site("codebook.Rmd")
    "docs/codebook.html"}),

  tar_file(rmd_dev, "dev.Rmd"),
  tar_file(html_dev, command = {!! tar_knitr_deps_expr("dev.Rmd")
    rmd_index
    siteconf
    sitefiles
    rmarkdown::render_site("dev.Rmd")
    "docs/dev.html"})
)
