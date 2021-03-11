t_reports <- list(
  tar_file(siteconf, "_site.yml"),
  tar_file(siteoutput, "_embed.yml"),
  tar_file(sitefiles, "sitefiles"),
  # https://github.com/jdblischak/workflowr/issues/238#issuecomment-782024069

  tar_file(rmd_index, "index.Rmd"),
  tar_file(html_index, command = {!! tar_knitr_deps_expr("index.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(rmd_index)
    "docs/index.html"}),

  tar_file(rmd_doc, "README.Rmd"),
  tar_file(html_doc, command = {!! tar_knitr_deps_expr("README.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(rmd_doc)
    "docs/README.html"}),

  tar_file(rmd_codebook, "codebook.Rmd"),
  tar_file(html_codebook, command = {!! tar_knitr_deps_expr("codebook.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(rmd_codebook)
    "docs/codebook.html"}),

  tar_file(rmd_dev, "dev.Rmd"),
  tar_file(html_dev, command = {!! tar_knitr_deps_expr("dev.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(rmd_dev)
    "docs/dev.html"}),

  tar_file(html_embed, command = {!! tar_knitr_deps_expr("index.Rmd")
    sitefiles
    render_nosite(rmd_index, "docs/embed/index.html", "docs/embed", siteoutput)
    fs::dir_copy(sitefiles, "docs/embed/sitefiles", overwrite = T)
    "docs/embed/index.html"})
)
