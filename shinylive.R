library("remotes")
install_version("shinylive", "0.3.0", repos = 'https://cran.rstudio.com/')
install_version("httpuv", "1.6.15", repos = 'https://cran.rstudio.com/')

library(shinylive)
library(httpuv)


shinylive::export(appdir = ".", destdir = "docs")
httpuv::runStaticServer("docs/", port = 8008)

## it doesn't work due to "package version mismatch"...