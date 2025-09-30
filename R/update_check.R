# R/update_check.R

.onAttach <- function(libname, pkgname) {
  # Warn if an update exists
  try({
    current <- utils::packageVersion(pkgname)
    
    desc_url <- "https://raw.githubusercontent.com/jmerkle1/MerkleLabGIS/main/DESCRIPTION"
    remote_desc <- read.dcf(url(desc_url))
    latest <- package_version(remote_desc[1, "Version"])
    
    if (latest > current) {
      packageStartupMessage(
        sprintf("A newer version of %s is available: %s (you have %s).",
                pkgname, latest, current)
      )
    }
  }, silent = TRUE)
}

