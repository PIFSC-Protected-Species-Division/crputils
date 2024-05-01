.onAttach <- function(library, pkgname)
{
  info <-utils::packageDescription(pkgname)
  package <- info$Package
  version <- info$Version
  date <- info$Date
  packageStartupMessage(
    paste(paste(package, version, paste("(",date, ")", sep=""), "\n"),
          "Demos and documentation can be found in the GitHub repository:\n",
          "https://github.com/PIFSC-Protected-Species-Division/crputils/ in the\n",
          "'example_workflows' folder and in the README.\n",
          "\n"
    )
  )
}
