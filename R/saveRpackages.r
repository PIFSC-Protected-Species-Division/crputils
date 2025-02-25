
# Yvonne Barkley

# Code to 


# First run this:
tmp <- installed.packages() #find all packages
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installedpkgs.rda") #will save R data file of all packages in your current working director, getwd()

# After updating R, run the following:
load(file = file.choose()) #select and load "installedpkgs.rda"
tmp <- installed.packages() # this should have fewer packages, just the basics
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()
