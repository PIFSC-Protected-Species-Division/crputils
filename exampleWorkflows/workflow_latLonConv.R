#' workflow for converting GPS coordinates to different formats
#'
#' initial motivation was for converting locations for .das files to the format
#' necessary for the cruise story map.
#' But, these could be used for any need of converting coordinates!
#'
#' Options are:
#'    - decimal degrees to degrees decimal minutes          ( dd to dmm)
#'    - decimal degrees to degrees minutes seconds          ( dd to dms)
#'    - degrees decimal minutes to decimal degrees          (dmm to dd )
#'    - degrees decimal minutes to degrees minutes seconds  (dmm to dms)
#'    - degrees minutes seconds to decimal degrees          (dms to dd )
#'    - degrees minutes seconds to degrees decimal minutes  (dms to dmm)
#'
#'
#' author: Selene Fregosi selene.fregosi [at] noaa.gov
#' date: 22 April 2024
#'

# install crputils package
install.packages('crputils',
                 repos=c('https://pifsc-protected-species-division.r-universe.dev',
                         'https://cloud.r-project.org'))


# ------ decimal degrees TO degrees decimal minutes -----------------------

# # single input coordinates
decdeg <- matrix(30.4875)
# # multiple input coordinates
decdeg <- matrix(c(30.4875, -155.6149, 9.25), # three input coordinates
                nrow = 3, ncol = 1, byrow = TRUE) # update nrow to num coords

# convert
degmin <- crputils::decdeg2degmin(decdeg)
degmin
# > degmin
#      [,1]   [,2]
# [1,]   30 29.250
# [2,] -155 36.894
# [3,]    9 15.000

# ------ decimal degrees TO degrees minutes seconds -----------------------

# # single input coordinates
decdeg <- matrix(30.4875)
# # multiple input coordinates
decdeg <- matrix(c(30.4875, -155.6149), # two input coordinates
                nrow = 2, ncol = 1, byrow = TRUE) # update nrow to num coords

# convert
degminsec <- crputils::decdeg2degminsec(decdeg)
degminsec
# > degminsec
#      [,1] [,2]  [,3]
# [1,]   30   29 15.00
# [2,] -155   36 53.64


# ------ degrees decimal minutes TO decimal degrees -----------------------

# single input coordinate
degmin <- matrix(c(30, 29.250), nrow = 1, ncol = 2)
# multiple input coordinates
degmin <- matrix(c(30, 29.250,            # first coordinate
                  -155, 36.894),         # second coordinate
                nrow = 2, ncol = 2, byrow = TRUE) # update nrow to num coords

# convert
decdeg <- crputils::degmin2decdeg(degmin)
decdeg
# > decdeg
#      decdeg
# 1   30.4875
# 2 -155.6149


# ------ degrees decimal minutes to degrees minutes seconds ---------------

# single input coordinate
degmin <- matrix(c(30, 29.250), nrow = 1, ncol = 2)
# multiple input coordinates
degmin <- matrix(c(30, 29.250,            # first coordinate
                  -155, 36.894,          # second coordinate
                  9, 15),                # third coordinate
                nrow = 3, ncol = 2, byrow = TRUE) # update nrow to num coords

# convert
degminsec <- crputils::degmin2degminsec(degmin)
degminsec
# > degminsec
#      [,1] [,2]  [,3]
# [1,]   30   29 15.00
# [2,] -155   36 53.64
# [3,]    9   15  0.00


# ------ degrees minutes seconds TO decimal degrees -----------------------

# single input coordinate
degminsec <- matrix(c(30, 29, 15), nrow = 1, ncol = 3)
# multiple input coordinates
degminsec <- matrix(c(30, 29, 15,          # first coordinate
                     -155, 36, 53.64,     # second coordinate
                     9, 15, 0),           # third coordinate
                   nrow = 3, ncol = 3, byrow = TRUE) # update nrow to num coords

# convert
decdeg <- crputils::degminsec2decdeg(degminsec)
decdeg
# > decdeg
#           [,1]
# [1,]   30.4875
# [2,] -155.6149
# [3,]    9.2500


# ------ degrees minutes seconds TO degrees decimal minutes ---------------

# single input coordinate
degminsec <- matrix(c(30, 29, 15), nrow = 1, ncol = 3)
# multiple input coordinates
degminsec <- matrix(c(30, 29, 15,          # first coordinate
                     -155, 36, 53.64),    # second coordinate
                   nrow = 2, ncol = 3, byrow = TRUE) # update nrow to num coords

#convert
degmin <- crputils::degminsec2degmin(degminsec)
degmin
# > degmin
#      [,1]   [,2]
# [1,]   30 29.250
# [2,] -155 36.894

