# HICEAS eDNA CONVERSION


# LOAD LIBRARIES ----------------------------------------------------------

install.packages('crputils',
                 repos = c('https://pifsc-protected-species-division.r-universe.dev',
                           'https://cloud.r-project.org'))
library(crputils)



# LOAD CSV ----------------------------------------------------------------

hiceas2023edna <- read.csv("C:/Users/Selene.Fregosi/Downloads/hiceas2023edna.csv",
                           header = FALSE)
colnames(hiceas2023edna) <- c('lat', 'lon')

# PREP DATA INPUTS --------------------------------------------------------

# this contains letters W and N for the direction so need to get rid of those
# W are already negative which is good!

# easiest to just pull out deg and min separately and put them into a matrix
latDeg <- as.numeric(sapply(strsplit(hiceas2023edna$lat, " "), "[[", 1))
latMin <- as.numeric(sapply(strsplit(hiceas2023edna$lat, " "), "[[", 2))
lat <- cbind(latDeg, latMin)

# easiest to just pull out deg and min separately and put them into a matrix
lonDeg <- as.numeric(sapply(strsplit(hiceas2023edna$lon, " "), "[[", 1))
lonMin <- as.numeric(sapply(strsplit(hiceas2023edna$lon, " "), "[[", 2))
lon <- cbind(lonDeg, lonMin)


# CONVERT -----------------------------------------------------------------

# degrees decimal minutes TO decimal degrees

lat_decdeg <- crputils::degmin2decdeg(lat)

lon_decdeg <- crputils::degmin2decdeg(lon)

edna_decdeg <- cbind(lat_decdeg, lon_decdeg)
colnames(edna_decdeg) <- c('lat', 'lon')

write.csv(edna_decdeg, 'C:/Users/Selene.Fregosi/Downloads/hiceas2023edna_asDecDeg.csv')
