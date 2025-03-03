# Script to backup Google Drive folders locally

library(googledrive)
# library(lubridate)
# library(cronR)

#authorize google drive
googledrive::drive_auth(email = 'selene.fregosi@noaa.gov')
#first time: Console window will prompt to opt in to caching/saving credentials
#then a browser window will popup and you login with email/password and give 
#tidyverse API access
#if cached/saved, subsequent times Console will prompt you to select existing 
#account or authorize a new one 
#alternatively can specify the email to skip that part 

#manually create a list of all the google drive folders you want to backup
'Acoustic Glider Spatial Modeling Project'
'agate_test_data'
'Bioacoustics_Seaglider_Group'
'CalCurCEAS Gliders 2024'
'Cross-platform BANTER Classification'
'CRP Acoustics'
'CRP_Purchases'
'CRP_Resources'
'CRP_Seagliders'
'FKW_annotations'
'For_Selene'
'GliderRodeo'
'GliderSummit'
'HI UxS Glider Project'
'HICEAS_BANTER'
'LLHARP'
'LLHARP MASTER'
'LLHARP Noise & Pc Manuscript'
'miscellaneous files'
'Non-longline haul sound'
'PAM-SI Projects'
'paperwork'
'personal'
'Software from JLKM'
'SPACIOUS postdoc'
'templates'
'xx archive'

#specify in and out paths
folder_backup <- 'Bioacoustics_Seaglider_Group'
# folder_backup <- 'SG639'
folder_local <- file.path('F:', 'google_drive_backup', folder_backup)

#get the google drive ID
folder_gd <- googledrive::drive_get(folder_backup)

#get all the files in a specified folder
# files_to_backup <- googledrive::drive_ls(path = 'Bioacoustics_Seaglider_Group')


downloadGoogleDriveFolder(folder_gd$id, folder_local)

# googledrive::local_drive_quiet()
# dasList_gd = googledrive::drive_ls(path = dir_gd$raw_das, pattern = 'DASALL')
# pamList = googledrive::drive_ls(path = dir_gd$raw_pam, pattern = pat)
# googledrive::drive_download(file = googledrive::as_id(pamList$id[1]),
#                             overwrite = TRUE, path = pamFile)