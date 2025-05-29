library(tidyverse)
library(ncdf4)
library(animation)

source("/Users/rqthomas/Documents/research/global-centers/ELCOM_FCR/R/write_run_elcom.R")

dir <- "/Users/rqthomas/Documents/research/global-centers/elcom-dev/AED_Tools_Private/FCR_ELCOM/"

df_metfx <- ropenmeteo::get_ensemble_forecast(
  latitude = 37.30,
  longitude = -79.83,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = c("temperature_2m", "wind_speed_10m", "wind_direction_10m", "precipitation","shortwave_radiation","relative_humidity_2m", "cloud_cover")) |> 
  ropenmeteo::add_longwave()

df_metfx_clean <-  df_metfx |> 
  mutate(year = year(datetime),
         doy = str_pad(yday(datetime),3, "left", "0"),
         hour = hour(datetime),
         minute = minute(datetime),
         second = second(datetime),
         fract_day = (hour * 60 * 60 + minute * 60 + second) / (60 * 60 * 24),
         TIME = as.double(paste0(year, doy)) + fract_day) |> 
  select(datetime, TIME, ensemble, prediction, variable) |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  rename(AIR_TEMP = temperature_2m,
         WIND_SPEED = wind_speed_10m,
         WIND_DIR = wind_direction_10m,
         SOLAR_RAD = shortwave_radiation,
         LW_RAD_IN = longwave_radiation,
         REL_HUM = relative_humidity_2m,
         RAIN = precipitation) |> 
  mutate(REL_HUM = REL_HUM / 100,
         RAIN = (RAIN/1000) * 24) |> 
  select(datetime, ensemble, TIME, AIR_TEMP, WIND_SPEED, WIND_DIR, SOLAR_RAD, LW_RAD_IN, REL_HUM, RAIN)


ens_members <- unique(df_metfx_clean$ensemble)
nmembers <- length(ens_members)

url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
targets <- read_csv(url , show_col_types = FALSE)

start_time <- Sys.time()

#Make sure a docker container is not already running
writeLines(c("#!/bin/bash", "docker stop ELCOM", "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
system2(paste0(dir, '/run_elcom_docker.sh'))

#Loop through ensemble members
for(ens in 1:nmembers){  
  print(ens)
  
  met_filename <- paste0("infiles/FCR_Met_", ens_members[ens], ".dat")
  profile_filename1 <- paste0("infiles/initialTempProfile1_", ens_members[ens], ".dat")  
  profile_filename2 <- paste0("infiles/initialTempProfile2_", ens_members[ens], ".dat")  
  outfile_filename <- paste0("outfiles_", ens_members[ens])  
  
  dir.create(file.path(dir, outfile_filename),showWarnings = FALSE)
  
  # Process met file
  
  df_ens <- df_metfx_clean |> 
    filter(ensemble == ens_members[ens]) |> 
    select(-ensemble, -datetime)
  
  cmd <- c("!", 
           "!",
           "!", 
           "!", 
           "7 data sets",
           "0 seconds between data", 
           "0 0	0	0	0	0	0	0",
           "!", 
           "TIME	AIR_TEMP	WIND_SPEED	WIND_DIR	SOLAR_RAD	LW_RAD_IN	REL_HUM	RAIN")
  
  for(i in 1:nrow(df_ens)){
    cmd <- c(cmd, stringr::str_flatten(df_ens[i,]," "))
  }
  
  writeLines(cmd, paste0(dir, "/", met_filename))
  
  # Process profiles
  
  profile <- targets |> 
    filter(as_date(datetime) == as_date(min(df_metfx_clean$datetime) + days(0)),
           variable == "Temp_C_mean",
           site_id == "fcre") |> 
    arrange(depth_m) |> 
    select(depth_m, observation)
  
  #FIRST PROFILE
  
  cmd <- c("!", "!", "!", "!", "1 data sets", "10 number of depths", "69 17 i,j", "DEPTH WTR_TEMP")
  
  curr_profile <- profile
  
  q_v <- rep(NA, nrow(curr_profile))
  w <- rnorm(nrow(curr_profile), 0, 1)
  w_new <- w
  vert_decorr_length <- 2
  model_sd <- 0.8
  
  for(kk in 1:nrow(curr_profile)){
    if(kk == 1){
      w_new[kk] <- w[kk]
    }else{
      alpha <- exp(-vert_decorr_length / abs((curr_profile$depth_m[kk]-curr_profile$depth_m[kk-1])))
      w_new[kk] <- ((1 - alpha) * w_new[kk-1] +  alpha * w[kk])
    }
    q_v[kk] <- w_new[kk] * model_sd
    curr_profile$observation[kk] <- curr_profile$observation[kk] + q_v[kk]
  }
  
  for(i in 1:nrow(curr_profile)){
    cmd <- c(cmd, stringr::str_flatten(curr_profile[i,]," "))
  }
  
  writeLines(cmd, paste0(dir, "/", profile_filename1))
  
  # SECOND PROFILE
  
  cmd <- c("!", "!", "!", "!", "1 data sets", "10 number of depths", "20 13 i,j", "DEPTH WTR_TEMP")
  
  curr_profile <- profile
  
  q_v <- rep(NA, nrow(curr_profile))
  w <- rnorm(nrow(curr_profile), 0, 1)
  w_new <- w
  vert_decorr_length <- 2
  model_sd <- 0.8
  
  for(kk in 1:nrow(curr_profile)){
    if(kk == 1){
      w_new[kk] <- w[kk]
    }else{
      alpha <- exp(-vert_decorr_length / abs((curr_profile$depth_m[kk]-curr_profile$depth_m[kk-1])))
      w_new[kk] <- ((1 - alpha) * w_new[kk-1] +  alpha * w[kk])
    }
    q_v[kk] <- w_new[kk] * model_sd
    curr_profile$observation[kk] <- curr_profile$observation[kk] + q_v[kk]
  }
  
  for(i in 1:nrow(curr_profile)){
    cmd <- c(cmd, stringr::str_flatten(curr_profile[i,]," "))
  }
  writeLines(cmd, paste0(dir, "/", profile_filename2))
  
  # Process inflow
  
  cmd <- c("3 data sets", "0 seconds between data", "0 1 1 1", "TIME	INFLOW	WTR_TEMP	SALINITY")
  
  df <- tibble::tibble(TIME = c(2015182.2500, 2015182.2917),
                       INFLOW = c(0.0287, 0.0288),
                       WTR_TEMP = c(20.78,20.72) ,
                       SALINITY = c(0, 0))
  
  for(i in 1:nrow(df)){
    cmd <- c(cmd, stringr::str_flatten(df[i,]," "))
  }
  #writeLines(cmd, paste0(dir, "/test_inflow.dat"))
  
  # Process outflow
  
  cmd <- c("3 data sets", "0 seconds between data", "0 2 2 2", "TIME	OUTFLOW	WTR_TEMP	SALINITY")
  
  df <- tibble::tibble(TIME = c(2015182.2500, 2015182.2917),
                       OUTFLOW = c(0.0408, 0.0405),
                       WTR_TEMP = c(20,20) ,
                       SALINITY = c(0, 0))
  
  for(i in 1:nrow(df)){
    cmd <- c(cmd, stringr::str_flatten(df[i,]," "))
  }
  #writeLines(cmd, paste0(dir, "/test_outflow.dat"))
  
  # Process Run ELCOM script
  
  config_ens <- readr::read_csv(paste0(dir, "/run_elcom.csv"), show_col_types = FALSE)
  
  config_ens$value[config_ens$variable == "start_date_cwr"] <- min(df_ens$TIME) + 2
  config_ens$value[config_ens$variable == "iter_max"] <- (60 * 60 * 24 * 1) /  as.numeric(config_ens$value[config_ens$variable == "del_t"])
  config_ens$value[config_ens$variable == "iter_out_restart"] <- config_ens$value[config_ens$variable == "iter_max"]
  config_ens$value[config_ens$variable == "iflow"] <- 0 
  config_ens$value[config_ens$value == "'outfiles'"] <- paste0("'", outfile_filename, "'")
  config_ens$value[config_ens$value == "Met_FCR.dat"] <- basename(met_filename)
  config_ens$value[config_ens$value == "initialTempProfile.dat"] <- basename(profile_filename1)
  config_ens <- bind_rows(config_ens, tibble(value = basename(profile_filename2), variable = "initial_profile_file", group = "input"))
  config_ens$value[config_ens$variable == "WIND_SPEED_HEIGHT"] <- 10 
  config_ens$value[config_ens$variable == "iquiet"] <- 1
  config_ens$value[config_ens$variable == "restart_out_file"] <- paste0("restart_final_", ens_members[ens])
  #config_ens$value[config_ens$variable == "irain"] <- 0
  config_ens <- config_ens |> 
    filter(!(value %in% c("FCR_bc_drain1_tracer.dat", "FCR_bc_drain1.dat", "FCR_bc_outflow.dat")))
  
  write_run_elcom(config_ens, dir)
  # RUN ELCOM FROM R USING DOCKER CONTAINER
  
  writeLines(c("#!/bin/bash", "docker stop ELCOM", "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
  system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
  system2(paste0(dir, '/run_elcom_docker.sh'))
  
  cmd <- paste0("docker run --name ELCOM --platform linux/amd64 -v ", dir,
                ":/working rqthomas/elcom:1.0 /bin/bash -c 'cd working; /ELCOM_FCR/binary/elcd_20250529'")
  writeLines(c("#!/bin/bash", cmd, "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
  system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
  system2(paste0(dir, '/run_elcom_docker.sh'))
  
  file.copy(file.path(dir, "restart_da.nc"), file.path(dir, paste0("restart_da_",ens_members[ens],".nc")), overwrite = TRUE)
  file.copy(file.path(dir, "restart_da.nc"), file.path(dir, paste0("restart_da_fx_",ens_members[ens],".nc")), overwrite = TRUE)
  file.remove(file.path(dir, "restart_da.nc"))
  
  # RUN TIME STEP 2 RESTARTING WITH DA ANALYSIS
  file.copy(file.path(dir, paste0("outfiles_", ens_members[ens]) , paste0("restart_final_", ens_members[ens], ".unf")), 
            file.path(dir, paste0("outfiles_", ens_members[ens]) , paste0("restart_final_fx_", ens_members[ens], ".unf")), 
            overwrite = TRUE)
  
  file.copy(file.path(dir, outfile_filename, "curtain.nc"), file.path(dir, outfile_filename, "curtain1.nc"), overwrite = TRUE)
  file.copy(file.path(dir, outfile_filename, "profile.nc"), file.path(dir, outfile_filename, "profile1.nc"), overwrite = TRUE)
  file.copy(file.path(dir, outfile_filename, "sheet_surface.nc"), file.path(dir, outfile_filename, "sheet_surface1.nc"), overwrite = TRUE)
  file.copy(file.path(dir, outfile_filename, "sheet_average.nc"), file.path(dir, outfile_filename, "sheet_average1.nc"), overwrite = TRUE)
  file.copy(file.path(dir, outfile_filename, "sheet_bottom_10cm.nc"), file.path(dir, outfile_filename, "sheet_bottom_10cm1.nc"), overwrite = TRUE)
  
}

# DA STEP

# READ IN FORECAST
nc_file <- file.path(dir, paste0("restart_da_",ens_members[1],".nc"))
nc <- nc_open(nc_file)
temp <- ncvar_get(nc, "WTR_TEMP")
grid_i <- ncvar_get(nc, "i")
grid_j <- ncvar_get(nc, "j")
grid_k <- ncvar_get(nc, "k")
grid_z <- ncvar_get(nc, "Z")
grid_x <- ncvar_get(nc, "X")
n <- ncvar_get(nc, "N")
ens_out <- matrix(NA, dim(temp), length(unique(ens_members)))
ens_out[, 1] <- temp
nc_close(nc)

for(ens in 2:nmembers){ 
  nc_file <- file.path(dir, paste0("restart_da_",ens_members[ens],".nc"))
  nc <- nc_open(nc_file)
  temp <- ncvar_get(nc, "WTR_TEMP")
  ens_out[, ens] <- temp
  summary(temp)
  nc_close(nc)
}

#ANALYSIS STEP
modelled_cell_index <- which(ens_out[, 1] >= -999)


location <- tibble(i = c(grid_i),
                   j = c(grid_j),
                   z = c(grid_z),
                   n = 1:length(c(grid_z))) |> 
  slice(modelled_cell_index) |> 
  mutate(n_modeled = 1:n())

profile <- targets |> 
  filter(as_date(datetime) == as_date(min(df_metfx_clean$datetime) + days(-10)),
         variable == "Temp_C_mean",
         site_id == "fcre") |> 
  arrange(depth_m) |> 
  select(depth_m, observation)

zt_index <- NULL
zt <- NULL
for(ii in 1:nrow(profile)){
  focal_location <- location |> 
    filter(i == 69 & j == 17) |> 
    mutate(abs_z = abs(-z - profile$depth_m[ii])) |> 
    filter(abs_z == min(abs_z))
  zt <- c(zt, profile$observation[ii])
  zt_index <- c(zt_index, focal_location$n_modeled)
}

x_matrix <- ens_out[modelled_cell_index, ]

summary(x_matrix[,1])
summary(x_matrix[,3])

ens_mean <- apply(x_matrix, 1, mean)

dit <- matrix(NA, nrow = nmembers, ncol = dim(x_matrix)[1])
for(m in 1:nmembers){
  dit[m, ] <- x_matrix[, m] - ens_mean
  if(m == 1){
    p_it <- dit[m, ] %*% t(dit[m, ])
  }else{
    p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it
  }
}

p_t <- (p_it / (nmembers - 1))



obs <- zt
psi <- rep(0.1, length(zt),)
psi_t <- diag(psi)

d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = obs, sigma=as.matrix(psi_t)))

h <- matrix(0, nrow = length(obs), ncol = dim(p_t)[1])

for(ii in 1:length(zt)){
  h[ii, zt_index[ii]] <- 1  
}


k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)

update <-  x_matrix + k_t %*% (d_mat - h %*% x_matrix)

ens_out2 <- ens_out
for(ens in 1:nmembers){ 
  ens_out2[modelled_cell_index, ens] <- update[, ens]
}

ens_out[modelled_cell_index[zt_index[1]], 1]
ens_out2[modelled_cell_index[zt_index[1]], 1]
zt[1]




#WRITE OUT ANALYSIS
for(m in 1:nmembers){ 
  nc_file <- file.path(dir, paste0("restart_da_",ens_members[m],".nc"))
  output <- nc_open(nc_file,write = TRUE)
  ncvar_put(output, varid = "WTR_TEMP", vals = ens_out2[ , m])
  nc_close(output)
}

ens <- 1



print(ens)


met_filename <- paste0("infiles/FCR_Met_", ens_members[ens], ".dat")
profile_filename1 <- paste0("infiles/initialTempProfile1_", ens_members[ens], ".dat")  
profile_filename2 <- paste0("infiles/initialTempProfile2_", ens_members[ens], ".dat")  
outfile_filename <- paste0("outfiles_", ens_members[ens])  

config_ens <- readr::read_csv(paste0(dir, "/run_elcom.csv"), show_col_types = FALSE)

config_ens$value[config_ens$variable == "start_date_cwr"] <- min(df_ens$TIME) + 3
config_ens$value[config_ens$variable == "iter_max"] <- (60 * 60 * 24 * 1) /  as.numeric(config_ens$value[config_ens$variable == "del_t"])
config_ens$value[config_ens$variable == "iter_out_restart"] <- config_ens$value[config_ens$variable == "iter_max"]
config_ens$value[config_ens$variable == "iflow"] <- 0 
config_ens$value[config_ens$value == "'outfiles'"] <- paste0("'", outfile_filename, "'")
config_ens$value[config_ens$value == "Met_FCR.dat"] <- basename(met_filename)
config_ens$value[config_ens$value == "initialTempProfile.dat"] <- basename(profile_filename1)
config_ens <- bind_rows(config_ens, tibble(value = basename(profile_filename2), variable = "initial_profile_file", group = "input"))
config_ens$value[config_ens$variable == "WIND_SPEED_HEIGHT"] <- 10 
config_ens$value[config_ens$variable == "iquiet"] <- 0
config_ens$value[config_ens$variable == "restart_out_file"] <- paste0("restart_final_", ens_members[ens])
config_ens$value[config_ens$variable == "irestart"] <- 1
config_ens <- config_ens |> 
  filter(variable != "restart_in_file")
config_ens <- bind_rows(config_ens, tibble(value = paste0("restart_final_", ens_members[ens], ".unf"), variable = "restart_in_file", group = "input"))


config_ens <- config_ens |> 
  filter(!(value %in% c("FCR_bc_drain1_tracer.dat", "FCR_bc_drain1.dat", "FCR_bc_outflow.dat")))

write_run_elcom(config_ens, dir)


file.copy(file.path(dir, outfile_filename, paste0("restart_final_fx_", ens_members[ens], ".unf")), 
          file.path(dir, "infiles", paste0("restart_final_", ens_members[ens], ".unf")), 
          overwrite = TRUE)

file.copy(file.path(dir, paste0("restart_da_",ens_members[ens],".nc")), file.path(dir, "restart_da.nc"), overwrite = TRUE)

write_run_elcom(config_ens, dir)

cmd <- paste0("docker run --name ELCOM --platform linux/amd64 -v ", dir,
              ":/working rqthomas/elcom:1.0 /bin/bash -c 'cd working; /ELCOM_FCR/binary/elcd_20250529'")
writeLines(c("#!/bin/bash", cmd, "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
system2(paste0(dir, '/run_elcom_docker.sh'))

file.copy(file.path(dir, outfile_filename, "curtain.nc"), file.path(dir, outfile_filename, "curtain2_da.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "profile.nc"), file.path(dir, outfile_filename, "profile2_da.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_surface.nc"), file.path(dir, outfile_filename, "sheet_surface2_da.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_average.nc"), file.path(dir, outfile_filename, "sheet_average2_da.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_bottom_10cm.nc"), file.path(dir, outfile_filename, "sheet_bottom_10cm2_da.nc"), overwrite = TRUE)


# RUN TIME STEP 2 RESTARTING WITH DA ANALYSIS

print(ens)


met_filename <- paste0("infiles/FCR_Met_", ens_members[ens], ".dat")
profile_filename1 <- paste0("infiles/initialTempProfile1_", ens_members[ens], ".dat")  
profile_filename2 <- paste0("infiles/initialTempProfile2_", ens_members[ens], ".dat")  
outfile_filename <- paste0("outfiles_", ens_members[ens])  


config_ens <- readr::read_csv(paste0(dir, "/run_elcom.csv"), show_col_types = FALSE)

config_ens$value[config_ens$variable == "start_date_cwr"] <- min(df_ens$TIME) + 3
config_ens$value[config_ens$variable == "iter_max"] <- (60 * 60 * 24 * 1) /  as.numeric(config_ens$value[config_ens$variable == "del_t"])
config_ens$value[config_ens$variable == "iter_out_restart"] <- config_ens$value[config_ens$variable == "iter_max"]
config_ens$value[config_ens$variable == "iflow"] <- 0 
config_ens$value[config_ens$value == "'outfiles'"] <- paste0("'", outfile_filename, "'")
config_ens$value[config_ens$value == "Met_FCR.dat"] <- basename(met_filename)
config_ens$value[config_ens$value == "initialTempProfile.dat"] <- basename(profile_filename1)
config_ens <- bind_rows(config_ens, tibble(value = basename(profile_filename2), variable = "initial_profile_file", group = "input"))
config_ens$value[config_ens$variable == "WIND_SPEED_HEIGHT"] <- 10 
config_ens$value[config_ens$variable == "iquiet"] <- 0
config_ens$value[config_ens$variable == "restart_out_file"] <- paste0("restart_final_", ens_members[ens])
config_ens$value[config_ens$variable == "irestart"] <- 1
config_ens <- config_ens |> 
  filter(variable != "restart_in_file")
config_ens <- bind_rows(config_ens, tibble(value = paste0("restart_final_", ens_members[ens], ".unf"), variable = "restart_in_file", group = "input"))


config_ens <- config_ens |> 
  filter(!(value %in% c("FCR_bc_drain1_tracer.dat", "FCR_bc_drain1.dat", "FCR_bc_outflow.dat")))

write_run_elcom(config_ens, dir)


file.copy(file.path(dir, outfile_filename, paste0("restart_final_fx_", ens_members[ens], ".unf")), 
          file.path(dir, "infiles", paste0("restart_final_", ens_members[ens], ".unf")), 
          overwrite = TRUE)

file.copy(file.path(dir, paste0("restart_da_fx_",ens_members[ens],".nc")), file.path(dir, "restart_da.nc"), overwrite = TRUE)

write_run_elcom(config_ens, dir)

cmd <- paste0("docker run --name ELCOM --platform linux/amd64 -v ", dir,
              ":/working rqthomas/elcom:1.0 /bin/bash -c 'cd working; /ELCOM_FCR/binary/elcd_20250529'")
writeLines(c("#!/bin/bash", cmd, "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
system2(paste0(dir, '/run_elcom_docker.sh'))

file.copy(file.path(dir, outfile_filename, "curtain.nc"), file.path(dir, outfile_filename, "curtain2_noda.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "profile.nc"), file.path(dir, outfile_filename, "profile2_noda.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_surface.nc"), file.path(dir, outfile_filename, "sheet_surface2_noda.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_average.nc"), file.path(dir, outfile_filename, "sheet_average2_noda.nc"), overwrite = TRUE)
file.copy(file.path(dir, outfile_filename, "sheet_bottom_10cm.nc"), file.path(dir, outfile_filename, "sheet_bottom_10cm2_noda.nc"), overwrite = TRUE)


curtain1_nc <- paste0("outfiles_", ens_members[ens], "/curtain1.nc")
curtain2da_nc <- paste0("outfiles_", ens_members[ens], "/curtain2_da.nc")
curtain2noda_nc <- paste0("outfiles_", ens_members[ens], "/curtain2_noda.nc")

nc <- nc_open(file.path(dir, curtain1_nc))
temp1 <- ncvar_get(nc, "TEMPERATURE")
depth <- ncvar_get(nc, "Z")
s <- ncvar_get(nc, "S")

#Date format
dates<- ncvar_get(nc, "Ordinal_Dates")
hour <- ncvar_get(nc, "Hour")
minute <- ncvar_get(nc, "Minute")
second <- ncvar_get(nc, "Second")

nc_close(nc)

# Extract month from the date object
doy <- substr(dates, 5, 7)
year <- substr(dates, 1, 4)
num_hour <- round(as.numeric(hour), 0)
num_min <- round(as.numeric(minute), 0)
num_sec <- round(as.numeric(second), 0)
full_date<- paste(year, doy)
date <- as_date(paste(year, doy, sep = "-"), format = "%Y-%j")
datetime <- as_datetime(date) + hours(num_hour) + minutes(num_min) + seconds(num_sec)

par(oma = c(2, 2, 3, 2))   # set margin size (oma) so that the title is included
col <- topo.colors

temp_curr <- temp1[,]
filled.contour(x = s,
               y = depth,
               z = (as.matrix(temp_curr)),
               color = col,
               zlim = range(c(10,28), na.rm = TRUE),
               main = paste0("Day 1: Temperature, °C ", datetime))


nc <- nc_open(file.path(dir, curtain2noda_nc))


temp1 <- ncvar_get(nc, "TEMPERATURE")
depth <- ncvar_get(nc, "Z")
s <- ncvar_get(nc, "S")

#Date format
dates<- ncvar_get(nc, "Ordinal_Dates")
hour <- ncvar_get(nc, "Hour")
minute <- ncvar_get(nc, "Minute")
second <- ncvar_get(nc, "Second")

nc_close(nc)

# Extract month from the date object
doy <- substr(dates, 5, 7)
year <- substr(dates, 1, 4)
num_hour <- round(as.numeric(hour), 0)
num_min <- round(as.numeric(minute), 0)
num_sec <- round(as.numeric(second), 0)
full_date<- paste(year, doy)
date <- as_date(paste(year, doy, sep = "-"), format = "%Y-%j")
datetime <- as_datetime(date) + hours(num_hour) + minutes(num_min) + seconds(num_sec)

par(oma = c(2, 2, 3, 2))   # set margin size (oma) so that the title is included
col <- topo.colors

temp_curr <- temp1[,]
filled.contour(x = s,
               y = depth,
               z = (as.matrix(temp_curr)),
               color = col,
               zlim = range(c(10, 28), na.rm = TRUE),
               main = paste0("Day 2 NO DA: Temperature, °C ", datetime))


nc <- nc_open(file.path(dir, curtain2da_nc))


temp1 <- ncvar_get(nc, "TEMPERATURE")
depth <- ncvar_get(nc, "Z")
s <- ncvar_get(nc, "S")

#Date format
dates<- ncvar_get(nc, "Ordinal_Dates")
hour <- ncvar_get(nc, "Hour")
minute <- ncvar_get(nc, "Minute")
second <- ncvar_get(nc, "Second")

nc_close(nc)

# Extract month from the date object
doy <- substr(dates, 5, 7)
year <- substr(dates, 1, 4)
num_hour <- round(as.numeric(hour), 0)
num_min <- round(as.numeric(minute), 0)
num_sec <- round(as.numeric(second), 0)
full_date<- paste(year, doy)
date <- as_date(paste(year, doy, sep = "-"), format = "%Y-%j")
datetime <- as_datetime(date) + hours(num_hour) + minutes(num_min) + seconds(num_sec)

par(oma = c(2, 2, 3, 2))   # set margin size (oma) so that the title is included
col <- topo.colors

temp_curr <- temp1[,]
filled.contour(x = s,
               y = depth,
               z = (as.matrix(temp_curr)),
               color = col,
               zlim = range(c(10, 28), na.rm = TRUE),
               main = paste0("Day 2With DA: Temperature, °C ", datetime))







