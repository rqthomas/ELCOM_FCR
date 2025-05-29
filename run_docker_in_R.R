library(tidyverse)
library(ncdf4)
library(animation)

dir <- "/Users/rqthomas/Documents/research/global-centers/ELCOM_FCR"
setwd(dir)

source("R/write_run_elcom.R")

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

url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
targets <- read_csv(url , show_col_types = FALSE)

start_time <- Sys.time()

#Make sure a docker container is not already running
writeLines(c("#!/bin/bash", "docker stop ELCOM", "docker rm ELCOM"), paste0(dir, "/run_elcom_docker.sh"))
system(paste0('chmod u+r+x ', dir, '/run_elcom_docker.sh'))
system2(paste0(dir, '/run_elcom_docker.sh'))

#Loop through ensemble members
for(ens in 1:length(ens_members)){
  
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
    filter(as_date(datetime) == as_date(min(df_metfx_clean$datetime) + days(1)),
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
  
  
  df <- tibble::tibble(TIME = c(2015182.2500, 2015182.2917),
                       OUTFLOW = c(0.0408, 0.0405),
                       WTR_TEMP = c(20,20) ,
                       SALINITY = c(0, 0))
  
  cmd <- c("3 data sets", "0 seconds between data", "0 2 2 2", "TIME	OUTFLOW	WTR_TEMP	SALINITY")
  

  for(i in 1:nrow(df)){
    cmd <- c(cmd, stringr::str_flatten(df[i,]," "))
  }
  #writeLines(cmd, paste0(dir, "/test_outflow.dat"))
  
  # Process Run ELCOM script
  
  config_ens <- readr::read_csv(paste0(dir, "/run_elcom.csv"), show_col_types = FALSE)
  
  config_ens$value[config_ens$variable == "start_date_cwr"] <- min(df_ens$TIME) + 2
  config_ens$value[config_ens$variable == "iter_max"] <- (60 * 60 * 24 * 6) /  as.numeric(config_ens$value[config_ens$variable == "del_t"])
  config_ens$value[config_ens$variable == "iflow"] <- 0 
  config_ens$value[config_ens$value == "'outfiles'"] <- paste0("'", outfile_filename, "'")
  config_ens$value[config_ens$value == "Met_FCR.dat"] <- basename(met_filename)
  config_ens$value[config_ens$value == "initialTempProfile.dat"] <- basename(profile_filename1)
  config_ens <- bind_rows(config_ens, tibble(value = basename(profile_filename2), variable = "initial_profile_file", group = "input"))
  config_ens$value[config_ens$variable == "WIND_SPEED_HEIGHT"] <- 10 
  config_ens$value[config_ens$variable == "iquiet"] <- 1
  #config_ens$value[config_ens$variable == "irain"] <- 0
  config_ens <- config_ens |> 
    filter(!(value %in% c("FCR_bc_drain1_tracer.dat", "FCR_bc_drain1.dat", "FCR_bc_outflow.dat")))
  
  write_run_elcom(config_ens, dir)
  
  # RUN ELCOM FROM R USING DOCKER CONTAINER
  
  cmd <- paste0("docker run --name ELCOM --platform linux/amd64 -v ", dir,
                ":/ELCOM_FCR rqthomas/elcom:1.0 /bin/bash -c 'cd ELCOM_FCR;binary/elcd_20250526'")
  writeLines(c("#!/bin/bash", cmd, "docker rm ELCOM"), paste0(dir, "/run_elcom_docker2.sh"))
  system(paste0('chmod u+r+x ', dir, '/run_elcom_docker2.sh'))
  system2(paste0(dir, '/run_elcom_docker2.sh'))
  
}

end_time <- Sys.time()

end_time - start_time

# # MOVIE
# for(ens in 1:length(ens_members[1:5])){
#   
#   movie.name <- paste0(dir, "/outfiles_", ens_members[ens], "/curtain.gif")  
#   
#   nc_file <- paste0(dir, "/outfiles_", ens_members[ens], "/curtain.nc")  
#   
#   output <- nc_open(nc_file)
#   temp <- ncvar_get(output, "TEMPERATURE")
#   depth <- ncvar_get(output, "Z")
#   s <- ncvar_get(output, "S")
#   
#   #Date format
#   dates<- ncvar_get(output, "Ordinal_Dates")
#   hour <- ncvar_get(output, "Hour")
#   minute <- ncvar_get(output, "Minute")
#   second <- ncvar_get(output, "Second")
#   
#   # Extract month from the date object
#   doy <- substr(dates, 5, 7)
#   year <- substr(dates, 1, 4)
#   num_hour <- round(as.numeric(hour), 0)
#   num_min <- round(as.numeric(minute), 0)
#   num_sec <- round(as.numeric(second), 0)
#   full_date<- paste(year, doy)
#   date <- as_date(paste(year, doy, sep = "-"), format = "%Y-%j")
#   datetime <- as_datetime(date) + hours(num_hour) + minutes(num_min) + seconds(num_sec)
#   
#   par(oma = c(2, 2, 3, 2))   # set margin size (oma) so that the title is included
#   col <- topo.colors
#   
#   saveGIF({
#     for (i in 1:dim(temp)[3]) {
#       temp_tstep <- temp[,,i]
#       filled.contour(x = s,
#                      y = depth,
#                      z = (as.matrix(temp_tstep)),
#                      color = col,
#                      zlim = range(c(temp), na.rm = TRUE),
#                      main = paste0("Ensemble: ",ens_members[ens], " Temperature:  ", as_date(datetime[i])))
#     }
#   }, interval = 0.2, movie.name = movie.name, ani.width = 800, ani.height = 300)
#   
# }

big_df <- NULL

for(ens in 1:length(ens_members[1:5])){
  
  nc_file <- paste0(dir, "/outfiles_", ens_members[ens], "/sheet_surface.nc")  
  
  output <- nc_open(nc_file)
  temp <- ncvar_get(output, "TEMPERATURE")
  dates<- ncvar_get(output, "Ordinal_Dates")
  x <- ncvar_get(output, "X")
  y <- ncvar_get(output, "Y")
  #Date format
  hour <- ncvar_get(output, "Hour")
  minute <- ncvar_get(output, "Minute")
  second <- ncvar_get(output, "Second")
  
  # Extract month from the date object
  doy <- substr(dates, 5, 7)
  year <- substr(dates, 1, 4)
  num_hour <- round(as.numeric(hour), 0)
  num_min <- round(as.numeric(minute), 0)
  num_sec <- round(as.numeric(second), 0)
  full_date<- paste(year, doy)
  date <- as.Date(paste(year, doy, sep = "-"), format = "%Y-%j")
  datetime <- as_datetime(date) + hours(num_hour) + minutes(num_min) + seconds(num_sec)
  
  for (i in 1:dim(temp)[3]) {
    temp_tstep <- apply(as.matrix(temp[,,i]),2, rev)
    df_curr <- as.data.frame(temp_tstep) |> 
      mutate(y = 1:n()) |> 
      pivot_longer(-y, names_to = "x", values_to = "Temperature") |> 
      mutate(x = as.numeric(substring(x, first = 2)),
             ens = ens_members[ens],
             datetime = as_date(datetime[i]))
    
    big_df <- bind_rows(big_df, df_curr)
  }
  
}

p <- ggplot(big_df) +
  geom_raster(aes(x = x, y = y, fill = Temperature)) + 
  scale_fill_viridis_c(na.value="white") +
  theme_void() +
  facet_grid(ens~datetime) +
  theme(legend.position="bottom")

ggsave(paste0(dir, "/ggplot.pdf"), p, height = 10, width = 5)






