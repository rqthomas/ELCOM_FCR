write_run_elcom <- function(config_ens, dir){
  
  
  cmd <- c("! --------------------------------------------------------------- !",
           "!  ELCOM Configuration file                                       !",
           "! --------------------------------------------------------------- !",
           "! --------------------------------------------------------------- !")
  
  curr_block <- config_ens |> 
    filter(group == "header") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "! Time controls                                                   !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "time") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "! Simulation module controls                                      !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "module") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Model settings and controls                                    !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "settings") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Default (uniformly distributed) values                         !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "initial") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Scalar filtering controls                                      !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "scalar") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Initalization and update options                               !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "initalization") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Meterological sensor heights                                   !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "met") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Turbulence modelling controls                                  !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "turbulence") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Iterative (conjugate gradient method) solution controls        !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "solution") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!   Input file names                                              !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "input") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Output controls                                                !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "output") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  Debugging controls                                             !")
  
  cmd <- c(cmd, break_cmd)
  
  curr_block <- config_ens |> 
    filter(group == "debug") |> 
    select(-group)
  
  for(i in 1:nrow(curr_block)){
    cmd <- c(cmd, paste0(" ", stringr::str_flatten(curr_block[i, ]," ")))
  }
  
  break_cmd <- c("! --------------------------------------------------------------- !",
                 "!  End                                                            !")
  
  cmd <- c(cmd, break_cmd)
  
  writeLines(cmd, paste0(dir, "/run_elcom.dat"))
}