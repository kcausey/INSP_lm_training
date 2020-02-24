
#-------------------Header------------------------------------------------
# Author: Kate Causey
# Date: 2/24/2020
# Purpose: Pull covariates for LM training at INSP
#
# source("/home/j/temp/jledes2/insp/data/covariates/pull_covariates.R", echo=T)
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list=ls())

library(data.table)

central_lib <- "/ihme/cc_resources/libraries/current/r"

source(file.path(central_lib,"get_covariate_estimates.R"))
source(file.path(central_lib,"get_location_metadata.R"))

locs <- get_location_metadata(35, gbd_round_id = 6)
locs <- locs[grep("MEX", ihme_loc_id)]

covariates <- fread("/home/j/temp/jledes2/insp/covariates.csv")

for(cov_id in covariates$covariate_id){

  tryCatch({
    dt <- get_covariate_estimates(covariate_id = cov_id,
                                  gbd_round_id = 6,
                                  decomp_step = "step1",
                                  location_id  =locs$location_id,
                                  year_id = 1990:2019)

    dt <- dt[,.(covariate_id,
                covariate_name_short,
                location_id,
                location_name,
                year_id,
                age_group_id,
                age_group_name,
                sex_id,
                sex,
                mean_value)]

    write.csv(dt, paste0("/home/j/temp/jledes2/insp/data/covariates/",
                         covariates[covariate_id==cov_id,covariate_name],
                         ".csv"), row.names = F)

    print(paste("finished with covariate_id", cov_id))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}


