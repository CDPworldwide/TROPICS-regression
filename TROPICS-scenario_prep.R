#!/usr/bin/Rscript
# Andres Chang

######################################################################################################################.

#                                            TROPICS-scenario_prep

######################################################################################################################.
# Description: This script reads, transforms, and generates metadata for SR15 scenario data.
# Section 1 defines path to SR15 data and ETP data, which is used for a couple intensity
# denominators (e.g., steel production)
# Section 2 loads all libraries
# Section 3 contains functions that load and interpolate data, as well as combining variables and generating
# meta-data
# Section 4.1-4.3 prepare variable lists to import from SR15
# Section 4.4 pulls data from the SR15 scenario database and ETP 2017
# Section 4.5 calculates the slopes for all chosen variables between 2020 and future years in 5-year increments
# Section 4.6 produces different subsets of all scenarios based on different combinations of filters, all exported
# as csv w/ a summary sheet to compare their characteristics. csv's can then be loaded into regression model script
######################################################################################################################.

#1. Preamble =================================================================================

# SR15 database fpaths
f_sr15 <- 'input/scenarios/sr15/sr15_r1_1/iamc15_scenario_data_world_r1.1.xlsx'
f_sr15_all_regions <- 'input/scenarios/sr15/sr15_r1_1/iamc15_scenario_data_all_regions_r1.1.xlsx'
f_sr15_meta <- 'input/scenarios/sr15/sr15_r1_1/sr15_metadata_indicators_r1.1.xlsx'
f_etp <- 'input/scenarios/etp/2017/excel_edited/ETP2017_industry_summary_2.xlsx'

# 2. Library =================================================================================
library(plyr)
library(dplyr)
library(readr)
library(zeallot)
library(futile.logger)
library(magrittr)
library(ggplot2)
library(openxlsx)
library(reshape2)
library(tidyr)

# 3. Functions ===============================================================================

# 3.1 Data loading functions =================================================================
get.SR15.meta <- function() {
  # Get SR15 metadata for merge with output df
  
  meta <- (read.xlsx(f_sr15_meta, sheet='meta') %>%
             mutate(`Model-Scenario` = paste(model, scenario, sep='-')) %>%
             select(-c(1:3))
  )
  return(meta)
}

get.all.data <- function(refresh, all_regions=FALSE) {
  # Returns a df of SR15 data
  # Args:
  # refresh (bool): if FALSE, get.all.data will use the existing sr15_all_data variable
  # in environment if available. Otherwise, pulls fresh from xlsx
  # all_regions (bool): if TRUE, pulls from regions xlsx. Otherwise, pulls world data
  
  if(!all_regions) {
    if(!exists("sr15_all_data") | refresh) {
      flog.debug('Pulling SR15 world data from Excel')
      
      sr15_all_data <- read.xlsx(f_sr15, 2)
      # Make model-scenario column to match with scenarios in SBTi scenario file
      sr15_ms <- paste(sr15_all_data$Model, sr15_all_data$Scenario, sep='-')
      sr15_all_data <- cbind(sr15_all_data, sr15_ms)
      colnames(sr15_all_data)[ncol(sr15_all_data)] <- "Model-Scenario"
    } else {flog.debug('Using existing sr_15_all_data var in environment')}
    return(sr15_all_data) 
  } else {
    if(!exists("sr15_all_regions_all_data") | refresh) {
      flog.debug('Pulling SR15 all regions data from Excel')
      
      sr15_all_regions_all_data <- read.xlsx(f_sr15_all_regions, 2)
      # Make model-scenario column to match with scenarios in SBTi scenario file
      sr15_ms <- paste(sr15_all_regions_all_data$Model, sr15_all_regions_all_data$Scenario, sep='-')
      sr15_all_regions_all_data <- cbind(sr15_all_regions_all_data, sr15_ms)
      colnames(sr15_all_regions_all_data)[ncol(sr15_all_regions_all_data)] <- "Model-Scenario"
    } else{flog.debug('Using existing sr_15_all_regions_all_data var in environment')}
    return(sr15_all_regions_all_data) 
  }
}

get.ETP.data <- function() {
  etp_data0 <- (read.xlsx(f_etp, sheet='WORLD', skipEmptyRows = F, skipEmptyCols = F)[103:109, 15:24])
  colnames(etp_data0) <- c('Variable', '2014', seq(2025, 2060, 5))
  return(etp_data0)
}

# 3.2 Gap-filler functions ===================================================================
interp.all <- function(df, id.cols=5, cdata_yrs_out=FALSE) {
  # Returns a dataframe with one column per year between 2000 and 2100
  # Where data is interpolated linearly based on spacing of available
  # SR15 data per model-scenario. (I.e., works with 5-year or 10-year
  # data or mixed.)
  # Args:
  # * df (dataframe): non-interpolated data -- generally from filter.IPCC
  # * id.cols (num vector): numb vector of leading columns to be kept as "ID" columns in
  # returned dataframe
  # cdata_yrs_out (bool): if TRUE, returns a list where the second item is
  # a dataframe of "keystone years," i.e., for each row, which years are
  # reported data and which are interpolated. If FALSE, function just returns
  # df of interpolated data
  
  
  int_df <- matrix(0, nrow(df), length(c(2000:2100)))
  cd_out <- matrix(0, nrow(df), length(c(2000:2100)))
  
  for(i in 1:nrow(df)) {
    # Row index to write to
    data_yrs <- colnames(df[i,])[!is.na(df[i,])]
    data_yrs <- unlist(lapply(data_yrs, function(x) {
      x0 <- type.convert(x)
      if(is.numeric(x0)) {return(x0)} else {return(NULL)}
    }))
    cdata_yrs <- as.character(data_yrs)
    
    for(k in 2000:2100) {
      yr_col <- as.character(k)
      if(yr_col %in% cdata_yrs) {
        int_df[i, k-1999] <- df[i, yr_col]
        cd_out[i, k-1999] <- 1
      } else {
        back_yr <- data_yrs[data_yrs < k][
          length(data_yrs[data_yrs < k])]
        forward_yr <- data_yrs[data_yrs > k][1]
        n_yrs <- forward_yr - back_yr
        int_yr <- k - back_yr
        int_data <- (
          df[i, as.character(back_yr)] +
            (int_yr/n_yrs) * (df[i, as.character(forward_yr)] - df[i, as.character(back_yr)]))
        
        if(length(int_data) != 0 & length(int_data) != 1) {
          print(df[i])
          print(int_data)
        }
        
        if(length(int_data) == 0) {
          int_df[i, k-1999] <- NA
        } else{
          int_df[i, k-1999] <- int_data}                                           
      }
      
    }
  }
  
  if(id.cols >1) {
    int_df <- bind_cols(df[,c(1:id.cols)], as.data.frame(int_df)) 
  } else {
    int_df <- cbind(df[,1], as.data.frame(int_df))
    colnames(int_df)[1] <- colnames(df)[1]
  }
  colnames(int_df)[c((id.cols+1):ncol(int_df))] <- sapply(c(2000:2100), as.character)
  if(cdata_yrs_out) {
    return(list(int_df, cd_out))
  } else {
    return(int_df) 
  }
}

calculate.AFOLU.cs <- function(df) {
  # Add a column estimating land use-related carbon sequestration due to
  # poor reporting of 'Carbon Sequestration|Land Use' in SR15 database
  
  mutate(df,
         `Carbon Sequestration|Land Use2` = case_when(
           `Emissions|CO2|AFOLU` < 0 ~ -`Emissions|CO2|AFOLU`,
           TRUE ~ 0
         )
  )
}

calculate.CDR <- function(df) {
  # Return a df with a CDR variable, which is a sum of all CDR categories present in
  # the scenario. Note that this does not include CO2 captured at the point of emissions,
  # e.g., fossil CCS, it is strictly focused on net negative CO2 via the definition
  # in SR15 Figure 2.10
  
  CDR_subs <- c('CCS|Biomass', 'Land Use2', 'Feedstocks',
                'Direct Air Capture', 'Enhanced Weathering', 'Other')
  all_CDR <- generate.varnames('Carbon Sequestration', CDR_subs, FALSE)
  all_CDR <- all_CDR[all_CDR %in% colnames(df)]
  
  df[,all_CDR][is.na(df[,all_CDR])] <- 0
  
  df$cdr <- apply(df, 1, function(X) {
    sum(as.numeric(X[all_CDR]))
  })
  return(df)
}

calculate.intensity.vars <- function(df) {
  
  mutate(df,
         INT.emKyoto_gdp=`Emissions|Kyoto Gases`/`GDP|PPP`,
         INT.emCO2EI_PE=`Emissions|CO2|Energy and Industrial Processes`/`Primary Energy`,
         INT.emCO2EI_cement = `Emissions|CO2|Energy and Industrial Processes`/`Cement`,
         INT.emCO2IndDemand_cement = `Emissions|CO2|Energy|Demand|Industry`/`Cement`,
         INT.emCO2EI_steel = `Emissions|CO2|Energy and Industrial Processes`/`Crude steel`,
         INT.emCO2IndDemand_steel = `Emissions|CO2|Energy|Demand|Industry`/`Crude steel`,
         INT.emCO2EI_aluminum = `Emissions|CO2|Energy and Industrial Processes`/`Total aluminium (primary and secondary)`,
         INT.emCO2IndDemand_aluminum = `Emissions|CO2|Energy|Demand|Industry`/`Total aluminium (primary and secondary)`,
         INT.emCO2Elec_elecGen = `Emissions|CO2|Energy|Supply|Electricity`/`Secondary Energy|Electricity`,
         INT.emCO2EI_elecGen = `Emissions|CO2|Energy and Industrial Processes`/`Secondary Energy|Electricity`,
         INT.emCO2Transport_gdp = `Emissions|CO2|Energy|Demand|Transportation`/`GDP|PPP`
  )
  
}

# 3.3 New meta-data ==========================================================================
calculate.new.meta <- function(df, slope_vars, slope_year_pairs) {
  
  df[, c("cdr|cumulative")] <- NA
  
  for(si in unique(df$`Model-Scenario`)) {
    df[df$`Model-Scenario` == si, "cdr|cumulative"] <- (
      sum(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE))
    df[df$`Model-Scenario` == si, "cdr|max"] <- (
      max(df[df$`Model-Scenario` == si, "cdr"], na.rm = TRUE))
    
    if(is.na(df[df$`Model-Scenario` == si & df$Year == 2030, "Emissions|Kyoto Gases"])) {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- NA
    } else {
      df[df$`Model-Scenario` == si, "Year of max Kyoto emissions"] <- (
      df[df$`Model-Scenario` == si, "Year"][
      df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"] == max(
          df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"], na.rm=T
        ) & !is.na(df[df$`Model-Scenario` == si, "Emissions|Kyoto Gases"])])
      
    }
    
    if(is.na(df[df$`Model-Scenario` == si & df$Year == 2030, "Emissions|CO2|Energy and Industrial Processes"])) {
      df[df$`Model-Scenario` == si, "Year of max EI CO2 emissions"] <- NA
    } else {
      df[df$`Model-Scenario` == si, "Year of max EI CO2 emissions"] <- (
        df[df$`Model-Scenario` == si, "Year"][
          df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"] == max(
            df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"], na.rm=T
          ) & !is.na(df[df$`Model-Scenario` == si, "Emissions|CO2|Energy and Industrial Processes"])])
    }
  }
  
  return(df)
}

# 3.4 Utility functions ======================================================================
generate.varnames <- function(var0, subvars, include.var0=TRUE) {
  # Returns a vector of IPCC SR15 variables from a nested category
  # Args
  # var0 (character): 'Parent' var, e.g. 'Emissions|CO2'
  # subvars (chr vector): 'Child' vars, e.g., c('Energy|Supply', 'Energy|Demand')
  # include.var0 (bool): whether or not to include var0 w/o any subvars in return
  
  subvars <- sapply(subvars, function(vi)paste(var0, '|', vi, sep=''),
                    USE.NAMES=FALSE)
  if(include.var0) {
    var_all <- c(var0, subvars)
  } else {
    var_all <- subvars
  }
  
  return(var_all)
}

# 4. Script ==================================================================================
#___4.1 Logging settings =====================================================================
null.result <- flog.threshold(DEBUG, name="ROOT")

#___4.2 Variable lists =======================================================================

#___4.2.1 Emissions variables to include in output dataframe =================================
em0 <- 'Emissions|CO2'
em_subs <- c('Energy and Industrial Processes', 'Energy', 'Industrial Processes',
             'Energy|Supply', 'Energy|Demand', 'Energy|Demand|Industry', 'Energy|Demand|Transportation',
             'Energy|Supply|Electricity', 'AFOLU')

#___4.2.2 Carbon seq variables to include in output dataframe ================================
cs0 <- 'Carbon Sequestration'
cs_subs <- c('CCS|Biomass', 'CCS|Biomass|Energy', 'CCS|Biomass|Energy|Supply',
             'CCS|Biomass|Energy|Supply|Electricity', 'CCS|Fossil', 'Land Use',
             'Feedstocks', 'Direct Air Capture', 'Enhanced Weathering', 'Other')

#___4.2.3 Any variables still missing ========================================================
other_vars <- c('Primary Energy', 'Secondary Energy|Electricity',
                'Emissions|Kyoto Gases', 'Emissions|CH4|AFOLU', 'Emissions|N2O|AFOLU', 'Price|Carbon',
                'Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity', 'GDP|PPP')


#___4.3 Prepare arguments for get.scenario.data ==============================================

all0 <- list(
  em0, cs0
)

all_subs <- list(
  em_subs, cs_subs
)

all_varnames <- c((lapply(c(1:length(all0)),
                           function(X) generate.varnames(all0[[X]], all_subs[[X]]))
                    %>% unlist()),
                  other_vars)

flog.debug('Looking for %s variables', length(all_varnames))

#____4.4 Pull and transform data according to Andres specs ===================================
# Get all SR15 data and filter to only the variables selected
ss0 <- get.all.data(TRUE) %>% filter(Variable %in% all_varnames)

# Interpolate and transform SR15 data
flog.debug('Interpolating SR15 data')
interp_sr15_data <- (ss0 %>% interp.all(id.cols=5) %>%
    as.data.frame() %>%
    melt(id = c(1:5), variable.name='Year') %>%
    dcast(`Model` + `Scenario` + `Year` ~ `Variable`, value.var='value') %>%
    mutate(`Model-Scenario`=paste(`Model`, `Scenario`, sep='-'),
           Year = as.numeric(as.character(Year))
    ) %>%
    select(c(ncol(.), 1:(ncol(.)-1)))
    )

# Get ETP data, interpolate, and transform to same structure as SR15
flog.debug('Pulling ETP data')
interp_etp_data <- (get.ETP.data()
               %>% interp.all(id.cols=1)
               %>% melt(id.vars='Variable', variable.name = 'Year')
               %>% dcast(Year ~ Variable)
               %>% mutate(
                 `Model-Scenario`='ETP-2DS',
                 `Model`='ETP',
                 `Scenario`='2DS',
                 Year = as.numeric(as.character(Year))
               )
               %>% select(c((ncol(.)-2):ncol(.), 1:(ncol(.)-2)))
               %>% rename(Cement = 'Cement ')
               %>% arrange(Year))

# Now merge them. We are dropping "Model-Scenario" from the ETP dataframe because
# it is being combined with SR15 scenario data to estimate intensity of certain
# industrial sectors

interp_data <- (interp_sr15_data
                %>% merge(interp_etp_data[, -c(1:3)], by='Year'))

flog.debug('Calculating new vars')
sr15_out <- (interp_data
                %>% calculate.AFOLU.cs()
                %>% calculate.CDR()
                %>% calculate.intensity.vars())

sr15_meta0 <- get.SR15.meta()
sr15_new_meta <- (sr15_out
              %>% calculate.new.meta())
meta_cols_new <- colnames(sr15_new_meta)[!colnames(sr15_new_meta) %in% colnames(sr15_out)]
sr15_meta <- (sr15_new_meta
              %>% select(c('Model-Scenario', meta_cols_new))
              %>% unique()
              %>% merge(sr15_meta0, by='Model-Scenario'))

#____4.5 Calculate slopes of each variable and transform to Chris specs ======================
keep_years <- seq(2020, 2050, 5)
sr15_var_long <- (sr15_out %>% filter(Year %in% keep_years)
              %>% melt(id.vars=c(1:4))
              %>% mutate(value=as.numeric(value))
              %>% dcast(Model + Scenario + `Model-Scenario` + variable ~ Year)
)

# Indicate a base year and final year to calculate all slopes
slope_byr <- 2020
slope_yrf_all <-seq(2025, 2050, 5)

# Slopes calculated linearly and compound, generate column names
slope_colsLinear <- paste0('slope', sapply(slope_yrf_all, function(X) X-slope_byr))
slope_colsCA <- paste0('slopeCA', sapply(slope_yrf_all, function(X) X-slope_byr))
all_slope_cols <- list(slope_colsLinear, slope_colsCA)

sr15_var_long[, unlist(all_slope_cols)] <- NA

# Pull all data for base year of slope calculation
byr_data <- sr15_var_long[, as.character(slope_byr)]

# Loop through each final year of slope calculation
for(i in c(1:length(slope_colsLinear))) {
  
  # Get column names to be filled
  c(colL, colCA) %<-% c(all_slope_cols[[1]][i], all_slope_cols[[2]][i])
  # Final year of slope calculation in loop
  slope_yrf <- slope_yrf_all[i]
  # Pull data for final year of slope calculation in loop
  yrf_data <- sr15_var_long[, as.character(slope_yrf)]
  
  # Calculate linear reduction and comoound reduction
  sr15_var_long[, colL] <- 100 * (yrf_data - byr_data)/byr_data/(slope_yrf-slope_byr)
  sr15_var_long[, colCA] <- 100 * ((yrf_data/byr_data)^(1/(slope_yrf-slope_byr))-1)
  
  # Replace with NA if infinite or over 1000% growth
  sr15_var_long[, colL][is.infinite(sr15_var_long[, colL]) | sr15_var_long[, colL] > 1000] <- NA
  sr15_var_long[, colCA][is.infinite(sr15_var_long[, colL]) | sr15_var_long[, colL] > 1000] <- NA
}

#____4.6 Filter based on different combinations of parameters ================================

sr15_prefilter <- sr15_var_long %>% merge(sr15_meta, by='Model-Scenario')

# Latest allowed peak year of emissions
latest_peak <- c(2100, 2020, 2025, 2030)
# Which mitigation scenarios are included by latest peak year filter?
applied_to <- list(c('Below 1.5C' , '1.5C low overshoot', '1.5C high overshoot', 'Lower 2C', 'Higher 2C'),
                   c('Below 1.5C' , '1.5C low overshoot', '1.5C high overshoot', 'Lower 2C'),
                   c('Below 1.5C', '1.5C low overshoot', '1.5C high overshoot')
                   )

# Which emissions variable? All Kyoto GHGs or just Energy & Industrial CO2?
which_peak_var <- c("Year of max Kyoto emissions", "Year of max EI CO2 emissions")

# Min annual CO2 (or max annual CDR, negative) (GT CO2/yr)
min_co2 <- c(-1000, -20, -15, -10)
which_cdr_var <- c("cdr|max", "minimum.net.CO2.emissions.(Gt.CO2/yr)")

# Data collected for output df
nvariations <- (length(latest_peak)*length(applied_to)*length(which_peak_var)*
                  length(min_co2)*length(which_cdr_var))
filtered_dfs <- vector(mode='list', length = nvariations)
c(peak_yrv, peak_varv, appliedv, cdr_limv, cdr_varv, nscenariosv) %<-% (
  lapply(c(1:6), function(X) vector(mode='character', length=nvariations)))

counter <- 0

for(pi in which_peak_var){
  for(ci in which_cdr_var) {
    for(ai in applied_to) {
      for(peak_yr in latest_peak) {
        for(cdr_val in min_co2) {
          
          counter <- counter + 1
          if(length(ai) > 4) {
            pscenarios <- '1.5C and 2C'
          } else if(length(ai) > 3){
            
            pscenarios <- '1.5C and lower 2C'} else {
              pscenarios <- '1.5C'
          }
          
          set_name <- paste0(pi,' for ', pscenarios, ' scenarios: ', peak_yr,
                            '. CDR less than ', -cdr_val, ' GT CO2/yr based on ', ci)
          
          peak_yrv[counter] <- peak_yr
          peak_varv[counter] <- pi
          appliedv[counter] <- pscenarios
          cdr_limv[counter] <- ci
          cdr_varv[counter] <- cdr_val
          
          
          if(ci == 'cdr|max') {
            filtered_dfi <- sr15_prefilter %>% filter(
              ((!! rlang::sym(pi) <= peak_yr) & (`cdr|max` <= -cdr_val*1000) & (category %in% ai)) |
                ((`cdr|max` <= -cdr_val*1000) & !category %in% ai))
          } else {
            filtered_dfi <- sr15_prefilter %>% filter(
              ((!! rlang::sym(pi) <= peak_yr) & (`minimum.net.CO2.emissions.(Gt.CO2/yr)` >= cdr_val) & (category %in% ai)) |
                 ((`minimum.net.CO2.emissions.(Gt.CO2/yr)` >= cdr_val) & (!category %in% ai)))
          }
          
          filtered_dfs[[counter]] <- filtered_dfi
          names(filtered_dfs)[counter] <- set_name
          # print(length(unique(filtered_dfi$`Model-Scenario`)))
          nscenariosv[counter] <- length(unique(filtered_dfi$`Model-Scenario`))
          write_excel_csv(filtered_dfi, path=paste0('TROPICS-scenario_data_csv/TROPICS_dataset-', counter, '.csv'))
        }
      }   
    }
  }
}

mapping <- cbind(names(filtered_dfs), peak_yrv, peak_varv, appliedv, cdr_limv,
                 cdr_varv, nscenariosv, seq(1, length(filtered_dfs)))

write.xlsx(mapping, 'TROPICS_dataset_mapping.xlsx')





