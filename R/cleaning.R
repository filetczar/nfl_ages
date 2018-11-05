######################
# Phil Azar 
# Topic: Clean NFL data
######################

# libraries
library(tidyverse)
library(stringr)



# read in data 
data_files <- list.files('./data/')

for(i in seq_along(data_files)){
  assign(x = str_sub(data_files[i], 5, -5L), 
         value = read_csv(paste0('./data/',data_files[i])))
}

# Cleaning List 
# Clean name to take out \\
# create unique key (player, age, team)
# get distinct
# break draft into round only 
# create age groups: <24, 25-28,29-32, 32+ 
  
scrubber <- function(data) { 
  data$name <- str_sub(data$Player, 
                       start=1L, 
                       end = str_locate(data$Player, '\\\\')[,1]-1)
  data$key <- paste0(str_replace(data$name, "[:blank:]", ""), data$Age,data$Tm, data$Year)
  
  data <- distinct(data, key, .keep_all = TRUE)
  
  data$G <- ifelse(is.na(data$G),0, data$G)
  data$GS <- ifelse(is.na(data$GS),0, data$GS)
  
  draft <- str_split(data$Draft, "-") %>% 
            reduce(.,rbind) %>% 
            as.data.frame(.,row.names=FALSE) %>% 
            mutate('Round' = ifelse(is.na(.$'V1'), 'Undrafted', .$'V1'), 
                   'Pick' =ifelse(is.na(.$'V2'), 'Undrafted', .$'V2')) %>% 
            select(Round, Pick)
  data <- dplyr::bind_cols(data, draft)
  
  data$ageGroup <- ifelse(data$Age <= 24, '< 24', 
                          ifelse(data$Age <= 28, '25-28', 
                                 ifelse(data$Age <= 32, '29-32', '32+')))
  return(data)
            
}

data_list <- list(data_ol, 
                  data_qb,
                  data_rb, 
                  data_wr, 
                  data_sec, 
                  data_te,
                  data_dl_lb, 
                  data_kickers)

data_list_clean <- lapply(data_list, scrubber)

seqx <- c('ol', 'qb', 'rb', 'wr', 'sec', 'te', 'dl_lb', 'sp_teams')

for(i in seq_along(data_list_clean)) {
  X = paste0('data_clean_', seqx[i])
  assign(x = X , value = data_list_clean[[i]])
  write_csv(x =get(X), path = paste0('./data/', X, '.csv'))
}



