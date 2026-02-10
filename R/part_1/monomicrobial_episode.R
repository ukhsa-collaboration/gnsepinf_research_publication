#' episode groupings
#'
#' assigns isolates of same species to an episode window
#' works on line listings only.
#' MONO EPISODES ARE PER ORGANISM PER PATIENT
#' 
#' @return Monomicrobial or Polymicrobial patient episodes
#'
#' @title patient episodes
#' 
#' @import data.frame
#'
#' @export
#' 

source("path/path.R")

monomicrobial_episode <- function(data, 
                                  patient_id, 
                                  spec_date, 
                                  species,
                                  spec_type,
                                  episode_window=c(1:365),
                                  episode_type=c("static","rolling")){
  
  require("tidyverse")
  require("lubridate")
  
  
  ## create an episode counter
  i <- 1 
  
  ## NOTE: in code, episode_window-1 becusae 0 days is counting wise, day 1
  episode_window <- episode_window-1
  
  ## clearly not the way to deal with NSE, but it works.
  data <- data %>% ungroup() %>% 
    rename(date=spec_date,
           id=patient_id,
           species=species,
           spec_type=spec_type) %>% 
    mutate(episode=0)
  
  
  ## LOOP ###############################################################################################
  ## this will loop until every isolate has been assigned an episode based on the episode_type
  while(with(data,min(episode,na.rm=T))==0) {
    
    ## day_s static episode window
    ## day_r rolling episode window
    ## day_l lag window for rolling episode
    
    data <- data %>% 
      ungroup() %>% 
      group_by(id,spec_type,species,episode) %>%
      arrange(id,spec_type,species,date) %>%
      mutate(ep_n=seq(1:n()),
             ep_n_max=max(ep_n,na.rm=T)) %>% 
      mutate(day_s=as.numeric(difftime(date,date[1],units="days"))) %>% 
      mutate(day_r=as.numeric(difftime(date,lag(date),units="days")),
             day_l=lag(day_r),
             day_r_max=(ep_n-1)*episode_window) %>%
      arrange(id,species,spec_type,date) %>% 
      ungroup() 
    
    ## group episodes based on specimen type, organism, and person
    ## tag ungrouped records which are X day_s apart
    
    ## ROLLING episodes ##################################################################################
    ##all isolates within X days of the last reported isolate per episode
    if(episode_type=="rolling"){
      
      data <- data %>% 
        group_by(id,spec_type,species,ep_n_max) %>% 
        arrange(id,spec_type,species,ep_n) %>% 
        mutate(r_=if_else(day_r %in% c(NA,0:episode_window),T,F),
               s_=if_else(day_s <= day_r_max,T,F),
               l_=if_else(day_l %in% c(NA,0:episode_window),T,F)) %>%
        ungroup(episode) %>% 
        mutate(episode=case_when(
          episode==0 & ep_n_max==1 & r_==T & l_==T ~ i,
          episode==0 & r_==T & s_==T & l_==T ~ i,
          TRUE ~ episode
        )) %>% 
        mutate(episode=if_else(
          episode==i & lag(episode)==0 & ep_n>1,
          0,episode
        ))
      
    }    
    
    ## STATIC episodes ###################################################################################
    ## all isolates within X days of the first reported isolate per episode
    if(episode_type=="static"){
      
      data <- data %>% 
        arrange(id,species,spec_type,date) %>% 
        mutate(episode=if_else(
          episode==0 & 
            day_s %in% c(0:episode_window),
          i,
          episode
        ))
    }
    
    ## EPISODE SUMMARY DATA ############################################################################## 
    ## some episode summary data post loop
    data <- data %>% ungroup() %>%
      group_by(id,species,episode) %>% 
      mutate(episode_n=seq(1:n()),
             episode_isolate_count=n(),
             episode_date_min=min(date,na.rm=T),
             episode_date_max=max(date,na.rm=T),
             episode_length=difftime(episode_date_max,episode_date_min,unit="days")) %>%
      ungroup() %>% 
      mutate(episode=ifelse(
        episode==0 & day_s==0 & ep_n==1,
        NA,episode)) %>% 
      arrange(id,species,episode,date)
    
    
    print(paste(episode_window+1,"day",episode_type,"episode round",i))
    
    ## add 1 to the episode counter
    i <- i+1 
    
  }
  
  
  data <- data %>% 
    ungroup() %>% 
    group_by(id,species) %>% 
    mutate(episode=if_else(is.na(episode),max(episode,na.rm=T)+1,episode)) %>% 
    ungroup() %>% 
    select(-day_r_max,-ep_n_max,-day_s,-day_r,-day_l) %>% 
    arrange(id,species,date)
  
  if(episode_type=="rolling"){
    data <- data %>% select(-r_,-s_,-l_)
  }
  
  ## because i cant figure out the NSE way to fix the date errors this seems to work.
  ## yes I know its bad practice, but unless someone can show me the correct way this is how its gonna be
  names(data)[names(data)=="date"] <- quo_name(spec_date)
  names(data)[names(data)=="species"] <- quo_name(species)
  names(data)[names(data)=="id"] <- quo_name(patient_id)
  names(data)[names(data)=="spec_type"] <- quo_name(spec_type)
  
  return(data)  
}

polymicrobial_episode <- function(data,
                                  patient_id,
                                  species,
                                  spec_date,
                                  spec_type){
  
  
  require("tidyverse")
  require("lubridate")
  
  ## NSE fix part 1
  data <- data %>% ungroup() %>% 
    rename(date=spec_date,
           id=patient_id,
           species=species,
           spec_type=spec_type) 
  
  ## Create some flags for the data; identify possible polymicrobial groupings
  data <- data %>% 
    group_by(id,spec_type,date) %>% 
    arrange(id,spec_type,date,species) %>% 
    mutate(poly_group=case_when(
      species!=lag(species) | 
        species!=lead(species) ~ T,
      TRUE ~ FALSE
    )) %>% 
    group_by(id,spec_type,poly_group) %>% 
    mutate(poly_n=seq(1:n())) %>% 
    arrange(date,species) %>% 
    mutate(poly_org_count=max(poly_n,na.rm=T),
           poly_episode=ifelse(poly_org_count!=1 & poly_group,0,NA),
           poly_days=NA) %>% 
    ungroup()
  
  ## loop up to group the records into sequential polymicrobial same day episodes
  i <- 1
  print(paste(sum(data$poly_episode==0,na.rm=T),"polymicrobial records"))
  
  while(min(data$poly_episode,na.rm=T)==0) {
    print(paste0("Episode ",i,": same-day polymicrobial"))
    data <- data  %>% 
      mutate(poly_sort=poly_episode) %>% 
      group_by(id,spec_type,poly_sort) %>% 
      arrange(date,species) %>% 
      mutate(poly_days=as.integer(difftime(date,date[1]))) %>% 
      mutate(poly_episode=if_else(poly_days==0 & poly_episode==0 & poly_group,
                                  i,poly_episode)) %>% 
      ungroup()
    
    print(paste(sum(data$poly_episode==0,na.rm=T),"records remaining"))
    
    i <- i+1
  }
  
  ## post loop summary data
  data <- data %>% select(-poly_sort,-poly_days) %>% 
    group_by(id,spec_type,poly_episode) %>% 
    mutate(poly_n=seq(1:n()),
           poly_org_count=max(poly_n,na.rm=T),
           poly_n=ifelse(is.na(poly_episode),NA,poly_n),
           episode_org_count=poly_org_count) %>% 
    ungroup()
  
  ## NSE fix part 2
  names(data)[names(data)=="date"] <- quo_name(spec_date)
  names(data)[names(data)=="species"] <- quo_name(species)
  names(data)[names(data)=="id"] <- quo_name(patient_id)
  names(data)[names(data)=="spec_type"] <- quo_name(spec_type)
  
  return(data)
}