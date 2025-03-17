infcalc <- function(dat, ctrlval = NULL, list_of_viruses = NULL) {
  newdat <- dat %>% as.data.frame()
  
  name_infection = paste("infection",list_of_viruses[[x]][1],list_of_viruses[[x]][2],sep = "_")
  name_neutralisation = paste("neutralisation",list_of_viruses[[x]][1], list_of_viruses[[x]][2], sep = "_")
  
  
  if(!is.null(ctrlval)) {
    
    for(x in 1:length(list_of_viruses)){
      
      ctrl = ctrlval[x]
      
      
      newdat <- newdat %>% 
        mutate(!!rlang::sym(name_infection) := (!!rlang::sym(list_of_viruses[[x]][1])/ctrl)*100) %>%
        mutate(!!rlang::sym(name_neutralisation) := 100 - !!sym(name_infection))
    }
    
    return(newdat)
    
  } else {
    
    
    for(x in 1:length(list_of_viruses)){
      
      
      ctrl = dat %>% filter(is_control == 1) %>% 
        summarise(mean = mean(!!rlang::sym(list_of_viruses[[x]][1]))) %>% 
        as.numeric()
      
      print(paste("processing", list_of_viruses[[x]][2]))
      
      newdat <- newdat %>%
        mutate(!!rlang::sym(name_infection) := (!!rlang::sym(list_of_viruses[[x]][1])/ctrl)*100) %>%
        mutate(!!rlang::sym(name_neutralisation) := 100 - !!rlang::sym(name_infection))
    }
    
    return(newdat)
  }
}
