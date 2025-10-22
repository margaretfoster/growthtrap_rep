simBetas <- function(parameters, nsims){                                                                            
    simbetas <- list()                                                                                              
    for(i in 1:length(parameters)) {                                                                                
        simbetas[[i]] <- do.call(rbind,                                                                             
                                 lapply(parameters[[i]],                                                            
                                        function(x)                                                                 
                                        rmvnorm(n=nsims,                                                            
                                                mu=x$est,                                                           
                                                Sigma=x$vcov)))                                                     
    }                                                                                                               
    return(simbetas)                                                                                                
}

