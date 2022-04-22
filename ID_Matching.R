## improrting data sets. 
library(foreign)
library(dplyr)
library(ggplot2)
library(fastLink)
library(devtools)
library(data.table)
library(tidyverse)
library(stringdist)

## importing data for 1921 economic elites. 
econ_elites_1921_path<-"C:/Users/buzga/Desktop/School/Reaserch/Vicky/Data/Orignal/1921_Economic.csv"
econ_elites_1921 <- read.csv(econ_elites_1921_path)
## creating first name and last name coloumn
econ_elites_1921$nombreapellido <- paste(econ_elites_1921$nombre,econ_elites_1921$apellido,sep=" ") 
## explore the above line a bit, this is workable but does not take care of things like "white space" at the end of merges which may cuase provlems 

## keeping only desired colommsn 
econ_elites_1921 <- econ_elites_1921 %>% select(c(nombreapellido, nombre, apellido, puesto, empresa, capitalnominal, sector, gics_code, gics_name, titulo1, titulo2, noble, nobles_firm, old_noble, old_nobles_firm, notes))

## creating unique id per data set 
econ_elites_1921 <- econ_elites_1921 %>% group_by(nombreapellido) %>% dplyr::mutate(id_eco = cur_group_id()) 
g1 <- gammaCKpar(econ_elites_1921$apellido, econ_elites_1921$apellido, cut.a = 0.92, cut.p = 0.86) 
g2 <- gammaCKpar(econ_elites_1921$nombre, econ_elites_1921$nombre, cut.a = 0.92, cut.p = 0.86) 
tc <- tableCounts(list(g1, g2), nobs.a = nrow(econ_elites_1921), nobs.b = nrow(econ_elites_1921))
tc2 <- tc
class(tc2) <- "matrix"
tc2 <- data.table(tc2)
pattern <- paste(tc2$gamma.1, tc2$gamma.2, sep = "-")
tc2$weights <- 0
tc2$weights[pattern %in% c("2-2", "2-1", "1-2", "1-1")] <- 1

em <- emlinkMARmov(tc, nobs.a = nrow(econ_elites_1921), nobs.b = nrow(econ_elites_1921))
em$zeta.j <- as.matrix(tc2$weights)
em.obj <- data.frame(em$patterns.w)
em.obj$weights <- em$zeta.j 
em$patterns.w <- em.obj  

m1 <- matchesLink(list(g1, g2)
                  , nobs.a = nrow(econ_elites_1921)
                  , nobs.b = nrow(econ_elites_1921)
                  , em = em
                  , thresh = 0.95) # TE recommends 0.95

possible.matches <- data.table(cbind(m1$inds.a, m1$inds.b))

# Finally, i saved only a third variable with the names and lastnames from data.a and data.b, so that that comparison is easier

# The last step was to manually compare the possible matches to see which ones were true


## this is where i can do some reaserch on how the naming conventiosn work. 
Matches2Inspect <- cbind(econ_elites_1921[possible.matches$V1, c("nombreapellido",'nombre', "id_eco", "apellido")],
                         econ_elites_1921[possible.matches$V2, ])

# colnames (Matches2Inspect) <- c("business","military")
Matches2Inspect <- Matches2Inspect[duplicated (Matches2Inspect)==FALSE,]



### re-naming colloumsm so that they are easier to work with. 
names(Matches2Inspect)[1] <- "nombreapellido_1"
names(Matches2Inspect)[2] <- "nombre_1"
names(Matches2Inspect)[3] <- "id_eco_1"
names(Matches2Inspect)[4] <- "apellido_1"
names(Matches2Inspect)[5] <- "nombreapellido"
names(Matches2Inspect)[6] <- "nombre"
names(Matches2Inspect)[7] <- "apellido"
names(Matches2Inspect)[length(names(Matches2Inspect))] <- "id_eco"


# create variable to arbitrate whether is a match or not


Matches2Inspect$empate <- 0

Matches2Inspect <- Matches2Inspect %>% select(empate, nombreapellido.1, nombreapellido.2, everything())

# Import dataset to arbitrate manually


## droping rows where the ids are straight matches. 
test<-Matches2Inspect[(Matches2Inspect$id_eco != Matches2Inspect$id_eco_1),]
check_last<-function(df,idx){
  ### finds potential matches based on "apredio"
  ### args: df- the data frame, idx- the index of currently checked row 
  
  ##takes the last name row
  str_1<-df$apellido_1[idx]
  str_2<-df$apellido[idx]
  
  ## ensures the string values are not null 
  if(typeof(str_1)=="character" & typeof(str_2)=="character"){
    ## takes off white space and puts them in lists
    str_1<-strsplit(str_squish(str_1), " ")[[1]]
    str_2<-strsplit(str_squish(str_2), " ")[[1]]
    ## iterates through and makes sure that the initials of the names 
    #that are preasent are the same in both 
    for( i in 1:min(length(str_1),length(str_2))){
      if(str_1[i][1]!=str_2[i][1]){
        return(FALSE)
      }
    }
    return(TRUE)
  }
  return(TRUE)
}


check_first<-function(df,idx){
  ### check_first- finds potential matches bassed on "nombre"  
  ### args: df- the data frame, idx- the index of currently checked row 
  
  ##takes the "nombre" row
  str_1<-df$nombre_1[idx]
  str_2<-df$nombre[idx]
  
  ## ensures the string values are not null 
  if(typeof(str_1)=="character" & typeof(str_2)=="character"){
    ## takes off white space and puts them in lists
    str_1<-strsplit(str_squish(str_1), " ")[[1]]
    str_2<-strsplit(str_squish(str_2), " ")[[1]]
    ## iterates through and makes sure that the names 
    #that are preasent
    for( i in 1:min(length(str_2),length(str_1))){
      if(str_1[i]!=str_2[i]){
        return(FALSE)
      }
    }
    return(TRUE)
  }
  return(TRUE)
}
hamming_check<-function(df,idx,strictness){
  ## checks that the hamming distance between two strings is less than our threshold 
  ## a and b are strings to be compared
  ## stricness is the ratio of diference we are ok with. 
  a<-df$nombreapellido_1
  a<-strsplit(str_squish(df$nombreapellido_1[idx]), " ")[[1]]
  b<-strsplit(str_squish(df$nombreapellido[idx]), " ")[[1]]
  for(i in 1:length(a)){
    edit_distance<-adist(a[i],b[i])[1]
    if(1-edit_distance/max(nchar(a),nchar(b))<strictness){
      return(FALSE)
    }
  }
  return(TRUE)
}

automate_matches<-function(df){
  ## runs all above functions to make a new coloumn R_Matches matches
  df <- df %>%
    add_column(R_Matches = 0)
  for(i in 1:length(df$nombreapellido)){
    if(check_first(df,i) & check_last(df,i)){
      df$R_Matches[i]=1
    }
    else{
      if(hamming_check(df,i,.8)){
        df$R_Matches[i]=1
      }
    }
    
  }
  return(df)
}

temp<-test
temp<-automate_matches(temp)
write.csv(temp,"C:/Users/buzga/Desktop/School/Reaserch/Vicky/Code/R/Buz Code/r_matches.csv")
