#for each dataset, retain only columns with NOS, type, RT, Acc, WF, and list of analysed covariates
#then retain only items (rows) with complete data
#check all WF >= 1
#make sure type is only monosemes and polysemes and remove from columns
ifelse(!dir.exists(file.path(getwd(), outpath, 'Misc')), dir.create(file.path(getwd(), outpath, 'Misc')), FALSE)

clean_df <- function(DFname, covlist, datalist){
  df.list <- get(DFname)[datalist]

  sink(paste("./", outpath, "/Misc/", DFname, "_cleanup.txt", sep = ""))
  
  for (i in datalist){
    
    #get required columns from each dataset
     
    dfcleaned <- df.list[[i]][,c("Word", "NOS", "NOM", "type", "RT", "Acc", "WF", covlist)]
    print(i)    
    print("starting items:")    
    print(nrow(dfcleaned))
    
    
    #make sure type is only monosemes and polysemes (and NOM is not more than 1) and remove from columns
    dfcleaned <- dfcleaned[(dfcleaned$type == "monoseme" | dfcleaned$type == "polyseme"), c("Word", "NOS", "NOM", "WF","RT", "Acc", covlist)]
    dfcleaned <- dfcleaned[!(dfcleaned$NOM > 1), c("Word", "NOS", "WF", "RT", "Acc", covlist)]
    print("no-homonym items:")
    print(nrow(dfcleaned)) 

    #check all WF >= 1
    dfcleaned <- dfcleaned[dfcleaned$WF >= 1,]
    print("WF >=1 items:")
    print(nrow(dfcleaned)) 
    
    ##print number of na in each column
    print(colSums(is.na(dfcleaned)))
    
    #take out items missing one or more covariates from each dataset
    dfcleaned <- dfcleaned[complete.cases(dfcleaned), ]
    dfcleaned <- na.omit(dfcleaned) #double insurance in case complete.cases missed anything
    print("complete.cases items:")
    print(nrow(dfcleaned))  
    
    df.list[[i]] <- dfcleaned
  }
  sink(file = NULL)
  sink()
  return(df.list) 
}


