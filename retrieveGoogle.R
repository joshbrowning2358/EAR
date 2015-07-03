#Retrieve data from googlespreadsheet
library("googlesheets")


retrieveGoogle <- function(wbtitle = c("Capacity Building database - Team X",
                                       "Capacity Building database - Team Y"),
                           reportype = "Capacity Building") {

  #Get capacity building worksheets                        
  if (reportype == "Capacity Building"){
      wbIndex = 1
      #clean-up column names
      cnames <- c("Project", "Country", "Partner_institution", "Status",
                  "Start", "Planned_finish", "FAO_contact", "Country_contact",
                  "Source_of_funds", "Budget", "Team")
  } else if(reportype == "Code Repo"){
      wbIndex = 2    
      #clean-up column names
      cnames <- c("Algorithm_Title", "Short_Description", "Language",
                  "Version_Controlled", "URL", "Access_Contact",
                  "Maintainer_Name", "Maintainer_Email", "Department","Team")
  } else {
    stop("Invalid reportype!")
  }
  
  #loop through workbook titles and merge into one df called master
  master = lapply(wbtitle, function(name){
    ss <- gs_title(name)
    cb.df <- as.data.frame(gs_read(ss, ws = wbIndex))
    cb.df$team <- rep(name, length(cb.df[,1]))
    cb.df
  })
  master <- do.call("rbind", master)
  colnames(master) <- cnames
    
  #remove the \n marks from columns, clean up team column
  master <- apply(master, 2, function(y) as.character(gsub("\n", "", y)))
  master <- as.data.frame(master)
  master[,length(master)] <- gsub("\\Capacity Building database - ", "",as.character(master[,length(master)]))

  master      

}  



##All of the function to access google sheets start w/ "gs_.."
##Skim https://github.com/jennybc/googlesheets
##and https://speakerdeck.com/jennybc/googlesheets-draft