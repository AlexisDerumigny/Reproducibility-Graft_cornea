logMARtoDecimal = approxfun(
  x = c(-0.1, 0, 0.05, 0.1, 0.15,
        0.2, 0.3, 0.4, 0.5, 0.7, 1, 1.3, 1.7),
  y = c(12, 10:1, 0.5, 0)
)

# Creates the visual acuities on the decimal scale
updateColnamesVA <- function (listeTableaux,
                              newname, oldnames){
  listeTab = listeTableaux # need to copy it to avoid bug of modification of an object within the pipe
  cat("  + *"); cat(newname); cat("* ")
  
  for (iTableau in 1:length(listeTab)) {
    whichVarVAlogMAR = which(colnames(listeTab[[iTableau]]) == oldnames)
    if (length(whichVarVAlogMAR) == 0){
      stop(paste("1-Missing VAlogMAR:", iTableau, newname, oldnames, collapse = " "))}
    datalogMAR  = as.numeric( listeTab[[iTableau]][ oldnames ][[1]] )
    if (length(datalogMAR) == 0){
      stop(paste("2-Missing VAlogMAR:", iTableau, newname, oldnames, collapse = " "))}
    datalogVA  = logMARtoDecimal( datalogMAR )
    if (length(datalogVA) == 0){
      stop(paste("3-Missing VAlogMAR:", iTableau, newname, oldnames, collapse = " "))}
    listeTab[[iTableau]][ newname ] <- datalogVA
  }
  
  cat("\n")
  return (listeTab)
}

# Put a new name for a lot of old names before
# and print some other text
updateColnames <- function (listeTableaux,
                            newname, oldnames = NULL, otherText = "")
{
  listeTab = listeTableaux # need to copy it to avoid bug of modification of an object within the pipe
  
  areColnamesUsed = FALSE
  oldColnamesUsed = character()
  
  if (!is.null(oldnames)){
    # We go over all tables
    for (iTableau in 1:length(listeTab)) {
      whichColnames_toChange = which(colnames(listeTab[[iTableau]]) 
                                     %in% c(newname,oldnames))
      # Changing the names of the columns only if necessary
      if (length(whichColnames_toChange) == 1){
        oldColnamesUsed = c(oldColnamesUsed, 
                            oldnames[oldnames %in% colnames(listeTab[[iTableau]])])
        colnames( listeTab[[iTableau]] )[ whichColnames_toChange ] <- newname
        areColnamesUsed = TRUE
      } else if (length(whichColnames_toChange) > 1){
        stop(paste("In 'updateColnames', two (or more) columns have similar names: ", 
                   '"', paste0(colnames( listeTab[[iTableau]] )[ whichColnames_toChange ],
                               collapse = '", "'), '"' ,
                   "\n coming from Table ", iTableau,
                   "\n Their new name should be:", newname, ".\n", collapse = "", sep = ""))
      }
    }
  } else {
    for (iTableau in 1:length(listeTab)) {
      areColnamesUsed = areColnamesUsed || (newname %in% colnames(listeTab[[iTableau]]))
    }
  }
  
  if(areColnamesUsed){
    cat("  + *"); cat(newname); cat("* ")
    if (length(oldColnamesUsed))
    {
      cat("(former ")
      cat(paste(paste0("*", sort(unique(oldColnamesUsed)), "*"), collapse = ", "))
      cat(") ")
    }
    cat(otherText)
    cat("\n")
  }
  
  return (listeTab)
}

# Merge several binary columns in one
mergingColnames <- function (listeTableaux,
                             newname, oldnames = NULL, otherText = ""){
  
  listeTab = listeTableaux # need to copy it to avoid bug of modification of an object within the pipe
  cat("  + *"); cat(newname); cat("* ")
  if (!is.null(oldnames)){
    # We go over all tables
    for (iTableau in 1:length(listeTab))
    {
      # Adding the new column if necessary
      if (! (newname %in% colnames(listeTab[[iTableau]])) ){
        listeTab[[iTableau]] = listeTab[[iTableau]] %>% add_column(!!newname := 0)
      }
      # Merging in the other variables
      for (icolumn in which(colnames(listeTab[[iTableau]]) %in% c(newname) ) )
      {
        listeTab[[iTableau]][newname] = listeTab[[iTableau]][newname] & listeTab[[iTableau]][icolumn]
      }
    }
    cat("(merge of former ")
    cat(paste(paste0("*",c(newname,oldnames),"*"), collapse = ", "))
    cat(") ")
  }
  
  cat(otherText)
  cat("\n")
  return (listeTab)
}


# Delete several columns
deletingColnames <- function (listeTableaux, oldnames, otherText = ""){
  
  listeTab = listeTableaux # need to copy it to avoid bug of modification of an object within the pipe
  deletedColnames = character()
  
  # We go over all tables
  for (iTableau in 1:length(listeTab)) {
    # Removing each variable
    colnamesToDelete = oldnames[oldnames %in% colnames(listeTab[[iTableau]])]
    deletedColnames = c(deletedColnames, colnamesToDelete)
    for (my_oldname in colnamesToDelete) {
      listeTab[[iTableau]][[my_oldname]] <- NULL
    }
  }
  
  if (length(deletedColnames) > 0){
    cat("  + Deleting: ")
    cat(paste(paste0("*", sort(unique(deletedColnames)), "*"), collapse = ", "))
    cat(otherText)
    cat("\n")
  }
  
  return (listeTab)
}


catCategory <- function(...) {cat("\n* "); cat(...); cat("\n")}
# catVari <- function(...) { cat("  + "); cat(...); cat("\n")}
