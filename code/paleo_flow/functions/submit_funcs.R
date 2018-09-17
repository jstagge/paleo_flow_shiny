# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}


# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data, file, login) {
  
  user_name <- chartr(" .,'", "____", data[[1]])
  
  the_hash <- digest::digest(data)
  fileName <- sprintf("%s_%s", humanTime(), user_name)
  
  meta_file <- file.path(responsesDir, paste0("meta/",fileName, "_meta_", the_hash,".csv"))
  reconst_file <- file.path(responsesDir, paste0("reconst/",fileName, "_reconst_", the_hash,".csv"))
  
  ### Save metadata and reconstruction
  write.csv(x = data, file = meta_file, row.names = FALSE, quote = TRUE)
  file.copy(file, reconst_file)

 ### Send an email with a backup copy  
  mailR::send.mail(from = "noresponse@donotrespond.com",
          to = "james.stagge@usu.edu",
          subject = "New paleoflow record",
          body = "This is the body",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = login[[1]], passwd = login[[2]], ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c(meta_file, reconst_file) ,
          debug = T) 
          
}
