result <- rpubsUpload("Storm Analysis", "Storm_Analysis.html")
if (!is.null(result$continueUrl)) 
  browseURL(result$continueUrl) else stop(result$error)

# update the same document with a new title
updateResult <- rpubsUpload("Storm Analysis", "Storm_Analysis.html", result$id)