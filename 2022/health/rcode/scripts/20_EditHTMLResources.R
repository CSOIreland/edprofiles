
library(stringr)


FolderPath <- "//CMADMIN01/BigDataDev/Projects/HealthProfiling/CompletedReports/HTMLExMapsEdit"


StartPattern <- "<script src=\""
EndPattern <- "/"
ReplacementText <- "../resources"  

StartPattern2 <- "<link href=\""

# Get list of HTML files in the folder
HtmlFiles <- list.files(FolderPath, pattern = "\\.html$", full.names = TRUE)

Counter <- 1
# Loop over each HTML file
for (File in HtmlFiles) {
  
  HtmlContent <- readLines(File, warn = FALSE)
  
  FirstPart <- HtmlContent[1:240]
  RemainingPart <- if (length(HtmlContent) > 240) HtmlContent[241:length(HtmlContent)] else character(0)
  
  
  FirstPartText <- paste(FirstPart, collapse = "\n")
  
  
  ModifiedFirstPart <- str_replace_all(FirstPartText, 
                                   paste0("(?<=", StartPattern, ").*?(?=/)"), 
                                   ReplacementText)
  
  ModifiedFirstPart2 <- str_replace_all(ModifiedFirstPart, 
                                       paste0("(?<=", StartPattern2, ").*?(?=/)"), 
                                       ReplacementText)
  
  
  ModifiedFirstPartLines <- strsplit(ModifiedFirstPart, "\n")[[1]]
  

  NewContent <- c(ModifiedFirstPartLines, RemainingPart)
  

  writeLines(NewContent, File)
  
  print(paste0("Iteration ",Counter," complete (",round(Counter*100/length(HtmlFiles),1),"%)"))
  
  Counter <- Counter+1
}


