library(knitr)
library(here)

# create r scripts from rmd
purl <- knitr::purl(here::here("SOE-NEFMC-2019.Rmd"))
knitr::read_chunk(purl) 
chunks <- knitr:::knit_code$get()
invisible(mapply(function(chunk, name) {
  writeLines(c(paste0(""), chunk), paste0("NE-",name,".R"))
}, chunks, names(chunks)))
unlink(purl) # delete the original purl script
knitr:::knit_code$restore() # remove chunks from current knitr session

