# Core API scraper for "FMNH" collections...
# built following these words of [wisdom?]:
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

install.packages("httr")
#install.packages("tm")
library("httr")
library("jsonlite")
#library("tm")

setwd("/Users/admin/Documents/DataSci/CORE_NHpubs")
#setwd("C:/Users/kwebbink/Documents/EMu/BibScraper")

ua <- user_agent("http://github.com/magpiedin/core-api-fmnh")


#https://core.ac.uk/api-v2/articles/search/fmnh?page=1&pageSize=10&metadata=true&fulltext=true&citations=false&similar=false&duplicate=false&urls=true&faithfulMetadata=false&apiKey=V37fDxTuPpsRchmjeH9JUqlKk0GSCLIg
query <- "fmnh"


key <- paste0("&apiKey=","V37fDxTuPpsRchmjeH9JUqlKk0GSCLIg")


core_path <- "/api-v2/articles/search/"

parameters <- paste0(core_path,
                     query, 
                     "?page=1",
                     "&pageSize=10",
                     "&metadata=true",
                     "&fulltext=true",
                     "&citations=false",
                     "&similar=true",
                     "&duplicate=false",
                     "&urls=true",
                     "&faithfulMetadata=false",
                     key)

url <- modify_url("https://core.ac.uk", path = parameters)

core_api <- function(path) {
  url <- modify_url("https://core.ac.uk", path = path)
  
  resp <- GET(url)
    if(http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
  
  parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "Core API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "core_api"
  )
}


print.core_api <- function(x, ...) {
  cat("<Core ", x$path, ">\n", sep = "")
  str(x$content$data)
  invisible(x)
}


# run the thing!
resp <- core_api(parameters)

printed <- print.core_api(resp)


### iterate through pages of results:

resp.all <- resp$content$data
resp_ext <- list()


for (i in 2:ceiling(resp$content$totalHits/10)) {
#for (i in 2:5) {  # if need to test with just a few pages
    
  Sys.sleep(10)
    
  parameters_ext <- paste0(core_path,
                           query, 
                           "?page=",i,
                           "&pageSize=10",
                           "&metadata=true",
                           "&fulltext=true", #change this back?
                           "&citations=false",
                           "&similar=true",
                           "&duplicate=false",
                           "&urls=true",
                           "&faithfulMetadata=false",
                           key)
  
  resp_ext <- core_api(parameters_ext)
  
#  write.csv(do.call(rbind, lapply(resp_ext$content$data, unlist)), file=paste0("resp",i,".csv"))
  
  #resp2$content$data[i*10:(i*10+9)] <- resp_ext$content$data[1:10]
  #resp2$content$data[length(resp)+1] <- list(resp_ext)
  resp.all <- append(resp.all, resp_ext$content$data)
  #respdf <- as.matrix(rbind(list(resp_ext), respdf))

  assign(paste0("resp_ext",i), as.list(resp_ext$content$data))

}


respallJSON <- toJSON(resp.all, matrix = c("rowmajor"), pretty = TRUE)
write(respallJSON, file="respall.json")

#respTextID <- as.character(resp.all[1][[1]]$identifiers[1])
#
#for (i in 2:NROW(resp.all)) {
#  respTextID <- append(respTextID, as.character(resp.all[i][[1]]$identifiers[1]))
#}

#respText <- as.character(resp.all[1][[1]]$fullText)
#for (i in 2:NROW(resp.all)) {
#  respText <- append(respText, as.character(resp.all[i][[1]]$fullText))
#}

#respTextURL <- as.character(resp.all[1][[1]]$fulltextUrls[1])
#for (i in 2:NROW(resp.all)) {
#  respTextURL <- append(respTextURL, as.character(resp.all[i][[1]]$fulltextUrls[1]))
#}



# COMPARE # pubs involving FMNH specimens to # pubs on NH overall (roughly)

# run the thing again!  for "All pubs that include term 'natural history'"

pagesize <- 99
year <- as.numeric(substr(as.character(Sys.Date()), 1,4))


query2 <- "field%20museum%20AND%20year%3A"

paramNH <- paste0(core_path,
                  query2, year,
                  "?page=1",
                  "&pageSize=",pagesize,
                  "&metadata=true",
                  "&fulltext=false", #change this back?
                  "&citations=false",
                  "&similar=false",
                  "&duplicate=false",
                  "&urls=true",
                  "&faithfulMetadata=false",
                  key)

respNH <- core_api(paramNH)
printed <- print.core_api(respNH)

respNH.all <- respNH$content$data
respNH_ext <- list()

Sys.sleep(30)

for (y in 2012:year) {

  paramNH <- paste0(core_path,
                     query2, y,
                     "?page=1",
                     "&pageSize=",pagesize,
                     "&metadata=true",
                     "&fulltext=false", #change this back?
                     "&citations=false",
                     "&similar=false",
                     "&duplicate=false",
                     "&urls=true",
                     "&faithfulMetadata=false",
                     key)
  
  respNH_ext <- core_api(paramNH)
  printed <- print.core_api(respNH_ext)
  
  respNH.all <- append(respNH.all, respNH_ext$content$data)
  
  assign(paste0("respNH_ext",y,".1"), as.list(respNH_ext$content$data))

  Sys.sleep(75)
  
  ### iterate through pages of results:
  
#for (i in 2:ceiling(respNH_ext$content$totalHits/pagesize)) {
for (i in 2:5) {  # if need to test with just a few pages
  
  param2_ext <- paste0(core_path,
                           query2, y,
                           "?page=",i,
                           "&pageSize=", pagesize,
                           "&metadata=true",
                           "&fulltext=false", #change this back?
                           "&citations=false",
                           "&similar=false",
                           "&duplicate=false",
                           "&urls=true",
                           "&faithfulMetadata=false",
                           key)
  
  respNH_ext <- core_api(param2_ext)
  
  #  write.csv(do.call(rbind, lapply(resp_ext$content$data, unlist)), file=paste0("resp",i,".csv"))
  
  #resp2$content$data[i*10:(i*10+9)] <- resp_ext$content$data[1:10]
  #resp2$content$data[length(resp)+1] <- list(resp_ext)
  respNH.all <- append(respNH.all, respNH_ext$content$data)
  #respdf <- as.matrix(rbind(list(resp_ext), respdf))
  
  assign(paste0("respNH_ext",y,".",i), as.list(respNH_ext$content$data))
  
  Sys.sleep(50)
  #  assign(paste0("resp_extB",i), list(do.call(rbind, lapply(resp_ext$content$data, unlist))))
  #  assign(paste0("resp_ext",i), data.frame(do.call(rbind, lapply(resp_ext$content$data, unlist))))
  
  #write.csv(paste0("resp_ext",i), file=paste0("resp_ext",i,".csv"))
  
  # respMtx <- do.call(rbind, lapply(resp_ext$content$data, unlist))
  # respMtx.all <- merge(respMtx.all, respMtx, all=TRUE)
  
}
}

respNHallJSON <- toJSON(respNH.all, matrix = c("rowmajor"), pretty = TRUE)
write(respNHallJSON, file="respNHall.json")



# DOWNLOAD PDF FULL TEXT 
#https://core.ac.uk:443/api-v2/articles/get/16643862/download/pdf?apiKey=V...
respPDF <- respTextURL[grep("pdf", respTextURL)]

pdf_path1 <- "/api-v2/articles/get/"
pdf_path2 <- "/download/pdf?"

pdf_param <- paste0(pdf_path1, resp.all[1][[1]]$id, pdf_path2, key)
download.file(paste0("https://core.ac.uk", pdf_param), destfile = paste0(resp.all[1][[1]]$id, ".pdf"))

#for (i in 47:48) {  # If need to test the upcoming for-loop
for (i in 2:NROW(resp.all)) {
  if (!is.null(grep("pdf", resp.all[i][[1]]$fulltextUrls[1]))) {
    pdf_param <- paste0(pdf_path1, resp.all[i][[1]]$id, pdf_path2, key)
    download.file(paste0("https://core.ac.uk", pdf_param), destfile = paste0(resp.all[i][[1]]$id, ".pdf"))
  }
  else {
    Sys.sleep(1)
  }
}

# CONVERT PDF to TXT
# à la http://stackoverflow.com/questions/21445659/use-r-to-convert-pdf-files-to-text-files-for-text-mining
# install this & change folder reference below if needed:   http://www.foolabs.com/xpdf/download.html

# 1) make a vector of PDF file names
pubPDFs <- list.files(path = getwd(), pattern = "pdf",  full.names = TRUE)

# 2) convert each PDF file that is named in the vector into a text file [in the same directory as the PDFs]
lapply(pubPDFs, function(i) system(paste('"/Users/admin/Documents/DataSci/CORE_NHpubs/xpdf/bin64/pdftotext"', 
                                         paste0('"', i, '"')), wait = FALSE) )


# ANALYZE PDF FULL TEXT - for each pub, list specimen #s




## TRY one/some of these?
#TEST2<-  sapply(resp_ext$content$data, unlist)
#write.csv(as.data.frame(do.call(merge, list(by="id", all=TRUE, lapply(resp_ext$content$data, unlist)))), file=paste0("AAresp.csv"))
#TEST <- do.call(merge, list(by.x="id",by.y="id", all=TRUE, lapply(resp_ext$content$data, unlist)))


### TRASH THESE:
#assign(paste0("resp_extTEST"), data.frame(do.call(rbind, lapply(resp_ext$content$data, unlist))))
#write.csv(resp_extTEST, file="respTest.csv")

#xfor (i in 2:ceiling(resp$content$totalHits/10)) {
#x  write.csv(data.frame(do.call(rbind, lapply(paste0("resp_ext",i,"$content$data"), unlist))), file=paste0("Aresp",i,".csv"))
#x}

for (i in 2:ceiling(resp$content$totalHits/10)) { 
  write.csv(DFnames[i], file=paste0("res",i,".csv"))
}


  #assign(paste0("bound",i), do.call(rbind, lapply(paste0("resp_ext",i,"$content$data", unlist))))

  
#for (i in seq(1,length(resp.all), by=3)) {
#resp.all.Mtx2 <- do.call(rbind, lapply(resp.all[[i]][[3]], unlist))
#}

# specific articles = resp[[1]][[3]][1:10]
# specific aggregated articles = resp.all[[1:NROW(resp.all)/3]][[3]][1:10]
###   FIX THIS TO rbind these as.matrix? respTEST[[1,4,7,etc]][[3]]


indexes <- seq(1,length(resp.all), by=3)

#  respMtx <- as.matrix(resp$content$data)
# How best to export?






############################## ???
rate_limit <- function() {
  core_api("/rate_limit")
}

#...& check the thing!
#write(resp[[1]][3][1], file = "coreAPI.json")
#resp[[1]][3][1]

# my ham-handed attempt at compiling paginated results, based on:
# http://stackoverflow.com/questions/35903786/using-r-and-httr-to-retrieve-data-okta-api-get-request-with-paginated-link-hea

#respTable <- fromJSON(resp$content)

# placeholder for 
#resp_content <- list()

# library("request") might have something useful for pagination in near-ish future?

