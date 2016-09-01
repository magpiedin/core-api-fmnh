# Core API scraper for "FMNH" collections...
# built following these words of [wisdom?]:
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

#install.packages("httr")
library("httr")
library("jsonlite")

setwd("C:/Users/kwebbink/Documents/EMu/BibScraper")

ua <- user_agent("http://github.com/magpiedin/core-api-fmnh")


#https://core.ac.uk/api-v2/articles/search/fmnh?page=1&pageSize=10&metadata=true&fulltext=true&citations=false&similar=false&duplicate=false&urls=true&faithfulMetadata=false&apiKey=V37fDxTuPpsRchmjeH9JUqlKk0GSCLIg
query <- "fmnh"

key <- paste0("&apiKey=","V37fDxTuPpsRchmjeH9JUqlKk0GSCLIg")


core_path <- "/api-v2/articles/search/"
parameters <- paste0(core_path,
                     query, 
                     "?page=3",
                     "&pageSize=10",
                     "&metadata=true",
                     "&fulltext=true",
                     "&citations=false",
                     "&similar=false",
                     "&duplicate=false",
                     "&urls=true",
                     "&faithfulMetadata=false",
                     key)



core_api <- function(path) {
  url <- modify_url("https://core.ac.uk", path = path)
  
  resp <- GET(url)
#  if(http_type(resp) != "application/json") {
#    stop("API did not return json", call. = FALSE)
#  }
  
  #parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  
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

respdf <- as.matrix(c(list(resp)))
resp_ext <- c()

for (i in 7:ceiling(resp$content$totalHits/10)) {
  
  parameters_ext <- paste0(core_path,
                              query, 
                              "?page=",i,
                              "&pageSize=10",
                              "&metadata=true",
                              "&fulltext=true",
                              "&citations=false",
                              "&similar=false",
                              "&duplicate=false",
                              "&urls=true",
                              "&faithfulMetadata=false",
                              key)
  
  resp_ext <- core_api(parameters_ext)
  
  #resp2$content$data[i*10:(i*10+9)] <- resp_ext$content$data[1:10]
  #resp2$content$data[length(resp)+1] <- list(resp_ext)
  #resp2 <- append(resp2, c(resp_ext))
  respdf <- as.matrix(rbind(list(resp_ext), respdf))
  

  
  Sys.sleep(11)
  
}


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

