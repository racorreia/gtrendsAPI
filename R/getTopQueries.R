#' Extracts information about the top queries associated with the requested search term
#'
#' @param terms The search term of interest. Only a single term can be queried at a time.
#' @param geo Applies a geographical restriction to data query. Parameter should be a string, please refer to ISO-3166-2 for the supported values.
#' @param property Defines the Google property to query data from. Options are images/news/froogle(Shopping)/youtube. Parameter should be a string, defaults to 'web' when unspecified.
#' @param category Defines a category filter for data extraction. Please consult with the Trends Explore page for possible legal input here. Parameter should be a string, defaults to 'All Categories' when unspecified.
#' @param startDate Defines the starting date for data extraction. Start date should be a month and a year in the format YYYY-MM e.g. 2010-01. Parameter should be a string, defaults to '2004-01' when unspecified.
#' @param endDate Defines the end date for data extraction. End date should be a month and a year in the format YYYY-MM e.g. 2010-01. Parameter should be a string, defaults to the current month and year when unspecified.
#' @param api.key Google API access key.
#' @return Returns the top 25 queries associated with the provided search term or topic.
#' @export
#'
#' @examples
#' \dontrun{
#' getTopQueries("apple", api.key = "YOUR_API_KEY")
#'
#' getTopQueries(terms="Amazon", geo = "US", property = "news",
#' startDate = "2014-01", endDate = "2014-12", api.key = "YOUR_API_KEY")
#' }



getTopQueries <- function(terms, geo=NULL, property=NULL, category=NULL, startDate=NULL, endDate=NULL, api.key) {

  #Base link of API call
  link <- "https://www.googleapis.com/trends/v1beta/topQueries?"

  #Add serach terms to link
  #Search terms should be a vector of up to five strings
  if (is.character(terms) == F){
    stop("terms must be in string format", call. = FALSE)
  }

  if (length(terms) > 1){
    stop("terms should be a single string", call. = FALSE)
  }else{
    link <- paste0(link, "term=", terms)
  }

  #Add Google API key to link
  if (is.character(api.key) == F){
    stop("api.key must be in string format", call. = FALSE)
  }

  link <- paste0(link, "&key=", api.key)

  #Add Geo filter to link
  if (is.null(geo) == F){
    if(is.character(geo) == F){
      stop("geo must be in string format", call. = FALSE)
    }else{
      if(geo %in% gtrendsAPI::regions[,1] == F){
        stop("geo must an ISO-3166-2 region code - call regions for currently supported codes", call. = FALSE)
      }else{
        link <- paste0(link, "&restrictions.geo=", geo)
      }
    }
  }

  #Add Property filter
  if (is.null(property) == F){
    if(is.character(property) == F){
      stop("property must be in string format", call. = FALSE)
    }else{
      if(property %in% gtrendsAPI::properties == F){
        stop("property must be one of \"web\", \"images\", \"news\", \"froogle\" or \"youtube\". Defaults to \"web\".", call. = FALSE)
      }else{
        link <- paste0(link, "&restrictions.property=", property)
      }
    }
  }

  #Add category filter
  if (is.null(category) == F){
    if(is.character(category) == F){
      stop("property must be in string format", call. = FALSE)
    }else{
      if(category %in% gtrendsAPI::categories[,1] == F){
        stop("property must be one of the categories recognized by Google", call. = FALSE)
      }else{
        categ <- as.numeric(unique(gtrendsAPI::categories[which(gtrendsAPI::categories==category),2]))
        link <- paste0(link, "&restrictions.category=", categ)
      }
    }
  }

  #Add startDate filter
  if (is.null(startDate) == F){
    if(is.character(startDate) == F){
      stop("startDate must be in string format", call. = FALSE)
    }else{
      if(grepl("^\\d{4}-\\d{2}$", startDate, perl=T) == F){
        stop("startDate should be a month and a year in the format YYYY-MM e.g. 2010-01", call. = FALSE)
      }else{
        link <- paste0(link, "&restrictions.startDate=", startDate)
      }
    }
  }

  #Add endDate filter
  if (is.null(endDate) == F){
    if(is.character(endDate) == F){
      stop("endDate must be in string format", call. = FALSE)
    }else{
      if(grepl("^\\d{4}-\\d{2}$",endDate, perl=T) == F){
        stop("endDate should be a month and a year in the format YYYY-MM e.g. 2010-01", call. = FALSE)
      }else{
        link <- paste0(link, "&restrictions.endDate=", endDate)
      }
    }
  }

  #Encode url
  link <- utils::URLencode(link)

  #GET call to extract data
  f <- httr::GET(link)

  if(httr::http_error(f) == T){

    stop(paste0(jsonlite::fromJSON(httr::content(f, as = "text"))$error$code, " - ", jsonlite::fromJSON(httr::content(f, as = "text"))$error$message), call. = FALSE)

  }else{

    #Convert data to JSON format
    d <- jsonlite::fromJSON(httr::content(f, as = "text"))

    #Arrange data table
    res <- d$item

    if(length(d)==0){

      res<-data.frame(a=NA, b=NA)
      names(res)<-c("topSearches", "value")

    }else{

      #Change column names to match standard output
      names(res)<-c("topSearches", "value")

    }

    #Add geographical scope of search
    if(is.null(geo) == T){
      res$geo<-"world"
    }else{
      res$geo<-geo
    }

    #Add temporal coverage
    if(is.null(startDate) == T & is.null(endDate) == T){
      res$time<-paste0("2004-01 ", format(Sys.Date(), "%Y-%m"))
    }

    if(is.null(startDate) == T & is.null(endDate) == F){
      res$time<-paste0("2004-01 ", endDate)
    }

    if(is.null(startDate) == F & is.null(endDate) == T){
      res$time<-paste0(startDate, " ", format(Sys.Date(), "%Y-%m"))
    }

    if(is.null(startDate) == F & is.null(endDate) == F){
      res$time<-paste0(startDate, " ", endDate)
    }

    #Add keyword
    res$keyword<-terms

    #Add property searched
    if(is.null(property) == T){
      res$gprop<-"web"
    }else{
      res$gprop<-property
    }

    #Add category filter
    if(is.null(category) == T){
      res$category<-"All categories"
    }else{
      res$category<-category
    }

  }

  return(res)

}


