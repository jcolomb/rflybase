#' Returns the stock number when the FBID number is provided, only 
#' one FBID number can be given at a time
#' 
#' @export
#' @param conn postgresql connection object, from a call to 
#' \code{\link{fb_connect}}
#' @param fbst (character) flybase stock number 
#' @return single character string of the stock number
#' @examples \dontrun{
#' conn <- fb_connect()
#' fb_get_stocknumber(conn, fbst = "FBst0000002")
#' }
fb_get_stocknumber <- function(conn, fbst) {
  query <- paste0(
    "SELECT s.name
  FROM stock s
    WHERE s.uniquename='",fbst,"' ;")
  
  res <- RPostgreSQL::dbGetQuery(conn, query)
  
  
  as.character(res)
}
