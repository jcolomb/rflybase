#' Returns the genetic element name when the FBID number is provided, only 
#' one number can be given at a time
#' 
#' @export
#' @param conn postgresql connection object, from a call to 
#' \code{\link{fb_connect}}
#' @param fbid (character) fb id number 
#' @return single character string of the gene name
#' @examples \dontrun{
#' conn <- fb_connect()
#' fb_name_from_fbid(conn, fbid = "FBtp0000001")
#' fb_name_from_fbid(conn, fbid = "FBti0038368")
#' fb_name_from_fbid(conn, fbid = "FBba0000025")
#' fb_name_from_fbid(conn, fbid = "FBab0044956")
#' }
fb_name_from_fbid <- function(conn, fbid) {
  if (substr(fbid, 0, 4) == "FBal"){
    query <- paste0(
      "SELECT DISTINCT(s.name)
      FROM feature f, feature_synonym fs, synonym s, cvterm cvt, cvterm cvt2,
      feature_relationship fr, cvterm cvt3
      WHERE f.type_id = cvt.cvterm_id AND cvt.name = 'gene' AND
      f.feature_id = fs.feature_id AND fs.synonym_id = s.synonym_id AND
      fs.is_current = 't' AND fs.is_internal = 'f' AND
      s.type_id = cvt2.cvterm_id AND cvt2.name = 'symbol' AND
      f.is_obsolete = 'f' AND f.feature_id = subject_id AND
      fr.type_id = cvt3.cvterm_id AND cvt3.name = 'alleleof' AND
      f.uniquename = '", fbid,"';")
  }else if (substr(fbid, 0, 4) == "FBti"){
    query <- paste0("SELECT DISTINCT(s.synonym_sgml)
  FROM feature f, feature_synonym fs, synonym s, cvterm cvt
                    WHERE f.feature_id = fs.feature_id AND fs.synonym_id = s.synonym_id AND
                    fs.is_current = 't' AND fs.is_internal = 'f' AND
                    s.type_id = cvt.cvterm_id AND cvt.name = 'symbol' AND
                    f.is_obsolete = 'f' AND f.uniquename = '", fbid,"';" )
  }else if (substr(fbid, 0, 4) == "FBtp"){
    query <- paste0("SELECT DISTINCT s.synonym_sgml
  FROM feature f, feature_synonym fs, synonym s, cvterm cvt
                    WHERE f.feature_id = fs.feature_id AND fs.synonym_id = s.synonym_id AND
                    fs.is_current = 't' AND fs.is_internal = 'f' AND
                    s.type_id = cvt.cvterm_id AND cvt.name = 'symbol' AND
                    f.is_obsolete = 'f' AND f.uniquename = '", fbid,"';")
  }else if (substr(fbid, 0, 4) == "FBba"){
    query <- paste0("SELECT s.synonym_sgml
  FROM feature f, feature_synonym fs, synonym s, cvterm cvt, cvterm cvt2
                    WHERE f.type_id = cvt.cvterm_id AND cvt.NAME = 'single balancer' AND
                    f.feature_id = fs.feature_id AND fs.synonym_id = s.synonym_id AND
                    fs.is_current = 't' AND fs.is_internal = 'f' AND
                    s.type_id = cvt2.cvterm_id AND cvt2.NAME = 'symbol' AND
                    f.is_obsolete = 'f' AND f.uniquename = '", fbid,"' GROUP BY s.synonym_sgml;")
  }else if (substr(fbid, 0, 4) == "FBab"){
    query <- paste0("SELECT s.synonym_sgml
  FROM feature f, feature_synonym fs, synonym s, cvterm cvt, cvterm cvt2
                    WHERE f.type_id = cvt.cvterm_id AND cvt.NAME = 'chromosome_structure_variation' AND
                    f.feature_id = fs.feature_id AND fs.synonym_id = s.synonym_id AND
                    fs.is_current = 't' AND fs.is_internal = 'f' AND
                    s.type_id = cvt2.cvterm_id AND cvt2.NAME = 'symbol' AND
                    f.is_obsolete = 'f' AND f.uniquename = '", fbid,"' GROUP BY s.synonym_sgml;")
  }else {return = fbid}
  
  res <- RPostgreSQL::dbGetQuery(conn, query)
  
  if (NROW(res) > 1) message(fbid)
  if (NROW(res) == 0) res <- fbid
  
  as.character(res)
}
