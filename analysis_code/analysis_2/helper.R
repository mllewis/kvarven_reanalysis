
# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(res.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res = read.csv( "stats_for_paper.csv",
                    stringsAsFactors = FALSE,
                    colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res$name) ) res[ res$name %in% name, ] = new.rows
    else res = rbind(res, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res = new.rows
  }
  
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )

  
  if ( print == TRUE ) {
    View(res)
  }
}