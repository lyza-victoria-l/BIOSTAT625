num_str = function(x) {
  pattern = as.character(x) # ensuring input is a string
  pattern_length = nchar(pattern) # length in digits of string
  buffer_zone = "" # in case we miss a pattern between batches.
  dynamic_limit = 1e10 # placeholder in case of any scary numbers Dr. Hui tests me on.
  current_number = 1 # start up current number
  total_digits = 0 # keep my memory sane.
  batch_size = 5000 # let's be safe here. 
  overlap = pattern_length - 1 # catch any numbers off batch edges.
  
  repeat {
    if (nchar(buffer_zone) >= overlap) {
      last_overlap <- substr(buffer_zone,
                             nchar(buffer_zone) - overlap + 1,
                             nchar(buffer_zone))
    } else {
      last_overlap <- buffer_zone
    }
    
    batch_string = paste(current_number:(current_number + batch_size - 1), collapse = "") # pull a string of numbers from
    full_string = paste0(last_overlap, batch_string)
    match_index = regexpr(pattern = pattern,
                          text = full_string,
                          fixed = T)
    
    if (match_index != -1) {
      index = total_digits - nchar(last_overlap) + match_index[1]
      
      return(index)
      break
    } else {
      current_number = current_number + batch_size
      buffer_zone = substr(batch_string,
                           nchar(batch_string) - overlap,
                           nchar(batch_string))
      total_digits = total_digits + nchar(batch_string)
      if (total_digits > dynamic_limit)
        return("Too big so I give up")
    }
  }
}
