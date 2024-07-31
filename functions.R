#Custom functions used for data cleaning 



# -------------------------------------------------------------------------

Zreplace <- function(dataframe, columns_to_replace, old_text, new_text) {
  require(rlang)
  ##Checks ##
  #ensure vector lengths are the same
  if(length(old_text) != length(new_text)){
    stop("Length of old_text and new_text must be equal")
  }
  
  #save whether vectors are characters to use for processing during input
  old_text_character <- is.character(old_text)
  new_text_character <- is.character(new_text)
  
  
  base_expression <- rlang::expr('dataframe %>% dplyr::mutate(across(all_of(columns_to_replace), ~dplyr::case_when(')
  argument_string <- c()
  for(i in seq_along(old_text)){
    
    #Character inputs need to be wrapped in commas or else they will be recognized as objects
    #by the Dplyr processing
    old_text_input <- ifelse(old_text_character, paste0("'",old_text[i],"'"),old_text[i])
    new_text_input <- ifelse(new_text_character, paste0("'",new_text[i],"'"),new_text[i])
    
    #On the last run of the loop, the final symbol does not include a comma
    if(i == length(old_text)){
      argument_element <- paste0('. == ', old_text_input, ' ~ ', new_text_input)
    } else {
      argument_element <- paste0('. == ', old_text_input ,' ~ ', new_text_input, ',')
    }
    argument_string <- paste0(argument_string, argument_element)
  }
  
  
  expression <- paste0(base_expression, argument_string, ')))')
  eval(rlang::parse_expr(expression))
}



