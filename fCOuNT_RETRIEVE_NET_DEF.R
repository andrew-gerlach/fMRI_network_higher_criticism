fCOuNT_RETRIEVE_NET_DEF = function(net_def, net_def_col) {

  if(!is.vector(net_def) | length(net_def) == 1) {

    # read in file name is provided
    if(is.character(net_def)){
      if(file.exists(net_def)) {
        file_type = tolower(file_ext(net_def))
        if(file_type == "csv") {
          net_def = read.csv(net_def)
        } else if(file_type == ".xlsx" | file_type == ".xls") {
          net_def = read_excel(net_def)
        } else {
          stop("Network definition file of unrecognized type, please provide .csv or Excel filetype")
        }
      } else {
        stop("Network definition file does not seem to exist!")
      }
    }

    # pull network definition column if needed
    if(is.data.frame(net_def)) {
      if(missing(net_def_col)) {
        stop("Network definition column name needed to read in network definition from table")
      } else {
        net_def = net_def %>% pull(net_def_col)
      }
    } else {
      stop("Unrecognized format of network definition! Please supply a data frame/tibble or readable filename (full path)")
    }

  }

  return(net_def)

}
