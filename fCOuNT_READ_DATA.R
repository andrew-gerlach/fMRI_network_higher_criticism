

fCOuNT_READ_DATA = function(data) {

  # Do nothing if data is already a data frame
  if(!is.data.frame(data)) {

    # Check if data is filename
    if(is.character(data)) {

      # Ensure file exists
      if(file.exists(data)) {

        # Determine file type and read in as appropriate
        file_type = tolower(file_ext(data))
        if(file_type == "csv") {
          data = read.csv(data)
        } else if(file_type == "tsv") {
          data = read.table(data, sep = "\t")
        } else if(file_type == "xlsx" | file_type == "xls") {
          data = read_excel(data)
        } else {
          stop("Data file of unrecognized type, please provide .csv, .tsv or Excel file type")
        }

      } else {

        stop("Data file does not seem to exist!")

      }

    } else {

      stop("Unrecognized format of data! Please supply a data frame/tibble or readable filename")

    }

  }

  return(data)

}

