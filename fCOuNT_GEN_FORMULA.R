
fCOuNT_GEN_FORMULA = function(data, test_type, form, var, controls) {

  # Construct formula if needed
  if(is.null(form)) {

      # TODO: need more logic here to construct formula if var and controls supplied but not form

  } else if(is.character(form) | is_formula(form)) {

    # Convert text to formula
    if(is.character(form)) { form = as.formula(form) }

    # Issue warnings if formula and controls were defined
    if(!is.null(controls)) {
      warning("Control variables being ignored since a formula was supplied!")
    }

    # format formula as characters and extract x and y components
    tmp = str_replace_all(format(form), " ", "")
    tmp = str_split(tmp, "~", simplify=T)
    y = tmp[1]
    x = str_split(tmp[2], "\\+", simplify=T)

    # check that FC is present
    if(tolower(y) != "fc" & !("fc" %in% tolower(x))) {
      stop("Formula must contain fc variable!")
    }

  } else {

    stop("Unsupported format for form, please provide text or formula")

  }

  # Determine index for variable of interest
  if(is.null(var)) {

    # get FC index assuming outcome is variable of interest
    if("fc" %in% x) {

      var_idx = which(x == "fc")

    # if variable of interest is not supplied, assume first in formula
    } else {

      var_idx = 1
      warning(paste("No variable of interest supplied, Performing inference on first variable in formula:", x[1]))

    }

  } else {

    # Ensure variable exists in data frame
    if(var %in% names(data)) {

      # get variable of interest index if FC is outcome
      if(y == "fc") {

        var_idx = which(x == var)

      # get fc index if variable of interest is outcome
      } else {

        var_idx = which(x == "fc")

      }

    } else {

      stop("Variable of interest does not exist in the data!")

    }

  }

  return(list(form=form, var_idx=var_idx))

}

