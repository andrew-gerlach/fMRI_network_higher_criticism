
fCOuNT_GEN_FORMULA = function(data, test_type, form, var, controls) {

  # flag for presence of interaction
  interaction = F
  
  # Construct formula if needed
  if(is.null(form)) {

    if(test_type != "t.one") {
      
      # TODO: need more logic here to construct formula if var and controls supplied but not form
      
    }

  } else if(is.character(form) | is_formula(form)) {

    # Convert text to formula
    if(is.character(form)) { form = as.formula(form) }

    # Issue warnings if formula and controls were defined
    if(!is.null(controls)) {
      warning("Control variables being ignored since a formula was supplied!")
    }

    # format formula as characters and extract x and y components
    tmp = paste(format(form), collapse="") %>%
      str_replace_all(" ", "") %>%
      str_split("~", simplify=T)
    y = tmp[1]
    x = str_split(tmp[2], "\\+", simplify=T)
    
    # separate out any interactions
    int_term = x[grepl("\\*", x)]
    # check for multiple interactions
    if (length(int_term) > 1) {
      stop("Only one interaction is currently supported in the formula")
    }
    # process interactions if present
    if (length(int_term) == 1) {
      z = str_split(int_term, "\\*", simplify=T)
      # check for 3-way+ interaction
      if (length(z) > 2) {
        stop("Only two-way interactions are currently supported in the formula")
      }
      # set interaction flag
      interaction = T
      # adjust list of terms
      idx = which(int_term == x)
      x = c(x[seq_len(idx - 1)], z, x[seq(idx + 1, length(x))], "interaction")
      x = x[!duplicated(x)]
    }
    
    # remove random effects
    x = x[!grepl("\\|", x)]

    # check that FC is present
    if(tolower(y) != "fc" & !("fc" %in% tolower(x))) {
      stop("Formula must contain fc variable!")
    }

  } else {

    stop("Unsupported format for form, please provide text or formula")

  }

  # Determine index for variable of interest
  
  if(test_type == "t.one") {
    
    # ignore variable of interest for a one-sided t-test
    var_idx = NULL
    
  } else {
    
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
      if(var == "intercept") {
        
        var_idx = 0
        
      } else if(var == "interaction") {
        
        # interactions always come last and only one is allowed
        var_idx = length(x)
        
      } else if(var %in% names(data)) {

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
    
    # check for categorical variables with more than two level and increment as needed 
    if(var_idx > 1) {
      for(i in 1 : (var_idx - 1)) {
        tmp = data %>% pull(x[i])
        if(is.factor(tmp)) {
          n = length(levels(tmp))
          var_idx = var_idx + n - 2
        }
      }
    }

  }

  return(list(form=form, var_idx=var_idx))

}

