#' fCOuNT_RUN_1st_LEVEL_TESTS
#'
#' @param data subject information/subject number (data.frame, n rows)
#' @param fc functional connectivity matrices (3D array, n x k x k)
#' @param test_type first level test type (t.one, t.two, lr, anova)(string)
#' @param form formula for fist level test (string)
#' @param var_idx variable of interest (string)
#'
#' @return first_level_results first level summary results (p-values, test statistic, node connections)
#'
#' @export

fCOuNT_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, form, var_idx, custom_fun = NULL) {

  plan(multisession)

  test_funs = list("t.one"  = fCOuNT_run_t_one,
                   "t.two"  = fCOuNT_run_t_two,
                   "anova"  = fCOuNT_run_anova,
                   "lr"     = fCOuNT_run_lr,
                   "mlr"    = fCOuNT_run_mlr,
                   "custom" = custom_fun)

  test_fun = test_funs[[test_type]]

  # get edge information
  k = dim(fc)[2]
  edges = which(upper.tri(matrix(0, k, k)), arr.ind = TRUE)
  # convert to more natural row-wise ordering
  edges = edges[order(edges[, 1]), ]
  K = nrow(edges)

  first_level_results = future_lapply(seq_len(K),
                                      function(idx) {
                                        fCOuNT_run_tests(idx,
                                                          data,
                                                          fc,
                                                          form,
                                                          var_idx,
                                                          test_fun,
                                                          edges) } ) %>%
    bind_rows() %>%
    remove_rownames()

  return(first_level_results)

}

fCOuNT_run_tests = function(idx, data, fc, form, var_idx, test_fun, edges) {

  # get matrix indices
  i = edges[idx, 1]
  j = edges[idx, 2]

  fc_vec = fc[, i, j]

  # require at least half of FC entries to exist (revisit this)
  if(sum(is.na(fc_vec)) > (length(fc_vec) / 2)) {

    return(data.frame(node1 = i,
                      node2 = j,
                      test_statistic = NA,
                      p_low = NA,
                      p_high = NA))

  } else {

    tmp = test_fun(fc_vec = fc_vec,
                   data = data,
                   form = form,
                   var_idx = var_idx)

    return(data.frame(node1 = i,
                      node2 = j,
                      test_statistic = tmp$test_statistic,
                      p_low = tmp$p_low,
                      p_high = tmp$p_high))

  }

}

### Test-type specific helper functions

fCOuNT_run_t_one = function(fc_vec, data, form = NULL, var_idx = NULL) {

  mod = t.test(fc_vec)
  return(list(test_statistic = unname(mod$statistic),
              p_low = pt(mod$statistic, mod$parameter),
              p_high = pt(-mod$statistic, mod$parameter)))

}

fCOuNT_run_t_two = function(fc_vec, data, form, var_idx = NULL) {

  data$fc = fc_vec
  mod = t.test(form, data)
  return(list(test_statistic = unname(mod$statistic),
              p_low = pt(mod$statistic, mod$parameter),
              p_high = pt(-mod$statistic, mod$parameter)))

}

fCOuNT_run_anova = function(fc_vec, data, form, var_idx = NULL) {

  data$fc = fc_vec
  mod = aov(form, data)
  return(list(test_statistic = summary(mod)[[1]][["F value"]][var_idx],
              p_low = summary(mod)[[1]][["Pr(>F)"]][var_idx],
              p_high = NA))

}

fCOuNT_run_lr = function(fc_vec, data, form, var_idx) {

  data$fc = fc_vec
  mod = lm(form, data)
  coefs = coef(summary(mod))
  tval = coefs[var_idx + 1, 3]
  return(list(test_statistic = tval,
              p_low = pt(tval, mod$df.residual),
              p_high = pt(-tval, mod$df.residual)))

}

fCOuNT_run_mlr = function(fc_vec, data, form, var_idx) {

  data$fc = fc_vec
  mod = lmer(form, data)
  coefs = coef(summary(mod))
  tval = coefs[var_idx + 1, 4]
  df = coefs[var_idx + 1, 3]
  return(list(test_statistic = tval,
              p_low = pt(tval, df),
              p_high = pt(-tval, df)))

}

