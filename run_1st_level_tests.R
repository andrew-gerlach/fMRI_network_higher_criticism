# ARoG: temporary script for running 1st level tests

fCOuNT_RUN_1ST_LEVEL_TESTS = function(data, fc, test_type, form, var_idx) {

  k = dim(fc)[2]
  K = k * (k - 1) / 2

  first_level_results = data.frame(node1=numeric(K),
                                   node2=numeric(K),
                                   direction=character(K),
                                   test_statistic=numeric(K),
                                   p_low=numeric(K),
                                   p_high=numeric(K))

  idx = 0
  for(i in 1 : (k - 1)) {
    for(j in (i + 1) : k) {

      # fill in table
      idx = idx + 1
      first_level_results$node1[idx] = i
      first_level_results$node2[idx] = j

      # add fc to data
      data$fc = fc[, i, j]

      # TODO: revisit this criteria
      if(sum(is.na(data$fc)) > (nrow(data) / 2)) {

        first_level_results$test_statistic[idx] = NA
        first_level_results$p_low[idx] = NA
        first_level_results$p_high[idx] = NA
        next

      }

      # perform tests
      if(test_type == "t.one") {

        # one sample t test
        mod = t.test(data$fc)
        first_level_results$test_statistic[idx] = mod$statistic
        first_level_results$p_low[idx] = pt(mod$statistic, mod$parameter)
        first_level_results$p_high[idx] = pt(-mod$statistic, mod$parameter)

      } else if(test_type == "t.two") {

        # two sample t test
        mod = t.test(form, data)
        first_level_results$test_statistic[idx] = mod$statistic
        first_level_results$p_low[idx] = pt(mod$statistic, mod$parameter)
        first_level_results$p_high[idx] = pt(-mod$statistic, mod$parameter)

      } else if(test_type == "anova") {

        # anova
        mod = aov(form, data)
        first_level_results$test_statistic[idx] = summary(mod)[[1]][["F value"]][1]
        first_level_results$p_low[idx] = summary(mod)[[1]][["Pr(>F)"]][1]
        first_level_results$p_high[idx] = NA

      } else if(test_type == "regression") {

        # linear regression
        mod = lm(form, data)
        coefs = coef(summary(mod))
        first_level_results$test_statistic[idx] = coefs[var_idx + 1, 3]
        first_level_results$p_low[idx] = pt(coefs[var_idx + 1, 3], mod$df.residual)
        first_level_results$p_high[idx] = pt(-coefs[var_idx + 1, 3], mod$df.residual)

      } else if(test_type == "mlr") {

        # multilevel regression
        mod = lmer(form, data)
        coefs = coef(summary(mod))
        first_level_results$test_statistic[idx] = coefs[var_idx + 1, 4]
        first_level_results$p_low[idx] = pt(coefs[var_idx + 1, 4], coefs[var_idx + 1, 3])
        first_level_results$p_high[idx] = pt(-coefs[var_idx + 1, 4], coefs[var_idx + 1, 3])

      } else {
        stop(paste("Test type", test_type, "is not supported!"))
      }
    }
  }

  return(first_level_results)

}
