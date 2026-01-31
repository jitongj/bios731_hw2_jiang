get_beta_hat <- function(fit, term = "x"){
  unname(coef(fit)[term])
}

get_se_hat <- function(fit, term = "x"){
  sqrt(vcov(fit)[term, term])
}
