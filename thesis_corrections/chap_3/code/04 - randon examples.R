
# calc_aPMS, one-comp -----------------------------------------------------

library(randon)

# one-comp regression function
eta = expression(
  exp(beta2 - beta3)/
    (exp(beta2)-exp(beta1)) *
    (exp(-exp(beta1)*t) - exp(-exp(beta2)*t))
)

# value of the fixed effects parameter
beta = list(beta1 = 0, beta2 = 1, beta3 = 1)

# interval over which the regression function is considered
tInt = c(0, 4)

calc_aPMS(eta, paramName_variedParam = "beta3", beta = beta, tInt = tInt)


# changing varLevel arg ---------------------------------------------------


calc_aPMS(eta, paramName_variedParam = "beta3", beta = beta, tInt = tInt, 
          varLevel = .05)


# sensBands, one-comp -----------------------------------------------------

m <- list(
  eta = expression(
    (exp(beta2) / exp(beta3)) /
      (exp(beta2) - exp(beta1)) *
      (exp( - exp(beta1) * t) - exp( - exp(beta2) * t))
  ),
  beta = list(beta1 = 0, beta2 = 1, beta3 = 1),
  tInt = c(0, 4)
)

plot_sensBands(model = m, VLrange_variedParam = .1)
plot_sensBands(model = m, 
               VLrange_variedParam = c(.1, .2, .3))
plot_sensBands(model = m, 
               VLrange_variedParam = c(.1, .2, .3), 
               paramName_variedParam = c("beta1", "beta2","beta3"))


# genOptimDesnRange, one-comp ---------------------------------------------

set.seed(123)
optDes_beta1 <- genOptimDesnRange(m, noSamples = 5)

names(optDes_beta1)


# inspect profile of objFuns ----------------------------------

{
  
  windows(width = 5, height = 5)
  
  # optimal designs from previous optimisation
  od <- optDes_beta1$optimDesign
  
  # objective functions used to generate od
  ofs <- unlist(optDes_beta1$objFuns)
  
  # range of alternative sampling times for t5
  t5s <- seq(0, 4, length.out = 100)
  
  # value of objective functions in range of t5s
  vof_t5s = sapply(1:nrow(od), function(i){
    # i'th objective function
    of_i = ofs[[i]]
    # i'th optimal design
    od_i = od[i,]
    # value of i'th objective function 
    # when modifying t5 in optimal design
    vof_i_t5s = sapply(t5s, function(t5){
      of_i(c(od_i[-5], t5))
    })
    return(vof_i_t5s)
  })
  
  matplot(t5s, vof_t5s, 
          type = "l", 
          col = 1,
          lty = 1,
          xlab = expression(t[5]), 
          ylab = "",
          xlim = c(0,4.4))
  text(cbind(4.2, vof_t5s[100,]), paste0("i = ", 1:nrow(od)), cex = .6)
  title(ylab=expression({psi[.]}^{(i)}~"("~t[5]~")"), line=2, cex.lab=1.2)
  
  file_name = "C:/Users/ketil/OneDrive/Documents/research/Research/Reports/thesis_corrections/chap_3/code/output/04 - randon examples - vals_objFun_t5s"
  savePlot(filename = file_name, type = "pdf")
  
  
  # adding optimal designs --------------------------------------------------
  
  
  # value of objective functions in optimal t5s
  vof_od = sapply(1:nrow(od), function(i){
    # i'th objective function
    of_i = ofs[[i]]
    # i'th optimal design
    od_i = od[i,]
    # value of i'th objective function 
    # when t5 is optimal
    vof_i_od_i = of_i(od_i)
    return(vof_i_od_i)
  })
  
  points(od[,5], vof_od)
  
  file_name = "C:/Users/ketil/OneDrive/Documents/research/Research/Reports/thesis_corrections/chap_3/code/output/04 - randon examples - vals_objFun_t5s - with optimal t5"
  savePlot(filename = file_name, type = "pdf")
  

# efficency profiles as fn of t5 ------------------------------------------

  
  # D-efficiency profiles in range of t5s
  dEff_t5s = sapply(1:nrow(od), function(i){
    # i'th objective function
    of_i = ofs[[i]]
    # i'th optimal design
    od_i = od[i,]
    # value of i'th objective function 
    # when modifying t5 in optimal design
    dEff_i_t5s = sapply(t5s, function(t5){
      exp(of_i(od_i) - of_i(c(od_i[-5], t5)))^(1/length(optDes_beta1$model$beta))
    })
    return(dEff_i_t5s)
  })
  
  matplot(t5s, dEff_t5s,
          type = "l",
          lty = 1, 
          col = 1,
          xlab = expression(t[5]),
          ylab = "",
          xlim = c(0, 4.4))
  text(cbind(4.2, dEff_t5s[100,]), paste0("i = ", 1:nrow(od)), cex = .6)
  title(ylab = expression({D[eff]}^{(i)}), line = 2, cex.lab = 1.2)
  
  file_name = "C:/Users/ketil/OneDrive/Documents/research/Research/Reports/thesis_corrections/chap_3/code/output/04 - randon examples - dEff_t5s"
  savePlot(filename = file_name, type = "pdf")
  dev.off()
  
}

