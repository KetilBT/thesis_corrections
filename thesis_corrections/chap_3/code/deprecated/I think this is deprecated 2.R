# extract the i'th objective function
# extract the i'th optimal design
# generate the D-efficiency profile function

od <- optDes_beta1$optimDesign

# objective functions used to generate od
ofs <- unlist(optDes_beta1$objFuns)

# range of alternative sampling times for t5
t5s <- seq(0, 4, length.out = 100)

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

        