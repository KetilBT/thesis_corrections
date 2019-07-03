t_5s = seq(0, 4, length.out = 100)

val_objFuns_t_5s = sapply(1:nrow(optDes_beta1$optimDesign), function(i){
  objFun_i = optDes_beta1$objFuns[i,][[1]]
  optimDesign_i = optDes_beta1$optimDesign[i,]
  val_objFun_i_t_5s = sapply(t_5s, function(t_5) objFun_i(c(optimDesign_i[-5], t_5)))
  return(val_objFun_i_t_5s)
})

matplot(pts, My, 
        type = "l", 
        col = 1,
        lty = 1,
        xlab = expression(t[5]), 
        ylab = expression(phi~"("~xi~")"))

for(i in 1:nrow(od)){
  of_i = optDes_beta1$objFuns[i,][[1]]
  od_i = optDes_beta1$optimDesign[i,]
  points(optimDesign_i[5], objFun_i())
}

xistar_ = t(sapply(1:nrow(optDes_beta1$optimDesign), function(i){
  f = optDes_beta1$objFuns[i,][[1]]
  x = optDes_beta1$optimDesign[i,]
  c(x[5], f(x))
}))

points(xy_star)

matplot(pts, t(apply(My, 1, function(x) exp(xy_star[,2] - x)^(1/5))),
        type = "l", 
        col = 1,
        lty = 1,
        xlab = expression(t[5]), 
        ylab = expression(phi~"("~xi~")"))
      