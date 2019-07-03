
args <- list(
  eta = expression(
    (beta1 + b1) * exp((beta2 + b2)*t)
  ), 
  Ebeta = c(1, - 1/10), 
  Vb = c(1, 1)/100, 
  Veps = 1/1000, 
  phi = "fixed",
  desvarNames = "t",
  noSamples = c(6)
)


objFun <- do.call(genObjFun, args)

Xi0 = 1:6/10

nlminb(
  start = Xi0,
  objective = objFun,
  lower = 0,
  upper = 30
)

var_mags <- seq(1/10^3, 1/10^5, length.out = 20)

optim_desns <- t(sapply(var_mags,
       function(x){
         args$Vb <- c(1, 1) * x
         objFun <- do.call(genObjFun, args)
         res <- try(nlminb(
           start = Xi0,
           objective = objFun,
           lower = 0,
           upper = 30
         )$par)
         if(class(res) == "try-error") return(rep(NA, length(Xi0)))
         return(res)
         }))

pdf(file = "C:/Users/kbt57713/Documents/Research/Reports/thesis_corrections/chap_3/code/output/02 - exponential decay - desngraph.pdf")
matplot(var_mags, optim_desns)
dev.off()
       