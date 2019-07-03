library(doptim)

objFun <- genObjFun(
  eta = expression(
    (beta1 + b1) + (beta2+b2) * t
    ), 
  Ebeta = c(0, 1), 
  Vb = c(Vb1 = 1, Vb2 = 1)/10, 
  Veps = .25^2, 
  phi = "fixed",
  desvarNames = "t",
  noSamples = c(6)
  )

Xi0 <- c(1, 2, 4, 6, 8, 10)
objFun(Xi0)

nlminb(
    start = Xi0,
    objective = objFun,
    lower = 0,
    upper = 10
)


args <- list(
  eta = expression(
    (beta1 + b1) + (beta2+b2) * t
  ), 
  Ebeta = c(0, 1), 
  Vb = c(Vb1 = 1, Vb2 = 1)/10, 
  Veps = .25^2, 
  phi = "fixed",
  desvarNames = "t",
  noSamples = c(6)
)


args$Vb = c(Vb1 = 1, Vb2 = 1)/10000

objFun <- do.call(genObjFun, args)

nlminb(
  start = Xi0,
  objective = objFun,
  lower = 0,
  upper = 10
)
