library(doptim)

args0 <- list(
  eta = expression(
    exp((beta2 + b2) - (beta3 + b3))/
      (exp(beta2+b2)-exp(beta1+b1)) *
      (exp(-exp(beta1+b1)*t) - exp(-exp(beta2 + b2)*t))
  ),
  Ebeta = c(0, 1, 1),
  Vb = c(0.05, 0.05, 0.1),
  Veps = 0.05^2,
  phi = "fixed",
  desvarNames = "t",
  noSamples = c(5),
  reduced = TRUE
)

# Table 1, row 1 ----------------------------------------------------------

objFun <- do.call(genObjFun, args0)

Xi0 <- seq(0, 4, length.out = 5)
nlminb(start = Xi0, objective = objFun, lower = 0, upper = 4)


# Table 2, row 2 ----------------------------------------------------------


args <- args0
args$returnFIM <- TRUE
objFun <- do.call(genObjFun, args)

f1 <- function(x) - log(det(objFun(x)[[1]][1:3, 1:3]))
f2 <- function(x) - log(det(objFun(x)[[1]][-(1:3), -(1:3)]))
f <- function(x) .5 * f1(x) + .5*f2(x)

Xi0 <-  seq(0, 4, length.out = 5)
nlminb(start = Xi0, objective = f, lower = 0, upper = 4)[c("par", "objective")]


# Table 2, row 1 ----------------------------------------------------------

# note: yes, I know the order is inverted. Row 2 was more interesting and row 1 is just a "bonus"

nlminb(start = Xi0, objective = f2, lower = 0, upper = 4)[c("par", "objective")]


# Table 3, row 1 ----------------------------------------------------------


args <- args0
args$noSamples <- c(3, 3)
objFun <- do.call(genObjFun, args)
Xi0 <- c(1:3, 1:3)
nlminb(start = Xi0, objective = objFun, lower = 0, upper = 4)[c("par", "objective")]


# Table 4, row 1 ----------------------------------------------------------

args <- args0
args$returnFIM <- TRUE

args1 <- args
args1$noSamples <- 3
f1 <- function(Xi) do.call(genObjFun, args1)(Xi)[[1]][1:3, 1:3]

args2 <- args
args2$noSamples <- 2
f2 <- function(Xi) do.call(genObjFun, args2)(Xi)[[1]][1:3, 1:3]

args3 <- args
args3$noSamples <- 1
f3 <- function(Xi) do.call(genObjFun, args3)(Xi)[[1]][1:3, 1:3]


f <- function(Xi) -log(det(
  30*f1(Xi[1:3]) + 4*f2(Xi[4:5]) + 2*f3(Xi[6])
  ))

case <- DEoptim::DEoptim(f, lower = rep(0, 6), upper = rep(4, 6))
nlminb(start = case$optim$bestmem, objective = f, lower = 0, upper = 4)[c("par", "objective")]

# check against article:
# x <- c(
#   0.2171453,
#   0.9761316 ,
#   2.495429,
#   0.7372122,
#   2.410444,
#   0.2618686
# )
# 
# f(x)
