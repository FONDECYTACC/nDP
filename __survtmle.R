library(survtmle)
if(!require(survtmle)){devtools::install_github("benkeser/survtmle")}
#> survtmle: Targeted Learning for Survival Analysis
#> Version: 1.1.2
set.seed(341796)

# simulate data
n <- 100
t_0 <- 10
W <- data.frame(W1 = runif(n), W2 = rbinom(n, 1, 0.5))
A <- rbinom(n, 1, 0.5)
T <- rgeom(n,plogis(-4 + W$W1 * W$W2 - A)) + 1
C <- rgeom(n, plogis(-6 + W$W1)) + 1
ftime <- pmin(T, C)
ftype <- as.numeric(ftime == T)

# apply survtmle for estimation
fit <- survtmle(ftime = ftime, ftype = ftype,
                trt = A, adjustVars = W,
                glm.trt = "1",
                glm.ftime = "I(W1*W2) + trt + t",
                glm.ctime = "W1 + t",
                method = "hazard",
                t0 = t_0)

# extract cumulative incidence at each timepoint
tpfit <- timepoints(fit, times = seq_len(t_0))

# examine output object produced by the timepoints function
tpfit
#> $est
#>              t1         t2         t3         t4         t5         t6
#> 0 1 0.032997470 0.06492788 0.09582530 0.12572293 0.15465313 0.18264737
#> 1 1 0.008014555 0.01603567 0.02406256 0.03209448 0.04013064 0.04817027
#>             t7         t8         t9        t10
#> 0 1 0.20973629 0.23594966 0.26131640 0.28586459
#> 1 1 0.05621257 0.06425675 0.07230203 0.08034761
#> 
#> $var
#>               t1           t2           t3           t4           t5
#> 0 1 4.565496e-04 0.0005099637 0.0004951975 0.0004852183 0.0005862762
#> 1 1 2.111604e-06 0.0003345475 0.0003208485 0.0003079889 0.0006317931
#>               t6           t7           t8           t9         t10
#> 0 1 0.0012883360 0.0013743743 0.0012847990 0.0020298371 0.003137739
#> 1 1 0.0006229943 0.0009761833 0.0009544762 0.0009534062 0.000956650

# examine plot of cumulative incidences
plot(tpfit)