#%% Question 1
getDefectiveProb <- function(p_1,p_2,number.N1,number.N2,number.sample){
    N1 = rbinom(n = number.N1, size = 1, prob = p_1)
    N2 = rbinom(n = number.N2, size = 1, prob = p_2)
    N = c(N1,N2)
    prob = sum(sample(N,number.sample))/number.sample
    return (prob)
}


#%% Question 2
p_1 = 0.05
p_2 = 0.02
number.N1 = 10000
number.N2 = 20000
number.sample = 200
simulation = 5000

MultiDefectiveProb = replicate(n = simulation, expr = getDefectiveProb(p_1,p_2,number.N1,number.N2,number.sample))

#%%
hist(MultiDefectiveProb)

#%%
quantile(MultiDefectiveProb,probs = c(0.025, 0.975))

#%%
mean(MultiDefectiveProb)

#%% True value
(number.N1*p_1+number.N2*p_2)/(number.N1+number.N2)

#%% Question 3
N1 = 10000
N2 = 20000
p = 0.1
e = 0.05
simulation = 50

getlessThanEProb <- function(N1,N2,p,e,simulation,sample){
  prob = mean(replicate(n = simulation, expr = abs(getDefectiveProb(p,p,N1,N2,sample)-p)<e))
  return (prob)
}

library(parallel)
RepParallel <- function(n, expr, simplify = "array",...){
  mc <- getOption("mc.cores", detectCores())
  answer <-
    mclapply(integer(n), eval.parent(substitute(function(...) expr)),mc.cores = mc,...)
  if (!identical(simplify, FALSE) && length(answer))
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}


getSample <- function(p,e,N1,N2){
  top = N1+N2
  button = 1
  while(button<top){
    pin_sample = ceiling((top-button)/2+button)
    pin_Pr = mean(RepParallel(20,getlessThanEProb(N1,N2,p,e,30,pin_sample)))
    if (pin_Pr > 0.95){
      top = pin_sample - 1
    }
    else{
      button = pin_sample + 1
    }
  }
  return (top)
}

system.time({
  ans <- getSample(p,e,N1,N2)
})
ans

sample.distribution = replicate(n = 40, expr = getSample(p,e,N1,N2))
mean(sample.distribution)
hist(sample.distribution)


rbinom(n = 10, size = 1, prob = 0.5)


ceiling(3.1)
