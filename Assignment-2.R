#%% Question 1
single_test <- function(p_1,p_2,number.N1,number.N2,number.sample){
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

multi_test = replicate(n = simulation, expr = single_test(p_1,p_2,number.N1,number.N2,number.sample))

#%%
hist(multi_test)

#%%
quantile(multi_test,probs = c(0.025, 0.975))

#%%
mean(multi_test)

#%% True value
(number.N1*p_1+number.N2*p_2)/(number.N1+number.N2)

#%% Question 3
N1 = 10000
N2 = 20000
p = 0.1
e = 0.05
sample = 134
simulation = 50

CalculateProb <- function(N1,N2,p,e,simulation,sample){
  prob = mean(replicate(n = simulation, expr = abs(single_test(p,p,N1,N2,sample)-p)<e))
  return (prob)
}

#CalculateProb(N1,N2,p,e,simulation,114)

library(parallel)
RepParallel <- function(n, expr, simplify = "array",...){
  mc <- getOption("mc.cores", 2)
  answer <-
    mclapply(integer(n), eval.parent(substitute(function(...) expr)),mc.cores = mc,...)
  if (!identical(simplify, FALSE) && length(answer))
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}

#system.time({
#  hist(replicate(500,CalculateProb(N1,N2,p,e,simulation,sample)))
#})
#system.time({
#  hist(RepParallel(500,CalculateProb(N1,N2,p,e,simulation,sample)))
#})

#mean(RepParallel(50,CalculateProb(N1,N2,p,e,simulation,134)))
top = N1+N2
button = 1
repeat{
  pin_sample = ceiling((top-button)/2+button)
  cat(i,'\n','pin',pin_sample,' button:',button,' top:',top,'\n')
  pin_Pr = mean(RepParallel(200,CalculateProb(N1,N2,p,e,simulation,pin_sample)))
  if (pin_Pr > 0.95){
    top = pin_sample - 1
  }
  else{
    button = pin_sample + 1
  }
  if (top == button) break
}



Q3_d <- function(p,e,N1,N2,simulation){
  top = N1+N2
  button = 1
  while(button<top){
    pin_sample = ceiling((top-button)/2+button)
    pin_Pr = mean(RepParallel(50,CalculateProb(N1,N2,p,e,simulation,pin_sample)))
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
  ans <- Q3_d(p,e,N1,N2,simulation)
})
ans

sample.distribution = replicate(n = 400, expr = Q3_d(p,e,N1,N2,simulation))
mean(sample.distribution)
hist(sample.distribution)

mean(sample.distribution)

Q3 <- function(p,e,N1,N2,simulation){
  N = N1+N2
  for (sample in 120:140){
    mean_Pr = mean(RepParallel(200,CalculateProb(N1,N2,p,e,simulation,sample)))
    if (mean_Pr>0.95){
      cat(sample,' ',mean_Pr)
      return (sample)
    }
  }
}

Q3(p,e,N1,N2,simulation)

sample.distribution = replicate(n = 10, expr = Q3(p,e,N1,N2,simulation))
hist(sample.distribution)
mean(sample.distribution)
sample.distribution
quantile(sample.distribution,probs = c(0.025, 0.975))
