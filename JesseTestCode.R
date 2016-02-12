
##################################  
###   
### Trying to use rand.code.org 
###
#################################

#Based on http://www.r-bloggers.com/rstudio-and-github/

##own stuff

git config --global user.email "lastunen@rand.org"
git config --global user.name "lastunen""

git remote add origin https://github.com/lastunen/MobileJesse.git
git config remote.origin.url git@github.com:lastunen/MobileJesse.git
git pull -u origin master
git push -u origin master

#Works

#You have now pushed your commit to GitHub, 
#and should be able to see your files in your GitHub account. 
#The Pull Push buttons in RStudio will now also work.
#Remember, after each Commit, you have to Push to GitHub, 
#this doesn’t happen automatically.

#Trying to clone Mobile (New Project...)

Cloning into 'mobile'...
fatal: unable to access 'http://code.rand.org/snowak/mobile.git/': Could not resolve host: code.rand.org

Cloning into ',DanaInfo=code.rand.org+mobile'...
fatal: https://smconnect.rand.org/snowak/,DanaInfo=code.rand.org+mobile.git/info/refs not valid: is this a git repository?

#Trying to push files into Mobile

git remote rm origin
git remote add origin https://smconnect.rand.org/snowak/,DanaInfo=code.rand.org+mobile.git #or
git remote add origin http://code.rand.org/snowak/mobile.git
git config remote.origin.url git@code.rand.org:snowak/mobile.git
git remote -v

git pull -u origin master
git push -u origin master

#error

Jesse:TestRepo jesse1$ git push -u origin master
ssh: Could not resolve hostname code.rand.org: nodename nor servname provided, or not known
fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.
Jesse:TestRepo jesse1$ 

#Remote control URLs tried

https://smconnect.rand.org/snowak/,DanaInfo=code.rand.org+mobile.git
https://smconnect.rand.org/owa/,DanaInfo=randmail.rand.org,SSL+redir.aspx?SURL=elMRv16CoPIKqeHuhLdnD41bQtGqVuia040fonInrXwHtHJKFyjTCGgAdAB0AHAAOgAvAC8AYwBvAGQAZQAuAHIAYQBuAGQALgBvAHIAZwAvAHMAbgBvAHcAYQBrAC8AbQBvAGIAaQBsAGUALgBnAGkAdAA.&URL=http%3a%2f%2fcode.rand.org%2fsnowak%2fmobile.git

##################################  
###   
### Utility function, single consumer
###
#################################

## Parameters
# Budget [bounds]
p1 = .6 #[0,+Inf] //price of goods in sector 1
p2 = .7 #[0,+Inf] //in sector 2
p3 = .2 #[0,+Inf] //in sector 3
p4 = .2 #[0,+Inf] //in sector 4
S = .9  #[0,1] //prob of employment = S * e (training expenditure) ^ eta
eta = .5 #[0,1] //prob of employment = S * e (training expenditure) ^ eta
tau = 0.1 #[0,1] //taxes when working
uben = 0.1 #[0,+Inf] //unemployment benefits 
wage = 15 #[0,+Inf] 

# Utility
gamma = .5
mu = .5

## Optimize with SOLNP PACKAGE, with four sectors for consumption
utility_function <- function(cvars) {
  labor <- cvars[1]
  e <- cvars[2] ##Expenditure on training
  c1 <- cvars[3]
  c2 <- cvars[4]
  c3 <- cvars[5]
  c4 <- cvars[6]
  gamma = gamma
  mu = mu
  U <- -((c1^gamma+c2^gamma+c3^gamma+c4^gamma)^(mu/gamma)*(1-labor-e)^(1-mu))
  return(U)
} 

ivals <- c(.3,.3,.3,.3,.3,.3)

## ”Inequality function” sets the budget constraint
ineqfun <- function(cvars){
  labor <- cvars[1]
  e <- cvars[2]
  c1 <- cvars[3]
  c2 <- cvars[4]
  c3 <- cvars[5]
  c4 <- cvars[6]
  p1=p1
  p2=p2
  p3=p3
  p4=p4
  tau=tau
  wage=wage
  S=S
  eta=eta
  prob <- S*(e^eta)
  uben=uben
  ineqlimit = ( ( (1 - tau) * prob * wage*labor + (1 - prob)*uben ) ) - ( p1*c1 + p2*c2 + p3*c3 + p4*c4 + e)
  return(c(ineqlimit))
}

ineqUB <- c(10000) ##mandatory upper limit
ineqLB <- c(0) ##expected income minus expenditures is at least 0

# Bounds for choice variables
UB = c(1000,1000,1000,1000,1000,1000)
LB = c(0,0,0,0,0,0)

# Evaluate function
solnp_utility = solnp(pars=ivals,fun=utility_function,ineqfun=ineqfun,ineqfun,ineqUB=ineqUB,ineqLB=ineqLB,LB=LB,UB=UB)

## Evaluate results
evaluate_results_utility <- function(results) {
  cvars <- results[[1]]
  labor <- cvars[1]
  e  <- cvars[2]
  c1 <- cvars[3]
  c2 <- cvars[4]
  c3 <- cvars[5]
  c4 <- cvars[6]
  prob <- S*(e^eta)
  utility <- (c1^gamma + c2^gamma + c3^gamma + c4^gamma)^(mu/gamma)*(1 - labor - e)^(1 - mu)
  expected_income <- (1 - tau)*wage*labor*prob + (1 - prob)*uben
  total_consumption <- c1*p1 + c2*p2 + c3*p3 + c4*p4 + e
  print(paste0("c1: ",round(c1,3)," --- c2: ",round(c2,3)," --- c3: ",round(c3,3)," --- c4: ",round(c4,3)))
  print(paste0("Total consumption: ",round(total_consumption,3), " --- Utility: ",round(utility,3)))
  print(paste0("Prob of empl.: ",round(prob,3)," --- Expected income: ",round(expected_income,3)))
  print(paste0("Labor: ",round(labor,3), " --- Training: ",round(e,3)," --- Leisure: ",round(1-labor-e,3)))
}

evaluate_results_utility(solnp_utility)
















##################################  
###   
### Production side: 4 education/skill levels, domestic and foreign labor, 
### unlimited domestic and foreign labor
###
###############################

## Parameters
price = .25 #[0,+Inf]
A = .25 #[0,+Inf]
theta1 = 2 #[0,1]
theta2 = 1 #[0,1]
theta3 = 0.8 #[0,1]
theta4 = 0.2 #[0,1]
rho = .4 #[0,1]
wage1 = 1  #[0,+Inf]
wage2 = 0.6  #[0,+Inf]
wage3 = 0.4  #[0,+Inf]
wage4 = 0.2  #[0,+Inf]
m1 = 1.1  #[0,+Inf]
m2 = 0.6 #[0,+Inf]
m3 = 0.5  #[0,+Inf]
m4 = 0.4  #[0,+Inf]

## Optimize with SOLNP PACKAGE
profit_function <- function(cvars) {
  L1 <- cvars[1]
  L2 <- cvars[2]
  L3 <- cvars[3]
  L4 <- cvars[4]
  I1 <- cvars[5]
  I2 <- cvars[6]
  I3 <- cvars[7]
  I4 <- cvars[8]
  profit <- -(price*A*((theta1*L1+I1)^rho+(theta2*L2+I2)^rho+(theta3*L3+I3)^rho+(theta4*L4+I4)^rho)^(1/rho)-
                wage1*L1-wage2*L2-wage3*L3-wage4*L4-m1*I1-m2*I2-m3*I3-m4*I4)
  return(profit)
} 

ivals <- c(.3,.3,.3,.3,.3,.3,.3,.3)

## ”Inequality function” sets the constraint (consumption-costs at least 0)
ineqfun <- function(cvars){
  L1 <- cvars[1]
  L2 <- cvars[2]
  L3 <- cvars[3]
  L4 <- cvars[4]
  I1 <- cvars[5]
  I2 <- cvars[6]
  I3 <- cvars[7]
  I4 <- cvars[8]
  price=price
  A=A
  theta1=theta1
  theta2=theta2
  theta3=theta3
  theta4=theta4
  wage1=wage1
  wage2=wage2
  wage3=wage3
  wage4=wage4
  m1=m1
  m2=m2
  m3=m3
  m4=m4
  ineqlimit = (price*A*((theta1*L1+I1)^rho+(theta2*L2+I2)^rho+(theta3*L3+I3)^rho+(theta4*L4+I4)^rho)^(1/rho)-
                 wage1*L1-wage2*L2-wage3*L3-wage4*L4-m1*I1-m2*I2-m3*I3-m4*I4)
  return(c(ineqlimit))
}

# Consumption-costs at least 0
ineqUB <- c(1000000)
ineqLB <- c(0)

# Bounds for choice variables
UB = c(10000,10000,10000,10000,10000,10000,10000,10000)
LB = c(0,0,0,0,0,0,0,0)

# Evaluate function
solnp_profit = solnp(pars=ivals,fun=profit_function,ineqfun=ineqfun,ineqfun,ineqUB=ineqUB,ineqLB=ineqLB,LB=LB,UB=UB)

# Evaluate results
evaluate_results_production <- function(results) {
  cvars <- results[[1]]
  L1 <- cvars[1]
  L2 <- cvars[2]
  L3 <- cvars[3]
  L4 <- cvars[4]
  I1 <- cvars[5]
  I2 <- cvars[6]
  I3 <- cvars[7]
  I4 <- cvars[8]
  revenue <- (price*A*((theta1*L1+I1)^rho+(theta2*L2+I2)^rho+(theta3*L3+I3)^rho+(theta4*L4+I4)^rho)^(1/rho))
  costs <- wage1*L1 + wage2*L2 + wage3*L3 + wage4*L4 + m1*I1 + m2*I2 + m3*I3 + m4*I4
  profit <- revenue - costs
  print(paste0("L1: ",signif(L1,3)," --- L2: ",signif(L2,3)," ---L3: ",signif(L3,3)," --- L4: ",signif(L4,3)))
  print(paste0("I1: ",signif(I1,3)," --- I2: ",signif(I2,3)," ---I3: ",signif(I3,3)," --- I4: ",signif(I4,3)))
  print(paste0("Revenue: ",signif(revenue,3), " --- Costs: ",signif(costs,3)," --- Profit: ",signif(profit,3)))
}

evaluate_results_production(solnp_profit)



