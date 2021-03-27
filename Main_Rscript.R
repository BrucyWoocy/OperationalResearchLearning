# Clear your environment of variables
rm(list = ls())


#NPP intensity
lambdaf = function(t){
  
  return(3 + (4/(t+1)))
  
}


#define DES function
time_next_arrival = function(s, lambda){
  t = s
  flag = 1
  
  while(flag){
    U1 = runif(1)
    t = t - (1/lambda) * log(U1)
    
    U2 = runif(1)
    
    if (U2 <= lambdaf(t)/lambda){
      T_s = t
      flag = 0
    }
  }
  
  return(T_s)
}



T = 4
lambda = 7
mu = 5
set.seed(1)


# Initialize

t= 0 # time
Na = 0 # number of arrivals at expert by time t
ND1 = 0 # number of departures from whisky expert by time t
ND2 = 0

# State variables # state of the system (number of customers in system) at time t
n1 = 0# ST will be a matrix that stores our (n,t) state-time pairs that we record 
n2 =0  # every time there is an event: arrival or departure.

ST = matrix(c(n1,n2,t) , nrow = 1, ncol = 3)

# Event List variables
   # use the nonhomogeneous Poisson to generate t_A = time of next arrival 

t_A =  time_next_arrival(t,lambda) 
t_D1 = Inf # time of next departure from expert ; set to Inf since server is idle
          
t_D2 = Inf #time of next departure from cashier

event_list =  matrix(c(t_A, t_D1, t_D2), nrow=1, ncol=3)

# Initialize output variables
Ae = c() # A(i) =  time of arrival of ith customer at expert
D1 = c() # D(i) =  time of departure of ith customer from expert
D2 = c() # time of departure of ith customer from cashier
T_p = 0 # time past T that the last customer departs

# Main loop
flag = 1

while(flag){
  
  if (t_A == min(t_A,t_D1,t_D2,T)){ # Case 1:  arrival at expert and within T
    
    t = t_A # move to time t_A
    Na = Na + 1 # count the arrival
    n1=n1+1
    t_A = time_next_arrival(t, lambda) # generate time of next arrival
    
    if (n1 == 1){ # system had been empty so the arrival goes to the server
  
      Y1 = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D1 = t + Y1 # set time of next departure from expert
 
    }
    Ae = c(Ae,t) # collect output data: A(N_A) = t
    event_list = rbind(event_list, c(t_A, t_D1, t_D2))
    
    
  } else if (t_D1==min(t_A,t_D1,t_D2,T)){ # Case 2: departure from expert and within T
  
    t = t_D1 # move to time t_D
    ND1 = ND1 + 1 # count the departure
    n1=n1-1
    n2=n2+1
    
    if (n1==0){
      t_D1=Inf
    }
    
      else {
        Y1=-(1/mu) * log(runif(1))
        t_D1=t+Y1
    }
    
    if (n2 == 1){ # no customers at the cashier
      Y2 = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D2 = t + Y2 # set time of next departure from cashier
    }
    
    D1 = c(D1,t) # collect output data: D(N_D) = t
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A, t_D1, t_D2))
    
  } else if (t_D2<=min(t_D1,t_D2,t_A,T)) {#case 3: departure from cashier and within T
    t=t_D2
    ND2 = ND2 +1 #count departure from cashier
    n2 = n2-1
    
    if (n2 == 0){ # no customers at the cashier
      t_D2 = Inf 
      
    } else { # new customer at the cashier
      Y2 = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D2 = t + Y2 # set time of next departure
    }
    
    D2 = c(D2,t)
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A, t_D1, t_D2))
    
  }  else if ((min(t_A,t_D1, t_D2) > T) & (n1 > 0)){
    # Case 4: time ended, customers remain at expert, go to next queue    
    
    t = t_D1 # ignore arrival t_A since > T but still need to deal with 
            # customers in the system so we go to the next departure
    ND1 = ND1 + 1 # count departure
    n1=n1-1
    n2 = n2+1
    
    if (n1 > 0) { 
      # if we still have customers, they go to the server, 
      # if no customers then it is taken care of in the next iter under Case 4 
      Y1 = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D1 = t + Y1 # set time of next departure
    }
    D1 = c(D1,t) # collect output data: D(N_D) = t
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_D1,t_D2))
    
    
  } else if ((min(t_A,t_D1, t_D2) > T) & (n1==0) & (n2>0)){ 
    # Case 5: time ended, customers emptied from expert, customers remain at 
    #cashier, go to next departure
    t = t_D2
    ND2=ND2 +1
    n2=n2-1
    
    if (n2>0) {
    Y2 = -(1/mu) * log(runif(1))
    t_D2 = t+Y2
    }
    
    D2 = c(D2,t)
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_D1,t_D2))
    
  } else if ((min(t_A,t_D1, t_D2) > T) & (n1==0) & (n2==0)){ 
    # Case 6: time ended, customers gone, the end
    flag = 0
    T_p =  max(t-T,0)
    
  } else {
    print(paste("I should not be here"))
  }
}

mean(D2-Ae)

