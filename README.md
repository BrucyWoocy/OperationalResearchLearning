# OperationalResearchLearning
This is a repository for my side projects in Operational Research.
<br><H3>Problem Description</H3> 
 A whisky shops opens for 4 hrs per day.

There is one whiskey expert who welcomes each customer individually and talks them through the best whiskey blends options according to the customer’s personal taste (until the  customer decides). Then each customer moves to the cashier1 area to pay and leaves the shop. There is only one expert and one cashier. Each arriving customer is firstly being  advised from the expert, then buys/pays in the cashier and leaves the shop, in this order of events. Therefore note that there are two separate queues, one for the expert and one  for the cashier. 

The shop asks for your advice to estimate the average time/number of customers within the shop, as they want to use this data to evaluate and improve its future services.

 (i) More specifically, advise WLR on how to estimate: the average time customers spend in the shop, the average time customers spend queueing/talking to the expert and the  average time customers spend queueing/paying to the cashier. 
<br>
<H4>Assumptions</H4>: customers arrival process is Nonhomogeneous Poisson Process with intensity λ(t) = 3 + 4/(t+1)
             service times are exponential random variables with rate µ = 5/hour

<br>
To do this, I use discrete event simulation coded in R
