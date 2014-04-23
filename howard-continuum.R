#
# Howard's improvement algorithm to get the MPE of the neighborhood
#

printf <- function(...) invisible(print(sprintf(...)));

# discretized strategy: 
# an array with values of the strategy (0 or 1) 

# function that gives the value of the broker strategy at any x given the 
# discretized strategy of the broker

continuous.valuation <- function(discretized.valuation, x, K) {
	
	f <- approxfun((0:K)/K, discretized.valuation);
	
	return(f(x));
	
}

iterate.contraction <- function(model, profit.function, white.valuation.function, black.valuation.function, K)
{
	
	######
	# profit function: is a vector of size K, that gives the value of the profit for each x1, ..., xK 
	# on the grid x_k = k/K
	
	# white valuation function: vector of size K, gives relaxed Whites' valuation for each x1, ..., xK
	# black valuation function: vector of size K, gives relaxed Blacks' valuation for each x1, ..., xK
	
	# function returns a list with the updated valuations but also the strategies of each player:
	# xi(x) for each x on the grid
	# sigma^r(x,s,mu) for each x on the grid, for each s in R,D, for each mu in w,b
	
	######
	# STEP 1: find households' strategies for each x,s,mu
	
	### define prices p^r(x,s,mu) for each x, s, mu
	
	price.distressed.white.to.white <- 		white.valuation.function - model$vlw     - 0.5*model$epsilon ;
	price.distressed.white.to.black <- 0.5*(white.valuation.function - model$epsilon - model$vlw + black.valuation.function - model$vlb);
	price.relaxed.white.to.black 	<- 0.5*(white.valuation.function			     - model$vlw + black.valuation.function - model$vlb);
	
	price.distressed.black.to.black <- 0.5*(black.valuation.function - model$epsilon - model$vlb + black.valuation.function - model$vlb);
	price.relaxed.black.to.white    <- 0.5*(black.valuation.function                 - model$vlb + white.valuation.function - model$vlw);
	price.distressed.black.to.white <- 0.5*(black.valuation.function - model$epsilon - model$vlb + white.valuation.function - model$vlw);
	
	### find household strategies
	
	# interracial sales
	
	strat.distressed.white.to.black <- (model$vlw + (1-model$alpha)* price.distressed.white.to.black ) >
										 (white.valuation.function - model$epsilon);
	
	strat.relaxed.white.to.black    <- (model$vlw + (1-model$alpha)* price.relaxed.white.to.black ) >
										 (white.valuation.function );

	# interracial sales
	
	strat.distressed.black.to.white <- (model$vlb + (1-model$alpha)* price.distressed.black.to.white ) >
										 (black.valuation.function - model$epsilon);
										
	strat.relaxed.black.to.white <- (model$vlb + (1-model$alpha)* price.relaxed.black.to.white ) >
										 (black.valuation.function );
	
	x.next.period 		<- array(0,K+1);
	x.next.period.xi	<- array(0,c(K+1,K+1));
	xi			  		<- array(0,K+1);
	one.period.profit 	<- array(0,K+1);
	one.period.profit.try <- array(0,c(K+1,K+1));
	
	xi.try 		<- array(0,c(K+1,K+1));
	profit.try 	<- array(0,c(K+1,K+1));
	
	######
	# STEP 2: find the broker's optimal strategy at each x
	
	# for each x on the grid....
	
	for (k in 1:(K+1)) {
	
		x <- (k-1)/K;
		
		one.period.profit.black <- 2*model$alpha * model$delta * ( x * model$lambda * price.distressed.black.to.black[k] +
											(1-x)*(		model$lambda*strat.distressed.white.to.black[k]	* price.distressed.white.to.black[k] + 
													(1-model$lambda)*strat.relaxed.white.to.black[k]	* price.relaxed.white.to.black[k]		) );
		one.period.profit.white <- 2*model$alpha * model$delta *
										((1-x) * model$lambda*price.distressed.white.to.white[k] +
											x  * (	    model$lambda * strat.distressed.black.to.white[k] * price.distressed.black.to.white[k] + 
													(1-model$lambda) * strat.relaxed.black.to.white[k]    * price.relaxed.black.to.white[k]    ));
		
		# for each xi on a grid of size K on [0,1]...
		
		best.l 		<- 1;
		best.profit <- -Inf;
		
		for (l in 1:(K+1)) {
		
			xi.try[k,l] <- (l-1)/K;
		
			# find the one period profit
			
			one.period.profit.try[k,l] <- one.period.profit.black * xi.try[k,l] + one.period.profit.white * (1-xi.try[k,l]);
		
			# find the next period fraction black
		
			x.next.period.xi[k,l] <- x + 	model$delta * (1-x) * xi.try[k,l] * (model$lambda*strat.distressed.white.to.black[k] + (1-model$lambda)*strat.relaxed.white.to.black[k]) -
									model$delta * x * (1-xi.try[k,l]) * (model$lambda*strat.distressed.black.to.white[k] + (1-model$lambda)*strat.relaxed.black.to.white[k]);
		
			# find the profit at x, with xi_k as selected
			
			profit.try[k,l] <- one.period.profit.try[k,l] + (1/(1+model$r)) * continuous.valuation(profit.function, x.next.period.xi[k,l], K);
		
			# if the xi yields a higher profit than the highest profit found so far, update
			
			if (profit.try[k,l] > best.profit) {
				
				best.l 					<- l;
				best.profit 			<- profit.try[k,l];
				best.x.next.period 		<- x.next.period.xi[k,l];
				best.one.period.profit 	<- one.period.profit.try[k,l];
				
			}
			
		}
			
		# set the optimal xi for that x 
		
		xi[k] 				<- (best.l-1)/K;
		x.next.period[k]	<- best.x.next.period;
	
		# set the value of the optimal profit at x
		
		profit.function[k] 	<- best.profit;
		one.period.profit[k] <- best.one.period.profit;
		
	}
		
	#####
	# STEP 3: update the value functions
	
	x <- (0:K)/K;
	
	expected.white <- model$delta*xi*(	model$lambda * (	 strat.distressed.white.to.black * (model$vlw + (1-model$alpha)*price.distressed.white.to.black ) + 
													     (1-strat.distressed.white.to.black) * (white.valuation.function - model$epsilon) ) + 
									(1-model$lambda) * (	strat.relaxed.white.to.black	 * (model$vlw + (1-model$alpha)*price.relaxed.white.to.black ) +
														 (1-strat.relaxed.white.to.black)	 * white.valuation.function 				) ) +
					  model$delta * (1-xi) *    model$lambda  * (model$vlw + (1-model$alpha) * price.distressed.white.to.white) +
					  model$delta * (1-xi) * (1-model$lambda) * (white.valuation.function) +
					  (1-model$delta*xi) * (   white.valuation.function - model$lambda*model$epsilon     );
	
	
	expected.black  <- model$delta*(1-xi)*(		model$lambda * (	strat.distressed.black.to.white		*	(model$vlb + (1-model$alpha)*price.distressed.black.to.white ) 
																+ (1-strat.distressed.black.to.white)	* 	(black.valuation.function - model$epsilon) ) + 
											(1-model$lambda) * (	strat.relaxed.black.to.white		*	(model$vlb + (1-model$alpha)*price.relaxed.black.to.white ) 
																+ (1-strat.relaxed.black.to.white)		*	(black.valuation.function) ) ) +
					   model$delta*xi*model$lambda	 * (model$vlb + (1-model$alpha) * price.distressed.black.to.black) +
					   model$delta*xi*(1-model$lambda) * (black.valuation.function) +
					  (1-model$delta*(1-xi)) * (black.valuation.function-model$lambda*model$epsilon);
									  
	for (k in 1:(K+1)) { 
					
		#printf("x.next.period[%f] : %f ", k, x.next.period[k]);
									 
		white.valuation.function[k] <- model$vw - model$rho.white * x[k] + model$beta * continuous.valuation(expected.white, x.next.period[k], K);
		black.valuation.function[k] <- model$vb - model$rho.black * x[k] + model$beta * continuous.valuation(expected.black, x.next.period[k], K);
	
	}
	
	updated.values.strategies <- list( 	x						 = (0:K)/K,
										white.valuation.function = white.valuation.function,
										black.valuation.function = black.valuation.function,
										profit.function			 = profit.function,
										strat.distressed.white.to.black = strat.distressed.white.to.black,
										strat.relaxed.white.to.black    = strat.relaxed.white.to.black,
										strat.distressed.black.to.white = strat.distressed.black.to.white,
										strat.relaxed.black.to.white	= strat.relaxed.black.to.white,
										xi								= xi,
										# for debugging:
										x.next.period 					= x.next.period,
										x.next.period.xi				= x.next.period.xi,
										expected.white					= expected.white,
										expected.black					= expected.black,
										one.period.profit				= one.period.profit,
										one.period.profit.try			= one.period.profit.try,
										xi.try							= xi.try,
										profit.try						= profit.try										
										);
	
	return(updated.values.strategies);
	
}

# Solve for the Markov Perfect Equilibrium of the neighborhood

solve.mpe <- function(model,profit.function, white.valuation.function, black.valuation.function, K, iterations = 10) {
	
	for (n in 1:iterations) {
		
		printf("Iteration number %f - contraction mapping", n);
		
		updated.values.strategies <- iterate.contraction(model,profit.function, white.valuation.function, black.valuation.function, K);
		
		profit.function 		 <- updated.values.strategies$profit.function;
		white.valuation.function <- updated.values.strategies$white.valuation.function;
		black.valuation.function <- updated.values.strategies$black.valuation.function;
		
	}
	
	return(updated.values.strategies);
	
}

# define the parameters of the model

model <- list(
	beta 	= 0.065, # households' discount factor
	lambda 	= 0.3, # households' probability of the distressed shock
	
	vw 			= 30000*(1-0.065), # Whites' one period utility of the neighborhood
	vlw 		= 8000,				  # Whites' value of the outside option
	rho.white 	= 9000*(1-0.065), # Whites' preferences for same race neighbors
	
	vb 			= 22000*(1-0.065),				  # Blacks' pdv of utility of the neighborhood
	vlb 		= 200,				  # Blacks' pdv of the outside option
	rho.black 	= -500, 			  # Blacks' distaste (>0) for same race neighbors
	
	epsilon = 1000,			# Shock in distressed state
	
	r 		= 0.05,				# Broker's interest rate
	delta 	= 0.068,			# Rate of arrival of offers
	alpha  	= 0.06			# Commission rate
	);

# define initial valuations vectors

K <- 400; # number of xs on the grid of [0,1]

# Initial conditions

initial.white.valuation.function	<- (model$vw)*(1+1/model$beta)                 * array(1,K+1)
initial.black.valuation.function	<- (model$vb)*(1+1/model$beta)                 * array(1,K+1)						
initial.broker.profit				<- 2*model$alpha*model$delta*model$lambda*0.5*((initial.white.valuation.function - model$vlw)+(initial.white.valuation.function - model$vlw)) * array(1,K+1);

# find the unique Markov Perfect Equilibrium

valuations.solved <- solve.mpe(model,initial.broker.profit,initial.white.valuation.function,initial.black.valuation.function,K,10)




