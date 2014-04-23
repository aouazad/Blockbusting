#
# Blockbusting R code
#
#

# PROBLEMS:
#   Find set of parameters for which selling to black is the strategy of distressed Whites at t=0
#   Staircase effect

# COMPARATIVE STATICS :

# - racial preferences rho								: profit of steady state white vs profit of blockbusting and existence of blockbusting equilibrium
# - broker discount factor								: same comparisons and existence of steady state white equilibrium
# - arrival rate of offers and blockbusting profits		: same comparisons
# - black households' option and blockbusting profits	: same comparisons

#  !!!  THEORY : Multiple brokers: one deviation property?

# CALCULUS : Gentrification equilibrium: start with all black neighborhood and go from there

# THEORY : Welfare analysis: welfare of whites compared to initial welfare and welfare gain for black buyers
			# Problem, negative surplus for black and white entrants...

# THEORY : Endogenous alpha: ? change alpha and check comparison of profits (not linear because of prices)
	# also dynamic alpha(t)

source("blockbusting_functions.R", echo=TRUE);

T <- 300; # Number of time periods (for calculation only, model has infinite number of time periods)

model <- list(
	beta 	= 0.065, # households' discount factor
	lambda 	= 0.3, # households' probability of the distressed shock
	
	vw 			= 25000*(1-0.065), # Whites' one period utility of the neighborhood
	vlw 		= 8000,				  # Whites' value of the outside option
	rho.white 	= 25000*(1-0.065), # Whites' preferences for same race neighbors
	
	vb 			= 15000*(1-0.065),				  # Blacks' pdv of utility of the neighborhood
	vlb 		= 200,				  # Blacks' pdv of the outside option
	rho.black 	= 0, 			  # Blacks' taste (>0) /distaste (<0) for same race neighbors
	
	epsilon = 200,			# Shock in distressed state
	
	r 		= 0.05,				# Broker's interest rate
	delta 	= 0.07,			# Rate of arrival of offers
	alpha  	= 0.06			# Commission rate
	);
	
	
# SIMPLE BLOCKBUSTING DYNAMICS

#solution.blockbusting <- solve.blockbusting.equilibrium(model,T);
#plot.blockbusting.solution(model,solution.blockbusting,T,"basic_");
#plot(	solution.blockbusting$time, solution.blockbusting$frac.blacks);
#plot(	solution.blockbusting$time, solution.blockbusting$turnover);
#plot(	solution.blockbusting$time, solution.blockbusting$commission.fees);
#plot(	solution.blockbusting$frac.blacks,	solution.blockbusting$relaxed.white.valuation		- model$vlw);
#lines(	solution.blockbusting$frac.blacks,	solution.blockbusting$distressed.white.valuation	- model$vlw);
#lines(	solution.blockbusting$frac.blacks,	solution.blockbusting$relaxed.black.valuation		- model$vlb);
#lines(	solution.blockbusting$frac.blacks,	solution.blockbusting$distressed.black.valuation	- model$vlb);

##
# FOR EACH COMPARATIVE STATICS:
#   1. Existence of blockbusting equilibrium
#   2. Existence of steady-state white equilibrium
#   3. Profit of blockbusting equilibrium is higher than profit of steady-state white?
#   4. For two values of the parameter, describe revenue at time t
#	5. Tipping point as a function of the parameter


##### COMPARATIVE STATICS FOR COMMISSION FEES ALPHA

data.for.plot.alpha <- perform.cs.equilibrium(model,"alpha",0.01,0.99,T);

setEPS();
postscript("05_alpha/profit_cs.eps");
plot.two.series(	data.for.plot.alpha$alpha,  data.for.plot.alpha$broker.pdv.profit, data.for.plot.alpha$threshold.profit.equilibrium);
dev.off();

plot.two.series(	data.for.plot.alpha$alpha,  data.for.plot.alpha$broker.pdv.profit, data.for.plot.alpha$threshold.profit.equilibrium);

setEPS();
postscript("05_alpha/profit_compared_ssw.eps");
plot.two.series(	data.for.plot.alpha$alpha,  data.for.plot.alpha$broker.pdv.profit, data.for.plot.alpha$steady.state.white.profit);
dev.off();

setEPS();
postscript("05_alpha/tipping_point.eps");
plot(	data.for.plot.alpha$alpha,  data.for.plot.alpha$tipping.frac.blacks, type = "l");
dev.off();

plot(	data.for.plot.alpha$alpha,  data.for.plot.alpha$tipping.frac.blacks, type = "l");

#setEPS();
#postscript("05_alpha/revenue_time_t.eps");
#compare.two.values(model,"alpha",0.05,0.09,T,100);
#dev.off();

#compare.two.values(model,"alpha",0.05,0.09,T,100);

#plot.two.series(data.for.plot.alpha$alpha, data.for.plot.alpha$threshold.white.valuation, data.for.plot.alpha$initial.white.valuation )

#stop();

##### COMPARATIVE STATICS FOR BLACKS' RACIAL PREFERENCES

data.for.plot.black.cs <- perform.cs.equilibrium(model,"rho.black",0,10000*(1-0.065),T);

setEPS();
postscript("02_blacks_racial_preferences/profit_cs.eps");
plot(data.for.plot.black.cs$rho.black,  data.for.plot.black.cs$broker.pdv.profit, type="l");
lines(data.for.plot.black.cs$rho.black, data.for.plot.black.cs$steady.state.white.profit);
lines(data.for.plot.black.cs$rho.black, data.for.plot.black.cs$threshold.profit.equilibrium);
dev.off();

setEPS();
postscript("02_blacks_racial_preferences/tipping_point.eps");
plot(data.for.plot.black.cs$rho.black,  data.for.plot.black.cs$tipping.frac.blacks);
dev.off();

setEPS();
postscript("02_blacks_racial_preferences/revenue_time_t.eps");
data.two.values.rho.black <- compare.two.values(model,"rho.black",0,10000*(1-0.065),T,70);
dev.off();

stop();

##### COMPARATIVE STATICS FOR RATE OF ARRIVAL OF OFFERS

data.for.plot.delta.cs <- perform.cs.equilibrium(model,"delta",0.05,0.07,T);

setEPS();
postscript("03_delta/profit_cs.eps");
plot.two.series( data.for.plot.delta.cs$delta, data.for.plot.delta.cs$broker.pdv.profit, data.for.plot.delta.cs$threshold.profit.equilibrium);
dev.off();

setEPS();
postscript("03_delta/tipping_point.eps");
plot( data.for.plot.delta.cs$delta,  data.for.plot.delta.cs$tipping.frac.blacks);
dev.off();

setEPS();
postscript("03_delta/revenue_time_t.eps");
compare.two.values(model,"delta",0.05,0.07,T,70);
dev.off();

stop();

##### COMPARATIVE STATICS FOR BROKER DISCOUNT FACTOR

data.for.plot.r <- perform.cs.equilibrium(model,"r",0.05,0.07,T);

setEPS();
postscript("04_r/profit_cs.eps");
plot.two.series(	data.for.plot.r$r,  data.for.plot.r$broker.pdv.profit, data.for.plot.r$threshold.profit.equilibrium);
#lines(	data.for.plot.r$r, 	data.for.plot.r$steady.state.white.profit);
dev.off();

setEPS();
postscript("04_r/tipping_point.eps");
plot(	data.for.plot.r$r,  data.for.plot.r$tipping.frac.blacks, type = "l");
dev.off();

setEPS();
postscript("04_r/revenue_time_t.eps");
compare.two.values(model,"r",0.05,0.08,T,100);
dev.off();

stop();

##### COMPARATIVE STATICS FOR WHITES' RACIAL PREFERENCES

data.for.plot <- perform.cs.equilibrium(model,"rho.white",2600,90000,T);

setEPS();
postscript("01_whites_racial_preferences/profit_cs.eps");

y.min <- min(unlist(data.for.plot$broker.pdv.profit),unlist(data.for.plot$threshold.profit.equilibrium));
y.max <- max(unlist(data.for.plot$broker.pdv.profit),unlist(data.for.plot$threshold.profit.equilibrium));

plot( data.for.plot$rho.white, data.for.plot$broker.pdv.profit, type = "l", yaxt="n", xaxt="n", xlab=expression(rho[w]), ylab=expression(Pi), ylim=c(y.min, y.max));
#lines(data.for.plot$rho.white, data.for.plot$steady.state.white.profit);
lines(data.for.plot$rho.white, data.for.plot$threshold.profit.equilibrium);

# find first value of rho.white such that profit is greater than ss white profit
# and  first value of rho.white such that profit is greater than the one-deviation property

#higher.profit 					<- which(unlist(data.for.plot$broker.pdv.profit) - unlist(data.for.plot$steady.state.white.profit) > 0);
#lowest.rho.with.highest.profit 	<- max(higher.profit);
#highest.rho.with.highest.profit <- min(higher.profit);

equilibrium.exists				<- which(unlist(data.for.plot$broker.pdv.profit) - unlist(data.for.plot$threshold.profit.equilibrium) > 0);
lowest.rho.with.existence 		<- max(equilibrium.exists);
highest.rho.with.existence		<- min(equilibrium.exists);

axis(1, at=c( data.for.plot$rho.white[lowest.rho.with.existence ],		data.for.plot$rho.white[highest.rho.with.existence]      ),	labels=expression(rho[min],rho[max])	);
#axis(1, at=c( data.for.plot$rho.white[lowest.rho.with.highest.profit],	data.for.plot$rho.white[highest.rho.with.highest.profit] ),	labels=expression(rho[min],rho[max])	);
dev.off();

setEPS();
postscript("01_whites_racial_preferences/tipping_point.eps");
plot(data.for.plot$rho.white, data.for.plot$tipping.frac.blacks, type = "l", xlab = expression(rho), ylab = expression(hat(t)), ylim=c(0,1), xaxt="n", yaxt="n" );
dev.off();

setEPS();
postscript("01_whites_racial_preferences/revenue_time_t.eps");
compare.two.values(model, "rho.white", 10000*(1-0.065), 32000, T, 100  );
dev.off();




##### COMPARATIVE STATICS BLACK OUTSIDE OPTION

# HERE YOU CAN ALSO PUT EXISTENCE OF STEADY STATE WHITE EQUILIBRIUM

#model$rho.white <- 30000;

#data.for.plot.vlb <- perform.cs.equilibrium(model,"vlb",90,5000,T);

#setEPS();
#postscript("06_vlb/profit_cs.eps");
#plot(	data.for.plot.vlb$vlb, 	data.for.plot.vlb$broker.pdv.profit, type="l");
#lines(	data.for.plot.vlb$vlb, 	data.for.plot.vlb$steady.state.white.profit);
#lines(	data.for.plot.vlb$vlb, 	data.for.plot.vlb$threshold.profit.equilibrium);
#dev.off();

#setEPS();
#postscript("06_vlb/household_equilibrium_condition.eps");
#plot(	data.for.plot.vlb$vlb,data.for.plot$threshold.white.valuation,type="l");
#lines(	data.for.plot.vlb$vlb,data.for.plot$initial.white.valuation);
#dev.off();

#setEPS();
#postscript("06_vlb/tipping_point.eps");
#plot(	data.for.plot.vlb$vlb, 		data.for.plot.vlb$tipping.frac.blacks, type="l");
#dev.off();

#setEPS();
#postscript("06_vlb/revenue_time_t.eps");
#compare.two.values(model,"vlb",5000,7000,T);
#dev.off();

##### WELFARE ANALYSIS

# 1. On racial preferences

#setEPS();
#postscript("01_whites_racial_preferences/welfare.eps");
#welfare.gain <- perform.cs.welfare(model,"rho.white",10000*(1-0.065), 18800, 100);
#plot(   welfare.gain$rho.white, welfare.gain$gain.incumbents, type="l");
#lines(  welfare.gain$rho.white, welfare.gain$gain.entrants );
#dev.off();

# 2. 

# 3. 

##### GENTRIFICATION EQUILIBRIUM

# 1. Revenue at each point in time t
# 2. Existence of gentrification equilibrium for different values of rho_w, together with existence of blockbusting equilibrium
# 3. Existence of steady-state black equilibrium and existence of steady-state white equilibrium


##### DYNAMIC ALPHA(t)
