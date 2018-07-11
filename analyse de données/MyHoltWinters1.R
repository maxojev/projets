#Fonction auxiliaire

eqm = function(pred, verite){
  return(mean((verite-pred)^2))
}


Myalpbetgam1 = function(serie, horizon, periode){

	alp = seq(0.1,1,length.out = 10)
	bet = seq(0.1,1,length.out = 10)
	gam = seq(0.1,1,length.out = 10)

	eqmtest= 10000000000000000000000000000000000
	pred= serie	
	
	if(horizon != 1){
		
		for(a in 1:length(alp)){
			for(b in 1:length(bet)){
				for(g in 1:length(gam)){
					
					let= HoltWinters(ts(serie, frequency=periode), alpha = alp[a], beta = bet[b], gamma = gam[g])
					mod=let$fitted

					for (i in horizon:nrow(mod)){
							
						pred[periode+i]= mod[i-horizon+1,2] + mod[i-horizon+1,3] * horizon + mod[i,4]
						
					}		
					
					eqmt= eqm(pred,serie)
						
					if(eqmt<= eqmtest){
						
						
						eqmtest= eqmt
						alpha = alp[a]
						beta = bet[b]
						gamma = gam[g]
					}
	
				}
			}
		}
#	plot(serie, type="l", col="blue")
#	lines(pred, type="l", col="red")
	return(c(alpha,beta,gamma))
	}
	else{
	
		let= HoltWinters(ts(serie, frequency=periode))
		alpha= let$alpha
		beta= let$beta
		gamma= let$gamma
		
	return(c(alpha,beta,gamma))
	}

}	

MyHoltWinters1 = function(serie,periode, d){

	app= serie[1:(length(serie)-d)]
	test= serie[(length(serie)-d +1):length(serie)]
	prediction= test
	
	for (i in 1:d){
		
		alpbetgam= Myalpbetgam1(app, i, periode)
		let= HoltWinters(ts(app, frequency=periode), alpha = alpbetgam[1], beta = alpbetgam[2], gamma = alpbetgam[3])
		prediction[i]= predict(let,n.ahead=i)[i]	
	}

	eqm_v= eqm(prediction,test)

	plot(serie, type="l")
	lines(c((length(serie)- length(test) +1):length(serie)),prediction,col="blue")
	
	return(eqm_v)
}











	