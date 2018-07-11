crea_mat_regPo= function(serie,horizon,k,p){
	
	matrix_tempon= matrix(serie[1:k],ncol=k, byrow=T)

	for(i in 2:(length(serie)-k-horizon+1)){

		matrix_tempon= rbind(matrix_tempon,serie[i:(i+k-1)])
	}
		m= matrix_tempon
	for(i in 1:length(p)){
		if (p[i]==1){
			m=matrix_tempon		
		} else{
			
			m=cbind(m,matrix_tempon^p[i])
		
			}
	}

	v= serie[k+horizon]
	
	for(j in (horizon+k+1):(length(serie))){

		v= cbind(v,serie[j])
	}
	
	y= v[1,]

   	serie_m= cbind(m,y)
	rownames(serie_m)= rep("",nrow(serie_m))
	
	return(data.frame(serie_m))
	}
