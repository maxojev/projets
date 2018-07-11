

#Fonctions auxiliaires=========================================================

#fonction qui crÃ©e la matrice apropriÃ©e Ã  l'Ã©tude avec la cible dans la colonne k+1
crea_mat= function(serie,horizon,k){
	
	matrix_tempon= matrix(serie[1:k],ncol=k, byrow=T)

	for(i in 2:(length(serie)-k-horizon+1)){

		matrix_tempon= rbind(matrix_tempon,serie[i:(i+k-1)])
	}
	
	v=serie[k+horizon]
	
	for(j in (horizon+k+1):(length(serie))){

		v= cbind(v,serie[j])
	}

	y= v[1,]
    serie_mat= cbind(matrix_tempon,y)
	rownames(serie_mat)= rep("",nrow(serie_mat))
	
	return(data.frame(serie_mat))
}

#Fonction permettant de calculer l'erreur quadratique moyenne

eqm = function(pred, verite){
		return(mean((verite-pred)^2))
}
#==============================================================================

reg_lineaire3.0= function(serie,horizon,k){
	
	matrice= crea_mat(serie,horizon,k)
	
	f= floor(runif(nrow(matrice),1,11))

	eqm0= 0
	
	for(i in 1:10){
	
		validation= matrice[(which(f==i)),]
		apprentissage= matrice[(which(f!=i)),]
	
		modele= lm(y~., data = apprentissage)
		prediction = predict(modele, validation[,1:k])
		eqm1= eqm(prediction,validation[,k+1])
		eqm0= c(eqm0,eqm1)
	}

	eqm_moy= mean(eqm0[2:length(eqm0)])
	variance= var(eqm0[2:length(eqm0)])
	
	return (list(eqm_moy,variance))
}

	

