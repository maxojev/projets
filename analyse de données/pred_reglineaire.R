#Fonctions auxiliaires=========================================================

#fonction qui crée la matrice apropriée à l'étude avec la cible dans la colonne k+1
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
	
	
	return (eqm_moy)
}

analyse_k_lineaire= function(serie,horizon){

 # k prenant des valeurs entre 3 et 30
  
  eqm_t= 100000000000000000000
  for (i in 3:50 ){
		eqm_test= min (reg_lineaire3.0(serie,horizon,i))
			
			if (eqm_test< eqm_t){
	  
				eqm_t=eqm_test
				k= i
			}
	}
	return (k)
}
#==============================================================================

pred_reglineaire= function(serie,horizon,d){


	apprentissage= serie[1:(length(serie)-d)]
	validation = serie[(length(serie)-d+1):length(serie)]

#Normalisation de la serie d'apprentissage
	
	app_n= (apprentissage-mean(apprentissage))/sd(apprentissage)
	k= analyse_k_lineaire(app_n,horizon)
	
	matrice= crea_mat(app_n,horizon,k)
	modele= lm(y~., data = matrice)
	
	deb= length(app_n)-k+1
	fin= length(serie)
	t=c(deb:fin)
	
	matpred= crea_mat(t,horizon,k)
	prediction = predict(modele, matpred[,1:k])

	eqm_v= eqm(prediction,validation)
	return(eqm_v)
}