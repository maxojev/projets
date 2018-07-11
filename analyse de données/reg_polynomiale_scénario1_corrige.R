#Fonctions auxiliaires=========================================================

#fonction qui crÃ©e la matrice apropriÃ©e Ã  l'Ã©tude avec la cible dans la colonne k+1
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

#Fonction permettant de calculer l'erreur quadratique moyenne

eqm = function(pred, verite){
  return(mean((verite-pred)^2))
}


# la variable p permet d'avoir une matrice avec des colonnes avec des degrÃ©s

reg_polynomiale_CrossVal= function(serie,horizon,k,p){

	matrice= crea_mat_regPo(serie,horizon,k,p)
  
	f= floor(runif(nrow(matrice),1,11))

	eqm0= 0
	
	for(i in 1:10){
	
		validation= matrice[(which(f==i)),]
		apprentissage= matrice[(which(f!=i)),]
	
		modele= lm(y~., data = apprentissage)
		prediction = predict(modele, validation[,1:(ncol(validation)-1)])
		eqm1= eqm(prediction,validation[,ncol(validation)])
		eqm0= c(eqm0,eqm1)
	}
	eqm_moy= mean(eqm0[2:length(eqm0)])
	
	return (eqm_moy)
}

analyse_kp_lineaire= function(serie,horizon){

 # k prenant des valeurs entre 3 et 70
 # p prenant des valeurs entre 1 et 5
  
  eqm_t= 100000000000000000000
  for (i in 3:round(length(serie)*0.05)){
		
		for (j in 1:5){
		
			j= switch(j,1,c(1,2),c(1,2,3),c(1,2,3,4),c(1,2,3,4,5))		
			eqm_test= reg_polynomiale_CrossVal(serie,horizon,i,j)
			#print(c(i,j,eqm_test))
				if (eqm_test< eqm_t){
	  
				eqm_t=eqm_test
				k= i
				p=j
				}	
		}	
	}
	return (c(k,p))
}
	
	
	
pred_regpolynomialeF = function(serie,horizon,d){

	apprentissage= serie[1:(length(serie)-d)]
	
	res= analyse_kp_lineaire(serie,horizon)
	
	k=res[1]
	p=res[2]
  print(p)	
	p= switch(p,1,c(1,2),c(1,2,3),c(1,2,3,4),c(1,2,3,4,5))
	
	matrice= crea_mat_regPo(serie,horizon,k,p)
	modele= lm(y~., data = matrice)
	
	deb= length(apprentissage)-k+1
	fin= length(serie)
	t=c(deb:fin)
#print(t)	
	matpred= crea_mat_regPo(serie[t],horizon,k,p)
#print(matpred)
	prediction = predict(modele, matpred)[1]

	return(prediction)
}


reg_polynomiale_scenario1 = function(serie,d){
	
	apprentissage= serie[1:(length(serie)-d)]
	test = serie[(length(serie)-d+1):length(serie)]
	prediction=test	
	
	for(i in 1:d){print(i)
		
		resultat= pred_regpolynomialeF(apprentissage,i,d)
		prediction[i]=resultat[1]
		
		
	}
#print(prediction)
	eqm_v= eqm(prediction,test)
	plot(serie, type="l")
	lines(c((length(serie)-d+1):length(serie)),prediction,col="red")
	
	return(list(prediction, eqm_v))
}


























