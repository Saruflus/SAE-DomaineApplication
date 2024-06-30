MMC=function(age,vitesse,methode,nbpara,precision=10,borne=-Inf,initial=NULL){
  
  SQ_methode=function(p){ #Cette fonction permet de calculer la somme carre des residus            #theta sont les parametre qu 
    sum((vitesse-methode(age,p))^2)}
  
  o=rep(10^6,precision) #o permet de stocker la fonction objectif qu'on voudra le plus bas
  r=rep(0,precision) #r permet de sotcker le RÂ²
  para=rep(10^6,precision*(nbpara+1)) #pour stocker tout les parametres
  
  for (i in seq(nbpara+1,precision*(nbpara+1),nbpara+1)){
    if(is.null(initial)){initial=runif(nbpara)}else{
      intial=rnorm(nbpara,initial,1)
    }
    #On cherche le min parmi les 40 essaies (pas de 5 pour sotcker les 4 parametres, cf "ici")
    sol=nlminb(start=initial,SQ_methode) #On initialise malheuresement avec runif... nlminb=optim
    res= nlminb(start = sol$par,SQ_methode,lower=borne)
    para[i:(i+nbpara-1)]=res$par #ici"
    o[i/(nbpara+1)]=res$objective}
  
  compt=which.min(o) #ou etait le min de notre fonction a minimiser ? 
  parametre=rep(0,nbpara)
  parametre=para[((nbpara+1)*compt):(((nbpara+1)*compt)+(nbpara-1))] #on stock les meilleurs parametres
  fit=methode(age,parametre) #regression 
  R2=sum((fit-mean(vitesse))^2)/sum((vitesse-mean(vitesse))^2) #RÂ² 
  # plot(age,vitesse,xlab="Age",ylab="Vitesse en m/s")
  # lines(sort(age),methode(sort(age),parametre),col=round(runif(1,1,3),0))
  peak=max((methode(sort(age),parametre)))
  return(list(parametre=parametre,objectif=min(o),R2=R2,peak=peak))}
