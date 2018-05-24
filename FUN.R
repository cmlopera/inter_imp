# Funciones

# Generaci?n de datos para la simulaci?n
gendata<-function(s,n=100,v=6,len=0.25,asist=T){
  # Probabilidades de asistencia
  if(asist){
    probs=c(0,0,0,0,0,0)
  } else {
    probs=c(0,0,0.1,0.1,0.2,0.2)
  }
  # tiempo inicio
  t0<-rep(0,n)
  alpha<--0.1
  while(alpha<=0){
  # variables auxiliares
  X1<-runif(n,0,1)
  X2<-runif(n,0,1)
  # par?metros del modelo de riesgo proporcional
  beta1<-0.3
  beta2<-0.25
  # funcion de riesgo
  tasariesgo<-beta1*X1+beta2*X2
  # tiempos de falla
  T<-rexp(n,tasariesgo) 
  # cuantil 75% de la distrib. de T
  q75<-log(4)/median(tasariesgo) #tasariesgo
  # alpha t.q. q75=alpha/2+4*len, con len=0.25
  alpha<-2*(q75-4*len)
  }
  alpha
  # tiempos primera visita
  t1=runif(n,0,alpha)
  # censura a derecha resultante en la ?ltima visita asumiendo que siempre van
  # print(sum(T>(t1+4*len))/n)
  # visitas
  tvisitas<-matrix(0,ncol=v-2,nrow=n)
  colnames(tvisitas)<-c("t2","t3","t4","t5")
  for(k in 1:ncol(tvisitas)){
    tvisitas[,k]=t1+k*len
  }
  # matriz con tiempos de falla y tiempos de visitas
  tmp<-cbind(T,t0,t1,tvisitas)
  # data frame con tiempos de falla, intervalo e indicadora de censura
  tmp<-as.data.frame(t(apply(tmp,1,intervalos,probs=probs,v=v)))
  names(tmp)<-c("tr","tl","tu","cen")
  # data frame adicionando tasas de riesgo y vars. aux.
  tmp<-cbind(tasariesgo,X1,X2,tmp)
  median(tmp$tr)
  return(tmp)
}

# Genera intervalos de censura
intervalos<-function(data,probs=probs,v=v){
  # tiempo de falla
  tr<-data[1]
  # probs de asistencia
  aleatorio<-runif(v,0,1)
  # tiempos de visitas reales
  tobs<-data[-1][aleatorio>probs]
  # intervalos de censura
  tl<-max(ifelse(tobs<tr,tobs,0))
  tu<-min(ifelse(tobs>=tr,tobs,Inf))
  # indicadora de censura a derecha
  cen=(tu<Inf)+0
  # vector con tiempos de falla, intervalo e indicadora de censura
  c(tr,tl,tu,cen)
}

# Calcular SE para estimaciones PO
estSE<-function(mm,dd,med){
  ff<-dd[mm,]
  tt<-icfit(Surv(ff$tl,ff$tu,type='interval2')~1)
  ss<-getsurv(times=med, tt)[[1]]$S
  return(ss)
}

# Calcular estimaciones para los vecinos de cada sujeto
estVC<-function(i,vecinos=vecinos,dd=dd,rm=rm){
  # i=18; vecinos=vecinos; dd=dd
  print(i)
  vv<-vecinos[[i]]
  vcn<-dd[vv,]
  tbvcn<-icfit(Surv(vcn$tl,vcn$tu,type='interval2')~1)
  plot(tbvcn,main=paste('i =',i),xlim=c(0,max(rm,ifelse(dd[i,6]==Inf,0,dd[i,6]),ifelse(tbvcn$intmap==Inf,0,tbvcn$intmap))));abline(v=dd[i,5:6],lwd=2);abline(v=rm,col='blue',lwd=2)
  # imputaci?n a traves de NPMLE
  if(dd[i,6]==Inf){
    tmp1<-colMeans(tbvcn$intmap)[colMeans(tbvcn$intmap)<Inf]
    tmp2<-tmp1[tmp1>=dd[i,5] & tmp1<=rm]
    t.imp<-if(length(tmp2)==0){
      c(rm,0)
    } else{
      c(tmp2[sample(length(tmp2),1)],1)
    }
  } else{
    tmp1<-colMeans(tbvcn$intmap)[colMeans(tbvcn$intmap)<Inf]
    tmp2<-tmp1[tmp1>=dd[i,5] & tmp1<=dd[i,6]]
    t.imp<-if(length(tmp2)==0){
      c(runif(1,dd[i,5],dd[i,6]),1)
    } else{
      c(tmp2[sample(length(tmp2),1)],1)
    }
  }
  t.imp
}
#lapply(1:2,estVC);head(dd)  # OJO sale error...
#uniroot(function(x) getsurv(times=x, tbvcn)[[1]]$S-0.4073236,dd[i,5:6])$root

# estimaciones generales basadas en datos generados con vars. aux.
estim<-function(dd,med){
     # dd<-datos[[1]];head(dd)
     # dd2<-datos[[2]];head(dd2)
  dd<-dd
  time<-dd$tr
  tl<-dd$tl
  tu<-dd$tu
  q50<-med
  
  # Estimaciones FO
  kmest<-survfit(Surv(time)~1,error="greenwood")
  FOES<-summary(kmest,times=q50)$surv
  FOSE<-summary(kmest,times=q50)$std.err
  FOLI<-summary(kmest,times=q50)$lower
  FOLS<-summary(kmest,times=q50)$upper
  FOCR<-(FOLI<0.5&FOLS>0.5)+0
  
  # Estimaciones PO
  tbest<-icfit(Surv(tl,tu,type='interval2')~1)#,conf.int=T,error="greenwood"
  rm <- max(tbest$intmap[-1,][tbest$intmap[-1,]<Inf])
  POES<-getsurv(times=q50, tbest)[[1]]$S
  #print(POES)
   # 500 remuestras para calcular SE en PO
  mm<-replicate(500,sample(100,replace=T))
  POSE<-sd(apply(mm,2,estSE,dd,q50))
  POCR<-(POES-qt(0.975,n-1)*POSE<0.5&POES+qt(0.975,n-1)*POSE>0.5)+0
  
# Estimaciones NPMLEI
  dd$mp<-ifelse(dd$cen==0,dd$tl,(dd$tl+dd$tu)/2)
  modph<-coxph(Surv(mp,cen)~X1+X2,data=dd);modph
  RS<-cbind(dd$X1,dd$X2)%*%modph$coefficients
  RSS<-scale(RS)
  dista<-as.matrix(dist(RSS,diag=T,upper=T))
  vecinos<-sapply(1:n,function(j) which(rank(dista[,j])<=vc+1&rank(dista[,j])!=j))

  # imputación a traves de NPMLE
  t.imp <- lapply(1:n,estVC,vecinos=vecinos,dd=dd,rm=rm)
  imp <- as.data.frame(do.call(rbind, t.imp));names(imp)<-c("t.i","cen.i")
  kmesti<-survfit(Surv(t.i,cen.i)~1,error="greenwood",data=imp)
  FOESi<-summary(kmesti,times=q50)$surv
  FOSEi<-summary(kmesti,times=q50)$std.err
  FOLIi<-summary(kmesti,times=q50)$lower
  FOLSi<-summary(kmesti,times=q50)$upper
  FOCRi<-(FOLIi<0.5&FOLSi>0.5)+0
  
  
  # Resultados obtenidos
  result<-c(FOES,FOSE,FOCR,POES,POSE,POCR,FOESi,FOSEi,FOCRi)
  result
}

