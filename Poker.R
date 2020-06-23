a <- c(.85881,.99700,.75289,.82813,.02818,.36065,.45649,.06451,.07582,.73994,
      .52480,.03333,.50410,.76568,.11767,.37587,.55763,.33089,.53339,.41700,
      .24577,.74797,.92023,.93143,.05520,.94996,.35838,.85376,.41727,.08969)

a <- c(0.06141,0.72484,0.94107,0.56766,0.14411,0.87648,0.81792,0.48999,
       0.18590,0.06060,0.11223,0.64794,0.52953,0.50502,0.30444,0.70688,
       0.25357,0.31555,0.04127,0.67347,0.28103,0.99367,0.44598,0.73997,
       0.27813,0.62182,0.82578,0.85923,0.51483,0.09099)

a <- c(0.03991,0.10461,0.93716,0.16894,0.98953,0.38555,0.95554,0.32886,0.5978,0.09958,0.17546,
     0.73704,0.92052,0.46215,0.15917,0.32643,0.52861,0.95819,0.06831,0.1964,0.69572,0.68777,0.3951,
     0.35905,0.85244,0.24122,0.66591,0.27699,0.06494,0.03152,0.61196,0.30231,0.92962,0.61773,0.22109,
     0.30532,0.21704,0.10274,0.12202,0.94205,0.03788,0.97599,0.75867,0.20717,0.82037,0.48228,0.63379,
     0.85783,0.47619,0.87481,0.88618,0.19161,0.4129,0.63312,0.71857,0.71299,0.23853,0.0587,0.01119,
     0.92784,0.27954,0.58909,0.82444,0.99005,0.04921,0.80863,0.00514,0.20247,0.81759,0.45197,0.33564,
     0.6078,0.4846,0.85558,0.15191,0.90899,0.75754,0.60833,0.25983,0.01291,0.78038,0.70267,0.43529,
     0.06318,0.38384,0.55986,0.86485,0.88722,0.56736,0.66164,0.87539,0.08823,0.94813,0.319,0.54155,
     0.16818,0.60311,0.74457,0.90561,0.72848)

a <- runif(100)

# PRUEBA POKER PARA 5 DIGITOS ---------------------------------------------

prueba_poker <- function(x,agrupar=F){
  
  n_divididos <- lapply(strsplit(substr(format(a, nsmall = 5),3,7),split = ""),as.numeric)
  
  d <- lapply(n_divididos,function(x){
    t <- table(x)
    par <-  sum(t==2)
    return(info =list(
      TD = length(t)==5,
      P1 = sum(t==2)==1 && !(sum(c(2,3) %in% t) ==2),
      P2 = sum(t==2)==2,
      TE = 3 %in% t && sum(t==2)!=1 ,
      TP = sum(c(2,3) %in% t) ==2 ,
      P = 4 %in% t,
      Q = 5 %in% t
      )) 
    })
  
  dd <- data.frame(vector = format(a, nsmall = 5),do.call(rbind.data.frame, d))
  row.names(dd) <- seq_along(dd$vector)
  
  fo = t(t(colSums(dd[-1])))
  
  as <- data.frame(tip = row.names(fo),fo,row.names = 1:length(fo))
  
  b <- data.frame(tip = c("TD","P1","P2","TE","TP","P","Q"),
                  pe = c(0.3024,0.5040,0.1080,0.072,0.0090,0.0045,0.001))
  
  f <- merge(b,as,by="tip",all.b=T,sort=F)
  f["fe"] <- length(a)*f$pe
  f["calc"] <- (f$fe-f$fo)**2/f$fe
  f["est"] <- 1
  if(agrupar = T){
    for(i in nrow(f):2){
    
    ff <- f[i,]
   
    if(f[i,"fe"]<=5 &  (f[i,"est"] == 1 ) ){
      
      su <- 0;f[i,"est"] <- 0
      
      while (f[i,"fe"]<=5) {  #print(f)
        f[i,"fo"] <-  f[i,"fo"] + f[i-1-su,"fo"] 
        f[i,"fe"] <-  f[i,"fe"] + f[i-1-su,"fe"] 
        
        if(f[i,"fe"]<=5){ 
          f[i-1-su,"est"] = 0 
        } else {
          f[i,"est"] = 1
          f[i-1-su,"est"] = 0 
          f[i,"tip"] = f[i-1-su,"tip"]
        }
        su <- su + 1  # print(f)
        
      }
    }
    }
    f <- f[f$est==1,]
  }
  f["calc"] <- (f$fe-f$fo)**2/f$fe
  
  est <- sum(f$calc) 
  crit <- qchisq(0.05,nrow(f)-1,lower.tail = F)
  dec <- ifelse(est > crit,"Rechaza: No estan ordenados al azar",
         "No se rechaza: Estan ordenados al azar")
  return(list(tabla_frecuencias = f,estadistico=est,critico = crit,decision=dec))
}

# FUNCIONES OPCIONALES PARA PRUEBA POKER ----------------------------------

options(scipen = 999)

s <- function(d,i){ r <- c()
  for(c in i){   r[c] <- ( 1/factorial(c) ) *  sum( ((-1) ** (0:c) )* choose(c,0:c)*(c-0:c)**d)  }
  return(r)
}
d <- 5
x <- a
fact <- factorial(0:d)
(fact[d + 1] / (d**d * fact[d - ind + 1] * s(d,1:d))) * length(x)/d

