### max likelihood functions
require("moments")
require("VGAM")


init.par<-function(x,dist) {
  par<-numeric()
  if (dist=="pois") {
    par<-mean(x) 
  } else if (dist=="nbinom") {
    mean<-mean(x)
    var<-var(x)
    par[1]<-mean^2/(var-mean)
    par[2]<-mean
  } else if (dist=="gamma") {
    mean<-mean(x)
    var<-var(x)
    par[1]<-mean^2/var
    par[2]<-mean/var
  } else if (dist=="lnorm") {
    par[1]<-mean(log(x))
    par[2]<-sd(log(x))
  } else if (dist=="weibull") {
    q10<- quantile(x,probs = 0.1)
    q90<- quantile(x,probs = 0.9)
    par[1]<-log(log(1/(1-0.1))/log(5))/log(q10/q90)
    par[2]<-q90/(log(1/(1-0.9))^(1/par[1]))
 
  } else if (dist=="lomax") {
    mean<-mean(x)
    var<-var(x)
    par[1]<-(mean^3+var*mean)/(var-mean^2)
    par[2]<-par[1]/mean+1
  }
  par
}


mix.lik<-function(par, x=obs, dist1, dist2, mix=F) {
  
  nb.param<-0
  
  if (dist1 %in% c("pois","exp")) {
    nb.param<-nb.param+1
  } else if (dist1 %in% c("gamma","lnorm","norm","nbinom","lomax","weibull")) {
    nb.param<-nb.param+2
  }
  
  if (!is.na(dist2) & !dist2=="") {
    ### 1 addditional parameter for the mixing parameter
    if (dist2 %in% c("pois","exp")) {
      nb.param<-nb.param+2
    } else if (dist2 %in% c("gamma","lnorm","norm","nbinom","lomax","weibull")) {
      nb.param<-nb.param+3
    }
  }
  
  if (is.na(dist2) | dist2=="") {
  
    if (dist1=="pois") {
      p1<-exp(par[1])
      eval(parse(text = paste("L<-log(d",dist1,"(x, p1))",sep="")))
    } else {
      p1<-exp(par[1])
      p2<-exp(par[2])
      
      if (dist1=="nbinom") {
        eval(parse(text = paste("L<-log(d",dist1,"(x, size=p1, mu=p2))",sep="")))
      } else {
        eval(parse(text = paste("L<-log(d",dist1,"(x, p1, p2))",sep="")))
      }
    }
    
  } else {
    p1<-exp(par[1])
    p2<-exp(par[2])
    p3<-exp(par[3])
    p4<-exp(par[4])
    mix<-1/(1+exp(-par[5]))
    
    if (dist2=="nbinom") {
      eval(parse(text = paste("L<-log(mix* d", dist1, "(x, size=p1, mu=p2) + (1-mix) *d", dist2, "(x, size=p3, mu=p4))",sep="")))
    } else {
      eval(parse(text = paste("L<-log(mix* d", dist1, "(x, p1, p2) + (1-mix) *d", dist2, "(x, p3, p4))",sep="")))
    }
  }
  -sum(L)
}


#test<- rweibull(1000,10,2)
#test<-readRDS("data/loss_data.Rds")
#(ini<-init.par(test$loss, "weibull"))
#mix.lik(par = log(ini), x=test$loss, dist1 = "weibull",dist2="")
#mix.lik.trunk(par = log(ini), x=test$loss, dist1 = "weibull",dist2="")


#summary(log(dweibull(test$loss, shape = ini[1],scale = ini[2])))
#summary(dweibull(test$loss, shape = ini[1],scale = ini[2],log=T))


mix.lik.trunk<-function(par, x=obs, dist1, dist2, right.trunk=Inf, mix=F) {
  
  nb.param<-0
  
  if (dist1 %in% c("pois","exp")) {
    nb.param<-nb.param+1
  } else if (dist1 %in% c("gamma","lnorm","norm","nbinom","lomax","weibull")) {
    nb.param<-nb.param+2
  }
  
  if (!is.na(dist2) & !dist2=="") {
    ### 1 addditional parameter for the mixing parameter
    if (dist2 %in% c("pois","exp")) {
      nb.param<-nb.param+2
    } else if (dist2 %in% c("gamma","lnorm","norm","nbinom","lomax","weibull")) {
      nb.param<-nb.param+3
    }
  }
  
  if (is.na(dist2) | dist2=="") {
    
    if (dist1=="pois") {
      p1<-exp(par[1])
      eval(parse(text = paste("L<-log(d",dist1,"(x, p1))",sep="")))
    } else {
      p1<-exp(par[1])
      p2<-exp(par[2])
      
      if (dist1=="nbinom") {
        eval(parse(text = paste("L<-log(d",dist1,"(x, size=p1, mu=p2))",sep="")))
      } else {
        eval(parse(text = paste("L<- d",dist1,"(x, p1, p2, log=T) - p",dist1,"(right.trunk, p1, p2, log=T)",sep="")))
      }
    }
    
  } else {
    p1<-exp(par[1])
    p2<-exp(par[2])
    p3<-exp(par[3])
    p4<-exp(par[4])
    mix<-1/(1+exp(-par[5]))
    
    if (dist2=="nbinom") {
      eval(parse(text = paste("L<-log(mix* d", dist1, "(x, size=p1, mu=p2) + (1-mix) *d", dist2, "(x, size=p3, mu=p4))",sep="")))
    } else {
      eval(parse(text = paste("L<-log(mix* d", dist1, "(x, p1, p2) + (1-mix) *d", dist2, "(x, p3, p4))",sep="")))
    }
  }
  -sum(L)
}



