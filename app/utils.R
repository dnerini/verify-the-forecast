error.function <- function(obs, fct, score, thr=0, ineq=">", offset=0.01) {
  if(score=="RMSE"){
    errors = fct - obs
    y = sqrt(mean(errors^2,na.rm=TRUE))
  }else if(score=="MAE"){
    errors = fct - obs
    y = mean(abs(errors),na.rm=TRUE)
  }else if(score=="ME"){
    errors = fct - obs
    y = mean(errors,na.rm=TRUE)
  }else if(score=="Corr"){
    y = cor(fct, obs, use="pairwise.complete.obs")
  }else if(score=="Scatter"){
    mult_res = 10.0*log10((fct + offset)/(obs + offset))
    y = 0.5*(quantile(mult_res, 0.84, na.rm=TRUE) - quantile(mult_res, 0.16, na.rm=TRUE))
  }else if(score=="CSI"){ 
    r = calculate.hits.etc(fct, obs, thr, ineq)
    y = r$H/(r$H+r$M+r$F) 
  }else if(score=="GSS"){ 
    r = calculate.hits.etc(fct, obs, thr, ineq)
    y = (r$POD-r$FA)/((1-r$s*r$POD)/(1-r$s)+r$FA*(1-r$s)/r$s)
  }else if(score=="HK"){ 
    r = calculate.hits.etc(fct, obs, thr, ineq)
    y = r$POD-r$FA
  }else if(score=="HSS"){ 
    r = calculate.hits.etc(fct, obs, thr, ineq)
    y = 2*(r$H*r$R-r$F*r$M)/((r$H+r$M)*(r$M+r$R)+(r$H+r$F)*(r$F+r$R))
  }else if(score=="SEDI"){ 
    r = calculate.hits.etc(fct, obs, thr, ineq)
    y = (log(r$FA)-log(r$POD)+log(1-r$POD)-log(1-r$FA))/(log(r$FA)+log(r$POD)+log(1-r$POD)+log(1-r$FA))
  } # if
  return(y)
}

calculate.hits.etc <- function(fct, obs, thr, ineq){
  
  # apply threshold
  if(ineq=="<"){
    fctb = fct < thr
    obsb = obs < thr
  }else if(ineq=="=" | ineq=="=="){
    fctb = fct == thr
    obsb = obs == thr
  }else if(ineq==">="){
    fctb = fct >= thr
    obsb = obs >= thr
  }else if(ineq=="<="){
    fctb = fct <= thr
    obsb = obs <= thr
  }else{
    fctb = fct > thr
    obsb = obs > thr
  }
  
  # calculate hits, misses, false positives, correct rejects
  H = sum((fctb==1)*(obsb==1)) # correctly predicted precip
  F = sum((fctb==1)*(obsb==0)) # predicted precip even though none there
  M = sum((fctb==0)*(obsb==1)) # predicted no precip even though there was
  R = sum((fctb==0)*(obsb==0)) # correctly predicted no precip
  
  # simple scores 
  POD = H/(H+M)       # probability of detection
  FAR = F/(H+F)       # false alarm ratio
  FA  = F/(F+R)        # false alarm rate = prob of false detection
  s = (H+M)/(H+M+F+R) # base rate = freq of observed events
  
  return(list("H"=H,"F"=F,"M"=M,"R"=R,"POD"=POD,"FAR"=FAR,"FA"=FA,"s"=s))
}

parse.thr <- function(thrstring){
  ineq = gsub('[^<>=]', '', thrstring)
  thr = as.numeric(gsub('[^0-9.-]', '', thrstring))
  return(list("ineq"=ineq,"thr"=thr))
}

get.plz <- function(x){
  if(x=='All'){
    plz = list.plz
  }else if(x=='North'){
    plz = list.region$north
  }else if(x=='West'){
    plz = list.region$west
  }else if(x=='South'){
    plz = list.region$south
  }else{
    plz = list.plz[list.names.en.2==x]
  }
  return(plz)
}
