error.function <- function(obs, fct, score) {
  if(score=="RMSE"){
    errors = fct - obs
    y = sqrt(mean(errors^2,na.rm=TRUE))
  }else if(score=="MAE"){
    errors = fct - obs
    y = mean(abs(errors),na.rm=TRUE)
  }else if(score=="ME"){
    errors = fct - obs
    y = mean(errors,na.rm=TRUE)
  } # if
  return(y)
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

