simulatePatient <- function() {

  patient <- list()

  coin <- runif(1, min = 0, max = 1)

  if (coin >= 0.5) {
    patient$male <- 0
  } else {patient$male <- 1}

  if (coin >= 0.258) {
    patient$smoker <- 0
  } else {patient$smoker <- 1}

  if (coin >= 0.4685) {
    patient$oxygen <- 0
  } else {patient$oxygen <- 1}

  if (coin >= 0.2265) {
    patient$statin <- 0
  } else {patient$statin <- 1}

  if (coin >= 0.6504) {
    patient$LAMA <- 0
  } else {patient$LAMA <- 1}

  if (coin >= 0.5206) {
    patient$LABA = 0
  } else {patient$LABA <- 1}

  if (coin >= 0.5723) {
    patient$ICS = 0
  } else {patient$ICS <- 1}

  patient$age  <- rnorm(1, mean = 64.68, sd = 8.75)
  patient$FEV1 <- rnorm(1, mean = 40.60, sd = 15.93)
  patient$BMI  <- rnorm(1, mean = 27.53, sd = 6.43)
  patient$SGRQ <- rnorm(1, mean = 49.95, sd = 16.72)

  patient$LastYrExacCount <- rpois (n = 1, lambda = 1.42)
  patient$LastYrSevExacCount <- rpois (n = 1, lambda = 0.29)


  return((flatten_list(patient)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}


#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
