model_run <- function() {

  patient <- list()

  if (runif(1, min = 0, max = 1) >= 0.5) {
    patient$male <- "Female"
  } else {patient$male <- "Male"}

  if (runif(1, min = 0, max = 1) >= 0.258) {
    patient$smoker <- "Former Smoker"
  } else {patient$smoker <- "Smoker"}

  if (runif(1, min = 0, max = 1) >= 0.4685) {
    patient$oxygen <- "No"
  } else {patient$oxygen <- "Yes"}

  if (runif(1, min = 0, max = 1) >= 0.2265) {
    patient$statin <- "No"
  } else {patient$statin <- "Yes"}

  if (runif(1, min = 0, max = 1) >= 0.6504) {
    patient$LAMA <- "No"
  } else {patient$LAMA <- "Yes"}

  if (runif(1, min = 0, max = 1) >= 0.5206) {
    patient$LABA = "No"
  } else {patient$LABA <- "Yes"}

  if (runif(1, min = 0, max = 1) >= 0.5723) {
    patient$ICS = "No"
  } else {patient$ICS <- "Yes"}

  patient$age  <- round(rnorm(1, mean = 64.68, sd = 8.75))
  patient$FEV1 <- round(rnorm(1, mean = 40.60, sd = 15.93))
  patient$BMI  <- round(rnorm(1, mean = 27.53, sd = 6.43))
  patient$SGRQ <- round(rnorm(1, mean = 49.95, sd = 16.72))

  #patient$LastYrExacCount <- rpois (n = 1, lambda = 1.42)
  #patient$LastYrSevExacCount <- rpois (n = 1, lambda = 0.29)

  covar <- cbind(c(2.55, 0.87), c(0.57, 0.44))
  exacSample <- rmvpois(10, c(1.42, 0.29), covar)[1,]
  patient$LastYrExacCount <- exacSample[1]
  patient$LastYrSevExacCount <- exacSample[2]


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
