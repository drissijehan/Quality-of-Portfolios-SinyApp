tauxx<-function(data,col,year,ctx,imp,cls,clsn,ctx1,imp1,cls1,clsn1,askctx,askimp,askcls,askclsn,...){
  if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1  | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,cls1]>1 )
    S=nrow(D3)
    to=S/N
  }
  
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N 
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 )
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,cls1]>1 )
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,clsn1]>1 )
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 |  D[,cls1]>1 )
    S=nrow(D3)
    to=S/N
  }
  
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1)
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 )
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,cls1]>1 )
    S=nrow(D3)
    to=S/N
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,clsn1]>1)
    S=nrow(D3)
    to=S/N
  }
  
  
  
  return(to)
}

obs<-function(data,col,year,ctx,imp,cls,clsn,ctx1,imp1,cls1,clsn1,askctx,askimp,askcls,askclsn,...){
  if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
  }
  
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
  }
  
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
  }
  
  
  
  return(N)
}

defaut<-function(data,col,year,ctx,imp,cls,clsn,ctx1,imp1,cls1,clsn1,askctx,askimp,askcls,askclsn,...){
  if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1  | D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 | D[,cls1]>1 )
    S=nrow(D3)
    
  }
  
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,cls1]>1 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,clsn1]>1)
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])) & (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 | D[,imp1]>=90 )
    S=nrow(D3)
    
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,cls1]>1 )
    S=nrow(D3)
    
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])) & (Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 | D[,clsn1]>1 )
    S=nrow(D3)
    
  }
  else if((askctx == "yes") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx]))  & (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1 |  D[,cls1]>1 )
    S=nrow(D3)
  }
  
  else if((askctx == "yes") & (askimp == "no") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,ctx]==0 | is.na(Y[,ctx])))
    N=nrow(D)
    D3=subset(D, D[,ctx1]==1)
    S=nrow(D3)
    
  }
  else if((askctx == "no") & (askimp == "yes") & (askcls == "no") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,imp]<90 | is.na(Y[,imp])))
    N=nrow(D)
    D3=subset(D,  D[,imp1]>=90 )
    S=nrow(D3)
    
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "yes") & (askclsn == "no")){
    Y=subset(data,data[,col]==year)
    D=subset(Y, (Y[,cls]<=1 | is.na(Y[,cls])))
    N=nrow(D)
    D3=subset(D, D[,cls1]>1 )
    S=nrow(D3)
  }
  else if((askctx == "no") & (askimp == "no") & (askcls == "no") & (askclsn == "yes")){
    Y=subset(data,data[,col]==year)
    D=subset(Y,(Y[,clsn]<=1 | is.na(Y[,clsn])))
    N=nrow(D)
    D3=subset(D, D[,clsn1]>1)
    S=nrow(D3)
    
  }
  
  return(S)
}

