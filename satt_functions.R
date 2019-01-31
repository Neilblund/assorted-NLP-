
#FUNCTIONS
authorFrex<-function(model){
  cat<-model$replicate$CAT+.01
  afrex<-matrix(NA, nrow=nrow(cat), ncol=ncol(cat), 
                dimnames = list(rownames=names(model$replicate$author.vocab), colnames=seq(ncol(cat))))
  
  for(i in 1:ncol(afrex)){
    afrex[,i]<-(cat[,i]/rowSums(cat))/sum(colSums(cat)[i]/rowSums(cat))
    
  }
  afr<-lapply(rownames(afrex), function(x) sort(setNames(object=afrex[x, ], seq(ncol(afrex))), decreasing=T))
  names(afr)<-rownames(afrex)
  
  return(afr)
  
}

topicInfo<-function(model){
  ctv<-model$replicate$CTV+.01
  frex<-matrix(NA, nrow=nrow(ctv), ncol=ncol(ctv), 
               dimnames = list(rownames=seq(nrow(ctv)), colnames=names(model$replicate$vocab))
  )
  
  for(i in 1:ncol(frex)){
    frex[,i]<-(ctv[,i]/rowSums(ctv))/sum(colSums(ctv)[i]/rowSums(ctv))
    
  }
  docs<-model$replicate$docnames
  topics<-model$replicate$topics
  tnames<-paste0("topic_",seq(nrow(model$replicate$CTV)))
  tops<-sapply(topics, function(x) prop.table(table(factor(x+1, levels=paste(seq(length(tnames)))))))
  colnames(tops)<-docs
  
  
  wfr<-lapply(rownames(frex), function(x){
    list("words"=sort(setNames(object=frex[x, ], colnames(frex)), decreasing=T),
         "authors"=sort(setNames(model$result$theta[,as.numeric(x)], rownames(model$result$theta)), decreasing=T),
         "documents"=sort(setNames(tops[as.numeric(x),], colnames(tops)), decreasing=T)
    )
  })
  
  names(wfr)<-paste0("topic_", rownames(frex))
  
  
  
  return(wfr)
  
}

#get the words most frequently assigned to a given author
authorWords<-function(model, weight=F){
  wrds<-names(model$replicate$vocab)[unlist(model$replicate$docs)+1]
  authors<-names(model$replicate$author.vocab)[unlist(model$replicate$authors)+1]
  atab<-as.data.frame.matrix(table(wrds, authors))
  
  if(weight==TRUE){
    afreq<-log(ncol(atab)/apply(atab, 1, function(x) sum(x>0)))
    
    afreq<-lapply(atab, function(x) sort((x*afreq), decreasing=T))
    
  }else{
    afreq<-lapply(atab, function(x) sort(setNames(object=x, rownames(atab)), decreasing=T))
  }
  return(afreq)
}

authorInfluence<-function(model, level='country'){
  Psi<-model$result$psi
  bestauthors<-lapply(seq(nrow(Psi)), function(x){
    authors<-Psi[x,which(Psi[x,]>0)]
    return(authors - (1/length(authors)))
    
  })
  best<-unlist(bestauthors)
  
  if(level=="agreement"){
    best<-data.frame("agreement"=rep(rownames(Psi), lengths(bestauthors)) ,
                     "country"=names(unlist(bestauthors)),
                     "measure"=unname(unlist(bestauthors)))
  }
  if(level=="country"){
    best<-tapply(best, INDEX=names(best), mean)
    
  }
  return(best)
}


