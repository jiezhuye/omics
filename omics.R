argv <- commandArgs(T)
library(MASS)
library(randomForest)
rfcv1<-function (trainx, trainy, cv.fold = 5, scale = "log", step = 0.5, 
    mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE, 
    ...) 
{
    classRF <- is.factor(trainy)
    n <- nrow(trainx)
    if (classRF) {
        f <- trainy
    }
    else {
        f <- factor(rep(1:5, length = length(trainy))[order(order(trainy))])
    }
    nlvl <- table(f)

    idx <- numeric(n)
    for (i in 1:length(nlvl)) {
        idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold, 
            length = nlvl[i]))
    }

    n <- nrow(trainx)	
    p <- ncol(trainx)

    if (scale == "log") {
        k <- floor(log(p, base = 1/step))
        n.var <- round(p * step^(0:(k - 1)))
        same <- diff(n.var) == 0
        if (any(same)) 
            n.var <- n.var[-which(same)]
        if (!1 %in% n.var) 
            n.var <- c(n.var, 1)
    }
    else {
        n.var <- seq(from = p, to = 1, by = step)
    }
    k <- length(n.var)
    cv.pred <- vector(k, mode = "list")
    for (i in 1:k) cv.pred[[i]] <- trainy
    if (classRF) {
        f <- trainy
    }
    else {
        f <- factor(rep(1:5, length = length(trainy))[order(order(trainy))])
    }
    nlvl <- table(f)
    idx <- numeric(n)
    for (i in 1:length(nlvl)) {
        idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold, 
            length = nlvl[i]))
    }
    impvar1=rep(0,p)
    for (i in 1:cv.fold) {
        all.rf <- randomForest(trainx[idx != i, , drop = FALSE], 
            trainy[idx != i], trainx[idx == i, , drop = FALSE], ntree=500,
            trainy[idx == i], mtry = mtry(p), importance = TRUE, 
            ...)
        cv.pred[[1]][idx == i] <- all.rf$test$predicted
        impvar <- (1:p)[order(all.rf$importance[, 1], decreasing = TRUE)]
	   impvar1 <- impvar1 + all.rf$importance[, 1]/all.rf$importanceSD
        for (j in 2:k) {
            imp.idx <- impvar[1:n.var[j]]
            sub.rf <- randomForest(trainx[idx != i, imp.idx, 
                drop = FALSE], trainy[idx != i], trainx[idx == 
                i, imp.idx, drop = FALSE], trainy[idx == i], 
                mtry = mtry(n.var[j]),ntree=500, importance = recursive, 
                ...)
            cv.pred[[j]][idx == i] <- sub.rf$test$predicted
            if (recursive) {
                impvar <- (1:length(imp.idx))[order(sub.rf$importance[, 
                  1], decreasing = TRUE)]
            }
            NULL
        }
        NULL
    }

        error.cv0 <- sapply(cv.pred, function(x)  cor(trainy,x,method="spearman"))

        error.cv1 <- sapply(cv.pred, function(x) mean((trainy-x)^2)/mean(trainy)/mean(x))

        error.cv2 <- sapply(cv.pred, function(x) (1-sum((trainy-x   )^2) / sum((trainy-mean(trainy))^2) ) )

        error.cv <- sapply(cv.pred, function(x) mean((trainy - 
            x)^2))


        error.cv3 <- sapply(cv.pred, function(x) mean(ifelse( trainy<1e-10 ,20,abs(trainy+1e-10-x)/(trainy+1e-10) )  ) )

	 MAE<-function (y_pred, y_true) {
  		  MAE <- mean(abs(y_true - y_pred))
   			 return(MAE)
		}

	 MedianAE=function (y_pred, y_true) {
  		  MedianAE <- median(abs(y_true - y_pred))
    		return(MedianAE)
		}
	RAE=function (y_pred, y_true) {
   		 RAE <- sum(abs(y_true - y_pred))/sum(abs(y_true - mean(y_true)))
    		return(RAE)
	}		

	 RMSE=function (y_pred, y_true) {
   		 RMSE <- sqrt(mean((y_true - y_pred)^2))
   		 return(RMSE)
		}

        error.mae <- sapply(cv.pred, function(x) MAE(x,trainy) )
        error.MedianAE <- sapply(cv.pred, function(x) MedianAE(x,trainy) )
        error.RAE <- sapply(cv.pred, function(x) RAE(x,trainy) )
        error.RMSE <- sapply(cv.pred, function(x)  RMSE(x,trainy) )

    names(error.cv0)<-names(error.cv1)<-names(error.cv2)<-names(error.cv) <- names(cv.pred) <- n.var
    tmp=data.frame(n.var = n.var, error.cor = error.cv0, error.nmse = error.cv1, error.r2 = error.cv2,
				error.mape=error.cv3,
                        error.mse = error.cv,error.mae=error.mae,error.MedianAE =error.MedianAE , error.RAE =error.RAE ,
				 error.RMSE = error.RMSE )
	impvar1=impvar1/cv.fold
	 pred=matrix(unlist(cv.pred),length(cv.pred),byrow=T)
	rownames(pred)=n.var
	colnames(pred)=rownames(trainx)
    list(impvar=impvar1,tmp=tmp[order(tmp$n.var),],pred=pred[order(tmp$n.var),])


}


########################################################################################
target=as.character(argv[1])
ind=as.character(argv[2])
out=as.character(argv[3])
tarf=read.csv(paste("/hwfssz1/ST_META/CD/jiezhuye/software/data/",target,sep=""),head=T,row.names=1)
rownames(tarf)=gsub("\"","",rownames(tarf))
indfs=unlist(strsplit(ind,","))			
predx=c()
tic=1
for(k in c( 1:length(indfs))){
	predf=read.csv(paste("/hwfssz1/ST_META/CD/jiezhuye/software/data/",indfs[k],sep=""),head=T,row.names=1)
	rownames(predf)=gsub("\"","",rownames(predf))
	if(tic>1){
		nm=intersect(rownames(predx),rownames(predf))
		predx=cbind(predf[pmatch(nm,rownames(predf)),],predx[pmatch(nm,rownames(predx)),])
	}else{
		predx=predf
	}
	tic=tic+1
}
		
nm=intersect(rownames(predx),rownames(tarf))
tarf=tarf[pmatch(nm,rownames(tarf)),]
predx=predx[pmatch(nm,rownames(predx)),]
###### processing
if( !grepl("CLR",ind) ){
	mic=which( ( (grepl("mgs",colnames(predx)))| (grepl("k__",colnames(predx)))) & ( apply(predx,2,function(x){    mean(x!=0,na.rm=T)  < 0.05})))
	if(length(mic)>0){
		predx=predx[,-mic]
	}
}
if( !grepl("CLR",target) ){
 	mic=which( ( (grepl("mgs",colnames(tarf)))| (grepl("k__",colnames(tarf)))) & ( apply(tarf,2,function(x){    mean(x!=0,na.rm=T)  < 0.2})))
         if(length(mic)>0){
          	tarf=tarf[,-mic]
	}
}
for(i in 1:ncol(predx)){
	if( length(levels(factor(predx[,i])))  <=7){
		predx[,i]=as.numeric(factor(predx[,i]))
	}else{
		predx[,i]=as.numeric(predx[,i])
	}
	predx[is.na(predx[,i]),i]=median(predx[,i],na.rm=T)
}
for(i in 1:ncol(tarf)){
	if( length(levels(factor(tarf[,i])))  <=7){
		tarf[,i]=as.numeric(factor(tarf[,i]))
	}else{
		tarf[,i]=as.numeric(tarf[,i])
	}
}
predx=predx[,apply(predx,2,function(x){ ifelse( max(table(x))/length(x) >=0.95,0,1)}) ==1]
tarf=tarf[,apply(tarf,2,function(x){ ifelse( max(table(x))/length(x) >=0.95,0,1)}) ==1]
prof1=t(predx)
tarf1=tarf
imp=matrix(NA,nrow(prof1),ncol(tarf1))
crsq=c()
pred=c()
library(randomForest)
for(i in 1:ncol(tarf1)){
	id= which(!is.na(tarf1[,i]))
	if(length(id) >= 100){
		set.seed(0)
		rf3=rfcv1(rbind(t(prof1[,id])),c(tarf1[id,i]))
		set.seed(0)
		cut=which.max( rank(rf3$tmp[,2]) + rank(-rf3$tmp[,8]))
		if(cut <=3){cut=3}
		tmp=data.frame(var=colnames(tarf1)[i],cut=rownames(rf3$pred)[cut],sample=length(id),rf3$tmp)
		crsq=rbind(crsq,tmp)
		px=matrix(NA,nrow(rf3$pred),ncol(prof1))
		colnames(px)=colnames(prof1)
		rownames(px)=rownames(rf3$pred)
		px[,id]=as.matrix(rf3$pred)
		tmp=data.frame(var=colnames(tarf1)[i],num=rownames(rf3$pred),cut=rownames(rf3$pred)[cut],px)
		pred=rbind(pred,tmp)			
		imp[pmatch(names(rf3$impvar),rownames(prof1)),i]=rf3$impvar
	}
}	
colnames(imp)=colnames(tarf1)
rownames(imp)=rownames(prof1)
write.csv(imp,paste(out,'.imp_omics.csv',sep=""))
write.csv(crsq,paste(out,'.crsq_omics.csv',sep=""))
write.csv(pred,paste(out,'.pred_omics.csv',sep=""))
