#' Descriptive statistics function
#'
#' This function calculates a suite of robust descriptive statistics for interval or ratio-scale data.
#' @param x Specifies the variable/vector to be analyzed (required).
#' @param type Specifies whether the data represents a sample or a population. Default is sample. Abbreviations s and p are ok.
#' @param plot Boolean to generate a histogram of the data. Default is FALSE.
#' @export
#' @examples
#' x <- runif(100,90,110)
#' descriptives(x)	#returns an analysis of a distribution with (most likely) no outliers.
#' x.2 <- c(rnorm(100,100,15), 19, 12, 190)
#' descriptives(x.2, type="p", plot=T)	#returns an analysis of a distribution with outliers using population formulas for variance and std. deviation and asks for the histogram.

descriptives <- function(x, type="sample", dig=3, plot=F){
	require(ggplot2)
	type <- tolower(type)
	if(type!="sample" & type!="s" & type!="population" & type!="p"){
		stop("Please specify whether your data as a sample or a population in the type option (default or ommitted option is sample). Note: this only affects the variance and std. deviation calculations.")}	else
	v.n <- length(x[!is.na(x)])
	m.n <- length(x[is.na(x)])
	mn <- mean(x, na.rm=T)
	m <- median(x, na.rm=T)
	mode.value <- function(x){
		x <- sort(x)
		mode.x <- as.data.frame(table(x))
		if(var(mode.x$Freq, na.rm=T)==0)
		mode.result <- data.frame(x=FALSE,Freq=FALSE)
		else(mode.result <- subset(mode.x, mode.x$Freq==max(mode.x$Freq)))
		aggregate(x ~ Freq, data=mode.result, paste, collapse=",")
		}
	v <- ifelse(type=="population", var(x, na.rm=T)*(length(x[!is.na(x)])-1)/length(x[!is.na(x)]), var(x, na.rm=T))
	s <- ifelse(type=="population", sd(x, na.rm=T)*sqrt((length(x[!is.na(x)])-1)/length(x[!is.na(x)])), sd(x, na.rm=T))
	x.min <- min(x, na.rm=T)
	x.max <- max(x, na.rm=T)
			dMADs  <-  function(x, threshold=4, zero.mad.action="warn"){
   		x <- sort(x)
   		two.sided.mad  <-  function(x, zero.mad.action="warn"){
      		x <- x[!is.na(x)]
   			m <- median(x)
   			abs.dev <- abs(x-m)
   			left.mad <- median(abs.dev[x<=m])
   			right.mad <- median(abs.dev[x>=m])
   			if(left.mad==0 || right.mad==0){
      			if(zero.mad.action == "stop") stop("MAD is 0")
      				if(zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
      					if(zero.mad.action %in% c("na", "warn and na")){
         					if(left.mad==0) left.mad <- NA
         					if(right.mad==0) right.mad <- NA
      						}
   				}
   			return(c(left.mad, right.mad))
		}
   			m <- median(x, na.rm=TRUE)
   			x.mad <- rep(two.sided.mad(x)[1], length(x))
   			x.mad[x > m] <- two.sided.mad(x)[2]
   			mad.distance <- abs(x-m)/x.mad
   			mad.distance[x==m] <- 0
   		dMADs.result <- unique(x[mad.distance > threshold])
   		if(length(dMADs.result)==1){return(format(round(dMADs.result, dig), nsmall=dig))}	else
   		if(length(dMADs.result)>1){return(list(format(round(dMADs.result[1],dig),nsmall=dig), paste(format(round(dMADs.result[2:length(dMADs.result)],dig),nsmall=dig))))}		else
   		return(NA)
   		}
	skew <- function(x){
    	sk <- function(xx){
        	n <- length(x[!is.na(xx)])
        	mn <- mean(xx, na.rm=TRUE)
        	dif.x <- xx-mn
        	m2 <- sum(dif.x^2, na.rm=TRUE)/n
        	m3 <- sum(dif.x^3, na.rm=TRUE)/n
        	m3/(m2^(3/2))
    		}
    	if(ncol(x)==1 || is.null(dim(x))) 
        	return(sk(x))
    		else return(apply(x, 2, sk))
		}
	kurtosis <- function(x){
    	kt <- function(xx){
        	n <- length(x[!is.na(xx)])
        	mn <- mean(xx, na.rm=TRUE)
        	dif.x <- xx-mn
        	m2 <- sum(dif.x^2, na.rm=TRUE)/n
        	m4 <- sum(dif.x^4, na.rm=TRUE)/n
        	(m4/m2^2)-3
    		}
    	if(ncol(x)==1 || is.null(dim(x))) 
        	return(kt(x))
    		else return(apply(x, 2, kt))
		}
	if(type=="sample" | type=="s"){	
	if(is.list(dMADs(x))==T){	
	       desc.out <- data.frame(cbind(c("valid n","missing","mean","median","modal value(s)","modal freq","sample variance","sample std. deviation","minimum","maximum","skew","kurtosis","potential outlier value(s)"), 
	       result=c(v.n,
	                m.n,
	                format(round(mn,dig),nsmall=dig),
	                format(round(m,dig),nsmall=dig),
	                mode.value(x)[2],
	                mode.value(x)[1],
	                format(round(v,dig),nsmall=dig),
	                format(round(s,dig),nsmall=dig),
	                format(round(x.min,dig),nsmall=dig),
	                format(round(x.max,dig),nsmall=dig),
	                format(round(skew(x),dig),nsmall=dig),
	                format(round(kurtosis(x),dig),nsmall=dig),
	                dMADs(x)[1]),
	        cont.=c("", "", "", "", "", "", "", "", "", "", "", "", dMADs(x)[2])),
	       row.names=T)	}	else
	       desc.out <- data.frame(cbind(c("valid n","missing","mean","median","modal value(s)","modal freq","sample variance","sample std. deviation","minimum","maximum","skew","kurtosis","potential outlier value(s)"), 
	       result=c(v.n,
	                m.n,
	                format(round(mn,dig),nsmall=dig),
	                format(round(m,dig),nsmall=dig),
	                mode.value(x)[2],
	                mode.value(x)[1],
	                format(round(v,dig),nsmall=dig),
	                format(round(s,dig),nsmall=dig),
	                format(round(x.min,dig),nsmall=dig),
	                format(round(x.max,dig),nsmall=dig),
	                format(round(skew(x),dig),nsmall=dig),
	                format(round(kurtosis(x),dig),nsmall=dig),
	                dMADs(x))),
	         	row.names=T)}	else
	if(type=="population" | type=="p"){         	     	
	if(is.list(dMADs(x))==T){	
	       desc.out <- data.frame(cbind(c("valid n","missing","mean","median","modal value(s)","modal freq","population variance","population std deviation","minimum","maximum","skew","kurtosis","potential outlier value(s)"), 
	       result=c(v.n,
	                m.n,
	                format(round(mn,dig),nsmall=dig),
	                format(round(m,dig),nsmall=dig),
	                mode.value(x)[2],
	                mode.value(x)[1],
	                format(round(v,dig),nsmall=dig),
	                format(round(s,dig),nsmall=dig),
	                format(round(x.min,dig),nsmall=dig),
	                format(round(x.max,dig),nsmall=dig),
	                format(round(skew(x),dig),nsmall=dig),
	                format(round(kurtosis(x),dig),nsmall=dig),
	                dMADs(x)[1]),
	        cont.=c("", "", "", "", "", "", "", "", "", "", "", "", dMADs(x)[2])),
	       row.names=T)	}	else
	       desc.out <- data.frame(cbind(c("valid n","missing","mean","median","modal value(s)","modal freq","population variance","population std deviation","minimum","maximum","skew","kurtosis","potential outlier value(s)"), 
	       result=c(v.n,
	                m.n,
	                format(round(mn,dig),nsmall=dig),
	                format(round(m,dig),nsmall=dig),
	                mode.value(x)[2],
	                mode.value(x)[1],
	                format(round(v,dig),nsmall=dig),
	                format(round(s,dig),nsmall=dig),
	                format(round(x.min,dig),nsmall=dig),
	                format(round(x.max,dig),nsmall=dig),
	                format(round(skew(x),dig),nsmall=dig),
	                format(round(kurtosis(x),dig),nsmall=dig),
	                dMADs(x))),
	         	row.names=T)}
	         	
	if(plot==T){
	nm <- deparse(substitute(x))	
	desc.hist <- ggplot2::ggplot(as.data.frame(x), aes(x=as.data.frame(x))) + ggplot2::geom_histogram(color="lightgray", fill="#2471A3") + ggplot2::labs(title=paste0("Histogram of ",nm), x=nm) + ggplot2::scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + ggplot2::theme_classic() + ggplot2::theme(axis.line.x=element_line(color="black", size=0.25, linetype="solid"), axis.line.y=element_line(color="black", size=0.25, linetype="solid"), plot.title = element_text(hjust = 0.5))
	suppressMessages(print(desc.hist))
	return(descriptives=desc.out)}	else	return(descriptives=desc.out)
	}