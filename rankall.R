rankall <- function(outcome, num ="best"){
	dat <- read.csv("outcome-of-care-measures.csv", colClasses = 					"character",na.strings="not available")
	
																																			
	
	if (!outcome %in% c('heart attack','heart failure','pneumonia')){
		stop('invalid outcome')
	}
	
	
	if (!is.numeric(num)){
		if(is.character(num)) {
			if (!num %in% c("best","worst")){
				stop("invalid num")
			}
		}
			
		
	}
	
	col = c('heart attack'=11,'heart failure'=17,'pneumonia'=23)
	state = unique(dat$State)
	state = state[order(state)]
	
	subdat <- dat[,c(2,7,col[outcome])]
	names(subdat) <- c("hospital","state","death")
	
	subdat$death <- as.numeric(subdat$death)
	subdat$hospital <- as.character(subdat$hospital)
	subdat <- subdat[complete.cases(subdat$death),]
	
	
	## return a list where the name of each item is state
	## and the value is the hospital name
		
	s = split(subdat, subdat$state) ## get a list of data frames ordered by state
	
	## process data in group, using lapply
	
	ls_hospital <- lapply(s, function(x, num) {
		x <- x[order(x$death, x$hospital, na.last= NA),]
		if (num =="best"){
			return(x$hospital[1])}
		else if (num =="worst"){
			return(x$hospital[nrow(x)])
		}
		else {
			return(x$hospital[num])}
	}, num)

	sta<-names(ls_hospital) ##get the state name
	ans <- cbind(unlist(ls_hospital), state=sta, rownames=sta) ## assemble the data
	ans <- as.data.frame(ans)
	colnames(ans) <- c("hospital","state")
	return(ans)
}