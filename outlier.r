rm(list=ls())
require2 <- function (package, ask = TRUE, ...) 
{
  package <- as.character(substitute(package))
  if (!suppressWarnings(require(package = package, character.only = TRUE))) {
    install_package <- ask.user.yn.question(paste("Package ", 
                                                  package, " is not installed. Do you want to install it now?"))
    if (install_package) 
      install.packages(pkgs = package)
  }
  require(package = package, character.only = TRUE)
}






boxplot.with.outlier.label <- function(y, label_name, ..., spread_text = T, data, plot = T, range = 1.5, label.col = "blue", push_text_right = 1.5, # enlarge push_text_right in order to push the text labels further from their point
                                       segement_width_as_percent_of_label_dist = .45, # Change this if you want to have the line closer to the label (range should be between 0 to 1
                                       jitter_if_duplicate = T, jitter_only_positive_duplicates = F,textcex=1)
{  
  # change log:
  # 19.04.2011 - added support to "names" and "at" parameters.
  
  
  # jitter_if_duplicate - will jitter (Actually just add a bit of numbers) so to be able to decide on which location to plot the label when having identical variables...
  require2(plyr) # for is.formula and ddply
  
  # a function to jitter data in case of ties in Y's
  jitter.duplicate <- function(x, only_positive = F)
  {
    if(only_positive) {
      ss <- x > 0
    } else {
      ss <- T
    }	
    ss_dup <- duplicated(x[ss])
    # ss <- ss & ss_dup
    temp_length <- length(x[ss][ss_dup])	
    x[ss][ss_dup] <- x[ss][ss_dup] + seq(from = 0.00001, to = 0.00002, length.out = temp_length)
    x
  }
  # jitter.duplicate(c(1:5))
  # jitter.duplicate(c(1:5,5,2))
  # duplicated(jitter.duplicate(c(1:5,5,2)))
  # jitter.duplicate(c(0,0,1:5,5,2))
  # duplicated(jitter.duplicate(c(0,0,1:5,5,2)))
  
  
  
  # handle cases where 
  if(jitter_if_duplicate) {
    # warning("duplicate jutter of values in y is ON")
    if(!missing(data)) {	#e.g: we DO have data
      # if(exists("y") && is.formula(y)) {		# F && NULL # F & NULL
      y_name <- as.character(substitute(y))	# I could have also used as.list(match.call())
      # credit to Uwe Ligges and Marc Schwartz for the help
      # https://mail.google.com/mail/?shva=1#inbox/12dd7ca2f9bfbc39
      if(length(y_name) > 1) {	# then it is a formula (for example: "~", "y", "x"
        model_frame_y <- model.frame(y, data = data)
        temp_y <- model_frame_y[,1]
        temp_y  <- jitter.duplicate(temp_y, jitter_only_positive_duplicates)	# notice that the default of the function is to work only with positive values...
        # the_txt <- paste(names(model_frame_y)[1], "temp_y", sep = "<<-") # wrong...
        the_txt <- paste("data['",names(model_frame_y)[1],"'] <- temp_y", sep = "")				
        eval(parse(text = the_txt))	# jutter out y var so to be able to handle identical values.
      } else {	# this isn't a formula
        data[,y_name] <- jitter.duplicate(data[,y_name], jitter_only_positive_duplicates)
        y <- data[,y_name]	# this will make it possible for boxplot(y, data) to work later (since it is not supposed to work with data when it's not a formula, but now it does :))
      }		
    } else {	# there is no "data"		 
      if(is.formula(y)) { # if(exists("y") && is.formula(y)) {		# F && NULL # F & NULL
        temp_y <- model.frame(y)[,1]
        temp_y  <- jitter.duplicate(temp_y, jitter_only_positive_duplicates)	# notice that the default of the function is to work only with positive values...
        temp_y_name <- names(model.frame(y))[1]	# we must extract the "names" before introducing a new enbironment (or there will be an error)
        environment(y) <- new.env()
        assign(temp_y_name, temp_y, environment(y))
        # Credit and thanks for doing this goes to Niels Richard Hansen (2 Jan 30, 2011)
        # http://r.789695.n4.nabble.com/environment-question-changing-variables-from-a-formula-through-model-frame-td3246608.html
        # warning("Your original variable (in the global environemnt) was just jittered.")	# maybe I should add a user input before doing this....
        # the_txt <- paste(names(model_frame_y)[1], "temp_y", sep = "<<-")
        # eval(parse(text = the_txt))	# jutter out y var so to be able to handle identical values.
      } else {
        y <- jitter.duplicate(y, jitter_only_positive_duplicates)
      }		
    }
  }
  # the_txt <- paste("print(",names(model_frame_y)[1], ")")
  # eval(parse(text = the_txt))	# jutter out y var so to be able to handle identical values.
  # print(ls())
  
  
  # y should be a formula of the type: y~x, y~a*b
  # or it could be simply y
  extreme <- NULL
  if(length(range) > 1){
    extreme <- range[2]
    range <- range[1]
  }
  bxp.extreme<-function(bp, extreme=2.5) {
    #function to identify 'extreme' outliers in a boxplot
    #z is a boxplot object returned by boxplot(... , plot=FALSE)
    #Returns a vector of length equal to length(z$out) with TRUE where
    ##outliers are outside
    #box ends by more than extreme * (interquartile range)
    boxrange <- bp$stats[4,bp$group] - bp$stats[2,bp$group]
    big.outlier<- (bp$out >  bp$stats[4,bp$group] + extreme*boxrange) |
      (bp$out <  bp$stats[2,bp$group] - extreme*boxrange)
    return(big.outlier)
  }
  
  
  
 
  if(missing(data)) {
    boxdata <- boxplot(y, plot = plot,range = range ,outpch=NA,...)
    if(!is.null(extreme) && plot){
      ext <- bxp.extreme(boxdata,extreme)
      points(boxdata$group, boxdata$out, pch=ifelse(ext, 2,1), col=ifelse(ext, 2,1))
    }
  } else {
    boxdata <- boxplot(y, plot = plot,data = data, range = range ,outpch=NA,...)
    if(!is.null(extreme) && plot){
      ext <- bxp.extreme(boxdata,extreme)
      points(boxdata$group, boxdata$out, pch=ifelse(ext, 2,1), col=ifelse(ext, 2,1))
    }
  }
  if(length(boxdata$names) == 1 && boxdata$names =="") boxdata$names <- 1	# this is for cases of type: boxplot(y) (when there is no dependent group)
  if(length(boxdata$out) == 0 ) {
    warning("No outliers detected for this boxplot")
    return(invisible())
  }
  
  if(!missing(data)) attach(data)	# this might lead to problams I should check out for alternatives for using attach here...
  
  
  # creating a data.frame with information from the boxplot output about the outliers (location and group)
  boxdata_group_name <- factor(boxdata$group)
  levels(boxdata_group_name) <- boxdata$names[as.numeric(levels(boxdata_group_name))]	# the subseting is for cases where we have some sub groups with no outliers
  if(!is.null(list(...)$at))	{	# if the user chose to use the "at" parameter, then we would like the function to still function (added on 19.04.2011)
    boxdata$group <- list(...)$at[boxdata$group]		
  }
  boxdata_outlier_df <- data.frame(group = boxdata_group_name, y = boxdata$out, x = boxdata$group)
  
  
  #browser()
  # Let's extract the x,y variables from the formula:
  if(is.formula(y))
  {
    model_frame_y <- model.frame(y)
    # old solution: (which caused problems if we used the names parameter when using a 2 way formula... (since the order of the names is different then the levels order we get from using factor)
    # y <- model_frame_y[,1]
    # x <- model_frame_y[,-1]
    
    y <- model_frame_y[,1]
    x <- model_frame_y[,-1]
    if(!is.null(dim(x))) {	# then x is a matrix/data.frame of the type x1*x2*..and so on - and we should merge all the variations...
      x <- apply(x,1, paste, collapse = ".")
    }
  } else {
    # if(missing(x)) x <- rep(1, length(y))
    x <- rep(1, length(y))	# we do this in case y comes as a vector and without x
  }	
  
  browser()
  # and put all the variables (x, y, and outlier label name) into one data.frame
  DATA <- data.frame(label_name, x ,y)
  
  return(DATA)
}


boxplot2 <- function(y,data,label_name,plot=TRUE,range=1.5,...){
  if(missing(data)){
    boxplot(y,...)
  }
  ## boxdata <- boxplot(y, plot = plot,range = range ,outpch=NA,...)
  if(is.formula(y))
  {
    model_frame_y <- model.frame(y)
    y <- model_frame_y[,1]
    x <- model_frame_y[,-1]
    if(!is.null(dim(x))) {  # then x is a matrix/data.frame of the type x1*x2*..and so on - and we should merge all the variations...
      x <- apply(x,1, paste, collapse = ".")
    }
  } else {
    # if(missing(x)) x <- rep(1, length(y))
    x <- rep(1, length(y))  # we do this in case y comes as a vector and without x
  }	
  
  
  # and put all the variables (x, y, and outlier label name) into one data.frame
  DATA <- data.frame(label_name, x ,y)
  return(DATA)
}
ana <- function(dta){
  for(i in 2:ncol(dta)){
    id <- as.logical(rbinom(nrow(dta),1,0.9))
    windows()
    par(mfrow=c(1,3))
    boxplot(dta[id,i]~dta$Study[id],main=paste(i,"-1"))
    boxplot2(dta[id,i]~dta$Study[id],main=paste(i,"-2"),label_name=paste(dta$Study[id],dta[id,i]))
   boxplot.with.outlier.label(dta[id,i]~dta$Study[id],label_name=paste(dta$Study[id],dta[id,i]),push_text_right = 0.65,main=paste(i,"-3"),segement_width_as_percent_of_label_dist = .2,textcex=0.85,range=c(1.5,2.5))
    
   # 
  }
}

dta <- list()

dta$Study <- factor(c(rep("S1",50),rep("S2",50)))
dta1 <- do.call("cbind",(lapply(1:5,function(i)rnorm(100))))
dta <- data.frame(dta,dta1)
ana(dta)
