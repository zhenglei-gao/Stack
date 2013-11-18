#' Outlier test
#'
#' A simple test for outliers. This functions returns all extreme values (if any) found in the specified vector.
#'
#' @param x a numeric vector of values
#' @return vector of outlier values
#' @examples \dontrun{
#' rp.outlier(mtcars$hp)
#' rp.outlier(c(rep(1,100), 200))
#' rp.outlier(c(rep(1,100), 200,201))
#' }
#' @references {
#' Credit goes to PaulHurleyuk: \url{http://stackoverflow.com/a/1444548/564164}
#'
#' \itemize{
#'  \item Lund, R. E. 1975, "Tables for An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 4, pp. 473-476.
#'  \item Prescott, P. 1975, "An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 1, pp. 129-132.
#' }
#' }
#' @export
rp.outlier <- function(x) {
    if (!is.numeric(x)) stop('Wrong variable type (!numeric) provided.')

    lundcrit<-function(a, n, q) {
        ## Calculates a Critical value for Outlier Test according to Lund
        ## See Lund, R. E. 1975, "Tables for An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 4, pp. 473-476.
        ## and Prescott, P. 1975, "An Approximate Test for Outliers in Linear Models", Technometrics, vol. 17, no. 1, pp. 129-132.
        ## a = alpha
        ## n = Number of data elements
        ## q = Number of independent Variables (including intercept)
        ## --------------------------------------------------------------
        ## Credit goes to PaulHurleyuk: \url{http://stackoverflow.com/a/1444548/564164}
        F<-qf(c(1-(a/n)),df1=1,df2=n-q-1,lower.tail=TRUE)
        crit<-((n-q)*F/(n-q-1+F))^0.5
        crit
    }

    model <- lm(x ~ 1)
    crit <- suppressWarnings(lundcrit(0.1, length(x), model$coefficients))
    if (!is.na(crit))
        return(x[which(abs(rstandard(model)) > crit)])
    else
        return()
}











# some helpful threads
# https://stat.ethz.ch/pipermail/r-help/2008-September/172641.html
# http://tolstoy.newcastle.edu.au/R/e4/help/08/02/4875.html
# http://tolstoy.newcastle.edu.au/R/e2/help/07/01/8598.html

# http://www.r-statistics.com/wp-content/uploads/2011/01/boxplot-add-label-for-outliers.r.txt

# last updated: 2013-08-21: added require2 function (originally from the installr package)



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
  
  ##browser()
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
  
  
  # and put all the variables (x, y, and outlier label name) into one data.frame
  DATA <- data.frame(label_name, x ,y)
  
  if(!is.null(list(...)$names))	{	# if the user chose to use the names parameter, then we would like the function to still function (added on 19.04.2011)
    DATA$x <- factor(DATA$x, levels = unique(DATA$x))
    levels(DATA$x) = list(...)$names	# enable us to handle when the user adds the "names" parameter # fixed on 19.04.11	# notice that DATA$x must be of the "correct" order (that's why I used split above
    # warning("Careful, the use of the 'names' parameter is experimental.  If you notice any errors please e-mail me at: tal.galili@gmail.com")
  }
  
  if(!missing(data)) detach(data)	# we don't need to have "data" attached anymore.
  
  # let's only keep the rows with our outliers 
  boxplot.outlier.data <- function(xx, y_name = "y")
  {
    y <- xx[,y_name]
    boxplot_range <- range(boxplot.stats(y, coef = range )$stats)
    ss <- (y < boxplot_range[1]) | (y > boxplot_range[2])
    return(xx[ss,])	
  }
  outlier_df <-ddply(DATA, .(x), boxplot.outlier.data)
  
  
  # create propor x/y locations to handle over-laping dots...
  if(spread_text) {
    # credit: Greg Snow
    require2(TeachingDemos)		
    temp_x <- boxdata_outlier_df[,"x"]
    temp_y1 <- boxdata_outlier_df[,"y"]
    temp_y2 <- temp_y1
    for(i in unique(temp_x))
    {
      tmp <- temp_x == i
      temp_y2[ tmp ] <- spread.labs( temp_y2[ tmp ], 1.3*strheight('A'), maxiter=6000, stepsize = 0.05) #, min=0 )
    }
    
  }
  
  
  
  # max(strwidth(c("asa", "a"))
  # move_text_right <- max(strwidth(outlier_df[,"label_name"]))	
  
  # plotting the outlier labels :)  (I wish there was a non-loop wise way for doing this)
  for(i in seq_len(dim(boxdata_outlier_df)[1]))
  {
    # ss <- (outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & (outlier_df[,"y"] %in% boxdata_outlier_df[i,]$y)
    
    # if(jitter_if_duplicate) {
    # ss <- (outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & closest.number(outlier_df[,"y"]  boxdata_outlier_df[i,]$y)
    # } else {
    ss <- (outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & (outlier_df[,"y"] %in% boxdata_outlier_df[i,]$y)
    # }
    
    current_label <- outlier_df[ss,"label_name"]
    temp_x <- boxdata_outlier_df[i,"x"]
    temp_y <- boxdata_outlier_df[i,"y"]		
    # cbind(boxdata_outlier_df,		temp_y2)
    # outlier_df
    
    
    
    if(spread_text) {
      temp_y_new <- temp_y2[i] # not ss			
      move_text_right <- strwidth(current_label) * push_text_right
      text( temp_x+move_text_right, temp_y_new, current_label, col = label.col,cex=textcex)			
      # strwidth
      segments( temp_x+(move_text_right/6), temp_y, temp_x+(move_text_right*segement_width_as_percent_of_label_dist), temp_y_new )
    } else {
      text(temp_x, temp_y, current_label, pos = 4, col = label.col,cex=textcex)
    }		
  }
  
  # outputing some of the information we collected
  list(boxdata = boxdata, boxdata_outlier_df = boxdata_outlier_df, outlier_df=outlier_df)
}






########################################
### examples to see that it works

# library(plyr)
# library(TeachingDemos)
# source("http://www.r-statistics.com/wp-content/uploads/2011/01/boxplot-with-outlier-label-r.txt") # Load the function
# set.seed(210)
# n <- 20
# y <- rnorm(n)
# x1 <- sample(letters[1:3], n,T)
# lab_y <- sample(letters, n)
# boxplot.with.outlier.label(y~x1, lab_y, push_text_right = 1.5, range = .3)
# data.frame(y, x1, lab_y)

# set.seed(10)
# x2 <- sample(letters[1:3], n,T)
# boxplot.with.outlier.label(y~x1*x2, lab_y, push_text_right = 1.5, range = .3)
# data.frame(y, x1, x2, lab_y)




#' Read xlsx files
#'
#' @param file The path to xlsx file
#' @param keep_sheets A vector of sheet name
#' @param header Whether include the head in the sheet
#' @para empty_row Whether to remove the empty rows
#' @export
xlsxToR <- function(file, keep_sheets = NULL, header = TRUE, empty_row = TRUE)
{
  suppressWarnings(file.remove(tempdir()))
  file.copy(file, tempdir())
  new_file <- list.files(tempdir(), full.name = TRUE, pattern = basename(file))
  new_file_rename <- gsub("xlsx$", "zip", new_file)
  file.rename(new_file, new_file_rename)
  unzip(new_file_rename, exdir = tempdir())
  # Get OS
  mac <- readLines(paste0(tempdir(), "/docProps/app.xml"), warn = FALSE)
  mac <- grep("Macintosh", mac)
  if (length(mac) > 0)
  {
    os_origin <- "1899-12-30" # documentation says should be "1904-01-01"
  } else
  {
    os_origin <- "1899-12-30"
  }
  # Get names of sheets
  sheet_names_str <- readLines(paste0(tempdir(), "/xl/workbook.xml"), warn = FALSE)[2]
  sheet_names_str <- gsub('.*<sheets>(.*)</sheets>.*', '\\1', sheet_names_str)
  sheet_names_str <- strsplit(sheet_names_str, '/>')[[1]]
  sheet_names <- NULL
  sheet_names$name <- gsub('.* name="(.*)"( sheetId.*)', '\\1', sheet_names_str)
  sheet_names$sheetId <- gsub('.* sheetId="(.*)"( r:id.*)', '\\1', sheet_names_str)
  sheet_names$id <- gsub('.* r:id="(.*)"$', '\\1', sheet_names_str)
  sheet_names <- as.data.frame(sheet_names,stringsAsFactors = FALSE)
  sheet_names$id <- gsub("\\D", "", sheet_names$id)
  if(!is.null(keep_sheets))
  {
    sheet_names <- sheet_names[sheet_names$name %in% keep_sheets,]
  }
  entries <- readLines(paste0(tempdir(), "/xl/sharedStrings.xml"), warn = FALSE)[2]
  entries <- gsub('^<sst .*">(<si>.*)</sst>$', '\\1', entries)
  entries <- strsplit(entries, '</si>')[[1]]
  entries <- gsub('^.*<t>(.+)</t>$', '\\1', entries)
  names(entries) <- seq_along(entries) - 1
  
  # Get column classes
  styles <- readLines(paste0(tempdir(), '/xl/styles.xml'), warn = FALSE)[2]
  numFmtId <- gsub('^.*<cellXfs count="\\d+">(.*)</cellXfs>.*$', '\\1', styles)
  numFmtId <- strsplit(numFmtId, '/><xf')[[1]]
  numFmtId <- as.numeric(gsub('.*numFmtId="(\\d+)".*', '\\1', numFmtId))
  cell_style <- as.data.frame(list(id = seq(0, by = 1, along = numFmtId),
                                   numFmtId = numFmtId), stringsAsFactors = FALSE)
  # Custom style
  numFmt <- gsub('^.*<numFmts count="\\d+">(.*)</numFmts>.*$', '\\1', styles)
  if (length(numFmt) > 0)
  {
    numFmt <- strsplit(numFmt, '/><numFmt')[[1]]
    numFmt_cid <- as.numeric(gsub('.*numFmtId="(\\d+)".*', '\\1', numFmt))
    cid_type <- rep(NA, length(numFmt_cid))
    formatCode <- gsub('.*formatCode="(.*)".*', '\\1', numFmt)
    pos <- grep('y|m|d', formatCode)
    if (length(pos) > 0)
    {
      date_format <- formatCode[grep('y|m|d', formatCode)]
    }
    pos <- grep('h', date_format)
    if (length(pos) > 0)
    {
      date_format <- date_format[-pos]
    }  
    pos <- cell_style$numFmtId %in% numFmt_cid[formatCode %in% date_format]
    cell_style$numFmtId[pos] <- 14
  }
  worksheet_paths <- paste0(tempdir(), "/xl/worksheets/sheet",
                            sheet_names$id, '.xml')
  worksheets <- as.list(NULL)
  for (i in seq(along = worksheet_paths))
  {
    sheet_data <- readLines(worksheet_paths[i], warn = FALSE)[2]
    sheet_data <- gsub('(.*<sheetData>)(.*)(</sheetData>.*)', '\\2', sheet_data)
    sheet_data <- strsplit(sheet_data, '</row>')[[1]]
    sheet_data <- strsplit(sheet_data, '</c>')
    sheet_data <- unlist(sheet_data)
    sheet_data <- gsub('(.*<row.*>)(<c.*)', '\\2', sheet_data)
    res <- NULL
    res$r <- gsub('.*r="(\\w+\\d+)".*', '\\1', sheet_data)
    res$v <- rep(NA, length(sheet_data))
    pos <- grep('.*<v>(.*)</v>.*', sheet_data)
    res$v[pos] <- gsub('.*<v>(.*)</v>.*', '\\1', sheet_data[pos])
    res$s <- rep(NA, length(sheet_data))
    pos <- grep('.* s="(\\d+|\\w+)"( |>).*', sheet_data)
    res$s[pos] <- gsub('.* s="(\\d+|\\w+)"( |>).*', '\\1', sheet_data[pos])
    res$t <- rep(NA, length(sheet_data))
    pos <- grep('.* t="(\\d+|\\w+)"( |>).*', sheet_data)
    res$t[pos] <- gsub('.* t="(\\d+|\\w+)"( |>).*', '\\1', sheet_data[pos])
    res <- as.data.frame(res, stringsAsFactors = FALSE)
    res$sheet <- sheet_names[sheet_names$id == i, 'name']
    entries_match <- entries[match(res$v, names(entries))]
    res$v[res$t == "s" & !is.na(res$t)] <-
      entries_match[res$t == "s"& !is.na(res$t)]
    res$cols <- match(gsub("\\d", "", res$r), LETTERS)
    res$rows <- as.numeric(gsub("\\D", "", res$r))
    nrow <- max(res$rows)
    ncol <- max(res$cols)
    if (header)
    {
      nrow <- nrow - 1
    }
    res_df <- as.data.frame(matrix(rep(NA, ncol * nrow), ncol = ncol),
                            stringsAsFactors = FALSE)
    style_df <- as.data.frame(matrix(rep(NA, ncol * nrow), ncol = ncol),
                              stringsAsFactors = FALSE)
    if (header)
    {
      header_df <- res[res$rows == 1,]
      header_name <- paste0('V', seq(ncol))
      header_name[header_df$cols] <- header_df$v
      names(res_df) <- header_name
      res <- res[res$rows != 1,]
      res$rows <- res$rows - 1
    }
    if (nrow(res) > 0)
    {
      res_df[as.matrix(res[,c('rows', 'cols')])] <- res$v
      s <- as.numeric(res$s)
      s <- cell_style$numFmtId[match(s, cell_style$id)]
      style_df[as.matrix(res[,c('rows', 'cols')])] <- s
    }
    style_df <- sapply(style_df, function(x)
    {
      ifelse(length(unique(x[!is.na(x)])) == 1, unique(x), NA)
    })
    style_col <- rep('character', length(style_df))
    style_col[style_df %in% 14:17] <- "date"
    style_col[style_df %in% c(18:21, 45:47)] <- "time"
    style_col[style_df %in% 22] <- "datetime"
    style_col[is.na(style_df) & !sapply(res_df, function(x) any(grepl("\\D", x)))] <- "numeric"
    res_df[] <- lapply(seq_along(res_df), function(i)
    {
      switch(style_col[i],
             character = res_df[,i],
             numeric = as.numeric(res_df[,i]),
             date = as.Date(as.numeric(res_df[,i]), origin = os_origin),
             time = strftime(as.POSIXct(as.numeric(res_df[,i]), origin = os_origin), format = "%H:%M:%S"),
             datetime = as.POSIXct(as.numeric(res_df[,i]), origin = os_origin))
    })
    if (empty_row)
    {
      pos <- apply(res_df, 1, function(x) sum(is.na(x))) != ncol(res_df)
      if (ncol(res_df) == 1)
      {
        col_name <- names(res_df)
        res_df <- data.frame(res_df[pos,])
        names(res_df) <- col_name
      } else
      {
        res_df <- res_df[pos,]
      }
    }
    sheet_n <- sheet_names$name[sheet_names$id == i]
    worksheets[[sheet_n]] <- res_df
  }
  if(length(worksheets) == 1)
  {
    worksheets <- worksheets[[1]]
  }
  worksheets
}


panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="na.or.complete"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}
panel.lm<-function (x, y, col = "blue", bg = NA, pch = 18,
            cex = 1, col.lm = "red", lwd=par("lwd"),diag=FALSE, ...)
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      {abline(lm(y~x,subset=ok), col = col.lm, ...)
       if(diag==TRUE) abline(c(0,0),c(1,1),col="green")}
  }
panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


