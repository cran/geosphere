
#.onLoad <- function(lib, pkg)  {
#	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
#	cat(paste(pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")\n", sep=""))
#	library.dynam("geosphere", pkg)	
#	return(invisible(0))
#}

