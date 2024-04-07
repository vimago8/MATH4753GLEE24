#' MySample
#'
#' @param n number of trials
#' @param iter number of iterations
#' @param time time, in seconds, between each iteration
#'
#' @return none, barplots printed to console
#' @export
#'
#' @examples
#' \dontrun{mysample(n=1000, iter=30, time=1)}
#'
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    #release the table
    Sys.sleep(time)
  }
}
