#'vinplot code
#'@param nr,nc
#'@return image
#'@export


package_binplot <- function(x,y,nr=20,nc=20, scale="raw") {

  x = rnorm(10000) ; y = rnorm(10000)


  zx = c(1:nr,rep(1,nc),1+trunc( nr*(x- min(x))/(max(x)-min(x)) ))
  zx[zx>nr] = nr
  zy = c(rep(1,nr),1:nc,1+trunc( nc*(y- min(y))/(max(y)-min(y)) ))
  zy[zy>nc] = nc
  z = table(zx,zy); z[,1]=z[,1]-1; z[1,]=z[1,]-1;
  if (scale=="l") {z= log(1+z)}
  image(z=t(z),x=seq(length=nr+1,from=min(x),to=max(x)),
        y= seq(length=nc+1,from=min(y),to=max(y)),
        xlab="",ylab="", col=topo.colors(100))
}

#'megaplot
#'@export
x = pima[,2]; y= pima[,3]; superplot(x,y)
##MEGA PLOT
megaplot = function(x,y) {
  xhist <- hist(x, plot=FALSE)
  yhist <- hist(y, plot=FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  xrange <- range(x)
  yrange <- range(y)
  nf <- layout(matrix(c(2,1,0,3),2,2,byrow=TRUE), c(1,3), c(3,1), TRUE) layout.show(nf)
  par(mar=c(1,1,1,1))
  plot(x, y, xlim=xrange, ylim=yrange, xlab="", ylab="") par(mar=c(1,1,1,1))
  barplot(yhist$counts, xlim=c(top, 0), space=0, horiz=TRUE) # ! par(mar=c(1,1,1,1))
  barplot(xhist$counts, ylim=c(top,0), space=0) #!ylim!
  invisible()
}


#'test Square a value
#'@param a vlaue
#'@return the square value
#'@export

package_square<-function(x){
  return (x^2)
}


