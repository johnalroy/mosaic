summary.mosaic<-function(m)	{

	g <- m$graph
	cat('\narea estimate: ',m$area,'\n\n',nrow(as.matrix(g)),' vertices, ',sum(g,na.rm=T),' edges\n\n',sep='');
	cat('edge counts\n\n')
	t <- table(rowSums(as.matrix(g),na.rm=T))
	for (i in 1:length(t))
		cat(' ',as.numeric(names(t[i])),': ',t[i],'\n',sep='')
	cat('\n')

}
