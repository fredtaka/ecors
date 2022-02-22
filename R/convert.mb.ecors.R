
#abc<-convert.mb.ecors(x=mb2000_2010,old.value = c(1,3,4,10,12),new.value = c(1,1,1,1,1), new.lu.class = c("native","native","native","native","native"))

convert.mb.ecors<-function(x,old.value,new.value,new.lu.class,new.color=NULL){
  if(class(x)!="mb.ecors"){stop("Argument x must be a mb.ecors class object.")}

  mb.prov<-eval(parse(text=x$object.name))

  if(length(old.value)!=length(new.value)){stop("old.value and new.value have different length")}

  if(is.null(new.lu.class)){new.lu.class<-rep(NA,length(new.value))}

  if(length(new.value)!=length(new.lu.class)){stop("new.value and new.lu.class have different length")}

  if(is.null(new.color)==F){
    if(length(new.value)!=length(new.color)){stop("new.value and new.color have different length")}
  }


  x$legend.mb$original.value<-x$legend.mb$pixel.value
  x$legend.mb$original.lu.class<-x$legend.mb$lu.class

  #create new legend
  for (i in 1:length(old.value)){
    x$legend.mb$pixel.value[x$legend.mb$original.value==old.value[i]]<-new.value[i]
    x$legend.mb$lu.class[x$legend.mb$original.value==old.value[i]]<-new.lu.class[i]
    if(is.null(new.color)==F){
      x$legend.mb$color[x$legend.mb$original.value==old.value[i]]<-new.color[i]}
  }

  if(is.null(new.color)){
    for(i in unique(x$legend.mb$pixel.value)){
      x$legend.mb$color[x$legend.mb$pixel.value==i]<-x$legend.mb$color[x$legend.mb$pixel.value==i][1] #pick the first value for all values of same category
    }
  }

  #create new palette
  x$palette.mb<-select(x$legend.mb,color,pixel.value,lu.class)
  x$legend.mb<-select(x$legend.mb,-color) #color do not make sense anymore

  for (i in 0:max(x$palette.mb$pixel.value)) {#preenchendo vazios na sequencia de valores de pixel
    if(i%in%x$palette.mb$pixel.value ==F){x$palette.mb<-rbind(x$palette.mb,data.frame(color="#ffffff",pixel.value=i,lu.class=paste0("NA",i)))}
  }
  x$palette.mb<-x$palette.m[!duplicated(x$palette.mb),]

  x$palette.mb<-x$palette.mb%>%arrange(pixel.value)

  list.years<-paste0("classification_",x$years)

  originals<-x$legend.mb$original.value
  target<-x$legend.mb$pixel.value

  if(length(x$years) == 1){
    mb.conv<-mb.prov$remap(
      from=originals,
      to=target,
      bandName=list.years)
    mb.conv<-mb.conv$rename(list.years)#checar se comporta o paste0("classification_",x$years)
  }

  if(length(x$years) > 1){
    mb.conv<-mb.prov$select(list.years[1])$rename("temp")

    for (i in list.years){

      prov<-mb.prov$remap(
        from=originals,
        to=target,
        bandName=i)
      prov<-prov$rename(i)
      mb.conv<-mb.conv$addBands(prov)
    }
    mb.conv<-mb.conv$select(list.years)

  }
  #TODO: replace R loop for Google Earth Engine map. It is not so easy as looks like.

  mb.conv<<-mb.conv

  cat(paste("\nObject mb.conv was exported to .GlobalEnv \n"))

  #update additional metadata
  x$object.name<-"mb.conv"

  if(x$post.processing=="none"){
    x$post.processing<-"converted pixel values"
  } else {x$post.processing<-c(x$post.processing,"converted pixel values")}

  return(x)

}
