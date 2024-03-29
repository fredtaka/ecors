#' Descritive statistics on ecors objects
#'
#' Performs descriptive statistics on study polygons at defined sampling periods.
#'
#' @param x ecors object (from get.ecors).
#' @param edge.pixels choose the treatment for pixels that coincide with the edges of the polygons: "weighted" uses the value of these pixels in the descriptive statistic weighted by the proportion of the area inside the polygon, "centroid" ignores pixels with the centroid outside the polygon.
#' @param remove.samples removes sample units that were misrepresented due to bad pixel removal. Minimum number of good pixels (num.pixelOK) and/or minimum proportion of good pixels (prop.pixelOK) can be selected.
#' @param summarizing selects whether the data will be summarized by year ("yearly") or for the entire evaluated period ("all"). The "yearly" option considers a year as every 12 months starting from the initial month of image collection.
#' @param by.image.save save a csv file with descriptive statistics for each image?
#' @param summary.save save a csv file with descriptive statistics summarizing all the images?
#' @param stats enables/disables the calculation of mean, median, standard deviation (sd) or count (of pixels in the range of values between lower.cutoff and upper.cutoff).
#' @param lower.cutoff Lower threshold values to ignore pixels in all stats (required for count). It must be a vector with a size equal to the number of bands/indexes to be analyzed.
#' @param upper.cutoff Upper threshold values to ignore pixels in all stats (required for count). It must be a vector with a size equal to the number of bands/indexes to be analyzed.
#' @param bands.index.subset Subset of bands to use for statistics.
#' @param spreadsheet.folder local folder to save csv files.
#'
#' @return
#' List with result tables and metadata. If enabled, csv files will be saved with tables (two for each descriptive statistics): results.mean.samples.csv, summary.mean.samples.csv, results.median.samples.csv, summary.median.samples.csv, results.sd.samples.csv, summary.sd.samples.csv, results.count.samples.csv, summary.count.samples.csv.
#'
#' @references
#' https://developers.google.com/earth-engine/guides/reducers_weighting
#'
#' @examples
#' #get a ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#'
#' #library(ecors)
#' d2020<-get.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots, buffer.points=500, buffer.plots=500,
#'     eval.area="site", projected=F, custom.crs=32723,
#'     collection="LANDSAT/LC08/C02/T1_L2", start=c("2020-01-01"), end=c("2020-12-31"),
#'     bands.eval=c("SR_B3","SR_B4"), bands.vis=F, indices=c("NDVI"), resolution=30,
#'     pOK=0.3, c.prob=NULL, c.dist=100, clouds.sentinel=NULL, cirrus.threshold=NULL, NIR.threshold=NULL, CDI.threshold=NULL, dmax.shadow=NULL,
#'     seasons=list(s1=c(11,12,1,2), s2=c(3,4), s3=c(5,6,7,8), s4=c(9,10)), group.by="season", composite=NULL)
#'
#' allpixels<-stats.ecors(x=d2020, edge.pixels="weighted", remove.samples=list(num.pixelOK=10,prop.pixelOK=0.8),
#'                   summarizing="all", by.image.save=T, summary.save=T,
#'                   stats=list(mean=T,median=F,sd=F,count=F),spreadsheet.folder=getwd() )
#'
#' rangepixels<-stats.ecors(x=d2020, edge.pixels="weighted", remove.samples=list(num.pixelOK=10,prop.pixelOK=0.8),
#'                   summarizing="all", by.image.save=T, summary.save=T,
#'                   stats=list(mean=T,median=F,sd=F,count=T), lower.cutoff=c(9000,8500), upper.cutoff=c(10000,10000), bands.index.subset=c("SR_B3","SR_B4"),
#'                   spreadsheet.folder=getwd() )
#'
#'
#' @export
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr

stats.ecors<-function(x, edge.pixels="weighted", remove.samples=list(num.pixelOK=NULL,prop.pixelOK=0.9),
                      summarizing="all", by.image.save=T, summary.save=T,
                      stats=list(mean=T,median=T,sd=T,count=F), lower.cutoff=NULL, upper.cutoff=NULL, bands.index.subset=NULL, spreadsheet.folder=getwd() ){


  if(class(x)!="ecors"){stop("Argument x must be a ecors class object.")}

  list2env(x,envir=environment())

  if(is.null(x$samples.gee)){stop("Argument x (ecors object) do not contain samples.")}

  if(substr(spreadsheet.folder,nchar(spreadsheet.folder),nchar(spreadsheet.folder))=="/"){spreadsheet.folder<-substr(spreadsheet.folder,1,nchar(spreadsheet.folder)-1)}

  if (summarizing=="yearly" & is.null(composite)==F){stop("Annual summarization of composited images makes no sense: two different (and incompatible) ways of summarizing season or month data. \nUse summarizing=\"all\" or rerun get.ecors function with composite=NULL")}
  bands.eval.indices<-c(bands.eval,indices)

  if(is.null(bands.index.subset)==F){
    if(sum(bands.index.subset%in%bands.eval.indices)<length(bands.index.subset)){stop("Selected bands in bands.index.subset are not selected when run get.ecors.")}
    bands.eval.indices<-bands.index.subset
  }
  if(stats$count==T & (is.null(lower.cutoff) | is.null(upper.cutoff))){stop("To use the count statistic, you need to select lower.cutoff and upper.cutoff values for each band and index.")}
  if(stats$count==T & (length(bands.eval.indices)!=length(lower.cutoff) | length(bands.eval.indices)!=length(upper.cutoff) )){
    stop("To use the count statistic, you need to select a lower.cutoff and upper.cutoff for each band and index.")}
  if(length(lower.cutoff)!=length(upper.cutoff)){stop("Length of lower.cutoff and upper.cutoff vectors need to be equal.")}
  if(length(lower.cutoff)>5) {stop("Currently only up to 5 lower.cutoff and upper.cutoff pairs of values are supported (for up to 5 bands or indices).")}

  if((edge.pixels%in%c("weighted","centroid"))==F){stop("Argument edge.pixels must be centroid or weighted.")}

  #para calcular estatísticas só nas bandas e índices de interesse e atualizando n.imagens (só faz sentido para imagens com máscara)
  if(is.null(composite)){
    colle.stat<-colle.mask$select(bands.eval.indices)
    n.imagens.est<-nrow(images.table)} else{
      colle.stat<-colle.mask.compo$select(bands.eval.indices)
      n.imagens.est<-colle.mask.compo$size()$getInfo()}

  listbands<-colle.stat[[1]]$bandNames()$getInfo()
  listbands1<-listbands[1]
  pixels1<-colle.stat[[1]]$select(listbands1)$unmask()$lt(1000000000)$rename("npixels") # camada de pixels = 1 (necessário para contabilizar pixels dentro de sample quando opção "ponderado")
  # nome da banda fará sentido na tabela produzida adiante: tab.npixels.samples
  pixels1.masca<-colle.stat$map(function(imagem){
    return( imagem<-imagem$select(listbands1)$lt(1000000000)$rename("npixelsOK") )#nome fará sentido na tabela produzida: tab.npixelsOK.samples
  })

  if(is.null(lower.cutoff)==F){

    colle.stat.full.range<-colle.stat
    pixels1.masca.full.range<-pixels1.masca


    if(length(lower.cutoff) == 1){
      colle.stat<-colle.stat$map(function(imagem){
        imgmask<-imagem$select(listbands[1])$gt(lower.cutoff[1])$multiply(imagem$select(listbands[1])$lt(upper.cutoff[1]))
        imagem<-imagem$updateMask(imgmask)
      })
    }
    if(length(lower.cutoff) == 2){
      colle.stat<-colle.stat$map(function(imagem){
        imgmask1<-imagem$select(listbands[1])$gt(lower.cutoff[1])$multiply(imagem$select(listbands[1])$lt(upper.cutoff[1]))
        imgmask2<-imagem$select(listbands[2])$gt(lower.cutoff[2])$multiply(imagem$select(listbands[2])$lt(upper.cutoff[2]))
        imgmask<-imgmask1$multiply(imgmask2)
        imagem<-imagem$updateMask(imgmask)
      })
    }
    if(length(lower.cutoff) == 3){
      colle.stat<-colle.stat$map(function(imagem){
        imgmask1<-imagem$select(listbands[1])$gt(lower.cutoff[1])$multiply(imagem$select(listbands[1])$lt(upper.cutoff[1]))
        imgmask2<-imagem$select(listbands[2])$gt(lower.cutoff[2])$multiply(imagem$select(listbands[2])$lt(upper.cutoff[2]))
        imgmask3<-imagem$select(listbands[3])$gt(lower.cutoff[3])$multiply(imagem$select(listbands[3])$lt(upper.cutoff[3]))
        imgmask<-imgmask1$multiply(imgmask2)$multiply(imgmask3)
        imagem<-imagem$updateMask(imgmask)
      })
    }
    if(length(lower.cutoff) == 4){
      colle.stat<-colle.stat$map(function(imagem){
        imgmask1<-imagem$select(listbands[1])$gt(lower.cutoff[1])$multiply(imagem$select(listbands[1])$lt(upper.cutoff[1]))
        imgmask2<-imagem$select(listbands[2])$gt(lower.cutoff[2])$multiply(imagem$select(listbands[2])$lt(upper.cutoff[2]))
        imgmask3<-imagem$select(listbands[3])$gt(lower.cutoff[3])$multiply(imagem$select(listbands[3])$lt(upper.cutoff[3]))
        imgmask4<-imagem$select(listbands[4])$gt(lower.cutoff[4])$multiply(imagem$select(listbands[4])$lt(upper.cutoff[4]))
        imgmask<-imgmask1$multiply(imgmask2)$multiply(imgmask3)$multiply(imgmask4)
        imagem<-imagem$updateMask(imgmask)
      })
    }
    if(length(lower.cutoff) == 5){
      colle.stat<-colle.stat$map(function(imagem){
        imgmask1<-imagem$select(listbands[1])$gt(lower.cutoff[1])$multiply(imagem$select(listbands[1])$lt(upper.cutoff[1]))
        imgmask2<-imagem$select(listbands[2])$gt(lower.cutoff[2])$multiply(imagem$select(listbands[2])$lt(upper.cutoff[2]))
        imgmask3<-imagem$select(listbands[3])$gt(lower.cutoff[3])$multiply(imagem$select(listbands[3])$lt(upper.cutoff[3]))
        imgmask4<-imagem$select(listbands[4])$gt(lower.cutoff[4])$multiply(imagem$select(listbands[4])$lt(upper.cutoff[4]))
        imgmask5<-imagem$select(listbands[5])$gt(lower.cutoff[5])$multiply(imagem$select(listbands[5])$lt(upper.cutoff[5]))
        imgmask<-imgmask1$multiply(imgmask2)$multiply(imgmask3)$multiply(imgmask4)$multiply(imgmask5)
        imagem<-imagem$updateMask(imgmask)
      })
    }
    pixels1.masca<-colle.stat$map(function(imagem){ #modificando para refletir mudança na máscara pelo lower.cutoff and upper.cutoff: original ficou como pixels1.masca.full.range
      return( imagem<-imagem$select(listbands1)$lt(1000000000)$rename("npixelsOK") )#nome fará sentido na tabela produzida: tab.npixelsOK.samples
    })

    cat("\n\nAll statistics will be calculated on pixels of values > lower.cutoff and < upper.cutoff (except pixel quality indicators)\n\n")

  }


  if (edge.pixels=="weighted"){

    if(stats$mean==T){
      cat("\nProcessing means on",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.mean.samples<-ee_extract(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$mean())
    }

    if(stats$median==T){
      cat("\nProcessing medians on",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.median.samples<-ee_extract(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$median())
    }

    if(stats$sd==T){
      cat("\nProcessing sd on",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.sd.samples<-ee_extract(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$stdDev())
    }

    if(stats$count==T){
      cat("\nCounting pixels on",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.count.samples<-ee_extract(x=pixels1.masca,y=samples.gee,scale=resolution,fun=ee$Reducer$sum())

      cat("\nProcessing proportion of OK pixels in each sample of",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.npixels.samples<-ee_extract(x=pixels1,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()) #valores na imagem de pixels1
      tab.npixelsOK.samples<-ee_extract(x=pixels1.masca.full.range,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()) #valores na coleção (pixels1 com múltiplas máscaras)
    }
    if(stats$count==F){
      cat("\nProcessing proportion of OK pixels in each sample of",n.imagens.est,"images (method: weighted). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.npixels.samples<-ee_extract(x=pixels1,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()) #valores na imagem de pixels1
      tab.npixelsOK.samples<-ee_extract(x=pixels1.masca,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()) #valores na coleção (pixels1 com múltiplas máscaras)
    }
  }


  if (edge.pixels=="centroid"){ #emprega o workaround do ee_extract2 -> adaptado para funcionar com $unweighted -> checar se válido/necessário nas novas versões do rgee (atsamplesl é versão 1.0.9)

    if(stats$mean==T){
      cat("\nProcessing means on",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.mean.samples<-ee_extract2(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$mean()$unweighted(),fun_name="mean")
    }

    if(stats$median==T){
      cat("\nProcessing medians on",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.median.samples<-ee_extract2(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$median()$unweighted(),fun_name="median")
    }

    if(stats$sd==T){
      cat("\nProcessing sd on",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.sd.samples<-ee_extract2(x=colle.stat,y=samples.gee,scale=resolution,fun=ee$Reducer$stdDev()$unweighted(),fun_name="stdDev")
    }

    if(stats$count==T){
      cat("\nCounting pixels (of values > lower.cutoff and < upper.cutoff) on",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.count.samples<-ee_extract2(x=pixels1.masca,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()$unweighted(),fun_name="sum")

      cat("\nProcessing proportion of OK pixels in each sample of",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.npixels.samples<-ee_extract2(x=pixels1,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()$unweighted(),fun_name="sum") #valores na imagem de pixels1
      tab.npixelsOK.samples<-ee_extract2(x=pixels1.masca.full.range,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()$unweighted(),fun_name="sum") #valores na coleção (pixels1 com múltiplas máscaras)
    }
    if(stats$count==F){
      cat("\nProcessing proportion of OK pixels in each sample of",n.imagens.est,"images (method: centroid). Started at",format(Sys.time(),"%H:%M"),"\n")
      tab.npixels.samples<-ee_extract2(x=pixels1,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()$unweighted(),fun_name="sum") #valores na imagem de pixels1
      tab.npixelsOK.samples<-ee_extract2(x=pixels1.masca,y=samples.gee,scale=resolution,fun=ee$Reducer$sum()$unweighted(),fun_name="sum") #valores na coleção (pixels1 com múltiplas máscaras)
    }
  }


  #pixelsOK
  prov.total<-matrix(rep(tab.npixels.samples[,ncol(tab.npixels.samples)],times=n.imagens.est),ncol=n.imagens.est)
  prov.OK<-as.matrix(tab.npixelsOK.samples[,c((ncol(tab.npixelsOK.samples)-n.imagens.est+1):ncol(tab.npixelsOK.samples))])
  tab.propOK.samples<-tab.npixelsOK.samples #aproveitando estrutura inicial para preenchimento
  names(tab.propOK.samples)<-gsub(pattern="npixelsOK",replacement="propOK",x=names(tab.propOK.samples))
  tab.propOK.samples[,c((ncol(tab.npixelsOK.samples)-n.imagens.est+1):ncol(tab.npixelsOK.samples))]<-prov.OK/prov.total
  tab.OK.samples<-merge(tab.npixelsOK.samples,tab.propOK.samples,suffixes=c(),sort=F)
  tab.OK.samples<-select(tab.OK.samples,"id","type",sort(names(tab.OK.samples)[!names(tab.OK.samples)%in%c("id","type")]))
  tab.OK.samples$get.ecor.date.time<-as.character(get.ecor.date.time)

  #prop count
  if(stats$count==T){
    names(tab.count.samples)<-gsub(pattern="npixelsOK", replacement="count", x=names(tab.count.samples))#ajuste no nome (para diferenciar do pixelsOK)
    prov.count<-as.matrix(tab.count.samples[,c((ncol(tab.count.samples)-n.imagens.est+1):ncol(tab.count.samples))])
    tab.prop.samples<-tab.count.samples #aproveitando estrutura inicial para preenchimento
    names(tab.prop.samples)<-gsub(pattern="count",replacement="prop",x=names(tab.count.samples))
    tab.prop.samples[,c((ncol(tab.count.samples)-n.imagens.est+1):ncol(tab.count.samples))]<-prov.count/prov.OK
    tab.count.prop.samples<-merge(tab.count.samples,tab.prop.samples,suffixes=c(),sort=F)
    tab.count.prop.samples<-select(tab.count.prop.samples,"id","type",sort(names(tab.count.prop.samples)[!names(tab.count.prop.samples)%in%c("id","type")]))
    tab.count.prop.samples$get.ecor.date.time<-as.character(get.ecor.date.time)
  }


  #substituindo nomes de colunas pelos códigos repeticao.season.imagem
  #obs: afeta só imagens sem composição (as com composição tiveram nome mudado nos metadata do GEE)
  f.codif.colunas<-function(tabela){
    nomes<-names(tabela)
    for (i in 1:nrow(images.table)){
      if(group.by=="season"){nomes<-gsub(pattern=images.table$images[i], replacement=images.table$rep.season.image[i], x=nomes)}
      if(group.by=="month"){nomes<-gsub(pattern=images.table$images[i], replacement=images.table$rep.month.image[i], x=nomes)}
    }
    return(nomes)
  }

  cat("\n\n")

  tab.stat.samples<-list()

  names(tab.OK.samples)<-f.codif.colunas(tab.OK.samples)
  if(by.image.save==T | summary.save==T){
    write.csv(tab.OK.samples,file=file.path(spreadsheet.folder,"tab.quality.samples.csv"))
    cat(paste0("_____\nFile with sample data quality indicators saved as: \n",file.path(spreadsheet.folder,"tab.quality.samples.csv"), "\n_____"))}
  tab.stat.samples$data.quality<-tab.OK.samples

  #checagem de configurações de usuário: mínimo de pixels OK e número de pixels da samples
  if(sum(tab.npixels.samples$npixels<remove.samples$num.pixelOK)>0){
    print(tab.npixels.samples)
    cat("remove.samples$num.pixelOK =",remove.samples$num.pixelOK,"\n")
    warning("Cutoff value for minimum number of good pixels in samples was exceeded on some sample(s) - all results from that sample(s) will be deleted")}

  if(is.null(composite)==F){
    if(group.by=="season"){list.images.stat<-unique(images.table$rep.season)}
    if(group.by=="month"){list.images.stat<-unique(images.table$rep.month)}
  } else {
    list.images.stat<-images.table$images}

  #função de remoção de samples com poucos pixels OK
  f.remove.samples<-function(tab.ori){
    if(is.null(remove.samples$num.pixelOK) & is.null(remove.samples$prop.pixelOK)){return(tab.ori)} else {
      for (i in list.images.stat){
        adeqP<-tab.propOK.samples[,grep(pattern=i,x=names(tab.propOK.samples),value=T)]
        adeqP[adeqP<remove.samples$prop.pixelOK]<-NA
        adeqP<-adeqP>0 #tranformar os não NA em T
        tab.ori[,grep(pattern=i,x=names(tab.ori),value=T)]<-
          tab.ori[,grep(pattern=i,x=names(tab.ori),value=T)]*adeqP

        adeqN<-tab.npixelsOK.samples[,grep(pattern=i,x=names(tab.npixelsOK.samples),value=T)]
        adeqN[adeqN<remove.samples$num.pixelOK]<-NA
        adeqN<-adeqN>0 #tranformar os não NA em T
        tab.ori[,grep(pattern=i,x=names(tab.ori),value=T)]<-
          tab.ori[,grep(pattern=i,x=names(tab.ori),value=T)]*adeqN
      }
      return(tab.ori)
    }
  }

  ##############################
  ### Funções sumarizadoras ####
  ##############################

  f.sumarizadora.month.yearly<-function(tabela,prefixo){
    resultado<-tabela[,c("id","type")]
    for (ba in bands.eval.indices){
      for (re in unique(images.table$rep)){
        for (me in 1:12){
          if(max(images.table$rep)<10){
            prov<-tabela[,grep(pattern=glob2rx(paste0("r",re,sprintf("m%02d",me),"i*",ba)),x=names(tabela),value=T)]} else {
              prov<-tabela[,grep(pattern=glob2rx(paste0(sprintf("r%02d",re),sprintf("m%02d",me),"i*",ba)),x=names(tabela),value=T)]}
          if(class(prov)=="data.frame"){prov.mean<-rowMeans(prov,na.rm=T)
          prov.mean[is.nan(prov.mean)]<-NA
          resultado[,ncol(resultado)+1]<-prov.mean} else {
            resultado[,ncol(resultado)+1]<-prov}
          if(max(images.table$rep)<10){names(resultado)[ncol(resultado)]<-paste0(prefixo,".r",re,sprintf("m%02d",me),"_",ba)} else {
            names(resultado)[ncol(resultado)]<-paste0(prefixo,sprintf(".r%02d",re),sprintf("m%02d",me),"_",ba)}
        }
      }
    }
    return(resultado)
  }

  f.sumarizadora.month.all<-function(tabela,prefixo){
    resultado<-tabela[,c("id","type")]
    for (ba in bands.eval.indices){
      for (me in 1:12){
        if(is.null(composite)){
          prov<-tabela[,grep(pattern=glob2rx(paste0(sprintf("*m%02d",me),"i*",ba)),x=names(tabela),value=T)]} else {
            prov<-tabela[,grep(pattern=glob2rx(paste0(sprintf("*m%02d",me),"_",ba)),x=names(tabela),value=T)]}
        if(class(prov)=="data.frame"){prov.mean<-rowMeans(prov,na.rm=T)
        prov.mean[is.nan(prov.mean)]<-NA
        resultado[,ncol(resultado)+1]<-prov.mean} else {
          resultado[,ncol(resultado)+1]<-prov}
        names(resultado)[ncol(resultado)]<-paste0(prefixo,sprintf(".m%02d",me),"_",ba)
      }
    }
    return(resultado)
  }

  #refazendo o do get.ecors
  if (group.by=="season"){
    seasons.used<-c("s1","s2","s3","s4")[lengths(seasons)>0]
    contador<-data.frame(matrix(nrow=1,ncol=length(seasons.used),c(0))) #versão para estações
    names(contador)<-seasons.used
  }
  if (group.by=="month"){
    seasons.used<-1:12
    dates.table$season<-dates.table$month #provisório para usar versão genérica do contador de repetições -> vai ser mudado adiante para "month"
    contador<-data.frame(matrix(nrow=1,ncol=12,c(0))) #versão para meses
    names(contador)<-1:12
  }

  f.sumarizadora.season.yearly<-function(tabela,prefixo){
    resultado<-tabela[,c("id","type")]
    for (ba in bands.eval.indices){
      for (re in unique(images.table$rep)){
        for (es in seasons.used){
          if(max(images.table$rep)<10){
            prov<-tabela[,grep(pattern=glob2rx(paste0("r",re,es,"i*",ba)),x=names(tabela),value=T)]} else {
              prov<-tabela[,grep(pattern=glob2rx(paste0(sprintf("r%02d",re),es,"i*",ba)),x=names(tabela),value=T)]}
          if(class(prov)=="data.frame"){prov.mean<-rowMeans(prov,na.rm=T)
          prov.mean[is.nan(prov.mean)]<-NA
          resultado[,ncol(resultado)+1]<-prov.mean} else {
            resultado[,ncol(resultado)+1]<-prov}
          if(max(images.table$rep)<10){names(resultado)[ncol(resultado)]<-paste0(prefixo,".r",re,es,"_",ba)} else {
            names(resultado)[ncol(resultado)]<-paste0(prefixo,sprintf(".r%02d",re),es,"_",ba)}
        }
      }
    }
    return(resultado)
  }

  f.sumarizadora.season.all<-function(tabela,prefixo){
    resultado<-tabela[,c("id","type")]
    for (ba in bands.eval.indices){
      for (es in seasons.used){
        if(is.null(composite)){
          prov<-tabela[,grep(pattern=glob2rx(paste0("*",es,"i*",ba)),x=names(tabela),value=T)]} else {
            prov<-tabela[,grep(pattern=glob2rx(paste0("*",es,"_",ba)),x=names(tabela),value=T)]}
        if(class(prov)=="data.frame"){prov.mean<-rowMeans(prov,na.rm=T)
        prov.mean[is.nan(prov.mean)]<-NA
        resultado[,ncol(resultado)+1]<-prov.mean} else {
          resultado[,ncol(resultado)+1]<-prov}
        names(resultado)[ncol(resultado)]<-paste0(prefixo,".",es,"_",ba)
      }
    }
    return(resultado)
  }

  f.sumarizadora.season.all<-function(tabela,prefixo){
    resultado<-tabela[,c("id","type")]
    for (ba in bands.eval.indices){
      for (es in seasons.used){
        if(is.null(composite)){
          prov<-tabela[,grep(pattern=glob2rx(paste0("*",es,"i*",ba)),x=names(tabela),value=T)]} else {
            prov<-tabela[,grep(pattern=glob2rx(paste0("*",es,"_",ba)),x=names(tabela),value=T)]}
        if(class(prov)=="data.frame"){prov.mean<-rowMeans(prov,na.rm=T)
        prov.mean[is.nan(prov.mean)]<-NA
        resultado[,ncol(resultado)+1]<-prov.mean} else {
          resultado[,ncol(resultado)+1]<-prov}
        names(resultado)[ncol(resultado)]<-paste0(prefixo,".",es,"_",ba)
      }
    }
    return(resultado)
  }


  ##################################################
  ####Executando sumarizações e salvando tudo ######
  ##################################################

  ### Mean
  if(stats$mean==T){
    tab.mean.samples<-f.remove.samples(tab.mean.samples) # isso tem que vir antes de mudar os nomes das colunas
    names(tab.mean.samples)<-f.codif.colunas(tab.mean.samples) # não afeta composições

    tab.mean.samples$get.ecor.date.time<-as.character(get.ecor.date.time)
    if(summarizing=="yearly"){
      if(group.by=="month"){tab.stat.samples$summary$mean<-f.sumarizadora.month.yearly(tab.mean.samples,"M")}
      if(group.by=="season"){tab.stat.samples$summary$mean<-f.sumarizadora.season.yearly(tab.mean.samples,"M")}
    }
    if(summarizing=="all"){
      if(group.by=="month"){tab.stat.samples$summary$mean<-f.sumarizadora.month.all(tab.mean.samples,"M")}
      if(group.by=="season"){tab.stat.samples$summary$mean<-f.sumarizadora.season.all(tab.mean.samples,"M")}
    }
    tab.stat.samples$summary$mean$get.ecor.date.time<-as.character(get.ecor.date.time)
    tab.stat.samples$results$mean<-tab.mean.samples

    if(by.image.save==T){
      write.csv(tab.mean.samples,file=file.path(spreadsheet.folder,"results.mean.samples.csv"))
      cat(paste0("_____\nFile with data by image was saved as: \n",file.path(spreadsheet.folder,"results.mean.samples.csv"), "\n_____"))}
    if(summary.save==T){
      write.csv(tab.stat.samples$summary$mean,file=file.path(spreadsheet.folder,"summary.mean.samples.csv"))
      cat(paste("_____\nFile with summarizingd data was saved as: \n",file.path(spreadsheet.folder,"summary.mean.samples.csv"), "\n_____"))}
  }

  ### Median
  if(stats$median==T){
    tab.median.samples<-f.remove.samples(tab.median.samples) # isso tem que vir antes de mudar os nomes das colunas
    names(tab.median.samples)<-f.codif.colunas(tab.median.samples) # não afeta composições

    tab.median.samples$get.ecor.date.time<-as.character(get.ecor.date.time)
    if(summarizing=="yearly"){
      if(group.by=="month"){tab.stat.samples$summary$median<-f.sumarizadora.month.yearly(tab.median.samples,"Mn")}
      if(group.by=="season"){tab.stat.samples$summary$median<-f.sumarizadora.season.yearly(tab.median.samples,"Mn")}
    }
    if(summarizing=="all"){
      if(group.by=="month"){tab.stat.samples$summary$median<-f.sumarizadora.month.all(tab.median.samples,"Mn")}
      if(group.by=="season"){tab.stat.samples$summary$median<-f.sumarizadora.season.all(tab.median.samples,"Mn")}
    }
    tab.stat.samples$summary$median$get.ecor.date.time<-as.character(get.ecor.date.time)
    tab.stat.samples$results$median<-tab.median.samples

    if(by.image.save==T){
      write.csv(tab.median.samples,file=file.path(spreadsheet.folder,"results.median.samples.csv"))
      cat(paste("_____\nFile with data by image was saved as: \n",file.path(spreadsheet.folder,"results.median.samples.csv"), "\n_____"))}
    if(summary.save==T){
      write.csv(tab.stat.samples$summary$median,file=file.path(spreadsheet.folder,"summary.median.samples.csv"))
      cat(paste("_____\nFile with summarized data was saved as: \n",file.path(spreadsheet.folder,"summary.median.samples.csv"), "\n_____"))}
  }

  ### SD
  if(stats$sd==T){
    tab.sd.samples<-f.remove.samples(tab.sd.samples) # isso tem que vir antes de mudar os nomes das colunas
    names(tab.sd.samples)<-f.codif.colunas(tab.sd.samples) # não afeta composições

    tab.sd.samples$get.ecor.date.time<-as.character(get.ecor.date.time)
    if(summarizing=="yearly"){
      if(group.by=="month"){tab.stat.samples$summary$sd<-f.sumarizadora.month.yearly(tab.sd.samples,"SD")}
      if(group.by=="season"){tab.stat.samples$summary$sd<-f.sumarizadora.season.yearly(tab.sd.samples,"SD")}
    }
    if(summarizing=="all"){
      if(group.by=="month"){tab.stat.samples$summary$sd<-f.sumarizadora.month.all(tab.sd.samples,"SD")}
      if(group.by=="season"){tab.stat.samples$summary$sd<-f.sumarizadora.season.all(tab.sd.samples,"SD")}
    }
    tab.stat.samples$summary$sd$get.ecor.date.time<-as.character(get.ecor.date.time)
    tab.stat.samples$results$sd<-tab.sd.samples

    if(by.image.save==T){
      write.csv(tab.sd.samples,file=file.path(spreadsheet.folder,"results.sd.samples.csv"))
      cat(paste("_____\nFile with data by image was saved as: \n",file.path(spreadsheet.folder,"results.sd.samples.csv"), "\n_____"))}
    if(summary.save==T){
      write.csv(tab.stat.samples$summary$sd,file=file.path(spreadsheet.folder,"summary.sd.samples.csv"))
      cat(paste("_____\nFile with summarized data was saved as: \n",file.path(spreadsheet.folder,"summary.sd.samples.csv"), "\n_____"))}
  }

  ### Count
  if(stats$count==T){
    backup.bands.eval.indices<-bands.eval.indices #workaround: no bands
    bands.eval.indices<-c("count","prop")
    tab.count.prop.samples<-f.remove.samples(tab.count.prop.samples) # isso tem que vir antes de mudar os nomes das colunas
    names(tab.count.prop.samples)<-f.codif.colunas(tab.count.prop.samples) # não afeta composições

    tab.count.prop.samples$get.ecor.date.time<-as.character(get.ecor.date.time)
    if(summarizing=="yearly"){
      if(group.by=="month"){tab.stat.samples$summary$count<-f.sumarizadora.month.yearly(tab.count.prop.samples,"cnt")}
      if(group.by=="season"){tab.stat.samples$summary$count<-f.sumarizadora.season.yearly(tab.count.prop.samples,"cnt")}
    }
    if(summarizing=="all"){
      if(group.by=="month"){tab.stat.samples$summary$count<-f.sumarizadora.month.all(tab.count.prop.samples,"cnt")}
      if(group.by=="season"){tab.stat.samples$summary$count<-f.sumarizadora.season.all(tab.count.prop.samples,"cnt")}
    }
    tab.stat.samples$summary$count$get.ecor.date.time<-as.character(get.ecor.date.time)
    tab.stat.samples$results$count<-tab.count.prop.samples

    bands.eval.indices<-backup.bands.eval.indices # reverting bands of workaround: no bands

    if(by.image.save==T){
      write.csv(tab.count.prop.samples,file=file.path(spreadsheet.folder,"results.count.samples.csv"))
      cat(paste("_____\nFile with data by image was saved as: \n",file.path(spreadsheet.folder,"results.count.samples.csv"), "\n_____"))}
    if(summary.save==T){
      write.csv(tab.stat.samples$summary$count,file=file.path(spreadsheet.folder,"summary.count.samples.csv"))
      cat(paste("_____\nFile with summarized data was saved as: \n",file.path(spreadsheet.folder,"summary.count.samples.csv"), "\n_____"))}
  }
  cat("\n")


  metadata<-list(get.ecor.date.time=as.character(get.ecor.date.time), collection=collection, start=start, end=end,
                 dates.table=dates.table, images.table=images.table,
                 composite=paste(composite,"by",group.by), seasons=seasons, group.by=group.by, summarizing=summarizing,
                 bands.eval=bands.eval, indices=indices, bands.used=bands.used,
                 samples.stats=unlist(stats), edge.pixels=edge.pixels, buffer=unlist(list("Points->circles(m)"=buffer.points,"Plots(m)"=buffer.plots)),
                 samples=tab.npixels.samples, PixelsOK=tab.OK.samples[,-ncol(tab.OK.samples)])

  metadata$samples$area.m2<-area.m2

  tab.stat.samples$metadata<-metadata

  return(tab.stat.samples)
}
