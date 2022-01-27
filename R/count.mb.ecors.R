#' Count pixels of each Land Use class
#'
#' Generate tables of number of pixels of each Land Use class in each reference polygon (site, samples, polygons - as defined in function get.mb.ecors) in each year.
#'
#' @param x mb.ecors object (from get.mb.ecors).
#' @param save.format select save results in "ods" or "RData" format (or both). Set to NULL if you do not want to save files.
#' @param save.metadata should save metadadata (txt)?
#' @param spreadsheet.folder local folder to save files.
#'
#' @return
#' List with result tables and metadata. If enabled, ods (recomended) or RData files will be saved with result tables an metadata.
#'
#' @export
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr
#' @importFrom readODS write_ods
#'
#'
#' @examples
#' #get a mb.ecors class object
#' FAL.IBGE.JBB<-st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' mb2000_2010<-get.mb.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.mb=6, years=c(2000,2010), resolution=30, evaluate="surroundings.samples",
#'      buffer1=500, buffer2=1000, buffer3=2000, cumulative.surroundings=F)
#'
#' table2000_2010<-count.mb.ecors(x=mb2000_2010, save.format=c("ods"))

count.mb.ecors<-function(x, save.format="ods", save.metadata=T, spreadsheet.folder=getwd()){

  if(class(x)!="mb.ecors"){stop("Argument x must be a mb.ecors class object.")}

  list2env(x,envir=environment())

  if(is.null(spreadsheet.folder)==F){
    if(substr(spreadsheet.folder,nchar(spreadsheet.folder),nchar(spreadsheet.folder))=="/"){spreadsheet.folder<-substr(spreadsheet.folder,1,nchar(spreadsheet.folder)-1)}
  }

  if(is.null(save.format)==F){
    if("ods"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_count_mb.ods"))==T){
        stop("File results_count_mb.ods already exists in the selected directory:  (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.")}
    }

    if("RData"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_count_mb.RData"))==T){
        stop("File results_count_mb.RData already exists in the selected directory: (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.")}
    }
  }

  mb.valores1<-mb$select("classification_1985")$lt(10000)$rename("valores1")


  fun.mb<-function(mi){
    mb.selecionado<-mb.valores1$addBands(mb$select(ee$String(mi)))
    resp<-mb.selecionado$reduceRegions(
      collection=polig,
      reducer=ee$Reducer$sum()$group(groupField=1),
      scale=resolution)
    return(resp)
  }

  fun.mb.1ano<-function(polig){
    mb.selecionado<-mb.valores1$addBands(mb$select(ee$String(paste0("classification_",years))))
    resp<-mb.selecionado$reduceRegions(
      collection=polig,
      reducer=ee$Reducer$sum()$group(groupField=1),
      scale=resolution)$getInfo()
    return(resp)
  }

  #preenchendo tabela
  preenchendo.tabela.mb<-function(redu.mb.polygons){
    lista.func.mb<-list()
    for (i in 1:nrow(polig.mb1)){
      if(evaluate=="surroundings.samples"){ lista.func.mb[[paste0("pol",i,"_id",polig.mb1$id[i])]]<-data.frame() } else {
        lista.func.mb[[paste0("pol",i)]]<-data.frame() }
    }

    if(length(years)>1){
      for (ano in 1:length(years)){
        ano.c0<-I(ano-1)
        redu.mb.polygons.ano<-redu.mb.polygons$get(ano.c0)$getInfo()

        for (pol in 1:nrow(polig.mb1)){
          tabela.mb<-legend.mb%>%select(pixel.value,lu.class)%>%as.data.frame()
          tabela.mb[,paste0("npixels_",years[ano])]<-0

          for (lu.class in 1:length(redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]])){
            tabela.mb[tabela.mb$pixel.value==redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["group"]] , paste0("npixels_",years[ano]) ]<-
              redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["sum"]]

          }
          prov<-sum(select(tabela.mb,paste0("npixels_",years[ano])))
          tabela.mb[, paste0("prop_",years[ano]) ]<-tabela.mb[, paste0("npixels_",years[ano]) ]/prov
          if(ncol(lista.func.mb[[pol]])==0){lista.func.mb[[pol]]<-tabela.mb} else {
            lista.func.mb[[pol]]<-full_join(tabela.mb,lista.func.mb[[pol]],by=c("pixel.value", "lu.class"),suffix=c("",""))}
        }
      }
    }

    if(length(years)==1){
      redu.mb.polygons.ano<-redu.mb.polygons #com mais de 1 ano muda no loop. Aqui é só mudança de nome por compatibilidade
      lista.func.mb.ano<-legend.mb%>%select(pixel.value,lu.class)%>%as.data.frame() #para gerar tabela "all"
      for (pol in 1:nrow(polig.mb1)){
        tabela.mb<-legend.mb%>%select(pixel.value,lu.class)%>%as.data.frame()
        tabela.mb[,paste0("npixels_",years)]<-0

        for (lu.class in 1:length(redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]])){
          tabela.mb[tabela.mb$pixel.value==redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["group"]] , paste0("npixels_",years) ]<-
            redu.mb.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["sum"]]

        }
        prov<-sum(select(tabela.mb,paste0("npixels_",years)))
        tabela.mb[, paste0("prop_",years) ]<-tabela.mb[, paste0("npixels_",years) ]/prov
        lista.func.mb[[pol]]<-tabela.mb

        if(nrow(polig.mb1)>1){#tabela todos polígonos juntos (para mesmo buffer)
          lista.func.mb.ano<-full_join(lista.func.mb.ano,tabela.mb,by=c("pixel.value", "lu.class"),suffix=c("",""))
          if(evaluate=="surroundings.samples"){
            names(lista.func.mb.ano)[ncol(lista.func.mb.ano)]<-paste0("pol",pol,"_id",polig.mb1$id[pol],"_prop")
            names(lista.func.mb.ano)[ncol(lista.func.mb.ano)-1]<-paste0("pol",pol,"_id",polig.mb1$id[pol],"_npixel")
          }
          if(evaluate!="surroundings.samples"){
            names(lista.func.mb.ano)[ncol(lista.func.mb.ano)]<-paste0("pol",pol,"_prop")
            names(lista.func.mb.ano)[ncol(lista.func.mb.ano)-1]<-paste0("pol",pol,"_npixel")
          }
        }
        if(nrow(polig.mb1)>1){lista.func.mb[[paste0("All_",years)]]<-lista.func.mb.ano}

      }
    }
    return(lista.func.mb) #para a função toda
  }

  ##Rodando
  if(exists("results.mb")==F){results.mb<-list()}

  if(length(years)>1){#rgee não aceita ee$List de tamanho menor que 2 | Aqui redu.mb.polygons é usado em dois significados: objeto e entrada (variável) de função
    years.gee<-paste0("classification_",years)
    years.gee<-years.gee%>%ee$List()
    polig<-polig.mb1.gee
    redu.mb.polygons<-years.gee$map(ee_utils_pyfunc(fun.mb))
    if(buffer1 > 0) {results.mb[[paste0("buffer_",buffer1)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)} else {
      results.mb[["geom_provided"]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)}

    if(buffer2 > 0){
      polig<-polig.mb2.gee
      redu.mb.polygons<-years.gee$map(ee_utils_pyfunc(fun.mb))
      results.mb[[paste0("buffer_",buffer2)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)
    }
    if(buffer3 > 0){
      polig<-polig.mb3.gee
      redu.mb.polygons<-years.gee$map(ee_utils_pyfunc(fun.mb))
      results.mb[[paste0("buffer_",buffer3)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)
    }
    cat(paste("\nRuns with more than 1 year do not generate summary tables of all polygons together (table \"All\") as this would not be practical to analyze: too large number of columns and long column names.\nUse the structure of the list of tables (\"results.mb\") to access results in R.\n"))
  }

  if(length(years)==1){#rgee não aceita ee$List de tamanho menor que 2
    redu.mb.polygons<-fun.mb.1ano(polig.mb1.gee)
    if(buffer1 > 0) {results.mb[[paste0("buffer_",buffer1)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)} else {
      results.mb[["geom_provided"]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)}

    if(buffer2 > 0){
      redu.mb.polygons<-fun.mb.1ano(polig.mb2.gee)
      results.mb[[paste0("buffer_",buffer2)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)
    }
    if(buffer3 > 0){
      redu.mb.polygons<-fun.mb.1ano(polig.mb3.gee)
      results.mb[[paste0("buffer_",buffer3)]]<-preenchendo.tabela.mb(redu.mb.polygons=redu.mb.polygons)
    }
  }
  results.mb[["polygon_info"]]<-st_drop_geometry(polig.mb1)%>%mutate(cod_polyg=NA)
  for (i in 1:nrow(polig.mb1)){
    if(evaluate=="surroundings.samples"){
      results.mb[["polygon_info"]][i,"cod_polyg"]<-paste0("pol",i,"_id",polig.mb1$id[i])
    } else {
      results.mb[["polygon_info"]][i,"cod_polyg"]<-paste0("pol",i)
    }
  }

  #Salvando
  if(is.null(save.format)==F){
    if("ods"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_count_mb.ods"))==T){
        stop("File results_count_mb.ods already exists in the selected directory:  (", file.path(spreadsheet.folder), "). \nChange the name or location of that file before proceeding.")}
      for(buf in 1:length(results.mb)){
        if(names(results.mb)[buf]=="polygon_info"){
          write_ods(results.mb[["polygon_info"]],path=paste0(file.path(spreadsheet.folder,sep=""),"results_count_mb.ods"),
                    sheet="polygon_info",append=T)
        } else {
          for(pol in 1:length(results.mb[[1]])){
            # write_ods(results.mb[[buf]][[pol]],path=paste0(file.path(spreadsheet.folder,sep=""),"results_count_mb.ods"),
            #           sheet=file.path(names(results.mb[buf]),"_",names(results.mb[[buf]][pol])),append=T)
            write_ods(results.mb[[buf]][[pol]],path=paste0(file.path(spreadsheet.folder,sep=""),"results_count_mb.ods"),
                      sheet=paste0(names(results.mb[buf]),"_",names(results.mb[[buf]][pol])),append=T)
          }#apesar dos nomes, funciona também com polígono sem buffer
        }
      }
      cat(paste0("\n File \"results_count_mb.ods\" saved:",file.path(spreadsheet.folder), " at ",format(Sys.time(),"%H:%M")))
    }

    if("RData"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_count_mb.RData"))==T){
        stop("File results_count_mb.RData already exists in the selected directory:  (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.")}
      save(results.mb,file=paste0(file.path(spreadsheet.folder,sep=""),"results_count_mb.RData"))
      cat(paste0("\n File \"results_count_mb.RData\" saved:",file.path(spreadsheet.folder), "\n at ",format(Sys.time(),"%H:%M")))
    }

  }
  if(save.metadata==T){
    sink(paste0(file.path(spreadsheet.folder,sep=""),"metadata.txt"))
    if("ods"%in%save.format){cat(paste0("\n File \"results_count_mb.ods\" saved:",file.path(spreadsheet.folder), "\n (approximately) at ",format(Sys.time(),"%H:%M")))}
    if("RData"%in%save.format){cat(paste0("\n File \"results_count_mb.RData\" saved:",file.path(spreadsheet.folder), "\n (approximately) at ",format(Sys.time(),"%H:%M")))}
    cat("\n")
    print(x)
    sink()
  }

}
