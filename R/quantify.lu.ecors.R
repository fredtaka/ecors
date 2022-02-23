#' Quantify the pixels of each Land Use class
#'
#' Generate tables of number of pixels of each Land Use class in each reference polygon (site, samples, polygons - as defined in function get.lu.ecors) in each year.
#'
#' @param x lu.ecors object (from get.lu.ecors).
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
#'
#' @examples
#' #get a lu.ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' lu2000_2010<-get.lu.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.lu="mapbiomas6", years=c(2000,2010), resolution=30, evaluate="surroundings.samples",
#'      buffer1=500, buffer2=1000, buffer3=2000, cumulative.surroundings=F)
#'
#' table2000_2010<-quantify.lu.ecors(x=lu2000_2010, save.format=c("ods"))

quantify.lu.ecors<-function(x, save.format="ods", save.metadata=T, spreadsheet.folder=getwd()){

  if(class(x)!="lu.ecors"){stop("Argument x must be a lu.ecors class object.")}

  lu.prov<-eval(parse(text=x$object.name))

  if(is.null(spreadsheet.folder)==F){
    if(substr(spreadsheet.folder,nchar(spreadsheet.folder),nchar(spreadsheet.folder))=="/"){spreadsheet.folder<-substr(spreadsheet.folder,1,nchar(spreadsheet.folder)-1)}
  }

  if(is.null(save.format)==F){
    if("ods"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_quantify_lu.ods"))==T){
        stop("File results_quantify_lu.ods already exists in the selected directory:  (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.")}
    }

    if("RData"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_quantify_lu.RData"))==T){
        stop("File results_quantify_lu.RData already exists in the selected directory: (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.")}
    }
  }

  band1<-lu$bandNames()$getInfo()[1]
  lu.valores1<-lu$select(band1)$lt(10000)$rename("valores1")


  fun.lu<-function(mi){
    lu.selecionado<-lu.valores1$addBands(lu$select(ee$String(mi)))
    resp<-lu.selecionado$reduceRegions(
      collection=polig,
      reducer=ee$Reducer$sum()$group(groupField=1),
      scale=x$resolution)
    return(resp)
  }

  fun.lu.1ano<-function(polig){
    lu.selecionado<-lu.valores1$addBands(lu$select(ee$String(paste0("classification_",x$years))))
    resp<-lu.selecionado$reduceRegions(
      collection=polig,
      reducer=ee$Reducer$sum()$group(groupField=1),
      scale=x$resolution)$getInfo()
    return(resp)
  }

  #preenchendo tabela
  preenchendo.tabela.lu<-function(redu.lu.polygons){
    lista.func.lu<-list()
    for (i in 1:nrow(x$polig.lu1)){
      if(x$evaluate=="surroundings.samples"){ lista.func.lu[[paste0("pol",i,"_id",x$polig.lu1$id[i])]]<-data.frame() } else {
        lista.func.lu[[paste0("pol",i)]]<-data.frame() }
    }

    if(length(x$years)>1){
      for (ano in 1:length(x$years)){
        ano.c0<-I(ano-1)
        redu.lu.polygons.ano<-redu.lu.polygons$get(ano.c0)$getInfo()

        for (pol in 1:nrow(x$polig.lu1)){
          tabela.lu<-x$legend.lu%>%select(pixel.value,lu.class)%>%as.data.frame()
          tabela.lu[,paste0("npixels_",x$years[ano])]<-0

          for (lu.class in 1:length(redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]])){
            tabela.lu[tabela.lu$pixel.value==redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["group"]] , paste0("npixels_",x$years[ano]) ]<-
              redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["sum"]]

          }
          prov<-sum(select(tabela.lu,paste0("npixels_",x$years[ano])))
          tabela.lu[, paste0("prop_",x$years[ano]) ]<-tabela.lu[, paste0("npixels_",x$years[ano]) ]/prov
          if(ncol(lista.func.lu[[pol]])==0){lista.func.lu[[pol]]<-tabela.lu} else {
            lista.func.lu[[pol]]<-full_join(tabela.lu,lista.func.lu[[pol]],by=c("pixel.value", "lu.class"),suffix=c("",""))}
        }
      }
    }

    if(length(x$years)==1){
      redu.lu.polygons.ano<-redu.lu.polygons #com mais de 1 ano muda no loop. Aqui é só mudança de nome por compatibilidade
      lista.func.lu.ano<-x$legend.lu%>%select(pixel.value,lu.class)%>%as.data.frame() #para gerar tabela "all"
      for (pol in 1:nrow(x$polig.lu1)){
        tabela.lu<-x$legend.lu%>%select(pixel.value,lu.class)%>%as.data.frame()
        tabela.lu[,paste0("npixels_",x$years)]<-0

        for (lu.class in 1:length(redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]])){
          tabela.lu[tabela.lu$pixel.value==redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["group"]] , paste0("npixels_",x$years) ]<-
            redu.lu.polygons.ano[["features"]][[pol]][["properties"]][["groups"]][[lu.class]][["sum"]]

        }
        prov<-sum(select(tabela.lu,paste0("npixels_",x$years)))
        tabela.lu[, paste0("prop_",x$years) ]<-tabela.lu[, paste0("npixels_",x$years) ]/prov
        lista.func.lu[[pol]]<-tabela.lu

        if(nrow(x$polig.lu1)>1){#tabela todos polígonos juntos (para mesmo buffer)
          lista.func.lu.ano<-full_join(lista.func.lu.ano,tabela.lu,by=c("pixel.value", "lu.class"),suffix=c("",""))
          if(x$evaluate=="surroundings.samples"){
            names(lista.func.lu.ano)[ncol(lista.func.lu.ano)]<-paste0("pol",pol,"_id",x$polig.lu1$id[pol],"_prop")
            names(lista.func.lu.ano)[ncol(lista.func.lu.ano)-1]<-paste0("pol",pol,"_id",x$polig.lu1$id[pol],"_npixel")
          }
          if(x$evaluate!="surroundings.samples"){
            names(lista.func.lu.ano)[ncol(lista.func.lu.ano)]<-paste0("pol",pol,"_prop")
            names(lista.func.lu.ano)[ncol(lista.func.lu.ano)-1]<-paste0("pol",pol,"_npixel")
          }
        }
        if(nrow(x$polig.lu1)>1){lista.func.lu[[paste0("All_",x$years)]]<-lista.func.lu.ano}

      }
    }
    return(lista.func.lu) #para a função toda
  }

  ##Rodando
  if(exists("results.lu")==F){results.lu<-list()}

  if(length(x$years)>1){#rgee não aceita ee$List de tamanho menor que 2 | Aqui redu.lu.polygons é usado em dois significados: objeto e entrada (variável) de função
    years.gee<-paste0("classification_",x$years)
    years.gee<-years.gee%>%ee$List()
    polig<-polig.lu1.gee
    redu.lu.polygons<-years.gee$map(ee_utils_pyfunc(fun.lu))
    if(x$buffer1 > 0) {results.lu[[paste0("buffer_",x$buffer1)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)} else {
      results.lu[["geom_provided"]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)}

    if(x$buffer2 > 0){
      polig<-polig.lu2.gee
      redu.lu.polygons<-years.gee$map(ee_utils_pyfunc(fun.lu))
      results.lu[[paste0("buffer_",x$buffer2)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)
    }
    if(x$buffer3 > 0){
      polig<-polig.lu3.gee
      redu.lu.polygons<-years.gee$map(ee_utils_pyfunc(fun.lu))
      results.lu[[paste0("buffer_",x$buffer3)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)
    }
    cat(paste("\nRuns with more than 1 year do not generate summary tables of all polygons together (table \"All\") as this would not be practical to analyze: too large number of columns and long column names.\nUse the structure of the list of tables (\"results.lu\") to access results in R.\n"))
  }

  if(length(x$years)==1){#rgee não aceita ee$List de tamanho menor que 2
    redu.lu.polygons<-fun.lu.1ano(polig.lu1.gee)
    if(x$buffer1 > 0) {results.lu[[paste0("buffer_",x$buffer1)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)} else {
      results.lu[["geom_provided"]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)}

    if(x$buffer2 > 0){
      redu.lu.polygons<-fun.lu.1ano(polig.lu2.gee)
      results.lu[[paste0("buffer_",x$buffer2)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)
    }
    if(x$buffer3 > 0){
      redu.lu.polygons<-fun.lu.1ano(polig.lu3.gee)
      results.lu[[paste0("buffer_",x$buffer3)]]<-preenchendo.tabela.lu(redu.lu.polygons=redu.lu.polygons)
    }
  }
  results.lu[["polygon_info"]]<-st_drop_geometry(x$polig.lu1)%>%mutate(cod_polyg=NA)
  for (i in 1:nrow(x$polig.lu1)){
    if(x$evaluate=="surroundings.samples"){
      results.lu[["polygon_info"]][i,"cod_polyg"]<-paste0("pol",i,"_id",x$polig.lu1$id[i])
    } else {
      results.lu[["polygon_info"]][i,"cod_polyg"]<-paste0("pol",i)
    }
  }

  #Salvando
  if(is.null(save.format)==F){
    if("ods"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_quantify_lu.ods"))==T){
        stop("File results_quantify_lu.ods already exists in the selected directory:  (", file.path(spreadsheet.folder), "). \nChange the name or location of that file before proceeding.")}
      for(buf in 1:length(results.lu)){
        if(names(results.lu)[buf]=="polygon_info"){
          write_ods(results.lu[["polygon_info"]],path=paste0(file.path(spreadsheet.folder,sep=""),"results_quantify_lu.ods"),
                    sheet="polygon_info",append=T)
        } else {
          for(pol in 1:length(results.lu[[1]])){
            write_ods(results.lu[[buf]][[pol]],path=paste0(file.path(spreadsheet.folder,sep=""),"results_quantify_lu.ods"),
                      sheet=paste0(names(results.lu[buf]),"_",names(results.lu[[buf]][pol])),append=T)
          }#apesar dos nomes, funciona também com polígono sem buffer
        }
      }
      cat(paste0("\n File \"results_quantify_lu.ods\" saved:",file.path(spreadsheet.folder), " at ",format(Sys.time(),"%H:%M", "\n")))
    }

    if("RData"%in%save.format){
      if(file.exists(file.path(spreadsheet.folder,"results_quantify_lu.RData"))==T){
        stop("File results_quantify_lu.RData already exists in the selected directory:  (",file.path(spreadsheet.folder),"). \nChange the name or location of that file before proceeding.\n")}
      save(results.lu,file=paste0(file.path(spreadsheet.folder,sep=""),"results_quantify_lu.RData"))
      cat(paste0("\n File \"results_quantify_lu.RData\" saved:",file.path(spreadsheet.folder), "\n at ",format(Sys.time(),"%H:%M", "\n")))
    }

  }
  if(save.metadata==T){
    sink(paste0(file.path(spreadsheet.folder,sep=""),"metadata.txt"))
    if("ods"%in%save.format){cat(paste0("\n File \"results_quantify_lu.ods\" saved:",file.path(spreadsheet.folder), "\n (approximately) at ",format(Sys.time(),"%H:%M", "\n")))}
    if("RData"%in%save.format){cat(paste0("\n File \"results_quantify_lu.RData\" saved:",file.path(spreadsheet.folder), "\n (approximately) at ",format(Sys.time(),"%H:%M", "\n")))}
    cat("\n")
    print(x)
    sink()
  }

}
