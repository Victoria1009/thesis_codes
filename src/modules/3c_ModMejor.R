#############################################################################
# titulo        : Identificacion del mejor modelo;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Identificar del mejor modelo de acuerdo con la metrica seleccionada;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Archivos de los modelos en formato rds;
# salida        : Tablas y graficas para identificar el mejor modelo;
# observaciones : ninguna;
##############################################################################

  # rm(list = ls()); options(scipen = 999, warn = -1)
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(data.table, PerformanceAnalytics, caret, plyr, dplyr, purrr, stringr))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))  
  source(paste0(r.dir,'/functions/3_ConfigModelos.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Definicion de la variable objetivo
  VarObj <- "tipo"
  ## Uso de 10 variables predictoras
  rfe_lim <- 10

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[['proyecto.carpeta']]

  proyecto.modelos.categoricas <- conf.args[['modelos.categoricas']]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))

  proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
  proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada <- trimws(paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-')))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(modelos.analisis.tabular, recursive = T, mode = "0777", showWarnings = F)
  modelos.analisis.figuras = paste0(proyecto.directorio,'/modelos/analisis/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(modelos.analisis.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))
  
  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particion
  load(paste0(datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  proyecto.modelos <- proyecto.modelos.categoricas
  proyecto.metricas <- proyecto.metricas.categoricas

  #identificar modelos entrenados
  modelos.procesados = basename(list.dirs(modelos.entrada, recursive = F, full.names = F))
  
  modelos.procesados = unique(sapply(modelos.procesados, "[", 1))

  modelos.parametros = NULL
    for (modelo in modelos.procesados){
      get(load(paste0(modelos.entrada,'/',modelo,'/',modelo,".rds")))
      bestTuneIndex <- as.numeric(rownames(modelo.ajuste$bestTune)[1])
      modelo.mejor <- modelo.ajuste$results[bestTuneIndex, 1:dim(modelo.ajuste$results)[2]]
      modelo.mejor = cbind('modelo'=modelo,modelo.mejor)
      modelos.parametros = rbind.fill(modelos.parametros, modelo.mejor)
      assign(paste0('m_',modelo), modelo.ajuste)
    }
    
    #metricas
    wild <- paste0(modelo.ajuste$perfNames,collapse='|')
    
    #parametros
    modelos.parametros <- modelos.parametros[,!grepl(wild, names(modelos.parametros), perl = TRUE)]
    
    #export models best combination parameters and features
    write.csv(modelos.parametros,file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'), row.names=F)
    
    ##### mensaje de salida ####
    cat(paste('### RESULTADO 1 de 3: Los parametros y metricas de los mejores modelos fueron generados y almacenados como tablas en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
    ##### final del mensaje de salida ####
    models = objects(pattern='*m_')
    #models = models[2:6]

    #create list for resampling
    resampling.list <- list()
    for(m in models){
      a <- get(m)
      name <- m
      resampling.list[[name]] <- a
    }

  
  #resampling for model comparison
  resamps <- resamples(resampling.list)

  #remover prefijo
  models = gsub('m_','',models)
  
  #metricas modelos
  modelos.metricas = data.frame('modelos'=models,summary(resamps)$statistics)
  modelos.metricas <- modelos.metricas %>% arrange_at(paste0(proyecto.metricas[1],'.Median'), desc)
  #exportar metricas modelos
  write.csv(modelos.metricas,paste0(modelos.analisis.tabular,"/mejoresmodelos_metricas.csv",sep=""), row.names=F)
  
  ##### mensaje de salida ####
  cat(paste('### RESULTADO 2 de 3: Los valores para realizar boxplots de los modelos creados fueron generados y almacenados como archivo tabular en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
  ##### final del mensaje de salida ####

  scales <- list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5, labels=rev(modelos.metricas$modelos)))

  png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
  print(bwplot(resamps, scales=scales, metric=proyecto.metricas, layout = c(2, 1), box.ratio = 1, auto.key = T))
  dev.off()

  ##### mensaje de salida ####
  cat(paste('### RESULTADO 3 de 3: Los graficos boxplots de los modelos creados fueron generados y almacenados como archivo PNG en la ruta', modelos.analisis.figuras,' ###'),'\n','\n')
  ##### final del mensaje de salida ####

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))
#}