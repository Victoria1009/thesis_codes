#############################################################################
# titulo        : Uso de los modelos para predicion;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Generar predicciones binarias event-noevent, de la susceptibilidad
#                 por deslizamientos de tierra;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Modelos en RDS;
# salida        : Archivos GeoTIFF y mapa de prediccion;
# observaciones : ninguna;
##############################################################################

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, raster, sf, stringr, doParallel, ggspatial, pals, gridExtra, viridis, purrr))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))
  source(paste0(r.dir,'/functions/4_Predict.R'))

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
  proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
  proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salidas/covariables')
  datos.salida.geotiff <- paste0(proyecto.directorio,'/prediccion/geotiff/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(datos.salida.geotiff, recursive = T, mode = "0777", showWarnings = F)
  datos.salida.figuras <- paste0(proyecto.directorio,'/prediccion/figuras/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(datos.salida.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  modelos.incertidumbre.raster = paste0(proyecto.directorio,'/modelos/incertidumbre/geotiff/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particicn
  print(modelos.datos.entrada)
  load(paste0(modelos.datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))
  train.variables <- names(train.data)[which(names(train.data) != 'target')]

  # Cargar 1_covariables y eliminar capas no usadas en el entrenamiento
  COV <- stack(paste0(datos.entrada,'/covariables.tif'))
  names(COV) <- readRDS(paste0(datos.entrada,'/covariables.rds'))
  capas.eliminar <- names(COV)[which(!names(COV) %in% train.variables)]
  COV <- dropLayer(COV, capas.eliminar)

  ##################
  #### ATENCION ####
  ##################
  
  #### OPCION 1 ####
  #### Por defecto, selecciona el modelo con mejores metricas de desempeño#### 
  #identificar mejor modelo
  # modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_metricas.csv'))
  # metrica <- paste0(proyecto.metricas.categoricas[1],'.Median')
  # modelos.mejor <- modelos.resultado[modelos.resultado[,metrica] == max(modelos.resultado[,metrica]), 'modelos']
  
  #### OPCION 2 ####
  #### Seleccion manual del modelo de aprendizaje####
  #### mejor.modelo puede ser: ranger;C5.0;xgbTree;svmLinear;mlp;svmRadial####
  #### Debe ajustarse manualmente el nombre y correr el script####
  modelos.mejor <- "svmRadial"
  index <- 1:nlevels(train.data[['target']])
  type <- 'prob'

  ##### output messages ####
  cat(paste0('### RESULTADO 1 de 3: El mejor modelo es ',modelos.mejor,' y se comprueba si ya existe archivo GeoTIFF de prediccion ###','\n'))
  ##### end output messages ####

  m <- get(load(paste0(modelos.entrada,modelos.mejor,'/',modelos.mejor,".rds",sep="")))

  prediccion.archivo.geotiff <- paste0(datos.salida.geotiff,'/Prediccion_',modelos.mejor,'.tif')
  if (!file.exists(prediccion.archivo.geotiff)){
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de prediccion usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
    # if (is(train.data[,'target'],'numeric')){
    #   start <- Sys.time()
    #   PredictGeoTIFF(COV, modelo.ajuste, prediccion.archivo.geotiff, type, index, train.data)
    #   print(Sys.time() - start)
    # } else if (is(train.data[,'target'],'factor')){
      proba.archivo.geotiff <- paste0(modelos.incertidumbre.raster,'/Probabilidad_',modelos.mejor,'.tif')
      prediction_prob <- stack(proba.archivo.geotiff)

      start <- Sys.time()
      no_cores <- detectCores() - 1
      beginCluster(no_cores)
      clusterR(prediction_prob, fun = calc, args = list(fun = nnet::which.is.max),
                            filename = prediccion.archivo.geotiff, format = "GTiff",
                            overwrite = T, datatype='INT1U', options='COMPRESS=YES')
      endCluster()
      print(Sys.time() - start)

      ## Equivalencias
      clases_traindata <- levels(train.data$target)

      # cargar raster prediccion
      pred <- raster(prediccion.archivo.geotiff)
      predvalues <- unique(pred)
      predclases <- clases_traindata[unique(pred)]

      # generar tabla equivalencia
      equivalencia.tabla <- data.frame(ID=predvalues,clase=predclases)

      write.csv(equivalencia.tabla,paste0(datos.salida.geotiff,'/CodificacionClases_',modelos.mejor,'.csv'), row.names=FALSE)

    }
  # } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de prediccion usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
  # }

  prediccion.archivo.figuras <- paste0(datos.salida.figuras,'/Prediccion_',modelos.mejor,'.png')
  if (!file.exists(prediccion.archivo.figuras)){
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de 3: La figura de prediccion de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.figuras,' ###','\n'))
    ##### end output messages ####
    pred <- raster(prediccion.archivo.geotiff)
    
    pred <- ratify(pred)
    rat <- levels(pred)[[1]]
    rat$class <- levels(train.data[['target']])[rat$ID]
    levels(pred) <- rat
      
    pred2 <- deratify(pred, 'class')
    
    n<-length(levels(pred2)[[1]]$class)
    
    cols <- pals::cols25(n)
    
    p <- rasterVis::levelplot(pred2, maxpixels = ncell(pred2), col.regions = cols, par.settings = list(axis.line = list(col = 'transparent'),
                                                                                                       strip.background = list(col = 'transparent'),
                                                                                                       strip.border = list(col = 'transparent')),
                              scales = list(col = 'transparent'))
      
    png(file = prediccion.archivo.figuras, width = 1400, height = 900, res=150, bg = "transparent")
    print(p)
    dev.off()
    } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de 3: La figura de prediccion de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', prediccion.archivo.figuras,' ###','\n'))
    ##### end output messages ####
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))
#}