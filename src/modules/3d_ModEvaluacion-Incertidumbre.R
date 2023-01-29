#############################################################################
# titulo        : Evaluacion del desempeno e incertidumbre de los modelos;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Generar metricas de evaluacion e informacion de incertidumbre 
#                 de los modelos;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Lista de modelos en RDS y tabla de resultados;
# salida        : Tablas y Graficos relacionados con la evaluacion e incertidumbre
#                 de los modelos;
# observaciones : ninguna;
##############################################################################

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, raster, sf, stringr, doParallel,
                                  Metrics, lime, quantregForest, hydroGOF,
                                  RColorBrewer, rasterVis, classInt, ggspatial, viridis,
                                  sf, plyr, dplyr, MLmetrics, scales, purrr, grid))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/3_ConfigModelos.R'))
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
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salidas/covariables/')
  in.limite.data <- paste0(proyecto.directorio,'/datos/limite')
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  
  modelos.incertidumbre.metricas = paste0(proyecto.directorio,'/modelos/incertidumbre/metricas/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(modelos.incertidumbre.metricas, recursive = T, mode = "0777", showWarnings = F)
  
  modelos.incertidumbre.figuras = paste0(proyecto.directorio,'/modelos/incertidumbre/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(modelos.incertidumbre.figuras, recursive = T, mode = "0777", showWarnings = F)
  
  modelos.incertidumbre.raster = paste0(proyecto.directorio,'/modelos/incertidumbre/geotiff/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/')
  dir.create(modelos.incertidumbre.raster, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particion
  load(paste0(modelos.datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))
  train.variables <- names(train.data)[which(names(train.data) != 'target')]
  test.data <- as.data.frame(particion['test'])
  names(test.data) <- sub('test.', "", names(test.data))

  # Cargar RFE
  load(paste0(exploratorio.variables,'/rfe.rds'))

  # Cargar 1_covariables y eliminar capas no usadas en el entrenamiento
  COV <- stack(paste0(datos.entrada,'covariables.tif'))
  
  names(COV) <- readRDS(paste0(datos.entrada,'covariables.rds'))
  
  capas.eliminar <- names(COV)[which(!names(COV) %in% train.variables)]
  
  COV <- dropLayer(COV, capas.eliminar)

  ##################
  #### ATENCION ####
  ##################
  
  #### OPCION 1 ####
  #### Por defecto, selecciona el modelo con mejores metricas de desempeño#### 
  #identificar mejor modelo
  #modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_metricas.csv'))
  #metrica <- paste0(proyecto.metricas.categoricas[1],'.Median')
  #modelos.mejor <- modelos.resultado[modelos.resultado[,metrica] == max(modelos.resultado[,metrica]), 'modelos']

  #### OPCION 2 ####
  #### Seleccion manual del modelo de aprendizaje####
  #### mejor.modelo puede ser: ranger;C5.0;xgbTree;svmLinear;mlp;svmRadial####
  #### Debe ajustarse manualmente el nombre y correr el script####
  modelos.mejor <- "svmRadial"
  pasos_total <- 7
  idx_varImport <- 2
  
  ##### mensaje de salida ####
  cat(paste0('### RESULTADO 1 de ', pasos_total, ' : El mejor modelo es ',modelos.mejor,' y se comprueba si ya existen las salidas de este componente ###','\n'))
  ##### final del mensaje de salida ####

  get(load(paste0(modelos.entrada,modelos.mejor,'/',modelos.mejor,".rds",sep="")))
  
  ##importancia
  incertidumbre.grafico.importancia <- paste0(modelos.incertidumbre.figuras,'/',idx_varImport,'_ImportanciaCovariables_',modelos.mejor,'.png')
  
  if (!file.exists(incertidumbre.grafico.importancia)){
    importancia <- modelos.variables.importancia(modelo.ajuste, modelos.mejor)
    
    png(file = incertidumbre.grafico.importancia, width = 700, height = 600)
    print(importancia)
    dev.off()
  }
  
  pred <- predict(modelo.ajuste, test.data)
  
  accuracy <- caret::confusionMatrix(pred,test.data$target)$overall["Accuracy"]
  kappa <- caret::confusionMatrix(pred,test.data$target)$overall["Kappa"]
  accuracy <- round(accuracy, 2)
  kappa <- round(kappa, 2)

  #https://github.com/harinath0906/Predict-Heart-Arrhythmia/blob/29cc11ba8aa0af41c18485e62882d680d0c4ac42/Predict_Heart_Arrhythmia.Rmd
  calcF1Scores = function(actual, predicted) {
    actual = as.numeric(actual)
    predicted = as.numeric(predicted)
    df = data.frame(actual = actual, predicted = predicted) #Creare a dataframe for actual and predicted for eacy comparison
    fone = recall = c()
    for (i in seq(min(actual), max(actual))) {  #Calculate fone and recall for every class
      tp = nrow(df[df$predicted == i & df$actual == i, ])
      fp = nrow(df[df$predicted == i & df$actual != i, ])
      fn = nrow(df[df$predicted != i & df$actual == i, ])
        #Calculate precision recall and f1
      PR = tp / (tp + fp) 
      RE = tp / (tp + fn)
      f1 = (2 * PR * RE) / (PR + RE)
        #Handle some exception scenarios
      if (tp == fp & fp == fn & fn == 0) 
      {
        PR = 1
        RE = 1
        f1 = 1
      }
      else if (tp == fp & fp == 0)
      {
        PR = 1
        RE = tp / (tp + fn)
        f1 = (2 * PR * RE) / (PR + RE)
          
      } else if (tp == 0)
      {
        PR = 0
        RE = 0
        f1 = 0
      }
      fone = c(fone, f1)
      recall = c(recall, RE) 
      }
    return(list(mean(fone), mean(recall))) #Avergae values of all classes to calculate macro f1
  }
    
  F1Score <- calcF1Scores(pred,test.data$target)[[1]]
  F1Score <- round(F1Score, 2)

  metricas.abreviaturas <- c('OA','Kappa','F1-score (macro)')
  metricas.nombres <- c('Overall Accuracy','Kappa','F-score')
  metricas.valores <- c(accuracy,kappa,F1Score)

  metricas.table <- data.frame(Abreviaturas=metricas.abreviaturas,Nombre=metricas.nombres,Valor=metricas.valores)
  write.csv(metricas.table,paste0(modelos.incertidumbre.metricas,'/MetricasEvaluacion_',modelos.mejor,'.csv'),row.names=FALSE)

  ##### mensaje de salida ####
  cat(paste0('### RESULTADO 2 de ', pasos_total, ' : Las metricas de desempeÃ±o del mejor modelo: ',modelos.mejor,' fueron generadas y almacenadas en la ruta ' , paste0(modelos.incertidumbre.metricas,'/metricas_evaluacion.csv'), ' ###','\n'))
  ##### final del mensaje de salida ####

  ggplotConfusionMatrix <- function(m){
    mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                     "Kappa", percent_format()(m$overall[2]))
    p <-
      ggplot(data = as.data.frame(m$table) ,
             aes(x = Prediction, y = sort(Reference,decreasing = T))) +
      geom_tile(aes(fill = log(Freq)), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      scale_x_discrete(labels=levels(pred)) +
      scale_y_discrete(labels=rev(levels(pred))) +
      geom_text(aes(x = Prediction, y = rev(Reference), label = Freq)) +
      labs(x='Prediccion', y='Observado') +
      theme(legend.position = "none") +
      ggtitle(mytitle) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    return(p)
  }

  confusionMatrix.archivo.figura <- paste0(modelos.incertidumbre.figuras,'/1_MatrizdeConfusion_',modelos.mejor,'.png')
  if (!file.exists(confusionMatrix.archivo.figura)){
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de ', pasos_total, ' : La figura de la matriz de confusion de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', confusionMatrix.archivo.figura,' ###','\n'))
    ##### end output messages ####
      
    cm <- confusionMatrix(factor(pred), factor(test.data$target), dnn = c("Prediction", "Reference"))
    p <- ggplotConfusionMatrix(cm)

    png(file = confusionMatrix.archivo.figura, width = 1000, height = 1000, res=150)
    print(p)
    dev.off()
      
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de ', pasos_total, ' : La figura de la matriz de confusion de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', confusionMatrix.archivo.figura,' ###','\n'))
    ##### end output messages ####
  }
    
  proba.archivo.geotiff <- paste0(modelos.incertidumbre.raster,'/Probabilidad_',modelos.mejor,'.tif')
  if (!file.exists(proba.archivo.geotiff)){
    index <- 1:nlevels(train.data[['target']])
    type <- 'prob'
    ##### output messages ####
    cat(paste0('### RESULTADO 4 de ', pasos_total, ' : El archivo GeoTIFF de probabilidad usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', proba.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
    PredictGeoTIFF(COV, modelo.ajuste, proba.archivo.geotiff, type, index, train.data)
  } else {
    cat(paste0('### RESULTADO 4 de ', pasos_total, ' : El archivo GeoTIFF de probabilidad usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', proba.archivo.geotiff,' ###','\n'))
  }

  entropia.archivo.geotiff <- gsub('Probabilidad_','Entropia_',proba.archivo.geotiff)
  if (!file.exists(entropia.archivo.geotiff)){
    ##### output messages ####
    cat(paste0('### RESULTADO 5 de ', pasos_total, ' : El archivo GeoTIFF de entropia usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', entropia.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
    entropy <- function (x) {
      - sum(x * log(x, base = length(x)), na.rm = TRUE)
    }

    prediction_prob <- stack(proba.archivo.geotiff)

    no_cores <- detectCores() - 1
    beginCluster(no_cores)
    clusterR(prediction_prob, fun = calc, args = list(fun = entropy),
                          filename = entropia.archivo.geotiff, format = "GTiff",
                          overwrite = T, datatype='FLT4S', options='COMPRESS=YES')
    endCluster()
  } else {
    cat(paste0('### RESULTADO 5 de ', pasos_total, ' : El archivo GeoTIFF de entropia usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', entropia.archivo.geotiff,' ###','\n'))
  }

  proba.archivo.grafica <- paste0(modelos.incertidumbre.figuras,'/3_',gsub('.tif$','.png',basename(proba.archivo.geotiff)))
  if (!file.exists(proba.archivo.grafica)){
    ##### output messages ####
    cat(paste0('### RESULTADO 6 de ', pasos_total, ' : La figura de probabilidades de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', proba.archivo.grafica,' ###','\n'))
    ##### end output messages ####
    r <- stack(proba.archivo.geotiff)
    dim1 <- round(length(names(r))/3,0)
    dim2 <- 3

    n<-100
      
    colr = viridis::viridis(n, direction=-1, begin = 0, end = 1)
    names(r) <- levels(train.data$target)
    titles <- gsub('_','\n',levels(train.data$target))

    dev.new(height=0.91*nrow(r)/50, width=1.09*ncol(r)/50)
    png(file = proba.archivo.grafica, width = 1200, height = 900, res=150)
    p <- levelplot(r, panel=panel.levelplot.raster, margin=F, names.attr=titles,
              at=seq(0, 1, length.out=n), col.regions = colr,
              par.strip.text=list(font=2, cex=0.5, lines=6),
              par.settings = list(axis.line = list(col = 'transparent'),
                                  strip.background = list(col = 'transparent'),
                                  strip.border = list(col = 'transparent')),
              main=list('Mapa de Probabilidades por clase: 0 (baja) a 1 (alta)'),
              scales = list(col = 'transparent'), colorkey=list(space="bottom",  height = 1, width = 1))
    print(p)
    dev.off()
      
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 6 de ', pasos_total, ' : La figura de probabilidades de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', proba.archivo.grafica,' ###','\n'))
    ##### end output messages ####
  }

  entropia.archivo.grafica <- paste0(modelos.incertidumbre.figuras,'/4_',gsub('.tif$','.png',basename(entropia.archivo.geotiff)))
  if (!file.exists(entropia.archivo.grafica)){
    ##### output messages ####
    cat(paste0('### RESULTADO 7 de ', pasos_total, ' : La figura de entropia de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', entropia.archivo.grafica,' ###','\n'))
    ##### end output messages ####
      r <- raster(entropia.archivo.geotiff)

    limite_shp <- st_read(in.limite.data)
    r_res <- crop(r,limite_shp)
    r_res <- mask(r_res,limite_shp)

    r <- r_res
    p <- ggplot() +
      annotation_map_tile(zoomin = -1) +
      layer_spatial(r, aes(fill = stat(band1)), alpha = 0.7) +
      scale_fill_viridis_c(name='Entropia',na.value = NA, direction=-1, option="inferno", alpha = 0.7,  limits = c(0, 1)) +
      annotation_scale(location = "tl") +
      annotation_north_arrow(location = "br", which_north = "true")
      
    png(file = entropia.archivo.grafica, width = 700, height = 600, res=150)
    print(p)
    #print(plot(r, main = paste0('Incertidumbre (promedio) - ',VarObj), col=pal))
    dev.off()
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 7 de ', pasos_total, ' : La figura de entropia de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', entropia.archivo.grafica,' ###','\n'))
    ##### end output messages ####
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))
