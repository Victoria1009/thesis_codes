#############################################################################
# titulo        : Entrenamiento de modelos
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Entrenar varios modelos de Aprendizaje de Maquinas;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Datos de entrenamiento;
# salida        : Modelos entrenados;
# observaciones : Los modelos usados viene por DEFECTO de la libreria caret o 
#                 listados en el archivo config.txt;
##############################################################################

#
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))
  source(paste0(r.dir,'/functions/3_ConfigModelos.R'))

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(raster, rgdal, caret, stringr,doParallel, purrr,
                                  MLmetrics))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Remover espacio en blanco de la variable
  
  ## Uso de 10 variables predictoras
  rfe_lim <- 10
  # Definicion de la variable objetivo
  VarObj <- "tipo"

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  
  proyecto.modelos.categoricas <- conf.args[['modelos.categoricas']]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))

  proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
  proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))

  # Modelos disponibles y configuraciones
  configuracion <- modelos.config.manual()
  modelos.lista <- configuracion[['modelos.dict']]
  tuneLenght <- configuracion[['tuneLenght']]

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

 # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particion
  load(paste0(modelos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  # Cargar rfe
  load(paste0(exploratorio.variables,'/rfe.rds'))

  modelos.listag = mapply(Add, proyecto.modelos.categoricas, proyecto.modelos.categoricas)
  #listmodelos = modelos.lista
  modelos.lista = modelos.listag[1:6]

  modelos.idx <- match(proyecto.modelos.categoricas,names(modelos.lista))

  modelos.objetivo <- modelos.lista[modelos.idx]
  modelos.objetivo <- na.omit(modelos.objetivo) 

    # ------------------------------------------------------- #
    # Modelos
    # ------------------------------------------------------- #
    # Crear formula
    fm <- as.formula(paste("target~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables segun usuario

        fitControl <- trainControl(method = "cv", #verificar tecnicas repeatedcv
                          number=10,
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          returnResamp = "all",
                          savePredictions = TRUE,
                          search = "random",
                          verboseIter = FALSE
        )

      for (modelo in names(modelos.objetivo)){
        #modelos.salida.temp <- paste0(modelos.salida,'/',listmodelos)
        modelos.salida.temp <- paste0(modelos.salida,'/',modelo)
        dir.create(modelos.salida.temp, recursive = T, mode = "0777", showWarnings = F)
        modelo.archivo <- paste0(modelos.salida.temp,'/',modelo,'.rds')
        if (!file.exists(modelo.archivo)){
          cat(paste0('El modelo ',modelo,' con sampling no existe, se requiere entrenarlo antes de su evaluacion','\n','\n'))

          # Calculate the number of cores
          no_cores <- detectCores() - 1
          cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
          registerDoParallel(cl)                #register the cluster

          ## foreach or lapply would do this faster
          set.seed(40)

          model.alias <- modelos.lista[modelo]

          if (getModelInfo(model.alias)[[model.alias]]$label[1] %in% c('RandomForest','Random Forest')){
            modelo.ajuste <- train(fm, data = train.data,
            method=modelos.lista[modelo],
            tuneLength = tuneLenght[modelo],
            num.trees = 500,
            importance = "impurity",
            metric=proyecto.metricas.categoricas[1],
            trControl = fitControl)
          } else{
            modelo.ajuste <- train(fm, data = train.data,
            method=modelos.lista[modelo],
            tuneLength = tuneLenght[modelo],
            metric=proyecto.metricas.categoricas[1],
            importance = TRUE,
            preProc = c("center", "scale"),
            trControl = fitControl)
          }

          stopCluster(cl = cl)

          if(modelo == 'C45'){
            .jcache(modelo.ajuste$finalModel$classifier)
          }
            save(modelo.ajuste, file=modelo.archivo)
          } else {
            cat(paste0('El modelo ',modelo,' con sampling existe, se puede usar para identificar mejor modelos','\n','\n'))
          }
          }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))