#############################################################################
# titulo        : Seleccion de variables para modelado;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Usar metodos de seleccion de variables para identificar variables 
#                 optimas para el modelado segun la variable objetivo;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Matriz de Datos de entrada;
# salida        : Graficos y modelos RFE y Boruta con las covariables optimas segun 
#                 la variable objetivo;
# observaciones : ninguna;
##############################################################################

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, doParallel, snow, randomForest, Boruta, 
                                  stringr, dplyr, e1071))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/2_Outliers.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Declaracion variable objetivo
  VarObj <- "tipo"

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[['proyecto.carpeta']]

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables.rds = paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.rds, recursive = T, mode = "0777", showWarnings = F)
  exploratorio.variables.figuras = paste0(proyecto.directorio,'/exploratorio/figuras/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.particion.datos = paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.particion.datos, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  datos.entrada <- paste0(proyecto.directorio,'/datos/salidas')

  # Cargar matrix observaciones
  matriz <- read.csv(paste0(datos.entrada,'/matriz/MatrixDatos.csv'),sep=',')

  covariables <- readRDS(paste0(datos.entrada,'/covariables/covariables.rds'))

  final_df <- data.frame(matriz[,c('id','X','Y')],target=matriz[,VarObj],matriz[,which(names(matriz)%in%covariables)])
  
  # final_df$ct_cobertura <- as.factor(final_df$ct_cobertura)
  # final_df$ct_litologia <- as.factor(final_df$ct_litologia)
  # final_df$ct_suelo <- as.factor(final_df$ct_suelo)
  # final_df$ct_ugm_p <- as.factor(final_df$ct_ugm_p)
  final_df$target <- as.factor(final_df$target)
  
  print(paste0('El numero inicial de perfiles de la matriz de datos es de ', dim(final_df)[1]))

  # identificar y remote outliers
  gooddata = computeOutliers(matriz[,covariables], type = 'remove')
  
  good_df_q95 = final_df[gooddata,]

  df_wnoise = good_df_q95

  # Remover variables con cero variabilidad
  df_wnoise[,nearZeroVar(df_wnoise)] = NULL

  # Datos finales
  data <- df_wnoise

  # Remover NAs - ##TODO eliminar variables con muchos NAs o eliminar registros
  data <- data[complete.cases(data), ]

  proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
  proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
  METRIC <- proyecto.metricas.categoricas[1]

  data_model <-data.frame(target=data[,'target'], data[,which(names(data) %in% covariables)])
  data_info <-data[,c('id','X','Y')]
  print(paste0('El numero final de perfiles posterior a la limpieza de datos atipicos es de ', dim(data_model)[1]))
  
  ##Conjunto de datos para entrenamiento y para validacion
  set.seed(225)
  inTrain <- createDataPartition(y = data_model[,1], p = .70, list = FALSE)
  train_data <- as.data.frame(data_model[inTrain,])
  test_data <- as.data.frame(data_model[-inTrain,])
  particion <- list(train=train_data,test=test_data)
  save(particion, file=paste0(modelos.particion.datos,'/particion.RData'))
  write.csv(train_data, file=paste0(modelos.particion.datos,'/entrenamiento.csv'), row.names=TRUE)
  write.csv(test_data, file=paste0(modelos.particion.datos,'/evaluacion.csv'), row.names=TRUE)
  
  ##Exportar datos info coordenadas
  data_info[inTrain,'particion'] <- 'entrenamiento'
  data_info[-inTrain,'particion'] <- 'evaluacion'
  write.csv(data_info, file=paste0(modelos.particion.datos,'/coordenadas.csv'), row.names=FALSE)
  print(paste0('La particion de entrenamiento contiene ',dim(train_data)[1],' perfiles'))
  print(paste0('La particion de evaluacion contiene ',dim(test_data)[1],' perfiles'))

  #Definir muestras de entrenamiento
  data <- train_data
  ##Seleccion de variables --> RFE
  file_name <- 'rfe.rds'
  exploratorio.variables.rfe <- paste0(exploratorio.variables.rds,'/',file_name)
  if (!file.exists(exploratorio.variables.rfe)){
    cat(paste0('Ejecutando la seleccion de variables de la variable objetivo ',VarObj,' usando el algoritmo RFE'),'\n','\n')
    #set.seed(40)
    search_limit <- round(dim(data[,-1])[2]/2)
    search_diff <- round((dim(data[,-1])[2]-search_limit)/3)
    search_regular <- seq(search_limit, dim(data[,-1])[2], search_diff)
    if (search_regular[length(search_regular)] == dim(data[,-1])[2]){
      subset = c(1:search_limit,search_regular[-1])
    } else{
      subset = c(1:search_limit,search_regular[-1], dim(data[,-1])[2])
    }
    #para que sea reproducible (fuente: https://stackoverflow.com/questions/32290513/making-recursive-feature-elimination-using-caret-parallel-on-windows)
    seeds <- vector(mode = "list", length = 51)
    for(i in 1:50) seeds[[i]] <- sample.int(1000, length(subset) + 1)
    seeds[[51]] <- sample.int(1000, 1)
    
    #procesamiento en paralelo
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores, type = "SOCK")
    registerDoParallel(cl)
    control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5, seeds = seeds) #number=10, repeats=10 de acuerdo FAO sin embargo MGuevara usa 5 https://github.com/DSM-LAC/MEXICO/search?q=rfe
    (rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=subset, rfeControl=control2)) #sizes se refiere al detalle de la curva,
    stopCluster(cl = cl)

    #exportar imagen
    png(file = paste0(exploratorio.variables.figuras,'/',stringr::str_replace(file_name,'.rds','.png')), width = 700, height = 550)
    print(plot(rfmodel, type=c("g", "o"), cex=2,cex.names = 2, metric = METRIC))
    dev.off()
    save(rfmodel, file=exploratorio.variables.rfe)
    
  } else {
    cat(paste0('El archivo RDS y figura de la seleccion de variables con el m??todo RFE de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.rfe)),'\n'))
  }

  file_name <- 'boruta.rds'
  exploratorio.variables.boruta <- paste0(exploratorio.variables.rds,'/',file_name)
  if (!file.exists(exploratorio.variables.boruta)){
    cat(paste0('Ejecutando la seleccion de variables de la variable objetivo ',VarObj,' usando el algoritmo Boruta'),'\n','\n')
    nCores <- detectCores() - 1
    set.seed(123)
    formula <- as.formula('target ~ .')
    (bor <- Boruta::Boruta(formula, data = data, doTrace = 0, num.threads = nCores, ntree = 30, maxRuns=500)) #se debe evaluar ntree (numero de arboles), maxRuns (cantidad de interacciones)
    save(bor, file=exploratorio.variables.boruta)
    png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
    par(mar = c(18, 4, 1, 1))
    plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
    dev.off()
  } else {
    cat(paste0('El archivo RDS y figura de la seleccion de variables con el m??todo Boruta de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.boruta)),'\n','\n'))
  }

  if (is(data$target,'factor')){
    #Definir muestras de entrenamiento
    set.seed(123)
    down_train <- downSample(x = train_data[, -ncol(train_data)],
                         y = train_data$target)
    down_train$Class <- NULL

    data <- down_train
    ##Seleccion de variables --> RFE
    file_name <- 'rfe_down.rds'
    exploratorio.variables.rfe <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.rfe)){
      cat(paste0('Ejecutando la seleccion de variables de la variable objetivo ',VarObj,' usando el algoritmo RFE con el dataset BALANCEADO'),'\n','\n')
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")
      registerDoParallel(cl)
      set.seed(40)
      control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5) #number=10, repeats=10 de acuerdo FAO sin embargo MGuevara usa 5 https://github.com/DSM-LAC/MEXICO/search?q=rfe
      search_limit <- round(dim(data[,-1])[2]/2)
      search_diff <- round((dim(data[,-1])[2]-search_limit)/3)
      search_regular <- seq(search_limit, dim(data[,-1])[2], search_diff)
      if (search_regular[length(search_regular)] == dim(data[,-1])[2]){
        subset = c(1:search_limit,search_regular[-1])
      } else{
        subset = c(1:search_limit,search_regular[-1], dim(data[,-1])[2])
      }
      (rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=subset, rfeControl=control2)) #sizes se refiere al detalle de la curva,
      stopCluster(cl = cl)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
      print(plot(rfmodel, type=c("g", "o"), cex=2,cex.names = 2, metric = METRIC))
      dev.off()
      predictors(rfmodel)[1:10]
      save(rfmodel, file=exploratorio.variables.rfe)
    } else {
      cat(paste0('El archivo RDS y figura de la seleccion de variables con el m??todo RFE de la variable objetivo ',VarObj,' con el dataset BALANCEADO ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.rfe)),'\n'))
    }

    file_name <- 'boruta_down.rds'
    exploratorio.variables.boruta <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.boruta)){
      cat(paste0('Ejecutando la seleccion de variables de la variable objetivo ',VarObj,' con el dataset BALANCEADO usando el algoritmo Boruta'),'\n','\n')
      nCores <- detectCores() - 1
      formula <- as.formula('target ~ .')
      (bor <- Boruta::Boruta(formula, data = data, doTrace = 0, num.threads = nCores, ntree = 30, maxRuns=500)) #se debe evaluar ntree (numero de arboles), maxRuns (cantidad de interacciones)
      save(bor, file=exploratorio.variables.boruta)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
      par(mar = c(18, 4, 1, 1))
      plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
      dev.off()
    } else {
      cat(paste0('El archivo RDS y figura de la seleccion de variables con el m??todo Boruta de la variable objetivo ',VarObj,'  con el dataset BALANCEADO ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.boruta)),'\n','\n'))
    }
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))