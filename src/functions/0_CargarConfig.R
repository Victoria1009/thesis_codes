#############################################################################
# titulo        : Datos del archivo config.txt;
# proposito     : Cargar los datos del archivo config.txt;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : config.txt;
# salida        : listado de variables para su uso en la herramienta;
# observaciones : ninguna;
##############################################################################

LoadConfig = function(x){ 
  conf.list <- lapply(strsplit(readLines(x, warn=FALSE)," "), as.character)

  #read target lines
  root.index <- grep("*proyecto.dir",conf.list)
  root.path = conf.list[[root.index]][[length(conf.list[[root.index]])]]

  #leer variables objetivo categoricas
  variables.categoricas.index <- grep("*variables.categoricas",conf.list)
  root.vars.categoricas = conf.list[[variables.categoricas.index]][[length(conf.list[[variables.categoricas.index]])]]
  
  #leer ajustes variables objetivo categoricas
  ajuste.categoricas.index <- grep("*categoricas.minobservaciones",conf.list)
  root.ajuste.categoricas = conf.list[[ajuste.categoricas.index]][[length(conf.list[[ajuste.categoricas.index]])]]

  #leer lista covariables en archivos raster
  covariables.raster.index <- grep("*covariables.raster",conf.list)
  root.covariables.raster = conf.list[[covariables.raster.index]][[length(conf.list[[covariables.raster.index]])]]

  #leer modelos segun variable
  modelos.categoricas.index <- grep("*modelos.categoricas",conf.list)
  root.modelos.categoricas = conf.list[[modelos.categoricas.index]][[length(conf.list[[modelos.categoricas.index]])]]
  
  #leer metricas segun variable
  metricas.categoricas.index <- grep("*metricas.categoricas",conf.list)
  root.metricas.categoricas = conf.list[[metricas.categoricas.index]][[length(conf.list[[metricas.categoricas.index]])]]

  #leer lista covariables en archivos vector
  covariables.vector.index <- grep("*covariables.vector",conf.list)
  root.covariables.vector = conf.list[[covariables.vector.index]][[length(conf.list[[covariables.vector.index]])]]
  
  #leer atributos covariables en archivos vector
  covariables.atributos.index <- grep("*vector.atributos",conf.list)
  root.covariables.atributos = conf.list[[covariables.atributos.index]][[length(conf.list[[covariables.atributos.index]])]]
  
  #lista para exportar de la funcion

  newlist = list(root.path, root.vars.categoricas, root.ajuste.categoricas, 
                 root.covariables.raster,root.modelos.categoricas,root.metricas.categoricas,
                 root.covariables.vector, root.covariables.atributos)

  names(newlist) = c('proyecto.carpeta', 'vars.categoricas','ajuste.categoricas',
                     'covariables.raster','modelos.categoricas','metricas.categoricas',
                     'covariables.vector','vector.atributos')

  return(newlist)
}

LoadConfig(conf.file)