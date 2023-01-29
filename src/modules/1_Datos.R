#############################################################################
# titulo        : Datos de entrada (matriz) y variables ambientales (GeoTIFF);
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Generar datos de entrada (matriz) y raster multibanda con las 
#                 variables ambientales (GeoTIFF);
# autor(es)     : Victoria Daniela Camacho;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Inventario de eventos de deslizamientos (inventario.csv);
#                 raster y vectores tematicos
# salida        : Datos de entrada y GeoTIFF con las covariables ambientales para 
#                 su uso en la prediccion;
# observaciones : ninguna;
##############################################################################

# ------------------------------------------------------- #
# Librerias y funciones
# ------------------------------------------------------- #
# Librerias
if (!require('pacman')) install.packages('pacman');

suppressMessages(library(pacman))
suppressMessages(pacman::p_load(readxl, tidyr, plyr, dplyr, raster, GSIF, 
                                aqp, sf,rgdal,smoothr, gdalUtilities, 
                                magrittr, stringr, caret,gdalUtilities,rgeos,
                                tidyverse))

# iniciar el monitoreo tiempo de procesamiento total
timeStart <- Sys.time()

# Funciones
r.dir <- gsub('\\\\', '/', r.dir)
source(paste0(r.dir,'/functions/0_CargarConfig.R'))

# ------------------------------------------------------- #
# Cargar archivo de configuracion y componentes
# ------------------------------------------------------- #
# Cargar archivo configuracion
conf.args <- LoadConfig(conf.file)

# Cargar componentes relacionados con este script
project.folder <- conf.args[['proyecto.carpeta']]
project.name <- sapply(strsplit(project.folder, '_'), tail, 1)

project.covars.vector <- conf.args[['covariables.vector']]
project.covars.vector <- unlist(strsplit(project.covars.vector,';'))

project.covars.raster <- conf.args[['covariables.raster']]
project.covars.raster <- unlist(strsplit(project.covars.raster,';'))

project.covars.list <- c(project.covars.vector, project.covars.raster)

#configurar los atributos de los archivos vectoriales
project.vector.atributos <- conf.args[['vector.atributos']]
project.vector.atributos <- unlist(strsplit(project.vector.atributos,';'))
names(project.vector.atributos) <- project.covars.vector

# ------------------------------------------------------- #
# Directorios de trabajo
# ------------------------------------------------------- #
# Declarar directorios
in.tb.data <- paste0(project.folder,'/datos/bd')
in.geo.data <- paste0(project.folder,'/datos')
in.limite.data <- paste0(project.folder,'/datos/limite')
out.tb.data <- paste0(project.folder,'/datos/salidas/matriz')
out.geo.data <- paste0(project.folder,'/datos/salidas/covariables')
dir.create(out.tb.data, recursive = T, mode = "0777", showWarnings = F)
dir.create(out.geo.data, recursive = T, mode = "0777", showWarnings = F)

# Definir directorio de trabajo
setwd(paste0(project.folder))

# Matriz Datos
matriz.datos.archivo <- paste0(out.tb.data,'/','MatrixDatos.csv')
if (!file.exists(matriz.datos.archivo)){
  
  # Mensaje de estado
  cat(paste0('La matriz de datos no existe, se procede a generarla','\n','\n'))
  
  # Covariables
  covariables.archivo.stack <- paste0(out.geo.data,'/covariables.tif')
  if (!file.exists(covariables.archivo.stack)){
    cat(paste0('El archivo stack geoTIFF de las covariables no existe, se requiere generarlo para extraer la matriz de datos','\n','\n'))
    
    # Cargar area limite
    limite_shp <- st_read(in.limite.data)
    if (dim(limite_shp)[1] > 1){
      limite_shp$id <- 0
      limite_shp <- limite_shp %>% dplyr::group_by(id) %>% dplyr::summarize()
    }
    
    # DEM
    if ('dem' %in% project.covars.list){
      # Cargar DEM (referencia de la resolucion espacial)
      filename_dem <- list.files(path=paste0(in.geo.data,'/raster/dem'), pattern='tif$', all.files=FALSE, full.names=TRUE,recursive=TRUE)
      if (basename(filename_dem) != 'dem.tif'){
        stop(paste0('Asegurese que el nombre del archivo dem es dem.tif o que la ruta ', dirname(filename_dem),' NO esta vacia'))
      } else{
        cat(paste0('El archivo geoTIFF de la covariable DEM existe, se va agregar al stack de covariables','\n','\n'))
        DEM_rast_res <- raster(filename_dem)
        names(DEM_rast_res) <- 'dem'
        resolucion_dem = res(DEM_rast_res)[1]
      }
    } else{
      filename_dem <- paste0(in.geo.data,'/raster/dem/dem.tif')
      stop(paste0('Asegurese que declare la covariable dem en el config.txt y que su archivo dem.tif este en la ruta ', dirname(filename_dem)))
    }
    
    # Derivados del DEM
    if ('derivados' %in% project.covars.list){
      # Crear stack de derivados del DEM
      DEMderivados_lista <- list.files(path=paste0(in.geo.data,'/raster/derivados'), pattern='tif$', all.files=FALSE, full.names=TRUE,recursive=TRUE)
      if (length(DEMderivados_lista) > 0){
        cat(paste0('Los archivos geoTIFF de la covariables derivados del DEM existen, se van agregar al stack de covariables','\n','\n'))
        DEMderivados_rast_res <- stack(DEMderivados_lista)
      } else{
        stop(paste0('Asegurese que la ruta ', paste0(in.geo.data,'/derivados'),' contiene al menos un archivo derivado del DEM'))
      }
    }
    
    if (project.covars.list %in% project.covars.vector){
      for (covar in project.covars.vector){
        out.dir <- paste0(in.geo.data,'/raster/',covar)
        dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
        covariable.archivo <- paste0(out.dir,'/',covar,'.tif')
        if (!file.exists(covariable.archivo)){
          cat(paste0('El archivo geoTIFF de la covariable ', covar, ' no existe, se requiere generarlo para luego agregar al stack de covariables','\n','\n'))
          # Lista de shapefiles
          covar_files <- list.files(path = paste0(in.geo.data,'/vector/',covar), pattern = "\\.shp$", full.names=TRUE)
          
          if (length(covar_files) > 0){
            # Atributo
            covar_atributo <- project.vector.atributos[[covar]]
            
            if (length(covar_files) > 1){
              covar_list <- lapply(covar_files, readOGR)
              
              # Seleccionar y procesar variable objetivo
              covar_list_target <- lapply(covar_list, "[", c(covar_atributo))
              covar_combined <- do.call(what = rbind, args=covar_list_target)
              covar_intersect <- raster::intersect(covar_combined, limite_shp)
              covar_intersect@data[[covar_atributo]] = gsub(",", "", covar_intersect@data[[covar_atributo]])
              covar_intersect <- aggregate(covar_intersect, covar_atributo)
              covar_vector <- fill_holes(covar_intersect, units::set_units(1, km^2))
            } else {
              covar_vector <- readOGR(paste0(in.geo.data,'/vector/',covar))
              covar_vector <- raster::intersect(covar_vector, limite_shp)
            }
            
            ## Extraer y exportar metadata
            metada.archivo <- gsub('.tif','.csv',covariable.archivo)
            metadata <- data.frame('ID'=seq_along(unique(covar_vector@data[[covar_atributo]])),'GRUPO'=unique(covar_vector@data[[covar_atributo]]))
            write.csv(metadata, metada.archivo, row.names=FALSE)
            
            ## Rasterizar capa
            covar_vector <- spTransform(covar_vector, CRS=projection(DEM_rast_res))
            covar_vector$grupos <- as.factor(covar_vector@data[[covar_atributo]])##
            covar_raster <- raster::rasterize(covar_vector, DEM_rast_res, 'grupos')
            covar_raster_ngb <- resample(covar_raster, DEM_rast_res, method='ngb')
            values(covar_raster_ngb) <- round(values(covar_raster_ngb),0)
            
            names(covar_raster_ngb) <- covar
            assign(paste0(covar, '_rast_res'), covar_raster_ngb)
            
            # Guardar archivo
            writeRaster(covar_raster_ngb, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
          } else{
            stop(paste0('Asegurese que la ruta ', paste0(in.geo.data,'/vector/',covar),' contiene al menos un archivo vector (shapefile) para ser rasterizado'))
          }
        } else{
          cat(paste0('El archivo geoTIFF de la covariable ',  covar, ' existe, se va agregar al stack de covariables','\n','\n'))
          covar_raster_ngb <- raster(covariable.archivo)
          names(covar_raster_ngb) <- covar
          assign(paste0(covar, '_rast_res'), covar_raster_ngb)
        }
      }
    }
    # Generar COV tif
    list_obj = objects(pattern="*_rast_res")
    list_covs <- lapply(list_obj, function(x) get(x))
    covariables.nombres <- unlist(lapply(list_obj, function(x) names(get(x))))
    
    # Crear stack
    covariables <- stack(list_covs)
    #covariables <- crop(covariables,limite_shp)
    #covariables <- mask(covariables,limite_shp)
    names(covariables) <- covariables.nombres
    
    # Exportar GeoTIFF covariables
    writeRaster(covariables,covariables.archivo.stack, drivers = 'GeoTIFF', overwrite=TRUE)
    saveRDS(names(covariables),str_replace(covariables.archivo.stack,'.tif','.rds'))
  } else{
    cat(paste0('El archivo stack geoTIFF de las covariables existe, se va usar para extraer la matriz de datos','\n','\n'))
    covariables <- stack(covariables.archivo.stack)
    covariables.nombres <-readRDS(str_replace(covariables.archivo.stack,'.tif','.rds'))
    names(covariables) <- covariables.nombres
  }
  
  # Unir base de datos
  ##Carga de base con variables a modelar
  datos_final <- data.frame(read_delim(paste0(in.tb.data,'/','inventario.csv'), 
                                         delim = ";", escape_double = FALSE, 
                                         col_types = cols(Fecha = col_character()),trim_ws = TRUE))

  datos_sp <-st_as_sf(datos_final, coords=c("X","Y"), crs = 3116)

  # Extraer valores metodo normal raster
  matriz_datos <- cbind(datos_final,raster::extract(covariables, datos_sp))
  
  # Exportar Matriz de Datos
  write.table(matriz_datos, paste0(out.tb.data,'/','MatrixDatos.csv'), row.names = F, sep=',')
  
  } else{
    # Mensaje de estado
    cat(paste0('La matriz de datos existe, se puede usar para los otros componentes','\n','\n'))
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))