#############################################################################
# titulo        : Prediccion;
# proposito     : Funcion para generar predicciones;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Raster de covariables;
# salida        : Raster clasificado;
# observaciones : ninguna;
##############################################################################

# Funtion to compute the Shannon entropy ----
entropy <-
  function (x) {
    - sum(x * log(x, base = length(x)), na.rm = TRUE)
  }

# Function to compute the confusion index ----
confusion <-
  function (x) {
    1 - diff(sort(x, decreasing = TRUE)[2:1])
  }

PredictGeoTIFF = function(COV, fit.model, out.file, type, index, train.data)
{ 

  if (type %in% c('prob','raw')){
    data_type = 'FLT4S'
  } else{
    data_type = 'INT1U'
  }

  no_cores <- detectCores() - 1
  beginCluster(no_cores)
  clusterR(COV, predict, args = list(fit.model, type = type, index = index),
      filename = out.file, format = "GTiff",
      overwrite = T, datatype=data_type, options='COMPRESS=YES')
  endCluster()

}