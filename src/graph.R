#############################################################################
# titulo        : Generacion de graficos para el documento;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Generar graficas como soporte a la documentacion
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
  
  suppressMessages(pacman::p_load(cowplot,reshape2,dplyr,ggcorrplot,tidyverse))
  
  # #Funciones
  # r.dir <- gsub('\\\\', '/', r.dir)
  # source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  # exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  # # Cargar rfe
  # load(paste0(exploratorio.variables,'/rfe.rds'))
  
  resultado <- as.data.frame(rfmodel["results"])
  
  ############  1. Plot resultado RF-RFE ############
  a <- ggplot(data=resultado, aes(x=results.Variables, y=results.Accuracy))+
        geom_line(color = "#00BFC4")+
        geom_point(color = "#00BFC4",size=2)+
        geom_point(aes(x=10,y=0.8544509),color = "#F8766D",size=2)+
        #labs(tittle = "Precisión según el número de variables explicativas")+
        theme(
          plot.title = element_text(size=8, hjust=0.5,),
          axis.title.x = element_text(size = 8),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_text(size = 8))+
        labs( title = "Accuracy según el número de variables explicativas",
              y = "Accuracy - validación cruzada", 
              x = "Número de variables")
  #plot(a)
  
  ############  2. boxplot variables "dem","co_distancef","co_distancev" ############
  datosbp <- train.data
  distancev <- as.data.frame(datosbp[c("target","dem","co_distancef","co_distancev")])
  distancelon <- distancev %>%
    rename("elevacion"="dem")
  distancevar <- reshape2::melt(distancev, id="target")
  
  b <- ggplot(distancevar, aes(x = variable, y = value, color = target)) +
    geom_boxplot()+
    labs( title = "Gráficos boxplot de las variables predictoras continuas vs la variable objetivo",
          y = "metros")+
    theme(
      plot.title = element_text(size=11, hjust=0.5,),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 11))
    
  #plot(b)
  
  ############  3. boxplot variables "co_ndvi","co_sbi","co_tcurvature" ############
  indicev <- as.data.frame(datosbp[c("target","co_ndvi","co_sbi","co_tcurvature")])
  indicevar <- reshape2::melt(indicev, id="target")
  c <- ggplot(indicevar, aes(x = variable, y = value, color = target)) +
    geom_boxplot()+
    labs( y = "valor",  x = "Variable predictora")+
    theme(
      axis.text.x = element_text(size =10),
      axis.title.x = element_text(size =11),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 11))
  
  #plot(c)
  
  ############ boxplot variables continuas gracifos 2 y 3 #####################
  plot_grid(b, c, labels=c("a", "b"), ncol = 1, nrow = 2)
  
  
  ########### matriz de correlacion variables continuas ###############
  continuas <- as.data.frame(datosbp[c("dem","co_distancef","co_distancev",
                                       "co_ndvi","co_sbi","co_tcurvature")])
  corr <- round(cor(continuas), 2)
  head(corr[, 1:6])
  ggcorrplot(corr,hc.order =TRUE,type ="lower", lab =TRUE,
             colors = c("#F8766D","white","#00BFC4"))+
    labs( title = "Correlación de Pearson - variables continuas")+
    theme(
      plot.title = element_text(size=10, hjust=0.5),
      axis.text.x = element_text(size = 9,angle = 90, vjust = 0.5, hjust=1),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_blank(),
      legend.text = element_text(size=9),
      legend.title = element_text(size=9))
    
  ########## boxplot accuracy de los modelos ###########
  accuracy <- resamps[["values"]][c("m_C5.0~Accuracy","m_ranger~Accuracy","m_mlp~Accuracy",
                                    "m_svmLinear~Accuracy","m_svmRadial~Accuracy",
                                    "m_xgbTree~Accuracy","Resample")]
  
  accuracy <- rename(accuracy,c("C50"="m_C5.0~Accuracy",
                                "ranger"="m_ranger~Accuracy",
                                "mlp"="m_mlp~Accuracy",
                                "svmLinerar"="m_svmLinear~Accuracy",
                                "svmRadial"="m_svmRadial~Accuracy",
                                "xgbTree"="m_xgbTree~Accuracy"))
  
  accuracylong <- reshape2::melt(accuracy,id="Resample")

  ac <- ggplot(accuracylong, aes(x = value, y = variable)) +
      geom_boxplot(color = "#00BFC4")+
      labs( title = "Accuracy")+
      theme(
        plot.title = element_text(size=10, hjust=0.5,),
        #axis.text.x = element_text(size = 10),
        #axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
        )
  #plot(ac)
  
  ########## boxplot indice kappa de los modelos ###########
  
  kappai <- resamps[["values"]][c("m_C5.0~Kappa","m_ranger~Kappa","m_mlp~Kappa",
                                    "m_svmLinear~Kappa","m_svmRadial~Kappa",
                                    "m_xgbTree~Kappa","Resample")]
  kappai <- rename(kappai,c("C50"="m_C5.0~Kappa",
                            "ranger"="m_ranger~Kappa",
                            "mlp"="m_mlp~Kappa",
                            "svmLinerar"="m_svmLinear~Kappa",
                            "svmRadial"="m_svmRadial~Kappa",
                            "xgbTree"="m_xgbTree~Kappa"))
  
  kappailong <- reshape2::melt(kappai,id="Resample")
  
  kp <- ggplot(kappailong, aes(x = value, y = variable)) +
    geom_boxplot(color = "#00BFC4")+
    labs( title = "Índice Kappa")+
    theme(
      plot.title = element_text(size=10, hjust=0.5,),
      #axis.text.x = element_text(size = 10),
      #axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  #plot(kp)
  plot_grid(ac, kp, labels=c("a", "b"), ncol = 2, nrow = 1)