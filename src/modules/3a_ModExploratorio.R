#############################################################################
# titulo        : Exploracion (descriptivo, grafifcos y analisis estadisticos)
#                 de los datos de entrada;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# proposito     : Explorar los datos de entrada, de entrenamiento y evaluacion;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : Particicion Datos de Entrenamiento y Evaluacion;
# salida        : Tabllas y gr�ficas de los analisis descriptivo y estadistico
# observaciones : ninguna;
##############################################################################

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  DescCategoricas <- function(covar,data,metadata_categoricas){
     if (covar != 'target'){
       clases <- factor(data[data$particion == 'train', covar])
       global_metadatos <- metadata_categoricas[[covar]]
       clases.tabla <- as.vector(global_metadatos[global_metadatos$ID %in% clases,'GRUPO'])
     } else{
      clases.tabla <- sort(unique(data[data$particion == 'train', covar]))
    }
  
    freqCat <- data.frame(table(data[data$particion == 'train', covar]))
    out_tb <- data.frame(Clase=clases.tabla, Frecuencia=freqCat$Freq)
    out_tb <- out_tb[order(out_tb$Frecuencia, decreasing = TRUE),]
    return(out_tb)
  }
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(ggplot2, tidyr, PerformanceAnalytics, stringr, caret,
                                 pals, psych, purrr, nortest, tseries, rstatix, PMCMR,
                                 rcompanion, multcompView))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()
  
  ## Uso de 10 variables predictoras
  rfe_lim <- 10
  # Definicion de la variable objetivo
  VarObj <- "tipo"
  
  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[['proyecto.carpeta']]
  project.covars.vector <- conf.args[['covariables.vector']]
  project.covars.vector <- unlist(strsplit(project.covars.vector,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  metadatos.categoricas <- paste0(proyecto.directorio,'/datos/raster')
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/exploratorio/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  carpeta.descriptiva = paste0(modelos.salida,'/descriptivo')
  dir.create(carpeta.descriptiva, recursive = T, mode = "0777", showWarnings = F)
  carpeta.graficos = paste0(modelos.salida,'/graficos')
  dir.create(carpeta.graficos, recursive = T, mode = "0777", showWarnings = F)
  carpeta.estadistico = paste0(modelos.salida,'/estadistico')
  dir.create(carpeta.estadistico, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particion
  load(paste0(modelos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub('train.', "", names(train.data))
  train.data$particion = 'train'
  test.data <- as.data.frame(particion['test'])
  names(test.data) <- sub('test.', "", names(test.data))
  test.data$particion = 'test'

  data = rbind(train.data,test.data)

  # Cargar rfe
  load(paste0(exploratorio.variables,'/rfe.rds'))

  # ------------------------------------------------------- #
  # Graficar segun tipo de variable
  # ------------------------------------------------------- #  
  
    covars = predictors(rfmodel)[1:rfe_lim]
    
    #covars = gsub('_','-',covars)
    
    covars_categoricas = project.covars.vector[project.covars.vector %in% covars]
    covars_continuas = covars[!covars %in% project.covars.vector]

    # descriptivo variable objetivo
    #metadata_categoricas <- list()
    desc_target <- DescCategoricas('target',data,metadata_categoricas)
    
    write.csv(desc_target,paste0(carpeta.descriptiva,'/1_descriptivo_variableobjetivo.csv'),row.names = FALSE)

    # descriptivo covariables continuas
    desc_continuas <- psych::describe(data[data$particion == 'train', covars_continuas])
    desc_continuas$vars <- covars_continuas
    write.csv(desc_continuas,paste0(carpeta.descriptiva,'/2_descriptivo_covariables-continuas.csv'),row.names = FALSE)

    if (length(covars_categoricas) > 0){

      metadata_categoricas = list()
      for (i in covars_categoricas){
        metadata_categoricas[[i]] <- read.csv(paste0(metadatos.categoricas,'/',i,'/',i,'.csv'), encoding="UTF-8")
      }

      # descriptivo covariables categoricas
      desc_categoricas <- lapply(covars_categoricas, function(x){DescCategoricas(x, data, metadata_categoricas)})
      names(desc_categoricas) <- covars_categoricas

      # Exportar tablas por separado
      lapply(names(desc_categoricas), function(x) write.csv(desc_categoricas[[x]],
        file =paste0(carpeta.descriptiva,'/3_descriptivo_covariable-categorica_',x,'.csv'), row.names=FALSE))
    }

    # directorios
    carpeta.barras = paste0(carpeta.graficos,'/1_barras')
    dir.create(carpeta.barras, recursive = T, mode = "0777", showWarnings = F)
    carpeta.boxplot = paste0(carpeta.graficos,'/2_boxplot')
    dir.create(carpeta.boxplot, recursive = T, mode = "0777", showWarnings = F)
    carpeta.correlacion = paste0(carpeta.graficos,'/3_correlacion')
    dir.create(carpeta.correlacion, recursive = T, mode = "0777", showWarnings = F)

    # lista de graficos
    # lista de graficos boxplots
    if (length(covars_continuas) > 0){
      lista.graficos.boxplots = sprintf('%s-boxplot', covars_continuas)
      lista.graficos = c('barras', lista.graficos.boxplots, 'train-correlationmatrix','test-correlationmatrix')
    } else{
      lista.graficos = c('barras','train-correlationmatrix','test-correlationmatrix')
    }

    lista.graficos.procesados = gsub("\\.png$", "", basename(list.files(path= modelos.salida, pattern = "\\.png$", recursive = T)))

    lista.graficos.faltantes <- setdiff(lista.graficos,lista.graficos.procesados)

    if (length(lista.graficos.faltantes) > 0){
      for (j in lista.graficos.faltantes){
        if (endsWith(j, 'correlationmatrix')){
          particion <- sapply(strsplit(j, "-", fixed = TRUE), "[", 1)
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == particion, covars_continuas])
          par(xpd=TRUE)
          dev.off()
        } else if (endsWith(j, 'boxplot')){
          covar_cont <- sapply(strsplit(j, "-", fixed = TRUE), "[", 1)
          clases.grafica <- sort(unique(data[data$particion == 'train', 'target']))
          png(file = paste0(carpeta.boxplot,'/',covar_cont,'-boxplot.png'), width = 700, height = 400)
          p <- ggplot(data = data[data$particion == 'train', names(data)], aes_string(x='target', y=covar_cont, fill='target')) +
            geom_boxplot() +
            ylab(covar_cont) +
            scale_fill_discrete(name=VarObj, labels=clases.grafica) +
            ggtitle(paste0('Boxplot ',VarObj, ' (x) vs ', covar_cont, ' (y)'))+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
            print(p)
          dev.off()
        } else if (endsWith(j, 'barras')){
          data_sorted <- within(data[data$particion == 'train', names(data)], target<- factor(target,
                                         levels=names(sort(table(target),
                                                           decreasing=TRUE))))

          png(file = paste0(carpeta.barras,'/',VarObj,'-barras.png'), width = 700, height = 400)
          p <- ggplot(data=data_sorted, aes(x=target)) +
            geom_bar()+
            ggtitle(paste0('Diagrama Barras ',VarObj))+
            theme(axis.text.x = element_text(angle = 90,hjust = 1))+
            labs(x="", y="Frecuencia")+
            guides(fill=F)
          print(p)
          dev.off()
        }
      }
    }

    # Estadistico
    target_dataset = data[data$particion == 'train',]

    ## Kruskal wallis
    KWdata <- function(covar){
      kw <- kruskal.test(target_dataset[,'target']~target_dataset[,covar])
      out_tb <- data.frame(covariable=covar, chi.squared=kw$statistic, pvalue=kw$p.value)
      return(out_tb)
    }

    KW.resultados <- lapply(covars, function(x){KWdata(x)})
    KW.resultados <- do.call('rbind', KW.resultados)
    # agregar significancia
    KW.resultados <- KW.resultados %>% mutate(Significancia = case_when(
      pvalue < 0.0001  ~ "**** <0,0001",
      pvalue < 0.001  ~ "*** <0,001",
      pvalue < 0.01   ~ "** <0,01",
      pvalue < 0.05   ~ "*<0,05",
      TRUE ~ "NS"
    ))

    # Exportar resultados kruskal
    write.csv(KW.resultados,paste0(carpeta.estadistico,'/1_kruskal_grupos.csv'),row.names=FALSE)

    ## Posthoc
    covar.signif <- KW.resultados[which(KW.resultados['pvalue']<0.05),'covariable']
    continuas_posthoc = covar.signif[covar.signif %in% covars_continuas]
    if (length(continuas_posthoc) > 0){
      postHOCdata <- function(covar){
        clases = levels(data[data$particion == 'train', 'target'])

        phoc <- pairwise.wilcox.test(target_dataset[,covar],target_dataset[,'target'],p.adjust.method ="BH",
                       paired = FALSE)
        # Cambiar formarto y asignar nombres
        PT <- phoc$p.value
        PT1 <- data.frame(rcompanion::fullPTable(PT))
        colnames(PT1) <- clases
        rownames(PT1) <- clases

        PT1_levels <- PT1 %>% mutate_at(vars(colnames(PT1)),  ~ case_when(. < 0.001 ~ "*** <0,001",
                                                                          . < 0.01   ~ "** <0,01",
                                                                          . < 0.05   ~ "* <0,05",
                                                                          TRUE ~ "NS"))
        rownames(PT1_levels) <- clases

        output <- list(PT1,PT1_levels)

        return(output)
      }

      phoc.resultados <- lapply(continuas_posthoc, function(x){postHOCdata(x)})
      names(phoc.resultados) <- continuas_posthoc

      # Chi-cuadrado
      ##Ho: no existe ninguna asociacion entre dos variables categóricas(son independientes).
      ##Ha: existe asociacion entre dos variables categóricas (hay dependencia).
      Chidata <- function(covar){
        chi <- chisq.test(data[data$particion == 'train', 'target'],data[data$particion == 'train', covar])
        out_tb <- data.frame(covariable=covar, chi.squared=chi$statistic, pvalue=chi$p.value)
        return(out_tb)
      }
      chi.resultados <- lapply(covars_categoricas, function(x){Chidata(x)})
      chi.resultados <- do.call('rbind', chi.resultados)
      # agregar significancia
      chi.resultados <- chi.resultados %>% mutate(Significancia = case_when(
        pvalue < 0.0001  ~ "**** <0,0001",
        pvalue < 0.001  ~ "*** <0,001",
        pvalue < 0.01   ~ "** <0,01",
        pvalue < 0.05   ~ "*<0,05",
        TRUE ~ "NS"
      ))

      # Export posthoc tabla y boxplot
      lapply(names(phoc.resultados), function(x) write.csv(phoc.resultados[[x]][1],
                                                           file =paste0(carpeta.estadistico,'/2a_posthoc-valores_',x,'.csv'), row.names=TRUE))

      lapply(names(phoc.resultados), function(x) write.csv(phoc.resultados[[x]][2],
                                                           file =paste0(carpeta.estadistico,'/2b_posthoc-significancia_',x,'.csv'), row.names=TRUE))

      # Exportar resultados Chi-cuadrado
      write.csv(chi.resultados,paste0(carpeta.estadistico,'/3_chi-cuadrado_c.csv'),row.names=FALSE)
    }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))