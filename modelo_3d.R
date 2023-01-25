df4 <- janitor::clean_names(df3)

df5 <- na.omit(df4)

modelo <-
  baquantreg:::multBayesQR(response = c("depressao", "ansiedade", "estresse"),
                           formulaPred = ~ idade,
                           directionPoint = 5,
                           tau = 0.10,
                           dataFile = df5,
          itNum = 2000, burnin = 1000, thin = 1, chains = NULL, betaValue = NULL,
          sigmaValue = 1, vSampleInit = NULL, priorVar = 100, hyperSigma = c(0.1,
                                                                             0.1),
          refresh = 100, bayesx = TRUE, sigmaSampling = TRUE,
          quiet = T, tobit = FALSE, numCores = 1, recordLat = FALSE,
          check_bayesx = FALSE, path_bayesx = NULL,
          adaptive_dir = FALSE,
          outfile = "C:/Users/R2/Documents/rebeca/resultados/",
          dir.rm = FALSE)


points_2 <- baquantreg:::drawQuantileRegion_3D(modelo,
                                               datafile = df5,
                                               response = c("depressao", "ansiedade", "estresse"), ngridpoints = 100,
                                             xValue = c(1, 20),
                                             path_folder = "C:/Users/R2/Documents/rebeca/resultados/", print_plot = FALSE)

points_3 <- baquantreg:::drawQuantileRegion_3D(modelo,
                                               datafile = df5,
                                               response = c("depressao", "ansiedade", "estresse"), ngridpoints = 100,
                                               xValue = c(1, 22),
                                               path_folder = "C:/Users/R2/Documents/rebeca/resultados/", print_plot = FALSE)

points_2[[1]]$y1 |> summary()
library(plot3D)
lines3D(points_2[[1]]$y1,
        points_2[[1]]$y2,
        points_2[[1]]$y3)

