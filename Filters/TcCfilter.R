TcCfilter<-function(imageRaster,param,eth, xi=0.9){
  
  if (dim(imageRaster)[1]<13 || dim(imageRaster)[1]<13) {
    stop("Please, insert an image with dimensions larger than 13 and repeat the
         process")
  }
  
  imagem_final<-imageRaster
  
  calculate_mean_complex <- function(window) {
    complex_values <- exp(1i * window)
    return(mean(complex_values))
  }
  
  filter_3x3_mod <- function(image) {
    filtered_image <- image
    for (i in 2:(nrow(image) - 1)) {
      for (j in 2:(ncol(image) - 1)) {
        window <- image[(i - 1):(i + 1), (j - 1):(j + 1)]
        sortedPhases <- sort(window)
        for (f in 1:length(sortedPhases)) {
          if(window[5] -  psi_epsilon < sortedPhases[f] || sortedPhases[f] <= window[5] + psi_epsilon){
            if (window[5] < sortedPhases[3] || window[5] > sortedPhases[7]) {
              filtered_image[i, j] <- mean(sortedPhases[3:7])
            }
          }
        }
      }
    }
    return(filtered_image)
  }
  
  
  s <- 11
  margen <- (s+1)/2  # Calculate the size of the margin for the neighborhood/lines
  margenm1 <- margen-1  # Calculate the margin minus 1/columns
  
  angulo_janela <- seq(0,19)*pi/20
  
  for (i in margen:(nrow(imageRaster) - margenm1)) {
    for (j in margen:(ncol(imageRaster) - margenm1)) {
      
      # 20 windows of size 11x11
      janela1<-janela2<-janela3<-janela4<-janela5<-janela6<-janela7<-janela8<-janela9<-janela10<-janela11<-janela12<-janela13<-janela14<-janela15<-janela16<-janela17<-janela18<-janela19<-janela20<-matrix(0, ncol=11,nrow=11)
      
      # 20 directional windows (i=rows, j=columns)
      #1
      janela1[5:7,1:11]<- imageRaster[(i-1):(i+1), (j-5):(j+5)]
      #2
      janela2[7,1:4]  <-imageRaster[(i+2), (j-5):(j-2)]
      janela2[6,1:7]  <-imageRaster[(i+1), (j-5):(j+1)]
      janela2[5,1:11] <-imageRaster[i, (j-5):(j+5)]
      janela2[4,5:11] <-imageRaster[(i-1), (j-1):(j+5)]
      janela2[3,8:11] <-imageRaster[(i-2), (j+2):(j+5)]
      #3
      janela3[3,10:11] <- imageRaster[(i+3), (j+4):(j+5)]
      janela3[4,8:11]  <- imageRaster[(i+2), (j+2):(j+5)]
      janela3[5,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      janela3[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      janela3[7,1:7]   <- imageRaster[(i+1), (j-5):(j+1)]
      janela3[8,1:4]   <- imageRaster[(i+2), (j-5):(j-2)]
      janela3[9,1:2]   <- imageRaster[(i+3), (j-5):(j-4)]
      #4
      janela4[2,11]    <- imageRaster[(i-4), (j+5)]
      janela4[3,10:11] <- imageRaster[(i-3), (j+4):(j+5)]
      janela4[4,8:11]  <- imageRaster[(i-2), (j+2):(j+5)]
      janela4[5,5:10]  <- imageRaster[(i-1), (j-1):(j+4)]
      janela4[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      janela4[7,2:7]   <- imageRaster[(i+1), (j-4):(j+1)]
      janela4[8,1:4]   <- imageRaster[(i+2), (j-5):(j-2)]
      janela4[9,1:2]   <- imageRaster[(i+3), (j-5):(j-4)]
      janela4[10,1]    <- imageRaster[(i+4), (j-5)]
      #5
      janela5[11,1]    <- imageRaster[(i+5), (j-5)]
      janela5[10,1:2]  <- imageRaster[(i+4), (j-5):(j-4)]
      janela5[9,1:3]   <- imageRaster[(i+3), (j-5):(j-3)]
      janela5[8,2:4]   <- imageRaster[(i+2), (j-4):(j-2)]
      janela5[7,3:7]   <- imageRaster[(i+1), (j-3):(j+1)]
      janela5[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      janela5[5,5:9]   <- imageRaster[(i-1), (j-1):(j+3)]
      janela5[4,8:10]  <- imageRaster[(i-2), (j+2):(j+4)]
      janela5[3,9:11]  <- imageRaster[(i-3), (j+3):(j+5)]
      janela5[2,10:11] <- imageRaster[(i-4), (j+4):(j+5)]
      janela5[1,11]    <- imageRaster[(i-5), (j+5)]
      #6
      janela6[11,1:2]  <- imageRaster[(i+5), (j-5):(j-4)]
      janela6[10,1:3]  <- imageRaster[(i+5), (j-5):(j-3)]
      janela6[9,2:4]   <- imageRaster[(i+3), (j-4):(j-2)]
      janela6[8,3:5]   <- imageRaster[(i+2), (j-3):(j-1)]
      janela6[7,4:6]   <- imageRaster[(i+1), (j-2):j]
      janela6[6,5:7]   <- imageRaster[i, (j-1):(j+1)]
      janela6[5,6:8]   <- imageRaster[(i-1), j:(j+2)]
      janela6[4,7:9]   <- imageRaster[(i-2), (j+1):(j+3)]
      janela6[3,8:10]  <- imageRaster[(i-3), (j+2):(j+4)]
      janela6[2,9:11]  <- imageRaster[(i-3), (j+3):(j+5)]
      janela6[1,10:11] <- imageRaster[(i-5), (j+4):(j+5)]
      #7
      janela7[11,1:3] <- imageRaster[(i+5), (j-5):(j-3)]
      janela7[10,2:4] <- imageRaster[(i+4), (j-4):(j-2)]
      janela7[9,3:5]  <- imageRaster[(i+3), (j-3):(j-1)]
      janela7[8,3:5]  <- imageRaster[(i+2), (j-3):(j-1)]
      janela7[7,3:6]  <- imageRaster[(i+1), (j-3):j]
      janela7[6,4:8]  <- imageRaster[i, (j-2):(j+2)]
      janela7[5,6:9]  <- imageRaster[(i-1), j:(j+3)]
      janela7[4,7:9]  <- imageRaster[(i-2), (j+1):(j+3)]
      janela7[3,7:9]  <- imageRaster[(i-3), (j+1):(j+3)]
      janela7[2,8:10] <- imageRaster[(i-4), (j+2):(j+4)]
      janela7[1,9:11] <- imageRaster[(i-5), (j+3):(j+5)]
      #8
      janela8[11,2:4] <- imageRaster[(i+5), (j-4):(j-2)]
      janela8[10,3:5] <- imageRaster[(i+4), (j-3):(j-1)]
      janela8[9,4:5]  <- imageRaster[(i+3), (j-2):(j-1)]
      janela8[8,4:5]  <- imageRaster[(i+2), (j-2):(j-1)]
      janela8[7,4:6]  <- imageRaster[(i+1), (j-2):j]
      janela8[6,5:8]  <- imageRaster[i, (j-1):(j+2)]
      janela8[5,6:8]  <- imageRaster[(i-1), j:(j+2)]
      janela8[4,7:8]  <- imageRaster[(i-2), (j+1):(j+2)]
      janela8[3,7:8]  <- imageRaster[(i-3), (j+1):(j+2)]
      janela8[2,7:9]  <- imageRaster[(i-4), (j+1):(j+3)]
      janela8[1,8:10] <- imageRaster[(i-5), (j+2):(j+4)]
      #9
      janela9[11,3:5] <- imageRaster[(i+5), (j-3):(j-1)]
      janela9[10,5:7] <- imageRaster[(i+4), (j-1):(j+1)]
      janela9[9,5:7]  <- imageRaster[(i+3), (j-1):(j+1)]
      janela9[8,6:7]  <- imageRaster[(i+2), j:(j+1)]
      janela9[7,6:7]  <- imageRaster[(i+1), j:(j+1)]
      janela9[6,6:8]  <- imageRaster[i, j:(j+2)]
      janela9[5,7:8]  <- imageRaster[(i-1), (j+1):(j+2)]
      janela9[4,7:8]  <- imageRaster[(i-2), (j+1):(j+2)]
      janela9[3,7:9]  <- imageRaster[(i-3), (j+1):(j+3)]
      janela9[2,7:9]  <- imageRaster[(i-4), (j+1):(j+3)]
      janela9[1,8:10] <- imageRaster[(i-5), (j+2):(j+4)]
      #10
      janela10[11,4:6] <- imageRaster[(i+5), (j-2):j]
      janela10[10,4:6] <- imageRaster[(i+4), (j-2):j]
      janela10[9,4:6] <- imageRaster[(i+3), (j-2):j]
      janela10[8,5:6] <- imageRaster[(i+2), (j-1):j]
      janela10[7,5:7] <- imageRaster[(i+1), (j-1):(j+1)]
      janela10[6,5:7] <- imageRaster[i, (j-1):(j+1)]
      janela10[5,5:7] <- imageRaster[(i-1), (j-1):(j+1)]
      janela10[4,6:7] <- imageRaster[(i-2), j:(j+1)]
      janela10[3,6:8] <- imageRaster[(i-3), j:(j+2)]
      janela10[2,6:8] <- imageRaster[(i-4), j:(j+2)]
      janela10[1,6:8] <- imageRaster[(i-5), j:(j+2)]
      #11
      janela11[1:11,5:7]<- imageRaster[(i-5):(i+5), (j-1):(j+1)]
      #12
      janela12[11,6:8] <- imageRaster[(i+5), j:(j+2)]
      janela12[10,6:8] <- imageRaster[(i+4), j:(j+2)]
      janela12[9,6:8]  <- imageRaster[(i+3), j:(j+2)]
      janela12[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      janela12[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      janela12[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela12[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      janela12[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      janela12[3,4:6]  <- imageRaster[(i-3), (j-2):j]
      janela12[2,4:6]  <- imageRaster[(i-4), (j-2):j]
      janela12[1,4:6]  <- imageRaster[(i-5), (j-2):j]
      #13
      janela13[11,7:9] <- imageRaster[(i+5), (j+1):(j+3)]
      janela13[10,7:9] <- imageRaster[(i+4), (j+1):(j+3)]
      janela13[9,7:8]  <- imageRaster[(i+3), (j+1):(j+2)]
      janela13[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      janela13[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      janela13[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela13[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      janela13[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      janela13[3,4:5]  <- imageRaster[(i-3), (j-2):(j-1)]
      janela13[2,3:5]  <- imageRaster[(i-4), (j-3):(j-1)]
      janela13[1,3:5]  <- imageRaster[(i-5), (j-3):(j-1)]
      #14
      janela14[11,8:10] <- imageRaster[(i+5), (j+2):(j+4)]
      janela14[10,7:9] <- imageRaster[(i+4), (j+1):(j+3)]
      janela14[9,7:8]  <- imageRaster[(i+3), (j+1):(j+2)]
      janela14[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      janela14[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      janela14[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela14[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      janela14[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      janela14[3,4:5]  <- imageRaster[(i-3), (j-2):(j-1)]
      janela14[2,3:5]  <- imageRaster[(i-4), (j-3):(j-1)]
      janela14[1,2:4]  <- imageRaster[(i-5), (j-4):(j-2)]
      #15
      janela15[11,9:11] <- imageRaster[(i+5), (j+3):(j+5)]
      janela15[10,8:10] <- imageRaster[(i+4), (j+2):(j+4)]
      janela15[9,7:9]  <- imageRaster[(i+3), (j+1):(j+3)]
      janela15[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      janela15[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      janela15[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela15[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      janela15[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      janela15[3,3:5]  <- imageRaster[(i-3), (j-3):(j-1)]
      janela15[2,2:4]  <- imageRaster[(i-4), (j-4):(j-2)]
      janela15[1,1:3]  <- imageRaster[(i-5), (j-5):(j-3)]
      #16
      janela16[11,10:11] <- imageRaster[(i+5), (j+4):(j+5)]
      janela16[10,9:11] <- imageRaster[(i+4), (j+3):(j+5)]
      janela16[9,8:10]  <- imageRaster[(i+3), (j+2):(j+4)]
      janela16[8,7:9]  <- imageRaster[(i+2), (j+1):(j+3)]
      janela16[7,6:8]  <- imageRaster[(i+1), j:(j+2)]
      janela16[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela16[5,4:6]  <- imageRaster[(i-1), (j-2):j]
      janela16[4,3:5]  <- imageRaster[(i-2), (j-3):(j-1)]
      janela16[3,2:4]  <- imageRaster[(i-3), (j-4):(j-2)]
      janela16[2,1:3]  <- imageRaster[(i-4), (j-5):(j-3)]
      janela16[1,1:2]  <- imageRaster[(i-5), (j-5):(j-4)]
      #17
      janela17[11,11] <- imageRaster[(i+5), (j+5)]
      janela17[10,10:11] <- imageRaster[(i+4), (j+4):(j+5)]
      janela17[9,7:11]  <- imageRaster[(i+3), (j+1):(j+5)]
      janela17[8,6:10]  <- imageRaster[(i+2), j:(j+4)]
      janela17[7,6:9]  <- imageRaster[(i+1), j:(j+3)]
      janela17[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela17[5,3:6]  <- imageRaster[(i-1), (j-3):j]
      janela17[4,2:6]  <- imageRaster[(i-2), (j-4):j]
      janela17[3,1:5]  <- imageRaster[(i-3), (j-5):(j-1)]
      janela17[2,1:2]  <- imageRaster[(i-4), (j-5):(j-4)]
      janela17[1,1]  <- imageRaster[(i-5), (j-5)]
      #18
      janela18[10,11] <- imageRaster[(i+4), (j+5)]
      janela18[9,10:11]  <- imageRaster[(i+3), (j+4):(j+5)]
      janela18[8,6:11]  <- imageRaster[(i+2), j:(j+5)]
      janela18[7,6:10]  <- imageRaster[(i+1), j:(j+4)]
      janela18[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      janela18[5,2:6]  <- imageRaster[(i-1), (j-4):j]
      janela18[4,1:6]  <- imageRaster[(i-2), (j-5):j]
      janela18[3,1:2]  <- imageRaster[(i-3), (j-5):(j-4)]
      janela18[2,1]  <- imageRaster[(i-4), (j-5)]
      #19
      janela19[9,11]  <- imageRaster[(i+3), (j+5)]
      janela19[8,9:11]  <- imageRaster[(i+2), (j+3):(j+5)]
      janela19[7,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      janela19[6,2:10]  <- imageRaster[i, (j-4):(j+4)]
      janela19[5,1:6]  <- imageRaster[(i-1), (j-5):j]
      janela19[4,1:3]  <- imageRaster[(i-2), (j-5):(j-3)]
      janela19[3,1]  <- imageRaster[(i-3), (j-5)]
      #20
      janela20[8,9:11]  <- imageRaster[(i+2), (j+3):(j+5)]
      janela20[7,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      janela20[6,1:11]  <- imageRaster[i, (j-5):(j+5)]
      janela20[5,1:7]  <- imageRaster[(i-1), (j-5):(j+1)]
      janela20[4,1:3]  <- imageRaster[(i-2), (j-5):(j-3)]
      
      todas_janelas<-list()
      
      todas_janelas[[1]]<-janela1
      todas_janelas[[2]]<-janela2
      todas_janelas[[3]]<-janela3
      todas_janelas[[4]]<-janela4
      todas_janelas[[5]]<-janela5
      todas_janelas[[6]]<-janela6
      todas_janelas[[7]]<-janela7
      todas_janelas[[8]]<-janela8
      todas_janelas[[9]]<-janela9
      todas_janelas[[10]]<-janela10
      todas_janelas[[11]]<-janela11
      todas_janelas[[12]]<-janela12
      todas_janelas[[13]]<-janela13
      todas_janelas[[14]]<-janela14
      todas_janelas[[15]]<-janela15
      todas_janelas[[16]]<-janela16
      todas_janelas[[17]]<-janela17
      todas_janelas[[18]]<-janela18
      todas_janelas[[19]]<-janela19
      todas_janelas[[20]]<-janela20
      
      # Averages of all the windows
      
      medias<-c(calculate_mean_complex(janela1),calculate_mean_complex(janela2),calculate_mean_complex(janela3),
                calculate_mean_complex(janela4),calculate_mean_complex(janela5),calculate_mean_complex(janela6),
                calculate_mean_complex(janela7),calculate_mean_complex(janela8),calculate_mean_complex(janela9),
                calculate_mean_complex(janela10),calculate_mean_complex(janela11),calculate_mean_complex(janela12),
                calculate_mean_complex(janela13),calculate_mean_complex(janela14),calculate_mean_complex(janela15),
                calculate_mean_complex(janela16),calculate_mean_complex(janela17),calculate_mean_complex(janela18),
                calculate_mean_complex(janela19),calculate_mean_complex(janela20))
      
      
      pos_janela_e_angulo<-which.max(Mod(medias))
      
      janela_selecionada<-todas_janelas[[pos_janela_e_angulo]]
      angulo_selecionado<-angulo_janela[pos_janela_e_angulo]
      
      
      if(max(Mod(medias))<eth){
        w <- 1 / sqrt((1:13 - 6)^2 + (1:13 - 6)^2)
        w[6] <- 0  # Delete masked pixels
        expValues <- exp(1i * angulo_selecionado)
        weightedSum <- sum(w * expValues)
        weightedSum <- weightedSum / sum(w)
        adjustedAngle <- Arg(weightedSum)
        diferencas<-(angulo_janela - adjustedAngle)
        indice_mais_proximo_de_zero <- which.min(abs(diferencas))
        janela_selecionada<-todas_janelas[[indice_mais_proximo_de_zero]]
        angulo_selecionado<-angulo_janela[indice_mais_proximo_de_zero]
      }
      
      integral_func <- function(l) {
        integrate(dTruncCauchy, lower = -l, upper = l, subdivisions = 100, param = param)$value
      }
      
      integral <- xi
      lower_limit <- -pi
      upper_limit <- pi
      psi_epsilon <- (-lower_limit + upper_limit) / 2
      valor_estimado<-integral_func(psi_epsilon)
      
      tolerance <- 0.001
      dm <- seq(0,pi,0.001)
      for (d in 1:length(dm)) {
        valor_estimado<-integral_func(dm[d])
        if(abs(valor_estimado - integral) < tolerance){
          psi_epsilon <- dm[d]
          break
        }
      }
      
      
      janela_selecionada_filtrada_3x3<-filter_3x3_mod(janela_selecionada)
      
      f <- function(janela_selecionada) {janela_selecionada * dTruncCauchy(janela_selecionada, param)}
      psi_bar <- integrate(f, lower = -psi_epsilon, upper = psi_epsilon, subdivisions = 100)$value
      
      f2 <- function(janela_selecionada){(janela_selecionada - psi_bar)^2 * dTruncCauchy(janela_selecionada, param)}
      sigma_v_squared <- integrate(f2, lower = -psi_epsilon, upper = psi_epsilon, subdivisions = 100)$value
      
      b <- 1- (sigma_v_squared/var(as.vector(janela_selecionada_filtrada_3x3)))
      b <- pmax(0, pmin(1, b)) # Limit b between 0 and 1
      
      psi_estimado<-mean(janela_selecionada) + b*(janela_selecionada-mean(janela_selecionada))
      janela_selecionada_filtrada_3x3b<-janela_selecionada_filtrada_3x3
      
      for (a in 1:ncol(janela_selecionada)) {
        for (b in 1:nrow(janela_selecionada)) {
          if(janela_selecionada_filtrada_3x3b[a,b]!=0){janela_selecionada_filtrada_3x3b[a,b]<-psi_estimado[a,b]}
        }
      }
      
      #1
      if(pos_janela_e_angulo==1){janela_selecionada_filtrada_3x3b[5:7,1:11]-> imagem_final[(i-1):(i+1), (j-5):(j+5)]}
      
      #2
      if(pos_janela_e_angulo==2){janela_selecionada_filtrada_3x3b[7,1:4]  ->imagem_final[(i+2), (j-5):(j-2)]
        janela_selecionada_filtrada_3x3b[6,1:7]  ->imagem_final[(i+1), (j-5):(j+1)]
        janela_selecionada_filtrada_3x3b[5,1:11] ->imagem_final[i, (j-5):(j+5)]
        janela_selecionada_filtrada_3x3b[4,5:11] ->imagem_final[(i-1), (j-1):(j+5)]
        janela_selecionada_filtrada_3x3b[3,8:11] ->imagem_final[(i-2), (j+2):(j+5)]}
      
      #3
      if(pos_janela_e_angulo==3){janela_selecionada_filtrada_3x3b[3,10:11] -> imagem_final[(i+3), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[4,8:11]  -> imagem_final[(i+2), (j+2):(j+5)]
        janela_selecionada_filtrada_3x3b[5,5:11]  -> imagem_final[(i+1), (j-1):(j+5)]
        janela_selecionada_filtrada_3x3b[6,4:8]   -> imagem_final[i, (j-2):(j+2)]
        janela_selecionada_filtrada_3x3b[7,1:7]   -> imagem_final[(i+1), (j-5):(j+1)]
        janela_selecionada_filtrada_3x3b[8,1:4]   -> imagem_final[(i+2), (j-5):(j-2)]
        janela_selecionada_filtrada_3x3b[9,1:2]   -> imagem_final[(i+3), (j-5):(j-4)]}
      
      #4
      if(pos_janela_e_angulo==4){janela_selecionada_filtrada_3x3b[2,11]    -> imagem_final[(i-4), (j+5)]
        janela_selecionada_filtrada_3x3b[3,10:11] -> imagem_final[(i-3), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[4,8:11]  -> imagem_final[(i-2), (j+2):(j+5)]
        janela_selecionada_filtrada_3x3b[5,5:10]  -> imagem_final[(i-1), (j-1):(j+4)]
        janela_selecionada_filtrada_3x3b[6,4:8]   -> imagem_final[i, (j-2):(j+2)]
        janela_selecionada_filtrada_3x3b[7,2:7]   -> imagem_final[(i+1), (j-4):(j+1)]
        janela_selecionada_filtrada_3x3b[8,1:4]   -> imagem_final[(i+2), (j-5):(j-2)]
        janela_selecionada_filtrada_3x3b[9,1:2]   -> imagem_final[(i+3), (j-5):(j-4)]
        janela_selecionada_filtrada_3x3b[10,1]    -> imagem_final[(i+4), (j-5)]}
      
      #5
      if(pos_janela_e_angulo==5){janela_selecionada_filtrada_3x3b[11,1]    -> imagem_final[(i+5), (j-5)]
        janela_selecionada_filtrada_3x3b[10,1:2]  -> imagem_final[(i+4), (j-5):(j-4)]
        janela_selecionada_filtrada_3x3b[9,1:3]   -> imagem_final[(i+3), (j-5):(j-3)]
        janela_selecionada_filtrada_3x3b[8,2:4]   -> imagem_final[(i+2), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[7,3:7]   -> imagem_final[(i+1), (j-3):(j+1)]
        janela_selecionada_filtrada_3x3b[6,4:8]   -> imagem_final[i, (j-2):(j+2)]
        janela_selecionada_filtrada_3x3b[5,5:9]   -> imagem_final[(i-1), (j-1):(j+3)]
        janela_selecionada_filtrada_3x3b[4,8:10]  -> imagem_final[(i-2), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[3,9:11]  -> imagem_final[(i-3), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[2,10:11] -> imagem_final[(i-4), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[1,11]    -> imagem_final[(i-5), (j+5)]}
      
      #6
      if(pos_janela_e_angulo==6){janela_selecionada_filtrada_3x3b[11,1:2]  -> imagem_final[(i+5), (j-5):(j-4)]
        janela_selecionada_filtrada_3x3b[10,1:3]  -> imagem_final[(i+5), (j-5):(j-3)]
        janela_selecionada_filtrada_3x3b[9,2:4]   -> imagem_final[(i+3), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[8,3:5]   -> imagem_final[(i+2), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[7,4:6]   -> imagem_final[(i+1), (j-2):j]
        janela_selecionada_filtrada_3x3b[6,5:7]   -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,6:8]   -> imagem_final[(i-1), j:(j+2)]
        janela_selecionada_filtrada_3x3b[4,7:9]   -> imagem_final[(i-2), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[3,8:10]  -> imagem_final[(i-3), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[2,9:11]  -> imagem_final[(i-3), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[1,10:11] -> imagem_final[(i-5), (j+4):(j+5)]}
      
      #7
      if(pos_janela_e_angulo==7){janela_selecionada_filtrada_3x3b[11,1:3] -> imagem_final[(i+5), (j-5):(j-3)]
        janela_selecionada_filtrada_3x3b[10,2:4] -> imagem_final[(i+4), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[9,3:5]  -> imagem_final[(i+3), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[8,3:5]  -> imagem_final[(i+2), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[7,3:6]  -> imagem_final[(i+1), (j-3):j]
        janela_selecionada_filtrada_3x3b[6,4:8]  -> imagem_final[i, (j-2):(j+2)]
        janela_selecionada_filtrada_3x3b[5,6:9]  -> imagem_final[(i-1), j:(j+3)]
        janela_selecionada_filtrada_3x3b[4,7:9]  -> imagem_final[(i-2), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[3,7:9]  -> imagem_final[(i-3), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[2,8:10] -> imagem_final[(i-4), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[1,9:11] -> imagem_final[(i-5), (j+3):(j+5)]}
      
      #8
      if(pos_janela_e_angulo==8){janela_selecionada_filtrada_3x3b[11,2:4] -> imagem_final[(i+5), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[10,3:5] -> imagem_final[(i+4), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[9,4:5]  -> imagem_final[(i+3), (j-2):(j-1)]
        janela_selecionada_filtrada_3x3b[8,4:5]  -> imagem_final[(i+2), (j-2):(j-1)]
        janela_selecionada_filtrada_3x3b[7,4:6]  -> imagem_final[(i+1), (j-2):j]
        janela_selecionada_filtrada_3x3b[6,5:8]  -> imagem_final[i, (j-1):(j+2)]
        janela_selecionada_filtrada_3x3b[5,6:8]  -> imagem_final[(i-1), j:(j+2)]
        janela_selecionada_filtrada_3x3b[4,7:8]  -> imagem_final[(i-2), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[3,7:8]  -> imagem_final[(i-3), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[2,7:9]  -> imagem_final[(i-4), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[1,8:10] -> imagem_final[(i-5), (j+2):(j+4)]}
      
      #9
      if(pos_janela_e_angulo==9){janela_selecionada_filtrada_3x3b[11,3:5] -> imagem_final[(i+5), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[10,5:7] -> imagem_final[(i+4), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[9,5:7]  -> imagem_final[(i+3), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[8,6:7]  -> imagem_final[(i+2), j:(j+1)]
        janela_selecionada_filtrada_3x3b[7,6:7]  -> imagem_final[(i+1), j:(j+1)]
        janela_selecionada_filtrada_3x3b[6,6:8]  -> imagem_final[i, j:(j+2)]
        janela_selecionada_filtrada_3x3b[5,7:8]  -> imagem_final[(i-1), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[4,7:8]  -> imagem_final[(i-2), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[3,7:9]  -> imagem_final[(i-3), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[2,7:9]  -> imagem_final[(i-4), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[1,8:10] -> imagem_final[(i-5), (j+2):(j+4)]}
      
      #10
      if(pos_janela_e_angulo==10){janela_selecionada_filtrada_3x3b[11,4:6] -> imagem_final[(i+5), (j-2):j]
        janela_selecionada_filtrada_3x3b[10,4:6] -> imagem_final[(i+4), (j-2):j]
        janela_selecionada_filtrada_3x3b[9,4:6] -> imagem_final[(i+3), (j-2):j]
        janela_selecionada_filtrada_3x3b[8,5:6] -> imagem_final[(i+2), (j-1):j]
        janela_selecionada_filtrada_3x3b[7,5:7] -> imagem_final[(i+1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[6,5:7] -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,5:7] -> imagem_final[(i-1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[4,6:7] -> imagem_final[(i-2), j:(j+1)]
        janela_selecionada_filtrada_3x3b[3,6:8] -> imagem_final[(i-3), j:(j+2)]
        janela_selecionada_filtrada_3x3b[2,6:8] -> imagem_final[(i-4), j:(j+2)]
        janela_selecionada_filtrada_3x3b[1,6:8] -> imagem_final[(i-5), j:(j+2)]}
      
      #11
      if(pos_janela_e_angulo==11){janela_selecionada_filtrada_3x3b[1:11,5:7]-> imagem_final[(i-5):(i+5), (j-1):(j+1)]}
      
      #12
      if(pos_janela_e_angulo==12){janela_selecionada_filtrada_3x3b[11,6:8] -> imagem_final[(i+5), j:(j+2)]
        janela_selecionada_filtrada_3x3b[10,6:8] -> imagem_final[(i+4), j:(j+2)]
        janela_selecionada_filtrada_3x3b[9,6:8]  -> imagem_final[(i+3), j:(j+2)]
        janela_selecionada_filtrada_3x3b[8,6:8]  -> imagem_final[(i+2), j:(j+2)]
        janela_selecionada_filtrada_3x3b[7,5:7]  -> imagem_final[(i+1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,5:7]  -> imagem_final[(i-1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[4,4:6]  -> imagem_final[(i-2), (j-2):j]
        janela_selecionada_filtrada_3x3b[3,4:6]  -> imagem_final[(i-3), (j-2):j]
        janela_selecionada_filtrada_3x3b[2,4:6]  -> imagem_final[(i-4), (j-2):j]
        janela_selecionada_filtrada_3x3b[1,4:6]  -> imagem_final[(i-5), (j-2):j]}
      
      #13
      if(pos_janela_e_angulo==13){janela_selecionada_filtrada_3x3b[11,7:9] -> imagem_final[(i+5), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[10,7:9] -> imagem_final[(i+4), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[9,7:8]  -> imagem_final[(i+3), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[8,6:8]  -> imagem_final[(i+2), j:(j+2)]
        janela_selecionada_filtrada_3x3b[7,5:7]  -> imagem_final[(i+1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,5:7]  -> imagem_final[(i-1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[4,4:6]  -> imagem_final[(i-2), (j-2):j]
        janela_selecionada_filtrada_3x3b[3,4:5]  -> imagem_final[(i-3), (j-2):(j-1)]
        janela_selecionada_filtrada_3x3b[2,3:5]  -> imagem_final[(i-4), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[1,3:5]  -> imagem_final[(i-5), (j-3):(j-1)]}
      
      #14
      if(pos_janela_e_angulo==14){janela_selecionada_filtrada_3x3b[11,8:10] -> imagem_final[(i+5), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[10,7:9] -> imagem_final[(i+4), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[9,7:8]  -> imagem_final[(i+3), (j+1):(j+2)]
        janela_selecionada_filtrada_3x3b[8,6:8]  -> imagem_final[(i+2), j:(j+2)]
        janela_selecionada_filtrada_3x3b[7,5:7]  -> imagem_final[(i+1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,5:7]  -> imagem_final[(i-1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[4,4:6]  -> imagem_final[(i-2), (j-2):j]
        janela_selecionada_filtrada_3x3b[3,4:5]  -> imagem_final[(i-3), (j-2):(j-1)]
        janela_selecionada_filtrada_3x3b[2,3:5]  -> imagem_final[(i-4), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[1,2:4]  -> imagem_final[(i-5), (j-4):(j-2)]}
      
      #15
      if(pos_janela_e_angulo==15){janela_selecionada_filtrada_3x3b[11,9:11] -> imagem_final[(i+5), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[10,8:10] -> imagem_final[(i+4), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[9,7:9]  -> imagem_final[(i+3), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[8,6:8]  -> imagem_final[(i+2), j:(j+2)]
        janela_selecionada_filtrada_3x3b[7,5:7]  -> imagem_final[(i+1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,5:7]  -> imagem_final[(i-1), (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[4,4:6]  -> imagem_final[(i-2), (j-2):j]
        janela_selecionada_filtrada_3x3b[3,3:5]  -> imagem_final[(i-3), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[2,2:4]  -> imagem_final[(i-4), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[1,1:3]  -> imagem_final[(i-5), (j-5):(j-3)]}
      
      #16
      if(pos_janela_e_angulo==16){janela_selecionada_filtrada_3x3b[11,10:11] -> imagem_final[(i+5), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[10,9:11] -> imagem_final[(i+4), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[9,8:10]  -> imagem_final[(i+3), (j+2):(j+4)]
        janela_selecionada_filtrada_3x3b[8,7:9]  -> imagem_final[(i+2), (j+1):(j+3)]
        janela_selecionada_filtrada_3x3b[7,6:8]  -> imagem_final[(i+1), j:(j+2)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,4:6]  -> imagem_final[(i-1), (j-2):j]
        janela_selecionada_filtrada_3x3b[4,3:5]  -> imagem_final[(i-2), (j-3):(j-1)]
        janela_selecionada_filtrada_3x3b[3,2:4]  -> imagem_final[(i-3), (j-4):(j-2)]
        janela_selecionada_filtrada_3x3b[2,1:3]  -> imagem_final[(i-4), (j-5):(j-3)]
        janela_selecionada_filtrada_3x3b[1,1:2]  -> imagem_final[(i-5), (j-5):(j-4)]}
      
      #17
      if(pos_janela_e_angulo==17){janela_selecionada_filtrada_3x3b[11,11] -> imagem_final[(i+5), (j+5)]
        janela_selecionada_filtrada_3x3b[10,10:11] -> imagem_final[(i+4), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[9,7:11]  -> imagem_final[(i+3), (j+1):(j+5)]
        janela_selecionada_filtrada_3x3b[8,6:10]  -> imagem_final[(i+2), j:(j+4)]
        janela_selecionada_filtrada_3x3b[7,6:9]  -> imagem_final[(i+1), j:(j+3)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,3:6]  -> imagem_final[(i-1), (j-3):j]
        janela_selecionada_filtrada_3x3b[4,2:6]  -> imagem_final[(i-2), (j-4):j]
        janela_selecionada_filtrada_3x3b[3,1:5]  -> imagem_final[(i-3), (j-5):(j-1)]
        janela_selecionada_filtrada_3x3b[2,1:2]  -> imagem_final[(i-4), (j-5):(j-4)]
        janela_selecionada_filtrada_3x3b[1,1]  -> imagem_final[(i-5), (j-5)]}
      
      #18
      if(pos_janela_e_angulo==18){janela_selecionada_filtrada_3x3b[10,11] -> imagem_final[(i+4), (j+5)]
        janela_selecionada_filtrada_3x3b[9,10:11]  -> imagem_final[(i+3), (j+4):(j+5)]
        janela_selecionada_filtrada_3x3b[8,6:11]  -> imagem_final[(i+2), j:(j+5)]
        janela_selecionada_filtrada_3x3b[7,6:10]  -> imagem_final[(i+1), j:(j+4)]
        janela_selecionada_filtrada_3x3b[6,5:7]  -> imagem_final[i, (j-1):(j+1)]
        janela_selecionada_filtrada_3x3b[5,2:6]  -> imagem_final[(i-1), (j-4):j]
        janela_selecionada_filtrada_3x3b[4,1:6]  -> imagem_final[(i-2), (j-5):j]
        janela_selecionada_filtrada_3x3b[3,1:2]  -> imagem_final[(i-3), (j-5):(j-4)]
        janela_selecionada_filtrada_3x3b[2,1]  -> imagem_final[(i-4), (j-5)]}
      
      #19
      if(pos_janela_e_angulo==19){janela_selecionada_filtrada_3x3b[9,11]  -> imagem_final[(i+3), (j+5)]
        janela_selecionada_filtrada_3x3b[8,9:11]  -> imagem_final[(i+2), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[7,5:11]  -> imagem_final[(i+1), (j-1):(j+5)]
        janela_selecionada_filtrada_3x3b[6,2:10]  -> imagem_final[i, (j-4):(j+4)]
        janela_selecionada_filtrada_3x3b[5,1:6]  -> imagem_final[(i-1), (j-5):j]
        janela_selecionada_filtrada_3x3b[4,1:3]  -> imagem_final[(i-2), (j-5):(j-3)]
        janela_selecionada_filtrada_3x3b[3,1]  -> imagem_final[(i-3), (j-5)]}
      
      #20
      if(pos_janela_e_angulo==20){janela_selecionada_filtrada_3x3b[8,9:11]  -> imagem_final[(i+2), (j+3):(j+5)]
        janela_selecionada_filtrada_3x3b[7,5:11]  -> imagem_final[(i+1), (j-1):(j+5)]
        janela_selecionada_filtrada_3x3b[6,1:11]  -> imagem_final[i, (j-5):(j+5)]
        janela_selecionada_filtrada_3x3b[5,1:7]  -> imagem_final[(i-1), (j-5):(j+1)]
        janela_selecionada_filtrada_3x3b[4,1:3]  -> imagem_final[(i-2), (j-5):(j-3)]}
      
    }
  }
  return(imagem_final)
}
