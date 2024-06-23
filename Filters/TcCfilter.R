TcCfilter<-function(imageRaster,param,eth, xi=0.9){

  if (dim(imageRaster)[1]<13 || dim(imageRaster)[1]<13) {
    stop("Please, insert an image with dimensions larger than 13 and repeat the
         process")
  }

  final_image<-imageRaster

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
  margin <- (s+1)/2  # Calculate the size of the margin for the neighborhood/lines
  marginm1 <- margin-1  # Calculate the margin minus 1/columns

  window_angle <- seq(0,19)*pi/20

  for (i in margin:(nrow(imageRaster) - marginm1)) {
    for (j in margin:(ncol(imageRaster) - marginm1)) {

      # 20 windows of size 11x11
      window1<-window2<-window3<-window4<-window5<-window6<-window7<-window8<-window9<-window10<-window11<-window12<-window13<-window14<-window15<-window16<-window17<-window18<-window19<-window20<-matrix(0, ncol=11,nrow=11)

      # 20 directional windows (i=rows, j=columns)
      #1
      window1[5:7,1:11]<- imageRaster[(i-1):(i+1), (j-5):(j+5)]
      #2
      window2[7,1:4]  <-imageRaster[(i+2), (j-5):(j-2)]
      window2[6,1:7]  <-imageRaster[(i+1), (j-5):(j+1)]
      window2[5,1:11] <-imageRaster[i, (j-5):(j+5)]
      window2[4,5:11] <-imageRaster[(i-1), (j-1):(j+5)]
      window2[3,8:11] <-imageRaster[(i-2), (j+2):(j+5)]
      #3
      window3[3,10:11] <- imageRaster[(i+3), (j+4):(j+5)]
      window3[4,8:11]  <- imageRaster[(i+2), (j+2):(j+5)]
      window3[5,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      window3[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      window3[7,1:7]   <- imageRaster[(i+1), (j-5):(j+1)]
      window3[8,1:4]   <- imageRaster[(i+2), (j-5):(j-2)]
      window3[9,1:2]   <- imageRaster[(i+3), (j-5):(j-4)]
      #4
      window4[2,11]    <- imageRaster[(i-4), (j+5)]
      window4[3,10:11] <- imageRaster[(i-3), (j+4):(j+5)]
      window4[4,8:11]  <- imageRaster[(i-2), (j+2):(j+5)]
      window4[5,5:10]  <- imageRaster[(i-1), (j-1):(j+4)]
      window4[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      window4[7,2:7]   <- imageRaster[(i+1), (j-4):(j+1)]
      window4[8,1:4]   <- imageRaster[(i+2), (j-5):(j-2)]
      window4[9,1:2]   <- imageRaster[(i+3), (j-5):(j-4)]
      window4[10,1]    <- imageRaster[(i+4), (j-5)]
      #5
      window5[11,1]    <- imageRaster[(i+5), (j-5)]
      window5[10,1:2]  <- imageRaster[(i+4), (j-5):(j-4)]
      window5[9,1:3]   <- imageRaster[(i+3), (j-5):(j-3)]
      window5[8,2:4]   <- imageRaster[(i+2), (j-4):(j-2)]
      window5[7,3:7]   <- imageRaster[(i+1), (j-3):(j+1)]
      window5[6,4:8]   <- imageRaster[i, (j-2):(j+2)]
      window5[5,5:9]   <- imageRaster[(i-1), (j-1):(j+3)]
      window5[4,8:10]  <- imageRaster[(i-2), (j+2):(j+4)]
      window5[3,9:11]  <- imageRaster[(i-3), (j+3):(j+5)]
      window5[2,10:11] <- imageRaster[(i-4), (j+4):(j+5)]
      window5[1,11]    <- imageRaster[(i-5), (j+5)]
      #6
      window6[11,1:2]  <- imageRaster[(i+5), (j-5):(j-4)]
      window6[10,1:3]  <- imageRaster[(i+5), (j-5):(j-3)]
      window6[9,2:4]   <- imageRaster[(i+3), (j-4):(j-2)]
      window6[8,3:5]   <- imageRaster[(i+2), (j-3):(j-1)]
      window6[7,4:6]   <- imageRaster[(i+1), (j-2):j]
      window6[6,5:7]   <- imageRaster[i, (j-1):(j+1)]
      window6[5,6:8]   <- imageRaster[(i-1), j:(j+2)]
      window6[4,7:9]   <- imageRaster[(i-2), (j+1):(j+3)]
      window6[3,8:10]  <- imageRaster[(i-3), (j+2):(j+4)]
      window6[2,9:11]  <- imageRaster[(i-3), (j+3):(j+5)]
      window6[1,10:11] <- imageRaster[(i-5), (j+4):(j+5)]
      #7
      window7[11,1:3] <- imageRaster[(i+5), (j-5):(j-3)]
      window7[10,2:4] <- imageRaster[(i+4), (j-4):(j-2)]
      window7[9,3:5]  <- imageRaster[(i+3), (j-3):(j-1)]
      window7[8,3:5]  <- imageRaster[(i+2), (j-3):(j-1)]
      window7[7,3:6]  <- imageRaster[(i+1), (j-3):j]
      window7[6,4:8]  <- imageRaster[i, (j-2):(j+2)]
      window7[5,6:9]  <- imageRaster[(i-1), j:(j+3)]
      window7[4,7:9]  <- imageRaster[(i-2), (j+1):(j+3)]
      window7[3,7:9]  <- imageRaster[(i-3), (j+1):(j+3)]
      window7[2,8:10] <- imageRaster[(i-4), (j+2):(j+4)]
      window7[1,9:11] <- imageRaster[(i-5), (j+3):(j+5)]
      #8
      window8[11,2:4] <- imageRaster[(i+5), (j-4):(j-2)]
      window8[10,3:5] <- imageRaster[(i+4), (j-3):(j-1)]
      window8[9,4:5]  <- imageRaster[(i+3), (j-2):(j-1)]
      window8[8,4:5]  <- imageRaster[(i+2), (j-2):(j-1)]
      window8[7,4:6]  <- imageRaster[(i+1), (j-2):j]
      window8[6,5:8]  <- imageRaster[i, (j-1):(j+2)]
      window8[5,6:8]  <- imageRaster[(i-1), j:(j+2)]
      window8[4,7:8]  <- imageRaster[(i-2), (j+1):(j+2)]
      window8[3,7:8]  <- imageRaster[(i-3), (j+1):(j+2)]
      window8[2,7:9]  <- imageRaster[(i-4), (j+1):(j+3)]
      window8[1,8:10] <- imageRaster[(i-5), (j+2):(j+4)]
      #9
      window9[11,3:5] <- imageRaster[(i+5), (j-3):(j-1)]
      window9[10,5:7] <- imageRaster[(i+4), (j-1):(j+1)]
      window9[9,5:7]  <- imageRaster[(i+3), (j-1):(j+1)]
      window9[8,6:7]  <- imageRaster[(i+2), j:(j+1)]
      window9[7,6:7]  <- imageRaster[(i+1), j:(j+1)]
      window9[6,6:8]  <- imageRaster[i, j:(j+2)]
      window9[5,7:8]  <- imageRaster[(i-1), (j+1):(j+2)]
      window9[4,7:8]  <- imageRaster[(i-2), (j+1):(j+2)]
      window9[3,7:9]  <- imageRaster[(i-3), (j+1):(j+3)]
      window9[2,7:9]  <- imageRaster[(i-4), (j+1):(j+3)]
      window9[1,8:10] <- imageRaster[(i-5), (j+2):(j+4)]
      #10
      window10[11,4:6] <- imageRaster[(i+5), (j-2):j]
      window10[10,4:6] <- imageRaster[(i+4), (j-2):j]
      window10[9,4:6] <- imageRaster[(i+3), (j-2):j]
      window10[8,5:6] <- imageRaster[(i+2), (j-1):j]
      window10[7,5:7] <- imageRaster[(i+1), (j-1):(j+1)]
      window10[6,5:7] <- imageRaster[i, (j-1):(j+1)]
      window10[5,5:7] <- imageRaster[(i-1), (j-1):(j+1)]
      window10[4,6:7] <- imageRaster[(i-2), j:(j+1)]
      window10[3,6:8] <- imageRaster[(i-3), j:(j+2)]
      window10[2,6:8] <- imageRaster[(i-4), j:(j+2)]
      window10[1,6:8] <- imageRaster[(i-5), j:(j+2)]
      #11
      window11[1:11,5:7]<- imageRaster[(i-5):(i+5), (j-1):(j+1)]
      #12
      window12[11,6:8] <- imageRaster[(i+5), j:(j+2)]
      window12[10,6:8] <- imageRaster[(i+4), j:(j+2)]
      window12[9,6:8]  <- imageRaster[(i+3), j:(j+2)]
      window12[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      window12[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      window12[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window12[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      window12[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      window12[3,4:6]  <- imageRaster[(i-3), (j-2):j]
      window12[2,4:6]  <- imageRaster[(i-4), (j-2):j]
      window12[1,4:6]  <- imageRaster[(i-5), (j-2):j]
      #13
      window13[11,7:9] <- imageRaster[(i+5), (j+1):(j+3)]
      window13[10,7:9] <- imageRaster[(i+4), (j+1):(j+3)]
      window13[9,7:8]  <- imageRaster[(i+3), (j+1):(j+2)]
      window13[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      window13[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      window13[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window13[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      window13[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      window13[3,4:5]  <- imageRaster[(i-3), (j-2):(j-1)]
      window13[2,3:5]  <- imageRaster[(i-4), (j-3):(j-1)]
      window13[1,3:5]  <- imageRaster[(i-5), (j-3):(j-1)]
      #14
      window14[11,8:10] <- imageRaster[(i+5), (j+2):(j+4)]
      window14[10,7:9] <- imageRaster[(i+4), (j+1):(j+3)]
      window14[9,7:8]  <- imageRaster[(i+3), (j+1):(j+2)]
      window14[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      window14[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      window14[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window14[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      window14[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      window14[3,4:5]  <- imageRaster[(i-3), (j-2):(j-1)]
      window14[2,3:5]  <- imageRaster[(i-4), (j-3):(j-1)]
      window14[1,2:4]  <- imageRaster[(i-5), (j-4):(j-2)]
      #15
      window15[11,9:11] <- imageRaster[(i+5), (j+3):(j+5)]
      window15[10,8:10] <- imageRaster[(i+4), (j+2):(j+4)]
      window15[9,7:9]  <- imageRaster[(i+3), (j+1):(j+3)]
      window15[8,6:8]  <- imageRaster[(i+2), j:(j+2)]
      window15[7,5:7]  <- imageRaster[(i+1), (j-1):(j+1)]
      window15[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window15[5,5:7]  <- imageRaster[(i-1), (j-1):(j+1)]
      window15[4,4:6]  <- imageRaster[(i-2), (j-2):j]
      window15[3,3:5]  <- imageRaster[(i-3), (j-3):(j-1)]
      window15[2,2:4]  <- imageRaster[(i-4), (j-4):(j-2)]
      window15[1,1:3]  <- imageRaster[(i-5), (j-5):(j-3)]
      #16
      window16[11,10:11] <- imageRaster[(i+5), (j+4):(j+5)]
      window16[10,9:11] <- imageRaster[(i+4), (j+3):(j+5)]
      window16[9,8:10]  <- imageRaster[(i+3), (j+2):(j+4)]
      window16[8,7:9]  <- imageRaster[(i+2), (j+1):(j+3)]
      window16[7,6:8]  <- imageRaster[(i+1), j:(j+2)]
      window16[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window16[5,4:6]  <- imageRaster[(i-1), (j-2):j]
      window16[4,3:5]  <- imageRaster[(i-2), (j-3):(j-1)]
      window16[3,2:4]  <- imageRaster[(i-3), (j-4):(j-2)]
      window16[2,1:3]  <- imageRaster[(i-4), (j-5):(j-3)]
      window16[1,1:2]  <- imageRaster[(i-5), (j-5):(j-4)]
      #17
      window17[11,11] <- imageRaster[(i+5), (j+5)]
      window17[10,10:11] <- imageRaster[(i+4), (j+4):(j+5)]
      window17[9,7:11]  <- imageRaster[(i+3), (j+1):(j+5)]
      window17[8,6:10]  <- imageRaster[(i+2), j:(j+4)]
      window17[7,6:9]  <- imageRaster[(i+1), j:(j+3)]
      window17[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window17[5,3:6]  <- imageRaster[(i-1), (j-3):j]
      window17[4,2:6]  <- imageRaster[(i-2), (j-4):j]
      window17[3,1:5]  <- imageRaster[(i-3), (j-5):(j-1)]
      window17[2,1:2]  <- imageRaster[(i-4), (j-5):(j-4)]
      window17[1,1]  <- imageRaster[(i-5), (j-5)]
      #18
      window18[10,11] <- imageRaster[(i+4), (j+5)]
      window18[9,10:11]  <- imageRaster[(i+3), (j+4):(j+5)]
      window18[8,6:11]  <- imageRaster[(i+2), j:(j+5)]
      window18[7,6:10]  <- imageRaster[(i+1), j:(j+4)]
      window18[6,5:7]  <- imageRaster[i, (j-1):(j+1)]
      window18[5,2:6]  <- imageRaster[(i-1), (j-4):j]
      window18[4,1:6]  <- imageRaster[(i-2), (j-5):j]
      window18[3,1:2]  <- imageRaster[(i-3), (j-5):(j-4)]
      window18[2,1]  <- imageRaster[(i-4), (j-5)]
      #19
      window19[9,11]  <- imageRaster[(i+3), (j+5)]
      window19[8,9:11]  <- imageRaster[(i+2), (j+3):(j+5)]
      window19[7,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      window19[6,2:10]  <- imageRaster[i, (j-4):(j+4)]
      window19[5,1:6]  <- imageRaster[(i-1), (j-5):j]
      window19[4,1:3]  <- imageRaster[(i-2), (j-5):(j-3)]
      window19[3,1]  <- imageRaster[(i-3), (j-5)]
      #20
      window20[8,9:11]  <- imageRaster[(i+2), (j+3):(j+5)]
      window20[7,5:11]  <- imageRaster[(i+1), (j-1):(j+5)]
      window20[6,1:11]  <- imageRaster[i, (j-5):(j+5)]
      window20[5,1:7]  <- imageRaster[(i-1), (j-5):(j+1)]
      window20[4,1:3]  <- imageRaster[(i-2), (j-5):(j-3)]

      all_windows<-list()

      all_windows[[1]]<-window1
      all_windows[[2]]<-window2
      all_windows[[3]]<-window3
      all_windows[[4]]<-window4
      all_windows[[5]]<-window5
      all_windows[[6]]<-window6
      all_windows[[7]]<-window7
      all_windows[[8]]<-window8
      all_windows[[9]]<-window9
      all_windows[[10]]<-window10
      all_windows[[11]]<-window11
      all_windows[[12]]<-window12
      all_windows[[13]]<-window13
      all_windows[[14]]<-window14
      all_windows[[15]]<-window15
      all_windows[[16]]<-window16
      all_windows[[17]]<-window17
      all_windows[[18]]<-window18
      all_windows[[19]]<-window19
      all_windows[[20]]<-window20

      # Averages of all the windows

      averages<-c(calculate_mean_complex(window1),calculate_mean_complex(window2),calculate_mean_complex(window3),
                  calculate_mean_complex(window4),calculate_mean_complex(window5),calculate_mean_complex(window6),
                  calculate_mean_complex(window7),calculate_mean_complex(window8),calculate_mean_complex(window9),
                  calculate_mean_complex(window10),calculate_mean_complex(window11),calculate_mean_complex(window12),
                  calculate_mean_complex(window13),calculate_mean_complex(window14),calculate_mean_complex(window15),
                  calculate_mean_complex(window16),calculate_mean_complex(window17),calculate_mean_complex(window18),
                  calculate_mean_complex(window19),calculate_mean_complex(window20))


      pos_window_end_angle<-which.max(Mod(averages))

      window_selected<-all_windows[[pos_window_end_angle]]
      angle_selected<-window_angle[pos_window_end_angle]


      if(max(Mod(averages))<eth){
        w <- 1 / sqrt((1:13 - 6)^2 + (1:13 - 6)^2)
        w[6] <- 0  # Delete masked pixels
        expValues <- exp(1i *  angle_selected)
        weightedSum <- sum(w * expValues)
        weightedSum <- weightedSum / sum(w)
        adjustedAngle <- Arg(weightedSum)
        differences<-(window_angle - adjustedAngle)
        index_closest_to_zero <- which.min(abs(differences))
        window_selected<-all_windows[[ index_closest_to_zero]]
        angle_selected<-window_angle[ index_closest_to_zero]
      }


      integral_func <- function(l) {
        integrate(dTruncCauchy, lower = -l, upper = l, subdivisions = 100, param = param)$value
      }

      integral <- xi
      lower_limit <- -pi
      upper_limit <- pi
      psi_epsilon <- (-lower_limit + upper_limit) / 2
      estimated_value<-integral_func(psi_epsilon)

      tolerance <- 0.001
      dm <- seq(0,pi,0.001)
      for (d in 1:length(dm)) {
        estimated_value<-integral_func(dm[d])
        if(abs(estimated_value - integral) < tolerance){
          psi_epsilon <- dm[d]
          break
        }
      }

      window_selected_filtered_3x3<-filter_3x3_mod(window_selected)

      f <- function(window_selected) {window_selected * dTruncCauchy(window_selected, param)}
      psi_bar <- integrate(f, lower = -psi_epsilon, upper = psi_epsilon, subdivisions = 100)$value

      f2 <- function(window_selected){(window_selected - psi_bar)^2 * dTruncCauchy(window_selected, param)}
      sigma_v_squared <- integrate(f2, lower = -psi_epsilon, upper = psi_epsilon, subdivisions = 100)$value

      b <- 1- (sigma_v_squared/var(as.vector(window_selected_filtered_3x3)))
      b <- pmax(0, pmin(1, b)) # Limit b between 0 and 1

      psi_estimated<-mean(window_selected) + b*(window_selected-mean(window_selected))
      window_selected_filtered_3x3b<-window_selected_filtered_3x3

      for (a in 1:ncol(window_selected)) {
        for (b in 1:nrow(window_selected)) {
          if(window_selected_filtered_3x3b[a,b]!=0){window_selected_filtered_3x3b[a,b]<-psi_estimated[a,b]}
        }
      }


      #1
      if(pos_window_end_angle==1){window_selected_filtered_3x3b[5:7,1:11]-> final_image[(i-1):(i+1), (j-5):(j+5)]}

      #2
      if(pos_window_end_angle==2){window_selected_filtered_3x3b[7,1:4]  ->final_image[(i+2), (j-5):(j-2)]
        window_selected_filtered_3x3b[6,1:7]  ->final_image[(i+1), (j-5):(j+1)]
        window_selected_filtered_3x3b[5,1:11] ->final_image[i, (j-5):(j+5)]
        window_selected_filtered_3x3b[4,5:11] ->final_image[(i-1), (j-1):(j+5)]
        window_selected_filtered_3x3b[3,8:11] ->final_image[(i-2), (j+2):(j+5)]}

      #3
      if(pos_window_end_angle==3){window_selected_filtered_3x3b[3,10:11] -> final_image[(i+3), (j+4):(j+5)]
        window_selected_filtered_3x3b[4,8:11]  -> final_image[(i+2), (j+2):(j+5)]
        window_selected_filtered_3x3b[5,5:11]  -> final_image[(i+1), (j-1):(j+5)]
        window_selected_filtered_3x3b[6,4:8]   -> final_image[i, (j-2):(j+2)]
        window_selected_filtered_3x3b[7,1:7]   -> final_image[(i+1), (j-5):(j+1)]
        window_selected_filtered_3x3b[8,1:4]   -> final_image[(i+2), (j-5):(j-2)]
        window_selected_filtered_3x3b[9,1:2]   -> final_image[(i+3), (j-5):(j-4)]}

      #4
      if(pos_window_end_angle==4){window_selected_filtered_3x3b[2,11]    -> final_image[(i-4), (j+5)]
        window_selected_filtered_3x3b[3,10:11] -> final_image[(i-3), (j+4):(j+5)]
        window_selected_filtered_3x3b[4,8:11]  -> final_image[(i-2), (j+2):(j+5)]
        window_selected_filtered_3x3b[5,5:10]  -> final_image[(i-1), (j-1):(j+4)]
        window_selected_filtered_3x3b[6,4:8]   -> final_image[i, (j-2):(j+2)]
        window_selected_filtered_3x3b[7,2:7]   -> final_image[(i+1), (j-4):(j+1)]
        window_selected_filtered_3x3b[8,1:4]   -> final_image[(i+2), (j-5):(j-2)]
        window_selected_filtered_3x3b[9,1:2]   -> final_image[(i+3), (j-5):(j-4)]
        window_selected_filtered_3x3b[10,1]    -> final_image[(i+4), (j-5)]}

      #5
      if(pos_window_end_angle==5){window_selected_filtered_3x3b[11,1]    -> final_image[(i+5), (j-5)]
        window_selected_filtered_3x3b[10,1:2]  -> final_image[(i+4), (j-5):(j-4)]
        window_selected_filtered_3x3b[9,1:3]   -> final_image[(i+3), (j-5):(j-3)]
        window_selected_filtered_3x3b[8,2:4]   -> final_image[(i+2), (j-4):(j-2)]
        window_selected_filtered_3x3b[7,3:7]   -> final_image[(i+1), (j-3):(j+1)]
        window_selected_filtered_3x3b[6,4:8]   -> final_image[i, (j-2):(j+2)]
        window_selected_filtered_3x3b[5,5:9]   -> final_image[(i-1), (j-1):(j+3)]
        window_selected_filtered_3x3b[4,8:10]  -> final_image[(i-2), (j+2):(j+4)]
        window_selected_filtered_3x3b[3,9:11]  -> final_image[(i-3), (j+3):(j+5)]
        window_selected_filtered_3x3b[2,10:11] -> final_image[(i-4), (j+4):(j+5)]
        window_selected_filtered_3x3b[1,11]    -> final_image[(i-5), (j+5)]}

      #6
      if(pos_window_end_angle==6){window_selected_filtered_3x3b[11,1:2]  -> final_image[(i+5), (j-5):(j-4)]
        window_selected_filtered_3x3b[10,1:3]  -> final_image[(i+5), (j-5):(j-3)]
        window_selected_filtered_3x3b[9,2:4]   -> final_image[(i+3), (j-4):(j-2)]
        window_selected_filtered_3x3b[8,3:5]   -> final_image[(i+2), (j-3):(j-1)]
        window_selected_filtered_3x3b[7,4:6]   -> final_image[(i+1), (j-2):j]
        window_selected_filtered_3x3b[6,5:7]   -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,6:8]   -> final_image[(i-1), j:(j+2)]
        window_selected_filtered_3x3b[4,7:9]   -> final_image[(i-2), (j+1):(j+3)]
        window_selected_filtered_3x3b[3,8:10]  -> final_image[(i-3), (j+2):(j+4)]
        window_selected_filtered_3x3b[2,9:11]  -> final_image[(i-3), (j+3):(j+5)]
        window_selected_filtered_3x3b[1,10:11] -> final_image[(i-5), (j+4):(j+5)]}

      #7
      if(pos_window_end_angle==7){window_selected_filtered_3x3b[11,1:3] -> final_image[(i+5), (j-5):(j-3)]
        window_selected_filtered_3x3b[10,2:4] -> final_image[(i+4), (j-4):(j-2)]
        window_selected_filtered_3x3b[9,3:5]  -> final_image[(i+3), (j-3):(j-1)]
        window_selected_filtered_3x3b[8,3:5]  -> final_image[(i+2), (j-3):(j-1)]
        window_selected_filtered_3x3b[7,3:6]  -> final_image[(i+1), (j-3):j]
        window_selected_filtered_3x3b[6,4:8]  -> final_image[i, (j-2):(j+2)]
        window_selected_filtered_3x3b[5,6:9]  -> final_image[(i-1), j:(j+3)]
        window_selected_filtered_3x3b[4,7:9]  -> final_image[(i-2), (j+1):(j+3)]
        window_selected_filtered_3x3b[3,7:9]  -> final_image[(i-3), (j+1):(j+3)]
        window_selected_filtered_3x3b[2,8:10] -> final_image[(i-4), (j+2):(j+4)]
        window_selected_filtered_3x3b[1,9:11] -> final_image[(i-5), (j+3):(j+5)]}

      #8
      if(pos_window_end_angle==8){window_selected_filtered_3x3b[11,2:4] -> final_image[(i+5), (j-4):(j-2)]
        window_selected_filtered_3x3b[10,3:5] -> final_image[(i+4), (j-3):(j-1)]
        window_selected_filtered_3x3b[9,4:5]  -> final_image[(i+3), (j-2):(j-1)]
        window_selected_filtered_3x3b[8,4:5]  -> final_image[(i+2), (j-2):(j-1)]
        window_selected_filtered_3x3b[7,4:6]  -> final_image[(i+1), (j-2):j]
        window_selected_filtered_3x3b[6,5:8]  -> final_image[i, (j-1):(j+2)]
        window_selected_filtered_3x3b[5,6:8]  -> final_image[(i-1), j:(j+2)]
        window_selected_filtered_3x3b[4,7:8]  -> final_image[(i-2), (j+1):(j+2)]
        window_selected_filtered_3x3b[3,7:8]  -> final_image[(i-3), (j+1):(j+2)]
        window_selected_filtered_3x3b[2,7:9]  -> final_image[(i-4), (j+1):(j+3)]
        window_selected_filtered_3x3b[1,8:10] -> final_image[(i-5), (j+2):(j+4)]}

      #9
      if(pos_window_end_angle==9){window_selected_filtered_3x3b[11,3:5] -> final_image[(i+5), (j-3):(j-1)]
        window_selected_filtered_3x3b[10,5:7] -> final_image[(i+4), (j-1):(j+1)]
        window_selected_filtered_3x3b[9,5:7]  -> final_image[(i+3), (j-1):(j+1)]
        window_selected_filtered_3x3b[8,6:7]  -> final_image[(i+2), j:(j+1)]
        window_selected_filtered_3x3b[7,6:7]  -> final_image[(i+1), j:(j+1)]
        window_selected_filtered_3x3b[6,6:8]  -> final_image[i, j:(j+2)]
        window_selected_filtered_3x3b[5,7:8]  -> final_image[(i-1), (j+1):(j+2)]
        window_selected_filtered_3x3b[4,7:8]  -> final_image[(i-2), (j+1):(j+2)]
        window_selected_filtered_3x3b[3,7:9]  -> final_image[(i-3), (j+1):(j+3)]
        window_selected_filtered_3x3b[2,7:9]  -> final_image[(i-4), (j+1):(j+3)]
        window_selected_filtered_3x3b[1,8:10] -> final_image[(i-5), (j+2):(j+4)]}

      #10
      if(pos_window_end_angle==10){window_selected_filtered_3x3b[11,4:6] -> final_image[(i+5), (j-2):j]
        window_selected_filtered_3x3b[10,4:6] -> final_image[(i+4), (j-2):j]
        window_selected_filtered_3x3b[9,4:6] -> final_image[(i+3), (j-2):j]
        window_selected_filtered_3x3b[8,5:6] -> final_image[(i+2), (j-1):j]
        window_selected_filtered_3x3b[7,5:7] -> final_image[(i+1), (j-1):(j+1)]
        window_selected_filtered_3x3b[6,5:7] -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,5:7] -> final_image[(i-1), (j-1):(j+1)]
        window_selected_filtered_3x3b[4,6:7] -> final_image[(i-2), j:(j+1)]
        window_selected_filtered_3x3b[3,6:8] -> final_image[(i-3), j:(j+2)]
        window_selected_filtered_3x3b[2,6:8] -> final_image[(i-4), j:(j+2)]
        window_selected_filtered_3x3b[1,6:8] -> final_image[(i-5), j:(j+2)]}

      #11
      if(pos_window_end_angle==11){window_selected_filtered_3x3b[1:11,5:7]-> final_image[(i-5):(i+5), (j-1):(j+1)]}

      #12
      if(pos_window_end_angle==12){window_selected_filtered_3x3b[11,6:8] -> final_image[(i+5), j:(j+2)]
        window_selected_filtered_3x3b[10,6:8] -> final_image[(i+4), j:(j+2)]
        window_selected_filtered_3x3b[9,6:8]  -> final_image[(i+3), j:(j+2)]
        window_selected_filtered_3x3b[8,6:8]  -> final_image[(i+2), j:(j+2)]
        window_selected_filtered_3x3b[7,5:7]  -> final_image[(i+1), (j-1):(j+1)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,5:7]  -> final_image[(i-1), (j-1):(j+1)]
        window_selected_filtered_3x3b[4,4:6]  -> final_image[(i-2), (j-2):j]
        window_selected_filtered_3x3b[3,4:6]  -> final_image[(i-3), (j-2):j]
        window_selected_filtered_3x3b[2,4:6]  -> final_image[(i-4), (j-2):j]
        window_selected_filtered_3x3b[1,4:6]  -> final_image[(i-5), (j-2):j]}

      #13
      if(pos_window_end_angle==13){window_selected_filtered_3x3b[11,7:9] -> final_image[(i+5), (j+1):(j+3)]
        window_selected_filtered_3x3b[10,7:9] -> final_image[(i+4), (j+1):(j+3)]
        window_selected_filtered_3x3b[9,7:8]  -> final_image[(i+3), (j+1):(j+2)]
        window_selected_filtered_3x3b[8,6:8]  -> final_image[(i+2), j:(j+2)]
        window_selected_filtered_3x3b[7,5:7]  -> final_image[(i+1), (j-1):(j+1)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,5:7]  -> final_image[(i-1), (j-1):(j+1)]
        window_selected_filtered_3x3b[4,4:6]  -> final_image[(i-2), (j-2):j]
        window_selected_filtered_3x3b[3,4:5]  -> final_image[(i-3), (j-2):(j-1)]
        window_selected_filtered_3x3b[2,3:5]  -> final_image[(i-4), (j-3):(j-1)]
        window_selected_filtered_3x3b[1,3:5]  -> final_image[(i-5), (j-3):(j-1)]}

      #14
      if(pos_window_end_angle==14){window_selected_filtered_3x3b[11,8:10] -> final_image[(i+5), (j+2):(j+4)]
        window_selected_filtered_3x3b[10,7:9] -> final_image[(i+4), (j+1):(j+3)]
        window_selected_filtered_3x3b[9,7:8]  -> final_image[(i+3), (j+1):(j+2)]
        window_selected_filtered_3x3b[8,6:8]  -> final_image[(i+2), j:(j+2)]
        window_selected_filtered_3x3b[7,5:7]  -> final_image[(i+1), (j-1):(j+1)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,5:7]  -> final_image[(i-1), (j-1):(j+1)]
        window_selected_filtered_3x3b[4,4:6]  -> final_image[(i-2), (j-2):j]
        window_selected_filtered_3x3b[3,4:5]  -> final_image[(i-3), (j-2):(j-1)]
        window_selected_filtered_3x3b[2,3:5]  -> final_image[(i-4), (j-3):(j-1)]
        window_selected_filtered_3x3b[1,2:4]  -> final_image[(i-5), (j-4):(j-2)]}

      #15
      if(pos_window_end_angle==15){window_selected_filtered_3x3b[11,9:11] -> final_image[(i+5), (j+3):(j+5)]
        window_selected_filtered_3x3b[10,8:10] -> final_image[(i+4), (j+2):(j+4)]
        window_selected_filtered_3x3b[9,7:9]  -> final_image[(i+3), (j+1):(j+3)]
        window_selected_filtered_3x3b[8,6:8]  -> final_image[(i+2), j:(j+2)]
        window_selected_filtered_3x3b[7,5:7]  -> final_image[(i+1), (j-1):(j+1)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,5:7]  -> final_image[(i-1), (j-1):(j+1)]
        window_selected_filtered_3x3b[4,4:6]  -> final_image[(i-2), (j-2):j]
        window_selected_filtered_3x3b[3,3:5]  -> final_image[(i-3), (j-3):(j-1)]
        window_selected_filtered_3x3b[2,2:4]  -> final_image[(i-4), (j-4):(j-2)]
        window_selected_filtered_3x3b[1,1:3]  -> final_image[(i-5), (j-5):(j-3)]}

      #16
      if(pos_window_end_angle==16){window_selected_filtered_3x3b[11,10:11] -> final_image[(i+5), (j+4):(j+5)]
        window_selected_filtered_3x3b[10,9:11] -> final_image[(i+4), (j+3):(j+5)]
        window_selected_filtered_3x3b[9,8:10]  -> final_image[(i+3), (j+2):(j+4)]
        window_selected_filtered_3x3b[8,7:9]  -> final_image[(i+2), (j+1):(j+3)]
        window_selected_filtered_3x3b[7,6:8]  -> final_image[(i+1), j:(j+2)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,4:6]  -> final_image[(i-1), (j-2):j]
        window_selected_filtered_3x3b[4,3:5]  -> final_image[(i-2), (j-3):(j-1)]
        window_selected_filtered_3x3b[3,2:4]  -> final_image[(i-3), (j-4):(j-2)]
        window_selected_filtered_3x3b[2,1:3]  -> final_image[(i-4), (j-5):(j-3)]
        window_selected_filtered_3x3b[1,1:2]  -> final_image[(i-5), (j-5):(j-4)]}

      #17
      if(pos_window_end_angle==17){window_selected_filtered_3x3b[11,11] -> final_image[(i+5), (j+5)]
        window_selected_filtered_3x3b[10,10:11] -> final_image[(i+4), (j+4):(j+5)]
        window_selected_filtered_3x3b[9,7:11]  -> final_image[(i+3), (j+1):(j+5)]
        window_selected_filtered_3x3b[8,6:10]  -> final_image[(i+2), j:(j+4)]
        window_selected_filtered_3x3b[7,6:9]  -> final_image[(i+1), j:(j+3)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,3:6]  -> final_image[(i-1), (j-3):j]
        window_selected_filtered_3x3b[4,2:6]  -> final_image[(i-2), (j-4):j]
        window_selected_filtered_3x3b[3,1:5]  -> final_image[(i-3), (j-5):(j-1)]
        window_selected_filtered_3x3b[2,1:2]  -> final_image[(i-4), (j-5):(j-4)]
        window_selected_filtered_3x3b[1,1]  -> final_image[(i-5), (j-5)]}

      #18
      if(pos_window_end_angle==18){window_selected_filtered_3x3b[10,11] -> final_image[(i+4), (j+5)]
        window_selected_filtered_3x3b[9,10:11]  -> final_image[(i+3), (j+4):(j+5)]
        window_selected_filtered_3x3b[8,6:11]  -> final_image[(i+2), j:(j+5)]
        window_selected_filtered_3x3b[7,6:10]  -> final_image[(i+1), j:(j+4)]
        window_selected_filtered_3x3b[6,5:7]  -> final_image[i, (j-1):(j+1)]
        window_selected_filtered_3x3b[5,2:6]  -> final_image[(i-1), (j-4):j]
        window_selected_filtered_3x3b[4,1:6]  -> final_image[(i-2), (j-5):j]
        window_selected_filtered_3x3b[3,1:2]  -> final_image[(i-3), (j-5):(j-4)]
        window_selected_filtered_3x3b[2,1]  -> final_image[(i-4), (j-5)]}

      #19
      if(pos_window_end_angle==19){window_selected_filtered_3x3b[9,11]  -> final_image[(i+3), (j+5)]
        window_selected_filtered_3x3b[8,9:11]  -> final_image[(i+2), (j+3):(j+5)]
        window_selected_filtered_3x3b[7,5:11]  -> final_image[(i+1), (j-1):(j+5)]
        window_selected_filtered_3x3b[6,2:10]  -> final_image[i, (j-4):(j+4)]
        window_selected_filtered_3x3b[5,1:6]  -> final_image[(i-1), (j-5):j]
        window_selected_filtered_3x3b[4,1:3]  -> final_image[(i-2), (j-5):(j-3)]
        window_selected_filtered_3x3b[3,1]  -> final_image[(i-3), (j-5)]}

      #20
      if(pos_window_end_angle==20){window_selected_filtered_3x3b[8,9:11]  -> final_image[(i+2), (j+3):(j+5)]
        window_selected_filtered_3x3b[7,5:11]  -> final_image[(i+1), (j-1):(j+5)]
        window_selected_filtered_3x3b[6,1:11]  -> final_image[i, (j-5):(j+5)]
        window_selected_filtered_3x3b[5,1:7]  -> final_image[(i-1), (j-5):(j+1)]
        window_selected_filtered_3x3b[4,1:3]  -> final_image[(i-2), (j-5):(j-3)]}


    }
  }
  return(final_image)
}
