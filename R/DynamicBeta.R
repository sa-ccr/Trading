#' Calculates the beta of an investment strategy or stock by applying the Kalman filter & smoother. Also the relevant covariances are returned so as
#' which can be used for the conf interval calculations for beta. The python package "Pykalman" is used for the calculations given its proven stability
#' @title Dynamic Beta via Kalman filter & smoother
#' @param csvfilename the name of csv file containing the track record of the fund & the benchmark
#' @return A list of beta values based on Kalman Filter & smoother and the respective covariances
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#'
#' @examples
#'
#' ## calling DynamicBeta() without an argument loads a test file containing all
#' dyn_beta_values = DynamicBeta() 
#'
DynamicBeta = function(csvfilename)
{
  tryCatch(
    {    pykalman = reticulate::import("pykalman", delay_load = TRUE)  },
    error = function(e)
    {    reticulate::py_install("pykalman")  
      pykalman = reticulate::import("pykalman", delay_load = TRUE)
    }
  )
  
  if(missing(input_file_path))
  {    data= read.csv(system.file("extdata", 'example_track_record.csv', package = "Trading"),stringsAsFactors = FALSE,strip.white=TRUE)
  }else
  {    data= read.csv(csvfilename,stringsAsFactors = FALSE,strip.white=TRUE)    }
  
  data$Date = as.Date(as.character(data$Date),'%d/%m/%Y')
  data[,3]=100*data[,3]
  data[,2] = 100*data[,2]
  
  delta = 1e-5
  trans_cov = delta / (1 - delta) * diag(2)
  obs_mat = cbind(data[,3], rep(1,nrow(data)))
  obs_mat = array(obs_mat,c(nrow(obs_mat),1,2))
  
  kf = pykalman$KalmanFilter(n_dim_obs=1, n_dim_state=2,
                             initial_state_mean=rep(0,2),
                             initial_state_covariance=matrix(1,nrow = 2,ncol=2),
                             transition_matrices=diag(2),
                             observation_matrices=obs_mat,
                             observation_covariance=1.0,
                             transition_covariance=trans_cov)
  
  kf$em(
    X       = data[,2],
    n_iter  = as.integer(100),
    em_vars = c('initial_state_covariance', 'transition_covariance', 'observation_covariance')
  )
  
  kf_values = kf$filter(data[,2])
  ks_values = kf$smooth(data[,2])
  
  plot(data$Date, kf_values[[1]][,1],type="l",col="red",main="Betas computed via Kalman filter & smoother",xlab = "Date",ylab = "Beta")
  lines(data$Date, ks_values[[1]][,1],col="green")
  
  return(list(kalman_filter = kf_values, kalman_smoother= ks))
}
