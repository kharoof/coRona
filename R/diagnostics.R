#' Fit a Model to the Data
#'
#' @param country Country/Region Name
#' @param model_type Model Type
#'
#' @return
#' @export
#'
fit_model <- function(country, model_type="Exponential") {

  #Calculate the duration since the first recorded case in the country
  country$duration <- country$value.date+1-first(country$value.date)

  if(model_type=="Exponential") {
    ## i.e. fit y = a*exp(b*x)
    model <- lm(log(value)~duration, data=country)

    a_value <- exp(model$coefficients[[1]])
    b_value <- model$coefficients[[2]]

    model <- list(type=model_type,a=a_value, b=b_value)
  }

  return(model)
}


#' Predict based on fitted model
#'
#' Make a prediction based on a fitted model and duration since first case in this country
#'
#' @param model Model Object returned from fit_model
#' @param duration Duration to Predict (from first case in country)
#'
#' @return Predictived Value
#' @export
#'
prediction <- function(model, duration) {

  if(model$type=="Exponential") {
    predicted_value <- model$a * exp(model$b*duration)
  }

  return(predicted_value)
}


#' Visualise Fitted vs Actual Values
#'
#' Graph the trend in numbers and visually compare a fitted curve to the actual data projecting out for 3 days
#' @param country Name of country to get data
#' @param country_name Used if you wish to specify a different name for The title in the Graph
#'
#' @return
#' @export
#'
visualise_fitted_vs_actual <- function(country, country_name="") {

  plot.data <- get_latest_country_data(country)

  if(country_name=="") {
    country_name <- deparse(substitute(country))
  }

  fitted_model <- fit_model(plot.data)


  #Time period from first case to latest + 3 days
  #3 days Provides visual indication of actual vs fitted growth
  time_period <- c(as.Date(plot.data$value.date), last(plot.data$value.date)+1, last(plot.data$value.date)+2, last(plot.data$value.date)+3)

  #Generate the data for plotting
  fitted <- prediction(fitted_model, 1:(nrow(plot.data)+3))
  actual <- c(plot.data$value, NA, NA, NA)
  results <- data.table(value.date = time_period, actual = actual, prediction= fitted)

  ggplot2::ggplot(results, ggplot2::aes(x=value.date))+ggplot2::geom_line(ggplot2::aes(y=actual, color="red"))+
    ggplot2::geom_point(ggplot2::aes(y=prediction))+ ggplot2::geom_line(ggplot2::aes(y=prediction))+
    ggplot2::labs( title="Actual vs Expected", subtitle=country_name , x="Date", y="Count")+ ggplot2::theme(legend.position = "")+
    ggplot2::scale_y_continuous(labels=scales::comma)
}

