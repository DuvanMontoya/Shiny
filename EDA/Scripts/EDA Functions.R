# EDA/Scripts/EDA Functions.R

str_to_MemoryVar <- function(string, accent = TRUE) {
  if (accent) {
    memory_var <- eval(parse(text = paste0("`", string, "`")), envir = parent.frame())
  } else {
    memory_var <- eval(parse(text = string), envir = parent.frame())
  }
  return(memory_var)
}

scale_function <- function(x, scale, shift) {
  (x*scale) - shift
}

inv_scale_function <- function(x, scale, shift) {
  (x + shift)/scale
}

search_vars_analytical <- function(var_name) {
  grep(var_name, colnames(analytical), value = TRUE, ignore.case = TRUE)
}

# s_curve_transform
s_curve_transform <- function(x,
                              shape       = "s-origin",
                              alpha       = 0.85,
                              beta        = 1,
                              maxValuePct = 100,
                              index_step  = 1) {
  max_val <- max(x, na.rm=TRUE)
  if(max_val == 0) {
    return(x)
  }
  # Escalamos a 0-100 * maxValuePct
  x_indexed <- 100 * x / max_val * maxValuePct
  
  i_max <- length(x)
  
  switch(tolower(shape),
    "s-shaped" = {
      (beta/(10^10))^(alpha^(seq_len(i_max)))
    },
    "s-origin" = {
      ((beta/(10^9))^(alpha^(seq_len(i_max)))) - (beta/(10^9))
    },
    "indexp" = {
      1 - exp((-alpha * seq_len(i_max))/10)
    },
    # Si no coincide, retorna x tal cual
    x
  )
}

# s_curve_indexing, first_derivative, etc.
s_curve_indexing <- function(serie, alpha, beta, index_step=1){
  if (is.data.frame(serie)) {
    i_max <- nrow(serie)
  } else {
    i_max <- length(serie)
  }
  index     <- seq(0, (i_max-1), by=index_step)
  beta_calc <- beta/(10^9)
  
  beta_calc^(alpha^index) - beta_calc
}

first_derivative <- function(x, shape="s-origin", alpha, beta, index_step=1) {
  i_max <- if(is.data.frame(x)) nrow(x) else length(x)
  index <- seq(0, (i_max-1), by=index_step)
  
  if(tolower(shape)=="s-origin") {
    (beta/(10^9))^(alpha^index)* alpha^index* log(beta/(10^9))* log(alpha)
  } else if(tolower(shape)=="s-shaped") {
    (beta/(10^10))^(alpha^index)* alpha^index* log(beta/(10^10))* log(alpha)
  } else if(tolower(shape)=="indexp") {
    (alpha*exp(-(alpha*index)/10))/10
  } else {
    rep(NA, i_max)
  }
}

second_derivative <- function(x, shape="s-origin", alpha, beta, index_step=1){
  i_max <- if(is.data.frame(x)) nrow(x) else length(x)
  index <- seq(0, (i_max-1), by=index_step)
  
  if(tolower(shape)=="s-origin") {
    log(beta/(10^9))*log(alpha)*(
      ((beta/(10^9))^(alpha^index)*(alpha^(2*index))*log(beta/(10^9))*log(alpha)) +
      ((beta/(10^9))^(alpha^index)*(alpha^index)*log(alpha))
    )
  } else if(tolower(shape)=="s-shaped") {
    log(beta/(10^10))*log(alpha)*(
      ((beta/(10^10))^(alpha^index)*(alpha^(2*index))*log(beta/(10^10))*log(alpha)) +
      ((beta/(10^10))^(alpha^index)*(alpha^index)*log(alpha))
    )
  } else if(tolower(shape)=="indexp") {
    (-(alpha^2)*exp(-(alpha*index)/10))/100
  } else {
    rep(NA, i_max)
  }
}

third_derivative <- function(x, shape="s-origin", alpha, beta, index_step=1){
  i_max <- if(is.data.frame(x)) nrow(x) else length(x)
  index <- seq(0, (i_max-1), by=index_step)
  
  if(tolower(shape)=="s-origin") {
    log(beta/(10^9))*log(alpha)*(
      ((beta/(10^9))^(alpha^index)*(alpha^(3*index))* (log(beta/(10^9)))^2*(log(alpha))^2) +
      3*((beta/(10^9))^(alpha^index)*(alpha^(2*index))* log(beta/(10^9))*(log(alpha))^2) +
      ((beta/(10^9))^(alpha^index)*(alpha^index)*(log(alpha))^2)
    )
  } else if(tolower(shape)=="s-shaped") {
    log(beta/(10^10))*log(alpha)*(
      ((beta/(10^10))^(alpha^index)*(alpha^(3*index))* (log(beta/(10^10)))^2*(log(alpha))^2) +
      3*((beta/(10^10))^(alpha^index)*(alpha^(2*index))* log(beta/(10^10))*(log(alpha))^2) +
      ((beta/(10^10))^(alpha^index)*(alpha^index)*(log(alpha))^2)
    )
  } else if(tolower(shape)=="indexp") {
    ((alpha^3)*exp(-(alpha*index)/10))/1000
  } else {
    rep(NA, i_max)
  }
}