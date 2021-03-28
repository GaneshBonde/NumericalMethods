x <- 3
h <- 0.1

f_x <- expression(4*x^2)
f_x

f_x_h <- expression(4 * (x + h)^2)
f_x_h

dx <- D(f_x, 'x')
dx

true_value <- eval(dx)
true_value

approx_value <- (eval(f_x_h) - eval(f_x))/h
approx_value

true_error_value <- true_value - approx_value
true_error_value

rel_true_error <- true_error_value / true_value
rel_true_error

x <- 3
h <- 0.0001

true_value <- eval(dx)
true_value

approx_value <- (eval(f_x_h) - eval(f_x))/h
approx_value

true_error_value <- true_value - approx_value
true_error_value

rel_true_error <- true_error_value / true_value
rel_true_error


