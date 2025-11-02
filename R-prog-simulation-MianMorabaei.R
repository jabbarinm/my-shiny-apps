library(reticulate)


source_python('C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first.py')

py_run_file("C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first.py")

py_run_string("k=2",convert=TRUE)
py$k

np <- import("numpy", convert = FALSE)
# do some array manipulations with NumPy
a <- np$array(c(1:4))
sum <- a$cumsum()

# convert to R explicitly at the end
py_to_r(sum)




