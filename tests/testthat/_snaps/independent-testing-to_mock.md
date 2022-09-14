# if df = NULL then the output is NULL

    Code
      to_mock(df = head(iris))
    Output
           Sepal.Length Sepal.Width Petal.Length Petal.Width Species 
      [1,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"
      [2,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"
      [3,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"
      [4,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"
      [5,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"
      [6,] "x.x"        "x.x"       "x.x"        "x.x"       "setosa"

# if mask = y then the table cells will be masked by 'y'

    Code
      to_mock(df = head(iris), mask = "y")
    Output
           Sepal.Length Sepal.Width Petal.Length Petal.Width Species 
      [1,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"
      [2,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"
      [3,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"
      [4,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"
      [5,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"
      [6,] "y.y"        "y.y"       "y.y"        "y.y"       "setosa"

