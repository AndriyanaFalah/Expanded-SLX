# Code SERVER or Back End Display in RShiny 

server = function(input, output){

  #Function to create covariate matrix (Matrix of X)
  matrix_X = function(dataframe_new, col.selected){
    matrix.data = matrix(NA, nrow = nrow(dataframe_new), ncol = length(col.selected)*nrow(dataframe_new))
    for (i in 1:nrow(dataframe_new)){
      for (j in 1:(length(col.selected)*nrow(dataframe_new))){
        if(j>length(col.selected)*(i-1) & j<=length(col.selected)*i){
          matrix.data[i,j] = dataframe_new[i, col.selected[(j-length(col.selected)*(i-1))]]
        }else{
          matrix.data[i,j] = as.numeric(0)
        }
      }
    }
    return(matrix.data)
  }
  matrix_XTilde = function(dataframe_new, col.selected){
    matrix.data = matrix(NA, nrow = nrow(dataframe_new), ncol = length(col.selected))
    for (i in 1:nrow(dataframe_new)){
      for (j in 1:(length(col.selected))){
        matrix.data[i,j] =  dataframe_new[i, col.selected[j]]
        
      }
    }
    return(matrix.data)
  }
  #Function to create Identity matrix as much as the size of covariate matrix (Matrix of X)
  matrixJ = function(dataframe_new,col.selected){
    x = diag(length(col.selected)*2)
    for (i in 1:(nrow(dataframe_new)-1)){
      x = rbind(x, diag(length(col.selected)*2))
    }
    return(x)
  }

  hideTab('tab', 'Vector & Matrix', session = getDefaultReactiveDomain())
  hideTab('tab', 'Result of Prediction', session = getDefaultReactiveDomain())
  hideTab('tab', 'Download Data', session = getDefaultReactiveDomain())

  checkTab = reactive({
    if(length(input$variableZ) == 0){
      hideTab('tabm', 'Matrix of Z', session = getDefaultReactiveDomain())
    }
  })


  dataframe = reactive({
    file.data = input$upload
    if(is.null(file.data)){
      return(NULL)
    }else{
      dataFrame = read.csv(file.data$datapath, sep = input$separator, header = input$header)
    }
  })

  output$printData = renderDT({
    dataFrame = dataframe()
    if(length(dataFrame)!=0){
      showTab('tab', 'Vector & Matrix', session = getDefaultReactiveDomain())
      showTab('tab', 'Result of Prediction', session = getDefaultReactiveDomain())
      showTab('tab', 'Download Data', session = getDefaultReactiveDomain())
    }
    datatable(dataFrame, options = list(
      scrollX = TRUE))
  })

  output$columeNames = renderText({
    dataFrame = dataframe()
    colnames(dataFrame)
  })

  #Function to display entries from Vector of Y
  output$optionColumnY = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colY", style="margin-top:30px;",
          checkboxGroupInput("variableY", "Select Column Name for Vector of Y:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowY= renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowY', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })

  Y = reactive({
    dataFrame = dataframe()
    matrix = dataFrame[,input$variableY]
    return(matrix)
  })

  output$vectorY = renderDataTable({
    if(length(input$variableY)==1){
      matrix = Y()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowY)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowY,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  #Function to display entries from Matrix of X
  output$OptionColumnX = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      h4("")
    }else{
      div(id = "colX", style="margin-top:30px;",
        checkboxGroupInput("variableX", "Select Column Name for Matrix of X:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowX = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowX', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolX = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolX', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableX)*nrow(dataFrame))), span("columns"))
  })


  X = reactive({
    dataFrame = dataframe()
    matrix = matrix_X(dataframe_new =dataFrame, col.selected = input$variableX)
    #return(matrix)
  })
  output$matrixX = renderDT({
    if(length(input$variableX)!=0){
      matrix = X()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowX) & !isTruthy(input$numcolX)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowX)){
        matrix = matrix[1:6,1:input$numcolX]
      }else if(!isTruthy(input$numcolX)){
        matrix = matrix[1:input$numrowX,1:6]
      }else{
        matrix = matrix[1:input$numrowX,1:input$numcolX]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })
  
  output$OptionColumnXTilde = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      h4("")
    }else{
      div(id = "colXTilde", style="margin-top:30px;",
          checkboxGroupInput("variableXTilde", "Select Column Name for Matrix of XTilde:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowXTilde = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowXTilde', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolXTilde = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolXTilde', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableXTilde))), span("columns"))
  })
  
  XTilde = reactive({
    dataFrame = dataframe()
    matrix = matrix_XTilde(dataframe_new = dataFrame, col.selected = input$variableXTilde)
    return(matrix)
  })
  output$matrixXTilde = renderDT({
    if(length(input$variableXTilde)!=0){
      matrix = XTilde()
      matrix = data.frame(matrix)
      if(length(input$variableY)==1){
        matrix
      }
      else{
        if(!isTruthy(input$numrowXTilde) & !isTruthy(input$numcolXTilde)){
          matrix
        }else if(!isTruthy(input$numrowXTilde)){
          matrix = matrix[1:6,1:input$numcolXTilde]
        }else if(!isTruthy(input$numcolXTilde)){
          matrix = matrix[1:input$numrowXTilde,1:6]
        }else{
          matrix = matrix[1:input$numrowXTilde,1:input$numcolXTilde]
        }  
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)
  
  
  
  #Function to display entries from Matrix of Z
  output$optionColumnZ = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colZ", style="margin-top:30px;",
          checkboxGroupInput("variableZ", "Select Column Name for Matrix of Z:", inline=TRUE, colnames(dataFrame)),
          hr(),
      )
    }
  })
  output$inputrowZ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowZ', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolZ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolZ', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableZ)*nrow(dataFrame))), span("columns"))
  })

  Z = reactive({
    dataFrame = dataframe()
    matrix = matrix_X(dataframe_new =dataFrame, col.selected = input$variableZ)
    #return(matrix)
  })

  output$matrixZ = renderDT({
    if(length(input$variableZ)!=0){
      matrix = Z()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowZ) & !isTruthy(input$numcolZ)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowZ)){
        matrix =  matrix[1:6,1:input$numcolZ]
      }else if(!isTruthy(input$numcolZ)){
        matrix = matrix[1:input$numrowZ,1:6]
      }else{
        matrix = matrix[1:input$numrowZ,1:input$numcolZ]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to display entries from Matrix of J
  output$optionColumnJ = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colJ", style="margin-top:30px;",
          checkboxGroupInput("variableJ", "Select Column Name for Matrix of J:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowJ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowJ', "Number of rows to display: "), span('from '), span(as.character(length(input$variableJ)*2*nrow(dataFrame))), span("rows"))
  })
  output$inputcolJ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolJ', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableJ)*2)), span("columns"))
  })

  J = reactive({
    dataFrame = dataframe()
    matrix = matrixJ(dataframe_new = dataFrame, col.selected = input$variableJ)
    return(matrix)
  })
  output$matrixJ = renderDT({
    if(length(input$variableJ)!=0){
      matrix = J()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowJ) & !isTruthy(input$numcolJ)){
        if(nrow(matrix)<=6 | ncol(matrix)<=6){
          matrix = matrix[1:nrow(matrix),1:ncol(matrix)]
        }else{
          matrix = matrix[1:6,1:6]
        }
      }else if(!isTruthy(input$numrowJ)){
        matrix = matrix[1:6,1:input$numcolJ]
      }else if(!isTruthy(input$numcolJ)){
        matrix = matrix[1:input$numrowJ,1:6]
      }else{
        matrix = matrix[1:input$numrowJ,1:input$numcolJ]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to display the result of multiplying Kronecker Matrikz of Z with Identity matrix
  ZI = reactive({
    dataFrame = dataframe()
    # Generate latitude and longitude diagonal matrix
    matrixZOld = matrix_X(dataframe_new = dataFrame, col.selected = input$variableZ)
    # Kronecker multiplication of Matrix of Z and the Identity matrix of size covariate matrix of X
    matrixZNew = kronecker(matrixZOld, diag(length(input$variableX)))
    return(matrixZNew)
  })

  output$warningZI = renderText({
    if(length(input$variableZ) == 0){
      return("Please fill in the Matrix of Z")
    }else{
      return(NULL)
    }
  })
  output$inputrowZI = renderUI({
    if(length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(ZI())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowZI', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$inputcolZI = renderUI({

    if(length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(ZI())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolZI', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$matrixZI = renderDT({
    if(length(input$variableZ)!=0){
      matrix = data.frame(ZI())
      if(!isTruthy(input$numrowZI) & !isTruthy(input$numcolZI)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowZI)){
        matrix = matrix[1:6,1:input$numcolZI]
      }else if(!isTruthy(input$numcolZI)){
        matrix = matrix[1:input$numrowZI,1:6]
      }else{
        matrix = matrix[1:input$numrowZI,1:input$numcolZI]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to calculate Weight Matrix of W (Inverse Distance Matrix)
  normXY = function(X,Y){
    norm = matrix(NA, nrow = length(X), ncol = length(Y))
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        norm[i,j] = sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
      }
    }
    return(norm)
  }

  weighted = function(X,Y){
    weight = matrix(NA, nrow = length(X), ncol = length(Y))
    norm = normXY(X,Y)
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        if(i==j){
          weight[i,j] = as.numeric(0)
        }else{
          weight[i,j] = 1/(norm[i,j])^2
        }
      }
    }
    return(weight)
  }

  inverseWeighted = function(X,Y){
    inv.weighted =  matrix(NA, nrow = length(X), ncol = length(Y))
    weight.matrix = weighted(X,Y)
    total.row = rowSums(weight.matrix)
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        inv.weighted[i,j] = weight.matrix[i,j]/total.row[i]
      }
    }
    return(inv.weighted)
  }
  W = reactive({
    dataFrame = dataframe()
    if(length(input$variableW)==2){
      matrixW = inverseWeighted(dataFrame[,input$variableW[1]], dataFrame[,input$variableW[2]])
    }
    return(matrixW)
  })
  output$optionColumnW = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colW", style="margin-top:30px;",
          checkboxGroupInput("variableW", "Select Column Name for Matrix of W:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })


  output$inputrowW = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowW', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolW = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolW', "Number of columns to display: ",), span('from '), span(as.character(nrow(dataFrame))), span("columns"))
  })


  output$matrixW = renderDT({
    if(length(input$variableW)==2){
      matrix = data.frame(W())
      if(!isTruthy(input$numrowW) & !isTruthy(input$numcolW)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowW)){
        matrix = matrix[1:6,1:input$numcolW]
      }else if(!isTruthy(input$numcolW)){
        matrix = matrix[1:input$numrowW,1:6]
      }else{
        matrix = matrix[1:input$numrowW,1:input$numcolW]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })
  

  #Function to calculate the multiplication of Matrix of X, Kronecker Matrix Z*I and Matrix of J
  A = reactive({
    X = X()
    ZI = ZI()
    J = J()
    A = X%*%ZI%*%J
    return(A)
  })

  output$warningA = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$inputrowA = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(A())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowA', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$inputcolA = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(A())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolA', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$matrixA = renderDT({
    if(length(input$variableX) != 0 & length(input$variableJ) != 0 & length(input$variableZ) != 0){
      matrix = data.frame(A())
      if(!isTruthy(input$numrowA) & !isTruthy(input$numcolA)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowA)){
        matrix = matrix[1:6,1:input$numcolA]
      }else if(!isTruthy(input$numcolA)){
        matrix = matrix[1:input$numrowA,1:6]
      }else{
        matrix = matrix[1:input$numrowA,1:input$numcolA]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })
  
  thetab0MLE <<- list()
  thetaMLE = reactive({
    XT = XTilde()
    X = X()
    W = W()
    Y = Y()
    A = A()
    Y = Y()
    ZI = ZI()
    J = J()
    theta = solve(t(XT)%*%t(W)%*%W%*%XT)%*%t(XT)%*%t(W)%*%Y
    thetab0MLE <<-list("theta" = theta)
    b0 = inv(t(A)%*%A)%*%t(A)%*%(Y-W%*%XT%*%theta)
    thetab0MLE <<-list("theta" = theta, "b0" = b0)
    beta = ZI%*%J%*%as.numeric(unlist(thetab0MLE$b0))
    thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta)
    thetaNew = solve(t(W%*%XT)%*%W%*%XT)%*%t(W%*%XT)%*%(Y-A%*%as.numeric(unlist(thetab0MLE$b0)))
    thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta, "thetaNew" = thetaNew)
    tol = 0.001
    error = as.numeric(sum(abs(as.numeric(unlist(theta))-as.numeric(unlist(thetaNew)))))
    print(error)
    while(error>tol){
      theta = as.numeric(unlist(thetab0MLE["thetaNew"]))
      thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta, "thetaNew" = thetaNew)
      b0 = inv(t(A)%*%A)%*%t(A)%*%(Y-W%*%XT%*%as.numeric(unlist(thetab0MLE["theta"])))
      thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta, "thetaNew" = thetaNew)
      beta = ZI%*%J%*%as.numeric(unlist(thetab0MLE$b0))
      thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta, "thetaNew" = thetaNew)
      thetaNew = solve(t(W%*%XT)%*%W%*%XT)%*%t(W%*%XT)%*%(Y-A%*%as.numeric(unlist(thetab0MLE$b0)))
      thetab0MLE <<-list("theta" = theta, "b0" = b0, "beta" = beta, "thetaNew" = thetaNew)
      error = sum(abs(as.numeric(unlist(theta))-as.numeric(unlist(thetaNew))))
      print(error)
    }
    return(as.numeric(unlist(thetab0MLE["theta"])))
  })
  
  output$warningthetaMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of W, matrix of X, vector of Y, or matrix of Z")
    }
  })
  
  output$inputrowthetaMLE = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = thetab0MLE["theta"]
      matrix = data.frame(matrix)
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowthetaMLE', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  
  output$ThetaMLE = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = matrix(thetaMLE())
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowthetaMLE)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowthetaMLE,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  b0MLE = reactive({
    return(as.numeric(unlist(thetab0MLE["b0"])))
  })
  
  output$warningb0MLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of W, matrix of X, vector of Y, or matrix of Z")
    }
  })
  
  output$inputrowb0MLE = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = thetab0MLE["b0"]
  matrix = data.frame(matrix)
  div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowb0MLE', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  
  output$b0MLE = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = thetab0MLE["b0"]
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowb0MLE)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowb0MLE,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)
  
  betaMLE = reactive({
    ZI = ZI()
    J = J()
    beta = ZI%*%J%*%as.numeric(unlist(thetab0MLE$b0))
    return(beta)
  })
  
  output$warningbetaMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of W, matrix of X, vector of Y, or matrix of Z")
    }
  })
  output$inputrowbetaMLE = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = betaMLE()
      matrix = data.frame(matrix)
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowBeta', "Jumlah baris yang ingin ditampilkan: "), span('dari '), span(as.character(nrow(matrix))), span("baris"))
    }
  })
  
  output$betaMLE = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = betaMLE()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowBeta)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowBeta,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)
  
  output$warningbetawrtX = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  betaWRTX = reactive({
    beta = betaMLE()
    matrix = matrix(as.numeric(beta), nrow = nrow(dataframe()), ncol = length(input$variableX), byrow = FALSE)
  })
  output$inputrowbetawrtX = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(betaWRTX())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowBetaWRTX', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  
  output$inputcolbetawrtX = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(betaWRTX())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolBetaWRTX', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })
  
  output$betawrtX = renderDT({
    if(length(input$variableX) != 0 & length(input$variableJ) != 0 & length(input$variableZ) != 0){
      matrix = data.frame(betaWRTX())
      if(!isTruthy(input$numrowBetaWRTX) & !isTruthy(input$numcolBetaWRTX)){
        matrix = matrix[1:nrow(matrix),1:ncol(matrix)]
      }else if(!isTruthy(input$numrowBetaWRTX)){
        matrix = matrix[1:nrow(matrix),1:input$numcolBetaWRTX]
      }else if(!isTruthy(input$numcolBetaWRTX)){
        matrix = matrix[1:input$numrowBetaWRTX,1:ncol(matrix)]
      }else{
        matrix = matrix[1:input$numrowBetaWRTX,1:input$numcolBetaWRTX]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })
  
  YHatMLE = reactive({
    k = length(input$variableX)
    b0 = matrix(as.numeric(unlist(thetab0MLE["b0"])), nrow=2*k, ncol=1)
    theta = matrix(as.numeric(unlist(thetab0MLE["theta"])), nrow=k, ncol=1)
    A = A()
    XT = XTilde()
    W = W()
    yhat = A%*%b0 + W%*%XT%*%theta
    return(yhat)
  })
  
  output$warningYHatMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  
  output$inputrowYHatMLE = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(YHatMLE())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowYHat', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$YHatMLE = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = YHatMLE()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowYHatMLE)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowYHatMLE,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)
  
  ErrorMLE = reactive({
    Y = Y()
    Yhat = YHatMLE()
    Error = abs(Y-Yhat)
    return(Error)
  })
  
  output$warningErrorMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  
  output$inputrowErrorMLE = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(ErrorMLE())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowErrorMLE', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  
  output$ErrorMLE = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = ErrorMLE()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowErrorMLE)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowErrorMLE,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)
  
  
  RMSEMLE = reactive({
    return(sqrt(mean((ErrorMLE())^2)))
  })
  
  output$warningRMSEMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  
  output$RMSEMLE = renderText({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      RMSE = RMSEMLE()
    }else{
      return(NULL)
    }
  })
  
  #Function to calculate the MAPE value
  MAPEMLE = reactive({
    Y = Y()
    return(mean(abs((ErrorMLE())/Y))*100)
  })
  
  output$warningMAPEMLE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  
  output$MAPEMLE = renderText({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      MAPE = MAPEMLE()
    }else{
      return(NULL)
    }
  })

  output$warningPrediction = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$warningDownloadData = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  #Function to display download prediction data
  dataPrediction = reactive({
    #dataSelected = dataframe()[,input$variablePrediction]
    dataPrediction1 = betaWRTX()
    colnames(dataPrediction1) = paste(rep("beta", ncol(dataPrediction1)),as.character(c(1:ncol(dataPrediction1))))
    YHat = YHatMLE()
    colnames(YHat) = "Yhat"
    matrix = cbind(dataPrediction1,YHat)
  })
  output$optionColumnPrediction = renderUI({
    dataFrame = dataframe()
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      div(id = "colPrediction", style="margin-top:30px;",
          checkboxGroupInput("variablePrediction", "Select Columns of Location, Latitude, and Longitude:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })

  output$inputrowdataPrediction = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(dataPrediction())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowdataPrediction', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })

  output$inputcoldataPrediction = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(dataPrediction())
      div(style="display: inline-block;vertical-align:top;", textInput('numcoldataPrediction', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$optionColumnDownloadData = renderUI({
    dataFrame = dataframe()
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0 | input$downloaddata != "AD"){
      return(NULL)
    }else{
      div(id = "colDownloadData", style="margin-top:30px;",
          checkboxGroupInput("variableDownloadData", "Select Columns of Location, Latitude, and Longitude:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste(input$downloaddata,"-"), Sys.time(), ".csv", sep="")
    },

    content = function(file) {
      if(input$downloaddata == "VY"){
        dataFrame = Y()
      }else if(input$downloaddata == "MX"){
        dataFrame = X()
      }else if(input$downloaddata == "MXT"){
          dataFrame = XTilde()
      }else if(input$downloaddata == "MW"){
        dataFrame = W()
      }else if(input$downloaddata == "MJ"){
        dataFrame = J()
      }else if(input$downloaddata == "MZ"){
        dataFrame = Z()
      }else if(input$downloaddata == "MKZ"){
        dataFrame = ZI()
      }else if(input$downloaddata == "MA"){
        dataFrame = A()
      }else if(input$downloaddata == "VB0"){
          dataFrame = as.numeric(unlist(thetab0MLE["b0"]))
      }else if(input$downloaddata == "VT"){
          dataFrame = as.numeric(unlist(thetab0MLE["beta"]))
      }else if(input$downloaddata == "VB"){
        dataFrame = betaMLE()
      }else if(input$downloaddata == "VBX"){
        dataFrame = betaWRTX()
      }else if(input$downloaddata == "VBY"){
        dataFrame = YHatMLE()
      }else if(input$downloaddata == "VBE"){
        dataFrame = ErrorMLE()
      }else if(input$downloaddata == "AD"){
        dataFrame = dataPrediction()
        if(length(input$variableDownloadData)!=0){
          dataSelected = dataframe()[,input$variableDownloadData]
          dataFrame = cbind(dataSelected, dataFrame)
          colnamesPrediction = c(input$variableDownloadData, paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
          colnames(dataFrame) = colnamesPrediction
        }else{
          colnames(dataFrame) = c(paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
        }

      }

      write.table(dataFrame, file, sep=input$separator)
    })

  output$showDownloadData = renderDT({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      if(input$downloaddata == "VY"){
        dataFrame = Y()
      }else if(input$downloaddata == "MX"){
        dataFrame = X()
      }else if(input$downloaddata == "MXT"){
        dataFrame = XTilde()
      }else if(input$downloaddata == "MW"){
        dataFrame = W()
      }else if(input$downloaddata == "MJ"){
        dataFrame = J()
      }else if(input$downloaddata == "MZ"){
        dataFrame = Z()
      }else if(input$downloaddata == "MKZ"){
        dataFrame = ZI()
      }else if(input$downloaddata == "MA"){
        dataFrame = A()
      }else if(input$downloaddata == "VB0"){
        dataFrame = as.numeric(unlist(thetab0MLE["b0"]))
      }else if(input$downloaddata == "VB"){
        dataFrame = betaMLE()
      }else if(input$downloaddata == "VBX"){
        dataFrame = betaWRTX()
      }else if(input$downloaddata == "VT"){
        dataFrame = as.numeric(unlist(thetab0MLE["theta"]))
      }else if(input$downloaddata == "VBY"){
        dataFrame = YHatMLE()
      }else if(input$downloaddata == "VBE"){
        dataFrame = ErrorMLE()
      }else if(input$downloaddata == "AD"){
        dataFrame = dataPrediction()
      }
      matrix = data.frame(dataFrame)
      if(input$downloaddata == "AD"){
        if(length(input$variableDownloadData)!=0){
          dataSelected = dataframe()[,input$variableDownloadData]
          matrix = cbind(dataSelected, dataFrame)
          colnamesPrediction = c(input$variableDownloadData, paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
          colnames(matrix) = colnamesPrediction
        }else{
          colnames(matrix) = c(paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
        }
        matrix
      }else{
        matrix
      }
      

    }else{
      return(NULL)
    }
  })


  
}
