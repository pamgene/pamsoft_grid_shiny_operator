library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

library(jsonlite)
library(shinybusy)

library(imager)
library(DT)
library(stringr)
library(tiff)

library(tim)
library(tools)

library(stringi)


# http://127.0.0.1:5402/test-team/w/8ef9012b2d2f050214e16189ba0406b4/ds/032404ca-b2af-4f67-8806-6bd0ffa8fff5/wa
# options("tercen.workflowId"= "8ef9012b2d2f050214e16189ba0406b4")
# options("tercen.stepId"= "032404ca-b2af-4f67-8806-6bd0ffa8fff5")

# http://127.0.0.1:5402/test-team/w/8ef9012b2d2f050214e16189ba0406b4/ds/a73a2842-ff0a-4db3-8f7e-cd5ce72bdb67
# options("tercen.workflowId"= "8ef9012b2d2f050214e16189ba0406b4")
# options("tercen.stepId"= "a73a2842-ff0a-4db3-8f7e-cd5ce72bdb67")

# http://127.0.0.1:5402/test-team/w/8ef9012b2d2f050214e16189ba0406b4/ds/032404ca-b2af-4f67-8806-6bd0ffa8fff5
# options("tercen.workflowId"= "8ef9012b2d2f050214e16189ba0406b4")
# options("tercen.stepId"= "032404ca-b2af-4f67-8806-6bd0ffa8fff5")


############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################



# ================================================
# SERVER FUNCTION
# ================================================
shinyServer(function(input, output, session) {
  
  df        <- reactiveValues( data=NULL  )

  gridSpotList <- reactiveValues( gridList=NULL, selectedGrid=1,
                                  imageList=NULL, selectedImage=-1 )
  
  grid      <- reactiveValues( X=NULL, Y=NULL, ROW=NULL, COL=NULL,
                               XD=NULL, YD=NULL, 
                               R=NULL, TYPE=NULL, MANUAL=NULL)
  selection <- reactiveValues( image=NULL  )

  mode      <- reactive({getMode(session)})
  
  docIdCols   <- reactive( get_document_id_cols( session )  )
  imgInfo <- reactive(prep_image_folder(session, docIdCols)  )
  
  imgDir <- tempdir(check=TRUE)
  # END OF SERVER VARIABLES DEFINITION
  # +++++++++
  
  output$opMode <- renderText({
    paste0("Mode is: ", mode() ) 
  })

  props     <- reactive({get_operator_props(getCtx(session), imgInfo())})
  
  output$selectedImage <- renderImage({
    req(imgInfo)


    if(is.null(df$data)){
      df$data <- get_data(session)
    }
    
    if( is.null(gridSpotList$gridList)){
      gridSpotList$gridList <- get_image_used_list( session )
      gridSpotList$selectedGrid <- 1
    }
    
    if( is.null(gridSpotList$imageList)){
      gridSpotList$imageList   <- get_image_list( df$data, 
                                                  gridSpotList$gridList[[gridSpotList$selectedGrid]] )
      gridSpotList$selectedImage <- 1
    }

    m <- mode()
    
    if( !is.null(m) && m == "run"){
      shinyjs::enable("runBtn")
    }

    outfile <- tempfile(fileext = '.jpeg', tmpdir = imgDir)
    
    selection$image <- gridSpotList$imageList[[1]][[gridSpotList$selectedImage]]

    selectedImage <- paste0( imgInfo()[1], '/', selection$image, '.', imgInfo()[2] )
    
    dfImg <- reactive(df$data %>% filter(Image == selection$image ) )
    
    grid$X <- (dfImg() %>% filter(variable == "gridX") %>% pull(.y))
    grid$Y <- (dfImg() %>% filter(variable == "gridY") %>% pull(.y))
    
    grid$ROW <- (dfImg() %>% filter(variable == "gridX") %>% pull(spotRow))
    grid$COL <- (dfImg() %>% filter(variable == "gridY") %>% pull(spotCol))
    
    grid$R <- (dfImg() %>% filter(variable == "diameter") %>% pull(.y))
    grid$MANUAL <- (dfImg() %>% filter(variable == "manual") %>% pull(.y))
    
    grid$TYPE <- (dfImg() %>% filter(variable == "bad") %>% pull(.y)) +
      (dfImg() %>% filter(variable == "empty") %>% pull(.y)) * 2 
    
    
    bf <- as.double(input$brightness)
    ct <- as.double(input$contrast)
    img <- drop(suppressWarnings( tiff::readTIFF(selectedImage) * 16 ))
    # Change to width x height
    img <- as.cimg(aperm(img, c(2,1)))
    
    img <- img / max(img)
    
    img <- ct * ((img + bf) - 0.5) + 0.5

    img[img > 1] = 1
    img[img < 0] = 0
    
    imager::save.image(img,outfile)
    
    list(src = outfile,
         id = "gridImage",
         contentType = 'image/jpeg',
         alt = "Grid image could not be loaded.")
  }, deleteFile = TRUE)
  
  
  observe({
    req(props)
    req(grid$X)
    req(grid$Y)

    spotPitch <- props()$grdSpotPitch
    spotSize  <- props()$grdSpotSize


    off <- (spotPitch * spotSize)/2

    x <- grid$Y
    y <- grid$X
    r <- grid$R/2 
    t <- grid$TYPE

  #IF manually moving th egrid, shoulld segmentation run again, or onl if a new grid is created?
    gridDf <- data.frame(x=x, y=y, radius=r, type=t, manual=grid$MANUAL[[1]])
    session$sendCustomMessage("imageDisplay", gridDf)
  })


  #Receive updated grid positions from JS
  observeEvent(input$gridOverlay, {
    req(df$data)
    
    N <- length(input$gridOverlay)
    gridInput<-as.vector(input$gridOverlay)


    Y <- gridInput[seq.int(1, N, by = 6)]
    X <- gridInput[seq.int(2, N, by = 6)]

    currentX <- grid$X
    currentY <- grid$Y
    
    data <- df$data
    

    # Changes to the images used for gridding are applied to all relevant images
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "gridX"] = X
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "gridY"] = Y
    
    # Needs to go through position refinement after segmentation 
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "grdXFixedPosition"] = 0
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "grdYFixedPosition"] = 0

    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "manual"] = 1
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "bad"]    = 0
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "empty"]  = 0

    df$data <- data
  })
   
 


  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Image and Grid button events (previous and next)
  observeEvent(input$nextGridBtn, {
    gridSpotList$selectedGrid <- gridSpotList$selectedGrid + 1


    if(gridSpotList$selectedGrid == length( gridSpotList$gridList ) ){
      shinyjs::disable( "nextGridBtn"  )
    }else{
      shinyjs::enable( "nextGridBtn"  )
    }
    shinyjs::enable( "prevGridBtn"  )
    

     gridSpotList$imageList   <- get_image_list( df$data, 
                                                 gridSpotList$gridList[[gridSpotList$selectedGrid]] )
     gridSpotList$selectedImage <- 1

     session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)
    
     shiny::updateSelectInput(session=session, inputId="imageused", 
                      choices=gridSpotList$gridList,
                      selected = gridSpotList$gridList[[gridSpotList$selectedGrid]] )
  }) # END OF nextGridBtn event


  observeEvent(input$prevGridBtn, {
    gridSpotList$selectedGrid <- gridSpotList$selectedGrid - 1

    if(gridSpotList$selectedGrid == 1 ){
     shinyjs::disable( "prevGridBtn"  )
    }else{
     shinyjs::enable( "prevGridBtn"  )
    }
    shinyjs::enable( "nextGridBtn"  )

    gridSpotList$imageList   <- get_image_list( df$data, 
                                                gridSpotList$gridList[[gridSpotList$selectedGrid]] )
    gridSpotList$selectedImage <- 1
    
    session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)
    
    shiny::updateSelectInput(session=session, inputId="imageused", 
                             choices=gridSpotList$gridList,
                             selected = gridSpotList$gridList[[gridSpotList$selectedGrid]] )
  }) # END OF prevGridBtn event


  observeEvent(input$nextImgBtn, {
    gridSpotList$selectedImage <- gridSpotList$selectedImage + 1

    nImg <- nrow(  gridSpotList$imageList )
    nGrid <- length(  gridSpotList$gridList )

    # From last image of a grid, jump to the last image of the subsequent grid [image used for gridding]
    if( gridSpotList$selectedImage > nImg  && 
        gridSpotList$selectedGrid < nGrid ){
    
      # Update grid and init grid selection
      gridSpotList$selectedGrid <- gridSpotList$selectedGrid + 1
      
      
      if(gridSpotList$selectedGrid == length( gridSpotList$gridList ) ){
        shinyjs::disable( "nextGridBtn"  )
      }else{
        shinyjs::enable( "nextGridBtn"  )
      }
      shinyjs::enable( "prevGridBtn"  )
      
      
      gridSpotList$imageList   <- get_image_list( df$data, 
                                                  gridSpotList$gridList[[gridSpotList$selectedGrid]] )
      gridSpotList$selectedImage <- 1
      
      session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)
      
      shiny::updateSelectInput(session=session, inputId="imageused", 
                               choices=gridSpotList$gridList,
                               selected = gridSpotList$gridList[[gridSpotList$selectedGrid]] )

      
    }else{
      session$sendCustomMessage("select_row", gridSpotList$selectedImage)
    }

    shinyjs::enable( "prevImgBtn"  )



    if(gridSpotList$selectedImage  < length(gridSpotList$imageList ) ||
       gridSpotList$selectedGrid <  nGrid){
      shinyjs::enable( "nextImgBtn"  )
    }else{
      shinyjs::disable( "nextImgBtn"  )
    }
    

  } ) # END OF nextImgBtn event


  observeEvent(input$prevImgBtn, {
    gridSpotList$selectedImage <- gridSpotList$selectedImage - 1
    
    # From last image of a grid, jump to the last image of the subsequent grid [image used for gridding]
    if( gridSpotList$selectedImage < 1  &&  gridSpotList$selectedGrid > 1 ){
      
      # Update grid and init grid selection
      gridSpotList$selectedGrid <- gridSpotList$selectedGrid - 1
      
      if(gridSpotList$selectedGrid == 1 ){
        shinyjs::disable( "prevGridBtn"  )
      }else{
        shinyjs::enable( "prevGridBtn"  )
      }
      shinyjs::enable( "nextGridBtn"  )
      
      gridSpotList$imageList   <- get_image_list( df$data, 
                                                  gridSpotList$gridList[[gridSpotList$selectedGrid]] )
      gridSpotList$selectedImage <- 1
      
      session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)
      
      shiny::updateSelectInput(session=session, inputId="imageused", 
                               choices=gridSpotList$gridList,
                               selected = gridSpotList$gridList[[gridSpotList$selectedGrid]] )
    }else{
      session$sendCustomMessage("select_row", gridSpotList$selectedImage)
    }
    
    shinyjs::enable( "nextImgBtn"  )

    
    if(gridSpotList$selectedImage  > 1 ||
       gridSpotList$selectedGrid >  1){
      shinyjs::enable( "prevImgBtn"  )
    }else{
      shinyjs::disable( "prevImgBtn"  )
    }
    
  } ) # END OF prevImgBtn event

  # END of Image and Grid button events
  #+++++++++++++++++++++++++++++++++++++++++++++++++



  observeEvent(input$imageused,{
    
    gridSpotList$selectedGrid <- which(gridSpotList$gridList == input$imageused)
    
    
    gridSpotList$imageList   <- get_image_list( df$data, 
                                                gridSpotList$gridList[[gridSpotList$selectedGrid]] )
    gridSpotList$selectedImage <- 1
    
    session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)


    if( gridSpotList$selectedGrid == 1){
      shinyjs::disable( "prevGridBtn"  )
      if(gridSpotList$selectedImage == 1){
        shinyjs::disable( "prevImgBtn"  )
      }else{
        shinyjs::enable( "prevImgBtn"  )
      }
    }else{
      shinyjs::enable( "prevGridBtn"  )
      shinyjs::enable( "prevImgBtn"  )
    }

    if( gridSpotList$selectedGrid == length(gridSpotList$gridList) ){
      shinyjs::disable( "nextGridBtn"  )
      
      if(gridSpotList$selectedImage == nrow(gridSpotList$imageList) ){
        shinyjs::disable( "nextImgBtn"  )
      }else{
        shinyjs::enable( "nextImgBtn"  )
      }
    }else{
      shinyjs::enable( "nextGridBtn"  )
    }
  })

  observeEvent( input$selectedImageRow, {
    # Comes as character from JS
    if(length(input$selectedImageRow) > 0){
      gridSpotList$selectedImage <- as.numeric(input$selectedImageRow)
    }

    if(gridSpotList$selectedImage == nrow(gridSpotList$imageList) &&
       gridSpotList$selectedGrid == length(gridSpotList$gridList)){
      shinyjs::disable( "nextImgBtn"  )
    }else{
      shinyjs::enable( "nextImgBtn"  )
    }

    if( gridSpotList$selectedGrid == 1){
      shinyjs::disable( "prevGridBtn"  )
      shinyjs::enable( "nextImgBtn"  )

      if(gridSpotList$selectedImage == 1){
        shinyjs::disable( "prevImgBtn"  )
      }else{
        shinyjs::enable( "prevImgBtn"  )
      }
    }else{
      shinyjs::enable( "prevGridBtn"  )
    }
    
    session$sendCustomMessage("select_row", gridSpotList$selectedImage)
  })

  output$imageusedpanel<-renderUI({
    req(gridSpotList$gridList)

    selectInput("imageused", "Grid Image", choices=gridSpotList$gridList,
                              selectize = TRUE, multiple = FALSE)
  })

  output$images <- DT::renderDT( {
      if (!is.null(gridSpotList$imageList)){ # Wait for the imageList to load
        DT::datatable( data=gridSpotList$imageList,
                   rownames = unlist(append( list("G"), rep("I", nrow(gridSpotList$imageList)-1)   )), 
                   selection='none', 
                   colnames="", filter="none", style="bootstrap4",
                   options = list(pageLength=15, pageLengthLsit=c(5,15,30),
                                  processing=FALSE, searching=FALSE ),
                   extensions=c("Select"),
                   callback=JS("
                                $('#images').find('table').DataTable().row(0).select();
                                
                                table.on('click.dt', 'tr', function(e, dt, type, indexes) {
                                var row = $(this).index()+1;
  
                                Shiny.setInputValue('selectedImageRow', row);
                            })")
                   ) 
      }
    }, server = FALSE)
  


  observeEvent( input$gridBtn, {
    show_modal_spinner(text = "Calculating Grid Position ... Please Wait")

    spotPitch <- props()$grdSpotPitch
    spotSize  <- props()$grdSpotSize
    
    selection$image <- gridSpotList$imageList[[1]][gridSpotList$selectedImage]
    
    selectedImage   <- paste0( imgInfo()[1], '/', selection$image, '.', imgInfo()[2] )

    dfImg <- df$data %>% filter(Image == selection$image )

    spotRow <- dfImg %>% filter(.ri == 0) %>% pull(spotRow)
    spotCol <- dfImg %>% filter(.ri == 0) %>% pull(spotCol)

    
    imgHdr <- suppressWarnings( tiff::readTIFF(selectedImage, payload=FALSE)  )
    # Note, image display is rotated
    imgHeight <- as.numeric(imgHdr$width )
    imgWidth <- as.numeric(imgHdr$length)
    
    imCenter.x <- imgWidth/2
    imCenter.y <- imgHeight/2
    
    
    # Matching matlab code
    # Place REF spots
    refIdx <- list()
    stdIdx <- list()
    
    refI   <- 1
    stdI   <- 1
    for( i in seq_along(spotRow)){
      if( spotRow[i] < 0 || spotCol[i] < 0){
        refIdx[refI] <- i
        refI <- refI + 1
      }else{
        stdIdx[stdI] <- i
        stdI <- stdI + 1
      }
    }
    
    nRef <- refI - 1
    spotsX <- list()
    spotsY <- list()
    
    for( i in 1:2 ){
      if(i==1){
        row <- abs(spotRow[unlist(refIdx)])
        col <- abs(spotCol[unlist(refIdx)])  
      }else{
        row <- abs(spotRow[unlist(stdIdx)])
        col <- abs(spotCol[unlist(stdIdx)])
        
      }
      
  
      # calculate grid coordinates, zeros centered
      rmp <- min(row) + (max(row)-min(row))/2
      cmp <- min(col) + (max(col)-min(col))/2
       
      # Considering 0 offset
      x = spotPitch*(row-rmp)
      y = spotPitch*(col-cmp)
      
  
      x = imCenter.x + x + 1
      y = imCenter.y + y + 1
      
      off <- 0
      if( i == 2){
        off <- nRef
      }
      for(i in seq_along(x)){
        spotsX[i+off] <- x[i]
        spotsY[i+off] <- y[i]
      }
    }
    

    off <- (spotPitch * spotSize)/2

    x <- unlist(spotsY)
    y <- unlist(spotsX)
    r <- rep( off*2, length(x) )
    t <- rep( 0, length(x) )

    newGrid <- data.frame(x=x, y=y, radius=r, type=t, manual=1)
    session$sendCustomMessage("imageDisplay", newGrid)



    if(selection$image == gridSpotList$gridList[[gridSpotList$selectedGrid]]){
      # Update position for all grids
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridX"] = y
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridY"] = x
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "diameter"] = r
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "bad"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "empty"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "manual"] = 1
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdXFixedPosition"] = 0 
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdYFixedPosition"] = 0 
      
      
    }else{
      # Update the image used for gridding
      df$data$grdImageNameUsed[df$data$Image == selection$image  ] = selection$image
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridX"] = y
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridY"] = x
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdXFixedPosition"] = 0 
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdYFixedPosition"] = 0 
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdRotation"] = 0
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "diameter"] = r
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "bad"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "empty"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "manual"] = 1
      
      # Rebuiild image list
      gridSpotList$gridList <- sort(unique(df$data$grdImageNameUsed))
      gridSpotList$selectedGrid <- which( gridSpotList$gridList ==  selection$image)
      
      shiny::updateSelectInput(session=session, inputId="imageused", 
                               choices=gridSpotList$gridList,
                               selected = gridSpotList$gridList[[gridSpotList$selectedGrid]] )
      
      session$sendCustomMessage("init_grid_images", gridSpotList$selectedImage)

    }
    


    remove_modal_spinner()
  } )

  observeEvent( input$runBtn, {
      progress <- Progress$new(session, min=1, max=1)
      progress$set(message="Running ... please wait ...")

      shinyjs::disable("runBtn")
      shinyjs::disable("gridBtn")

      

      tryCatch({
          ctx <- getCtx(session)

          #inData <- ctx$select()

          idxX <- which(df$data$variable == "gridX")
          idxY <- which(df$data$variable == "gridY")
          idxFX <- which(df$data$variable == "grdXFixedPosition")
          idxFY <- which(df$data$variable == "grdYFixedPosition")
          idxR <- which(df$data$variable == "diameter")
          idxM <- which(df$data$variable == "manual")
          idxC <- which(df$data$variable == "gridY" )
          
          idxB <- which(df$data$variable == "bad")
          idxE <- which(df$data$variable == "empty")
          idxO <- which(df$data$variable == "grdRotation")

     
          outDf <- data.frame(
            .ci=df$data$.ci[ idxC  ],
            gridX=df$data$.y[ idxX  ],
            gridY=df$data$.y[ idxY  ],
            grdXFixedPosition=df$data$.y[ idxFX  ],
            grdYFixedPosition=df$data$.y[ idxFY  ],
            diameter=df$data$.y[ idxR  ],
            manual=df$data$.y[ idxM  ],
            bad=df$data$.y[ idxB  ],
            empty=df$data$.y[ idxE  ],
            grdRotation=df$data$.y[ idxO  ],
            grdImageNameUsed=df$data$grdImageNameUsed[idxX], 
            Image=df$data$Image[idxX]  )


          outDf %>%
          ctx$addNamespace() %>%
          ctx$save()
          progress$close()
      }, error = function(e) {
        progress$set(message=paste0("Failed : ", toString(e)))
        print(paste0("Failed : ", toString(e)))
      })

      # df$data$.y[manualIdx & df$data$variable == "gridY"] <- oldY
      # df$data$.y[manualIdx & df$data$variable == "gridX"] <- oldX
  }) #END observeEvent : input$saveBtn

  
  # Called after all widgets are loaded 
  observeEvent( input$pageLoaded, {
    shinyjs::enable("gridBtn")
    remove_modal_spinner()
  })
})



get_image_list <- function(df, imageUsed){
  req(df)
  req(imageUsed)

  values <- df %>% select(c("Image", "grdImageNameUsed")) %>%
            filter(grdImageNameUsed == imageUsed) %>%
            pull(Image) %>% unique() %>% as.data.frame() 

  # Adjust based on image grid
  if( nrow(values) > 1){
    gridImgIdx <- which(values == imageUsed)
    values <- values$.[- gridImgIdx]
    
  
    # Sort the list
    sortVals <- sort(unlist(as.list(lapply( values, function(x){
      factors <- str_split(x, '[_]', Inf)
      cyc <- factors[[1]][5]
      cyc <- as.numeric(substr(cyc, 2, 100))
      return(cyc)
    }))), decreasing = TRUE, index.return = TRUE)
    
    values <- append(as.list(imageUsed), as.list(values[unlist(sortVals[2])]) )
  }
  
  outDf <- do.call(rbind.data.frame, values)
  colnames(outDf) <- c('.')
  
  
  #return(as.data.frame(rev(values[[1]])))
  return( outDf )
}


get_image_used_list <- function( session ){
 ctx <- getCtx(session)

 required.cnames = c("grdImageNameUsed")
 cnames.with.ns = ctx$cnames
 required.cnames.with.ns = lapply(required.cnames, function(required.cname){
   Find(function(cname.with.ns){
     endsWith(cname.with.ns, required.cname)
   }, cnames.with.ns, nomatch=required.cname)
 })


 values <- ctx$cselect(required.cnames.with.ns) %>% unique() %>% as.list()


 return(values[[1]])
}


get_document_id_cols <- function( session ){
  
  ctx <- getCtx(session)
  #values <- ctx %>% cselect("documentId") %>% unique() %>% as.list()
  
  
  colNames <- ctx$cnames %>% as.list()
  
  # Checking for documentId columns
  docIdCols <- unname(unlist(colNames[unlist(lapply(colNames, function(x){
    return(grepl("documentId", x, fixed = TRUE))
  } ))]))

  return(docIdCols)
}

getMode = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}


get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Table Data")
  
  show_modal_spinner(spin="fading-circle", text = "Loading data")

  # documetnId column may have the prefix, as it is now possible to have multiple document columns  
  colNames <- ctx$cnames %>% as.list()
  
  # Checking for documentId columns
  docIdCols <- unname(unlist(colNames[unlist(lapply(colNames, function(x){
    return(grepl("documentId", x, fixed = TRUE))
  } ))]))
  

  required.cnames = append( docIdCols, c("grdImageNameUsed","Image","spotRow","spotCol","ID") )
  required.rnames = c("variable")
  
  cnames.with.ns = ctx$cnames
  rnames.with.ns = ctx$rnames
  
  # here we keep the order of required.cnames
  required.cnames.with.ns = lapply(required.cnames, function(required.cname){
    Find(function(cname.with.ns){
      endsWith(cname.with.ns, required.cname)
    }, cnames.with.ns, nomatch=required.cname)
  })
  required.rnames.with.ns = lapply(required.rnames, function(required.rname){
    Find(function(rname.with.ns){
      endsWith(rname.with.ns, required.rname)
    }, rnames.with.ns, nomatch=required.rname)
  })

  cTable <- ctx$cselect(required.cnames.with.ns)
  rTable <- ctx$rselect(required.rnames.with.ns)

  # override the names
  names(cTable) = required.cnames
  names(rTable) = required.rnames
  

  qtTable <- ctx$select(c(".ci", ".ri", ".y"))
  cTable[[".ci"]] = seq(0, nrow(cTable)-1)
  rTable[[".ri"]] = seq(0, nrow(rTable)-1)
  
  qtTable <- dplyr::left_join(qtTable,cTable,by=".ci")
  qtTable <- dplyr::left_join(qtTable,rTable,by=".ri")


  qtTable$variable <- stri_split_fixed(qtTable$variable, ".", 2, simplify = TRUE)[,2]

  # Fix the position to avoid re-running segmentation position refinement  
  qtTable$.y[ qtTable$variable == 'grdXFixedPosition' ] = qtTable$.y[ qtTable$variable == 'gridX' ]
  qtTable$.y[ qtTable$variable == 'grdYFixedPosition' ] = qtTable$.y[ qtTable$variable == 'gridY' ]
  
  progress$close()
  
  return(qtTable)
  
}


prep_image_folder <- function(session, docIdCols){
  
  docIdCols <- docIdCols()
  progress <- Progress$new(session, min=1, max=1)
  
  
  progress$set(message="Downloading images")
  
  ctx <- getCtx(session)
  show_modal_spinner(spin="fading-circle", text = "Loading data")
  
  progress$set(message=paste0( length(docIdCols), " documentId columns")  )
  
  if(length(docIdCols) == 1){
    docIds <- ctx$cselect(docIdCols)

    f.names <- tim::load_data(ctx, unique(unlist(docIds[1])) )
    f.names <- grep('*/ImageResults/*', f.names, value = TRUE )

    imageResultsPath <- dirname(f.names[1])
    layoutDir <- dirname(imageResultsPath)
    fext <- file_ext(f.names[1])
    res <- (list(imageResultsPath, fext, layoutDir))
  }else{
    docIds <- ctx$cselect(docIdCols)

    f.names.a <- tim::load_data(ctx, unique(unlist(docIds[1]))[1] )
    f.names.b <- tim::load_data(ctx, unique(unlist(docIds[2])) )

    f.names <- grep('*/ImageResults/*', f.names.a, value = TRUE )
    a.names <- f.names.b

    
    progress$set(message=paste0( unique(unlist(docIds[1])), '  :  ', f.names.a )  )
    stop('check')
    if(length(f.names) == 0 ){
      f.names <- grep('*/ImageResults/*', f.names.b, value = TRUE )
      a.names <- f.names.a
    }

    if(length(f.names) == 0 ){
      progress$set(message="No 'ImageResults/' path found within provided files.")
      
      progress$set(message=paste0(f.names.a,
                                  '  -  ',
                                  f.names.b))
      
      
      stop("No images found")

    }
    
    
    imageResultsPath <- dirname(f.names[1])
    fext <- file_ext(f.names[1])
    layoutDir <- dirname(a.names[1])

    res <- (list(imageResultsPath, fext, layoutDir))
    
    
  }

  progress$close()
  
  # Images for all series will be here
  return(res)
  
}


get_operator_props <- function(ctx, imgInfo){
  imagesFolder <- imgInfo[1]
  
  
  sqcMinDiameter     <- 0.45
  sqcMaxDiameter     <- 0.85
  grdSpotPitch       <- 0
  grdSpotSize        <- 0.66
  grdRotation        <- seq(-2,2, by=0.25)
  qntSaturationLimit <- 4095
  segMethod          <- "Edge"
  segEdgeSensitivity <- list(0, 0.05)
  isDiagnostic       <- TRUE

  # Get the image width
  #Evolve2 -> Width 697

  operatorProps <- ctx$query$operatorSettings$operatorRef$propertyValues

  for( prop in operatorProps ){

    if (prop$name == "Min Diameter"){
      sqcMinDiameter <- as.numeric(prop$value)
    }

    if (prop$name == "Max Diameter"){
      sqcMaxDiameter <- as.numeric(prop$value)
    }

    if (prop$name == "Rotation"){
      if(prop$value == "0"){
        grdRotation <- as.numeric(prop$value)
      }else{
        prts <- as.numeric(unlist(str_split(prop$value, ":")))
        grdRotation <- seq(prts[1], prts[3], by=prts[2])
      }
    }

    if (prop$name == "Saturation Limit"){
      qntSaturationLimit <- as.numeric(prop$value)
    }

    if (prop$name == "Spot Pitch"){
      grdSpotPitch <- as.numeric(prop$value)
    }

    if (prop$name == "Spot Size"){
      grdSpotSize <- as.numeric(prop$value)
    }

    if (prop$name == "Diagnostic Output"){
      if( prop$value == "Yes" ){
        isDiagnostic <- TRUE
      }else{
        isDiagnostic <- FALSE
      }

    }

    if (prop$name == "Edge Sensitivity"){
      segEdgeSensitivity[2] <- as.numeric(prop$value)
    }
  }


  if( grdSpotPitch == 0 ){
    img_type <- get_imageset_type(imagesFolder)
    switch (img_type,
            "evolve3" = {grdSpotPitch<-17.0},
            "evolve2" = {grdSpotPitch<-21.5},
            "none"={stop("Cannot automatically detect Spot Pitch. Please set it to a value different than 0.")}
    )
  }
  
  props <- list()

  props$sqcMinDiameter <- sqcMinDiameter
  props$sqcMaxDiameter <- sqcMaxDiameter
  props$grdSpotPitch <- grdSpotPitch
  props$grdSpotSize <- grdSpotSize
  props$grdRotation <- grdRotation
  props$qntSaturationLimit <- qntSaturationLimit
  props$segEdgeSensitivity <- segEdgeSensitivity
  props$segMethod <- segMethod
  props$isDiagnostic <- isDiagnostic

  # Get array layout
  
  layoutDir <- paste(imgInfo[3], "*Layout*", sep = "/")
  props$arraylayoutfile <- Sys.glob(layoutDir)


  return (props)
}


get_imageset_type <- function(imgPath){
  defaultW <- getOption("warn")
  options(warn = -1)
  
  
  exampleImage <- Sys.glob(paste(imgPath, "*tif", sep = "/"))[[1]]
  
  tiffHdr <- readTIFF(exampleImage, payload = FALSE)
  
  imgTypeTag <- "none"
  
  if( tiffHdr['width'] == 552 && tiffHdr['length'] == 413){
    # Evolve3 Image Set
    imgTypeTag <- "evolve3"
  }
  
  if( tiffHdr['width'] == 697 && tiffHdr['length'] == 520){
    # Evolve2 Image Set
    imgTypeTag <- "evolve2"
  }
  options(warn = defaultW)
  return(imgTypeTag)
}