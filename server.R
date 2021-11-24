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



#http://localhost:5402/admin/w/ac924e73ee442b910408775d770a36be/ds/b2f94ffe-9764-4d76-8fa4-ddf0b54ba6eb
# options("tercen.workflowId"= "ac924e73ee442b910408775d770a36be")
# options("tercen.stepId"= "b2f94ffe-9764-4d76-8fa4-ddf0b54ba6eb")


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
  
  grid      <- reactiveValues( X=NULL, Y=NULL, 
                               XD=NULL, YD=NULL, 
                               R=NULL, TYPE=NULL, MANUAL=NULL)
  selection <- reactiveValues( image=NULL  )

  mode      <- reactive({getMode(session)})
  
  docId   <- reactive( get_document_id( session )  )
  imgInfo <- reactive(prep_image_folder(session, docId)  )
  
  imgDir <- tempdir(check=TRUE)
  # END OF SERVER VARIABLES DEFINITION
  # +++++++++
  
  output$opMode <- renderText({
    paste0("Mode is: ", mode() ) 
  })

  props     <- reactive({get_operator_props(getCtx(session), imgInfo()[1])})
  
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
    
    grid$Y <- (dfImg() %>% filter(variable == "gridX") %>% pull(.y))
    grid$X <- (dfImg() %>% filter(variable == "gridY") %>% pull(.y))
    
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

    x <- grid$X
    y <- grid$Y
    r <- grid$R/2 #rep( off, length(x) )
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


    X <- gridInput[seq.int(1, N, by = 6)]
    Y <- gridInput[seq.int(2, N, by = 6)]


    data <- df$data

    # Changes to the images used for gridding are applied to all relevant images

    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "gridX"] = Y
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "gridY"] = X

    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "manual"] = 1
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "bad"] = 0
    data$.y[df$data$grdImageNameUsed == gridSpotList$gridList[[gridSpotList$selectedGrid]] & df$data$variable == "empty"] = 0

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

    spotsX <- list()
    spotsY <- list()

    tetrisSpotCenter.x <- 0
    tetrisSpotCenter.y <- 0
    stdSpotCenter.x    <- 0
    stdSpotCenter.y    <- 0

    nTetris <- 0
    nStd    <- 0

    for(i in seq_along(spotRow)){
      spot.x <- abs(spotRow[i]) * spotPitch
      spot.y <- abs(spotCol[i]) * spotPitch

      spotsX <- append(spotsX, spot.x)
      spotsY <- append(spotsY, spot.y)

      if( spotRow[i] < 0 ){
        tetrisSpotCenter.x <- tetrisSpotCenter.x + spot.x
        tetrisSpotCenter.y <- tetrisSpotCenter.y + spot.y
        nTetris <- nTetris + 1
      }else{
        stdSpotCenter.x <- stdSpotCenter.x + spot.x
        stdSpotCenter.y <- stdSpotCenter.y + spot.y
        nStd <- nStd + 1
      }
    }


    tetrisSpotCenter.x <- tetrisSpotCenter.x / nTetris
    tetrisSpotCenter.y <- tetrisSpotCenter.y / nTetris

    stdSpotCenter.x <- stdSpotCenter.x / nStd
    stdSpotCenter.y <- stdSpotCenter.y / nStd

    diffCenter.x <- tetrisSpotCenter.x - stdSpotCenter.x
    diffCenter.y <- tetrisSpotCenter.y - stdSpotCenter.y

    spotCenter.x    <- 0
    spotCenter.y    <- 0
    nSpots <- 0

    for(i in seq_along(spotRow)){
      spot.x <- spotsX[[i]]
      spot.y <- spotsY[[i]]

      spot.x <- spot.x + diffCenter.x
      spot.y <- spot.y + diffCenter.y

      spotCenter.x <- spotCenter.x + spot.x
      spotCenter.y <- spotCenter.y + spot.y

      nSpots <- nSpots + 1
      if( spotRow[i] > 0 ){
        spotsX[i] <- spot.x
        spotsY[i] <- spot.y
      }
    }

    spotCenter.x <- spotCenter.x/nSpots
    spotCenter.y <- spotCenter.y/nSpots


    imgHdr <- suppressWarnings( tiff::readTIFF(selectedImage, payload=FALSE)  )
    # Note, image display is rotated
    imgHeight <- as.numeric(imgHdr$width )
    imgWidth <- as.numeric(imgHdr$length)


    imCenter.x <- imgWidth/2
    imCenter.y <- imgHeight/2

    # Center grid on image
    diffCenter.x <- spotCenter.x - imCenter.x
    diffCenter.y <- spotCenter.y - (imgHeight - imCenter.y)


    for(i in seq_along(spotsY)){
        spot.x <- spotsX[[i]]
        spot.y <- spotsY[[i]]

        spot.x <- spot.x - diffCenter.x
        spot.y <- spot.y - diffCenter.y

        spotsX[i] <- spot.x
        spotsY[i] <- spot.y
    }


    off <- (spotPitch * spotSize)/2

    x <- unlist(spotsX)
    y <- unlist(spotsY)
    r <- rep( off*2, length(x) )
    t <- rep( 0, length(x) )

    newGrid <- data.frame(x=x, y=y, radius=r, type=t, manual=1)
    session$sendCustomMessage("imageDisplay", newGrid)



    if(selection$image == gridSpotList$gridList[[gridSpotList$selectedGrid]]){
      # Update position for all grids
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridX"] = x
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridY"] = y
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "diameter"] = r
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "bad"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "empty"] = 0
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "manual"] = 1
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdXFixedPosition"] = x
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdYFixedPosition"] = y
      
      
    }else{
      # Update the image used for gridding
      df$data$grdImageNameUsed[df$data$Image == selection$image  ] = selection$image
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridX"] = x
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "gridY"] = y
      
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdXFixedPosition"] = x
      df$data$.y[df$data$grdImageNameUsed == selection$image & df$data$variable == "grdYFixedPosition"] = y
      
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


          outDf <- data.frame(
            .ci=df$data$.ci[ idxC  ],
            gridX=df$data$.y[ idxX  ],
            gridY=df$data$.y[ idxY  ],
            grdXFixedPosition=df$data$.y[ idxFX  ],
            grdYFixedPosition=df$data$.y[ idxFY  ],
            diameter=df$data$.y[ idxR  ],
            manual=df$data$.y[ idxM  ],
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

  }) #END observeEvent : input$saveBtn

  
  # Called after all widgets are loaded 
  observeEvent( input$pageLoaded, {
    shinyjs::enable("gridBtn")
    remove_modal_spinner()
  })
})


remove_variable_ns <- function(varName){
  fname <- str_split(varName, '[.]', Inf)
  fext <- fname[[1]][2]
  
  return(fext)
}


get_image_list <- function(df, imageUsed){
  req(df)
  

  
  #print(paste0(deparse(sys.calls()[[sys.nframe()-2]]), " - ", imageUsed))
  
  values <- df %>% select(c("Image", "grdImageNameUsed")) %>%
            filter(grdImageNameUsed == imageUsed) %>%
            pull(Image) %>% unique() %>% as.data.frame() 
  
  
  return(as.data.frame(rev(values[[1]])))
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


# get_image_used_list <- function( dfData ){
# 
#   values <- dfData %>% select("grdImageNameUsed") %>% unique() %>% as.list()
#   
#   return(values[[1]])
# }
# 

get_document_id <- function( session ){
  
  ctx <- getCtx(session)
  values <- ctx %>% cselect("documentId") %>% unique() %>% as.list()
  
  return(values[[1]][1])
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
  
  
  required.cnames = c("documentId","grdImageNameUsed","Image","spotRow","spotCol","ID")
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
  
  qtTable = dplyr::left_join(qtTable,cTable,by=".ci")
  
  rTable[[".ri"]] = seq(0, nrow(rTable)-1)
  
  

  qtTable = dplyr::left_join(qtTable,rTable,by=".ri")
  
  show_modal_spinner(spin="fading-circle", text = "Loading data (0%)")
  qtTable$variable = sapply(qtTable$variable, remove_variable_ns)
  
  progress$close()
  
  show_modal_spinner(spin="fading-circle", text = "Loading data (50%)")
  
  return(qtTable)
  
}

prep_image_folder <- function(session, docId){
  docId <- docId()
  progress <- Progress$new(session, min=1, max=1)
  
  
  progress$set(message="Downloading images")
  
  
  ctx <- getCtx(session)
  
  
  #1. extract files
  doc   <- ctx$client$fileService$get(docId )
  filename <- tempfile()
  writeBin(ctx$client$fileService$download(docId), filename)
  
  show_modal_spinner(spin="fading-circle", text = "Loading data (70%)")
  
  on.exit(unlink(filename, recursive = TRUE, force = TRUE))
  
  image_list <- vector(mode="list", length=length(grep(".zip", doc$name)) )
  
  # unzip archive (which presumably exists at this point)
  tmpdir <- tempfile()
  unzip(filename, exdir = tmpdir)
  show_modal_spinner(spin="fading-circle", text = "Loading data (90%)")
  
  imageResultsPath <- file.path(list.files(tmpdir, full.names = TRUE), "ImageResults")
  
  
  f.names <- list.files(imageResultsPath, full.names = TRUE)

  
  
  
  fdir <- str_split_fixed(f.names[1], "/", Inf)
  fdir <- fdir[length(fdir)]
  
  fname <- str_split(fdir, '[.]', Inf)
  fext <- fname[[1]][2]
  
  progress$close()
  
  # Images for all series will be here
  return(list(imageResultsPath, fext))
  
}



get_operator_props <- function(ctx, imagesFolder){
  sqcMinDiameter <- -1
  grdSpotPitch   <- -1
  grdSpotSize   <- -1
  
  operatorProps <- ctx$query$operatorSettings$operatorRef$propertyValues
  
  for( prop in operatorProps ){
    if (prop$name == "MinDiameter"){
      sqcMinDiameter <- as.numeric(prop$value)
    }
    
    if (prop$name == "SpotPitch"){
      grdSpotPitch <- as.numeric(prop$value)
    }
    
    if (prop$name == "SpotSize"){
      grdSpotSize <- as.numeric(prop$value)
    }
  }
  
  if( is.null(grdSpotPitch) || grdSpotPitch == -1 ){
    grdSpotPitch <- 21.5
  }
  
  if( is.null(grdSpotSize) || grdSpotSize == -1 ){
    grdSpotSize <- 0.66
  }
  
  if( is.null(sqcMinDiameter) || sqcMinDiameter == -1 ){
    sqcMinDiameter <- 0.45
  }
  
  props <- list()
  
  props$sqcMinDiameter <- sqcMinDiameter
  props$grdSpotPitch <- grdSpotPitch
  props$grdSpotSize <- grdSpotSize
  
  
  # Get array layout
  layoutDirParts <- str_split_fixed(imagesFolder, "/", Inf)
  nParts  <- length(layoutDirParts) -1 # Layout is in parent folder
  
  layoutDir = ''
  
  for( i in 1:nParts){
    layoutDir <- paste(layoutDir, layoutDirParts[i], sep = "/")
  }
  layoutDir <- paste(layoutDir, "*Layout*", sep = "/")
  
  props$arraylayoutfile <- Sys.glob(layoutDir)
  
  return (props)
}


