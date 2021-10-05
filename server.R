library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

library(imager)
library(DT)
library(stringr)
library(tiff)


OP_DEBUG <- TRUE


options("tercen.workflowId"= "cff9a1469cd1de708b87bca99f003d42")
options("tercen.stepId"= "14ed29d4-9072-4305-baf7-433174aa6829")


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


get_data <- function( session ){
  progress <- Progress$new(session, min=1, max=1)
  
  
  progress$set(message="Loading Table Data")
  ctx <- getCtx(session)
  
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
  qtTable$variable = sapply(qtTable$variable, remove_variable_ns)
  
  progress$close()
  
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
  
  on.exit(unlink(filename, recursive = TRUE, force = TRUE))
  
  image_list <- vector(mode="list", length=length(grep(".zip", doc$name)) )
  
  # unzip archive (which presumably exists at this point)
  tmpdir <- tempfile()
  unzip(filename, exdir = tmpdir)
  
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



# ================================================
# SERVER FUNCTION
# ================================================
shinyServer(function(input, output, session) {
  gridImageList <- reactive( get_image_used_list(session) )
  imageChoiceList <- reactiveValues(data=NULL)
  imageSelection <- reactiveValues( imageIdx=1, gridIdx=1 )
  
  mode = reactive({getMode(session)})
  
  grid    <- reactiveValues(X=NULL, Y=NULL)
  df <- reactiveValues( data=NULL  )
  selection <- reactiveValues( image=NULL  )
  
  
  docId  <-reactive( get_document_id(session)  )
  imgInfo <- reactive(prep_image_folder(session, docId)  )
  
  selection <- reactiveValues(img=NULL)
  
  
  imageList <- reactive(get_image_list(session, gridImageList()[[imageSelection$gridIdx]] ))
  outfile <- '/tmp/grid.png' #tempfile(fileext = '.png')
  dtImageList <- reactive( imageChoiceList$data )
  # END OF SERVER VARIABLES DEFINITION
  # +++++++++
  
  
  
  if(OP_DEBUG == TRUE){
    output$opMode <- renderText( paste0("Mode is: ", mode() ) )
  }
  
  
  
  output$selectedImage <- renderImage({
    m = mode()

    
    if (!is.null(m) && m == 'run'){
      # Only available in 'run' mode
      shinyjs::enable("runBtn")
      shinyjs::enable("applyBtn")
    }
    
    
    if(is.null(df$data)){
        df$data <- get_data(session)
    }

    #clk <- Clicked()
    #if(is.null(clk)){
    #  clk <- 1
    #}
    
    print("Rendering image")
    
    
    selection$image <- imageList()[[1]][imageSelection$imageIdx]
    
    dfImg <- reactive(df$data %>% filter(Image == selection$image ) )

    
    grid$X <- reactive(dfImg() %>% filter(variable == "gridX") %>% pull(.y))
    grid$Y <- reactive(dfImg() %>% filter(variable == "gridY") %>% pull(.y))
    
    #selectedImage <- imageList()[[1]][clk]
    selectedImage <- paste0( imgInfo()[1], '/', selection$image, '.', imgInfo()[2] )
     
    # Generate the PNG
    bf <- as.double(input$brightness)
    ct <- as.double(input$contrast)
    img <- suppressWarnings( readTIFF(selectedImage) * 16 )
    img <- add.colour(as.cimg(aperm(img, c(2,1))))
  
    
    img <- img / max(img)
    img <- img + bf
    
    img <- ct * (img - 0.5) + 0.5
    img <- overlay_grid(img, grid$X(), grid$Y())
    

    img[img > 1] = 1
    img[img < 0]  = 0

    imager::save.image(img,outfile)
    
  #  # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         alt = "Grid image could not be loaded.")
  }, deleteFile = TRUE)
  
  

  
  observeEvent(input$mouseup, {
    if(input$mouseup == 1){
      if(OP_DEBUG == TRUE){
        output$mouseup <- renderText(
          paste0('Dragged mouse from (',
                 input$xDwn, ',', input$yDwn, 
                 ') to (', input$x, ',', input$y, ')' )
        )
      }

      #UPDATE Grid position
      gX <- grid$X()
      gY <- grid$Y()
      
      dY <- input$x - input$xDwn
      dX <- input$y - input$yDwn
      
      selectedSpot <- -1
      
      for(i in seq_along(gX)){
        center_x <- gX[i]
        center_y <- gY[i]
        
        if( is_in_circle(center_x, center_y, input$yDwn, input$xDwn, 4)){
          selectedSpot <- i
          
        }
      }

      if( dX != 0 || dY != 0){
        if(selectedSpot > -1 ){
          gY[selectedSpot] <- gY[selectedSpot] + dY
          gX[selectedSpot] <- gX[selectedSpot] + dX
        }else{
          gY <- gY + dY
          gX <- gX + dX
        }
        
        # Update df
        d <- isolate(df$data)

        d$.y[d$variable == "gridX" & d$Image == selection$image] <- gX
        d$.y[d$variable == "gridY" & d$Image == selection$image] <- gY
  
        df$data <- d
      }
    }
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Image and Grid button events (previous and next)
  observeEvent(input$nextGridBtn, {
    imageSelection$gridIdx <- imageSelection$gridIdx + 1
    
    if(imageSelection$gridIdx == length( gridImageList()) ){
      shinyjs::disable( "nextGridBtn"  )
    }else{
      shinyjs::enable( "nextGridBtn"  )
    }
    

    shiny::updateSelectInput(session = session, 
                             inputId = "imagedused", 
                             selected=gridImageList()[imageSelection$gridIdx], 
                             choices=gridImageList() )
    
    imageChoiceList$data <- get_image_list(session, gridImageList()[ imageSelection$gridIdx])
    dtImageList <- reactive( imageChoiceList$data )
  })
  
  
  observeEvent(input$prevGridBtn, {
    imageSelection$gridIdx <- imageSelection$gridIdx - 1
    
    if(imageSelection$gridIdx == 1 ){
      shinyjs::disable( "prevGridBtn"  )
    }else{
      shinyjs::enable( "prevGridBtn"  )
    }
    
    shiny::updateSelectInput(session = session, 
                             inputId = "imagedused", 
                             selected=gridImageList()[imageSelection$gridIdx], 
                             choices=gridImageList() )
    
    imageChoiceList$data <- get_image_list(session, gridImageList()[ imageSelection$gridIdx])
    dtImageList <- reactive( imageChoiceList$data )
  })
  
  
  observeEvent(input$nextImgBtn, {
    imageSelection$imageIdx <- imageSelection$imageIdx+1
    
    # From last image of a grid, jump to the first image of the subsequent grid
    if( imageSelection$imageIdx > length(imageList()[[1]]) && imageSelection$gridIdx < length( gridImageList())){
      imageSelection$imageIdx <- 1
      imageSelection$gridIdx <- imageSelection$gridIdx + 1
      
      if(imageSelection$gridIdx == length( gridImageList()) ){
        shinyjs::disable( "nextGridBtn"  )
      }else{
        shinyjs::enable( "nextGridBtn"  )
      }
      
      shiny::updateSelectInput(session = session, 
                               inputId = "imagedused", 
                               selected=gridImageList()[imageSelection$gridIdx], 
                               choices=gridImageList() )
      
      imageChoiceList$data <- get_image_list(session, gridImageList()[ imageSelection$gridIdx])
      dtImageList <- reactive( imageChoiceList$data )
    }
    
    
    # Test if previous/next buttons need to be disabled
    if(imageSelection$imageIdx > 1 || imageSelection$gridIdx > 1){
      shinyjs::enable( "prevImgBtn"  )
    }
    
    if( imageSelection$gridIdx > 1 ){
      shinyjs::enable( "prevGridBtn"  )
    }
    
    
    if(imageSelection$imageIdx < nrow(imageChoiceList$data) && imageSelection$gridIdx < length(gridImageList())){
      shinyjs::enable( "nextImgBtn"  )
    }else{
      shinyjs::disable( "nextImgBtn"  )
    }
  } ) # END OF nextImgBtn event
  
  
  observeEvent(input$prevImgBtn, {
    imageSelection$imageIdx <- imageSelection$imageIdx-1
    
    # From first image of a grid, jump to the last image of the previous grid
    if( imageSelection$imageIdx < 1 && imageSelection$gridIdx > 1 ){
      
      imageSelection$gridIdx <- imageSelection$gridIdx -1
      imageChoiceList$data   <- get_image_list(session, gridImageList()[ imageSelection$gridIdx])
      imageSelection$imageIdx <- nrow(imageChoiceList$data)
      
      if(imageSelection$gridIdx == 1 ){
        shinyjs::disable( "prevGridBtn"  )
      }else{
        shinyjs::enable( "prevGridBtn"  )
      }
      
      shiny::updateSelectInput(session = session, 
                               inputId = "imagedused", 
                               selected=gridImageList()[imageSelection$gridIdx], 
                               choices=gridImageList() )
      
      dtImageList <- reactive( imageChoiceList$data )
    }
    
    
    # Test if previous/next buttons need to be disabled
    if(imageSelection$imageIdx == 1 && imageSelection$gridIdx == 1){
      shinyjs::disable( "prevGridBtn"  )
      shinyjs::disable( "prevImgBtn"  )
    }else{
      shinyjs::enable( "prevImgBtn"  )
    }
  } ) # END OF prevImgBtn event
  
  # END of Image and Grid button events
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  observeEvent(input$imagedused,{ 
    imageSelection$gridIdx <- which(gridImageList()== input$imagedused) 
    
    imageChoiceList$data <- get_image_list(session, gridImageList()[ imageSelection$gridIdx] )
    
    if( imageSelection$gridIdx == 1){
      shinyjs::disable( "prevGridBtn"  )
      if(imageSelection$imageIdx == 1){
        shinyjs::disable( "prevImgBtn"  )
      }else{
        shinyjs::enable( "prevImgBtn"  )
      }

    }else{
      shinyjs::enable( "prevGridBtn"  )
      shinyjs::enable( "prevImgBtn"  )
    }
    
    if( imageSelection$gridIdx == length(gridImageList()) ){
      shinyjs::disable( "nextGridBtn"  )
      
      if(imageSelection$imageIdx == nrow(imageChoiceList$data) ){
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
      imageSelection$imageIdx <- as.numeric(input$selectedImageRow)
    }
    
    if(as.numeric(input$selectedImageRow) == nrow(imageChoiceList$data) &&
       imageSelection$gridIdx == length(gridImageList())){
      shinyjs::disable( "nextImgBtn"  )
    }else{
      shinyjs::enable( "nextImgBtn"  )
    }
    
    
    if( imageSelection$gridIdx == 1){
      
      shinyjs::disable( "prevGridBtn"  )
      shinyjs::enable( "nextImgBtn"  )
      
      if(as.numeric(input$selectedImageRow) == 1){
        shinyjs::disable( "prevImgBtn"  )
      }else{
        shinyjs::enable( "prevImgBtn"  )
      }
    }else{
      shinyjs::enable( "prevGridBtn"  )
    }
    
    
    
  } )
  
  
  output$imageusedpanel<-renderUI({
    selectInput("imagedused", "Grid Image", choices=gridImageList(), 
                selected=1, selectize = FALSE, multiple = FALSE)
  })
  

  output$currentGrid <- renderText(paste0( "CURRENT GRID: ", input$imagedused  )  )
  
  
  
  
  output$images <-
    renderDataTable( {
      
      DT::datatable( data=dtImageList(), 
                 selection=list(mode="single", selected=imageSelection$imageIdx),
                 colnames="", filter="none", style="bootstrap4",
                 options = list(pageLength=15, pageLengthLsit=c(5,15,30)),
                 callback=JS("table.on('click.dt', 'tr', function(e, dt, type, indexes) {
                              var row = $(this).children('td').html();
                              
                              Shiny.setInputValue('selectedImageRow', row);
                          });")
                 ) 
    })
  

  
  #Clicked <- eventReactive(imageSelection$imageIdx,{
    #input$images_rows_selected
    
    #imageSelection$imageIdx <- input$images_rows_selected 
    
    # MIGHT BE UNUSED
    # selection$img <- input$images_rows_selected 
  #})
  
  
  observeEvent( selection$img, {
    if(imageList()[[1]][selection$img] == input$imagedused && !is.null(mode()) && mode() == "run"){
      enable("applyBtn")
    }else{
      disable("applyBtn")
    }

  } )
  
  #observeEvent( input$imagedused, {disable("applyBtn")} )
  

  observeEvent( input$applyBtn, {
    print("TBD")
    } )
  
  
  observeEvent( input$runBtn, {
    

      progress <- Progress$new(session, min=1, max=1)
      progress$set(message="Running ... please wait ...")
      
      shinyjs::disable("applyBtn")
      shinyjs::disable("runBtn")
      
      
      tryCatch({
          ctx <- getCtx(session)
          
          df$data %>%
          ctx$addNamespace() %>%
          ctx$save()
          progress$close()  
      }, error = function(e) {
        progress$set(message=paste0("Failed : ", toString(e)))
        print(paste0("Failed : ", toString(e)))
      })
      
  }) #END observeEvent : input$saveBtn
})


is_in_circle <- function( center_x, center_y, x, y, r){
  d <- sqrt((center_x - x)**2 + (center_y - y)**2)
  return(d<r)
}


remove_variable_ns <- function(varName){
  fname <- str_split(varName, '[.]', Inf)
  fext <- fname[[1]][2]
  
  return(fext)
}


overlay_grid <- function(img, gridX, gridY){

  c1 <- imager::draw_circle(img*0, gridY, gridX, 8, filled = TRUE, color="green") 
  c2 <- imager::draw_circle(img*0, gridY, gridX, 6, filled = TRUE, color="green") 
  
  circ <- c1-c2
  
  img[circ>0] <- circ[circ>0]
  
  return(img)
}

get_image_list <- function(session, imageUsed){
  ctx <- getCtx(session)
  
  if(!is.null(imageUsed))
  {
    values <- ctx %>% cselect(ds0.Image, ds1.grdImageNameUsed) %>%
    filter(ds1.grdImageNameUsed == imageUsed) %>%
    pull(ds0.Image) %>% unique() %>% as.data.frame()
  }else{
    v0    <- ctx %>% cselect(ds1.grdImageNameUsed) %>% unique() %>% as.list()
    values <- ctx %>% cselect(ds0.Image, ds1.grdImageNameUsed) %>%
      filter(ds1.grdImageNameUsed == v0[[1]]) %>%
      pull(ds0.Image) %>% unique() %>% as.data.frame()
  }
  
  return(values)
}


get_image_used_list <- function(session ){
  ctx <- getCtx(session)
  
  values <- ctx %>% cselect(ds1.grdImageNameUsed) %>% unique() %>% as.list()
  
  return(values[[1]])
}


get_document_id <- function(session ){
  ctx <- getCtx(session)
  
  
  values <- ctx %>% cselect(documentId) %>% unique() %>% as.list()
  
  return(values[[1]][1])
}

getMode = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}
