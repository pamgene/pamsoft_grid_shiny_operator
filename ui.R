library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)


js <- "

$(document).on('shiny:value', function(evt) {

  if(evt.name === 'selectedImage') {
    setTimeout(function(){
      //var imgDiv = document.getElementById('selectedImage');
      //imgDiv.style.visibility = 'hidden';


      grid = new Array();
      
      for (let i = 0; i < globalThis.x.length; i++) {
        var spot = new Object();
        spot.x = globalThis.x[i]
        spot.y = globalThis.y[i]
        spot.radius = globalThis.radius[i]
        grid.push(spot)
      }
      
      var startX = 0;
      var startY = 0;
      
      
      customMessageHandler(grid)
      

    }, 0);
  }
});
"




shinyUI(
  
  fluidPage(
    shinyjs::useShinyjs(),
    tags$script(HTML(js)),
    tags$head(HTML("<script type='text/javascript'>
                  var customMessageHandler = function(list){
                  var Arc = function(x, y, radius, radians) {
                      this.x = x;
                      this.y = y;
                      this.radius = radius;
                      this.radians = radians;
                      this.isDragging = false;
              
              
                      this.render = function(ctx) {
                          ctx.save();
              
                          ctx.beginPath();
                          ctx.arc(this.x, this.y, this.radius, 0, this.radians, false);
                          ctx.linewidth = 2;
                          ctx.strokeStyle = '#00FF00';
                          ctx.stroke();
              
                          ctx.restore();
                      }
                  }
              
                  var MouseTouchTracker = function(canvas, callback){
                      this.isDragging = false;
                      this.list = new Array();
              
                      function processEvent(evt) {
                          var rect = canvas.getBoundingClientRect();
                          var offsetTop = rect.top;
                          var offsetLeft = rect.left;
              
                          if (evt.touches) {
                              return {
                                  x: evt.touches[0].clientX - offsetLeft,
                                  y: evt.touches[0].clientY - offsetTop
                              }
                          } else {
                              return {
                                  x: evt.clientX - offsetLeft,
                                  y: evt.clientY - offsetTop
                              }
                          }
                      }
              
                      function onDown(evt) {
                          evt.preventDefault();
                          var coords = processEvent(evt);
                          callback('down', coords.x, coords.y);
                      }
              
                      function onUp(evt) {
                          evt.preventDefault();
                          callback('up');
                      }
              
                      function onMove(evt) {
                          evt.preventDefault();
                          var coords = processEvent(evt);
                          callback('move', coords.x, coords.y);
                      }
              
                      canvas.ontouchmove = onMove;
                      canvas.onmousemove = onMove;
              
                      canvas.ontouchstart = onDown;
                      canvas.onmousedown = onDown;
                      canvas.ontouchend = onUp;
                      canvas.onmouseup = onUp;
                  }
              
                  function isHit(shape, x, y) {
                      if (shape.constructor.name === 'Arc') {
                          var dx = shape.x - x;
                          var dy = shape.y - y;
                          if (dx * dx + dy * dy < shape.radius * shape.radius) {
                              return true
                          }
                      } else {
                          if (x > shape.x - shape.width * 0.5 && y > shape.y - shape.height * 0.5 && x < shape.x + shape.width - shape.width * 0.5 && y < shape.y + shape.height - shape.height * 0.5) {
                              return true;
                          }
                      }
              
                      return false;
                  }
              
                  var gridImage = document.getElementById('gridImage');
                  var canvas = document.getElementById('gridCanvas');
              
                  canvas.width = 697
                  canvas.height = 520
              
                  var ctx = canvas.getContext('2d');
              
                  globalThis.grid = list.map(spot => new Arc(spot.x, spot.y, spot.radius, Math.PI * 2));
                  globalThis.mtt = new MouseTouchTracker(canvas,
                      function(evtType, x, y) {
                          ctx.clearRect(0, 0, canvas.width, canvas.height);
              
                          switch(evtType) {
              
                              case 'down':
                                  startX = x;
                                  startY = y;
              
                                  found = globalThis.grid.find(element => isHit(element, x, y));
              
                                  if (found){
                                      found.isDragging = true;
                                  } else {
                                      this.isDragging = true;
                                  }
                                  break;
              
                              case 'up':

                                  globalThis.grid.forEach(el => el.isDragging = false);
                                  this.isDragging = false;

                                  Shiny.setInputValue('gridOverlay', globalThis.grid);
                                  break;
              
                              case 'move':
                                  var dx = x - startX;
                                  var dy = y - startY;
                                  startX = x;
                                  startY = y;
              

                                  found = globalThis.grid.find(element =>  element.isDragging);
              
                                  if (found){
              
                                      found.x += dx;
                                      found.y += dy;
                                  } else {
                                      if (this.isDragging){
                                          globalThis.grid.forEach(el => {
                                              el.x += dx;
                                              el.y += dy;
                                          })
                                          
                                      }
                                  }
                                  break;
                          }
              
                          ctx.drawImage(gridImage,0,0)
                          globalThis.grid.forEach(spot => spot.render(ctx));
              
                      }
                  );
              
                  
                  ctx.drawImage(gridImage,0,0)
                  globalThis.grid.forEach(spot => spot.render(ctx));
              }
              
              
              
              
              var startX = 0;
              var startY = 0;
              
              Shiny.addCustomMessageHandler('imageDisplay', function(gridList){
                //Set this before triggering the image loaded event
                globalThis.x      = gridList.x;
                globalThis.y      = gridList.y;
                globalThis.radius = gridList.radius;

              });
              

                </script>")),
    tags$script(HTML(
      "var currentRow = 0;
      
      Shiny.addCustomMessageHandler('select_row',function(rows) {
      var table = $('#images').find('table').DataTable();
      
      table.rows().deselect();
      table.row(rows-1).select();
      
      currentRow = rows-1;
      })"
      )),
    
    sidebarLayout(
      
      sidebarPanel(
                   fluidRow( column(12, uiOutput("imageusedpanel")),
                             column(12, uiOutput("imagepanel"))   ),
                   fluidRow(column(2, disabled(actionButton("prevGridBtn", label = "<< Grid"  )) ),
                            column(2, disabled(actionButton("prevImgBtn", label = "< Image"  )) ) ,
                            column(2, actionButton("nextImgBtn", label = "Image >"  ) )  ,
                            column(2, actionButton("nextGridBtn", label = "Grid >>"  ) )
                            ),
                   br(),
                   fluidRow( column(12, DT::dataTableOutput("images"))  ) 
                   ),
      mainPanel(
        
        # 
        fluidRow( column(8, tags$canvas(id="gridCanvas", style="background-color: white") ),
                        style = "height:620px; "),
        
        
        fluidRow( column(4, sliderInput("brightness", "Brightness",
                                        min = -0.5, max = 0.5, ticks=FALSE, 
                                        value = 0, step = 0.01) ),
                  
                  column(4, sliderInput("contrast", "Contrast",
                                        min = 0.2, max = 4,ticks=FALSE, 
                                        value = 1, step = 0.05))
                  ),
        fluidRow(  
                  column(2, 
                         tags$div(title="Save all grid position changes", 
                                  disabled(actionButton("runBtn", label = "Run", width="120px"  ))))
                  ),
        fluidRow( column(8, imageOutput(outputId = "selectedImage")   ,  
                         style = "height:5px; visibility:hidden") ),
        ) # END mainPanel

    )  # END sidebarLayout          
  ) # END fluidPage
) # END shinyUI
 
