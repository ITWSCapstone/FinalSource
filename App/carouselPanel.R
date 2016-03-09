#adapted from: https://github.com/dcurrier/carouselPanel/blob/master/carouselPanel.R

carouselPanel <- function(..., auto.advance=FALSE){
  n = paste(strsplit(paste(strsplit(as.character(rnorm(1)), "[.]")[[1]], collapse=""), "-")[[1]], collapse="")
  contents = list(...)
  tagList(
    # Set up Javascript to call carousel when document is ready
    if( !auto.advance ){
      singleton(tags$head(tags$script("$(document).ready(function(){
                                        $('.carousel').carousel({
                                          interval: false
                                      }));
    });")))
      }else{
        singleton(tags$head(tags$script("$(document).ready(function(){
                                          $('.carousel').carousel({
                                            interval: 5000,
                                            pause: 'hover'
                                          })

                                        });")))
      },
    
    #Set up carousel
    div(id=paste0("carousel-", n), class="carousel slide", `data-interval`=tolower(as.character(auto.advance)),
        # Carousel Inner Div - contains the content to display
        div(class="carousel-inner",
            div(class="item active", contents[[1]], style="padding: 0px 70px;"),
            mapply(function(elm){
              list(div(class="item", elm, style="padding: 0px 70px;"))
            }, contents[2:length(contents)], SIMPLIFY=F, USE.NAMES=F)),
        
        # Carousel controls
        a(class="left carousel-control",
          `data-slide`="prev",
          href=paste0("#carousel-", n),
          style="background: transparent; color: #000",
          HTML(paste0("<i class='fa fa-chevron-left'></i>")) ),
        
        a(class="right carousel-control",
          `data-slide`="next",
          href=paste0("#carousel-", n),
          style="background: transparent; color: #000",
          HTML(paste0("<i class='fa fa-chevron-right'></i>")) ),
        
        # Generate the carousel indicators
        HTML("<ol class='carousel-indicators'>"),
        tag('li', list(class='active', `data-slide-to`=paste(0),
                       `data-target`=paste0("#carousel-", n))),
        mapply(function(i){
          list(tag('li', list(class='', `data-slide-to`=paste(i),
                              `data-target`=paste0("#carousel-", n))) )
        }, 1:(length(contents)-1), SIMPLIFY=F, USE.NAMES=F),
        HTML("</ol>")
    ),
    # source: https://gist.github.com/smbache/eeef4bf21b053da780eb
    tags$script(sprintf("$('#carousel-%s').bind('slide.bs.carousel', function (e) {
                        $(e.target).find('.shiny-bound-output').each(function(i) {
                        $('#' + this.id).trigger('hidden')
                        });
                        $(e.relatedTarget).find('.shiny-bound-output').each(function(i) {
                        $('#' + this.id).trigger('shown')
                        });
                        }); ", n))
    )
  }