login_screen <- function(req, title = "Adlabs BARB Spot Browser") {
  shiny::addResourcePath("img", system.file("img", package = "googleAuthR"))
  shiny::addResourcePath("css", system.file("css", package = "googleAuthR"))
  shiny::fillPage(
    padding = 50,
    title = title,
    shiny::tags$head(shiny::tags$link(rel = "stylesheet",
                                      href = "css/button.css")),
    
    shiny::img(
      src = "https://storage.googleapis.com/itv-logos/background_b_w.jpg",
      style = '
        left: 50%;
        position: absolute;
        transform: translate(-50%, 0);
        overflow: hidden;
        top: 0;
        z-index: -1;
          height: 100%;
    width: 100%;'
    ),
    
    shiny::a(
      href = gar_shiny_auth_url(req),
      shiny::tags$button(class = "loginBtn loginBtn--google",
                         "Login with Google")
    )
  )
}
