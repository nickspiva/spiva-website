library(shiny)
library(bslib)
library(ggplot2)

ui <- page_sidebar(
  title = "My Shiny App",
  sidebar = sidebar(
    "Shiny is available on CRAN, so you can install it in the usual way from your R console: derp",
    code("install.packages(\"shiny\")")
  ),
  card(
    card_header("Introducing Shiny"),
    card_body(
      "Shiny is a package from Posit that makes it incredibly easy to build
     interactive web applications with R. For an introduction and live examples, 
     visit the Shiny homepage (https://shiny.posit.co).",
      height = "50%"
    ),
    card_image(
      file = "shiny-hex.svg",
      alt = "Shiny's hex sticker",
      href = "https://github.com/rstudio/shiny",
      height = "300px"
    ),
    card_footer("Shiny is a product of Posit.")
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

#sidebar with a card, card has a header, dscriptive text, and an image, and a footer.
