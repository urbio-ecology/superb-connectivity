source(here("app/packages.R"))
source(here("app/colours.R"))
source(here("app/ui.R"))
source(here("app/server.R"))

shinyApp(ui = ui, server = server)
