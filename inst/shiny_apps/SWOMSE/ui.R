

# -- header ----
header <- shinydashboardPlus::dashboardHeader(
  title='SWOMSE',
  leftUi = tagList(
    dropdownButton(
      width=800,
      label = "Operating Models",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      tableOutput("OMs")
    ),
    dropdownButton(
      width=800,
      label = "Candidate Management Procedures",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      tableOutput("MPs")
    ),
    dropdownButton(
      width=800,
      label = "Tuning Codes",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      tableOutput("tunings")
    ),
    dropdownButton(
      width=800,
      label = "Performance Metrics",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      tableOutput("PMs")
    ),
    dropdownButton(
      width=800,
      label = "Links",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      uiOutput("links")
    )

    ),
  controlbarIcon=shiny::icon('gears')
)


# -- rhs controlbar ----
controlbar <- dashboardControlbar(overlay = FALSE, width=450,skin='light', collapsed = TRUE,
                                  FiltersUI('filters')

)

# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(id='sidebar',
    menuItem("CMP Performance", tabName = "CMPPerf", icon = icon("stats",lib="glyphicon")),
    menuItem("CMP Compare", tabName = "CMPCompare", icon = icon("transfer",lib="glyphicon")),
    menuItem("Trade-Off", tabName = "TradeOff", icon = icon("xmark")),
    menuItem("Kobe Time", tabName = "KobeTime", icon = icon("chart-line")),
    menuItem("Violin Plot", tabName = "ViolinPlot", icon = icon("bar-chart")),
    menuItem("Quilt Plot", tabName = "QuiltPlot", icon = icon("table")),
    # menuItem("Additional Analyses", tabName = "add_analyses", icon = icon("plus")) # ,
    menuItem("CMP Project", tabName = "cmp_project", icon = icon("arrow-right"))
  )
)


# -- body ----
body <- dashboardBody(height = 800,
  tags$head(
    includeScript(path = "www/js/js4checkbox.js"),
    includeScript(path = "www/js/index.js"),
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),
    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$link(rel="shortcut icon", href="favicon.ico"),

    tags$style(HTML("#SessionID{font-size:12px;}")),
    tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
        /* Font of SLICK title */
      ")),
    tags$script(
      'var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    '),
    tags$script("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      "),
    tags$style(HTML(
      ".checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
    ))

  ),
  tabItems(
    tabItem(tabName = "CMPPerf",
            CMPPerf_UI('CMPPerf')
    ),
    tabItem(tabName = "CMPCompare",
            CMPCompare_UI('CMPCompare')
    ),
    tabItem(tabName = "TradeOff",
            TradeOff_UI('TradeOff')
    ),
    tabItem(tabName = "KobeTime",
            KobeTime_UI('KobeTime')
    ),
    tabItem(tabName = "ViolinPlot",
            Violin_UI('Violin')
    ),
    tabItem(tabName = "QuiltPlot",
            QuiltPlot_UI('QuiltPlot')
    ),
    # tabItem(tabName = "add_analyses",
    #         add_analyses_UI('add_analyses')
    # ),
    tabItem(tabName='cmp_project',
            cmp_project_UI('cmp_project_1'))
  )
)


# -- page ----

dashboardPage(
  skin = "blue-light",
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=controlbar,
  title='SWOMSE',
  dashboardFooter(left =  paste0("SWOMSE version:", packageVersion('SWOMSE')),
                  right = h6("Copyright", HTML("&#169;"), tags$a(href='https://bluematterscience.com/',
                                                                 target="_blank", paste("Blue Matter Science Ltd.", format(Sys.Date(), "%Y")))))
)


