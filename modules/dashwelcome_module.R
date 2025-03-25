dashwelcomeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      # Font + Ripple + FontAwesome
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;500;700&display=swap"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/node-waves/0.7.6/waves.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/node-waves/0.7.6/waves.min.js"),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          Waves.init();
          Waves.attach('.waves-effect', ['waves-button']);
        });
      ")),
      tags$style(HTML("
        body {
          font-family: 'Poppins', sans-serif;
          background-color: #ffffff  !important;
        }

        .main-header {
          background-color: #ffffff  !important;
        }

        .hero {
          background: linear-gradient(to right, #871E2E, #a53d3d);
          color: white;
          padding: 60px 40px;
          border-radius: 0px;
          text-align: center;
          box-shadow: 0 6px 18px rgba(0,0,0,0.15);
          margin: 0px;
          width: 100%;
          position: relative;
        }

        .hero .logo-top-right {
          position: absolute;
          top: 20px;
          right: 30px;
          height: 60px;
        }

        .hero h1 {
          font-weight: 700;
          font-size: 36px;
          margin-bottom: 20px;
        }

        .hero p {
          font-size: 18px;
          margin-bottom: 30px;
        }

        .neon-button {
          font-size: 16px;
          padding: 12px 25px;
          border-radius: 30px;
          font-weight: 500;
          color: #ffffff;
          border: 2px solid white;
          position: relative;
          background: transparent;
          overflow: hidden;
          transition: 0.3s ease-in-out;
        }

        .neon-button::before {
          content: '';
          position: absolute;
          top: 0;
          left: -75%;
          width: 200%;
          height: 100%;
          background: linear-gradient(120deg, transparent, rgba(255,255,255,0.4), transparent);
          transform: skewX(-20deg);
        }

        .neon-button:hover {
          background: white;
          color: #871E2E !important;
        }

        .neon-button:hover::before {
          animation: shine 0.7s linear;
        }

        @keyframes shine {
          0% { left: -75%; }
          100% { left: 125%; }
        }

        .info-box {
          background-color: rgba(255,255,255,0.15);
          padding: 25px;
          border-radius: 18px;
          font-size: 16px;
          margin-top: 30px;
          margin-left: 20px;
          margin-right: 20px;
        }

        .info-box ul {
          list-style: none;
          padding-left: 0;
        }

        .info-box li {
          margin: 10px 0;
        }

        .custom-box {
          background: rgba(255,255,255,0.15);
          backdrop-filter: blur(12px);
          -webkit-backdrop-filter: blur(12px);
          border-radius: 18px;
          padding: 20px;
          box-shadow: 0 8px 24px rgba(0,0,0,0.1);
          margin-bottom: 30px;
          transition: transform 0.2s ease;
        }

        .custom-box:hover {
          transform: translateY(-3px);
        }

        .custom-box-title {
          font-size: 18px;
          font-weight: 600;
          color: #871E2E;
          margin-bottom: 15px;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }

        .custom-box-title .fa {
          margin-right: 10px;
        }

        .sidebar-menu > li.active > a {
          background-color: #871E2E !important;
          color: #ffffff !important;
          font-weight: bold;
        }
      "))
    ),
    
    # HERO AREA
    div(
      class = "hero waves-effect",
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/en/thumb/1/1d/2020_Summer_Olympics_logo_new.svg/160px-2020_Summer_Olympics_logo_new.svg.png",
        class = "logo-top-right"
      ),
      tags$h1(icon("volleyball-ball", class = "fa-bounce"), "Welcome to the Tokyo Olympics 2020 - Volleyball!"),
      tags$p("Explore detailed statistics and insights from the Volleyball tournament of the Tokyo Olympics 2020."),
      
      tags$a(
        href = "https://en.volleyballworld.com/volleyball/competitions/olympics-2020/",
        class = "btn neon-button waves-effect",
        icon("globe", class = "fa-spin"), " Official Website"
      ),
      
      div(
        class = "info-box",
        HTML("
          <ul>
            <li><i class='fas fa-chart-line fa-bounce'></i> <strong>Dashboard - Player</strong> and <strong>Dashboard - Team</strong>: Competition statistics by skill category.</li>
            <li><i class='fas fa-volleyball-ball fa-spin'></i> <strong>Stat Analysis - Player</strong> and <strong>Stat Analysis - Team</strong>: Interactive court visualizations.</li>
          </ul>
        ")
      )
    ),
    
    # ONDA SEPARATRICE
    tags$div(HTML('
      <div style="margin-top: -20px; overflow: hidden; line-height: 0;">
        <svg viewBox="0 0 500 40" preserveAspectRatio="none" style="height: 40px; width: 100%;">
          <path d="M0,0 C150,40 350,0 500,40 L500,00 L0,0 Z" style="stroke: none; fill: #f2e9e4;"></path>
        </svg>
      </div>
    ')),
    
    fluidRow(
      width = 6, # Set the width of the column
      offset = 3, # Offset the column to center it (12 - 6) / 2 = 3
      div(
        class = "custom-box waves-effect",
        tags$div(class = "custom-box-title",
                 tagList(icon("sitemap", class = "fa-bounce"), "Competition Formula"),
                 tags$span(style = "font-size: 12px; background: #871E2E; color: white; padding: 4px 10px; border-radius: 8px;", "Pool Phase")),
        img(src = "https://images.volleyballworld.com/image/private/t_editorial_landscape_12_desktop/f_png/fivb-prd/hpjvxrwqa7kiaiksnh4o.png",
             width = "100%",
             class = "waves-effect",
             style = "border-radius: 12px;")
      )
    ),
    
    fluidRow(
      width = 6, # Set the width of the column
      offset = 3, # Offset the column to center it (12 - 6) / 2 = 3
      div(
        class = "custom-box waves-effect",
        tags$div(
          class = "custom-box-title",
          tagList(icon("sitemap", class = "fa-bounce"), "Qualified Men's Teams"),
          tags$span(style = "font-size: 12px; background: #871E2E; color: white; padding: 4px 10px; border-radius: 8px;", "Pools A & B")
        ),
        img(
          src = "https://images.volleyballworld.com/image/private/t_editorial_landscape_12_desktop/f_png/fivb-prd/vi6gt0vls3t5lmzwif5b.png",
          width = "100%",
          class = "waves-effect",
          style = "border-radius: 12px;"
        )
      )
    )
  )
}
