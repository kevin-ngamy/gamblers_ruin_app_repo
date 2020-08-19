### Full code of gambler's ruin shiny app
## Created by Kevin Tongam Anggatama
## Technologies : R language, Rstudio, Shiny framework, HTML

## 5 Packages used in this app
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(shinyalert)

## Codes are separated in 2 big chunks, 1. User interface code (UI) : code for the web interface, and 2. Server (backend) code

### User Interface Code
ui <- fluidPage(
    
    # Application title
    titlePanel(strong("Gambler's Ruin Monte Carlo Simulation")),
    br(),
    
    # Code for sidebar layout
    sidebarPanel(align = "left",
                 actionBttn(
                     inputId = "intro",
                     label = "Introduction", 
                     style = "stretch",
                     color = "danger"
                 ),
                 dropdown( # adding dropdown menu for inputs list
                     tags$h3("Inputs"),
                     pickerInput("side",
                                 label = "Select Side", 
                                 choices = c("Head", "Tail"),
                                 selected = "Head"), 
                     numericInput("prob", 
                                  label = "Probability of Winning",
                                  min = 0, max = 1, value = 0.5, step = 0.01, width = "50%"),
                     numericInput("init_stake", 
                                  label = "Initial Money",
                                  min = 10, max = 500, 1, value = 30, width = "50%"),
                     numericInput("win_target", 
                                  label = "Winning Target",
                                  min = 20, max = 2000, 1, value = 60, width = "50%"),
                     sliderInput("simul", 
                                 label = "Number of Trials", 
                                 min = 10, max = 10000, value = 1000),
                     br(actionBttn("rep_sim",
                                   label = "Run or Repeat Simulation",
                                   style = "unite", 
                                   color = "danger",
                                   icon = icon("refresh"))),
                     
                     br(actionButton("dist",
                                     label = "Show Distribution",
                                     icon = icon("refresh"),
                                     class = "btn-danger",
                                     width = "40%")),
                     br(),
                     actionButton("chance",
                                  label = "Probability of Ruin", class = "btn-warning"),
                     shinycssloaders::withSpinner(textOutput("prob_result"),
                                                  size = getOption("spinner.size", 0.5),
                                                  type = getOption("spinner.type", 3),
                                                  color.background = getOption("spinner.color.background", 3),
                                                  color = getOption("spinner.color", "red")),
                     
                     useShinyalert(), # Shiny alert for popup message (credits : Dean Attali)
                     
                     style = "unite", icon = icon("sliders"),
                     status = "danger", color = "success", label = "Inputs", width = "400px", 
                     animate = animateOptions(
                         enter = animations$fading_entrances$fadeInLeftBig,
                         exit = animations$fading_exits$fadeOutRightBig
                     ))),
    
    
    # Main panel consist of 2 panel, one for rendering ploylt output of the simulation result for the money over the trials,
    # and the other on is to plot the money distribution plot
    mainPanel(
        
        tabsetPanel(
            tabPanel("Visualization", 
                     shinycssloaders::withSpinner(plotlyOutput("plot", ## Shiny withspinner adds loading animation when button is clicked (credits : Dean Attali)
                                                               width = "900px", 
                                                               height = "500px")),
                     tabPanel("Distribution Plot (re-click)", 
                              shinycssloaders::withSpinner(plotOutput("dist")))),
            tabPanel("About Project",
                     br(h4("The purpose of this portfolio project is to run interactive monte carlo simulation on gambler's ruin problem coin flips."),
                        br("This app is programmed in R language and Shiny framework."),
                        br("Published in 19 August 2020."),
                        br(),
                        p("Creator:", 
                          a("Kevin Tongam Anggatama", 
                            href = "https://www.linkedin.com/in/kevin-tongam-anggatama-001461134/", 
                            target = "_blank")),
                        br("I study economics and data science full time. Beside it, 
                    I do mathematics and music in my free time. This program is also to complement
                    my studies in stochastic process. Currently looking for data science
                       internship opportunity. Any offer is more than welcomed."),
                        br(),
                        p(h5(strong("All the codes and documentations for this app is provided in my Github:", 
                                    a("Source Code", 
                                      href = "https://github.com/kevin-ngamy/gamblers_ruin_app_repo.git", 
                                      target = "_blank")))),
                        br(),
                        p(h5(strong("Check my another shiny apps project:", 
                                    br(a("-","Interactive dataviz web apps", 
                                         href = "https://kevintongam.shinyapps.io/shinyapps_gapminder/", 
                                         target = "_blank")),
                                    br(a("-","K-means clustering interactive app", 
                                         href = "https://kevintongam.shinyapps.io/kmeans_project_shiny/", 
                                         target = "_blank"))))),
                        br(h5(strong("contact: kevintongam98@gmail.com")))) # tab for project description
                     
            )
            
        ))
    
)

### Code for the server backend (as the function of inputted values in UI and rendered outputs)
server <- function(input, output) {
    
    
    shinyalert(
        title = "Hello.",
        text = "This is a simulation of Gambler's ruin problem with monte carlo simulation.
    Please read the introduction first, and fill the inputs, thank you.",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "https://yisluth.files.wordpress.com/2010/10/2003-indonesia-500-rupiah-copy.jpg",
        animation = TRUE
    )
    
    judi <- function(k,n,p) { ## judi is the function to generate random outcome (+1 and -1) from the coin flips trials
        # k = initial stake, n = win target, p = win probability
        stake <- k
        while (stake > 0 & stake < n) {
            bet <- sample(c(-1,1), 1, prob = c(1-p, p))
            
            return(bet) # returning +1 or -1 at each trial
        }
    }
    
    sim <- eventReactive(input$rep_sim, { ## sim is the Monte carlo simulation using the inputted values, simulated for inputted number of simulations times
        
        replicate(input$simul, judi(input$init_stake, 
                                    input$win_target, 
                                    input$prob))
        
    })
    
    total_gain <- function(k, trials, monte){ ## Total gain is the function to generate sequences of accumulated money gain or lose (sum of initial money with returned sim value)
        
        step <- c()
        
        for (i in 1:trials) {
            step[i] = k + sum(monte[1:i])
            
            if (step[i] == 0 | step[i] == input$win_target) {
                break
            }
        }
        return(step[1:trials])
        
    }
    
    profit <- reactive({ ## generates value from total gain function 
        
        total_gain(input$init_stake, input$simul, sim())
        
    })
    
    time <- reactive({ ## generate series of sequence from 1 to inputted number of simulations
        
        seq(1, input$simul, 1)
        
    })
    
    accum_gain <- reactive({ ## accum_gain returns profit and time as data frame with column 1 = money, column 2 = number of trials
        
        gain_data <- data.frame(cbind(profit(), time()))
        names(gain_data)[1] <- "Money"
        names(gain_data)[2] <- "Trials"
        gain_data
        
    })
    
    viz <-  eventReactive(input$rep_sim, { ## Viz is the visualization output (line chart) from the accum_gain
        
        gain_plot <- ggplot(accum_gain(), aes(x = Trials, y = Money)) + 
            geom_line() + 
            geom_hline(yintercept = c(0, input$win_target),
                       linetype="dashed", 
                       color = c("red", "blue"), 
                       size = 0.5) +
            ylim(c(0, input$win_target)) + 
            xlim(c(1, input$simul)) + 
            xlab("Number of Trials") + ylab("Money ($)")
        gain_plot + geom_hline(yintercept = input$init_stake, linetype="dashed", 
                               color = "black", size = 0.5, alpha = 0.75)
        
    })
    
    output$plot <- renderPlotly({
        ggplotly(
            viz()
        )
    })
    
    distribution <- eventReactive(input$dist, {
        dist_plot <- ggplot(accum_gain(), aes(x = Money, y = Trials)) +
            geom_col(fill = "red",
                     size = 0.2) +
            xlab("Money") +
            ylab("Count")
        
        dist_plot
    })
    
    output$dist <- renderPlot({
        distribution()
    })
    
    
    gamble <- function(k,n,p) { ## gamble function returns value of 1 if the simulations hit 0 (ruin) and 1 if it hits winning target (n)
        stake <- k
        
        while (stake > 0 & stake < n) {
            bet <- sample(c(-1, 1), 1, prob = c(1 - p, p))
            stake <- stake + bet
            }
        if (stake == 0)
            return(1)
        else return(0)
        
    }
    
    sim_prob <- eventReactive(input$chance, {
        
        prob_result <- replicate(input$simul, gamble(input$init_stake, 
                                                     input$win_target, 
                                                     input$prob))
        prob_result <- mean(prob_result)
        
        
    })
    
    output$prob_result <- renderText( ## output of probability based on the monte carlo simulation
        
        paste("Your probability of ruin on average after", input$simul, "number of simulations is", round(sim_prob(), 4))
        
    )
    
    
    isolate({observeEvent(input$intro, {
        showModal(modalDialog(print(h2("This program demonstrates Gambler's ruin problem in coin flips with monte carlo simulation.",
                                       h5(p("Creator:", 
                                            a("Kevin Tongam Anggatama", 
                                              href = "https://www.linkedin.com/in/kevin-tongam-anggatama-001461134/", 
                                              target = "_blank"))),
                                       br(h5("You bet on coin flips and choose head side. Each time the head side of the coin appeared you earn 1$ (+1),
                                           otherwise tail appeared you lose $1 (-1).", "You start with initial stake (money) of k dollar, and you want your money to reach e dollar (e > k). 
                                           The game stops if you either reach e dollar (win), or lose all your money to 0 (ruin). 
                                           How much money you would have after certain number of trials (flips)? Will you eventually reach e dollar (win) or lose all your money to 0 (ruined)
                                           after certain trials?
                                           This is a classic probabiliy game names Gambler's run problem, first disscussed by Blaise Pascal and Pierre de Fermat.",
                                             br(h5("on each flip you will get either +1 or -1. Here you will simulate the game to see how your money change at each trial (flips), and see wether you win or ruin.
                                                 You can set your own winning probability (p), if p = 0.5 then probability of losing is q = 1 - p = 0.5 (fair game with Bernoulli distribution), 
                                                 otherwise unfair. You'll may notice that if the game is fair, your money will exhibit random walks (stochastic and unbiased) at each trial,
                                                 and if the game is unfair, it will exhibit an upward/downward drift (biased) based on the probability favor. Nevertheless the chart you
                                                   will see is the random moves of the remaining money after accumulated with the +1 or -1 result on each trial.",
                                                   br("Ruin Probability is the sum of all win or ruin results as 0 and 1, respectively, after number of monte carlo simulations divided by the number of simulations.
                                                      It will return the probability of ruin (this will take few seconds to run."))),
                                             br(h5("If the chart (money) hits the blue dashed line you win (and the game stops), if it hits the red dashed line (0) you lose
                                              (ruin) and the game stops. Now fill the inputs and simulate.")))))), 
                              title = "Introduction"))})
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

