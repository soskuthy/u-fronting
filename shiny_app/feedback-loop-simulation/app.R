#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ggplot2)

### simulation functions

# function for category update (+ discard if outside boundaries)
update.1d.proto <- function (coord, m, sdev, bounds, c) {
  #if (coord < bounds[1] || coord > bounds[2]) {return(category)}
  if (coord < bounds[1]) {
    coord <- bounds[1]
  } else if (coord > bounds[2]) {
    coord <- bounds[2]
  }
  new.m <- (c * m + coord) / (c + 1)
  #new.sdev <- sqrt((c * ((new.m - m)**2 + sdev**2) + (coord - new.m)**2) / (c + 1))
  new.sdev <- sqrt((new.m - m)**2 + (((c-1)*sdev**2 + (coord - new.m)**2) / c))
  return(c(new.m, new.sdev))
}

# function for applying bias
logistic.bias.1d.proto <- function (coord, b, b.s, scale=1) {
  return(coord + b.s * ((1 / (1 + exp((coord - b) / scale))) - 0.5))
}

misperception.1d.proto <- function (coord, c1.m, c1.sd, c2.m, c2.sd, functional.load) {
  p1 <- dnorm(coord, c1.m, c1.sd)
  p2 <- p1 + dnorm(coord, c2.m, c2.sd)
  misp.prob <- functional.load*(1-(p1 / p2))
  return(sample(c(F,T), 1, prob=c(1-misp.prob, misp.prob)))
}


main.loop <- function (from, to, canvas,
                       cc, noise.prod,
                       u.mean, u.sd,
                       comp.mean, comp.sd, functional.load,
                       bias, bias.strength, bias.scale, bias.freq
                       ) {
  u.m <- u.mean
  u.s <- u.sd
  for (i in from:to) {
    target <- logistic.bias.1d.proto(rnorm(1, u.m, u.s),
                                     bias, 
                                     sample(c(bias.strength,0), size=1, prob=c(bias.freq, 1-bias.freq)),
                                     scale=bias.scale)
    if (!misperception.1d.proto(target, u.m, u.s, comp.mean, comp.sd, functional.load)) {
      target <- target + rnorm(1, 0, noise.prod)
      new.pars <- update.1d.proto(target, u.m, u.s, canvas, cc)
      u.m <- new.pars[1]
      #u.s <- new.pars[2]
    }
  }
  return(c(u.m, u.s))
}

create.single.hist.plot <- function (canvas, u.mean, u.sd, comp.mean, comp.sd, bias) {
  xs.1 <- seq(u.mean-u.sd*4, u.mean+u.sd*4 , length.out=500)
  xs.2 <- seq(comp.mean-comp.sd*4, comp.mean+comp.sd*4 , length.out=500)
  ys.1 <- dnorm(xs.1, u.mean, u.sd)
  ys.1 <- ys.1*(0.8/max(ys.1))
  ys.2 <- dnorm(xs.2, comp.mean, comp.sd)
  ys.2 <- ys.2*(0.8/max(ys.2))
  plot(c(), c(), type="n", xlim=canvas, ylim=c(0,1),
       xlab="F2",ylab="",
       cex.lab=1.5, font.lab=2,
       cex.axis=1.2, yaxt="n")
  mtext("Density", 2, line=1, cex=1.5, font=2)
  lines(xs.2, ys.2, lwd=3, col="salmon")
  lines(xs.1, ys.1, lwd=3, col="deepskyblue4")
  abline(v=bias, lwd=2, lty=2, col="grey")
}

create.multi.hist.plot <- function (canvas, u.means.multi, comp.mean, comp.sd, bias) {
  xs.2 <- seq(comp.mean-comp.sd*4, comp.mean+comp.sd*4 , length.out=500)
  d <- density(unlist(lapply(u.means.multi, function (x) x[length(x)])), bw=0.05)
  d$y <- d$y*(0.8/max(d$y))
  ys.2 <- dnorm(xs.2, comp.mean, comp.sd)
  ys.2 <- ys.2*(0.8/max(ys.2))
  plot(c(), c(), type="n", xlim=canvas, ylim=c(0,1),
       xlab="F2",ylab="",
       cex.lab=1.5, font.lab=2,
       cex.axis=1.2, yaxt="n")
  mtext("Density", 2, line=1, cex=1.5, font=2)
  lines(xs.2, ys.2, lwd=3, col="salmon")
  lines(d, lwd=3, col="deepskyblue4")
  abline(v=bias, lwd=2, lty=2, col="grey")
}

create.single.traj.plot <- function (canvas, u.means, comp.mean, bias) {
  xs <- seq(0, temp.iter, vis.freq)
  plot(c(), c(), type="n", xlim=c(0,iterations), ylim=canvas,
       xlab="Iterations", ylab="F2",
       cex.lab=1.5, font.lab=2,
       cex.axis=1.2)
  lines(xs, u.means, lwd=3, col="deepskyblue4")
  abline(h=bias, lwd=2, lty=2, col="grey")
  abline(h=comp.mean, lwd=2, lty=2, col="salmon")
}

create.multi.traj.plot <- function (canvas, u.means.multi, comp.mean, bias) {
  xs <- seq(0, iterations, vis.freq)
  plot(c(), c(), type="n", xlim=c(0,iterations), ylim=canvas,
       xlab="Iterations", ylab="F2",
       cex.lab=1.5, font.lab=2,
       cex.axis=1.2)
  for (i in 1:length(u.means.multi)) {
    if (i==length(u.means.multi)) {
      lines(xs, u.means.multi[[i]], lwd=3, col="deepskyblue4")
    } else {
      lines(xs, u.means.multi[[i]], lwd=3, col=rgb(0, 104, 139, 70, maxColorValue=255))
    }
  }
  abline(h=bias, lwd=2, lty=2, col="grey")
  abline(h=comp.mean, lwd=2, lty=2, col="salmon")
}

seq.special <- function (from, to, by) {
  if ((to - from) %% by == 0) {
    return(seq(from, to, by))
  } else {
    return(c(seq(from, to, by), to))
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; width: 60% } .form-group { display: table-row;}")
  ),
  
  # Application title
  titlePanel("Simulating /u/-fronting"),
  
  # Sidebar with various controls
  sidebarLayout(
    sidebarPanel(
      h4("Model parameters"),
      numericInput(inputId="c", label="Category weight", value=100),
      numericInput(inputId="noise.prod", label="Production noise", value=0.005),
      br(),
      h4("/u/ parameters"),
      numericInput(inputId="bias.freq", label="Coronal proportion", value=0.5),
      numericInput(inputId="bias.strength", label="Degree of coarticulation", value=0.001),
      br(), 
      h4("Competitor parameters"),
      br(), 
      numericInput(inputId="comp.m", label="Competitor position", value=0.85),
      numericInput(inputId="func.load", label="Functional load", value=0.1),
      br(), 
      h4("Simulation parameters"),
      br(), 
      numericInput("simulations", "Number of simulations", 
                   value=1,min=1,step=1),
      numericInput("iterations", "Number of iterations", 
                   value=100000,min=1,step=1),
      numericInput("vis.steps", "Visualisation steps",
                   value=100,min=1,step=1),
      actionButton("run", "Run")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("seriesPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #outputData <- reactiveValues(data = list())
  
  trigger <- reactiveValues(go=F, plot=0)
  
  output$histPlot <- renderPlot({
    if (trigger$plot > 0) {
      isolate({
        if (input$simulations==1) {
          create.single.hist.plot(canvas.plot, u.mean, u.sd, comp.mean, comp.sd, bias)
        } else {
          create.multi.hist.plot(canvas.plot, u.means.multi, comp.mean, comp.sd, bias)
        }
      })
    }
  })
  
  output$seriesPlot <- renderPlot({
    if (trigger$plot > 0) {
      isolate({
        if (input$simulations==1) {
          create.single.traj.plot(canvas.plot, u.means, comp.mean, bias)
        } else {
          create.multi.traj.plot(canvas.plot, u.means.multi, comp.mean, bias)
        }
      })
    }
  })

  observeEvent(input$run, {
    
    trigger$plot <- 0

    # initialise parameters
    
    u.sd <<- u.sd
    u.mean <<- u.start.m
    comp.mean <<- input$comp.m
    bias.strength <<- input$bias.strength
    bias.freq <<- input$bias.freq
    noise.prod <<- input$noise.prod
    functional.load <<- input$func.load
    cc <<- input$c
    iterations <<- input$iterations
    simulations <<- input$simulations
    vis.steps <<- input$vis.steps
    
    to.save <<- input$to.save
    file.name <<- input$file.name
    
    # calculate visualisation frequency
    vis.freq <<- iterations %/% vis.steps
    vis.max <<- vis.steps * vis.freq
    
    u.means <<- u.mean
    u.means.multi <<- list()
    temp.iter <<- 0
    sim.counter <<- 1
    
    
    trigger$go <- TRUE
  })
  
  main.single.observe <- observe({
    if (!trigger$go) {
      return(NULL)
    }
    if (isolate(input$simulations==1)) {
      if (temp.iter < iterations) {
        from <- temp.iter + 1
        to <- min(temp.iter+vis.freq, iterations)
        out <- main.loop(from=from, to=to, canvas=canvas,
                         cc=cc, noise.prod=noise.prod,
                         u.mean=u.mean, u.sd=u.sd,
                         comp.mean=comp.mean, comp.sd=comp.sd, functional.load=functional.load,
                         bias=bias, bias.strength=bias.strength, bias.scale=bias.scale, bias.freq=bias.freq
        )
        u.mean <<- out[1]
        u.sd <<- out[2]
        temp.iter <<- to
        u.means <<- c(u.means, u.mean)
        isolate(trigger$plot <- trigger$plot+1)
        invalidateLater(50)
      } else {
        trigger$go <- F
      }
    }
  })
  

  main.multiple.observe <- observe({
    if (!trigger$go) {
      return(NULL)
    }
    if (isolate(input$simulations) > 1) {
      if (sim.counter <= simulations) {
        isolate(u.mean <<- u.start.m)
        u.sd <<- u.sd
        u.means <- u.mean
        for (i in seq(0,iterations-vis.freq,vis.freq)) {
          from <- i + 1
          to <- min(i+vis.freq, iterations)
          out <- main.loop(from=from, to=to, canvas=canvas,
                           cc=cc, noise.prod=noise.prod,
                           u.mean=u.mean, u.sd=u.sd,
                           comp.mean=comp.mean, comp.sd=comp.sd, functional.load=functional.load,
                           bias=bias, bias.strength=bias.strength, bias.scale=bias.scale, bias.freq=bias.freq
          )
          u.mean <<- out[1]
          u.sd <<- out[2]
          u.means <- c(u.means, out[1])
        }
        u.means.multi[[length(u.means.multi)+1]] <<- u.means
        sim.counter <<- sim.counter + 1
        isolate(trigger$plot <- trigger$plot+1)
        invalidateLater(50)
      } else {
        trigger$go <- F
      }
    }
    #})
    
  })

}
# fixed parameters
bias <- 0.7
bias.scale <- 0.1
canvas <- c(-0.1,1.1)
canvas.plot <- c(0,1)
u.start.m <- 0.15
u.sd <- 0.07
comp.sd <- 0.07

# Run the application 
shinyApp(ui = ui, server = server)
