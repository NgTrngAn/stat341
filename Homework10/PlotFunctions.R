# STAT 341 

#########################################################
# Computing Continuous Probabilities
#########################################################
library(ggplot2)

#########################################################
##Plots the p.d.f. for the Uniform distribution with
##parameters minimum = A and maximum = B

plot.unif.pdf <- function(A, B, val, fnx){
  y<- seq(A, B, length.out=1000)
  fy<- dunif(y, A, B)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Min = ", as.character(A), ",", 
               " Max = ", as.character(B), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    geom_segment(aes(x = A, y = 0, 
                     xend = A-0.1*(B-A), yend = 0), 
                 arrow = arrow(angle = 10))+
    geom_segment(aes(x = B, y = 0, 
                     xend = B+0.1*(B-A), yend = 0), 
                 arrow = arrow(angle = 10))+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Uniform Distribution p.d.f.",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dunif(val, A, B)), color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dunif(val, A, B)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qunif(val,A,B)), aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.unif.CDF <- function(A, B, fnx, val=0){
  y<- seq(A, B, length.out=1000)
  Fy<- punif(y, A, B)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Min = ", as.character(A), ",", 
               " Max = ", as.character(B), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    geom_segment(aes(x = A, y = 0, 
                     xend = A-0.1*(B-A), yend = 0), 
                 arrow = arrow(angle = 10))+
    geom_segment(aes(x = B, y = 1, 
                     xend = B+0.1*(B-A), yend = 1), 
                 arrow = arrow(angle = 10))+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Uniform Distribution C.D.F.",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = punif(val, A, B)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=punif(val, A, B)), color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qunif(val, A, B), Fy=val), color="red") +
      geom_segment(aes(x = A-0.1*(B-A), y = val, xend = qunif(val, A, B), yend = val), 
                   color="red", linetype="longdash")
  }
}

##########################################################
##Plots the p.d.f. for the Normal distribution with
##parameters mean = mu and standard deviation = sigma

plot.norm.pdf <- function(mu, sigma, fnx, val){
  y<- seq(mu - 4*sigma, mu+4*sigma, length.out=1000)
  fy<- dnorm(y, mu, sigma)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Mean = ", as.character(mu), ",", 
               " Std. Dev. = ", as.character(sigma), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust = 0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Normal Distribution p.d.f.",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dnorm(val, mu, sigma)), color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dnorm(val, mu, sigma)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qnorm(val, mu, sigma)), aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.norm.CDF <- function(mu, sigma, fnx, val){
  y<- seq(mu-4*sigma, mu+4*sigma, length.out=1000)
  Fy<- pnorm(y, mu, sigma)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Mean = ", as.character(mu), ",", 
               " Std. Dev. = ", as.character(sigma), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust = 0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Normal Distribution C.D.F.",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = pnorm(val, mu, sigma)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=pnorm(val, mu, sigma)), color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qnorm(val, mu, sigma), Fy=val), color="red") +
      geom_segment(aes(x = mu-4*sigma, y = val, xend = qnorm(val, mu, sigma), yend = val), 
                   color="red", linetype="longdash")
  }
}


#########################################################
##Plots the p.d.f. of the Gamma distribution with 
##parameters shape = alpha and scale = beta

plot.gamma.pdf <- function(alpha, beta, fnx, val){
  y<- seq(0, qgamma(0.9995, shape = alpha, scale = beta), length.out=1000)
  fy<- dgamma(y, shape = alpha, scale = beta)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Shape = ", as.character(alpha), ",", 
               " Scale = ", as.character(beta), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Gamma Distribution p.d.f",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dgamma(val, shape = alpha, scale = beta)), 
                    color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dgamma(val, shape = alpha, scale = beta)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qgamma(val, shape = alpha, scale = beta)), 
                   aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.gamma.CDF <- function(alpha, beta, fnx, val){
  y<- seq(0, qgamma(0.9995, shape = alpha, scale = beta), length.out=1000)
  Fy<- pgamma(y, shape = alpha, scale = beta)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Shape = ", as.character(alpha), ",", 
               " Scale = ", as.character(beta), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Gamma Distribution C.D.F",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = pgamma(val, shape = alpha, scale = beta)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=pgamma(val, shape = alpha, scale = beta)), 
                 color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qgamma(val, shape = alpha, scale = beta), Fy=val), 
                    color="red") +
      geom_segment(aes(x = 0, y = val, xend = qgamma(val, shape = alpha, scale = beta), yend = val), 
                   color="red", linetype="longdash")
  }
}

###########################################################
##Plots the p.d.f. of the Exponential distribution with
##parameter rate = lambda

plot.exp.pdf <- function(lambda, fnx, val){
  y<- seq(0, qexp(0.9995, lambda), length.out=1000)
  fy<- dexp(y, lambda)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Rate = ", as.character(lambda), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Exponential Distribution p.d.f",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dexp(val, lambda)), color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dexp(val, lambda)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qexp(val, lambda)), 
                   aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.exp.CDF <- function(lambda, fnx, val){
  y<- seq(0, qexp(0.9995, lambda), length.out=1000)
  Fy<- pexp(y, lambda)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Rate = ", as.character(lambda), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Exponential Distribution C.D.F",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = pexp(val, lambda)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=pexp(val, lambda)), color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qexp(val, lambda), Fy=val),  color="red") +
      geom_segment(aes(x = 0, y = val, xend = qexp(val, lambda), yend = val), 
                   color="red", linetype="longdash")
  }
}


###########################################################
##Plots the p.d.f. of the Chi-Squared distribution with
##parameter nu = degrees of freedom

plot.chisq.pdf <- function(nu, fnx, val){
  y<- seq(0, qchisq(0.9995, nu), length.out=1000)
  fy<- dchisq(y, nu)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Degrees of Freedom = ", as.character(nu), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Chi-Squared Distribution p.d.f",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dchisq(val, nu)), color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dchisq(val, nu)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qchisq(val, nu)), 
                   aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.chisq.CDF <- function(nu, fnx, val){
  y<- seq(0, qchisq(0.9995, nu), length.out=1000)
  Fy<- pchisq(y, nu)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Degrees of Freedom = ", as.character(nu), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Chi-Squared Distribution C.D.F",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = pchisq(val, nu)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=pchisq(val, nu)), color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qchisq(val, nu), Fy=val),  color="red") +
      geom_segment(aes(x = 0, y = val, xend = qchisq(val, nu), yend = val), 
                   color="red", linetype="longdash")
  }
}


###########################################################
##Plots the p.d.f. of the Beta distribution with
## shape parameters = alpha, beta

plot.beta.pdf <- function(alpha, beta, fnx, val){
  y<- seq(0, 1, length.out=1000)
  fy<- dbeta(y, alpha, beta)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Shape 1 = ", as.character(alpha), ",", 
               " Shape 2 = ", as.character(beta), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Beta Distribution p.d.f",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG + geom_point(data=data.frame(y=val, fy=dbeta(val, alpha, beta)), color="red") +
      geom_segment(aes(x = val, y = 0, xend = val, yend = dbeta(val, alpha, beta)), 
                   color="red", linetype="longdash")
  }else if(fnx == 2){#cdf
    GG + geom_area(data=subset(plotdata, y<=val), aes(y=fy), fill = "red", alpha = 0.5)
  }else if(fnx == 3){#quf
    GG + geom_area(data=subset(plotdata, y<=qbeta(val, alpha, beta)), 
                   aes(y=fy), fill = "red", alpha = 0.5)
  }
}

plot.beta.CDF <- function(alpha, beta, fnx, val){
  y<- seq(0, 1, length.out=1000)
  Fy<- pbeta(y, alpha, beta)
  plotdata<- data.frame(cbind(y, Fy))
  Dist<- paste("Shape 1 = ", as.character(alpha), ",", 
               " Shape 2 = ", as.character(beta), sep = "")
  GG <- ggplot(plotdata, aes(x = y, y = Fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y = Q(p)",
         y = "F(y) = p",
         title = "Beta Distribution C.D.F",
         subtitle = Dist)
  
  if(fnx == 1){#pmf
    GG 
  }else if(fnx == 2){#cdf
    GG + geom_segment(aes(x = val, y = 0, xend = val, yend = pbeta(val, alpha, beta)), 
                      color="red", linetype="longdash") +
      geom_point(data=data.frame(y=val, Fy=pbeta(val, alpha, beta)), color="red")
  }else if(fnx == 3){#quf
    GG + geom_point(data=data.frame(y=qbeta(val, alpha, beta), Fy=val),  color="red") +
      geom_segment(aes(x = 0, y = val, xend = qbeta(val, alpha, beta), yend = val), 
                   color="red", linetype="longdash")
  }
}

