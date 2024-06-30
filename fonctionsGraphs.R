library(dplyr)
library(ggplot2)
library(glmtoolbox)
library(gridExtra)

modele_lineaire <- function(x, p) {
  p[1]*x + p[2]  # Équation 1 : P(t) = at + b
}

modele_quadratique <- function(x, p) {
  p[1]*x^2 + p[2]*x + p[3]  # Équation 2 : P(t) = at^2 + bt + c
}

modele_moore <- function(x, p) {
  (p[1]*(1 - exp(-p[2]*x))) + (p[3]*(1 - exp(p[4]*x)))  # Équation 3 : P(t) = a(1 − e^(-bt)) + c(1 − e^(dt))
}

# graph_lineaire ----
graph_lineaire <- function(dataset, x.axis, y.axis, x.label, epreuve, printTable=TRUE, ncol=2, point.size=3.6, point.col="#E683F6", line.size=1.5, line.col="blue") {
  # convertir epreuve en vecteur si ce n'est pas déjà le cas
  if (!is.vector(epreuve)) {
    epreuve <- as.vector(epreuve)
  }
  
  
  # créer une liste pour stocker les graphiques individuels
  plots <- list()
  
  # boucle sur chaque épreuve
  for (ep in epreuve) {
    # filtre pour garder uniquement les données de l'épreuve spécifiée
    data <- dataset %>%
      filter(epreuve == ep)
    
    # afficher la table filtrée dans la console
    if (printTable == T) {
      print(data)
    }
    
    # calcul des coefficients de régression linéaire
    lm_model <- lm(data[[y.axis]] ~ data[[x.axis]])
    
    # création du graphique pour cette épreuve
    plot <- ggplot(data) +
      aes_string(x = x.axis, y = y.axis) +
      geom_smooth(method = "lm", se = FALSE, color = line.col, size = line.size) +
      geom_point(shape = "circle", size = point.size, colour = point.col) +
      labs(
        x = x.label,
        y = y.axis,
        title = ep,
        subtitle = "",
        caption = paste("y =", round(coef(lm_model)[1], 3), "+", round(coef(lm_model)[2], 4), "x")
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 15L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        axis.title.y = element_text(size = 13L, face = "bold"),
        axis.title.x = element_text(size = 13L, face = "bold")
      ) +
      xlim(min(data[[x.axis]])-5, max(data[[x.axis]])+10)
    
    # ajouter le graphique à la liste
    plots[[length(plots) + 1]] <- plot
  }
  
  # afficher les graphiques en grille
  grid.arrange(grobs = plots, ncol = ncol) # ajuster le nombre de colonnes
}

# graph_lineaire: exemple ----
# graph_lineaire(dataset = best_femmes, x.axis = "age_at_ep", y.axis = "speed", epreuve = list_epreuve, x.label="Age", printTable=FALSE, ncol=3, point.size=3.6, point.col="#E683F6", line.size=1.5, line.col="blue3")






# graph_quadratique ----
graph_quadratique <- function(dataset, x.axis, y.axis, x.label, epreuve, printTable=TRUE, valueTable = FALSE, ncol=2, point.size=3.6, point.col="#E683F6", point.shape = 16, line.size=1.5, line.col="blue", table_name="result") {
  if (!is.vector(epreuve)) {
    epreuve <- as.vector(epreuve)
  }
  
  plots <- list()
  results <- data.frame(Epreuve = character(),
                        a = numeric(),
                        b = numeric(),
                        c = numeric(),
                        R2_ajuste = numeric(),
                        AICc = numeric(),
                        BIC = numeric(),
                        # estimated_maximum = numeric(),
                        # estimated_objective = numeric(),
                        residus.sum = numeric(),
                        residus.sd = numeric(),
                        ShapiroWilk_normality.W = numeric(),
                        ShapiroWilk_normality.p.value = numeric())
  
  
  for (ep in epreuve) {
    data <- dataset %>%
      filter(epreuve == ep)
    
    if (printTable == T) {
      print(data)
    }
    
    
    model <- glm(data[[y.axis]] ~ poly(data[[x.axis]], 2))
    # coef <- c(coef(model)[[1]], coef(model)[[2]], coef(model)[[3]])
    residus <- residuals(model)
    residus.sum <- sum(residus)
    residus.sd <- sd(residus)
    ShapiroWilk_normality.W <- shapiro.test(residus)$statistic
    ShapiroWilk_normality.p.value <- shapiro.test(residus)$p.value
    
    R2.adjusted <- adjR2(model)
    
    AICc <- AIC(model)
    BIC <- BIC(model)
    
    # optimize_result <- optimize(function(x) modele_quadratique(x, coef$parametre), interval = c(0, 100), maximum = TRUE)
    # estimated_maximum <- optimize_result$maximum
    # estimated_objective <- optimize_result$objective
    
    results[nrow(results) + 1, ] <- list(ep, coef(model)[[1]], coef(model)[[2]], coef(model)[[3]], R2.adjusted, AICc, BIC,
                                         # estimated_maximum, estimated_objective,
                                         residus.sum, residus.sd, ShapiroWilk_normality.W, ShapiroWilk_normality.p.value)
    
    
    plot <- ggplot(data) +
      aes_string(x = x.axis, y = y.axis) +
      geom_smooth(method = "glm", formula = y ~ poly(x, 2), se = FALSE, color = line.col, size = line.size) +
      geom_point(shape = point.shape, size = point.size, colour = point.col) +
      labs(
        x = x.label,
        y = y.axis,
        title = ep,
        subtitle = "",
        caption = paste("y =", round(coef(model)[[1]], 3), "+", round(coef(model)[[2]], 3), "x +", round(coef(model)[[3]], 3), "x^2")
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 15L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 13L, face = "bold"),
        axis.title.x = element_text(size = 13L, face = "bold")
      ) +
      xlim(min(data[[x.axis]])-5, max(data[[x.axis]])+10)
    
    plots[[length(plots) + 1]] <- plot
  }
  
  grid.arrange(grobs = plots, ncol = ncol)
  
  if (valueTable == T) {
    assign(table_name, results, envir = .GlobalEnv)
  }
}

# graph_quadratique: exemple ----
#graph_quadratique(dataset = best_femmes, x.axis = "age_at_ep", y.axis = "speed", epreuve = list_epreuve, x.label="Age", valueTable = TRUE, printTable=FALSE, ncol=3, point.size=3.6, point.col="#E683F6", line.size=1.5, line.col="blue3")



# graph_moore ----

graph_moore <- function(dataset, epreuve,
                        x.axis, x.label, y.axis,
                        printTable=TRUE, valueTable = F,
                        ncol=1, point.size=3.6, point.col="#E683F6", point.shape = 16, line.size=1.5, line.col="blue", table_name="result") {
  if (!is.vector(epreuve)) {
    epreuve <- as.vector(epreuve)
  }
  plots <- list()
  results <- data.frame(Epreuve = character(),
                        a = numeric(), b = numeric(), c = numeric(), d = numeric(),
                        R2_ajuste = numeric(), AICc = numeric(), BIC = numeric(),
                        estimated_maximum = numeric(), estimated_objective = numeric(),
                        residus.mean = numeric(), residus.sum = numeric(),
                        ShapiroWilk_normality.W = numeric(), ShapiroWilk_normality.p.value = numeric()
                        )
  
  for (ep in epreuve) {
    data <- dataset %>%
      filter(epreuve == ep)
      if (printTable == TRUE) {
      print(data)
    }
    
    coef <- MMC(data[[x.axis]], data[[y.axis]], methode = modele_moore, nbpara = 4, precision=100, borne = 0, initial = c(max(data[[y.axis]]), 0.1554491, 0.40300051, 0.025860582))
    model <- lm(data[[y.axis]] ~ modele_moore(data[[x.axis]], coef$parametre))
    
    residus <- residuals(model)
    residus.sum <- sum(residus)
    residus.sd <- sd(residus)
    ShapiroWilk_normality.W <- shapiro.test(residus)$statistic
    ShapiroWilk_normality.p.value <- shapiro.test(residus)$p.value
    R2.adjusted <- adjR2(model)
    AICc <- AIC(model)
    BIC <- BIC(model)
    
    optimize_result <- optimize(function(x) modele_moore(x, coef$parametre), interval = c(0, 100), maximum = TRUE)
    estimated_maximum <- optimize_result$maximum
    estimated_objective <- optimize_result$objective
    
    results[nrow(results) + 1, ] <- list(ep, coef$parametre[1], coef$parametre[2], coef$parametre[3], coef$parametre[4], R2.adjusted, AICc, BIC, estimated_maximum, estimated_objective, residus.sum, residus.sd, ShapiroWilk_normality.W, ShapiroWilk_normality.p.value)
    
    plot <- ggplot(data) +
      aes_string(x = x.axis, y = y.axis) +
      geom_smooth(method = "lm", formula = y ~ modele_moore(x, coef$parametre), se = FALSE, color = line.col, size = line.size) +
      geom_point(shape = point.shape, size = point.size, colour = point.col) +
      labs(x = x.label, y = y.axis, title = ep, subtitle = "",
        caption = paste("y = ", round(coef$parametre[1], 2),"(1 - exp(-", round(coef$parametre[2], 2),"*x)) + (", round(coef$parametre[3], 2), "(1 - exp(", round(coef$parametre[4], 2), "*x)))")) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 15L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 13L, face = "bold"),
        axis.title.x = element_text(size = 13L, face = "bold")
      ) +
      xlim(min(data[[x.axis]])-5, max(data[[x.axis]])+10)
    plots[[length(plots) + 1]] <- plot
  }
  grid.arrange(grobs = plots, ncol = ncol)
  if (valueTable == T) {
    assign(table_name, results, envir = .GlobalEnv)
  }
}

# graph_moore: exemple ----
#graph_moore(dataset = best_femmes, x.axis = "age_at_ep", y.axis = "speed", epreuve = c("middle-long/5000-metres", "middle-long/10000-metres"), x.label="Age", printTable=FALSE, valueTable = TRUE, ncol=2, point.size=3.6, point.col="#E683F6", line.size=1.5, line.col="blue3")




