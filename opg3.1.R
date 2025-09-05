# Funktion der slår n terninger og returnerer antal 5'ere + sandsynlighed
terning_slag <- function(n = 25000) {
  # Slå n terninger (værdier fra 1 til 6)
  slag <- sample(1:6, size = n, replace = TRUE)
  
  # Tæl antal 5'ere
  antal_5 <- sum(slag == 5)
  
  # Beregn sandsynlighed
  p_hat <- antal_5 / n
  
  # Print resultat
  cat("Antal 5'ere:", antal_5, "\n")
  cat("Sandsynlighed for at slå en 5'er:", p_hat, "\n")
  
  # Returnér som liste
  return(list(antal_5 = antal_5, sandsynlighed = p_hat))
}

# Kør funktionen
resultat <- terning_slag(25000)