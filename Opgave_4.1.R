# OPGAVE 4.1
# De R-code die gebruikt is in het voorbeeld in paragraaf 4 staat hieronder. Kopieer het script naar R. 
n <- 10
g <- 5:10
alpha <- round(pbinom(q = (g-1), size = n, prob = .5, lower.tail = FALSE), 4)
tabel <- data.frame(grenswaarde = g, alpha)


# De functie die gebruikt wordt om de alpha waarde uit te rekenen is pbinom(). Zie de helpfunctie voor de argumenten.
# Stel dat de proefpersoon van 100 kaarten moet voorspellen of deze rood of zwart is. 
# Hoeveel kaarten moet een proefpersoon dan goed voorspellen om de claim van paranormaal begaafdheid gehonoreerd te krijgen. 
# Ga ervan uit dat een α-risico van 0.05 als acceptabel wordt beschouwd.

test_binominaal <- function (p_aantal, p_grenswaardes = NA, p_kans, p_alpha ){
  n <- p_aantal
  if (is.na(p_grenswaardes)){
    g <- round(p_aantal/2):p_aantal
  } else{
    g <- p_grenswaardes
  }
  alpha <- round(pbinom(q = (g-1), size = n, prob = p_kans, lower.tail = FALSE), 4)
  tabel <- data.frame(grenswaarde = g, alpha)
  head(tabel[ alpha< p_alpha ,  ],1)
}

test_binominaal(10, p_kans = 0.5, p_alpha = 0.15)

