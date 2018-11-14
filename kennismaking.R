# Opgave 1.1
8 + 7
19 * 5
3^5
3 ** 5
125^(1/3)
125^1/3
125^0.5
38 %% 5
38 %/% 5
sqrt(256)
sqrt(125)
round(sqrt(125), 2)
round(x = 3.467, digits=1)
round(3.467, 1)
round(-5.23)
floor(5.983)
ceiling(5.983)
ceiling(-3.67)
floor(38/5)
trunc(100/3)
log(x = 1000, base=10)
log(1000, 10)
log(10, 1000)
log(base=10, x=1000)
log(1000)
log(81, 3)

# Opgave 1.2a
3 < 7
5 == 6
5 != 6
-7 < -8
sqrt(16) + sqrt(9) == sqrt(25)
is.numeric(25)
is.numeric("25")
is.character("banaan")
3 < 4 & 4 < 5
3 < 4 & -3 < -4
3 < 4 | -3 < -4
3 < 4 | 4 < 5
xor(3 < 4, 4 < 3)
xor(3 < 4, 4 < 5)
xor(3 < 2, 5 < 4)


# Opgave 1.2b
TRUE + TRUE
(3 < 4) + (4 < 5)
as.numeric(TRUE)
FALSE == 0


# Opgave 1.3
x <- 5
x ^ 2
print(x ^ 2)
y <- 20
z <- y/x
print(z)
a1 <- 9.1
a2 <- 5.7
a3 <- 7.5
a
x <- 25; y <- 180
sqrt(x)
log(x * 4, base = 10)
ceiling(y / x)
floor(y / x)
abs(x - y)


# Opgave 3.1a
a = 1:5
b = 2:3
a + b
3 * a
b ** 2
c = c(1, 3, 5, 7, 9, 11)
d = seq(1, 11, by=2)
c
d

# Opgave 3.1b
x <- 1:10
y <- c(x, x^2, x^3, x^4)
y
str(y)
matrix01 <- cbind(x, x^2,
                  x^3, x^4)
matrix01
str(matrix01)
alphabet <- letters
ALPHABET <- LETTERS
alphabet
ALPHABET
matrix02 <- rbind(alphabet,
                  ALPHABET)
matrix02
str(matrix02)
matrix03 <- matrix(1:16,
                   nrow = 4, ncol = 4)
matrix04 <- matrix(rep(1,
                       times = 16), nrow = 4)
matrix03
matrix04
matrix03 + matrix04
matrix03 * matrix04

#voor wie bekend is met matrixrekening:
matrix03 %*% matrix04
df <- cbind(1:26, alphabet,
            ALPHABET)
df
str(df)
names(df)
names(df) <- c("NR", "letter", "LETTER")
df
names(df)


# Opdracht 3.1c 
voorl <- c("A", "G. J.", "K")
tussenv <- c(NULL, "van", NULL)
anaam <- c("Peters", "Buren", "Bril")
inkomen <- c(2500, 3250, NA)
bestand <- data.frame(cbind(voorl, tussenv, anaam, inkomen),
                      stringsAsFactors = FALSE)
names(bestand) <- c("voorl", "tussenv", "anaam", "inkomen")
aantal <- length(bestand$anaam)
str(bestand)
bestand$inkomen <- as.numeric(bestand$inkomen)
str(bestand)
gem_inkomen <- mean(bestand$inkomen)
gem_inkomen
gem_inkomen <- mean(bestand$inkomen, na.rm = TRUE)
gem_inkomen
sd_inkomen <- sd(bestand$inkomen, na.rm = TRUE)
sd_inkomen
na.omit(bestand)

#install.packages("dplyr")
library(dplyr)
