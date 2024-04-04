# Frage 2: exchange_money (15P)

# Schreiben Sie eine Funktion, die Euros in US-Dollars umrechnet und vice versa.
# Verwenden Sie den Wechselkurs vom 09.01.2024: 1 USD = 0,92 EUR als Standardwert.
# Achten Sie darauf, alle Argumente gründlich zu überprüfen.
# Nennen Sie die Funktion `ex02exchange_money`.
# Ihnen stehen die Tests "2.1", "2.2" und "2.3" mit je mehreren Assertions zur Verfügung.


## Eingabe

# `value` Numerischer Vektor ohne fehlende Werte. Der Geldbetrag, der gewechselt wird
# `currency`: Character Skalar. Die Ausgangswährung, muss entweder "EUR" oder "USD" annehmen. Per Default: "USD"
# `eur_per_usd`: Numerischer Skalar (>0). Der Wechselkurs, zu dem gewechselt wird. Per Default: 0.92

## Ausgabe

#`numeric(n)`

## Beispiele

#ex02exchange_money(0.8)
#> [1] 0.736
#ex02exchange_money(0.8, "USD", 0.92)
#> [1] 0.736
#ex02exchange_money(2.44, "EUR", 0.92)
#> [1] 2.652174
#ex02exchange_money(0.8, "USD", 0.89)
#> [1] 0.712
#ex02exchange_money(1:4, "USD", 0.89)
#> [1] 0.89 1.78 2.67 3.56
#ex02exchange_money(c(1:4, NA), "USD", 0.89)
#> [1] 0.89 1.78 2.67 3.56   NA
#ex02exchange_money(c("Eins", 3, 3.1), "USD", 0.89)
#> Error in ex02exchange_money(c("Eins", 3, 3.1), "USD", 0.89) :
#>  Assertion on 'value' failed: Must be of type 'numeric', not 'character'.
#ex02exchange_money(1:4, "CAD", 0.89)
#> Error in match.arg(currency, c("USD", "EUR")) :
#>   'arg' should be one of “USD”, “EUR”

#' Currency conversion between EUR and USD
#' This function coverts EUR to USD and vice versa.
#' The exchange rate can be supplied manually but defaults to
#' 1 USD = 0.92 EUR
#' Arguments:
#' value: temperature in Fahrenheit or Celsius. A numeric vector.
#' currency: the currency that value is in. Must be "USD" for US Dollars or
#'           "EUR" for Euros.
#' eur_per_usd: The exchange rate EUR/USD, a numeric scaler.
#' Returns: a numeric vector indicating the temperature in Celsius.
ex02exchange_money <- function(value, currency = "USD", 
                               eur_per_usd = 0.92) {
#TODO
}
