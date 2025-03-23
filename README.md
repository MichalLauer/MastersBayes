# Dependencies

```r
tinytex::tlmgr_install("babel")
tinytex::tlmgr_install("babel-english")
tinytex::tlmgr_install("babel-slovak")
tinytex::tlmgr_install("babel-czech")
tinytex::tlmgr_install("colorprofiles")
```

- **Neporovnávám bayesovský a frekventistický přístup.**
- Nedělat předpoklady, soustředit se na aplikaci.

---

# Úvod

- ...

# Analýza časových řad

- typy dat, klíčové vlastnosti, představení TS

## Analýza časových řad

- definice časových řad, jejich popis.
- stacionarita.
- problém s heteroskedasticitou, "shlukování" variability.

## Finanční data

- je tam volatilita.
- příprava returns.

## Modely GARCH

- navázání na motivaci, proč je model důležitý (heteroskedasticita).
- popis modelu, odvození, vlastnosti.

### Modely ARCH

- zjednodušený GARCH, vlastnosti, představení.

## Modely SV

- navázání na motivaci, proč je model důležitý (heteroskedasticita).
- popis modelu, odvození, vlastnosti.

## Porovnání modelů

- jak se modely liší?.
- jaké jsou výhody/nevýhody?

# Bayesovský přístup ke statistice

- co to je bayesovská statistika, jak přistupuje ke statistice.

## Představení bayesovské statistiky

- obecné porovnání oproti frekventistickým metodám.
- hlavní výhody bayesovské statistiky.
- interpretace výsledků.

## Tvoření modelů

- Jak se v bayesovské statistice tvoří obecně modely.
- aplikace na bayesovském vzorci.

## Simulační metody

- motivace simulačních metod, popis MCMC (M-H, Gibbs), HMC (NUTS).

## Bayesovské GARCH modely

- spojení bayesovské statistiky a GARCH modelů.
- volba apriorních rozdělení, posteriorní rozdělení.
- příklad kódu v jazyce Stan.
- praktické použité - najít nějaké články, kde se to už použilo.

## Aplikace na SV modely

- spojení bayesovské statistiky a SV modelů.
- volba apriorních rozdělení, posteriorní rozdělení.
- příklad kódu v jazyce Stan.
- praktické použité - najít nějaké články, kde se to už použilo.

# Predikce finančních řad

- představení analýzy a aplikace

## Finanční časové řady

- představení finančních časových řad.
- proč je nutné zde používat GARCH/SV.
- nestacionarita -> returns.

## Aplikace

- použití GARCH/SV pro predikci finančních časových řad.
- interpretace koeficientů, predikční schopnost.
- vizualizace apriorních/posteriorních rozdělení.
- porovnání modelů na různých finančních řadách.
