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

## Finanční data

- příprava returns.
- problém s heteroskedasticitou, "shlukování" variability.

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

# Bayesovská statistika

- co to je bayesovská statistika, jak přistupuje ke statistice.
- obecné porovnání oproti frekventistickým metodám.
- hlavní výhody bayesovské statistiky.
- interpretace výsledků.

## Bayesian formula and models

- popis bayesovského vzorečku
- data, hypotézy
- tvoření modelů

## Simulační metody

- Jak funguje MCMC

### Metropolis Algorithm

First developed by Metropolis et al.
Basic symmetric proposal distribution
Acceptance/rejection criteria

### Metropolis-Hastings Algorithm

Hastings' generalization of Metropolis
Asymmetric proposal distributions
Modified acceptance ratio

### Gibbs Sampling

Conditional distribution sampling
When and why it's efficient
Relationship to Metropolis-Hastings

### Hamiltonian Monte Carlo

Physics-inspired approach using Hamiltonian dynamics
Momentum variables and trajectory simulation
Benefits for high-dimensional spaces

### No-U-Turn Sampler

Extension of HMC
Adaptive path length selection

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
