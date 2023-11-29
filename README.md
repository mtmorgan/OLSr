
# OLSr

<!-- badges: start -->
<!-- badges: end -->

The Ontology Lookup Service ([OLS][]) is a repository of biomedical
ontologies provided by the European Bioinformatics Institute
('EMBL-EBI'). [OLSr][] provides an interface to this service, allowing
discovery of ontologies, terms, and relations. Results are presented
in a 'tidy' framework, so that manipulation tasks are easily
accomplished with verbs such as 'filter()', 'select()', and
'mutate()'. An effort is made to integrate with other packages useful
for more comprehensive biological and ontological analysis.

[OLS]: https://www.ebi.ac.uk/ols4/
[OLSr]: https://mtmorgan.github.io/OLSr

## Installation

Install the development version of [OLSr][] with

``` r
if (!"remotes" in rownames(installed.packages()))
    install.packages("remotes", repos = "https://cran.r-project.org")
devtools::install_github("mtmorgan/OLSr")
```

Attach the installed package to your *R* session with

``` r
library(OLSr)
```

## Use

The [introductory vignette][intro] outlines common use cases,
including

- Discovering ontologies available in the OLS.
- Retrieving terms within an ontology
- Retrieving relatives (parents, ancestors, children, descendents) of
  terms.

The CELLxGENE [case study vignette][cxg] outlines ontology
representation as graphs, and manipulation and visualization using
igraph, visNetwork and other *R* packages.

  
[OLSr][] uses a cache to minimize the number of calls to the OLS; the
introductory vignette includes a short section discussing cache
management.

## Next steps

See the [introductory vignette][intro] for next steps.

[intro]: ./articles/a_introduction.html
[cxg]: ./articles/b_case_study_cxg.html
