
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `sherlock`

<!-- badges: start -->

[![R-CMD-check](https://github.com/Netflix/sherlock/workflows/R-CMD-check/badge.svg)](https://github.com/Netflix/sherlock/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/Netflix/sherlock/master.svg)](https://codecov.io/github/Netflix/sherlock?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5652010.svg)](https://doi.org/10.5281/zenodo.5652010)
<!-- badges: end -->

> Causal Machine Learning for Population Segment Discovery and Analysis

**Authors:** [Nima Hejazi](https://nimahejazi.org) and [Wenjing
Zheng](https://www.linkedin.com/in/wenjing-zheng/)

-----

## Causal Segmentation Analysis with `sherlock`

The `sherlock` R package implements an approach for population
segmentation analysis (or subgroup discovery) using recently developed
techniques from causal machine learning. Using data from randomized A/B
experiments or observational studies (quasi-experiments), `sherlock`
takes as input a set of user-selected candidate segment dimensions –
often, a subset of measured pre-treatment covariates – to discover
particular segments of the study population based on the estimated
heterogeneity of their response to the treatment under consideration. In
order to quantify this treatment response heterogeneity, the
*conditional average treatment effect* (CATE) is estimated using a
nonparametric, doubly robust framework (Vanderweele et al. 2019; van der
Laan and Luedtke 2015; Luedtke and van der Laan 2016b, 2016a),
incorporating state-of-the-art ensemble machine learning (van der Laan,
Polley, and Hubbard 2007; Coyle et al. 2021) in the estimation
procedure.

For background and details on using `sherlock`, see the package
vignette. For an overview of the statistical methodology, see our
[conference manuscript](https://arxiv.org/abs/2111.01223) (Hejazi,
Zheng, and Anand 2021) from [CODE @ MIT
2021](https://ide.mit.edu/events/2021-conference-on-digital-experimentation-mit-codemit/).

-----

## Installation

Install the *most recent version* from the `master` branch on GitHub via
[`remotes`](https://CRAN.R-project.org/package=remotes):

``` r
remotes::install_github("Netflix/sherlock")
```

<!--
Eventually, the package will make its way to [CRAN](https://CRAN.R-project.org).
At that point, a stable version may be installed via


```r
install.packages("sherlock")
```
-->

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/Netflix/sherlock/issues).

-----

## Citation

After using the `sherlock` R package, please cite the following:

``` 
    @software{netflix2021sherlock,
      author={Hejazi, Nima S and Zheng, Wenjing and {Netflix, Inc.}},
      title = {{sherlock}: Causal machine learning for segment discovery
        and analysis},
      year  = {2021},
      note = {R package version 0.2.0},
      doi = {10.5281/zenodo.5652010},
      url = {https://github.com/Netflix/sherlock}
    }

    @article{hejazi2021framework,
      author = {Hejazi, Nima S and Zheng, Wenjing and Anand, Sathya},
      title = {A framework for causal segmentation analysis with machine
        learning in large-scale digital experiments},
      year = {2021},
      journal = {Conference on Digital Experimentation at {MIT}},
      volume = {(8\textsuperscript{th} annual)},
      publisher = {MIT Press},
      url = {https://arxiv.org/abs/2111.01223}
    }
```

-----

## License

The contents of this repository are distributed under the Apache 2.0
license. See file
[`LICENSE.md`](https://github.com/Netflix/sherlock/blob/master/LICENSE.md)
for details.

-----

## References

<div id="refs" class="references">

<div id="ref-coyle2021sl3">

Coyle, Jeremy R, Nima S Hejazi, Ivana Malenica, Rachael V Phillips, and
Oleg Sofrygin. 2021. *sl3: Modern Pipelines for Machine Learning and
Super Learning*. <https://doi.org/10.5281/zenodo.1342293>.

</div>

<div id="ref-hejazi2021framework">

Hejazi, Nima S, Wenjing Zheng, and Sathya Anand. 2021. “A Framework for
Causal Segmentation Analysis with Machine Learning in Large-Scale
Digital Experiments.” *Conference on Digital Experimentation at MIT*
(8<sup>th</sup> annual). <https://arxiv.org/abs/2111.01223>.

</div>

<div id="ref-Luedtke16b">

Luedtke, Alex, and Mark van der Laan. 2016a. “Optimal Individualized
Treatments in Resource-Limited Settings.” *International Journal of
Biostatistics* 12 (1): 283–303.
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6052884/>.

</div>

<div id="ref-Luedtke16a">

———. 2016b. “Super-Learning of an Optimal Dynamic Treatment Rule.”
*International Journal of Biostatistics* 12 (1): 305–32.
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6056197/>.

</div>

<div id="ref-vdl2007super">

van der Laan, Mark J, Eric C Polley, and Alan E Hubbard. 2007. “Super
Learner.” *Statistical Applications in Genetics and Molecular Biology* 6
(1).

</div>

<div id="ref-vdL15">

van der Laan, Mark, and Alex Luedtke. 2015. “Targeted Learning of the
Mean Outcome Under an Optimal Dynamic Treatment Rule.” *Journal of
Causal Inference* 3 (1): 61–95.
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4517487/>.

</div>

<div id="ref-vanderweele19">

Vanderweele, Tyler, Alex Luedtke, Mark van der Laan, and Ronald Kessler.
2019. “Selecting Optimal Subgroups for Treatment Using Many Covariates.”
*Epidemiology* 30 (3): 334–41.
<https://journals.lww.com/epidem/Fulltext/2019/05000/Selecting_Optimal_Subgroups_for_Treatment_Using.5.aspx>.

</div>

</div>
