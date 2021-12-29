# About

This app uses the Defined Approaches for Skin Sensitization (DASS)
outlined in the OECD DASS Guideline No. 497\[[1](#ref-oecd_dass)\]. The
Defined Approaches (DAs) integrate results from *in vitro* and *in
silico* test methods to predict chemical hazard potential.

# System Requirements

This app works on Windows. This app requires
[R](https://www.r-project.org/). When first launching the app locally,
the *renv*\[[2](#ref-renv)\] package will be installed. While launching,
the data.table\[[3](#ref-datatable)\], DT\[[4](#ref-dt)\],
shiny\[[5](#ref-shiny)\], shinyBS\[[6](#ref-shinybs)\], and
shinyjs\[[7](#ref-shinyjs)\] packages will be installed.

This app was created using R v.4.1.2\[[8](#ref-R-base)\] and has been
tested in Google Chrome v.91 on Windows 10.

# Installation

If installing from Bitbucket, clone the repository using the “Clone”
button at the top of the repository. If installing from zip file, unzip
the folder.

Open the downloaded folder. To launch the app, click the run\_app.bat
file.

# Data Import

Once the app is launched, to begin, click *Browse* to upload your data.
The data file must be comma-delimited (.csv) or tab-delimited (.tsv,
.txt). The first row should contain column names. Columns must be
formatted as described in Step 2. Once you select a file, the data can
be viewed by clicking on the *Data* tab.

# Step 1: Select Defined Approaches

After uploading your data, the *Step 1* tab will open. Select the DAs
you want to apply and click Done. The DAs are based on the first 3 key
events (KEs) in the Adverse Outcome Pathway (AOP) for Skin Sensitization
Initiated by Covalent Binding to Proteins\[[9](#ref-oecd_aop)\].

The KEs are covered by validated OECD test methods:

1.  The direct peptide reactivity assay (DPRA)\[[10](#ref-oecd_dpra)\]
    maps to the first KE, protein binding.
2.  The KeratinoSens™\[[11](#ref-oecd_ks)\] assay maps to the second KE,
    keratinocyte activation.
3.  The human cell line activation test
    (h-CLAT)\[[12](#ref-oecd_hclat)\] maps to the third KE, dendritic
    cell activation.

## 2 out of 3

The 2 out of 3 (2o3) DA is a sequential testing strategy that identifies
skin sensitization hazard based on KEs 1-3. Two concordant results from
DPRA, KeratinoSens™, or h-CLAT determine the final classification as a
sensitizer or non-sensitizer. If there are only results from two assays
and the results are discordant, the chemical can’t be classified and
will return an “Inconclusive” result.

2o3 does not evaluate potency.

## Integrated Testing Strategy

This app implements version 2 of the Integrated Testing Strategy (ITSv2)
DA. ITSv2 predicts skin sensitization hazard potential and potency
category based on KEs 1 and 3 and *in silico* predictions from the OECD
QSAR Toolbox\[[13](#ref-YORDANOVA201989)\]. Chemicals are scored for
each assay result and the summed scores are used to predict chemical
hazard and potency using the scoring schemes in Tables 1 and 2.

| Score | h-CLAT MIT<br>(&mu;g/mL)     | DPRA<br>mean of %-C<br>and %-K depletion | DPRA<br>%-C depletion | OECD QSAR<br>Toolbox |
|-------|------------------------------|------------------------------------------|-----------------------|----------------------|
| 3     | &le;10                       | &ge;42.47                                | &ge;98.24             | -                    |
| 2     | >10, &le;150                 | &ge;22.62, >42.47                        | &ge;23.09, >98.24     | -                    |
| 1     | >150, &le;5000               | &ge;6.38, >22.62                         | &ge;13.89, >23.09     | Positive             |
| 0     | Negative<br>(Not calculated) | <6.38                                    | <13.89                | Negative             |

*Table 1.* Test method scoring scheme for version 2 of the Integrated
Testing Strategy defined approach, adapted from \[[1](#ref-oecd_dass)\]

| Combined<br>Score | DPRA +<br>h-CLAT +<br> OECD QSAR TB | DPRA + h-CLAT | DPRA + OECD QSAR TB *or*<br>h-CLAT + OECD QSAR TB |
|-------------------|-------------------------------------|---------------|---------------------------------------------------|
| 7                 | UN GHS 1A                           | - -           |                                                   |
| 6                 | UN GHS 1A                           | UN GHS 1A     | -                                                 |
| 5                 | UN GHS 1B                           | UN GHS 1*     | -                                                 |
| 4                 | UN GHS 1B                           | UN GHS 1B     | UN GHS 1*                                         |
| 3                 | UN GHS 1B                           | UN GHS 1B     | UN GHS 1*                                         |
| 2                 | UN GHS 1B                           | UN GHS 1B     | UN GHS 1B                                         |
| 1                 | NC                                  | Inconclusive  | Inconclusive                                      |
| 0                 | NC                                  | NC            | Inconclusive                                      |

*Table 2.* Potency predictions for combined scores from available
information sources. 1\* indicates conclusive for hazard, inconclusive
for potency. Adapted from \[[1](#ref-oecd_dass)\].

## Key Event 3/1 Sequential Testing Strategy

The KE 3/1 Sequential Testing Strategy (STS) predicts skin sensitization
hazard and potency based on KEs 1 and 3. If the h-CLAT predicts a
sensitizer, then the potency category is determined by the h-CLAT
Minimum Induction Threshold (MIT). If the h-CLAT predicts a
non-sensitizer, then the DPRA result defines both hazard and potency.
The KE 3/1 STS scheme is shown in Table 3.

| Test Method | Result            | Hazard   | Potency  |
|-------------|-------------------|----------|----------|
| h-CLAT      | MIT &le;10        | Positive | 1A       |
| h-CLAT      | MIT >10, &le;5000 | Positive | 1B       |
| h-CLAT      | Negative          | Use DPRA | Use DPRA |
| DPRA        | Positive          | Positive | 1B       |
| DPRA        | Negative          | Negative | NC       |

*Table 3.* Hazard and potency prediction scheme. Adapted from
\[[14](#ref-NUKADA2013609)\] and \[[15](#ref-takenouchi2015)\]

# Step 2: Select Data Columns for Predictions

After clicking “Done,” the panel for column selection will expand. All
assay endpoints that are needed to apply the selected DAs will be shown.
Use the dropdown menus to select the columns corresponding to the given
assay result. Click “Done” to evaluate the values in the column for
proper formatting.

Columns must be formatted correctly to ensure an accurate prediction. A
description of the column requirements are given below.

## DPRA

### % Depletion

DPRA % Depletion is used in ITSv2. The % Cysteine and % Lysine depletion
columns must contain only numbers or NA if there is no DPRA outcome for
the given chemical. Negative values imply co-elution. If the value for %
Cysteine depletion is negative for a given chemical, then DPRA results
can’t be used to predict skin sensitization. If only the value for
Lysine is negative, then % Cysteine depletion can be used for scoring
(Table 1). Otherwise, the mean of % Cysteine depletion and % Lysine
depletion will be used to score the chemical.

### Call

DPRA call should be an indicator for a positive or negative outcome from
DPRA. Positive outcomes must be indicated by “p,” “pos,” “positive,” or
1. Negative outcomes must be indicated by “n,” “neg,” “negative,” or 0.
Any other values will not be used to predict skin sensitization hazard.

If % Cysteine depletion and % Lysine depletion are required, you will
have the option to use those values to derive the positive or negative
calls. Calls are defined as outlined in OECD Test Guideline 442c
\[[10](#ref-oecd_dpra)\].

## h-CLAT

### Call

h-CLAT call should be an indicator for a positive or negative outcome
from h-CLAT. Positive outcomes must be indicated by “p,” “pos,”
“positive,” or 1. Negative outcomes must be indicated by “n,” “neg,”
“negative,” or 0. Any other values will not be used to predict skin
sensitization hazard.

### Minimum Induction Threshold

h-CLAT minimum induction threshold (MIT) must contain either positive
numeric values for positive outcomes or “n,” “neg,” “negative,” or “Inf”
to indicate a negative outcome. Any other values will not be used to
predict skin sensitization potency. Missing values will not be used to
predict skin sensitization potency.

## KeratinoSens™ Call

KeratinoSens™ (KS) call should be an indicator for a positive or
negative outcome from the KS assay. Positive outcomes must be indicated
by “p,” “pos,” “positive,” or 1. Negative outcomes must be indicated by
“n,” “neg,” “negative,” or 0. Any other values will not be used to
predict skin sensitization hazard.

## OECD QSAR Toolbox

### Call

The OECD QSAR Toolbox (TB) call should be an indicator for a positive or
negative prediction where positive predictions are indicated by “p,”
“pos,” “positive,” or 1. Negative predictions must be indicated by “n,”
“neg,” “negative,” or 0. Any other values will not be used to predict
skin sensitization hazard.

### Applicability Domain

The OECD QSAR Toolbox (TB) applicability domain (AD) should be an
indicator for whether the chemical is in the AD of the toolbox’s models.
A value of “In” or 1 indicates that the chemical is in the AD. A value
of “Out” or 0 indicates that the chemical is outside the AD and the OECD
QSAR TB prediction will not be used to predict skin sensitization
potency.

# Step 3: Review Selection

The selected columns will be evaluated for proper formatting. In the
*Step 3* tab, a table with the selected columns will show if any columns
contain invalid values. Click “Run” to run the DASS predictions. If
flagged columns were not fixed prior to running the predictions, any
invalid values will be marked as missing and will not be used to predict
skin sensitization hazard and potency.

# Step 4: Results

The *Step 4* tab will show a table with the original data with the DASS
prediction columns appended to the end. The table will also contain new
columns with the converted column values that were used in the
predictions. This may be useful if predictions were run on flagged data.

Click the *Download Results* button to download a file with the results
table.

# References

<span class="csl-left-margin">1. </span><span
class="csl-right-inline">OECD. Guideline no. 497: Defined approaches on
skin sensitisation. 2021. p. 54.
doi:<https://doi.org/https://doi.org/10.1787/b92879a4-en></span>

<span class="csl-left-margin">2. </span><span
class="csl-right-inline">Ushey K. Renv: Project environments. 2021.
Available: <https://CRAN.R-project.org/package=renv></span>

<span class="csl-left-margin">3. </span><span
class="csl-right-inline">Dowle M, Srinivasan A. Data.table: Extension of
‘data.frame‘. 2021. Available:
<https://CRAN.R-project.org/package=data.table></span>

<span class="csl-left-margin">4. </span><span
class="csl-right-inline">Xie Y, Cheng J, Tan X. DT: A wrapper of the
JavaScript library ’DataTables’. 2021. Available:
<https://CRAN.R-project.org/package=DT></span>

<span class="csl-left-margin">5. </span><span
class="csl-right-inline">Chang W, Cheng J, Allaire J, Sievert C,
Schloerke B, Xie Y, et al. Shiny: Web application framework for r. 2021.
Available: <https://CRAN.R-project.org/package=shiny></span>

<span class="csl-left-margin">6. </span><span
class="csl-right-inline">Bailey E. shinyBS: Twitter bootstrap components
for shiny. 2015. Available:
<https://CRAN.R-project.org/package=shinyBS></span>

<span class="csl-left-margin">7. </span><span
class="csl-right-inline">Attali D. Shinyjs: Easily improve the user
experience of your shiny apps in seconds. 2020. Available:
<https://CRAN.R-project.org/package=shinyjs></span>

<span class="csl-left-margin">8. </span><span class="csl-right-inline">R
Core Team. R: A language and environment for statistical computing.
Vienna, Austria: R Foundation for Statistical Computing; 2021.
Available: <https://www.R-project.org/></span>

<span class="csl-left-margin">9. </span><span
class="csl-right-inline">OECD. The adverse outcome pathway for skin
sensitisation initiated by covalent binding to proteins. 2014. p. 105.
doi:<https://doi.org/https://doi.org/10.1787/9789264221444-en></span>

<span class="csl-left-margin">10. </span><span
class="csl-right-inline">OECD. Test no. 442C: In chemico skin
sensitisation. 2021. p. 40.
doi:<https://doi.org/https://doi.org/10.1787/9789264229709-en></span>

<span class="csl-left-margin">11. </span><span
class="csl-right-inline">OECD. Test no. 442D: In vitro skin
sensitisation. 2018. p. 51.
doi:<https://doi.org/https://doi.org/10.1787/9789264229822-en></span>

<span class="csl-left-margin">12. </span><span
class="csl-right-inline">OECD. Test no. 442E: In vitro skin
sensitisation. 2018. p. 65.
doi:<https://doi.org/https://doi.org/10.1787/9789264264359-en></span>

<span class="csl-left-margin">13. </span><span
class="csl-right-inline">Yordanova D, Schultz TW, Kuseva C, Tankova K,
Ivanova H, Dermen I, et al. Automated and standardized workflows in the
OECD QSAR toolbox. Computational Toxicology. 2019;10: 89–104.
doi:<https://doi.org/10.1016/j.comtox.2019.01.006></span>

<span class="csl-left-margin">14. </span><span
class="csl-right-inline">Nukada Y, Miyazawa M, Kazutoshi S, Sakaguchi H,
Nishiyama N. Data integration of non-animal tests for the development of
a test battery to predict the skin sensitizing potential and potency of
chemicals. Toxicology in Vitro. 2013;27: 609–618.
doi:<https://doi.org/10.1016/j.tiv.2012.11.006></span>

<span class="csl-left-margin">15. </span><span
class="csl-right-inline">Takenouchi O, Fukui S, Okamoto K, Kurotani S,
Imai N, Fujishiro M, et al. Test battery with the human cell line
activation test, direct peptide reactivity assay and DEREK based on a
139 chemical data set for predicting skin sensitizing potential and
potency of chemicals. Journal of Applied Toxicology. 2015;735:
1318–1332. doi:<https://doi.org/10.1002/jat.3127></span>
