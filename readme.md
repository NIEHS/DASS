# About

This app uses the Defined Approaches for Skin Sensitization (DASS)
outlined in the OECD DASS Guideline No. 497[1]. The
Defined Approaches (DAs) integrate results from *in vitro* and *in
silico* test methods to predict chemical hazard potential.

# System Requirements

This app works on Windows. This app requires
[R](https://www.r-project.org/). When first launching the app locally,
the *renv*[2] package will be installed. While launching,
the data.table[3], DT[4],
shiny[5], shinyBS[6], and
shinyjs[7] packages will be installed.

This app was developed and tested on Windows 10 using R
v.4.1.2[8] and Google Chrome v.91.

# Installation

If installing from Bitbucket, clone the repository using the *Clone*
button at the top of the repository. If installing from zip file, unzip
the folder.

Open the downloaded folder. To launch the app, click the *run\_app.bat*
file.

# Data Import

To begin, click *Browse* and select your data file. The data must be
comma-delimited (.csv), tab-delimited (.txt, .tsv), or the first
worksheet in an excel file (.xls, .xlsx). Data should be in a tabular
format with rows corresponding to chemicals and columns corresponding to
assay endpoints. The first row should contain column names. Each row,
except the first, should contain data for only a single chemical (i.e.,
a single row should not contain data for multiple chemicals). Each assay
endpoint that is required for using the DAs should have a column that is
formatted as described in Step 2 below.

Once a file is selected, the data can be viewed by clicking on the *View
Data* tab.

# Step 1: Select Defined Approaches

After uploading the data, the *Step 1* tab will open. Select the DAs to
apply and click *Done*. The DAs are based on the first 3 key events
(KEs) in the Adverse Outcome Pathway (AOP) for Skin Sensitization
Initiated by Covalent Binding to Proteins[9]. Each KE
is represented by a validated OECD test method:

1.  The direct peptide reactivity assay (DPRA) [10]
    maps to the first KE, protein binding.
2.  The KeratinoSens™[11] assay maps to the second KE,
    keratinocyte activation.
3.  The human cell line activation test
    (h-CLAT)[12] maps to the third KE, dendritic
    cell activation.

## 2 out of 3

The 2 out of 3 (2o3) DA is a sequential testing strategy that predicts
skin sensitization hazard identification (sensitizer or non-sensitizer)
based on KEs 1-3. Two concordant results from DPRA, KeratinoSens™, or
h-CLAT determine the final prediction as a sensitizer or non-sensitizer.
If there are only results from two assays and the results are
discordant, the chemical can’t be classified and will return an
“Inconclusive” result.

2o3 does not predict GHS potency category.

## Integrated Testing Strategy

The Integrated Testing Strategy (ITS) DA predicts skin sensitization
hazard identification and potency category based on KEs 1 and 3 and *in
silico* predictions from either [Derek
Nexus](https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm)
or the OECD QSAR Toolbox[13]. Chemicals are
scored for each assay result and the summed scores are used to predict
chemical hazard identification and potency using the scoring schemes in
Tables 1 and 2.

<table>
<caption>
Table 1: Test method scoring scheme for the Integrated Testing Strategy,
adapted from [1]
</caption>
<thead>
<tr>
<th style="text-align:center;">
Score
</th>
<th style="text-align:center;">
h-CLAT MIT<br>(μg/mL)
</th>
<th style="text-align:center;">
DPRA<br>mean % Cysteine<br>and Lysine depletion
</th>
<th style="text-align:center;">
DPRA<br>% Cysteine depletion
</th>
<th style="text-align:center;">
In Silico Prediction
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
≤ 10
</td>
<td style="text-align:center;">
≥42.47
</td>
<td style="text-align:center;">
≥98.24
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
&gt;10, ≤ 150
</td>
<td style="text-align:center;">
≥ 22.62, &lt; 42.47
</td>
<td style="text-align:center;">
≥ 23.09, &lt; 98.24
</td>
<td style="text-align:center;">
</td>
</tr>
<tr>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
&gt;150, ≤ 5000
</td>
<td style="text-align:center;">
≥ 6.38, &lt; 22.62
</td>
<td style="text-align:center;">
≥ 13.89, &lt; 23.09
</td>
<td style="text-align:center;">
Positive
</td>
</tr>
<tr>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
Negative (Not Calculated)
</td>
<td style="text-align:center;">
&lt; 6.38
</td>
<td style="text-align:center;">
&lt; 13.89
</td>
<td style="text-align:center;">
Negative
</td>
</tr>
</tbody>
</table>
<table>
<caption>
Table 2: Integrated Testing Strategy potency predictions for combined
scores from available information sources. 1* indicates conclusive for
hazard, inconclusive for potency. Adapted from [1].
</caption>
<thead>
<tr>
<th style="text-align:center;">
Score
</th>
<th style="text-align:center;">
DPRA +<br>h-CLAT+<br>OECD QSAR TB
</th>
<th style="text-align:center;">
DPRA + h-CLAT
</th>
<th style="text-align:center;">
DPRA + OECD QSAR TB or<br>h-CLAT + OECD QSAR TB
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
7
</td>
<td style="text-align:center;">
UN GHS 1A
</td>
<td style="text-align:center;">

-   </td>
    <td style="text-align:center;">

    -   </td>
        </tr>
        <tr>
        <td style="text-align:center;">
        6
        </td>
        <td style="text-align:center;">
        UN GHS 1A
        </td>
        <td style="text-align:center;">
        UN GHS 1A
        </td>
        <td style="text-align:center;">

        -   </td>
            </tr>
            <tr>
            <td style="text-align:center;">
            5
            </td>
            <td style="text-align:center;">
            UN GHS 1B
            </td>
            <td style="text-align:center;">
            UN GHS 1\*
            </td>
            <td style="text-align:center;">

            -   </td>
                </tr>
                <tr>
                <td style="text-align:center;">
                4
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1\*
                </td>
                </tr>
                <tr>
                <td style="text-align:center;">
                3
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1\*
                </td>
                </tr>
                <tr>
                <td style="text-align:center;">
                2
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                <td style="text-align:center;">
                UN GHS 1B
                </td>
                </tr>
                <tr>
                <td style="text-align:center;">
                1
                </td>
                <td style="text-align:center;">
                NC
                </td>
                <td style="text-align:center;">
                Inconclusive
                </td>
                <td style="text-align:center;">
                Inconclusive
                </td>
                </tr>
                <tr>
                <td style="text-align:center;">
                0
                </td>
                <td style="text-align:center;">
                NC
                </td>
                <td style="text-align:center;">
                NC
                </td>
                <td style="text-align:center;">
                Inconclusive
                </td>
                </tr>
                </tbody>
                </table>

## Key Event 3/1 Sequential Testing Strategy

The KE 3/1 Sequential Testing Strategy (STS) predicts skin sensitization
hazard identification and potency based on KEs 1 and 3. If the h-CLAT
predicts a sensitizer, then the hazard identification and potency
categories are determined by the h-CLAT Minimum Induction Threshold
(MIT). If the h-CLAT predicts a non-sensitizer, then the hazard
identification and potency categories are determined by DPRA results.
The KE 3/1 STS scheme is shown in Table 3.

<table>
<caption>
Table 3. Hazard identification and potency prediction scheme for Key
Event 3/1 Sequential Testing Strategy. Adapted from [14] and [15].
</caption>
<thead>
<tr>
<th style="text-align:center;">
Test Method
</th>
<th style="text-align:center;">
Result
</th>
<th style="text-align:center;">
Hazard Identification
</th>
<th style="text-align:center;">
GHS Potency Category
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
h-CLAT
</td>
<td style="text-align:center;">
MIT ≤ 10
</td>
<td style="text-align:center;">
Positive
</td>
<td style="text-align:center;">
1A
</td>
</tr>
<tr>
<td style="text-align:center;">
h-CLAT
</td>
<td style="text-align:center;">
MIT &gt; 10, ≤ 5000
</td>
<td style="text-align:center;">
Positive
</td>
<td style="text-align:center;">
1B
</td>
</tr>
<tr>
<td style="text-align:center;">
h-CLAT
</td>
<td style="text-align:center;">
MIT Negative
</td>
<td style="text-align:center;">
Use DPRA
</td>
<td style="text-align:center;">
Use DPRA
</td>
</tr>
<tr>
<td style="text-align:center;">
DPRA
</td>
<td style="text-align:center;">
Positive
</td>
<td style="text-align:center;">
Positive
</td>
<td style="text-align:center;">
1B
</td>
</tr>
<tr>
<td style="text-align:center;">
DPRA
</td>
<td style="text-align:center;">
Negative
</td>
<td style="text-align:center;">
Negative
</td>
<td style="text-align:center;">
NC
</td>
</tr>
</tbody>
</table>

# Step 2: Select Data Columns for Predictions

After clicking *Done*, the panel for column selection will expand. All
assay endpoints that are needed to apply the selected DAs will be shown.
Dropdown menus under each assay endpoint contain the column names from
the uploaded data. Use the dropdown menus to select the names of the
columns corresponding to each given assay endpoint. Each column can only
be selected once. Click *Done* to evaluate the values in the column for
proper formatting.

Columns must be formatted correctly to ensure an accurate prediction.
Descriptions of the column requirements are given below.

## DPRA

### % Depletion

DPRA %-Cysteine (%C) and %-Lysine (%K) depletion values are used in the
ITS DA. The columns for %C and %K depletion should contain only numeric
values. Numeric values should not have commas. Missing values should be
blank or labeled as “NA.” Any invalid values will be treated as missing
and will not be used to predict skin sensitization hazard.

The mean of %C and %K depletion for each chemical is used to score the
chemical using the scoring scheme shown in Table 1. Any negative %C or
%K depletion values are set to 0 when calculating the mean, as specified
in OECD Test Guideline 442c[10]. If there is no %C
depletion value for a given chemical, then the DPRA results can’t be
used for that chemical. If only the value for %K depletion is missing,
then the %C depletion values are used for scoring, with a different
scoring scheme as shown in Table 1.

### Hazard Identification

DPRA hazard identification is used in the 2o3 and KE 3/1 STS DAs. The
column for hazard identification should be an indicator for a positive
or negative outcome from DPRA. Positive outcomes must be indicated by
“p,” “pos,” “positive,” or 1. Negative outcomes must be indicated by
“n,” “neg,” “negative,” or 0. The values are not case sensitive. Missing
values should be blank or labeled as “NA.” Any invalid values will be
treated as missing and will not be used to predict skin sensitization
hazard.

Alternatively, the DPRA %C and %K depletion values can be provided and
the app will define the chemical hazard identifications as outlined in
OECD Test Guideline 442c [10].

## h-CLAT

### Hazard Identification

H-CLAT hazard identification is used in the 2o3 DA. The column for
h-CLAT hazard identification should be an indicator for a positive or
negative outcome from h-CLAT. Positive outcomes must be indicated by
“p,” “pos,” “positive,” or 1. Negative outcomes must be indicated by
“n,” “neg,” “negative,” or 0. The values are not case sensitive. Missing
values should be blank or labeled as “NA.” Any invalid values will be
treated as missing and will not be used to predict skin sensitization
hazard.

### Minimum Induction Threshold

The h-CLAT minimum induction threshold (MIT) is used in the ITS and KE
3/1 STS DAs. The column for h-CLAT MIT must contain either positive
numeric values for positive outcomes or “n,” “neg,” “negative,” or “Inf”
to indicate negative outcomes. The values are not case sensitive.
Missing values should be blank or labeled as “NA.” Any invalid values
will be treated as missing and will not be used to predict skin
sensitization hazard.

## KeratinoSens™ Hazard Identification

KeratinoSens™ (KS) hazard identification is used in the 2o3 DA. The
column for KS hazard identification should be an indicator for a
positive or negative outcome from the KS assay. Positive outcomes must
be indicated by “p,” “pos,” “positive,” or 1. Negative outcomes must be
indicated by “n,” “neg,” “negative,” or 0. The values are not case
sensitive. Missing values should be blank or labeled as “NA.” Any
invalid values will be treated as missing and will not be used to
predict skin sensitization hazard.

Alternatively, iMax values can be provided and evaluated for hazard
identification. The column corresponding to KS iMax should only contain
numeric values. Chemicals with KS iMax values ≥ 1.5 are labeled as
positive and chemicals with KS iMax values &lt;1.5 are labeled as
negative.

## In Silico Prediction

### Hazard Identification

*In silico* predictions should be derived from either [Derek
Nexus](https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm)
or the OECD QSAR Toolbox[13]. The column for
*in silico* hazard identification should be an indicator for a positive
or negative prediction where positive predictions are indicated by “p,”
“pos,” “positive,” or 1. Negative predictions must be indicated by “n,”
“neg,” “negative,” or 0. The values are not case sensitive. Missing
values should be blank or labeled as “NA.” Any invalid values will be
treated as missing and will not be used to predict skin sensitization
hazard.

### Applicability Domain

The applicability domain (AD) from the user’s chosen *in silico* tool
should be an indicator for whether the chemical is in the AD of the
tool’s models. A value of “In” or 1 indicates that the chemical is in
the AD. A value of “Out” or 0 indicates that the chemical is outside the
AD and the *in silico* prediction will not be used to predict skin
sensitization hazard. The values are not case sensitive. Missing values
should be blank or labeled as “NA.” Any invalid values will be treated
as missing and will not be used to predict skin sensitization hazard.

# Step 3: Review Selection

After clicking *Done* in *Step 2*, the selected columns will be
evaluated for proper formatting. The *Step 3* tab will display a table
with 3 columns. The “Variable” column contains the name of the assay
endpoint the app requested. The “Selected Column” column has the name of
the data column that was selected for the assay endpoint. Verify that
the selected columns are correct. If needed, return to *Step 2*, update
the selected columns and click *Done* to update the table in *Step 3*.
The “Flag” column will contain text describing the formatting
requirement that was violated. Review the format of any flagged columns.
Updates to the data format must be made externally, and the data will
need to be re-uploaded. After reviewing the selections, click *Run* to
run the DASS predictions using the columns shown in “Selected Column.”
If there are any unresolved flags, any invalid values will be marked as
missing and will not be used to predict skin sensitization hazard.

# Step 4: Results

The *Step 4* tab will display a table with the DASS predictions appended
to the uploaded data and highlighted in . If ITS was selected, then the
individual ITS scores are also appended and highlighted in .

The data columns that were selected in *Step 2* are highlighted in .
These data columns are reformatted for use in the DAs. The reformatted
columns are appended to the data and highlighted in . Table 4 contains a
description of the columns and how they were reformatted. It may be
useful to compare the selected columns and their transformations to
ensure that data were properly interpreted, especially if the DAs were
run with flagged data.

Click the *Download Results* button to download an excel workbook with
the results table.

<table>
<caption>
Table 4: Descriptions of columns in the results that contain the
reformatted user column data. These are the the values used in
evaluating skin sensitization with the DASS.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Column Name
</th>
<th style="text-align:left;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DPRA Hazard Id. Input
</td>
<td style="text-align:left;">
Positive hazard identifications are set to 1 and negative hazard
identifications are set to 0. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
DPRA %-C Depletion Input
</td>
<td style="text-align:left;">
Only numeric values are retained. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
DPRA %-K Depletion Input
</td>
<td style="text-align:left;">
Only numeric values are retained. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
DPRA Mean (Calculated)
</td>
<td style="text-align:left;">
The calculated mean of DPRA %-C and %-K depletion values, where any
negative values are set to 0 when calculating the mean. DPRA mean is
used when the ITS DA is selected or when DPRA hazard identification is
derived from depletion values.
</td>
</tr>
<tr>
<td style="text-align:left;">
DPRA Hazard Id. Input (Calculated)
</td>
<td style="text-align:left;">
The hazard identification derived from DPRA Mean (Calculated) when 2o3
or KE 3/1 STS DAs are selected and the user provides DPRA %-C and %-K
depletion columns instead of a DPRA hazard identification column.
</td>
</tr>
<tr>
<td style="text-align:left;">
h-CLAT Hazard Id. Input
</td>
<td style="text-align:left;">
Positive hazard identifications are set to 1 and negative hazard
identifications are set to 0. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
h-CLAT MIT Input
</td>
<td style="text-align:left;">
Numeric values for positive h-CLAT results are retained. In addition,
negative h-CLAT results are set to Inf. Any invalid values are set to
NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
Keratinosens(TM) iMax Input
</td>
<td style="text-align:left;">
Only numeric values are retained. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
Keratinosens(TM) Hazard Id. Input
</td>
<td style="text-align:left;">
Positive hazard identifications are set to 1 and negative hazard
identifications are set to 0. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
Keratinosens(TM) Hazard Id. Input (Calculated)
</td>
<td style="text-align:left;">
The hazard identification derived from KS(TM) iMax Input.
</td>
</tr>
<tr>
<td style="text-align:left;">
In Silico Hazard Id. Input
</td>
<td style="text-align:left;">
Positive hazard identifications are set to 1 and negative hazard
identifications are set to 0. Any invalid values are set to NA.
</td>
</tr>
<tr>
<td style="text-align:left;">
In Silico Applicability Domain Input
</td>
<td style="text-align:left;">
Predictions in the applicability domain are set to 1 and predictions
outside the applicability domain are set to 0. Any invalid values are
set to NA.
</td>
</tr>
</tbody>
</table>

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
