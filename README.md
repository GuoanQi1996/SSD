# Substitution Segment Detection (SSD)
Substitution segment detection (SSD) is designed to accurately detect the substitution segments in chromosome segments substitution line (CSSL). This work is similar to what is better known in the field as "detection of introgression in the genome of one species from the genome of another species", but differs in that the detection of introgression is geared towards the natural population of the species being studied,  _i.e._ people need to generate a population of species A and a population of species B to get the introgression segments in one species from the one another, usually by [rIBD method](https://www.nature.com/articles/ncomms5392) or [ABBA-BABA method](https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1000550), whereas the CSSL is usually line-specific. Nevertheless, CSSL has well-defined parents and a clear breeding process, therefore, we are able to accurately define the origin of the genomic segments in CSSL based on the rules governing allele transmission across generations.

[![DOI - 10.1111/pbi.14436](https://img.shields.io/badge/DOI-10.1111%2Fpbi.14436-blue)](https://onlinelibrary.wiley.com/doi/full/10.1111/pbi.14436)
[![Maintained - Cotton Presicion Breeding Academy](https://img.shields.io/badge/Maintained-Cotton_Presicion_Breeding_Academy-green)](http://cotton.zju.edu.cn/)

## Installation
SSD is a R script tool. Please make sure the `R` is available in the OS. Directly download the repository and the run `SSD.R` by `Rscript`.
```bash
Rscript SSD.R
```
or 
```bash
Rscript SSD.R --help
```
The packages `data.table`, `dplyr`, `ggplot2`, `Rcpp` and `stats` are required for the script, `SSD` will automatically detect the dependencies and install an unavailable packages. The users can also install the required packages manually if the network is not connected.

## Input
SSD require variant-major additive component file (TRAW) as the input format, this format can be generated from the standard Variant Call Format (VCF) by [PLINK](https://www.cog-genomics.org/plink/) or [PLINK2](https://www.cog-genomics.org/plink/2.0/). Using the exampled file `example.vcf.gz`, we can get `example.traw` by the following command. 
```bash
# PLINK 1.9
gzip -d -k example.vcf.gz
plink --vcf example.vcf.gz --allow-extra-chr --recode A-transpose --out example
# PLINK 2.0 (recommanded, fast and easy)
plink2 --vcf example.vcf.gz --allow-extra-chr --export Av --out example
```
After get the `TRAW` file, the next we should usually do is to confirm the coding of sample names in it. In `PLINK` and `PLINK2` the coding method is slightly different in generating `TRAW` header, but the way we confirm it is easy, we just need to open the `example.traw`, or if the file is too large:
```bash
head example.traw
```
Then we should be aware of the names highlighted, sometimes it may be `HY2603_HY2603`, `HY2631_HY2631`, depends on whether `PLINK` or `PLINK2` is used.

![6135e3b41287a9fd9a03b6577375a82](https://github.com/user-attachments/assets/45257499-1c52-420b-ab9d-423029e44dae)

## Quick start
```bash
# basic 
Rscript SSD.R --tfile example.traw --ap 0_TM-1 --dp 0_3-79 --out example
# decrease the window size (defaulted 1 Mb), will increase the possibility of substitution detected (more sensitive), with more false positive
Rscript SSD.R --tfile example.traw --ap 0_TM-1 --dp 0_3-79 --window 500000 --step 20000 --out example
# conduct adjustment procedure for those regions with initial substitution proportion lower than 0.5 (defaulted 0.7) by --adj, this command is related to the error tolerance --err.
# the lower the --adj, the higher the --err, the more substitution would be detected (with more false positive)
Rscript SSD.R --tfile example.traw --ap 0_TM-1 --dp 0_3-79 --window 500000 --step 20000 --adj 0.5 --err 0.1 --out example
# change the size of output figures
Rscript SSD.R --tfile example.traw --ap 0_TM-1 --dp 0_3-79 --window 500000 --step 20000 --adj 0.5 --err 0.1 --width 8 --height 4 --out example
```
## Parameters
| Paramater | Description | Defaulted | Type |
| --- | --- | --- | --- |
| --tfile | TRAW file contain genotypes of CSSLs and their recurrent/donor parent lines | - | Mandatory | 
| --ap | ID of recurrent/adaptive parental line (in TRAW's header) | - | Mandatory | 
| --dp | ID of donor parental line (in TRAW's header) | - | Mandatory | 
| --adj | Baselevel of adjustment for inferring the parental origin of ambiguous alleles (NLSA), genomic region with LSP lower than 0.8 but higher than the given value would start adjustment process | 0.7 | Optional |
| --err | Error rate tolerance of the adjustment process | 0.05 | Optional |
| --window | Window size (bp) of sliding window analysis for calculate the local substitution proportion (LSP) | 1000000 | Optional |
| --step | Step size (bp) of sliding window analysis for calculate the LSP | 20000 | Optional |
| --width | Width for the output figure | 16 | Optional |
| --height | Height for the output figure | 16 | Optional |
| --out | Prefix for the names of output files | SSD | Optional |
| --help | Print the help message for SSD uasge | - | - |

## Output
| File | Description |
| --- | --- |
| *.blockwise.intro | Test statistics for each genomic region tested |
| *.merged.intro | Substituion segments detected in the current CSSL | 
| *.png | Figure shows the Substituion segments detected in the current CSSL |

## Citation
If you find this tools useful, please cite
```
Qi, G., Si, Z., Xuan, L., Han, Z., Hu, Y., Fang, L., Dai, F. and Zhang, T. (2024), Unravelling the genetic basis and regulation networks related to fibre quality improvement using chromosome segment substitution lines in cotton. Plant Biotechnology Journal (2024) **22** _pp._ 3135-3150, https://doi.org/10.1111/pbi.14436
```
## Issue
Guo-An Qi (guoan.qi@foxmail.com)
