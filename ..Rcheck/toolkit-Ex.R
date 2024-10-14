pkgname <- "toolkit"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('toolkit')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cleanLatex")
### * cleanLatex

flush(stderr()); flush(stdout())

### Name: cleanLatex
### Title: Remove LaTeX Auxiliary Files
### Aliases: cleanLatex

### ** Examples

## Not run: 
##D # Clean the current working directory
##D cleanLatex()
##D 
##D # Clean a specific directory
##D cleanLatex("/path/to/latex/files", verbose = TRUE)
## End(Not run)



cleanEx()
nameEx("combine_table")
### * combine_table

flush(stderr()); flush(stdout())

### Name: combine_table
### Title: Combine Two Tables with different statistics.
### Aliases: combine_table

### ** Examples

tab1<-margin.table(Titanic,c(1,4))
tab2 <- round(prop.table(tab1, margin = 1) * 100, 2)
combine_table(tab1,tab2,)




cleanEx()
nameEx("commit_and_tag")
### * commit_and_tag

flush(stderr()); flush(stdout())

### Name: commit_and_tag
### Title: Commit Changes and Tag Version in Git
### Aliases: commit_and_tag

### ** Examples

## Not run: 
##D   commit_and_tag("1.0.2")
## End(Not run)




cleanEx()
nameEx("compare_categorical")
### * compare_categorical

flush(stderr()); flush(stdout())

### Name: compare_categorical
### Title: Perform Cross-Tabulation and Fisher's Exact Test
### Aliases: compare_categorical

### ** Examples

compare_categorical(mtcars, 'cyl', 'gear', varname = 'Gear')




cleanEx()
nameEx("compare_numerical")
### * compare_numerical

flush(stderr()); flush(stdout())

### Name: compare_numerical
### Title: Compute Mean and SD for Two Samples by Grouping Variable and
###   Perform One-Way ANOVA
### Aliases: compare_numerical

### ** Examples

compare_numerical(mtcars, 'cyl', 'mpg', varname = 'Miles per Gallon')




cleanEx()
nameEx("compile_handouts")
### * compile_handouts

flush(stderr()); flush(stdout())

### Name: compile_handouts
### Title: Create and Compile Handouts from a List of TeX Files
### Aliases: compile_handouts

### ** Examples

## Not run: 
##D   tex_files <- list.files(pattern = "^Sta.*\\.tex")
##D   compile_handouts(tex_files)
## End(Not run)




cleanEx()
nameEx("compile_rnws")
### * compile_rnws

flush(stderr()); flush(stdout())

### Name: compile_rnws
### Title: Knit Files to PDF and Track Status
### Aliases: compile_rnws

### ** Examples

## Not run: 
##D   file_list = list.files(pattern = "^Sta.*Rnw", ignore.case = TRUE)
##D   knit_results <- compile_rnws(file_list)
##D   print(knit_results)
## End(Not run)




cleanEx()
nameEx("compile_texs")
### * compile_texs

flush(stderr()); flush(stdout())

### Name: compile_texs
### Title: Compile LaTeX Files
### Aliases: compile_texs

### ** Examples

## Not run: 
##D   compile_latex_files()
##D   compile_latex_files(c("file1.tex", "file2.tex"))
## End(Not run)




cleanEx()
nameEx("count_percent")
### * count_percent

flush(stderr()); flush(stdout())

### Name: count_percent
### Title: Calculate Counts and Percentages of Factor Levels
### Aliases: count_percent

### ** Examples

levels= LETTERS[1:3]
factor1 <- levels%>%sample(size = 10, replace = TRUE)%>%factor()
count_percent(factor1,1)




cleanEx()
nameEx("cut_txt")
### * cut_txt

flush(stderr()); flush(stdout())

### Name: cut_txt
### Title: Cut text file into chunks with specified number of chapters
### Aliases: cut_txt

### ** Examples

## Not run: 
##D cut_txt(file = "your_file.txt", marker = "第.*章", num_chapters = 25)
## End(Not run)




cleanEx()
nameEx("draw_normal")
### * draw_normal

flush(stderr()); flush(stdout())

### Name: draw_normal
### Title: Draw a normal distribution plot using ggplot2
### Aliases: draw_normal

### ** Examples

draw_normal(mu = 0, sd = 1, color_density = "red")




cleanEx()
nameEx("fun_links")
### * fun_links

flush(stderr()); flush(stdout())

### Name: fun_links
### Title: Generate Links to Function Documentation
### Aliases: fun_links

### ** Examples

function_list <- c("compile_rnws", "cleanFolder", "knit_rnws")
result <- fun_links(function_list, package_name = "toolkit")
cat(result)



cleanEx()
nameEx("meanSD")
### * meanSD

flush(stderr()); flush(stdout())

### Name: meanSD
### Title: Calculate Mean and Standard Deviation, and Format the Output
### Aliases: meanSD

### ** Examples

meanSD(c(10.5, 5.3, 7.8), digits = 3)



cleanEx()
nameEx("plot_coef")
### * plot_coef

flush(stderr()); flush(stdout())

### Name: plot_coef
### Title: Plot Regression Coefficients
### Aliases: plot_coef

### ** Examples

# Example usage:
fit<-lm(mpg~.,data=mtcars)
coef(fit)%>%plot_coef(remove_intercept = TRUE)




cleanEx()
nameEx("print_table")
### * print_table

flush(stderr()); flush(stdout())

### Name: print_table
### Title: Print a Styled Table for HTML or LaTeX Output
### Aliases: print_table

### ** Examples

print_table(mtcars[1:10, 1:3], num_col = 2, rowname = 'car')




cleanEx()
nameEx("print_table_stripe")
### * print_table_stripe

flush(stderr()); flush(stdout())

### Name: print_table_stripe
### Title: Print a styled LaTeX table with alternating row colors
### Aliases: print_table_stripe

### ** Examples

print_table_stripe(mtcars[1:5, 1:3])




cleanEx()
nameEx("rmd_hook_set")
### * rmd_hook_set

flush(stderr()); flush(stdout())

### Name: rmd_hook_set
### Title: Set custom knit hooks for R Markdown
### Aliases: rmd_hook_set

### ** Examples

rmd_hook_set()




cleanEx()
nameEx("rmd_opts_set")
### * rmd_opts_set

flush(stderr()); flush(stdout())

### Name: rmd_opts_set
### Title: Set default options for code chunks in R Markdown
### Aliases: rmd_opts_set

### ** Examples

rmd_opts_set()




cleanEx()
nameEx("set_ggplot_theme")
### * set_ggplot_theme

flush(stderr()); flush(stdout())

### Name: set_ggplot_theme
### Title: Set custom theme for ggplot2 plots
### Aliases: set_ggplot_theme

### ** Examples

set_ggplot_theme()




cleanEx()
nameEx("summarize_plots")
### * summarize_plots

flush(stderr()); flush(stdout())

### Name: summarize_plots
### Title: Summarize and Plot Columns of a Data Frame
### Aliases: summarize_plots

### ** Examples

# Example with the mtcars dataset
summarize_plots(mtcars)




cleanEx()
nameEx("summarize_table")
### * summarize_table

flush(stderr()); flush(stdout())

### Name: summarize_table
### Title: Summarize Data Frame by Column Type
### Aliases: summarize_table

### ** Examples

summarize_table(mtcars)




cleanEx()
nameEx("zkbl")
### * zkbl

flush(stderr()); flush(stdout())

### Name: zkbl
### Title: Format and style a table for LaTeX output
### Aliases: zkbl

### ** Examples

zkbl(mtcars[1:5,])




cleanEx()
nameEx("zpvalue")
### * zpvalue

flush(stderr()); flush(stdout())

### Name: zpvalue
### Title: Format p-value for Display
### Aliases: zpvalue

### ** Examples

# Example usage of the function:
zpvalue(0.005)
zpvalue(0.0005)




cleanEx()
nameEx("zrange")
### * zrange

flush(stderr()); flush(stdout())

### Name: zrange
### Title: Calculate and Format Range of a Numeric Vector
### Aliases: zrange

### ** Examples

zrange(c(1.234, 5.678, 9.012))
zrange(c(10, 20, 30, 40, 50), digits = 1)




cleanEx()
nameEx("zround")
### * zround

flush(stderr()); flush(stdout())

### Name: zround
### Title: Round and Format Numbers to Specific Decimal Places
### Aliases: zround

### ** Examples

x = c(5.555, 1.115, -0.002)
zround(x, method = 1)
zround(x, method = 2)
formatC(x, digits = 2, format = "f")
formattable::formattable(x, digits = 2, format = "f")




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
