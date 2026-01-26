#------- test new functions
devtools::load_all()
print_flextable(mtcars[1:10, 1:3], num_col = 2, rowname_label = 'car')

print_kable(mtcars[1:10, 1:3], num_col = 2, rowname_label = 'car')
print_kable(mtcars[1:10, 1:3], num_col = 2, rowname_label = 'car',format='latex')

#save(df,new_df,file='df.Rdata')
load('df.Rdata')
kbl(new_df,row.names = FALSE,format = 'latex')%>%
  kableExtra::kable_classic(full_width = F) %>%
  kableExtra::kable_styling(position = 'center',
                            font_size = fontsize)
kbl(df,row.names = FALSE)%>%
  kableExtra::kable_classic(full_width = F) %>%
  kableExtra::kable_styling(position = 'center',
                            font_size = fontsize,
                            full_width = FALSE) %>%
  kableExtra::row_spec(0, font_size = fontsize+2)
print_kable(df,format = 'html',num_col = 2)
#------
file.edit('.gitignore')
file.edit('.github/workflows/pkgdown.yaml')
file.edit('.github/workflows/Release.yml')
file.edit('.github/workflows/R-CMD-check.yml')
file.edit('_pkgdown.yml')
file.edit('NAMESPACE')
file.edit('DESCRIPTION')
file.edit('README.md')
file.edit('NEWS.Rmd')
#----- built website
# pkgdown::build_favicons() # run once when you have your man/figures/logo.png
library(pkgdown)
library(roxygen2)
roxygenise(clean = TRUE)
#build_home()
#build_reference()
build_site()
preview_site()

# new version  ------
rmarkdown::render("NEWS.Rmd", output_file = "NEWS.md")
toolkit::git_commit_push("New version 1.1.3")
toolkit::git_tag_push('1.1.3')


## Ensure Your Working Directory is Up-to-Date
system("git add .")
system("git commit -m 'Release version 1.1.4'")
system("git push")  # Pushes the latest changes to GitHub"
# Create Tab and push. ------
system("git tag -a v1.1.4 -m 'Release version 1.1.4'")
system("git push origin v1.1.4")

#  ----  buid pdf manual
library(roxygen2)
devtools::build_manual()
