

# just simple code to move figures created in 01-wiwa-analysis.R
# to where they need to be for LaTeXing the document

#### DEFINE PATHS: You must do this for your own installation ####
figoutpath <- "../../git-overleaf-repos/gaiah-overleaf/figures/"
figinpath <- "outputs/figures"

#### NAME FIGS TO MOVE ####
paper_figs <- c("pmgcd_altogether.pdf", "pmgcd_boxplots.pdf", "rem_mig_dist_fig.pdf", "figure1.pdf")
paper_figs_full <- file.path(figinpath, paper_figs)


#### PDF CROP THINGS: This requires a system call to pdfcrop ####
call <- paste("cd ", figinpath, "; for i in ", paste(paper_figs, collapse = " "), "; do pdfcrop $i; done")
system(call)


#### THEN MOVE all the -crop.pdf things ####
call <- paste("mv ", figinpath,  "/*-crop.pdf ", figoutpath, sep = "")
system(call)
