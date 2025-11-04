# Load the functions
source("publications/_scripts/bibtex_2_website_qmds.R")


# Run and format library
bibtex_2_website_qmds(
    bibfile = "publications/zotero_bibtex_output/bibtex_2_website_qmds/bibtex_2_website_qmds.bib",
    pdf_files_folder_location = "publications/zotero_bibtex_output/bibtex_2_website_qmds/",
    outfold = "publications/articles/",
    overwrite = TRUE
)

