#' -------------------
#' Code for converting exported bibtex file into format that:
#' 1) is split and make available for a listing page
#' by 2) putting each article into a .qmd file and
#' 3) the pdfs are copied into the relevant folder with the 
#' qmd file of each reference.
#' 
#' This is not perfect by any means, and was developed around
#' an export from Zotero. It might need some retooling for bibTeX
#' generated from other sources, but it was my hope to make it
#' easy to modify.
#' 
#' @title bibtex_2_website_qmds
#' @description import publications from a bibtex file to a quarto website
#' @author Jarrett Byrnes (2025) <jarrett.byrnes@umb.edu>
#' based on bibtex_2academic from 
#' Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' and  Peter Paul Pichler (2019) <pichler@pik-potsdam.de>
# http://www.pik-potsdam.de/~pichler/blog/post/set-this-up/setting-up-this-site/
#' -------------------


# load libraries  ---------------------------------------------------------

library(RefManageR)
library(dplyr)
library(tidyr)
library(stringr)
library(bib2df)
library(janitor)
library(glue)



#' Convert bibTeX to a set of qmds for a website
#'
#' @param bibfile The full path and filename, relative to the working 
#' directory of the .bib file.
#' @param pdf_files_folder_location Full path to where the directory
#' containing pdfs is located.
#' @param outfold The path to the folder where qmds and files will
#' be written out.
#' @param overwrite Should filed and folders be overwritten? 
#' Defaults to FALSE
#'

bibtex_2_website_qmds <- function(
    bibfile,
    pdf_files_folder_location,
    outfold,
    overwrite = FALSE
    ){
  print("Starting...")
  
# setup ---------------------------------------------------------
# strip trailing /
  pdf_files_folder_location <- gsub("\\/$", "", pdf_files_folder_location)
  outfold <- gsub("\\/$", "", outfold)
  
# where are the input files/folders relative to the toplevel .Rproj file
#bibfile <- "publications/better_bib_latex/JEKB_publications/JEKB_publications.bib"
#pdf_files_folder_location <- "publications/better_bibtex/JEKB_publications/"


# select destination folder -----------------------------------------------
# Identify the folder in which publications will be kept
#outfold <- "publications/articles"
outfold_foryaml <- gsub("^.*\\/", "\\.\\/", outfold)

#overwrite <- FALSE



# read bibtex -------------------------------------------------------------
print("Reading in file...")
# Import the bibtex file and convert to data.frame
mypubs <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
  as.data.frame() |>
  mutate(across(everything(), .fns = ~str_remove_all(., "[{}\"]")),
         across(everything(), .fns = ~str_replace_all(., "\\\\%", "%")),
         across(everything(), .fns = ~str_replace_all(., "\\\\&", "and"))
  )

##
# some more modification
##
print("Cleaning entries...")

# if there is no date, make it year
if(is.null(mypubs$date)) mypubs$date <- mypubs$year

# if there is no journaltitle, make it journal
if(is.null(mypubs$journaltitle)) mypubs$journaltitle <- mypubs$journal

mypubs <- mypubs |>
  # create a year field
  mutate(year = str_extract(date, "\\d{4}") |> as.numeric(),
         jrnl_short =  gsub(" ", "_", tolower(mypubs$journal))
         
  )

##
# something easier to work with
##

mypubs_to_print <- mypubs |>
  mutate(row_number = row_number()) |>
  select(
    row_number,
    title,
    year,
    jrnl_short,
    bibtype,
    author,
    journaltitle,
    volume,
    number,
    pages,
    doi,
    keywords,
    file,
    abstract,
    url
  )

##
# fix up null DOIs and some other bits for the next step
##
print("Preparing to write out...")

mypubs_to_print <- mypubs_to_print |>
  mutate(doi = ifelse(is.na(doi), row_number(), doi),
         pages = gsub("--", "-", pages),
         abstract = gsub("&gt;", ">", abstract),
         abstract = gsub("\\\\", "", abstract),
         # get rid of extra cruft from files to just have the file
         file = str_remove_all(file, "\\:application\\/pdf"),
         file = str_remove(file, ".*PDF\\:"),
         file = str_remove(file, ".*Submitted Version\\:"),
         file = str_replace(file, "\\.pdf.*$", "\\.pdf"),
         #file = str_replace(file, ".*\\:files", "files"),
         filename = str_remove_all(file, ".*\\/")
  )

##
# fix up authors and a few more bits that are new
##
make_auth <- function(one_auth){
  auth_hugo <- str_replace_all(one_auth, " and ", "\", \"")
  auth_hugo <- stringi::stri_trans_general(auth_hugo, "latin-ascii")
  auth_hugo <- paste0("[\"", auth_hugo, "\"]")
  return(auth_hugo)
}

mypubs_to_print <- mypubs_to_print |> 
  mutate(author = make_auth(author),
         foldername = paste(journaltitle, year, doi, sep = "_"),
         foldername = gsub("\\/", "_", foldername))


# iterate over all lines and print ---------------------------------------------------------
# I've become fond of for loops again lately
print("Writing out qmds...")
for(i in 1:nrow(mypubs_to_print)){
  create_qmd(mypubs_to_print[i,], 
             overwrite = overwrite,
             outfold = outfold,
             outfold_foryaml = outfold_foryaml,
             pdf_files_folder_location = pdf_files_folder_location,
             mypubs = mypubs)
}

print("Done.")

}


## 
# the function
##

create_qmd <- function(x, 
                       overwrite = FALSE,
                       outfold,
                       outfold_foryaml,
                       pdf_files_folder_location,
                       mypubs){
  #  x <- as.data.frame(x)
  print(x$title)
  
  # if a folder does not exist, make it
  if (!dir.exists(glue("{outfold}/{x$foldername}/"))){
    dir.create(glue("{outfold}/{x$foldername}/"))
  }
  
  
  yaml <- glue(
    '---
    title: "{x$title}"
    date: "{x$year}"
    year: "{x$year}"
    author: {x$author}
    publication: "{x$journaltitle}"
    volume: "{x$volume}"
    number: "{x$number}"
    pages: "{x$pages}"
    doi: "{x$doi}"
    abstract: "{x$abstract}"
    keywords: "{x$keywords}"
    url: "{x$url}"
    image: featured.png    
    url_preprint: ""
    url_code: ""
    url_dataset: ""
    bib: "{outfold_foryaml}/{x$foldername}/cite.bib"
    pdf: "{outfold_foryaml}/{x$foldername}/{x$filename}"
    ---
    '
  )
  
  # write out the above text string
  if(file.exists(glue("{outfold}/{x$foldername}/index.qmd"))) {
    if(!overwrite){
      stop(glue("{outfold}/{x$foldername}/index.qmd exists!\nUse overwrite = TRUE if you are OK with overwriting."))
    }
  }
  sink(file = glue("{outfold}/{x$foldername}/index.qmd"), 
       append = FALSE)
  cat(yaml)
  sink()
  
  # move the pdf
  # file.copy was being weird so I just went with cp and its overwrite option
  # -n for do not overwrite
  ov <- ""
  if(!overwrite) ov <- "-n"
  system(glue("cp '{pdf_files_folder_location}/{x$file}' '{outfold}/{x$foldername}/' {ov}"))
  #file.copy(glue("'{pdf_files_folder_location}/{x$file}'"), glue("{outfold}/{x$foldername}/"),
  #          overwrite = overwrite)
  
  # write out .bib file
  WriteBib( as.BibEntry(mypubs[x$row_number,]), 
            glue("{outfold}/{x$foldername}/cite.bib")
  )
  
  
}