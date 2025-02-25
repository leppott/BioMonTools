# Helper functions so can repeat code without repeating the code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com
# 2023-11-06
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_results <- function() {
# Remove results folder contents
# Create subfolders
# Copy input file  
  
  # Remove all files in "Results" folder
  # Triggered here so can run different files
  fn_results <- list.files(path_results
                           , full.names = TRUE
                           , include.dirs = TRUE
                           , recursive = TRUE)
  message(paste0("Files and folders in 'results' folder (before removal) = "
                 , length(fn_results)))
  # file.remove(fn_results) # ok if no files and only files
  unlink(fn_results, recursive = TRUE) # includes directories
  # QC, repeat 
  fn_results2 <- list.files(path_results
                            , full.names = TRUE
                            , include.dirs = TRUE
                            , recursive = TRUE)
  message(paste0("Files in 'results' folder (after removal [should be 0]) = "
                 , length(fn_results2))) 
  
}## clean_results

#' @param import_file Imported file, input$fn_input
#
copy_import_file <- function(import_file) {
  
  # result folder and files
  path_results_sub <- file.path(path_results, dn_files_input)
  
  # Add "Results" sub folder if missing
  boo_Results <- dir.exists(file.path(path_results_sub))
  if (boo_Results == FALSE) {
    dir.create(file.path(path_results_sub))
  }
  
  # Copy to "Results" sub-folder - Import "as is"
  file.copy(import_file$datapath, file.path(path_results_sub
                                               , import_file$name))
  
}## copy_import_file


#~~~~~~~~~~~
# 2023-11-08
# build report tables
# Use data frame of information to build from other files
#' @param df_template_data data frame of template data
#' @param fld_name_orig Column name in data file
#' @param fld_name_disp Column name to use in report
#' @param fld_desc Column description
#' @param fld_incl
#' @param fld_folder
#' @param fld_file
#' @param fld_color
#' @param fld_sort
#' @param path_files list.files of files from imported zip file
#' @param tbl_name Name of data table; used in meta data output
#' 
#' @return A list of two elements is returned; 1 is the data table
#' and 2 is the meta data (from the template file).

# Default names so don't have to include but can change if needed

build_report_table <- function(df_template_data
                               , fld_name_orig = "original name"
                               , fld_name_disp = "display name"
                               , fld_desc = "descriptor"
                               , fld_incl = "inclusion"
                               , fld_folder = "source folder"
                               , fld_file = "source file (or suffix)"
                               , fld_colr = "color code"
                               , fld_sort = "sort"
                               , path_files = file.path("results", "_user_input")
                               , tbl_name = NA
                               ) {
  

  boo_QC <- FALSE
  
  if (boo_QC) {
    setwd("C:/Users/Erik.Leppo/OneDrive - Tetra Tech, Inc/MyDocs_OneDrive/GitHub/BCGcalc/inst/shiny-examples/BCGcalc")
    path_results_user <- file.path(path_results, dn_files_input)
    #
    path_files = file.path("results", "_user_input")
    # tbl_name <- "topindicator"
    tbl_name <- "taxatrans"
    #
    # copy from clipboard
    df_template_data <- read.delim("clipboard")
    names(df_template_data) <- c("original name"
                                 , "display name"
                                 , "descriptor"
                                 , "inclusion"
                                 , "source folder"
                                 , "source file (or suffix)"
                                 , "color code"
                                 , "sort"
                                 , "notes")
    fld_name_orig    <- names(df_template_data)[1]
    fld_name_disp    <- names(df_template_data)[2]
    fld_desc         <- names(df_template_data)[3]
    fld_incl         <- names(df_template_data)[4]
    fld_folder       <- names(df_template_data)[5]
    fld_file         <- names(df_template_data)[6]
    fld_colr         <- names(df_template_data)[7]
    fld_sort         <- names(df_template_data)[8]
  }## boo_QC
  
  # status
  msg <- paste0("build report table; ", tbl_name)
  message(msg)
  
  # tibble to data frame
  df_template_data <- as.data.frame(df_template_data)
  
  #QC, Incl to lower case
  df_template_data[, fld_incl] <- tolower(df_template_data[, fld_incl])
  
  # QC, blank display name to original
  df_template_data[, fld_name_disp] <- ifelse(is.na(df_template_data[, fld_name_disp])
                                              , df_template_data[, fld_name_orig]
                                              , df_template_data[, fld_name_disp])
  
  # Check for file(s) and Loop through
  # combine folder and file
  df_import_files <- unique(df_template_data[, c(fld_folder, fld_file)])
  # add incl
  df_import_files_req <- unique(df_template_data[df_template_data[, fld_incl] == "required", c(fld_incl, fld_folder, fld_file)])
  # if all optional
  if (nrow(df_import_files_req) == 0) {
    # No required, all optional
    df_import_files[, fld_incl] <- "optional"
  } else {
    # Combine
    df_import_files <- merge(df_import_files, df_import_files_req)
    df_import_files[is.na(df_import_files[, fld_incl]), ] <- "optional"
  }## IF ~ nrow(df_import_files_req)
  # Show
  df_import_files
  
  #

  # Loop, Verify Files ----
  fld_csv <- "csv"
  fld_exists <- "exists"
  fld_path <- "path"
  pat_csv <- "\\.csv$"
  df_import_files[, fld_csv] <- grepl(pat_csv, df_import_files[, fld_file])
  
  #f <- 2 # QC
  for (f in seq_len(nrow(df_import_files))) {
    f_file <- df_import_files[f, fld_file, TRUE]
    f_csv <- df_import_files[f, fld_csv, TRUE]
    f_dir <- df_import_files[f, fld_folder, TRUE]
    f_req <- df_import_files[f, fld_incl, TRUE]

    if (f_csv) {
      if (is.null(f_dir) 
          | is.na(f_dir) 
          | f_dir == "NA" 
          | f_dir == tolower("root")) {
        # 
        path_f <- f_file
      } else {
        path_f <- paste0(f_dir, "/", f_file)
      }## IF ~ f_dir
      #
      # update table
      df_import_files[f, fld_path] <- path_f
      df_import_files[f, fld_exists] <- file.exists(file.path(path_files, path_f))
      #
    } else {
      # 
      f_pat <- paste0(f_file, ".csv$")
      path_dir_f <- file.path(path_files, f_dir)
      # find
      pot_match <- list.files(path_dir_f, f_pat)
      #
      if (length(pot_match) > 1 & f_req == TRUE) {
        # non-unique name
        # end process with pop up
        sa_title <- paste0("Report, QC Source Data; ", f_file)
        msg <- paste0("More than one version of file!") 
        shinyalert::shinyalert(title = sa_title
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        shiny::validate(msg)
      }## IF ~ length(pot_match)
      #
      df_import_files[f, "path"] <- paste0(f_dir, "/", pot_match)
      df_import_files[f, fld_exists] <- file.exists(file.path(path_files, df_import_files[f, "path"]))
    }## IF ~ f_csv
  }## FOR ~ f
  
  # show
  df_import_files
  
  
  ## primary key
  pk_orig <- df_template_data[1, fld_name_orig, TRUE]
  pk_disp <- df_template_data[1, fld_name_disp, TRUE]
  
  # Loop, Fields and Merge----
  # i <- 1 # QC
  for (i in seq_len(nrow(df_import_files))) {
    
    i_exists <- df_import_files[i, fld_exists, TRUE]
    if (i_exists == FALSE) {
      next
    }## IF ~ i_exists
    
    i_file <- df_import_files[i, fld_file, TRUE]
    i_dir  <- df_import_files[i, fld_folder, TRUE]
    i_path <- df_import_files[i, fld_path, TRUE]
    
    # Shiny Alert Title
    sa_title <- paste0("Report, QC Source Data; ", i_file)
    
    # import
    # use read_csv to maintain full column names
    #df_i_orig <- as.data.frame(readr::read_csv(file.path(path_files, i_path)))

    # Exception for BCG_Attr
  
    ## BCG_Attr, import
    ## Quick hack, get message about cols not matching
    df_i_orig <- as.data.frame(readr::read_csv(file.path(path_files, i_path)
                                       , col_types = readr::cols(.default = "?"
                                                            , BCG_Attr = "c"
                                                            , BCG_ATTR = "c")))
    ## BCG_Attr, prefix
    ## Need to prefix BCG_Attr so output in Excel won't be converted.
    boo_BCG_Attr <- tolower("BCG_Attr") %in% tolower(names(df_i_orig))
    if (boo_BCG_Attr) {
      col_BCG_Attr <- match(tolower("BCG_Attr"), tolower(names(df_i_orig)))
      df_i_orig[, col_BCG_Attr] <- paste0("'", df_i_orig[, col_BCG_Attr])
    }## IF ~ boo_BCG_Attr
    

    ## i, all
    i_template <- df_template_data[df_template_data[, fld_file] == i_file, ]
    
    ## i, fields
    i_fld_orig <- i_template[, fld_name_orig]
    i_fld_disp <- i_template[, fld_name_disp]
    i_incl <- i_template[, fld_incl, TRUE]
    
    # QC, PK
    pk_i_boo <- tolower(pk_orig) %in% tolower(names(df_i_orig))
    
    # QC, Shiny Alert, Primary Key
    # Shiny Alert
    if (!pk_i_boo) {
      # end process with pop up
      msg <- paste0("'Primary Key' missing!\n"
                    , pk_orig) 
      shinyalert::shinyalert(title = sa_title
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      shiny::validate(msg)
    }## IF ~ req fields
    
    # identify PK
    pk_i <- names(df_i_orig)[tolower(names(df_i_orig)) %in% tolower(pk_orig)]
    
    # Swap PK if different
    if (pk_i != pk_orig) {
      i_fld_orig[tolower(i_fld_orig) %in% tolower(pk_orig) == TRUE] <- pk_i
    }## IF ~ pk_i != pk_orig
    
    # Add PK after file 1
    ## Only the first file has the PK listed in the template
    if (i != 1) {
      i_fld_orig <- c(i_fld_orig, pk_i)
      i_fld_disp <- c(i_fld_disp, pk_disp)
      i_incl <- c(i_incl, "required")
    }## IF ~ i != 1 ~ Add PK to field list
     
    #QC 
    ## if any display names blank then use original name
    i_fld_disp_na <- is.na(i_fld_disp)
    i_fld_disp_blank <- i_fld_disp == ""
    i_fld_disp[i_fld_disp_na] <- i_fld_orig[i_fld_disp_na]
    i_fld_disp[i_fld_disp_blank] <- i_fld_orig[i_fld_disp_blank]
    
    # Inclusion
    i_fld_inc <- i_incl
    i_fld_req <- i_fld_orig[i_fld_inc == "required"]
    i_fld_req_missing <- i_fld_orig[!i_fld_req %in% names(df_i_orig)]
    
    # QC, Shiny Alert, Required fields
    if (length(i_fld_req_missing) > 0) {
        # end process with pop up
        msg <- paste0("'Required' column name(s) missing!\n"
                      , paste0(i_fld_req_missing, collapse = "\n")) 
        shinyalert::shinyalert(title = sa_title
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        shiny::validate(msg)
    }## IF ~ req fields
    
    # Check for columns
    i_fld_orig_present <- i_fld_orig[i_fld_orig %in% names(df_i_orig)]
    i_fld_disp_present <- i_fld_disp[i_fld_orig %in% names(df_i_orig)]
    
    # data
    df_i_disp <- df_i_orig[, i_fld_orig_present]
    # rename columns
    names(df_i_disp) <- i_fld_disp_present
    
    # merge files
    ## probably need to do up front in separate loop
    if (i == 1) {
      df_results <- df_i_disp
    } else {
      df_results <- merge(df_results, df_i_disp, by = pk_disp)
    }## IF ~ i == 1
    
  }## FOR ~ i
  
  # Redo order of columns to match template
  col_template_result <- df_template_data[, fld_name_disp]
  col_result_order <- col_template_result[col_template_result %in% names(df_results)]
  df_results <- df_results[, col_result_order]
  
  # meta data
  col_table <- rep(tbl_name, nrow(df_template_data))
  col_meta <- c(fld_name_disp, fld_desc, fld_incl, fld_colr, fld_sort)
  df_meta <- cbind(col_table
                   , df_template_data[, col_meta])
  names(df_meta)[1] <- "table"

  # Sort
  if (sum(!is.na(df_meta[, fld_sort])) > 0) {
    df_sort <- df_meta[!is.na(df_meta[, fld_sort]), c(fld_name_disp, fld_sort)]
    df_sort <- df_sort[order(df_sort[, fld_sort]), ]
    col_sort_order <- df_sort[, fld_name_disp]
    # apply sort order
    #message(nrow(df_results))
   # df_results <- df_results[order(df_results[, col_sort_order]), ]
    #df_results <- df_results[with(df_results, order(col_sort_order)), ]
    # df_results <- dplyr::arrange(df_results, {{col_sort_order}})
    #df_results <- df_results[do.call(what = order, args = df_results[, col_sort_order]), ]
   # df_results <- dplyr::arrange(df_results, .dots = col_sort_order)
    df_results <- dplyr::arrange(df_results, dplyr::across(dplyr::all_of(col_sort_order)))
    #message(nrow(df_results))
  }## IF ~ sort
  
  # RESULTS ----
  ls_results <- vector(mode = "list", length = 2)
  ls_results$data <- df_results
  ls_results$meta <- df_meta
  return(ls_results)
  
}## build_report_table

