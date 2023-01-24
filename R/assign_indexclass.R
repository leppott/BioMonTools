#' @title Assign Index_Class
#'
#' @description Assign Index_Class for based on user input fields.
#' If use the same name of an existing field the information will be
#' overwritten.
#'
#' Multiple criteria are treated as "AND" so all must be met to be assigned
#' to a particular Index_Class.
#'
#' Internally uses `tidyr` and `dplyr`
#'
#' If Index_Class is included in data then it is renamed Index_Class_Orig and
#' returned in the output data frame.
#'
#' @details Requires use of reference file with criteria.
#'
#' @param data Data frame (wide format) with metric values to be evaluated.
#' @param criteria  Data frame of metric thresholds to check.
#' @param name_indexclass Name for new Index_Class column.
#' Default = INDEX_CLASS
#' @param name_indexname Name for Index Name column.
#' Default = INDEX_NAME
#' @param name_siteid Name for Site ID column.
#' Default = SITEID
#' @param data_shape Shape of data; wide or long.  Default is 'wide'
#'
#' @return Returns a data frame with new column added.
#'
#' @examples
#' # Packages
#' library(readxl)
#'
#' # EXAMPLE 1
#' # Create Example Data
#' df_data <- data.frame(SITEID = paste0("Site_", LETTERS[1:10])
#'                      , INDEX_NAME = "BCG_MariNW_Bugs500ct"
#'                      , GRADIENT = round(runif(10, 0.5, 1.5), 1)
#'                      , ELEVATION = round(runif(10, 700, 800), 1))
#'
#' # Import Checks
#' df_criteria <- read_excel(system.file("extdata/IndexClass.xlsx"
#'                                           , package = "BioMonTools")
#'                           , sheet = "Index_Class")
#'
#' # Run Function
#' df_results <- assign_IndexClass(df_data, df_criteria, "INDEX_CLASS")
#'
#' # Results
#' df_results
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
assign_IndexClass <- function(data
                              , criteria
                              , name_indexclass = "INDEX_CLASS"
                              , name_indexname = "INDEX_NAME"
                              , name_siteid = "SITEID"
                              , data_shape = "WIDE") {
  #
  # global variable bindings ----
  #`%>%` <- dplyr::`%>%`

  # QC ----
  boo_DEBUG <- FALSE
  if(boo_DEBUG == TRUE){
    Ex <- 3
    if(Ex == 1) {
      data <- data.frame(SITEID = paste0("Site_", LETTERS[1:6])
                         , INDEX_NAME = "MBSS_2005_Bugs"
                         , ECO3 = c(63:67, 69))
      criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                 , package = "BioMonTools")
                                     , sheet = "Index_Class")
      # doesn't work for single field with multiple choices
      name_indexclass <- "INDEX_CLASS"
      name_indexname <- names(data)[2]
      name_siteid <- names(data)[1]
      data_shape <- "WIDE"
    } else if(Ex == 2) {
      data <- data.frame(SITEID = paste0("Site_", LETTERS[1:10])
                         , INDEX_NAME = "BCG_MariNW_Bugs500ct"
                         , GRADIENT = round(runif(10, 0.5, 1.5), 1)
                         , ELEVATION = round(runif(10, 700, 800), 1))
      criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                 , package = "BioMonTools")
                                     , sheet = "Index_Class")
      name_indexclass <- "INDEX_CLASS"
      name_indexname <- names(data)[2]
      name_siteid <- names(data)[1]
      data_shape <- "WIDE"
    } else if(Ex == 3) {
      badentry <- c(NA, "")
      data <- data.frame(stationcode = paste0("Site_", LETTERS[1:14])
                         , iname = "BCG_MariNW_Bugs500ct"
                         , GRADIENT = c(round(runif(10, 0.5, 1.5), 1)
                                        , badentry, 1, NA)
                         , ELEVATION = c(round(runif(10, 700, 800), 1)
                                         , badentry, NA, 700))
      criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                 , package = "BioMonTools")
                                     , sheet = "Index_Class")
      name_indexclass <- "iclass"
      name_indexname <- names(data)[2]
      name_siteid <- names(data)[1]
      data_shape <- "WIDE"
    }## IF ~ Ex
  }##IF~boo_DEBUG~END

  # Munge ----
  # to data frame (from Tibble)
  data <- as.data.frame(data)
  criteria <- as.data.frame(criteria)

  # Index_Class to Index_Class_Orig
  # AND keep copy of orig data
  if(name_indexclass %in% names(data)) {
    # rename IC to IC_Orig
    boo_IC <- names(data) %in% name_indexclass
    names(data)[boo_IC] <- paste0(name_indexclass, "_ORIG")
  }## IF ~ name_indexclass

  # retain original
  data_orig <- data

  # Names to Upper Case
  names(data) <- toupper(names(data))
  names(criteria) <- toupper(names(criteria))
  criteria[, "FIELD"] <- toupper(criteria[, "FIELD"])

  # Parameters to Upper Case
  name_indexclass_u <- toupper(name_indexclass)
  name_indexname_u  <- toupper(name_indexname)
  name_siteid_u     <- toupper(name_siteid)
  data_shape      <- toupper(data_shape)

  # Get matching criteria columns
  data_IndexName <- unique(data[, name_indexname_u])

  # Criteria Columns
  col_criteria <- unique(criteria[criteria$INDEX_NAME == data_IndexName
                                  , "FIELD"])

  # use complete cases to remove NA
  # "" still there

  # data to long
  if (data_shape == "WIDE") {
    data_long <- tidyr::pivot_longer(
                                   data[complete.cases(data[, col_criteria]), ]
                                   , cols = tidyr::all_of(col_criteria)
                                   , names_to = "Data_Criteria_Name"
                                   , values_to = "Data_Criteria_Value")
  } else if (data_shape == "LONG") {
    df_long <- data[complete.cases(data[, col_criteria]), ]
  } else {
    msg <- "data_shape must be specified as 'wide' or 'long'."
    stop(msg)
  }##IF.input.shape.END
  #

  # Remove ""
  data_long_good <- data_long[!data_long$Data_Criteria_Value == "", ]

  # Calc -----
  # merge metrics and checks
  df_merge <- merge(data_long_good
                    , criteria
                    , by.x = c(name_indexname_u, "Data_Criteria_Name")
                    , by.y = c("INDEX_NAME", "FIELD"))
  #
  # perform evaluation (adds Pass/Fail, default is NA)

  # >
  # <
  # >=
  # <=
  # ==
  # !=

  df_merge[, "EXPR"] <- eval(expression(paste(df_merge[, "Data_Criteria_Value"]
                                             , df_merge[, "SYMBOL"]
                                             , df_merge[, "VALUE"])))

  # y <- apply(df.merge$Expr, 1, function(x) eval(parse(text=x)))
  #
  # x <- "1 < 2"
  # eval(parse(text=x))

  # temporary (quick and dirty)
  for (i in seq_len(nrow(df_merge))){
    df_merge[i, "EVAL"] <- eval(parse(text = df_merge[i, "EXPR"]))
  }## FOR ~ i

  # apply?
  # deparse etc

  # may need to remove values that are null or ""
  # 2nd part below should take care of those without all evaluations
  # but in some cases the "eval" statement gets messed up with ""

  # summarize
  ## INDEX_CLASS from criteria
  df_eval <- dplyr::summarize(dplyr::group_by(df_merge
                                              , .data[[name_indexname_u]]
                                              , .data[[name_siteid_u]]
                                              , INDEX_CLASS)
                              , EVAL_SUM = sum(EVAL, na.rm=TRUE)
                              , EVAL_N = dplyr::n()
                              , EVAL_EVAL = EVAL_SUM == EVAL_N
                              , .groups = "drop_last")
  df_eval <- df_eval[df_eval$EVAL_EVAL == TRUE, ]

  # Above doesn't work for MBSS (single equals criteria)
  # but does for PacNW for 2 criteria for every index_class


  # QC check ----
  # number of assignments
  df_n_assign <- dplyr::summarize(dplyr::group_by(df_eval
                                                  , .data[[name_indexname_u]]
                                                  , .data[[name_siteid_u]])
                                  , N_CLASS = dplyr::n()
                                  , .groups = "drop_last")
  df_mult_assign <- df_n_assign[df_n_assign$N_CLASS > 1, ]

  if(nrow(df_n_assign) > nrow(data)) {
    msg <- "Some sites have multiple INDEX_CLASS assignments."
    message(msg)
  } else if (nrow(df_n_assign) < nrow(data)) {
    msg <- "Some sites have no INDEX_CLASS assignments."
    message(msg)
  }## IF ~ nrow

  # Results ----
  df_results <- merge(data_orig
                      , df_eval[, c(name_indexname_u
                                    ,  name_siteid_u
                                    , "INDEX_CLASS")]
                      , by.x = c(name_indexname, name_siteid)
                      , by.y = c(name_indexname_u, name_siteid_u)
                      , sort = FALSE
                      , all.x = TRUE)

  # retain original column order
  df_results <- df_results[, c(names(data_orig), "INDEX_CLASS")]
  # name index class per user parameter
  names(df_results)[ncol(df_results)] <- name_indexclass

  # create output
  return(df_results)
  #
}##FUNCTION.END
