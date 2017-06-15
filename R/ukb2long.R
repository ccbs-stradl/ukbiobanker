# Convert UKB wide format to long format

# Author: Mark Adams
# Date: 2 July 2015


# Given a data.frame of UKB data with an f.eid column and
# other columns of the same type (numeric, factor, character)
ukb_fields2long_type <- function(ukb) {

# check that there is an f.eid field
  if(!'f.eid' %in% names(ukb)) {
    stop('ukb data input needs an f.eid field')
  }

# besides f.eid field, other columns need to be the same type
  ukb_data <- ukb %>% dplyr::select(-f.eid)
  col_classes <- unlist(lapply(lapply(ukb_data, class), function(x) x[1]))
  if(length(unique(col_classes)) != 1) {
    stop('data inputs are of mixed type: ', paste(unique(col_classes), collapse=', '))
  }

# besides f.eid field, other columns need the form
 # f.1234.5.6
  col_names <- names(ukb_data)
  has_fpattern <- stringr::str_detect(col_names, 'f.[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
  if(!all(has_fpattern)) {
    stop('ukb data column names do not match f.1234.5.6 pattern: ', paste(col_names[!has_fpattern], collapse=', '))
  }

  ukb_long <- ukb %>%
  # list out every id, field on a separate row 
    tidyr::gather(field, value, -f.eid) %>%
  # remove id/field combos with no data
    dplyr::filter(!is.na(value)) %>%
  # split apart field, instance, array
    tidyr::separate(field,
             into=c('f', 'field', 'instance', 'array'),
             sep='\\.') %>%
  # merge field and array labels back togther as f1234_6
    tidyr::unite(ffield, f, field, sep='') %>%
    tidyr::unite(field_array, ffield, array, sep='_') %>%
  # spread field/array across different columns
    tidyr::spread(field_array, value) %>%
  # code instance as integer
    dplyr::mutate(instance=as.integer(instance))
  
    return(ukb_long)

}

# turn a single f.eid, f.1234.5.6 pair into long format
ukb_fields2long_single <- function(ukb) {

  # check that there are only 2 columns with the right name
  # formatting
  if(! ncol(ukb) == 2) {
    stop('ukb data should have only 2 columns, found ', ncol(ukb))
  }
  if(! 'f.eid' %in% names(ukb)) {
    stop('ukb data should have an f.eid column, found ', paste(names(ukb)))
  }
  has_fpattern <- stringr::str_detect(names(ukb), 'f.[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
  if(!any(has_fpattern)) {
    stop('ukb data should have data column named with pattern f.1234.5.6, found ', paste(names(ukb)))
  }
  
  # extract field, instance, array from data column name
  field_column <- names(ukb)[has_fpattern]
  if(length(field_column) > 1) {
          stop('ukb data should have only 1 field column, found ', paste(field_column))
  }
  # extract f.1234.5.6 into 1234, 5, 6
  fia <- stringr::str_match(field_column, 'f.([[:digit:]]+).([[:digit:]]+).([[:digit:]]+)')
  field <- fia[,2]
  instance <- as.numeric(fia[,3])
  farray <- fia[,4]

  # construct new data.frame with f.eid, instance, and f1234_6
  ukb_long <- data.frame(f.eid=ukb$f.eid, instance=instance, value=ukb[,field_column])
  
  # remove instances with missing values, which may be recreated
  # as NA during a merge with other columns
  #ukb_long_obs <- ukb_long %>% dplyr::filter(!is.na(value))

  names(ukb_long)[3] <- paste0('f', field, '_', farray)

  # remove rows where there was not an observations
  return(ukb_long)

}

#' Take in a UKB data.frame and return instances split across
#' rows
#'
#' UKB variables are formatted as f.FIELD.INSTANCE.ARRAY where
#' FIELD is the variable
#' INSTANCE is the assessment occasion
#' ARRAY are multiple responses to the variable assessed at
#' the same time.
#'
#' What we want to do is turn the data from
#'
#' f.eid f.1.0.0 f.1.1.0 
#' 1     4       5
#' 2     17      2
#'
#' to
#'
#' f.eid instance f.1.0
#' 1     0        4
#' 1     1        5
#' 2     0        17
#' 2     1        2
#'
#' Some care in interpeting what the 'instance' refers to as
#' different variables use different codings of instance:
#'
#'  1. Diet Questionnaires Cycle http://biobank.ctsu.ox.ac.uk/crystal/instance.cgi?id=1
#'  2. Assessment Centre Visit http://biobank.ctsu.ox.ac.uk/crystal/instance.cgi?id=2
#'
#' Instance coding 2 is more useful for most analyses
#' 0 =	Initial assessment visit (2006-2010) at which participants were recruited and consent given
#' 1 =	First repeat assessment visit (2012-13)
#' 2	Imaging visit (2014+)
#'
#' These can be matched with f.53 Date of attending assessment
#' center http://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=53
#' to determine exactly the length of time between instances
#' @export
#' @examples
#'   ukb_long <- ukb_fields2long(ukb)
ukb_fields2long <- function(ukb) {

  # check that there is an f.eid field
  if(!'f.eid' %in% names(ukb)) {
    stop('ukb data input needs an f.eid field')
  }

  # list of columns transformed to long format
  ukb_long_list <- list()

  # column names that match the f.1234.5.6 UKB pattern
  has_fpattern <- stringr::str_detect(names(ukb), 'f.[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
  field_cols <- names(ukb)[which(has_fpattern)]

  # go through each columns and turn it into long format
  for(f in field_cols) {
    
   # extract single field and participant ID
   ukb_col <- ukb[,c('f.eid', f)]

   # long format
   ukb_col_long <- ukb_fields2long_single(ukb_col)

   ukb_long_list <- c(ukb_long_list, list(ukb_col_long))

  }

  # merge all the long-format columns into a single data.frame
  # we can't merge or dplyr::full_join here because joining
  # across instances creates NAs in the merged data.frame
  # that later need to be filled by values. So instead
  # we can merge each instance separately, then row bind
  # across instances
  # get the instance that each long data.frame belongs to
  instances <- unlist(lapply(ukb_long_list, function(x) x$instance[1]))
 
 # for each unique instance, recursively merge all the 
 # associated data frames, then rbind the result
 ukb_instances <- lapply(unique(instances), function(inst) {
  ukb_long_instance_list <- ukb_long_list[which(instances %in% inst)]
  if(length(ukb_long_instance_list) == 1) {
    return(ukb_long_instance_list[[1]])
  } else { 
    # rows are all in same order, so bind columns
    ukbl <- do.call(cbind, ukb_long_instance_list)
    # remove rundandant f.eid and instance columns
    return(ukbl[unique(names(ukbl))])
  }})

 # use rbind.fill since results might not have the same number
 # of columns
 if(length(ukb_instances) == 1) {
         ukb_long <- ukb_instances[[1]]
 } else {
   ukb_long <- do.call(plyr::rbind.fill, ukb_instances)
 }

  
  # get the field_array columns (everything but f.eid and instance
  ukb_long_cols <- names(ukb_long)
  field_array <- stringr::str_match(ukb_long_cols, "f(\\d+)_(\\d+)")

  # remove rows where there are no observations (individual was
  # not assessed in that instance
  obs <- which(rowSums(!is.na(ukb_long)) > 2)
  
  # rearrange columns into ascending order of field, array
  # increasing order of field, array as numeric values
  field_array_order <- order(as.numeric(field_array[,2]), as.numeric(field_array[,3]), na.last=FALSE)

  return(ukb_long[obs,field_array_order])

}


#' convert into 'superlong' format with separate row for
#' each array.
#' @export 
ukb_fieldarray2long <- function(ukb) {

  ukb_long <- ukb %>%
    # list out every id, field on a separate row 
      tidyr::gather(field, value, -f.eid) %>%
    # remove id/field combos with no data
      dplyr::filter(!is.na(value)) %>%
    # split apart field, instance, array
      tidyr::separate(field,
               into=c('f', 'field', 'instance', 'array'),
               sep='\\.') %>%
    # merge field and array labels back togther as f1234
      tidyr::unite(ffield, f, field, sep='') %>%
    # spread field/array across different columns
      tidyr::spread(ffield, value) %>%
    # turn instance and array into integers
      mutate(instance=as.integer(instance),
             array=as.integer(array))

   return(ukb_long)
}
