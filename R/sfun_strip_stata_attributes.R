sfun_strip_stata_attributes = function(x) {
  restore.point("sfun_strip_stata_attributes")
  # For data frames, apply to each column. For vectors, strip attributes directly.
  if (is.data.frame(x)) {
    # Apply recursively to each column, ensuring it stays a data.frame/tibble
    x[] = lapply(x, sfun_strip_stata_attributes)
    return(x)
  } else {
    # Handle individual vectors (columns)
    if (inherits(x, "haven_labelled")) {
      # Convert haven_labelled to its underlying base type (numeric, character, or factor if labels imply factors)
      x = haven::zap_labels(x)
      x = haven::zap_formats(x)
      x = haven::zap_missing(x)
    }
    
    # Preserve Date class, just strip other attributes.
    # This must come before is.numeric(x) check as Date objects are also numeric.
    if (inherits(x, "Date")) {
      attr_names_to_remove = setdiff(names(attributes(x)), c("names", "class")) # Keep class for Date
      if (length(attr_names_to_remove) > 0) {
        attributes(x)[attr_names_to_remove] = NULL
      }
      return(x)
    }
    
    # Explicitly cast to base R types to ensure no problematic attributes remain
    # This also handles cases where a variable might have been an R factor or other
    # specific class that Stata doesn't have a direct equivalent for.
    if (is.numeric(x)) {
      # Removed explicit rounding. Let `compare_df`'s tolerance handle precision differences.
      x = as.numeric(x)
    } else if (is.character(x)) {
      x = as.character(x)
    } else if (is.logical(x)) {
      x = as.logical(x)
    } else if (is.factor(x)) {
      # Convert factors to character for consistency with Stata strings.
      # Stata doesn't have factors, string conversion is the closest equivalent.
      x = as.character(x)
    }
    # For any other generic attributes, remove them, but keep 'names'.
    attr_names_to_remove = setdiff(names(attributes(x)), "names")
    if (length(attr_names_to_remove) > 0) {
      attributes(x)[attr_names_to_remove] = NULL
    }
    return(x)
  }
}

