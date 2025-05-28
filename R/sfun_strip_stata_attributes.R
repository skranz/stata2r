sfun_strip_stata_attributes = function(x) {
  restore.point("sfun_strip_stata_attributes")
  # For data frames, apply to each column. For vectors, strip attributes directly.
  if (is.data.frame(x)) {
    x[] = lapply(x, sfun_strip_stata_attributes)
    return(x)
  } else {
    # For haven_labelled, it's better to convert to base type (numeric, character, factor)
    if (inherits(x, "haven_labelled")) {
      # This effectively zaps all haven-specific attributes and converts to base R type
      # If it's a numeric variable with labels, it remains numeric. If it was string, it remains string.
      x = haven::zap_labels(x)
      x = haven::zap_formats(x)
      x = haven::zap_missing(x)
    }
    # For any other generic attributes, remove them.
    # Keep 'names' attribute for vectors (column names), if present, as it's fundamental.
    attr_names = setdiff(names(attributes(x)), "names")
    if (length(attr_names) > 0) {
      attributes(x)[attr_names] = NULL
    }
    return(x)
  }
}

