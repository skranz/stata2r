sfun_strip_stata_attributes = function(x) {
  # Stata generate/replace typically does not carry forward variable labels.
  # So, remove the 'label' attribute if it exists.
  attr(x, "label") = NULL
  # Also remove any value labels (e.g. from haven::labelled)
  attr(x, "labels") = NULL
  # Remove "labelled" class if present, as it's specific to haven/labelled package
  attr(x, "class") = setdiff(class(x), "labelled")
  # Stata format attributes (format.stata, display_label) are often preserved by Stata commands
  # We will NOT remove them by default, as they often persist after data manipulation.
  # attr(x, "format.stata") = NULL # Removed this line
  # attr(x, "display_label") = NULL # Removed this line
  # Remove any other haven-specific attributes that might be added and are not standard R
  attr(x, "na_values") = NULL # Stata extended missing values
  attr(x, "na_range") = NULL  # Stata extended missing values
  return(x)
}


