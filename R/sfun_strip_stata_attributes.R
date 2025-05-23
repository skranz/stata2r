sfun_strip_stata_attributes = function(x) {
  # Stata generate/replace typically does not carry forward variable labels.
  # So, remove the 'label' attribute if it exists.
  attr(x, "label") = NULL
  # Also remove any value labels (e.g. from haven::labelled)
  attr(x, "labels") = NULL
  # Remove "labelled" class if present, as it's specific to haven/labelled package
  attr(x, "class") = setdiff(class(x), "labelled")
  # Remove Stata format attributes that are not typically preserved by Stata commands
  attr(x, "format.stata") = NULL
  attr(x, "display_label") = NULL # Often comes with format.stata
  # Remove any other haven-specific attributes that might be added and are not standard R
  attr(x, "na_values") = NULL # Stata extended missing values
  attr(x, "na_range") = NULL  # Stata extended missing values
  return(x)
}


