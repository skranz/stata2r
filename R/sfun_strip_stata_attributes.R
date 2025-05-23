sfun_strip_stata_attributes = function(x) {
  # Stata generate/replace typically does not carry forward variable labels.
  # So, remove the 'label' attribute if it exists.
  attr(x, "label") = NULL
  # Also remove any value labels (e.g. from haven::labelled)
  attr(x, "labels") = NULL
  # Remove "labelled" class if present, as it's specific to haven/labelled package
  attr(x, "class") = setdiff(class(x), "labelled")
  return(x)
}

