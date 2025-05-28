sfun_strip_stata_attributes = function(x) {
  restore.point("sfun_strip_stata_attributes")
  # no need to strip attributes
  # actually all calls to it should be removed from code
  return(x)
}

