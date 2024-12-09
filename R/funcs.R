# volume extraction function
# if volume spilled is reported, extract, regardless of whether other gallons are reported
# otherwise, take max numeric value for gallons
extract_fun <- function(descrip, hasgal, hasvol, hasmlg) {
  if(hasvol)
    out <- str_extract(descrip, "(?<=spill volume:\\s)\\d+|(?<=spill volume:\\sestimated\\s)\\d+")
  if(hasgal & !hasvol)
    out <- str_extract_all(descrip, "\\d+\\s*gallons?")[[1]] |>
      str_extract("\\d+")
  if(hasmlg & !hasgal & !hasvol){
    out <- gsub('mg\\/l', '', descrip)
    out <- str_extract_all(out, "\\d*\\.?\\d+\\s*mg[\\.\\s]?")[[1]] |>
      str_extract("\\d*\\.?\\d+")
  }
  if(length(out) != 0 & !anyNA(out))
    out <- as.numeric(out) |> max()
  else
    out <- NA
  if(hasmlg & !hasgal & !hasvol)
    out <- out * 1e6
  return(out)
}