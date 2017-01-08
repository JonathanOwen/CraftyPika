sample_text <- function(text_lines, n_samples=20){
    total_lines <- nrow(text_lines)
    begin_sample <- floor(runif(n=1, min=1, max=1+total_lines-n_samples))
    text_lines$text_line[begin_sample:(begin_sample+n_samples-1)]
}