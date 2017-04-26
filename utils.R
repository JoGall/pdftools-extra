require(pdftools)

#=================================================================================
# FUNCTIONS
#=================================================================================

# 'pdf_cols()' reads a PDF with text split into columns
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

read_text <- function(text, n_col) {
    result <- ''
    
    #get indices of most frequent spaces (" ")
    lstops <- gregexpr(pattern =" ",text)
    stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
    
    #slice by number of columns
    for(i in seq(1, n_col, by=1)) {
        temp_result <- sapply(text, function(x){
        start <- 1
        stop <-stops[i] 
        if(i > 1)            
            start <- stops[i-1] + 1
        if(i == n_col) #last column, read until end.
            stop <- nchar(x)+1
        substr(x, start=start, stop=stop)
        }, USE.NAMES=FALSE)
        temp_result <- trim(temp_result)
        result <- append(result, temp_result)
    }

    result
}

pdf_cols <- function(filename, n_col) {

    txt <- pdf_text(filename)
    result <- ''

    for (i in 1:length(txt)) {
        page <- txt[i]
        t1 <- unlist(strsplit(page, "\n"))      
        maxSize <- max(nchar(t1))
        t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
        result = append(result, read_text(t1, n_col))
    }
    result
}


#'concat_hyphens()' function concatenates words split by a breaking hyphen
concat_hyphens <- function(result) {

    for(i in 1:(length(result)-1)) {

        if(nchar(result[i]) > 0) {
        
            line1 <- strsplit(result[i], split=" ")[[1]]
            endstr <- line1[length(line1)]
            endchars <- strsplit(endstr, "")[[1]]

            #if final character of line is a hyphen
            if(endchars[length(endchars)] == '-') {
            
                #concatenate hyphenated word on first line
                prfx <- endchars[1:length(endchars)-1]
                prfx = paste(prfx, sep="", collapse="")
                line2 <- strsplit(result[i+1], split=" ")[[1]]
                sfx <- line2[1]
                newstr <- paste(prfx, sfx, sep="")
                
                #replace word prefix with full word
                result[i] <- c(paste(c(line1[1:(length(line1)-1)], newstr), sep="", collapse=" "))
                
                #remove word suffix from following line
                result[i+1] <- paste(line2[2:length(line2)], sep="", collapse=" ")
            
            }
        }
    }
    result
}

#=================================================================================
# EXAMPLE
#=================================================================================

# source("pdftools-extra/utils.R")
#
# filename <- "pdftools-extra/loremipsum_ex.pdf"
# 
# txt <- pdf_cols(filename, 3)
# 
# #remove empty lines
# txt <- txt[nchar(txt)>0]
# 
# #concatenate words split by a breaking hyphen
# txt <- concat_hyphens(txt)
# 
# #convert to tidy text format
# tidytext::text_df <- data_frame(line = 1:length(txt), text = txt)


#=================================================================================
# TODO
#=================================================================================

# 1) Read PDFs containing a mixed number of columns

# 2) Extract tables from PDFs alongside text

# 3) Handle lines ending with a hyphen when word does not need concatenated, e.g.
# "Shakespeare's birthplace was Stratford-
# upon-Avon, UK."

# 4) Handle special characters and quotations (\") inside text
