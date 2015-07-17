corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        df = complete(directory)
        ids = df[df["nobs"] > threshold, ]$id
        corr = numeric()
        for (i in ids) {
                filename = formatC(i, width = 3, flag = "0")
                file_path = paste(directory, "/", filename,".csv", sep = "")
                ## read CSV file
                Filedata = read.csv(file_path)
                ## find the complete case from CSV file
                diff = Filedata[complete.cases(Filedata), ]
                corr = c(corr, cor(diff$sulfate, diff$nitrate))
        }
        return(corr)
}