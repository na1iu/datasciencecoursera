complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        nobs = numeric()
        for (i in id) {
                filename = formatC(i, width = 3, flag = "0")
                file_path = paste(directory, "/", filename,".csv", sep = "")
                ## read CSV file
                Filedata = read.csv(file_path)
                ## find the complete case from CSV file
                ccases = complete.cases(Filedata)
                ## sum the complete cases
                total = sum(ccases)
                nobs = c(nobs, total)
        }
        return (data.frame(id, nobs))
}