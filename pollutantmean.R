pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating the location of
        ## the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating the name of the
        ## pollutant for which we will calculate the mean; either 'sulfate' or
        ## 'nitrate'.
        
        ## 'id' is an integer vector indicating the monitor ID numbers to be used
        
        ## Return the mean of the pollutant across all monitors list in the 'id'
        ## vector (ignoring NA values)
        data = numeric()
        for (i in id) {
                filename = formatC(i, width = 3, flag = "0")
                file_path = paste(directory, "/", filename,".csv", sep = "")
                ## read CSV file
                Filedata = read.csv(file_path)
                ## select a specific column from CSV file
                columndata = Filedata[[pollutant]]
                ## extend the dimention of data and sum all data
                data = c(data, columndata)
        }
        return(round(mean(data, na.rm = TRUE), 3))
}