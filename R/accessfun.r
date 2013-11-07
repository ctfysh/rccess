#' Extract SQL Codes
#' 
#' Separete the SQL codes into several single ones.
#' 
#' This function is used to separete the SQL codes into several single ones
#' which can be easily called by Mircrosoft Access with R package RODBC.
#' 
#' @param file the filename of SQL codes
#' @return A string vector with several single SQL codes
#' @export
#' 
querysep=function(file){
    query=readLines(file,warn=F)
    query=query[!grepl("--",query)]
    query=gsub("\t","",query)
    query=gsub(" +"," ",query)
    query=gsub("^ +","",query)
    querys=NULL
    n1=1
    for(i in grep(";",query)){
        n2=i
        querys=c(querys,paste(query[n1:n2],collapse=" "))
        n1=i+1
    }
    return(gsub("^ *","",querys))
}

#' Create a New *.mdb Database
#' 
#' A new *.mdb database will be created by copying the "EmptyDB.mdb" file with
#' your database name.
#' 
#' @param dbname the database name which you create
#' @param emptydb the empty database you will copy
#' @param dir the directory of your new created database
#' @param ... arguments to be passed to \code{\link{file.copy}}
#' @return A new *.mdb database
#' @export
#' 
createmdb=function(dbname="newdb.mdb",
                   emptydb=system.file("EmptyDB.mdb",package="rccess"),
                   dir=getwd(),...){
    file.copy(emptydb,paste(dir,dbname,sep="/"),...)
}

#' Call with Batch SQL Codes
#' 
#' Submit each SQL query to a *.mdb database, and retrieve the results.
#' 
#' @param channel connection handle as returned by \code{\link{odbcConnect}}
#' @param query any valid SQL statement
#' @param ... arguments to be passed to \code{\link{sqlQuery}}
#' @return Values are same with \code{\link{sqlQuery}}
#' @export
#' 
sqlQuerys=function(channel,queries,...){
    lapply(queries,sqlQuery,channel=channel,...)
}

#' Read Tables and Fields under Each Table in the Database
#' 
#' Create a list of the fields in each table.
#' 
#' @param channel connection handle as returned by \code{\link{odbcConnect}}
#' @param tableType specify zero or more types in separate elements of a 
#' character vector
#' @param ... arguments to be passed to \code{\link{file.copy}}
#' @return A list of the fields in each table will be returned
#' @export
#' 
sqlFields=function(channel,tableType="TABLE",...){
    stb=sqlTables(channel,tableType=tableType,...)$TABLE_NAME
    fld=lapply(stb,function(x) sqlColumns(channel,x,...)$COLUMN_NAME)
    names(fld)=stb
    return(fld)
}



