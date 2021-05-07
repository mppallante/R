### Nome : Banco de Dados         ### 
### Tipo : Anotação               ###
### Autor: Matheus Pina Pallante. ### 
### Ano  : 2021                   ### 

##### Pacotes
require(odbc)
require(DBI)
require(data.table)

##### Conexão Banco de Dados
conexao = function(){
        return(dbConnect(odbc::odbc(),
                         Driver   = " ",
                         Server   = " ",
                         Database = " ",
                         UID      = " ",
                         pwd      = " "))
}
con <- conexao()

##### Consulta Banco de Dados
consulta = function(query){
        x <- dbGetQuery(con, query)
        x <- as.data.table(query)
        return(x)
}
