library(dplyr)

diario = data.frame(matrix(ncol = 4, nrow = 0))
colnames(diario) <- c("tipo", "conta", "valor", "historico")

lancamento <- function(tipo, conta, valor, historico = "."){
  temp <- data.frame(tipo, conta, valor, historico)
  rbind(diario, temp)
}

diario <- lancamento("d", "Capital a Integralizar", 10000, "Subscrição de Capital")
diario <- lancamento("c", "Capital Social", 10000, "Subscrição de Capital")
diario <- lancamento("d", "Caixa", 10000, "Integralização de Capital")
diario <- lancamento("c", "Capital a Integralizar", 10000, "Integralização de Capital")

conferidor <- function(){
  temp = diario %>% select(tipo, valor) %>% group_by(tipo) %>% summarise(Soma = sum(valor))
  print(paste0("Créditos: ", temp$Soma[1]))
  print(paste0("Débitos: ", temp$Soma[2]))
}

conferidor()

planoContas <- data.frame(categoria = c("A", "A", "E", "E", "E"),
                          conta = c("Ativo Total", "Caixa",
                                    "Patrimônio Líquido", "Capital Social", "Capital a Integralizar"),
                          natureza = c("d", "d", "c", "c", "c")
)

balancete <- planoContas  

levantaBalancete <- function(){
  temp <- diario
  temp <- left_join(temp, planoContas, all.x = TRUE, all.y = FALSE)
  temp$valor <- ifelse (temp$tipo == temp$natureza, temp$valor*1, temp$valor*-1)
  temp <- temp %>% group_by(conta) %>% summarise(valor = sum(valor))
  temp
} 

levantaBalancete()

imprimeDiario <-function(){
  print(diario)
}

imprimeDiario()
