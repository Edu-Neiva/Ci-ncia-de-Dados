Prática Ciência de Dados no R
================

Prática de ciência de dados no R em que serão respondidas algumas
perguntas a respeito dos dados de vendas de eletrônicos ao longo de 12
meses.

Cada linha do data frame representa o item de uma compra que pode ou não
conter mais itens; temos também a data e hora da compra, o valor do item
e a quantidade comprada, além do endereço completo. Isso significa que
linhas diferentes com o mesmo ID representam diferentes produtos que
foram comprados simultâneamente, com entrega para um único endereço.
Além disso, não deve haver linhas idênticas.

### Limpeza e organização

O primeiro passo é analisar o conteúdo das colunas para indentificar
problemas.

``` r
library(dplyr)
library(ggplot2)
library(dygraphs)
library(tidyverse)
library(hrbrthemes)
library(forcats)
library(viridis)
library(ggthemes)
library(lubridate)
library(xts)
```

``` r
head(all_data,n=10)
```

    ##    Order.ID                    Product Quantity.Ordered Price.Each
    ## 1    176558       USB-C Charging Cable                2      11.95
    ## 2                                                                 
    ## 3    176559 Bose SoundSport Headphones                1      99.99
    ## 4    176560               Google Phone                1        600
    ## 5    176560           Wired Headphones                1      11.99
    ## 6    176561           Wired Headphones                1      11.99
    ## 7    176562       USB-C Charging Cable                1      11.95
    ## 8    176563 Bose SoundSport Headphones                1      99.99
    ## 9    176564       USB-C Charging Cable                1      11.95
    ## 10   176565         Macbook Pro Laptop                1       1700
    ##        Order.Date                       Purchase.Address
    ## 1  04/19/19 08:46           917 1st St, Dallas, TX 75001
    ## 2                                                       
    ## 3  04/07/19 22:30      682 Chestnut St, Boston, MA 02215
    ## 4  04/12/19 14:38   669 Spruce St, Los Angeles, CA 90001
    ## 5  04/12/19 14:38   669 Spruce St, Los Angeles, CA 90001
    ## 6  04/30/19 09:27      333 8th St, Los Angeles, CA 90001
    ## 7  04/29/19 13:03 381 Wilson St, San Francisco, CA 94016
    ## 8  04/02/19 07:46       668 Center St, Seattle, WA 98101
    ## 9  04/12/19 10:58        790 Ridge St, Atlanta, GA 30301
    ## 10 04/24/19 10:38 915 Willow St, San Francisco, CA 94016

``` r
summary(all_data)
```

    ##      Order.ID                          Product              Quantity.Ordered 
    ##          :   545   USB-C Charging Cable    :21903   1               :168552  
    ##  Order ID:   355   Lightning Charging Cable:21658   2               : 13324  
    ##  160873  :     5   AAA Batteries (4-pack)  :20641   3               :  2920  
    ##  165665  :     4   AA Batteries (4-pack)   :20577   4               :   806  
    ##  178158  :     4   Wired Headphones        :18882                   :   545  
    ##  193511  :     4   Apple Airpods Headphones:15549   Quantity Ordered:   355  
    ##  (Other) :185933   (Other)                 :67640   (Other)         :   348  
    ##    Price.Each             Order.Date    
    ##  11.95  :21903                 :   545  
    ##  14.95  :21658   Order Date    :   355  
    ##  2.99   :20641   12/15/19 20:16:     8  
    ##  3.84   :20577   04/02/19 13:24:     7  
    ##  11.99  :18882   10/30/19 21:28:     7  
    ##  150    :15450   12/11/19 13:24:     7  
    ##  (Other):67739   (Other)       :185921  
    ##                                Purchase.Address 
    ##                                        :   545  
    ##  Purchase Address                      :   355  
    ##  193 Forest St, San Francisco, CA 94016:     9  
    ##  223 Elm St, Los Angeles, CA 90001     :     8  
    ##  279 Sunset St, San Francisco, CA 94016:     8  
    ##  176 North St, San Francisco, CA 94016 :     7  
    ##  (Other)                               :185918

A função *summary* nos mostra alguns problemas: há linhas completamente
vazias e outras preenchidas com o nome da respectiva variável.

O código abaixo remove as linhas vazias, em seguida as linhas
preenchidas com o nome da coluna.

``` r
#remove linhas vazias
dados = all_data[!apply(all_data == "", 1, all), ]

#remove linhas cujo valor da coluna Order.ID seja 'Order ID' 
dados = subset(dados, Order.ID != 'Order ID')
```

As variáveis ‘Quantity.Ordered’ e ‘Price Each’ foram interpretados como
fatores, elas devem ser convertidas para o formato numérico, mas antes,
é necessário converter para o formato ‘character’ do R.

``` r
dados$Quantity.Ordered = as.numeric(as.character(dados$Quantity.Ordered))
dados$Price.Each = as.numeric(as.character(dados$Price.Each))
```

Agora que os dados estão prontos para o uso, vamos chamar novamente a
função *summary*.

    ##     Order.ID                          Product      Quantity.Ordered
    ##  160873 :     5   USB-C Charging Cable    :21903   Min.   :1.000   
    ##  165665 :     4   Lightning Charging Cable:21658   1st Qu.:1.000   
    ##  178158 :     4   AAA Batteries (4-pack)  :20641   Median :1.000   
    ##  193511 :     4   AA Batteries (4-pack)   :20577   Mean   :1.124   
    ##  194253 :     4   Wired Headphones        :18882   3rd Qu.:1.000   
    ##  196615 :     4   Apple Airpods Headphones:15549   Max.   :9.000   
    ##  (Other):185925   (Other)                 :66740      
    
    ##    Price.Each               Order.Date          
    ##  Min.   :   2.99   12/15/19 20:16:     8  
    ##  1st Qu.:  11.95   04/02/19 13:24:     7  
    ##  Median :  14.95   10/30/19 21:28:     7  
    ##  Mean   : 184.40   12/11/19 13:24:     7  
    ##  3rd Qu.: 150.00   01/17/19 21:54:     6  
    ##  Max.   :1700.00   02/06/19 11:14:     6  
    ##                    (Other)       :185909  
    ##
    ##                                Purchase.Address 
    ##  193 Forest St, San Francisco, CA 94016:     9  
    ##  223 Elm St, Los Angeles, CA 90001     :     8  
    ##  279 Sunset St, San Francisco, CA 94016:     8  
    ##  176 North St, San Francisco, CA 94016 :     7  
    ##  197 Center St, San Francisco, CA 94016:     7  
    ##  284 Walnut St, San Francisco, CA 94016:     7  
    ##  (Other)                               :185904

    ##     20in Monitor 
    ##                       4101 
    ##     27in 4K Gaming Monitor           27in FHD Monitor         
    ##                       6230                       7507 
    ##     34in Ultrawide Monitor      AA Batteries (4-pack) 
    ##                       6181                      20577 
    ##     AAA Batteries (4-pack)   Apple Airpods Headphones 
    ##                      20641                      15549 
    ## Bose SoundSport Headphones              Flatscreen TV 
    ##                      13325                       4800 
    ##               Google Phone                     iPhone 
    ##                       5525                       6842 
    ##                   LG Dryer         LG Washing Machine 
    ##                        646                        666 
    ##   Lightning Charging Cable         Macbook Pro Laptop 
    ##                      21658                       4724 
    ##                    Product            ThinkPad Laptop 
    ##                          0                       4128 
    ##       USB-C Charging Cable            Vareebadd Phone 
    ##                      21903                       2065 
    ##           Wired Headphones 
    ##                      18882


    ## [1] "Número total de produtos vendidos: 209079"

Alguns resultados interessantes:

-   O produto mais vendido foi ‘USB-C Charging Cable’, com 21903
    unidades vendidas;

-   O item mais barato custou 2.99, e o mais caro, 1700 dólares;

-   O preço médio de cada item foi 184,40 dólares.

------------------------------------------------------------------------

É possível obter a quantidade de endereços diferentes registrada.

    ## [1] "Número de endereços: 140789"

E a distribuição do número de unidades de cada item na compra.

    ##      1      2      3      4      5      6      7      8      9 
    ## 168552  13324   2920    806    236     80     24      5      3

A maioria absoluta dos produtos foi comprada como item único (168552),
enquanto dois itens iguais foram pedidos em apenas 13324 vezes, caindo
ainda mais para 2920 para 3 itens iguais na mesma compra.

### Quais dias do mês venderam mais?

Vamos agora utilizar o pacote lubridate que funciona da seguinte forma:

time = c(“2019-04-17”, “2019-01-23”) - **isto está no formato
character**

ymd(time) - **isto está em formato de data**

A vantagem do formato de data do R, é que o ggplot (ferramenta gráfica)
o reconhece automaticamente.

É necessário criar uma nova coluna com o valor total de cada linha como
primeiro passo.

Note que a nova variável ‘Date’ é do tipo Date, e não factor ou
character.

A função *xts* do pacote xts cria novo data frame que é indexado pelas
datas. Em segida, a função *apply.daily*, do mesmo pacote, obtém a soma
das vendas de cada dia.

``` r
venda.data = xts(dados[, "Value"], order.by = dados$Date)

venda.data = apply.daily(venda.data, sum)
n= nrow(venda.data)

venda.data = venda.data[1:(n-1),]
```
![alt text](https://github.com/Edu-Neiva/Data-Science/blob/An%C3%A1lise-de-Vendas/venda-mes.PNG?raw=true)


A variação das vendas também pode ser analisada diariamente.
![](https://github.com/Edu-Neiva/Data-Science/blob/An%C3%A1lise-de-Vendas/venda-dia.PNG?raw=true)


Os gráficos gerados acima são interativos em HTML, mas o GitHub não os suporta.

### Em que horas do dia foram registradas mais vendas?

É necessário extrair o horário da venda. Para isso, basta notar que a
hora da compra são os caracteres 10 e 11 das célula de Order.Date

``` r
dados$Hour = substr(as.character(dados$Order.Date), start=10,stop=11)
```

E então, as vendas são somadas segundo o horário em que ocorreram.

``` r
#Valor total vendido para cada hora do dia
venda.hora=aggregate(Value ~Hour,
                    data=dados,FUN=sum)
```

![alt text](https://github.com/Edu-Neiva/Data-Science/blob/An%C3%A1lise-de-Vendas/unnamed-chunk-15-1.png?raw=true)

Compras no site foram realizadas com mais frequência entre 10h e 21h,
atingindo picos as 12 e também as 19h.

### Qual cidade vendeu mais?

Deve-se extrair a cidade e o estado, pois há duas cidade com o mesmo
nome em estados diferentes. Depois, basta aplicar a função *aggregate*
novamente.

Agora, não é possível saber exatamente quais serão os índices a serem
selecionados, pois os endereços não tem um formato padrão como hora e
data. Então, vamos separar a string do endereço primeiro, utilizando a
vírgula como separador para então selecionar a cidade e o estado.

``` r
#strsplit retorna uma lista
city=  strsplit(as.character(dados$Purchase.Address), split = ',')

#convertendo para o formato vetor e selecionando apenas a cidade e o estado (a cidade é o segundo elemento e o estado são os caracteres 2 e 3 do terceiro elemento de cada objeto da lista)
city = unlist(lapply(city, function(x)  paste(x[2],substr(x[3],start=2,stop=3))))


dados$City = city
```

E após agrupar as vendas por cidade, pode-se obter o resultado na forma
de um gráfico.

    ##                 City     Value
    ## 1         Atlanta GA 2795498.6
    ## 2          Austin TX 1819581.8
    ## 3          Boston MA 3661642.0
    ## 4          Dallas TX 2767975.4
    ## 5     Los Angeles CA 5452570.8
    ## 6   New York City NY 4664317.4
    ## 7        Portland ME  449758.3
    ## 8        Portland OR 1870732.3
    ## 9   San Francisco CA 8262203.9
    ## 10        Seattle WA 2747755.5

![alt text](https://github.com/Edu-Neiva/Data-Science/blob/An%C3%A1lise-de-Vendas/unnamed-chunk-18-1.png?raw=true)

![](https://github.com/Edu-Neiva/Data-Science/blob/An%C3%A1lise-de-Vendas/unnamed-chunk-19-1.png?raw=true)

Observa-se o mesmo comportamento em todas as cidades: as vendas sobem de
janeiro a abril e depois caem até o fim de setembro, antes de voltarem a
subir rapidamente.

### Quais produtos são mais frequentemente vendidos juntos?

Primeiro, agrupam-se as vendas por ID e acrescenta-se os produtos. Mas
antes é preciso verificar se existem linhas duplicadas:

``` r
nrow(dados[duplicated(dados) | duplicated(dados, fromLast=TRUE),])
```

    ## [1] 528

Existem 528 linhas duplicadas.

``` r
#remove linhas cujos IDs e Produtos sejam iguais e associa a um novo data frame

venda.id <-
  dados %>%
  distinct(Order.ID,Product, .keep_all=FALSE) 
```

Agora, a função *count* do pacote dplyr cria um novo data frame com a
frequência dos níveis.

    ##   Order.ID                Product
    ## 1   176560           Google Phone
    ## 2   176560       Wired Headphones
    ## 3   176574           Google Phone
    ## 4   176574   USB-C Charging Cable
    ## 5   176586 AAA Batteries (4-pack)
    ## 6   176586           Google Phone

Note que agora todos os ID’s aparecem no mínimo duas vezes.

O código abaixo faz a contagem dos pares mais frequentes.
*combn(unique(x), 2, paste, collapse=“-”)* encontra as combinações
únicas dos produtos e os mostra juntos, *tapply* agrupa por ID e retorna
uma lista, *unlist* transforma num vetor e *table* calcula as
frequências de cada combinação. Essas frequências são sorteadas em ordem
decrescente e *tail* seleciona as 5 maiores.

``` r
teste=venda.id

results = tail(sort(table(unlist(tapply(as.character(teste$Product), 
            teste$Order.ID, FUN=function(x) combn(unique(x), 2, paste, 
            collapse=" e "))))),
           5)


(results)
```

    ## 
    ## Vareebadd Phone e USB-C Charging Cable        Google Phone e Wired Headphones 
    ##                                    361                                    413 
    ##              iPhone e Wired Headphones    Google Phone e USB-C Charging Cable 
    ##                                    447                                    985 
    ##      iPhone e Lightning Charging Cable 
    ##                                   1002

A tabela acima exibe os pares de produtos mais comumente vendidos
juntos.
