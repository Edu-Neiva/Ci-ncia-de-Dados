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
    ##                                Purchase.Address 
    ##  193 Forest St, San Francisco, CA 94016:     9  
    ##  223 Elm St, Los Angeles, CA 90001     :     8  
    ##  279 Sunset St, San Francisco, CA 94016:     8  
    ##  176 North St, San Francisco, CA 94016 :     7  
    ##  197 Center St, San Francisco, CA 94016:     7  
    ##  284 Walnut St, San Francisco, CA 94016:     7  
    ##  (Other)                               :185904

    ##                                          20in Monitor 
    ##                          0                       4101 
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

    ## 193 Forest St, San Francisco, CA 94016      223 Elm St, Los Angeles, CA 90001 
    ##                                      9                                      8 
    ## 279 Sunset St, San Francisco, CA 94016  176 North St, San Francisco, CA 94016 
    ##                                      8                                      7 
    ## 197 Center St, San Francisco, CA 94016 284 Walnut St, San Francisco, CA 94016 
    ##                                      7                                      7

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

<div id="htmlwidget-6acffabcb78505df3fff" style="width:672px;height:480px;" class="dygraphs html-widget"></div>
<script type="application/json" data-for="htmlwidget-6acffabcb78505df3fff">{"x":{"attrs":{"title":"Evolução das vendas por mês","labels":["month","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2019-01-31T00:00:00.000Z","2019-02-28T00:00:00.000Z","2019-03-31T00:00:00.000Z","2019-04-30T00:00:00.000Z","2019-05-31T00:00:00.000Z","2019-06-30T00:00:00.000Z","2019-07-31T00:00:00.000Z","2019-08-31T00:00:00.000Z","2019-09-30T00:00:00.000Z","2019-10-31T00:00:00.000Z","2019-11-30T00:00:00.000Z","2019-12-31T00:00:00.000Z"],[1813586.44,2202022.42,2807100.38,3390670.24,3152606.75,2577802.26,2647775.76,2244467.88,2097560.13,3736726.88,3199603.2,4613443.34]]},"evals":[],"jsHooks":[]}</script>

Também pode-se fazer um gráfico interativo da distribuição da evolução
das vendas por dia.

<div id="htmlwidget-3b704de8fdc28fc68929" style="width:672px;height:480px;" class="dygraphs html-widget"></div>
<script type="application/json" data-for="htmlwidget-3b704de8fdc28fc68929">{"x":{"attrs":{"title":"Evolução das vendas por dia","labels":["day","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"daily","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2019-01-01T00:00:00.000Z","2019-01-02T00:00:00.000Z","2019-01-03T00:00:00.000Z","2019-01-04T00:00:00.000Z","2019-01-05T00:00:00.000Z","2019-01-06T00:00:00.000Z","2019-01-07T00:00:00.000Z","2019-01-08T00:00:00.000Z","2019-01-09T00:00:00.000Z","2019-01-10T00:00:00.000Z","2019-01-11T00:00:00.000Z","2019-01-12T00:00:00.000Z","2019-01-13T00:00:00.000Z","2019-01-14T00:00:00.000Z","2019-01-15T00:00:00.000Z","2019-01-16T00:00:00.000Z","2019-01-17T00:00:00.000Z","2019-01-18T00:00:00.000Z","2019-01-19T00:00:00.000Z","2019-01-20T00:00:00.000Z","2019-01-21T00:00:00.000Z","2019-01-22T00:00:00.000Z","2019-01-23T00:00:00.000Z","2019-01-24T00:00:00.000Z","2019-01-25T00:00:00.000Z","2019-01-26T00:00:00.000Z","2019-01-27T00:00:00.000Z","2019-01-28T00:00:00.000Z","2019-01-29T00:00:00.000Z","2019-01-30T00:00:00.000Z","2019-01-31T00:00:00.000Z","2019-02-01T00:00:00.000Z","2019-02-02T00:00:00.000Z","2019-02-03T00:00:00.000Z","2019-02-04T00:00:00.000Z","2019-02-05T00:00:00.000Z","2019-02-06T00:00:00.000Z","2019-02-07T00:00:00.000Z","2019-02-08T00:00:00.000Z","2019-02-09T00:00:00.000Z","2019-02-10T00:00:00.000Z","2019-02-11T00:00:00.000Z","2019-02-12T00:00:00.000Z","2019-02-13T00:00:00.000Z","2019-02-14T00:00:00.000Z","2019-02-15T00:00:00.000Z","2019-02-16T00:00:00.000Z","2019-02-17T00:00:00.000Z","2019-02-18T00:00:00.000Z","2019-02-19T00:00:00.000Z","2019-02-20T00:00:00.000Z","2019-02-21T00:00:00.000Z","2019-02-22T00:00:00.000Z","2019-02-23T00:00:00.000Z","2019-02-24T00:00:00.000Z","2019-02-25T00:00:00.000Z","2019-02-26T00:00:00.000Z","2019-02-27T00:00:00.000Z","2019-02-28T00:00:00.000Z","2019-03-01T00:00:00.000Z","2019-03-02T00:00:00.000Z","2019-03-03T00:00:00.000Z","2019-03-04T00:00:00.000Z","2019-03-05T00:00:00.000Z","2019-03-06T00:00:00.000Z","2019-03-07T00:00:00.000Z","2019-03-08T00:00:00.000Z","2019-03-09T00:00:00.000Z","2019-03-10T00:00:00.000Z","2019-03-11T00:00:00.000Z","2019-03-12T00:00:00.000Z","2019-03-13T00:00:00.000Z","2019-03-14T00:00:00.000Z","2019-03-15T00:00:00.000Z","2019-03-16T00:00:00.000Z","2019-03-17T00:00:00.000Z","2019-03-18T00:00:00.000Z","2019-03-19T00:00:00.000Z","2019-03-20T00:00:00.000Z","2019-03-21T00:00:00.000Z","2019-03-22T00:00:00.000Z","2019-03-23T00:00:00.000Z","2019-03-24T00:00:00.000Z","2019-03-25T00:00:00.000Z","2019-03-26T00:00:00.000Z","2019-03-27T00:00:00.000Z","2019-03-28T00:00:00.000Z","2019-03-29T00:00:00.000Z","2019-03-30T00:00:00.000Z","2019-03-31T00:00:00.000Z","2019-04-01T00:00:00.000Z","2019-04-02T00:00:00.000Z","2019-04-03T00:00:00.000Z","2019-04-04T00:00:00.000Z","2019-04-05T00:00:00.000Z","2019-04-06T00:00:00.000Z","2019-04-07T00:00:00.000Z","2019-04-08T00:00:00.000Z","2019-04-09T00:00:00.000Z","2019-04-10T00:00:00.000Z","2019-04-11T00:00:00.000Z","2019-04-12T00:00:00.000Z","2019-04-13T00:00:00.000Z","2019-04-14T00:00:00.000Z","2019-04-15T00:00:00.000Z","2019-04-16T00:00:00.000Z","2019-04-17T00:00:00.000Z","2019-04-18T00:00:00.000Z","2019-04-19T00:00:00.000Z","2019-04-20T00:00:00.000Z","2019-04-21T00:00:00.000Z","2019-04-22T00:00:00.000Z","2019-04-23T00:00:00.000Z","2019-04-24T00:00:00.000Z","2019-04-25T00:00:00.000Z","2019-04-26T00:00:00.000Z","2019-04-27T00:00:00.000Z","2019-04-28T00:00:00.000Z","2019-04-29T00:00:00.000Z","2019-04-30T00:00:00.000Z","2019-05-01T00:00:00.000Z","2019-05-02T00:00:00.000Z","2019-05-03T00:00:00.000Z","2019-05-04T00:00:00.000Z","2019-05-05T00:00:00.000Z","2019-05-06T00:00:00.000Z","2019-05-07T00:00:00.000Z","2019-05-08T00:00:00.000Z","2019-05-09T00:00:00.000Z","2019-05-10T00:00:00.000Z","2019-05-11T00:00:00.000Z","2019-05-12T00:00:00.000Z","2019-05-13T00:00:00.000Z","2019-05-14T00:00:00.000Z","2019-05-15T00:00:00.000Z","2019-05-16T00:00:00.000Z","2019-05-17T00:00:00.000Z","2019-05-18T00:00:00.000Z","2019-05-19T00:00:00.000Z","2019-05-20T00:00:00.000Z","2019-05-21T00:00:00.000Z","2019-05-22T00:00:00.000Z","2019-05-23T00:00:00.000Z","2019-05-24T00:00:00.000Z","2019-05-25T00:00:00.000Z","2019-05-26T00:00:00.000Z","2019-05-27T00:00:00.000Z","2019-05-28T00:00:00.000Z","2019-05-29T00:00:00.000Z","2019-05-30T00:00:00.000Z","2019-05-31T00:00:00.000Z","2019-06-01T00:00:00.000Z","2019-06-02T00:00:00.000Z","2019-06-03T00:00:00.000Z","2019-06-04T00:00:00.000Z","2019-06-05T00:00:00.000Z","2019-06-06T00:00:00.000Z","2019-06-07T00:00:00.000Z","2019-06-08T00:00:00.000Z","2019-06-09T00:00:00.000Z","2019-06-10T00:00:00.000Z","2019-06-11T00:00:00.000Z","2019-06-12T00:00:00.000Z","2019-06-13T00:00:00.000Z","2019-06-14T00:00:00.000Z","2019-06-15T00:00:00.000Z","2019-06-16T00:00:00.000Z","2019-06-17T00:00:00.000Z","2019-06-18T00:00:00.000Z","2019-06-19T00:00:00.000Z","2019-06-20T00:00:00.000Z","2019-06-21T00:00:00.000Z","2019-06-22T00:00:00.000Z","2019-06-23T00:00:00.000Z","2019-06-24T00:00:00.000Z","2019-06-25T00:00:00.000Z","2019-06-26T00:00:00.000Z","2019-06-27T00:00:00.000Z","2019-06-28T00:00:00.000Z","2019-06-29T00:00:00.000Z","2019-06-30T00:00:00.000Z","2019-07-01T00:00:00.000Z","2019-07-02T00:00:00.000Z","2019-07-03T00:00:00.000Z","2019-07-04T00:00:00.000Z","2019-07-05T00:00:00.000Z","2019-07-06T00:00:00.000Z","2019-07-07T00:00:00.000Z","2019-07-08T00:00:00.000Z","2019-07-09T00:00:00.000Z","2019-07-10T00:00:00.000Z","2019-07-11T00:00:00.000Z","2019-07-12T00:00:00.000Z","2019-07-13T00:00:00.000Z","2019-07-14T00:00:00.000Z","2019-07-15T00:00:00.000Z","2019-07-16T00:00:00.000Z","2019-07-17T00:00:00.000Z","2019-07-18T00:00:00.000Z","2019-07-19T00:00:00.000Z","2019-07-20T00:00:00.000Z","2019-07-21T00:00:00.000Z","2019-07-22T00:00:00.000Z","2019-07-23T00:00:00.000Z","2019-07-24T00:00:00.000Z","2019-07-25T00:00:00.000Z","2019-07-26T00:00:00.000Z","2019-07-27T00:00:00.000Z","2019-07-28T00:00:00.000Z","2019-07-29T00:00:00.000Z","2019-07-30T00:00:00.000Z","2019-07-31T00:00:00.000Z","2019-08-01T00:00:00.000Z","2019-08-02T00:00:00.000Z","2019-08-03T00:00:00.000Z","2019-08-04T00:00:00.000Z","2019-08-05T00:00:00.000Z","2019-08-06T00:00:00.000Z","2019-08-07T00:00:00.000Z","2019-08-08T00:00:00.000Z","2019-08-09T00:00:00.000Z","2019-08-10T00:00:00.000Z","2019-08-11T00:00:00.000Z","2019-08-12T00:00:00.000Z","2019-08-13T00:00:00.000Z","2019-08-14T00:00:00.000Z","2019-08-15T00:00:00.000Z","2019-08-16T00:00:00.000Z","2019-08-17T00:00:00.000Z","2019-08-18T00:00:00.000Z","2019-08-19T00:00:00.000Z","2019-08-20T00:00:00.000Z","2019-08-21T00:00:00.000Z","2019-08-22T00:00:00.000Z","2019-08-23T00:00:00.000Z","2019-08-24T00:00:00.000Z","2019-08-25T00:00:00.000Z","2019-08-26T00:00:00.000Z","2019-08-27T00:00:00.000Z","2019-08-28T00:00:00.000Z","2019-08-29T00:00:00.000Z","2019-08-30T00:00:00.000Z","2019-08-31T00:00:00.000Z","2019-09-01T00:00:00.000Z","2019-09-02T00:00:00.000Z","2019-09-03T00:00:00.000Z","2019-09-04T00:00:00.000Z","2019-09-05T00:00:00.000Z","2019-09-06T00:00:00.000Z","2019-09-07T00:00:00.000Z","2019-09-08T00:00:00.000Z","2019-09-09T00:00:00.000Z","2019-09-10T00:00:00.000Z","2019-09-11T00:00:00.000Z","2019-09-12T00:00:00.000Z","2019-09-13T00:00:00.000Z","2019-09-14T00:00:00.000Z","2019-09-15T00:00:00.000Z","2019-09-16T00:00:00.000Z","2019-09-17T00:00:00.000Z","2019-09-18T00:00:00.000Z","2019-09-19T00:00:00.000Z","2019-09-20T00:00:00.000Z","2019-09-21T00:00:00.000Z","2019-09-22T00:00:00.000Z","2019-09-23T00:00:00.000Z","2019-09-24T00:00:00.000Z","2019-09-25T00:00:00.000Z","2019-09-26T00:00:00.000Z","2019-09-27T00:00:00.000Z","2019-09-28T00:00:00.000Z","2019-09-29T00:00:00.000Z","2019-09-30T00:00:00.000Z","2019-10-01T00:00:00.000Z","2019-10-02T00:00:00.000Z","2019-10-03T00:00:00.000Z","2019-10-04T00:00:00.000Z","2019-10-05T00:00:00.000Z","2019-10-06T00:00:00.000Z","2019-10-07T00:00:00.000Z","2019-10-08T00:00:00.000Z","2019-10-09T00:00:00.000Z","2019-10-10T00:00:00.000Z","2019-10-11T00:00:00.000Z","2019-10-12T00:00:00.000Z","2019-10-13T00:00:00.000Z","2019-10-14T00:00:00.000Z","2019-10-15T00:00:00.000Z","2019-10-16T00:00:00.000Z","2019-10-17T00:00:00.000Z","2019-10-18T00:00:00.000Z","2019-10-19T00:00:00.000Z","2019-10-20T00:00:00.000Z","2019-10-21T00:00:00.000Z","2019-10-22T00:00:00.000Z","2019-10-23T00:00:00.000Z","2019-10-24T00:00:00.000Z","2019-10-25T00:00:00.000Z","2019-10-26T00:00:00.000Z","2019-10-27T00:00:00.000Z","2019-10-28T00:00:00.000Z","2019-10-29T00:00:00.000Z","2019-10-30T00:00:00.000Z","2019-10-31T00:00:00.000Z","2019-11-01T00:00:00.000Z","2019-11-02T00:00:00.000Z","2019-11-03T00:00:00.000Z","2019-11-04T00:00:00.000Z","2019-11-05T00:00:00.000Z","2019-11-06T00:00:00.000Z","2019-11-07T00:00:00.000Z","2019-11-08T00:00:00.000Z","2019-11-09T00:00:00.000Z","2019-11-10T00:00:00.000Z","2019-11-11T00:00:00.000Z","2019-11-12T00:00:00.000Z","2019-11-13T00:00:00.000Z","2019-11-14T00:00:00.000Z","2019-11-15T00:00:00.000Z","2019-11-16T00:00:00.000Z","2019-11-17T00:00:00.000Z","2019-11-18T00:00:00.000Z","2019-11-19T00:00:00.000Z","2019-11-20T00:00:00.000Z","2019-11-21T00:00:00.000Z","2019-11-22T00:00:00.000Z","2019-11-23T00:00:00.000Z","2019-11-24T00:00:00.000Z","2019-11-25T00:00:00.000Z","2019-11-26T00:00:00.000Z","2019-11-27T00:00:00.000Z","2019-11-28T00:00:00.000Z","2019-11-29T00:00:00.000Z","2019-11-30T00:00:00.000Z","2019-12-01T00:00:00.000Z","2019-12-02T00:00:00.000Z","2019-12-03T00:00:00.000Z","2019-12-04T00:00:00.000Z","2019-12-05T00:00:00.000Z","2019-12-06T00:00:00.000Z","2019-12-07T00:00:00.000Z","2019-12-08T00:00:00.000Z","2019-12-09T00:00:00.000Z","2019-12-10T00:00:00.000Z","2019-12-11T00:00:00.000Z","2019-12-12T00:00:00.000Z","2019-12-13T00:00:00.000Z","2019-12-14T00:00:00.000Z","2019-12-15T00:00:00.000Z","2019-12-16T00:00:00.000Z","2019-12-17T00:00:00.000Z","2019-12-18T00:00:00.000Z","2019-12-19T00:00:00.000Z","2019-12-20T00:00:00.000Z","2019-12-21T00:00:00.000Z","2019-12-22T00:00:00.000Z","2019-12-23T00:00:00.000Z","2019-12-24T00:00:00.000Z","2019-12-25T00:00:00.000Z","2019-12-26T00:00:00.000Z","2019-12-27T00:00:00.000Z","2019-12-28T00:00:00.000Z","2019-12-29T00:00:00.000Z","2019-12-30T00:00:00.000Z","2019-12-31T00:00:00.000Z"],[65681.94,70813.2,47046.2,62012.21,46524.63,52777.49,53676.42,56112.47,55153.13,56660.92,78414.54,48126.41,61284.39,50090.48,64869.32,51305,55152.75,48469.74,56964.73,68084.88,60948.29,59380.92,57572.45,55848.86,57721.49,71476.18,63683.97,57419.48,61692.21,56238.61,62383.13,71757.33,77893.74,72971.05,70260,76124.54,86290.9,74316.68,94507.13,87770.12,76934.51,75116.02,81070.51,92570.14,71620.69,75581.65,87786.35,84481.74,80362.52,75558.98,75050.82,80969.31,82520.99,65925.8,74934.91,71061.15,80006.81,90377.62,68200.41,90943.91,92309.7,85518.78,88485.21,100209.44,86296.07,95664.75,91815.88,94645.37,83418.55,88651.19,90488.99,88235.73,82178.02,84574.49,81095.76,81371.81,100970.47,85103.11,86559.98,85403.07,102992.11,89978.4,78756.76,101651.05,98915.11,95202.2,92156.67,97068.06,95571.96,90867.78,112697.76,104496.71,113611.75,116214.99,114012.88,109928.87,103941.95,94885.95,112286.25,120373.48,113048.28,109263.75,106087.76,105730.53,110242.2,120860.86,126445.19,129520.55,101059.56,96773.82,120653.93,91174.98,111792.56,136177.32,124453.24,115389.09,122127.73,107756.66,122115.62,117546.02,107690.74,97324.25,105895,104348.54,109929.27,96172.39,94958.09,107953.46,109371,106879.76,108997.9,105095.31,90449.07,103800.64,91007.38,97925.9,105365.07,98970.22,108252.43,100939.51,99856.81,112042.05,95498.61,100628.17,89041.19,113490.2,95637.76,104614.89,103177.54,103003.08,84290.52,90536.69,86997.62,85261.91,86559.43,88272.75,87353.04,77574.34,91885.25,99484.39,72277.92,93080.46,85872.03,82626.14,92760.07,91163.59,76546.5,81022.16,98105.83,94349.83,88466.26,85703.94,74628.57,89733.69,65980.7,94449.32,68766.31,71480.38,99353.32,93773.55,83736.27,89500.91,76026.2,93628,102134.73,72285.7,99457.61,70338.14,84543.82,83604.82,89916.88,80924.08,78338.91,98082.88,94690.12,90238.14,87845.78,78741.93,96480.18,73736.12,84184.39,104283.77,90392.25,75213.24,83754,74325.07,71835.06,80624.03,87004.43,77803.5,87488.84,90352.23,72168.44,69120.53,76031.31,70642.64,76464.89,83380.34,74489.65,64919.08,78485.46,95160.63,77852.09,66674.74,70020.41,83965.86,73802.24,66786.54,72013.67,59915,73513.04,74967.59,53060.42,81481.5,60535.27,66452.51,80612.89,73430.21,68615.65,75286.79,73259.51,65785.16,65573.82,67913.39,72275.38,62071.29,71784.82,67834.51,70087.1,70665.44,64098.3,79465.82,74257.35,77830.23,74776.11,78224.28,79399.76,73422.21,57940.24,62443.57,76563.3,64202.46,63013.83,62876.26,47909.61,70232.03,66751.42,78245.17,79307.77,66331.18,72096.63,73285.42,72255.25,133545.32,115401.66,99928.92,121014.29,122682.14,134384.92,121833.37,108477.64,106094.11,125440.59,116730.85,124765.05,122900.14,128103.24,107623.75,114107.87,130827.84,119705.72,117261.52,129369.48,114909.48,134168.94,118348.49,109809.6,123926.85,126993.86,119612.37,126483.5,133132.28,98910.76,130232.33,107058.9,125772.34,89898.24,103673.17,105864.68,93916.89,117692.05,105236.88,115754.83,104955.4,110945.82,100467.22,93579.46,99578.47,110547.06,94234.47,104077.54,100369.25,111336.53,116783.61,98591.99,115879.28,102594.45,130244.5,121062.75,95920.67,128022.25,95941.37,92666.45,106936.68,148153.63,149025.71,142570.45,166727.69,155079.3,151910.47,139645.75,143527.63,147135.13,162832.74,146367.03,144195.15,153332.14,141991.78,143269.72,162970.61,157364.72,154598.77,137732.67,160181.3,154756.87,147348.09,152214.45,152888.82,152268.42,144912.02,126628.05,134015.5,156024.62,152319.81,131454.3]]},"evals":["attrs.interactionModel"],"jsHooks":[]}</script>

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

![](Análise-de-Vendas_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

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

![](Análise-de-Vendas_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

![](Análise-de-Vendas_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

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
