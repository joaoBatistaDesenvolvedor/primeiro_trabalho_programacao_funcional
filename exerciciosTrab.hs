-- 1 Escreva a função analisa_raizes que, dados os 3 coeficientes a, b e c de uma
--equação quadrática ax2 +bx + c = 0, realiza a análise das raízes da equação. A equação
--é dita degenerada, se o coeficiente do termo quadrático a for igual a zero. Por outro lado,
--uma equação não degenerada possui o número de raízes reais de acordo com as regras:
--(1) a equação possui duas raízes reais, se b2 > 4*a*c; (2) a equação possui uma raiz real,
--se b2 = 4*a*c; (3) a equação não possui raízes reais, se b2 < 4*a*c. A análise de saída
--deve ser uma das 4 opções a seguir: “1-possui duas raízes reais”, “2-possui uma raiz
--real”, “3-nenhuma raiz real” ou “4-equação degenerada”.

{-# LANGUAGE ScopedTypeVariables #-}
equacao2grau::Float->Float->Float ->String

equacao2grau a b c | a==0="EQUACAO DEGENERADA"
                   | delta>0="duas raizes"
                   |delta==0="uma raiz"
                   |otherwise ="nao possui nenhuma raiz real"


      where
        delta=b^2 -4*a*c

-- 2) Escreva a função equacao que recebe três valores reais a, b, c. Se a for diferente de
--0, a função retorna uma tupla com as duas raízes da equação de segundo grau ax2 + bx
-- + c = 0. Se Se a for igual a 0, a função retorna uma tupla, sendo o primeiro elemento a
--solução da equação de primeiro grau bx + c = 0 e o segundo elemento o próprio a.

equacao::Float->Float->Float->(Float,Float)
equacao a b c
  | a /=0 && delta==0 = ((-b+sqrt delta)/(2*a),(-b+sqrt delta)/(2*a))
  | a/=0 && delta >0 = ((-b+sqrt delta)/(2*a),(-b-sqrt delta)/(2*a))
  | a /=0 && delta <0 = (0,0)
  | otherwise = (a,0)
  where
      delta = b ^ 2 - 4 * a * c






--3) Considere que o preço de uma passagem de ônibus intermunicipal pode variar
--dependendo da idade do passageiro. Crianças até 10 anos pagam 40% e bebês (abaixo
--de 2 anos) pagam apenas 15%. Pessoas com 70 anos ou mais pagam apenas 50% do
--preço total. Os demais passageiros, pagam a tarifa normal (100%). Faça uma função que
--tenha como entrada: o valor total da passagem, a data atual e a data de nascimento do
--passageiro. Como saída, a função retorna o valor a ser pago. (Obs.: na solução, deve ser
--definido o tipo data para representar a tupla (d,m,a)).


passagem_intermunicipal::Float->(Int, Int,Int)->(Int, Int,Int)->Float

passagem_intermunicipal valor_passagem (dia_at,mes_at,ano_at) (dia_nac,mes_nac,ano_nac)
  | ano_at - ano_nac <2 = valor_passagem -valor_passagem *15/100
  | ano_at - ano_nac >2 && ano_at - ano_nac <=10 = valor_passagem-valor_passagem *40/100
  | ano_at - ano_nac >=70 = valor_passagem- valor_passagem *50/100
  | otherwise = valor_passagem




--4) Construa funções que gerem as seguintes listas, utilizando-se lista por compressão.
--Todas as funções devem utilizar a lista de inteiros de 1 a 20 em pelo menos um dos
--geradores. Apresentar o código da função e o resultado da lista gerada

--a) gera1: gerar a lista de inteiros, contendo o cubo de todos
--os pares entre 3 e 11.
--b) gera2: gerar a lista de duplas formadas tendo o primeiro
--elemento menor ou igual a 5 e o segundo elemento no intervalo
--fechado entre o valor do primeiro elemento e o seu triplo.
--c) gera3: a partir de uma lista l1=[15,16], gerar a lista com
--todos os elementos dentro do intervalo fechado definido entre
--1 e cada elemento de l1 (Obs.: pode ter elemento repetido na
--lista final).
--d) gera4: gerar uma lista de duplas, onde cada dupla são 2
--números consecutivos de 1 a 10, sendo o primeiro elemento par
--(Ex: (2,3) e (4,5))
--e) gera5: a partir da lista de duplas geradas no item d,
--gerar a lista onde cada elemento corresponde à soma dos
--elementos da dupla.


quartro_a::[Int]
quartro_a =[num^3| num<-[1..20],num >=3,num <=11]

quartro_b:: [(Integer,Integer)]
quartro_b =[(x,y) | x<-[0..5],y<-[x..15]]



quartro_c::[Int]->[Int]
quartro_c [n1,n2]=[1 .. n2]


quartro_d::[(Integer,Integer)]
quartro_d =[(x,y)| x<-[1..10],even x, y<-[x+1]]





quartro_e :: [Integer]
quartro_e=[x+y |(x,y)<-quartro_d ]





--5) a) Escreva uma função (usando compreensão de listas) que calcula a quantidade de
--números que são positivos e múltiplos de 3 (ao mesmo tempo) de uma lista de inteiros:
-- > contaNegM2 [1,-3,-4,3,4,-5,-8,-7,9]

_positivo_3::[Integer ]->Integer
_positivo_3 xs=   sum  [1 | x<-xs, x>0, x `mod` 3==0]



--b) Escreva uma função (usando compreensão de listas) que extrai números que são
--positivos e múltiplos de 3 (ao mesmo tempo) de uma lista de inteiros e os retorna em uma
--nova lista:
-- >listaNegM2 [1,-3,-4,3,4,-5,-8,-7,9]
--[3,9]

qtd_positivo_3::[Integer ]->[Integer]
qtd_positivo_3 xs=     [x | x<-xs, x>0, x `mod` 3==0]





--6) Escreva a função primos a seguir que recebe dois valores inteiros x,y e retorna todos
--os números primos que se encontram entre x e y. Obs: construir uma segunda função
--fatores que retorna todos os divisores de um número inteiro e utilizá-la na elaboração
--da função primos.
-- > primos 10 50
--[11,13,17,19,23,29,31,37,41,43,47]

divisores::Int->[Int]
divisores n=[x | x <- [1..n], n `mod` x==0]
primos :: Int -> Bool
primos x=divisores x==[1,x]
lista_primos::Int->Int->[Int]
lista_primos n1 n2 =[x| x<-[n1..n2],primos x]




--7) Construa a função mmc a seguir que calcula o valor do mínimo múltiplo comum de três
-- números inteiros.
-- > mmc 2 3 4
-- 12



mdc::Int->Int->Int
mdc a b | a < b = mdc b a
 | b == 0 = a
 | otherwise = mdc b (mod a b)




mmc::Int->Int->Int
mmc x y = x * y `div` mdc x y


mmc_3numeros::Int->Int->Int->Int
mmc_3numeros x y z = mmc x (mmc y z)


--8) Escreva uma função que calcula a série a seguir, dados um número real x e o número
--de termos a serem calculados n. Obs: se preciso, use a função fromIntegral para
--converter n de Inteiro para Float.

calcula_serie :: Float -> Int -> Float
calcula_serie x 0 = 0
calcula_serie x n = if even n then (x/fromIntegral n) + calcula_serie x (n - 1) else (fromIntegral n/x) + calcula_serie x (n - 1)




-- 9) Escreva a função fizzbuzz a seguir que recebe um inteiro n e retorna uma lista de
--strings. Para cada inteiro i entre 1 e n, a lista será composta da seguinte forma.
-- • Se i é divisível por 2, escreva “Fizz”.
-- • Se i é divisível por 3, escreva “Buzz”.
-- • Se i é divisível por ambos 2 e 3, escreva “FizzBuzz”.
-- • Caso contrário, diga “No”.
  

decide :: Int -> String
decide x= if(x `mod` 2==0 && x `mod` 3==0 ) then "fizzBuzz" else if(x `mod` 2 == 0 ) then "Fizz" else if(x `mod` 3 == 0 ) then "Buzz"   else "no"
  

fizzBuzz::Int->[String]
fizzBuzz n=[ decide x  | x<-[1..n]]




--10) Usando lista por compreensão, escreva a função seleciona_multiplos que recebe um
--lista de inteiros e um inteiro n e retorna uma nova lista com todos os números presentes na lista


seleciona_multiplos::Integer->[Integer]->[Integer]
seleciona_multiplos n xs=[x| x<-xs, x `mod` n==0]








--11) Escreva a função unica_ocorrencia a seguir que recebe um elemento e uma lista
--e verifica se existe uma única ocorrência do elemento na lista .
-- > unica_ocorrencia 2 [1,2,3,2]
-- False


unica_ocorrencia::Integer->[Integer]->Bool
unica_ocorrencia n1 xs =(sum[1| x<-xs, x==n1])==1



-- 12) Crie uma função que intercala os elementos de duas listas de qualquer tamanho
-- numa nova lista. Obs: as listas de entrada devem ser do mesmo tipo mas podem ter
-- tamanhos diferentes. Caso sejam diferentes, os elementos excedentes da lista maior
-- devem complementar a lista de saída
-- > intercala [1,2,3,4] [100,200]

intercala::[Int]->[Int]->[Int]
intercala x [] = x
intercala [] x = x
intercala (a:xs) (b:ys) = a: b: intercala xs ys





--13) Construa a função zipar que recebe duas listas de mesmo tipo e elabora uma terceira lista
--de sublistas de tamanho 2 formadas por um elemento da primeira lista e outro da segunda lista.
--Assim, a primeira sublista da lista de saída é formada com o primeiro elemento de cada lista, a
--segunda sublista é formada com o segundo elemento de cada uma, e assim por diante. Se listas
--de entrada tiverem tamanhos diferentes, a lista de saída é truncada para o tamanho da menor lista
--de entrada. Obs: não é permitido usar funções de manipulação de listas da biblioteca PRELUDE
--padrão do Haskell. Ex:
-- > zipar [1,2,3,4] [5,6,7,8]
--[[1,5],[2,6],[3,7],[4,8]]
-- > zipar [1,2,3] [4,5]
--[[1,4],[2,5]]




zipar :: [Integer ] -> [Integer ] -> [[Integer ]]
zipar [] ys = []
zipar (x:xs) [] = []
zipar (x:xs) (y:ys) = [x,y] : zipar xs ys





--14) Defina novos tipos para representar os dados contidos numa agenda pessoal. Para
--cada contato, armazene as informações: nome, endereço, telefone, e-mail. Em seguida,
--crie uma função para recuperar o nome de um contato, a partir do email. Caso o número
--não seja encontrado, retornar a mensagem “Email desconhecido”.


type Contato = (String,String,String,String)
type Agenda = [Contato]

listaContato::Agenda
listaContato = [("anna","mg","1234-5678","anna@email.com"), ("joao","us","10152-5678","joao@email.com"),("pedro","ca","605014-5678","pedro@email.com")]


encontraContato::String->Agenda->String
encontraContato email [] = "e-mail nao econtrado"
encontraContato email ((nome,_,_,e):xs) | email == e = nome
 | otherwise = encontraContato email xs









--15) Seja o tipo Pessoa e a lista de pessoas a seguir.
--O tipo pessoa é uma tupla que inclui nome, altura, idade e estado civil (‘c’ ou ‘s’).
--type Pessoa = (String, Float, Int, Char)
--pessoas :: [Pessoa]
--pessoas = [ ("Rosa", 27, 1.66,'F'),
--("João", 1.85, 26, 'C'),
--("Maria", 1.55, 62, 'S'),
--("Jose", 1.78, 42, 'C'),
--("Paulo", 1.93, 25, 'S'),
-- ("Clara", 1.70, 33, 'C'),
-- ("Bob", 1.45, 21, 'C'),
-- ("Rosana", 1.58,39, 'S'),
-- ("Daniel", 1.74, 72, 'S'),
--("Jocileide", 1.69, 18, 'S') ]
-- Escreva funções que, dada a lista pessoas, retornem:
-- • A altura média entre todas as pessoas.
-- • A idade da pessoa mais nova.
-- • O nome e o estado civil da pessoa mais velha.
-- • Todos os dados de cada pessoa com 50 anos ou mais.
-- • O número de pessoas casadas com idade superior a i (ex: i = 35)

type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27, 'C'),
          ("João", 1.85, 26, 'C'),
          ("Maria", 1.55, 62, 'S'),
          ("Jose", 1.78, 42, 'C'),
          ("Paulo", 1.93, 25, 'S'),
          ("Clara", 1.70, 33, 'C'),
          ("Bob", 1.45, 21, 'C'),
          ("Rosana", 1.58, 39, 'S'),
          ("Daniel", 1.74, 72, 'S'),
          ("Jocileide", 1.69, 18, 'S') ]

alturaM :: [Pessoa] -> Float
alturaM l = sum [ a | (n,a,i,e) <- l] / fromIntegral (length l)

idadeMNova :: [Pessoa] -> Int
idadeMNova [(n,a,i,e)] = i
idadeMNova ((np,ap,ip,ep):(ns,as,is,es):r)
     | ip < is = idadeMNova ((np,ap,ip,ep):r)
     | otherwise = idadeMNova ((ns,as,is,es):r)

maisVelho :: [Pessoa] -> (String, Char)
maisVelho [(n,a,i,e)] = (n,e)
maisVelho ((np,ap,ip,ep):(ns,as,is,es):r)
     | ip > is = maisVelho ((np,ap,ip,ep):r)
     | otherwise = maisVelho ((ns,as,is,es):r)

cinquentaOuMais :: [Pessoa] -> [Pessoa]
cinquentaOuMais l = [ (n,a,i,e) | (n,a,i,e) <- l, i >= 50 ]

casadasMaisI :: Int -> [Pessoa] -> Int
casadasMaisI id l = length [ (n,a,i,e) | (n,a,i,e) <- l, i > id, e == 'C' ]



 --16) Escreva a função insere_ord a seguir, que recebe uma lista polimórfica
-- ordenada de elementos (critério de ordenação crescente) e um novo elemento x (do
-- mesmo tipo da lista) e retorna a nova lista com o novo elemento inserido
-- > insere_ord 5 [1,4,7,11]
-- [1,4,5,7,11]
-- > insere_ord 'g' "abcjkl"
-- "abcgjkl"


insereOrd ::(Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y: insereOrd x ys










 --17) Escreva a função reverte a seguir que recebe uma lista polimórfica e retorna uma
-- lista com seus elementos ao contrário.
-- > reverte [1,2,3,4]
-- [4,3,2,1]
-- > reverte "abcd"
--"dcba"

inverte_lista::[a]->[a]
inverte_lista []=[]
inverte_lista (x:xs)=inverte_lista xs++[x]



-- 18) Escreva a função elimina_repet a seguir que recebe uma lista polimórfica e
-- retorna uma lista sem elementos repetidos.
-- > elimina_repet [3,3,2,9,1,7,2,5,9,7]
-- [3,2,9,1,7,5]
-- > elimina_repet "mississippi"
-- "misp"

elimina_repet :: (Eq a) => [a] -> [a]
elimina_repet [] = []
elimina_repet (x:xs) = x : elimina_repet (filter (/= x) xs)



--19) Escreva a função notasTroco a seguir usando compreensão de listas que calcula
-- todas as combinações de notas para devolver o troco durante um pagamento, a partir de
--uma lista com os valores das notas disponíveis (definido no arquivo .hs) e o valor do troco
-- x (argumento da função). Ex:
--Considere disponiveis = [1,2,5,10,20,50,100]

notas:: [Int]
notas = [1,2,5,10,20,50,100]
notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco valor = [x:xs | x <- notas, valor >= x,
 xs <- notasTroco (valor-x) ]











--20) Desenvolver a função nRainhas que resolve o Problema das N rainhas, para um valor
--n dado como entrada. Esse problema consiste em posicionar N rainhas num tabuleiro N x
--N de forma que nenhuma rainha possa capturar outra rainha em um único movimento. O
--resultado deve conter uma lista de todas as soluções possíveis para o valor n dado como
--entrada, em que cada solução é uma lista que apresenta a posição da linha de cada
--rainha em ordem de coluna (colunas de 1 a N). Por exemplo, a lista [3,1,4,2] é uma
--possível solução para o problema de 4 rainhas em um tabuleiro 4x4, onde: a 1a
 --rainha é
--posicionada na 1a
 --coluna e 3
--a
 --linha, a 2a
 --rainha é posicionada na 2a
 --co--luna e 1
--a
 --linha, a 3a
--rainha é posicionada na 3a
 --coluna e 4
--a
 --linha e a 4a
 --rainha é posicionada na 4a
 --coluna e 2
--a
--linha. Note que essa não é a única solução para a instância de 4 rainhas e a lista [3,1,4,2]
--é uma sub-lista da lista de saída.





tira_elem :: Int -> [Int] -> [Int]
tira_elem x [] = []
tira_elem x (h:t) = if h == x then t
                              else (h:tira_elem x t)

verificaD :: Int -> [Int] -> Bool
verificaD _ [_] = True
verificaD x (h:s:t) = if abs (s-h) == x then False
                                        else verificaD (x+1) (h:t)

resolve :: Int -> [Int] -> [[Int]]
resolve 0 _ = [[]]
resolve k l = [ h:t | h <- l, t <- resolve (k-1) (tira_elem h l), verificaD 1 (h:t) ]

nRainhas :: Int -> [[Int]]
nRainhas n = resolve n [1..n]


