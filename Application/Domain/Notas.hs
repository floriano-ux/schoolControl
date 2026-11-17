module Application.Domain.Notas
    ( Pesos (..)
    , Situacao (..)
    , Conceito (..)
    , notaMinimaParcial
    , notaMinimaAV3
    , notaMinimaFinal
    , mediaPonderada
    , pesosPadrao
    , comporAV
    , mediaSimples
    , arredondar
    , notaAV1
    , notaAV2
    , notaAV3
    , mediaParcial
    , mediaFinal
    , classificarMedia
    , situacaoDaMatricula
    , mediaDaTurma
    , nomeDaSituacao
    ) where

import IHP.Prelude
import Generated.Types

-- Pesos da composição de uma AV: Prova Escrita (PE) e Avaliação Formativa (AVf).
data Pesos = Pesos
    { pesoProva     :: Double
    , pesoFormativa :: Double
    } deriving (Eq, Show)

-- Situação acadêmica de um aluno numa disciplina.
data Situacao
    = Aprovado
    | Reprovado
    | AguardandoAV3   -- média de AV1 e AV2 >= 4,0, mas AV3 ainda não foi lançada
    | Trancado
    deriving (Eq, Show)

-- Faixas de desempenho
data Conceito
    = Insuficiente
    | Regular
    | Bom
    | Otimo
    | Excelente
    deriving (Eq, Show, Ord, Enum, Bounded)

-- Média entre AV1 e AV2 abaixo disso: reprovação por nota.
notaMinimaParcial :: Double
notaMinimaParcial = 4.0

-- AV3 abaixo disso: reprovação por nota.
notaMinimaAV3 :: Double
notaMinimaAV3 = 4.0

-- Nota final (NF) a partir da qual o aluno é aprovado.
notaMinimaFinal :: Double
notaMinimaFinal = 5.0


-- Currying e closures

mediaPonderada :: Pesos -> Double -> Double -> Double
mediaPonderada Pesos { pesoProva, pesoFormativa } = \pe avf ->
    arredondar ((pe * pesoProva + avf * pesoFormativa) / (pesoProva + pesoFormativa))

-- PE vale 70%, AVf vale 30%.
pesosPadrao :: Pesos
pesosPadrao = Pesos { pesoProva = 0.7, pesoFormativa = 0.3 }

-- AVn = 70% PEn + 30% AVfn
comporAV :: Double -> Double -> Double
comporAV = mediaPonderada pesosPadrao

mediaSimples :: Double -> Double -> Double
mediaSimples = mediaPonderada (Pesos 1 1)

-- Arredonda para 2 casas decimais, evitando erros de ponto flutuante.
arredondar :: Double -> Double
arredondar x = fromIntegral (round (x * 100) :: Integer) / 100


notaAV1 :: Grade -> Double
notaAV1 grade = comporAV grade.pe1 grade.avf1

notaAV2 :: Grade -> Double
notaAV2 grade = comporAV grade.pe2 grade.avf2

-- AV3 só existe depois que PE3 e AVf3 forem lançadas.
notaAV3 :: Grade -> Maybe Double
notaAV3 grade = comporAV <$> grade.pe3 <*> grade.avf3

-- (AV1 + AV2) / 2
mediaParcial :: Grade -> Double
mediaParcial grade = mediaSimples (notaAV1 grade) (notaAV2 grade)

-- NF = ((AV1 + AV2) / 2 + AV3) / 2. Enquanto a AV3 não é lançada, devolve a média parcial.
mediaFinal :: Grade -> Double
mediaFinal grade = case notaAV3 grade of
    Nothing  -> mediaParcial grade
    Just av3 -> mediaSimples (mediaParcial grade) av3


classificarMedia :: Double -> Conceito
classificarMedia media
    | media < 4.0 = Insuficiente
    | media < 6.0 = Regular
    | media < 7.5 = Bom
    | media < 9.0 = Otimo
    | otherwise   = Excelente

-- Classificação
--   1. (AV1 + AV2) / 2 < 4,0            -> Reprovado
--   2. AV3 ainda não lançada            -> AguardandoAV3
--   3. AV3 < 4,0                        -> Reprovado
--   4. NF >= 5,0                        -> Aprovado
--   5. NF < 5,0                         -> Reprovado
situacaoDaMatricula :: EnrollmentStatus -> Grade -> Situacao
situacaoDaMatricula Trancada _ = Trancado
situacaoDaMatricula _ grade
    | mediaParcial grade < notaMinimaParcial = Reprovado
    | otherwise = case notaAV3 grade of
        Nothing -> AguardandoAV3
        Just av3
            | av3 < notaMinimaAV3                  -> Reprovado
            | mediaFinal grade >= notaMinimaFinal -> Aprovado
            | otherwise                            -> Reprovado

-- Recursão em cauda com acumuladores estritos.
mediaDaTurma :: [Double] -> Maybe Double
mediaDaTurma = go 0 0
  where
    go :: Double -> Int -> [Double] -> Maybe Double
    go _ 0 [] = Nothing
    go !soma !qtd [] = Just (soma / fromIntegral qtd)
    go !soma !qtd (x:xs) = go (soma + x) (qtd + 1) xs

nomeDaSituacao :: Situacao -> Text
nomeDaSituacao situacao = case situacao of
    Aprovado      -> "Aprovado"
    Reprovado     -> "Reprovado"
    AguardandoAV3 -> "Aguardando AV3"
    Trancado      -> "Trancado"
