module Application.Domain where

data StatusMatricula = Ativa | Trancada | Concluida
    deriving (Show, Eq)

data SituacaoAcademica = Aprovado | Reprovado | EmExame | EmCurso
    deriving (Show, Eq)

data StudentRecord = StudentRecord
    { studentName  :: String
    , notaP1       :: Double
    , notaP2       :: Double
    , pesoP1       :: Double
    , pesoP2       :: Double
    , exameFinal   :: Maybe Double
    } deriving (Show, Eq)

calcularIndiceMedio :: Double -> Double -> (Double -> Double -> Double)
calcularIndiceMedio w1 w2 = \n1 n2 -> (n1 * w1 + n2 * w2) / (w1 + w2)

calcularMediaPadrao :: Double -> Double -> Double
calcularMediaPadrao = calcularIndiceMedio 0.4 0.6

avaliarSituacao :: StudentRecord -> SituacaoAcademica
avaliarSituacao student = 
    let media = calcularMediaPadrao (notaP1 student) (notaP2 student)
    in case exameFinal student of
        Nothing -> 
            if media >= 7.0 
                then Aprovado 
                else if media >= 4.0 then EmExame else Reprovado
        Just notaExame -> 
            if (media + notaExame) / 2.0 >= 5.0 
                then Aprovado 
                else Reprovado

listarAprovados :: [StudentRecord] -> [String]
listarAprovados alunos = 
    [ studentName a ++ " - Aprovado (Média: " ++ show (calcularMediaPadrao (notaP1 a) (notaP2 a)) ++ ")"
    | a <- alunos
    , avaliarSituacao a == Aprovado
    ]