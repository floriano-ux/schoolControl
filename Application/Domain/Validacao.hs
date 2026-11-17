module Application.Domain.Validacao
    ( ErroValidacao (..)
    , validarNota
    , validarCargaHoraria
    , validarCourse
    , validarSubject
    , validarGrade
    , emailValido
    , mensagemDeErro
    ) where

import IHP.Prelude
import Generated.Types
import qualified Data.Text as Text

data ErroValidacao
    = CampoVazio Text
    | EmailInvalido Text
    | NotaForaDoIntervalo Text Double
    | CargaHorariaInvalida Int
    deriving (Eq, Show)

-- Uma nota é válida se estiver entre 0 e 10.
validarNota :: Text -> Double -> Either ErroValidacao Double
validarNota campo nota
    | nota >= 0 && nota <= 10 = Right nota
    | otherwise               = Left (NotaForaDoIntervalo campo nota)

-- A carga horária de um curso ou disciplina precisa ser positiva.
validarCargaHoraria :: Int -> Either ErroValidacao Int
validarCargaHoraria horas
    | horas > 0 = Right horas
    | otherwise = Left (CargaHorariaInvalida horas)

validarCourse :: Course -> [ErroValidacao]
validarCourse course = validarNomeECarga course.name course.workload

validarSubject :: Subject -> [ErroValidacao]
validarSubject subject = validarNomeECarga subject.name subject.workload

validarNomeECarga :: Text -> Int -> [ErroValidacao]
validarNomeECarga nome carga =
    [ CampoVazio "name" | Text.null (Text.strip nome) ]
    <> lefts [ validarCargaHoraria carga ]

-- Valida todas as notas parciais de uma vez.
validarGrade :: Grade -> [ErroValidacao]
validarGrade grade = lefts
    ( [ validarNota "pe1"  grade.pe1
      , validarNota "avf1" grade.avf1
      , validarNota "pe2"  grade.pe2
      , validarNota "avf2" grade.avf2
      ]
    <> opcional "pe3"  grade.pe3
    <> opcional "avf3" grade.avf3
    )
  where
    opcional campo = maybe [] (\nota -> [validarNota campo nota])

-- Um e-mail precisa de exatamente um "@", com algo antes e um domínio com ponto depois.
emailValido :: Text -> Bool
emailValido email = case Text.splitOn "@" email of
    [usuario, dominio] -> not (Text.null usuario) && "." `Text.isInfixOf` dominio
    _                  -> False

mensagemDeErro :: ErroValidacao -> Text
mensagemDeErro erro = case erro of
    CampoVazio _                -> "Este campo é obrigatório"
    EmailInvalido _             -> "Informe um e-mail válido"
    NotaForaDoIntervalo _ _     -> "A nota deve estar entre 0 e 10"
    CargaHorariaInvalida _      -> "A carga horária deve ser maior que zero"
