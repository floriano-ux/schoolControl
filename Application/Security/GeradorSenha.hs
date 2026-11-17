module Application.Security.GeradorSenha
    ( FonteAleatoria
    , tamanhoSenhaGerada
    , montarSenha
    , gerarSenhaAleatoria
    ) where

import IHP.Prelude
import Application.Security (AuthConfig (..))
import Control.Monad (replicateM)
import Control.Monad.State (StateT, evalStateT, state, lift)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text

-- Porta para a fonte de aleatoriedade. Em produção: getRandomBytes (crypton).
type FonteAleatoria = Int -> IO ByteString

maiusculas, minusculas, digitos, alfabeto :: Text
maiusculas = "ABCDEFGHJKLMNPQRSTUVWXYZ"
minusculas = "abcdefghijkmnopqrstuvwxyz"
digitos    = "23456789"
alfabeto   = maiusculas <> minusculas <> digitos

tamanhoSenhaGerada :: AuthConfig -> Int
tamanhoSenhaGerada config = max 12 config.minPasswordLength

-- Núcleo puro: consome bytes aleatórios e devolve a senha, ou Nothing se os bytes acabarem.
-- Garante ao menos uma maiúscula, uma minúscula e um dígito e embaralha o resultado.
montarSenha :: Int -> [Word8] -> Maybe Text
montarSenha tamanho = evalStateT gerar
  where
    gerar = do
        obrigatorios <- mapM sortear [maiusculas, minusculas, digitos]
        restantes    <- replicateM (max 0 (tamanho - length obrigatorios)) (sortear alfabeto)
        Text.pack <$> embaralhar (obrigatorios <> restantes)

sortear :: Text -> StateT [Word8] Maybe Char
sortear conjunto = Text.index conjunto <$> sortearIndice (Text.length conjunto)

-- Amostragem por rejeição: descarta bytes >= limite para eliminar o viés do mod.
sortearIndice :: Int -> StateT [Word8] Maybe Int
sortearIndice n = do
    byte <- proximoByte
    let valor  = fromIntegral byte :: Int
        limite = 256 - 256 `mod` n
    if valor < limite then pure (valor `mod` n) else sortearIndice n

proximoByte :: StateT [Word8] Maybe Word8
proximoByte = do
    proximo <- state (\bytes -> (uncons bytes, drop 1 bytes))
    maybe (lift Nothing) (pure . fst) proximo

-- Fisher-Yates.
embaralhar :: [a] -> StateT [Word8] Maybe [a]
embaralhar [] = pure []
embaralhar xs = do
    i <- sortearIndice (length xs)
    case List.splitAt i xs of
        (antes, escolhido : depois) -> (escolhido :) <$> embaralhar (antes <> depois)
        _                           -> lift Nothing

gerarSenhaAleatoria :: FonteAleatoria -> AuthConfig -> IO Text
gerarSenhaAleatoria fonte config = tentar
  where
    tamanho = tamanhoSenhaGerada config
    tentar = do
        bytes <- fonte (tamanho * 4)
        maybe tentar pure (montarSenha tamanho (BS.unpack bytes))
