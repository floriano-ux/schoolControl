module Application.Security
    ( 
      AuthConfig (..)
    , configPadrao
    , carregarAuthConfig
    , ErroSenha (..)
    , mensagemDaSenha
    , validarForcaSenha
    , avaliarForcaSenha
    , tamanhoMinimoSenha
    , gerarHashSenha
    , conferirSenha
    , cifrarCampo
    , decifrarCampo
    , cifrarOpcional
    , decifrarOpcional
    ) where

import IHP.Prelude
import Data.Char (isDigit, isLetter)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import Control.Monad.Reader (Reader, runReader, asks)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import "crypton" Crypto.KDF.BCrypt (hashPassword, validatePassword)
import "crypton" Crypto.Cipher.AES (AES256)
import "crypton" Crypto.Cipher.Types
    ( Cipher (cipherInit), AEADMode (AEAD_GCM), AuthTag (AuthTag, unAuthTag)
    , aeadInit, aeadSimpleEncrypt, aeadSimpleDecrypt
    )
import "crypton" Crypto.Error (throwCryptoError, maybeCryptoError)
import "crypton" Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base (Base64))


-- ---------------------------------------------------------------------------
-- Configuração de autenticação
-- ---------------------------------------------------------------------------

data AuthConfig = AuthConfig
    { bcryptCost        :: Int        
    , minPasswordLength :: Int        -- tamanho mínimo exigido da senha
    , encryptionKey     :: ByteString 
    } deriving (Show)

-- Padrões de desenvolvimento. Em produção, sobrescritos por variáveis de ambiente.
configPadrao :: AuthConfig
configPadrao = AuthConfig
    { bcryptCost = 10
    , minPasswordLength = 8
    , encryptionKey = chaveDe32Bytes "schoolcontrol-dev-key-change-me!"
    }

-- Garante uma chave de exatamente 32 bytes (AES-256), truncando ou completando com zeros.
chaveDe32Bytes :: Text -> ByteString
chaveDe32Bytes texto = BS.take 32 (TE.encodeUtf8 texto <> BS.replicate 32 0)

-- Limite de IO: lê o ambiente e devolve uma config imutável
carregarAuthConfig :: IO AuthConfig
carregarAuthConfig = do
    custo <- lookupEnv "AUTH_BCRYPT_COST"
    chave <- lookupEnv "AUTH_ENCRYPTION_KEY"
    pure configPadrao
        { bcryptCost = fromMaybe configPadrao.bcryptCost (custo >>= readMaybe)
        , encryptionKey = maybe configPadrao.encryptionKey (chaveDe32Bytes . cs) chave
        }


-- ---------------------------------------------------------------------------
-- Força da senha — validação pura usando o monad Reader para a configuração
-- ---------------------------------------------------------------------------

data ErroSenha
    = SenhaMuitoCurta Int
    | SenhaSemNumero
    | SenhaSemLetra
    deriving (Eq, Show)

mensagemDaSenha :: ErroSenha -> Text
mensagemDaSenha erro = case erro of
    SenhaMuitoCurta minimo -> "A senha precisa ter pelo menos " <> tshow minimo <> " caracteres"
    SenhaSemNumero         -> "A senha precisa conter pelo menos um número"
    SenhaSemLetra          -> "A senha precisa conter pelo menos uma letra"

tamanhoMinimoSenha :: Reader AuthConfig Int
tamanhoMinimoSenha = asks (.minPasswordLength)

validarForcaSenha :: Text -> Reader AuthConfig (Either ErroSenha ())
validarForcaSenha senha = do
    minimo <- tamanhoMinimoSenha
    pure $ if
        | Text.length senha < minimo    -> Left (SenhaMuitoCurta minimo)
        | not (Text.any isDigit senha)  -> Left SenhaSemNumero
        | not (Text.any isLetter senha) -> Left SenhaSemLetra
        | otherwise                     -> Right ()

-- Executa o Reader com a config dada
avaliarForcaSenha :: AuthConfig -> Text -> Either ErroSenha ()
avaliarForcaSenha config senha = runReader (validarForcaSenha senha) config


-- ---------------------------------------------------------------------------
-- Hash de senhas com bcrypt
-- ---------------------------------------------------------------------------

-- Gera o hash bcrypt 
gerarHashSenha :: AuthConfig -> Text -> IO Text
gerarHashSenha config senha = do
    hash <- hashPassword config.bcryptCost (TE.encodeUtf8 senha) :: IO ByteString
    pure (TE.decodeUtf8 hash)

-- Confere a senha contra o hash bcrypt armazenado. Função pura.
conferirSenha :: Text -> Text -> Bool
conferirSenha senha hash =
    validatePassword (TE.encodeUtf8 senha) (TE.encodeUtf8 hash)


-- ---------------------------------------------------------------------------
-- Criptografia simétrica autenticada de campos sensíveis (AES-256-GCM)
-- ---------------------------------------------------------------------------

nonceBytes, tagBytes :: Int
nonceBytes = 12
tagBytes   = 16

-- Cifra um texto. O nonce aleatório torna esta operação um efeito (IO).
cifrarCampo :: AuthConfig -> Text -> IO Text
cifrarCampo config texto = do
    nonce <- getRandomBytes nonceBytes :: IO ByteString
    let cipher    = throwCryptoError (cipherInit config.encryptionKey) :: AES256
        aead      = throwCryptoError (aeadInit AEAD_GCM cipher nonce)
        (tag, ct) = aeadSimpleEncrypt aead BS.empty (TE.encodeUtf8 texto) tagBytes
        pacote    = nonce <> BA.convert (unAuthTag tag) <> ct
    pure (TE.decodeUtf8 (convertToBase Base64 pacote))

-- Decifra um texto. Pode falhar (Base64 inválido, chave errada, adulteração) -> Maybe.
decifrarCampo :: AuthConfig -> Text -> Maybe Text
decifrarCampo config blob = do
    pacote <- either (const Nothing) Just (convertFromBase Base64 (TE.encodeUtf8 blob)) :: Maybe ByteString
    let (nonce, resto) = BS.splitAt nonceBytes pacote
        (tag, ct)      = BS.splitAt tagBytes resto
    cipher <- maybeCryptoError (cipherInit config.encryptionKey) :: Maybe AES256
    aead   <- maybeCryptoError (aeadInit AEAD_GCM cipher nonce)
    plano  <- aeadSimpleDecrypt aead BS.empty ct (AuthTag (BA.convert tag))
    pure (TE.decodeUtf8 plano)

-- Campo opcional: texto em branco não é cifrado (vira Nothing); caso contrário, cifra.
cifrarOpcional :: AuthConfig -> Text -> IO (Maybe Text)
cifrarOpcional config texto
    | Text.null (Text.strip texto) = pure Nothing
    | otherwise                    = Just <$> cifrarCampo config texto

-- Decifra um campo opcional já armazenado (Nothing permanece Nothing).
decifrarOpcional :: AuthConfig -> Maybe Text -> Maybe Text
decifrarOpcional config = (>>= decifrarCampo config)
