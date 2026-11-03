-- Adiciona o CPF (cifrado) aos usuários. Campo opcional.
ALTER TABLE users ADD COLUMN document TEXT DEFAULT NULL;
