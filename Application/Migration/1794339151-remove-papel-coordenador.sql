-- O papel coordenador deixa de existir. Contas de coordenador não têm vínculos
-- (matrículas são de alunos e disciplinas, de professores), então são excluídas.
DELETE FROM users WHERE user_role = 'coordenador';

-- O Postgres não remove valores de um ENUM: o tipo é recriado sem 'coordenador'.
ALTER TYPE user_role RENAME TO user_role_antigo;
CREATE TYPE user_role AS ENUM ('admin', 'professor', 'aluno');
ALTER TABLE users ALTER COLUMN user_role DROP DEFAULT;
ALTER TABLE users ALTER COLUMN user_role TYPE user_role USING user_role::text::user_role;
ALTER TABLE users ALTER COLUMN user_role SET DEFAULT 'aluno';
DROP TYPE user_role_antigo;
