-- Usuários de teste para desenvolvimento local.
-- O IHP carrega este arquivo quando cria o banco (primeiro "devenv up").
-- Senha de todos: senha123  (apenas para testes; não use em produção)
-- Hash em bcrypt (gerado por Application.Security.gerarHashSenha).
INSERT INTO public.users (name, email, password_hash, user_role) VALUES
    ('Administrador', 'admin@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'admin'),
    ('Carla Coordenadora', 'coordenador@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'coordenador'),
    ('Paulo Professor', 'professor@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'professor'),
    ('Ana Aluna', 'aluno@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'aluno');
