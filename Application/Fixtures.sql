-- Usuários de teste para desenvolvimento local.
-- O IHP carrega este arquivo quando cria o banco (primeiro "devenv up").
-- Senha de todos: senha123  (apenas para testes; não use em produção)
-- Hash em bcrypt (gerado por Application.Security.gerarHashSenha).
INSERT INTO public.users (name, email, password_hash, user_role) VALUES
    ('Administrador', 'admin@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'admin'),
    ('Carla Coordenadora', 'coordenador@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'coordenador'),
    ('Paulo Professor', 'professor@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'professor'),
    ('Ana Aluna', 'aluno@schoolcontrol.dev', '$2b$10$JZgEcoTcIG5sp4UrqnAYgu4J1uCBrIGWqZ4IiwwM6fAQF28V7a6c.', 'aluno');

-- Contas de teste por papel (senhas no padrão da política atual):
--   floriano@schoolcontrol.dev / Floriano123  (aluno)
--   ruan@schoolcontrol.dev     / Ruan12345    (professor)
--   nicolly@schoolcontrol.dev  / Nicolly123   (admin)
INSERT INTO public.users (name, email, password_hash, user_role) VALUES
    ('Floriano', 'floriano@schoolcontrol.dev', '$2b$10$ISuDhLrNzMWY91uE4gXrge5eaG6cIfbHgKKrUjrj1nQuFj.Kvl4Z6', 'aluno'),
    ('Ruan', 'ruan@schoolcontrol.dev', '$2b$10$FjaHgIJFFLyaKXXiF0kZwex8K1uWXn4Rym7jcEr4fir8A7EfVf5LO', 'professor'),
    ('Nicolly', 'nicolly@schoolcontrol.dev', '$2b$10$1akJJVrzjKxE6QDxVJ9KG.rvbt5rTcI43jefrd7dP94IZQKZx2jT.', 'admin')
ON CONFLICT (email) DO NOTHING;
