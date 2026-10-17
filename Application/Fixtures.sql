-- Usuários de teste para desenvolvimento local.
-- O IHP carrega este arquivo quando cria o banco (primeiro "devenv up").
-- Senha de todos: senha123  (apenas para testes; não use em produção)
INSERT INTO public.users (name, email, password_hash, user_role) VALUES
    ('Administrador', 'admin@schoolcontrol.dev', 'sha256|17|JpwqLfZedt9soolJOb72Dw==|U23kL5I+PZU1nU6rcbEtvztnla2to/3Uc8aifcBFj60=', 'admin'),
    ('Carla Coordenadora', 'coordenador@schoolcontrol.dev', 'sha256|17|JpwqLfZedt9soolJOb72Dw==|U23kL5I+PZU1nU6rcbEtvztnla2to/3Uc8aifcBFj60=', 'coordenador'),
    ('Paulo Professor', 'professor@schoolcontrol.dev', 'sha256|17|JpwqLfZedt9soolJOb72Dw==|U23kL5I+PZU1nU6rcbEtvztnla2to/3Uc8aifcBFj60=', 'professor'),
    ('Ana Aluna', 'aluno@schoolcontrol.dev', 'sha256|17|JpwqLfZedt9soolJOb72Dw==|U23kL5I+PZU1nU6rcbEtvztnla2to/3Uc8aifcBFj60=', 'aluno');
