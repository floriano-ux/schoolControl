-- Aborta se algum e-mail de students já pertence a um usuário que não é aluno:
-- a matrícula passaria a apontar para um professor, coordenador ou admin.
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM students s
        JOIN users u ON lower(u.email) = lower(s.email)
        WHERE u.user_role <> 'aluno'
    ) THEN
        RAISE EXCEPTION 'Há estudantes cujo e-mail pertence a um usuário que não é aluno. Corrija antes de migrar.';
    END IF;
END $$;

-- 1. Disciplinas + uma disciplina padrão por curso (para não perder matrículas)
CREATE TABLE subjects (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    course_id UUID NOT NULL,
    name TEXT NOT NULL,
    workload INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX subjects_course_id_index ON subjects (course_id);
ALTER TABLE subjects ADD CONSTRAINT subjects_ref_course_id FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE;
ALTER TABLE subjects ADD CONSTRAINT subjects_workload_check CHECK (workload > 0);
ALTER TABLE subjects ADD CONSTRAINT subjects_course_id_name_key UNIQUE (course_id, name);

INSERT INTO subjects (course_id, name, workload)
SELECT id, name || ' — Disciplina geral', workload FROM courses;

-- 2. students -> users (papel aluno). Hash inválido: a senha precisa ser regenerada pelo admin.
INSERT INTO users (name, email, password_hash, user_role)
SELECT s.name, s.email, '!', 'aluno' FROM students s
WHERE NOT EXISTS (SELECT 1 FROM users u WHERE lower(u.email) = lower(s.email));

-- 3. Remapeia matrículas
ALTER TABLE enrollments DROP CONSTRAINT enrollments_ref_student_id;
UPDATE enrollments e SET student_id = u.id
  FROM students s JOIN users u ON lower(u.email) = lower(s.email)
 WHERE e.student_id = s.id;

ALTER TABLE enrollments ADD COLUMN subject_id UUID;
UPDATE enrollments e SET subject_id = sub.id FROM subjects sub WHERE sub.course_id = e.course_id;
ALTER TABLE enrollments ALTER COLUMN subject_id SET NOT NULL;
ALTER TABLE enrollments DROP CONSTRAINT enrollments_student_id_course_id_key;
ALTER TABLE enrollments DROP CONSTRAINT enrollments_ref_course_id;
DROP INDEX IF EXISTS enrollments_course_id_index;
ALTER TABLE enrollments DROP COLUMN course_id;

-- 4. Novas FKs/índices (iguais ao Schema.sql) e remoção da tabela antiga
CREATE INDEX enrollments_subject_id_index ON enrollments (subject_id);
ALTER TABLE enrollments ADD CONSTRAINT enrollments_ref_student_id FOREIGN KEY (student_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE enrollments ADD CONSTRAINT enrollments_ref_subject_id FOREIGN KEY (subject_id) REFERENCES subjects (id) ON DELETE CASCADE;
ALTER TABLE enrollments ADD CONSTRAINT enrollments_student_id_subject_id_key UNIQUE (student_id, subject_id);
DROP TABLE students;
